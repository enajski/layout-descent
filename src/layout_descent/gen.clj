(ns layout-descent.gen
  "Seeded synthetic data and argument samples. Equal inputs give equal outputs, so two plans of one
  task are always measured on the same data."
  (:import (java.util Random)))

(defn level-key
  "The i-th key of a level of the given type."
  [type i]
  (case type
    :long (long i)
    :keyword (keyword (str "k" i))
    :string (str "s" i)))

;; Double leaves are multiples of 0.25 far below 2^53, so a sum is exact in any order of addition.
(defn- leaf-value [type ^Random r]
  (case type
    :double (* 0.25 (.nextInt r 4000))
    :long (long (.nextInt r 1000))
    :object (str "v" (.nextInt r 1000))))

(defn- drawn-indices
  "k distinct indices of 0..n-1 in rising order, drawn by r without replacement. Floyd's sampling:
  k draws and memory for k indices, whatever n is."
  [^Random r n k]
  (let [n (long n)]
    (loop [j (- n (long k))
           chosen (sorted-set)]
      (if (= j n)
        chosen
        (let [t (long (.nextInt r (int (inc j))))]
          (recur (inc j) (conj chosen (if (contains? chosen t) j t))))))))

(defn- node [types leaf-type fanout fill degree ^Random r depth]
  (if (= depth (count types))
    (leaf-value leaf-type r)
    (let [type (nth types depth)
          width (long (nth fanout depth))
          child #(node types leaf-type fanout fill degree r (inc depth))]
      (if-some [k (nth degree depth nil)]
        (persistent!
         (reduce (fn [m i] (assoc! m (level-key type i) (child)))
                 (transient {})
                 (drawn-indices r width (min (long k) width))))
        (let [p (double (nth fill depth))]
          (persistent!
           (reduce (fn [m i]
                     (if (< (.nextDouble r) p)
                       (assoc! m (level-key type i) (child))
                       m))
                   (transient {})
                   (range width))))))))

(defn data
  "The nested map of a normal plan's workload. opts may replace :fanout, :fill, :degree or :seed.
  A level with a degree gives every parent exactly that many keys, or its whole fanout when the
  fanout is smaller; a level without one keeps each key with the probability of its fill."
  ([plan] (data plan nil))
  ([plan opts]
   (let [{:keys [fanout fill degree seed]} (merge (:workload plan) opts)]
     (node (mapv :type (:levels plan)) (:type (:leaf plan)) fanout fill degree (Random. (long seed)) 0))))

(defn paths
  "Every full key path of nested data with n levels."
  [data n]
  (if (zero? n)
    [[]]
    (vec (for [[k child] data
               path (paths child (dec n))]
           (into [k] path)))))

(defn arg-samples
  "Argument vectors for one pattern: one key per :key level, in level order. A share :miss of them
  miss: half through a key that the level never holds, half through a key drawn from the whole
  level. With :boxed-ints every second sample passes its :long keys as Integer.
   Supply :key-paths to share precomputed paths across patterns."
  [plan pattern data {:keys [n seed miss boxed-ints key-paths] :or {n 64 seed 1 miss 0.25}}]
  (let [types (mapv :type (:levels plan))
        fanout (:fanout (:workload plan))
        bound (filterv #(= :key (nth (:bind pattern) %)) (range (count types)))
        all (when (seq bound) (or key-paths (paths data (count types))))
        r (Random. (long seed))
        absent (fn [level] (level-key (nth types level) -1))
        any (fn [level] (level-key (nth types level) (.nextInt r (int (nth fanout level)))))
        sample (fn [i]
                 (let [roll (.nextDouble r)
                       base (if (seq all) (nth all (.nextInt r (count all))) (mapv any (range (count types))))
                       changed (nth bound (.nextInt r (count bound)))
                       path (cond
                              (< roll (/ (double miss) 2)) (assoc base changed (absent changed))
                              (< roll (double miss)) (assoc base changed (any changed))
                              :else base)]
                   (mapv (fn [level]
                           (let [k (nth path level)]
                             (if (and boxed-ints (odd? i) (= :long (nth types level))) (int k) k)))
                         bound)))]
    (if (empty? bound)
      [[]]
      (mapv sample (range n)))))

(defn- scaled
  "A small sample of the workload shape. It drops :degree: the given fill decides, so the sparse
  sample stays sparse, and a plan has the same edge datasets with and without a degree."
  [plan cap fill seed]
  (data plan {:fanout (mapv #(min (long cap) (long %)) (:fanout (:workload plan)))
              :fill (vec (repeat (count (:levels plan)) fill))
              :degree nil
              :seed seed}))

(defn edge-datasets
  "Small datasets that hold the cases a layout gets wrong first: no data, one entry, inner maps
  with no entries, and sparse and dense samples of the workload shape."
  [plan]
  (let [types (mapv :type (:levels plan))
        n (count types)
        leaf (leaf-value (:type (:leaf plan)) (Random. 3))
        path (fn [i] (mapv #(level-key % i) types))
        single (assoc-in {} (path 0) leaf)]
    [{:name "empty" :data {}}
     {:name "single" :data single}
     {:name "empty-inner"
      :data (reduce (fn [m depth] (assoc-in m (subvec (path depth) 0 depth) {}))
                    single
                    (range 1 n))}
     {:name "sparse" :data (scaled plan 6 0.5 11)}
     {:name "dense" :data (scaled plan 4 1.0 12)}]))
