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

(defn- node [types leaf-type fanout fill ^Random r depth]
  (if (= depth (count types))
    (leaf-value leaf-type r)
    (let [type (nth types depth)
          p (double (nth fill depth))]
      (persistent!
       (reduce (fn [m i]
                 (if (< (.nextDouble r) p)
                   (assoc! m (level-key type i) (node types leaf-type fanout fill r (inc depth)))
                   m))
               (transient {})
               (range (nth fanout depth)))))))

(defn data
  "The nested map of a normal plan's workload. opts may replace :fanout, :fill or :seed."
  ([plan] (data plan nil))
  ([plan opts]
   (let [{:keys [fanout fill seed]} (merge (:workload plan) opts)]
     (node (mapv :type (:levels plan)) (:type (:leaf plan)) fanout fill (Random. (long seed)) 0))))

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

(defn- scaled [plan cap fill seed]
  (data plan {:fanout (mapv #(min (long cap) (long %)) (:fanout (:workload plan)))
              :fill (vec (repeat (count (:levels plan)) fill))
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
