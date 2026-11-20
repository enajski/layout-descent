(ns layout-descent.model
  "Reference semantics. Every pattern is answered from the plain nested map, and an emitted layout
  is correct only when it agrees with this namespace.")

(defn- walk [bind depth node args scanned]
  (cond
    (= depth (count bind)) [[scanned node]]

    (= :key (nth bind depth))
    (let [child (get node (first args))]
      (if (nil? child)
        []
        (walk bind (inc depth) child (rest args) scanned)))

    :else
    (mapcat (fn [[k child]] (walk bind (inc depth) child args (conj scanned k))) node)))

(defn entries
  "Every [scanned-keys leaf] pair the pattern selects. args holds one key per :key level, in
  level order."
  [pattern data args]
  (walk (:bind pattern) 0 data args []))

(defn run
  "The answer of one pattern. A pattern with no :fold and a :scan level reduces with f from init;
  f receives the accumulator, the scanned keys in level order, and the leaf."
  ([plan pattern data args] (run plan pattern data args nil nil))
  ([plan pattern data args f init]
   (let [found (entries pattern data args)]
     (case (:fold pattern)
       :sum (reduce + (if (= :double (:type (:leaf plan))) 0.0 0) (map second found))
       :count (long (count found))
       (if (every? #{:key} (:bind pattern))
         (second (first found))
         (reduce (fn [acc [ks leaf]] (apply f acc (conj ks leaf))) init found))))))
