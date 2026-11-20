;; The code that examples/risk/plan.edn was written from. It is read by `facts`, never loaded.
(ns risk.core
  "Risk scores in one nested map: {tenant {entity {feature score}}}.")

(defn value [db tenant entity feature]
  (get-in db [tenant entity feature]))

(defn entity-features [db tenant entity]
  (reduce-kv (fn [acc feature score] (conj acc [feature score]))
             []
             (get-in db [tenant entity])))

(defn feature-total [db feature]
  (reduce + 0.0 (for [entities (vals db)
                      features (vals entities)
                      :let [score (get features feature)]
                      :when score]
                  score)))

(defn tenant-size [db tenant]
  (reduce + 0 (map count (vals (get db tenant)))))
