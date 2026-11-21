(ns board.model)

(defn payload [row] #?(:clj (:desktop row) :cljs (:browser row)))
(defn pair [id v] [id v])
