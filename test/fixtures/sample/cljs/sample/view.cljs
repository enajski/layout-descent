;; A fixture for the facts extractor: a ClojureScript file with no map access. It is read, never
;; loaded.
(ns sample.view)

(defn label [tile]
  (str "tile " tile))
