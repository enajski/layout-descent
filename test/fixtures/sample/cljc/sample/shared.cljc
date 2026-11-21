;; A fixture for the facts extractor: a file that both languages read. It is read, never loaded.
(ns sample.shared)

(defn score [db tenant entity]
  (get-in db [:scores tenant entity]))
