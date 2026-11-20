;; A fixture for the facts extractor. It is read, never loaded.
(ns sample.core
  (:require [clojure.string :as str]))

(defn score [db tenant entity]
  (get-in db [:scores tenant entity :risk]))

(defn features [db tenant entity]
  (get-in db [:scores tenant entity]))

(defn record! [state tenant entity value]
  (swap! state assoc-in [:scores tenant entity :risk] value))

(defn bump [db tenant entity]
  (update-in db [:scores tenant entity :risk] (fnil inc 0)))

(defn threaded [db tenant entity]
  (-> db
      (get-in [:scores tenant entity :risk])
      (or 0.0)))

(defn dynamic [db path]
  (get-in db path))

(defn tenant-names [db]
  (map str/upper-case (keys (get db :scores))))

(defn total [db tenant]
  (reduce-kv (fn [acc _ features] (+ acc (get features :risk 0.0))) 0.0 (get-in db [:scores tenant])))
