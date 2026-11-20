;; A fixture for the facts extractor. It is read, never loaded.
(ns sample.core
  (:require [clojure.core :as core] [clojure.string :as str]))

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

(defn qualified [db tenant risk]
  (get (clojure.core/get-in db [:scores tenant]) risk))

(defn aliased [db tenant risk]
  (get (core/get db tenant) risk))

(defn shadowed [db id get]
  (clojure.core/get (get db :transformed) id))

(defn root-scan [db]
  (keys db))

(defn constant-key [db]
  (get db :scores))

(defn threaded-last [db f init]
  (->> db (reduce-kv f init)))

(defn threaded-last-key [tenant scores]
  (->> tenant (get scores)))

(defn threaded-bare [db]
  (->> db keys))

(defn multi-write [db tenant entity]
  (assoc (get db :scores) tenant 1 entity 2)
  (dissoc (get db :scores) tenant entity))

(defn risky? [db tenant entity limit]
  (< limit (get-in db [:scores tenant entity :risk] 0.0)))
