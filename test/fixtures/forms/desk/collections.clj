(ns desk.collections
  (:require [clojure.string :as string]))

(def holder (into [] (map :id) [{:id 1}]))

(defn scan [records]
  [(into [] (map (comp string/trim :label)) records)
   (into [] (filter :active?) records)
   (into [] (map :value) records)
   (reduce-kv (fn [total entry-key record]
                (if (string/blank? entry-key) total (+ total (:amount record))))
              0 records)])

(defn labels [records]
  (map string/trim (map :label records)))

(defn remove-private [record]
  (swap! record dissoc :private))

(defn opaque-transducer [records transform]
  (into [] transform records))
