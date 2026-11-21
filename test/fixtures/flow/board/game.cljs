(ns board.game (:require [board.model :as model]))

(defonce state (atom {}))

(defn act [row x y direction]
  (let [[id payload] (model/pair (:id row) (model/payload row))
        kind (get-in @state [:world [x y] :tile/kind])]
    (case direction
      :move (swap! state (fn [s]
                           (as-> s m
                             (update-in m [:world [x y] :tile/ids] (fnil conj #{}) id)
                             (update-in m [:entities id] assoc :position [x y]))))
      nil)
    {:actor id :body payload :seen-kind kind}))

(defn reread [table a b]
  (get-in (assoc-in table [:tiles a :flag] true) [:tiles b :flag]))

(defn separate-snapshots [r x]
  (swap! r assoc :value x)
  (:value @r))
