;; A fixture for the facts extractor: a ClojureScript file whose map has vectors as keys. It is
;; read, never loaded.
(ns sample.grid)

(defonce state (atom {:world {}}))

(defn tile [x y]
  (get-in @state [:world [x y]]))

(defn place! [x y tile]
  (swap! state assoc-in [:world [x y]] tile))

(defn seen? [x y]
  (get-in @state [:world [x y] :tile/seen?]))

(defn ground [world x]
  (get world [x :ground]))

(defn kind [world x y]
  (:tile/kind (get world [(inc x) y])))

;; Not a composite key: constants only, a symbol that holds the vector, and one part.
(defn origin [world]
  (get-in world [:tiles [0 0]]))

(defn held [world position]
  (get-in world [:tiles position]))

(defn single [world x]
  (get-in world [:tiles [x]]))
