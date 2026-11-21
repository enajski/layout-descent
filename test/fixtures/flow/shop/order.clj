(ns shop.order (:require [shop.catalog :as catalog]))

(defn settle [listing qty sink k]
  (let [sale (catalog/capture listing)
        price (:sale-price sale)
        invoice {:amount (* qty price) :receipt-price price}]
    (sink invoice)
    (get sale k)
    invoice))

(defn inventory [stock] {:sale-price (:inventory-price stock)})

(defn alternatives [choose? a b] (if choose? (:x a) (:x b)))

(defn only-one [cold hot]
  (let [m (assoc {:x (:from-cold cold) :y (:other cold)} :x (:from-hot hot))]
    (+ (:x m) (get m :absent 0))))

(defn pipeline [items]
  (->> items (mapv (fn [{:keys [unit]}] {:amount (* 2 unit)})) (mapv :amount)))

(defn increment-line [order listing]
  (-> order (assoc-in [:line :qty] (:count listing)) (update-in [:line :qty] inc)))

(defn cycle [row] (cycle (:next row)))
