(ns shop.catalog)

(defn capture [{:keys [price currency]}]
  {:sale-price price :currency currency :origin #?(:clj :jvm :cljs :js)})
