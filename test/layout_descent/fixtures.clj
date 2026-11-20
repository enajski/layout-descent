(ns layout-descent.fixtures
  "Plans shared by the tests."
  (:require [layout-descent.plan :as plan]))

(def small-risk
  "The example task on little data: three levels, one pattern of every mode."
  '{:layout/version 1
    :name risk
    :levels [{:id :tenant :type :keyword}
             {:id :entity :type :long}
             {:id :feature :type :keyword}]
    :leaf {:type :double}
    :patterns [{:id :value :bind [:key :key :key]}
               {:id :entity-features :bind [:key :key :scan]}
               {:id :feature-total :bind [:scan :scan :key] :fold :sum}
               {:id :tenant-size :bind [:key :scan :scan] :fold :count}]
    :orders [{:perm [0 1 2] :format :hash}]
    :workload {:fanout [4 60 8]
               :fill [1.0 0.9 0.6]
               :seed 7
               :weights {:value 10 :entity-features 6 :feature-total 2 :tenant-size 1}}})

(defn normal [plan]
  (assert (empty? (plan/problems plan)) (pr-str (plan/problems plan)))
  (plan/normalize plan))

(defn with-order
  "The plan with one more order that serves the given patterns."
  [plan perm format ids]
  (let [plan (normal plan)
        index (count (:orders plan))]
    (normal (-> plan
                (update :orders conj {:perm perm :format format})
                (update :serve into (map (fn [id] [id index])) ids)))))
