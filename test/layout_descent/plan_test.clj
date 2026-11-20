(ns layout-descent.plan-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [layout-descent.fixtures :as fx]
            [layout-descent.plan :as plan]))

(def example (plan/read-plan "examples/risk/plan.edn"))

(defn- codes [plan]
  (set (map :code (plan/problems plan))))

(deftest example-is-valid
  (is (= [] (plan/problems (edn/read-string (slurp "examples/risk/plan.edn")))))
  (is (= [:partial-index] (mapv :code (plan/warnings example))))
  (testing "the hash is written into journals, so it changes only on purpose"
    (is (= "e471a80f71f4" (plan/plan-hash example)))))

(deftest problems-name-the-place
  (is (= #{:not-a-map} (codes [])))
  (is (contains? (codes (assoc fx/small-risk :objectve {})) :unknown-key))
  (is (contains? (codes (assoc fx/small-risk :layout/version 2)) :bad-version))
  (testing "a pattern must not shadow a clojure.core name in the emitted namespace"
    (is (seq (plan/problems (assoc-in fx/small-risk [:patterns 0 :id] :get)))))
  (testing "an order is a permutation of the levels"
    (is (seq (plan/problems (assoc-in fx/small-risk [:orders 0 :perm] [0 1 1])))))
  (testing "a pattern is served by an order that exists"
    (is (seq (plan/problems (assoc fx/small-risk :serve {:value 3})))))
  (is (every? #(and (keyword? (:code %)) (vector? (:path %)) (string? (:message %)))
              (plan/problems (assoc fx/small-risk :leaf {:type :float} :name "Bad Name")))))

(deftest hash-covers-layout-decisions-only
  (let [base (fx/normal fx/small-risk)
        h (plan/plan-hash base)]
    (is (= h (plan/plan-hash (assoc base :name 'other))))
    (is (= h (plan/plan-hash (assoc-in base [:workload :seed] 99))))
    (is (= h (plan/plan-hash (assoc-in base [:objective :min-gain] 0.5))))
    (is (= h (plan/plan-hash (assoc-in base [:patterns 0 :doc] "a note"))))
    (is (not= h (plan/plan-hash (assoc-in base [:orders 0 :format] :csf))))
    (testing "the task is the other half"
      (is (= (plan/task-hash base) (plan/task-hash (assoc-in base [:orders 0 :format] :csf))))
      (is (not= (plan/task-hash base) (plan/task-hash (assoc-in base [:workload :seed] 99)))))))

(deftest neighbors-stay-in-the-task
  (let [base (fx/normal fx/small-risk)
        found (plan/neighbors base)]
    (is (seq found))
    (is (every? #(empty? (plan/problems (:plan %))) found))
    (is (every? #(= (plan/task-key base) (plan/task-key (plan/normalize (:plan %)))) found))
    (is (apply distinct? (plan/plan-hash base) (map :hash found)))
    (is (every? #(and (:op (:move %)) (string? (:why %))) found))
    (testing "an order that gives :feature-total a bound prefix is proposed"
      (is (some #(= {:op :add-order :perm [2 0 1] :format :csf :serves [:feature-total]} (:move %)) found)))))

(deftest unused-orders-are-pruned
  (let [plan (fx/with-order fx/small-risk [2 0 1] :csf [])]
    (is (= [:unused-order] (filterv #{:unused-order} (map :code (plan/warnings plan)))))
    (is (= (plan/plan-hash (fx/normal fx/small-risk)) (plan/plan-hash (plan/prune-unused plan))))))

(deftest render-is-a-fixed-point
  (doseq [plan [example
                (fx/normal (assoc fx/small-risk :emit '{:ns demo.layout :path "gen/demo/layout.clj"}))]]
    (let [text (plan/render plan)]
      (is (str/starts-with? text "{:layout/version 1\n :name "))
      (is (= plan (plan/normalize (edn/read-string text))))
      (is (= text (plan/render (plan/normalize (edn/read-string text))))))))

(deftest plan-file-holds-exactly-one-value
  (let [f (java.io.File/createTempFile "layout-plan" ".edn")]
    (try
      (spit f (str (plan/render example) ";; trailing comment\n"))
      (is (= example (plan/read-plan f)))
      (spit f (str (plan/render example) "{}\n"))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"exactly one EDN value" (plan/read-plan f)))
      (finally (.delete f)))))

(deftest plan-validation-handles-malformed-values
  (is (= #{:bad-order} (codes (assoc fx/small-risk :orders [0]))))
  (is (= #{:bad-emit} (codes (assoc fx/small-risk :emit '{:ns demo/layout :path "layout.clj"}))))
  (is (= #{:bad-ns-per-byte} (codes (assoc fx/small-risk :objective {:ns-per-byte ##NaN}))))
  (is (= (:objective (plan/normalize fx/small-risk))
         (:objective (plan/normalize (assoc fx/small-risk :objective {:ns-per-byte nil :min-gain nil}))))))
