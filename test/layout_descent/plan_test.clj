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
                (fx/normal (assoc fx/small-risk :emit '{:ns demo.layout :path "gen/demo/layout.clj"}))
                (fx/normal (-> fx/small-risk
                               (assoc :notes ["Fanout from the bench." "Weights are a guess."]
                                      :emit '{:ns demo.layout :path "gen/demo/layout.clj"})
                               (assoc-in [:workload :degree] [nil 12 3])))]]
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

(def sparse-case
  "The accepted plan of the sparse-layout case study, whose hashes are in its journal."
  '{:layout/version 1
    :name sparse-layout-case
    :levels [{:id :entity :type :long} {:id :feature :type :keyword}]
    :leaf {:type :double}
    :patterns [{:id :point :bind [:key :key]}
               {:id :row-sum :bind [:key :scan] :fold :sum}
               {:id :col-sum :bind [:scan :key] :fold :sum}]
    :orders [{:perm [0 1] :format :csf} {:perm [1 0] :format :csf}]
    :serve {:col-sum 1 :point 0 :row-sum 0}
    :workload {:fanout [2048 256] :fill [1.0 0.046875] :seed 7
               :weights {:col-sum 1 :point 1 :row-sum 1}}
    :objective {:ns-per-byte 0.0 :min-gain 0.05}})

(deftest optional-keys-keep-the-hashes-of-existing-plans
  (is (= "e471a80f71f4" (plan/plan-hash example)))
  (is (= "4e4122ee6a7e" (plan/task-hash example)))
  (is (= "31d255ce3c2c" (plan/plan-hash (fx/normal sparse-case))))
  (is (= "7c4f57b77b62" (plan/task-hash (fx/normal sparse-case))))
  (testing "a plan that gives neither key has neither in its normal form"
    (is (not (contains? example :notes)))
    (is (= [:fanout :fill :seed :weights] (keys (:workload example))))))

(deftest degree-is-validated-against-the-fanout
  (let [with-degree #(assoc-in fx/small-risk [:workload :degree] %)]
    (is (= #{} (codes (with-degree [nil 12 3]))))
    (is (= #{} (codes (with-degree [4 60 8]))))
    (is (= #{} (codes (with-degree [nil nil nil]))))
    (doseq [bad [[nil 12] [nil 12 3 1] [nil 61 3] [0 12 3] [nil -1 3] [nil 1.5 3] [nil "12" 3] '(nil 12 3) 12]]
      (is (= #{:bad-degree} (codes (with-degree bad))) (pr-str bad)))
    (is (= [[:workload :degree]] (mapv :path (plan/problems (with-degree [5 1 1])))))
    (testing "without :fanout the default fanout is the limit"
      (let [bare (update fx/small-risk :workload dissoc :fanout)
            fanout (first (:fanout (:workload (plan/normalize bare))))]
        (is (= #{} (codes (assoc-in bare [:workload :degree] [fanout nil nil]))))
        (is (= #{:bad-degree} (codes (assoc-in bare [:workload :degree] [(inc fanout) nil nil]))))))
    (testing "an unusable fanout is the only problem that it causes"
      (is (= #{:bad-fanout} (codes (-> (with-degree [nil 12 3]) (assoc-in [:workload :fanout] [4 60]))))))))

(deftest degree-is-part-of-the-task
  (let [base (fx/normal fx/small-risk)
        exact (fx/normal (assoc-in fx/small-risk [:workload :degree] [nil 12 3]))]
    (is (= [:fanout :fill :degree :seed :weights] (keys (:workload exact))))
    (is (= [nil 12 3] (:degree (:workload exact))))
    (is (= (plan/plan-hash base) (plan/plan-hash exact)))
    (is (not= (plan/task-hash base) (plan/task-hash exact)))
    (is (not= (plan/task-hash exact)
              (plan/task-hash (assoc-in fx/small-risk [:workload :degree] [nil 12 4]))))
    (testing "all nil means what no :degree means"
      (is (= base (fx/normal (assoc-in fx/small-risk [:workload :degree] [nil nil nil])))))
    (testing "a neighbor keeps the degree, so it stays in the task"
      (is (every? #(= (plan/task-key exact) (plan/task-key (plan/normalize (:plan %))))
                  (plan/neighbors exact))))))

(deftest notes-survive-render-and-read
  (is (= #{} (codes (assoc fx/small-risk :notes "One note."))))
  (is (= #{} (codes (assoc fx/small-risk :notes []))))
  (doseq [bad [1 :note ["a" 1] '("a") {:text "a"}]]
    (is (= #{:bad-notes} (codes (assoc fx/small-risk :notes bad))) (pr-str bad)))
  (is (= [[:notes]] (mapv :path (plan/problems (assoc fx/small-risk :notes 1)))))
  (doseq [notes ["Fanout from bench_data.clj:9; \"small\" case."
                 ["Fanout from bench_data.clj:9." "Weights 1/1/1: the bench runs equal cases."]]]
    (let [plan (fx/normal (assoc fx/small-risk :notes notes))
          text (plan/render plan)
          f (java.io.File/createTempFile "layout-notes" ".edn")]
      (is (= [:layout/version :name :notes :levels] (take 4 (keys plan))))
      (is (str/starts-with? text "{:layout/version 1\n :name risk\n :notes "))
      (try
        (spit f text)
        (is (= notes (:notes (plan/read-plan f))))
        (is (= plan (plan/read-plan f)))
        (is (= text (plan/render (plan/read-plan f))))
        (finally (.delete f)))
      (testing "notes are in neither hash"
        (is (= (plan/plan-hash (fx/normal fx/small-risk)) (plan/plan-hash plan)))
        (is (= (plan/task-hash (fx/normal fx/small-risk)) (plan/task-hash plan))))
      (testing "new layout decisions keep the notes: accept replaces :orders and :serve only"
        (let [other (fx/with-order fx/small-risk [2 0 1] :csf [:feature-total])
              accepted (assoc plan :orders (:orders other) :serve (:serve other))]
          (is (= notes (:notes (plan/normalize (edn/read-string (plan/render accepted))))))
          (is (= (plan/plan-hash other) (plan/plan-hash accepted))))))))

(deftest a-vector-of-notes-renders-one-note-on-a-line
  (is (str/includes? (plan/render (fx/normal (assoc fx/small-risk :notes ["First." "Second."])))
                     " :notes [\"First.\"\n         \"Second.\"]\n :levels ")))

(deftest free-memory-is-reported-when-a-plan-has-several-orders
  (let [two (fx/with-order fx/small-risk [2 0 1] :csf [:feature-total])
        codes-of #(mapv :code (plan/warnings %))
        found (first (filter #(= :memory-not-priced (:code %)) (plan/warnings two)))]
    (is (= [:memory-not-priced] (codes-of two)))
    (is (= [:objective :ns-per-byte] (:path found)))
    (is (str/includes? (:message found) "Set :ns-per-byte"))
    (testing "one order has no extra order to pay for"
      (is (= [:partial-index] (codes-of (fx/normal fx/small-risk)))))
    (testing "a price on memory removes the warning"
      (is (= [] (codes-of (fx/normal (assoc two :objective {:ns-per-byte 0.001})))))
      (is (= [:memory-not-priced] (codes-of (fx/normal (assoc two :objective {:ns-per-byte 0}))))))))

(deftest partial-index-says-what-its-gain-measures
  (let [[warning] (plan/warnings (fx/normal fx/small-risk))]
    (is (= :partial-index (:code warning)))
    (is (= [:patterns :feature-total] (:path warning)))
    (is (str/includes? (:message warning) "reaches only 0 of them before it must scan."))
    (is (str/ends-with? (:message warning)
                        "A gain from adding an order for this pattern measures the missing index, not the layout format."))))
