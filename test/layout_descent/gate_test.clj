(ns layout-descent.gate-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [layout-descent.emit :as emit]
            [layout-descent.fixtures :as fx]
            [layout-descent.gate :as gate]
            [layout-descent.plan :as plan]
            [layout-descent.rt :as rt]))

(defn- task [leaf-type]
  {:layout/version 1
   :name 'probe
   :levels [{:id :a :type :keyword} {:id :b :type :long} {:id :c :type :string}]
   :leaf {:type leaf-type}
   :patterns (into [{:id :point :bind [:key :key :key]}
                    {:id :tail :bind [:key :key :scan]}
                    {:id :middle :bind [:key :scan :key]}
                    {:id :head :bind [:scan :key :key]}
                    {:id :everything :bind [:scan :scan :scan]}
                    {:id :size :bind [:key :scan :scan] :fold :count}
                    {:id :by-c :bind [:scan :scan :key] :fold :count}]
                   (when-not (= :object leaf-type)
                     [{:id :total :bind [:scan :key :scan] :fold :sum}
                      {:id :grand-total :bind [:scan :scan :scan] :fold :sum}]))
   :orders [{:perm [0 1 2] :format :hash}]
   :workload {:fanout [5 6 4]}})

(deftest every-order-agrees-with-the-model
  (doseq [leaf-type [:double :long :object]
          perm (plan/permutations 3)
          format [:hash :csf]]
    (let [plan (fx/normal (assoc (task leaf-type) :orders [{:perm perm :format format}]))
          result (gate/verify plan)]
      (is (:ok result) (pr-str leaf-type perm format (:warnings result) (:mismatches result)))
      (is (pos? (long (:checks result)))))))

(deftest other-depths-agree-with-the-model
  (doseq [[levels patterns]
          [[[{:id :a :type :long}]
            [{:id :point :bind [:key]} {:id :everything :bind [:scan]} {:id :total :bind [:scan] :fold :sum}]]
           [[{:id :a :type :string} {:id :b :type :long}]
            [{:id :point :bind [:key :key]} {:id :column :bind [:scan :key]} {:id :size :bind [:key :scan] :fold :count}]]
           [[{:id :a :type :keyword} {:id :b :type :long} {:id :c :type :long} {:id :d :type :keyword}]
            [{:id :point :bind [:key :key :key :key]}
             {:id :slice :bind [:key :scan :key :scan]}
             {:id :total :bind [:scan :key :scan :key] :fold :sum}
             {:id :size :bind [:scan :scan :key :scan] :fold :count}]]]
          format [:hash :csf]
          perm [(vec (range (count levels))) (vec (reverse (range (count levels))))]]
    (let [plan (fx/normal {:layout/version 1 :name 'depth :levels levels :leaf {:type :double}
                           :patterns patterns :orders [{:perm perm :format format}]
                           :workload {:fanout (vec (repeat (count levels) 4))}})
          result (gate/verify plan)]
      (is (:ok result) (pr-str (count levels) perm format (:warnings result) (:mismatches result))))))

(deftest emission-is-deterministic
  (let [plan (fx/with-order fx/small-risk [2 0 1] :csf [:feature-total])
        text (emit/source plan)]
    (is (= text (emit/source (plan/normalize plan))))
    (is (str/includes? text (plan/plan-hash plan)))
    (is (= 'layout-descent.generated.risk (gate/source-ns text)))
    (is (= 'my.app.layout (gate/source-ns (emit/source plan {:ns 'my.app.layout}))))))

(defn- mutate [text old new]
  (let [changed (str/replace-first text old new)]
    (assert (not= text changed) (str "The mutation target is gone: " old))
    changed))

(deftest the-gate-catches-wrong-code
  (let [plan (fx/with-order fx/small-risk [2 0 1] :csf [:feature-total])
        text (emit/source plan {:ns (gate/eval-ns plan)})]
    (is (:ok (gate/verify-source plan text)))
    (testing "a sum that stops one leaf early"
      (let [result (gate/verify-source plan (mutate text "(aget ptrs1 b1)" "(dec (aget ptrs1 b1))"))]
        (is (false? (:ok result)))
        (is (= :feature-total (:pattern (first (:mismatches result)))))))
    (testing "a count that ignores the size of the last level"
      (is (false? (:ok (gate/verify-source plan (mutate text "(.size ^java.util.HashMap n2)" "1"))))))
    (testing "a point lookup that skips the key coercion finds no Integer key"
      (is (false? (:ok (gate/verify-source plan (mutate text "(long k1)" "k1"))))))
    (testing "a reflective call is a warning, and a warning fails the gate"
      (let [result (gate/verify-source plan (str text "\n(defn leak [x] (.length x))\n"))]
        (is (false? (:ok result)))
        (is (seq (:warnings result)))
        (is (empty? (:mismatches result)))))
    (testing "text that does not compile is a failed gate, not an exception"
      (is (false? (:ok (gate/verify-source plan (str text "\n(defn broken [] (undefined-symbol))\n"))))))))

(deftest ingest-rejects-wrong-types
  (let [descriptor {:types [:keyword :long] :leaf :double :orders [{:perm [0 1] :format :csf}]}
        code #(try (rt/build-dataset descriptor %) :built (catch clojure.lang.ExceptionInfo e (:code (ex-data e))))]
    (is (= :built (code {:a {1 1.0}})))
    (is (= :built (code {:a {(int 1) 1.0}})) "an Integer key is a :long key")
    (is (= :bad-key (code {"a" {1 1.0}})))
    (is (= :bad-key (code {:a {1.5 1.0}})))
    (is (= :bad-leaf (code {:a {1 1}})))
    (is (= :bad-leaf (code {:a {1 nil}})))
    (is (= :not-a-map (code {:a 1.0})))))
