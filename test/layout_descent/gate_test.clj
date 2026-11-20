(ns layout-descent.gate-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [layout-descent.emit :as emit]
            [layout-descent.fixtures :as fx]
            [layout-descent.gate :as gate]
            [layout-descent.model :as model]
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
        text (emit/source plan {:ns 'layout-descent.gate-probe})]
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

(deftest the-descriptor-is-literal-and-has-only-layout-fields
  (let [plan (fx/normal (assoc-in fx/small-risk [:orders 0 :notes] "not a runtime field"))
        descriptor (->> (emit/source plan)
                        (re-find #"(?s)\(def descriptor\s+('.*?)\)\n\n")
                        second
                        read-string)]
    (is (= '(quote {:types [:keyword :long :keyword]
                    :leaf :double
                    :orders [{:perm [0 1 2] :format :hash}]})
           descriptor))))

(deftest the-gate-compiles-the-delivered-namespace
  (let [ns-sym 'my.app.layout
        configured (fx/normal (assoc fx/small-risk :emit {:ns ns-sym :path "layout.clj"}))
        result (gate/verify configured)]
    (is (:ok result) (:warnings result))
    (is (= ns-sym (:ns result)))
    (is (nil? (find-ns ns-sym)))
    (testing "the gate does not modify a namespace already in use"
      (let [existing (find-ns 'layout-descent.rt)
            result (gate/verify (assoc-in configured [:emit :ns] (ns-name existing)))]
        (is (false? (:ok result)))
        (is (identical? existing (find-ns 'layout-descent.rt)))))))

(deftest long-key-queries-have-map-semantics
  (doseq [format [:hash :csf]
          perm [[0 1 2] [1 0 2]]]
    (let [plan (fx/normal (assoc (task :long) :orders [{:perm perm :format format}]))
          data {:a {1 {"x" 10} Long/MIN_VALUE {"x" 20} Long/MAX_VALUE {"x" 30}}}
          layout (gate/load-layout plan)
          ds ((:build layout) data)
          pattern (first (:patterns plan))
          point (get-in layout [:fns :point])]
      (doseq [key [nil "1" 1.0 1.75 -1 1 (int 1) 1N (biginteger 1)
                   Long/MIN_VALUE (bigint Long/MIN_VALUE) (dec' Long/MIN_VALUE)
                   Long/MAX_VALUE (biginteger Long/MAX_VALUE) (inc' Long/MAX_VALUE)]]
        (is (= (model/run plan pattern data [:a key "x"])
               (point ds :a key "x"))
            (pr-str format perm key))))))

(deftest custom-scans-are-unordered-and-can-stop
  (doseq [format [:hash :csf]
          perm [[0 1 2] [2 1 0]]]
    (let [plan (fx/normal (assoc (task :long) :orders [{:perm perm :format format}]))
          data (array-map :b (array-map 2 (array-map "y" 5 "x" 7) 1 {"x" 11})
                          :a {3 {"z" 13}})
          layout (gate/load-layout plan)
          ds ((:build layout) data)
          scan (get-in layout [:fns :everything])
          pattern (some #(when (= :everything (:id %)) %) (:patterns plan))
          sum (fn [acc _ _ _ leaf] (+ acc leaf))
          calls (atom 0)]
      (is (str/includes? (:returns (emit/signature plan pattern)) "order-independent"))
      (is (= (model/run plan pattern data [] sum 0) (scan ds sum 0) 36))
      (is (= :empty (scan ((:build layout) {}) (constantly (reduced :done)) :empty)))
      (is (nil? (scan ds (fn [& _] (swap! calls inc) (reduced nil)) :empty)))
      (is (= 1 @calls))
      (let [nested (reduced :inner)]
        (is (identical? nested (scan ds (constantly (reduced nested)) nil)))))))

(defn- check-csf-sum [leaf-type data]
  (let [id :grand-total
        plan (fx/normal {:layout/version 1 :name 'numeric-probe
                         :levels [{:id :key :type :long}]
                         :leaf {:type leaf-type}
                         :patterns [{:id id :bind [:scan] :fold :sum}]
                         :orders [{:perm [0] :format :csf}]})
        layout (gate/load-layout plan)]
    (:mismatches (gate/check-built plan layout :numeric data ((:build layout) data) {id [[]]}))))

(deftest adversarial-sums-expose-traversal-order
  (testing "double cancellation changes the result"
    (let [[mismatch] (check-csf-sum :double (array-map 0 1.0e16 2 -1.0e16 1 1.0))]
      (is (= :grand-total (:pattern mismatch)))
      (is (= ["Double" 1.0] (some-> mismatch :expected read-string)))
      (is (= ["Double" 0.0] (some-> mismatch :actual read-string)))))
  (testing "CSF can avoid a checked overflow in the model"
    (let [[mismatch] (check-csf-sum :long (array-map 0 Long/MAX_VALUE 2 1 1 -1))]
      (is (= :grand-total (:pattern mismatch)))
      (is (= :threw (some-> mismatch :expected read-string first)))
      (is (= ["Long" Long/MAX_VALUE] (some-> mismatch :actual read-string)))))
  (testing "CSF can introduce a checked overflow absent in the model"
    (let [[mismatch] (check-csf-sum :long (array-map 0 Long/MAX_VALUE 2 -1 1 1))]
      (is (= :grand-total (:pattern mismatch)))
      (is (= ["Long" Long/MAX_VALUE] (some-> mismatch :expected read-string)))
      (is (= :threw (some-> mismatch :actual read-string first)))))
  (testing "matching overflows are not disagreements"
    (is (empty? (check-csf-sum :long (array-map 0 Long/MAX_VALUE 1 1))))))

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
