(ns layout-descent.facts-test
  (:require [clojure.test :refer [deftest is testing]]
            [layout-descent.facts :as facts]))

(def ^:private found (facts/analyze ["test/fixtures/sample/src"]))

(defn- at [row]
  (first (filter #(= row (:row %)) (:sites found))))

(deftest sites-carry-their-path
  (is (= 1 (:files found)))
  (is (= [] (:unparsed found)))
  (is (= {:fn "get-in" :kind :read :target "db" :in "sample.core/score"
          :path [":scores" "?tenant" "?entity" ":risk"] :shape [":scores" "?" "?" ":risk"] :keyed 2}
         (select-keys (at 6) [:fn :kind :target :in :path :shape :keyed])))
  (testing "a function handed to swap! is a write to the reference"
    (is (= {:fn "assoc-in" :kind :write :via "swap!" :target "state" :shape [":scores" "?" "?" ":risk"]}
           (select-keys (at 12) [:fn :kind :via :target :shape]))))
  (testing "a threaded call has its map supplied by the macro"
    (is (= {:target "<threaded>" :shape [":scores" "?" "?" ":risk"]}
           (select-keys (at 19) [:target :shape]))))
  (testing "a path that is not a literal is reported without a shape"
    (is (nil? (:shape (at 23)))))
  (testing "a scan of a nested access continues the path of that access"
    (is (= [":scores" "*"] (:shape (at 26))))
    (is (= {:fn "reduce-kv" :kind :scan :shape [":scores" "?" "*"]}
           (select-keys (first (filter #(= "reduce-kv" (:fn %)) (:sites found))) [:fn :kind :shape])))))

(deftest shapes-are-grouped-most-keyed-first
  (let [summary (facts/summarize found 2)
        top (first (:shapes summary))]
    (is (= [":scores" "?" "?" ":risk"] (:shape top)))
    (is (= {:reads 2 :writes 2 :scans 0 :site-count 4} (select-keys top [:reads :writes :scans :site-count])))
    (is (= 2 (count (:sites top))) "the limit bounds the sites, not the counts")
    (is (= 1 (:dynamic-count summary)))
    (is (= (:site-count summary) (+ 1 (reduce + (map :site-count (:shapes summary))))))))

(defn- in [function]
  (filter #(= (str "sample.core/" function) (:in %)) (:sites found)))

(deftest chains-use-resolved-core-identities
  (is (= [{:fn "get" :target "db" :path [":scores" "?tenant" "?risk"]}]
         (mapv #(select-keys % [:fn :target :path]) (in "qualified"))))
  (is (= [{:fn "get" :target "db" :path ["?tenant" "?risk"]}]
         (mapv #(select-keys % [:fn :target :path]) (in "aliased"))))
  (is (= [{:fn "get" :target "(get db :transformed)" :path ["?id"]}]
         (mapv #(select-keys % [:fn :target :path]) (in "shadowed")))))

(deftest root-scans-remain-relevant
  (is (= [{:kind :scan :target "db" :shape ["*"]}]
         (mapv #(select-keys % [:kind :target :shape]) (in "root-scan"))))
  (is (empty? (in "constant-key"))))

(deftest threading-uses-the-macro-argument-position
  (doseq [function ["threaded-last" "threaded-bare"]]
    (is (= [{:kind :scan :target "<threaded>" :shape ["*"]}]
           (mapv #(select-keys % [:kind :target :shape]) (in function)))))
  (is (= [{:target "scores" :path ["?<threaded>"]}]
         (mapv #(select-keys % [:target :path]) (in "threaded-last-key")))))

(deftest writes-report-each-key
  (is (= [["assoc" [":scores" "?tenant"]]
          ["assoc" [":scores" "?entity"]]
          ["dissoc" [":scores" "?tenant"]]
          ["dissoc" [":scores" "?entity"]]]
         (mapv (juxt :fn :path) (in "multi-write")))))
