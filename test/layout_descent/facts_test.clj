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
    (is (= {:fn "reduce-kv" :kind :scan :shape [":scores" "?" "*" ":risk"]}
           (select-keys (first (filter #(= "reduce-kv" (:fn %)) (:sites found))) [:fn :kind :shape])))))

(deftest shapes-are-grouped-most-keyed-first
  (let [summary (facts/summarize found 2 {:min-depth 0 :writes true})
        top (first (:shapes summary))]
    (is (= [":scores" "?" "?" ":risk"] (:shape top)))
    (is (= {:reads 3 :writes 2 :scans 0 :site-count 5} (select-keys top [:reads :writes :scans :site-count])))
    (is (= 2 (count (:sites top))) "the limit bounds the sites, not the counts")
    (is (= 1 (:dynamic-count summary)))
    (is (= {:shapes 0 :sites 0 :writes 0} (:hidden summary)))
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

(def ^:private scans (facts/analyze ["test/fixtures/sample/scans"]))

(defn- scan-sites [function]
  (mapv (juxt :fn :kind :target :path)
        (filter #(= (str "sample.scans/" function) (:in %)) (:sites scans))))

(deftest a-scan-continues-into-its-function
  (testing "a read of the scanned value is one site on the scanned map"
    (is (= [["reduce-kv" :scan "rows" ["*" "?col"]]] (scan-sites "col-sum")))
    (is (= [{:file "sample/scans.clj" :row 8 :shape ["*" "?"] :keyed 1}]
           (mapv #(select-keys % [:file :row :shape :keyed])
                 (filter #(= "sample.scans/col-sum" (:in %)) (:sites scans))))))
  (testing "a scan of the scanned value nests"
    (is (= [["reduce-kv" :scan "rows" ["*" "*"]]] (scan-sites "grand-total"))))
  (testing "#(), a type hint and a function name bind the value the same way"
    (is (= [["reduce-kv" :scan "rows" ["*" "?col"]]] (scan-sites "short-col-sum")))
    (is (= [["reduce-kv" :scan "rows" ["*" "?col"]]] (scan-sites "hinted")))
    (is (= [["reduce-kv" :scan "db" [":scores" "?tenant" "*" ":features" "?feature"]]]
           (scan-sites "deep"))))
  (testing "each read of the value is a path of the scan"
    (is (= [["reduce-kv" :scan "rows" ["*" "?a"]] ["reduce-kv" :scan "rows" ["*" "?b"]]]
           (scan-sites "two-reads")))))

(deftest a-scan-stays-apart-when-the-value-is-not-the-root
  (doseq [[function target] [["shadowed" "row"] ["destructured" "weights"] ["rebound" "%3#"]]]
    (testing function
      (is (= [["reduce-kv" :scan "rows" ["*"]] ["get" :read target ["?col"]]]
             (scan-sites function)))))
  (testing "the key parameter and another map"
    (is (= [["reduce-kv" :scan "rows" ["*"]] ["get" :read "other" ["?k"]]] (scan-sites "key-read"))))
  (testing "a write to the value"
    (is (= [["reduce-kv" :scan "rows" ["*"]] ["assoc" :write "out" ["?k"]] ["dissoc" :write "row" ["?col"]]]
           (scan-sites "rebuilt")))))

(defn- absolute [path]
  (.getCanonicalPath (java.io.File. ^String path)))

(deftest files-are-relative-to-the-common-root
  (is (= (absolute "test/fixtures/sample/src") (:root found)))
  (is (= #{"sample/core.clj"} (set (map :file (:sites found)))))
  (let [both (facts/analyze ["test/fixtures/sample/src" "test/fixtures/sample/scans"])]
    (is (= (absolute "test/fixtures/sample") (:root both)))
    (is (= #{"src/sample/core.clj" "scans/sample/scans.clj"} (set (map :file (:sites both)))))
    (is (= (+ (count (:sites found)) (count (:sites scans))) (count (:sites both)))))
  (testing "a file is under its own directory"
    (let [one (facts/analyze ["test/fixtures/sample/src/sample/core.clj"])]
      (is (= (absolute "test/fixtures/sample/src/sample") (:root one)))
      (is (= #{"core.clj"} (set (map :file (:sites one))))))))

(defn- shown [summary]
  (mapv (juxt :shape :depth :reads :writes :scans :site-count) (:shapes summary)))

(deftest the-summary-hides-shallow-shapes-and-writes
  (let [summary (facts/summarize found 5)
        all (facts/summarize found 5 {:min-depth 0 :writes true})]
    (is (= [[[":scores" "?" "?" ":risk"] 2 3 0 0 3]
            [[":scores" "?" "?"] 2 2 0 0 2]
            [["?" "?"] 2 1 0 0 1]
            [[":scores" "?" "*" ":risk"] 2 0 0 1 1]]
           (shown summary)))
    (is (= (:root found) (:root summary)))
    (is (= (count (:sites found)) (:site-count summary) (:site-count all)))
    (testing "hidden counts the shapes, their sites with the writes, and the writes"
      (is (= {:shapes 4 :sites 12 :writes 6} (:hidden summary)))
      (is (= (:site-count summary)
             (+ (long (:dynamic-count summary))
                (long (reduce + (map :site-count (:shapes summary))))
                (long (:sites (:hidden summary)))))))
    (testing "a shape that only writes is hidden whole"
      (is (some #{[":scores" "?"]} (map :shape (:shapes all))))
      (is (= [[[":scores" "?" "?" ":risk"] 2 3 2 0 5]]
             (take 1 (shown (facts/summarize found 5 {:writes true}))))))
    (testing ":min-depth alone keeps the writes hidden"
      (let [shallow (facts/summarize found 5 {:min-depth 1})]
        (is (= {:shapes 1 :sites 6 :writes 6} (:hidden shallow)))
        (is (some #{["*"]} (map :shape (:shapes shallow))))
        (is (not-any? #{[":scores" "?"]} (map :shape (:shapes shallow))))))
    (testing "a scan that goes on into a read counts both levels"
      (is (= [[[":scores" "?" "*" ":features" "?"] 3 0 0 1 1] [["*" "?"] 2 0 0 5 5] [["*" "*"] 2 0 0 1 1]]
             (shown (facts/summarize scans 0)))))))

(deftest limit-zero-gives-counts-only
  (let [summary (facts/summarize found 0)]
    (is (= 4 (count (:shapes summary))))
    (is (every? #(= [] (:sites %)) (:shapes summary)))
    (is (every? #(pos? (long (:site-count %))) (:shapes summary)))
    (is (= 1 (:dynamic-count summary)))
    (is (= [] (:dynamic summary)))))
