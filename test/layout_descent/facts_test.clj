(ns layout-descent.facts-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
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

(def ^:private grid (facts/analyze ["test/fixtures/sample/cljs"]))

(defn- grid-sites [function]
  (mapv (juxt :kind :path :shape :keyed)
        (filter #(= (str "sample.grid/" function) (:in %)) (:sites grid))))

(deftest a-vector-key-is-one-level
  (testing "each computed part is a computed key of that level"
    (is (= [[:read [":world" "[?x ?y]"] [":world" "[? ?]"] 2]] (grid-sites "tile")))
    (is (= [[:write [":world" "[?x ?y]"] [":world" "[? ?]"] 2]] (grid-sites "place!")))
    (is (= [[:read [":world" "[?x ?y]" ":tile/seen?"] [":world" "[? ?]" ":tile/seen?"] 2]]
           (grid-sites "seen?")))
    (is (= [[:read ["[? ?y]"] ["[? ?]"] 2]] (grid-sites "kind"))))
  (testing "a constant part stays a constant"
    (is (= [[:read ["[?x :ground]"] ["[? :ground]"] 1]] (grid-sites "ground"))))
  (testing "constants only, a symbol that holds the vector and one part stay one computed key"
    (is (= [[:read [":tiles" "?"] [":tiles" "?"] 1]] (grid-sites "origin")))
    (is (= [[:read [":tiles" "?position"] [":tiles" "?"] 1]] (grid-sites "held")))
    (is (= [[:read [":tiles" "?"] [":tiles" "?"] 1]] (grid-sites "single"))))
  (testing "the depth of a shape counts each computed part"
    (is (= [[[":world" "[? ?]"] 2 1 0 0 1]
            [[":world" "[? ?]" ":tile/seen?"] 2 1 0 0 1]
            [["[? ?]"] 2 1 0 0 1]]
           (shown (facts/summarize grid 0))))
    (is (= {:shapes 2 :sites 5 :writes 1} (:hidden (facts/summarize grid 0))))
    (is (= [[[":tiles" "?"] 1 3 0 0 3] [["[? :ground]"] 1 1 0 0 1]]
           (drop 3 (shown (facts/summarize grid 0 {:min-depth 1})))))))

(deftest sources-count-the-files-by-extension
  (is (= {:clj 1 :cljc 0 :cljs 0} (:sources found) (:sources (facts/summarize found 0))))
  (testing "a file with no access counts as a source, not as a file with sites"
    (is (= {:clj 0 :cljc 0 :cljs 2} (:sources grid)))
    (is (= 1 (:files grid))))
  (testing "a .cljc file counts one time"
    (let [shared (facts/analyze ["test/fixtures/sample/cljc"])]
      (is (= {:clj 0 :cljc 1 :cljs 0} (:sources shared)))
      (is (= 1 (:files shared) (count (:sites shared))))
      (is (= [] (:warnings (facts/summarize shared 0))))))
  (testing "the counts of several paths add up"
    (let [all (facts/analyze ["test/fixtures/sample/src" "test/fixtures/sample/scans"
                              "test/fixtures/sample/cljc" "test/fixtures/sample/cljs"])]
      (is (= {:clj 2 :cljc 1 :cljs 2} (:sources all)))
      (is (= 4 (:files all))))))

(deftest clojurescript-sources-are-a-warning
  (is (= [] (:warnings (facts/summarize found 0))))
  (let [[warning :as warnings] (:warnings (facts/summarize grid 0))]
    (is (= [:clojurescript-sources] (mapv :code warnings)))
    (is (= #{:code :message} (set (keys warning))))
    (is (every? #(str/includes? (:message warning) %)
                [".clj 0, .cljc 0, .cljs 2" "for the JVM" "JavaScript runtime" "does not load"])))
  (testing "one .cljs file among others is enough"
    (let [mixed (facts/summarize (facts/analyze ["test/fixtures/sample/src" "test/fixtures/sample/cljs"]) 0)]
      (is (= [:clojurescript-sources] (mapv :code (:warnings mixed))))
      (is (str/includes? (:message (first (:warnings mixed))) ".clj 1, .cljc 0, .cljs 2")))))

(deftest a-hint-says-when-every-shape-is-hidden
  (testing "no hint while a shape is shown"
    (is (not (contains? (facts/summarize found 0) :hint)))
    (is (not (contains? (facts/summarize grid 0) :hint))))
  (testing "no hint when there is no shape at all"
    (let [none (facts/summarize (assoc found :sites []) 0)]
      (is (= {:shapes 0 :sites 0 :writes 0} (:hidden none)))
      (is (not (contains? none :hint)))))
  (testing "every shape hidden"
    (let [summary (facts/summarize found 0 {:min-depth 9})]
      (is (= [] (:shapes summary)))
      (is (pos? (long (:shapes (:hidden summary)))))
      (is (every? #(str/includes? (:hint summary) %) ["Every shape is hidden" "min-depth 1" "writes true"]))
      (is (= (dec (count (:hint summary))) (str/index-of (:hint summary) ".")) "one sentence")))
  (testing "the hint does not name options that the call has already"
    (let [constant (assoc (first (:sites found)) :shape [":a" ":b"] :path [":a" ":b"] :keyed 0)
          summary (facts/summarize (assoc found :sites [constant]) 0 {:min-depth 1 :writes true})]
      (is (= [] (:shapes summary)))
      (is (str/includes? (:hint summary) "min-depth 0"))
      (is (not (str/includes? (:hint summary) "writes true")))
      (is (= (:hint summary)
             (:hint (facts/summarize (assoc found :sites [constant]) 0 {:min-depth 1 :writes false})))
          "no write site is hidden, so writes true would show nothing")
      (is (= "Every shape is hidden: writes true shows the write sites."
             (:hint (facts/summarize (assoc found :sites [constant (assoc constant :kind :write :row -1)]) 0
                                     {:min-depth 1 :writes false})))))))
