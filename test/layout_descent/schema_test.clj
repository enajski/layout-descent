(ns layout-descent.schema-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [layout-descent.cli :as cli]
            [layout-descent.flow :as flow]
            [layout-descent.schema :as schema])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- model [root opts]
  (schema/analyze root (flow/source-files root) nil (merge {:limit 30 :sites 3} opts)))

(defn- entity [answer name]
  (first (filter #(= name (:name %)) (:entities answer))))

(defn- counts [entity]
  (into {} (map (juxt :key (juxt :reads :writes :scans :misses))) (:keys entity)))

(deftest group-records-by-shared-keys
  (is (= [{:keys #{:a :b :c :d :e} :records [1 2]} {:keys #{:x :y} :records [3]}]
         (schema/group [{:id 2 :keys #{:a :b :d}} {:id 4 :keys #{:a}} {:id 3 :keys #{:x :y}}
                        {:id 1 :keys #{:a :b :c :e}}])))
  (is (= [[1] [2]] (map :records (schema/group [{:id 1 :keys #{:a :b :c :d :e}} {:id 2 :keys #{:a :b :x :y}}])))
      "two shared keys of four are under 60 %")
  (is (= [[1] [2 3]] (map :records (schema/group [{:id 1 :keys #{:a :b :c :d}} {:id 2 :keys #{:p :q :r}}
                                                   {:id 3 :keys #{:p :q}}])))))

(deftest entities-and-links-of-a-tree
  (let [m (model "test/fixtures/flow" {})
        names (map :name (:entities m))]
    (is (= ["shop.catalog/capture" "shop.order/only-one" "cold" "listing" "shop.order/settle"] names))
    (is (= {:entities 0 :loose-records 11} (:hidden m)))
    (is (and (empty? (:truncated m)) (empty? (:untraced m)) (empty? (:unparsed m))))
    (is (= #{["listing" "shop.catalog/capture" 2] ["listing" "shop.order/settle" 1] ["cold" "shop.order/only-one" 2]}
           (set (for [{:keys [kind from to fields]} (:links m) :when (= :carries kind)
                      :let [by-id (into {} (map (juxt :id :name)) (:entities m))]]
                  [(by-id from) (by-id to) fields]))))
    (is (some #(= {:from [":price"] :to [":sale-price"]} %) (mapcat :examples (:links m))))
    (is (= 1 (get-in (counts (entity m "shop.order/only-one")) [":absent" 3])))
    (is (= ["e1"] (map :id (:entities (model "test/fixtures/flow" {:limit 1})))))
    (is (= {:entities 4 :loose-records 11} (:hidden (model "test/fixtures/flow" {:limit 1}))))
    (is (empty? (:links (model "test/fixtures/flow" {:limit 1}))) "a link needs both entities shown")))

(deftest records-of-one-entity
  (let [m (model "test/fixtures/forms" {:entity "e1"})
        order (first (:entities m))]
    (is (= ["order (:id)" 2 {:param 1 :built 1}] ((juxt :name :records :kinds) order)))
    (is (= {":id" [1 1 0 0] ":total" [1 1 0 0]} (counts order)))
    (is (= #{:param :built} (set (map :kind (:records m)))))
    (is (every? #(re-matches #"r\d+" (:id %)) (:records m)))
    (is (= #{[":id"] [":total"]} (set (mapcat #(map :path (:fields %)) (:records m)))))
    (is (= #{[:carries [":id"] [":id"]] [:carries [":total"] [":total"]]}
           (set (map (juxt :kind (comp :path :from) (comp :path :to)) (:record-links m)))))
    (is (every? #(= "e1" (get-in % [:from :entity]) (get-in % [:to :entity])) (:record-links m))))
  (let [m (model "test/fixtures/forms" {})]
    (is (= [["*"]] (map :path (:levels (entity m "orders")))) "a scanned collection is a level")
    (is (= {":mode" [1 1 0 0] ":retries" [0 1 0 0]} (counts (entity m "desk.forms/defaults"))))
    (is (= #{:caught :opaque-call :unsupported-macro :unmodeled-dynamic-binding
              :indirect-call :unsupported-transducer}
           (set (map :reason (:boundaries m)))))
    (is (not (contains? m :records))))
  (is (= 9 (count (:records (model "test/fixtures/forms" {:entity "loose"})))))
  (is (= :schema-entity (try (model "test/fixtures/forms" {:entity "e99"})
                             (catch clojure.lang.ExceptionInfo e (:code (ex-data e)))))))

(deftest reusable-local-summaries
  (let [m (model "test/fixtures/schema-summary" {})
        ks (fn [e] (into {} (map (juxt :key :shapes) (:keys e))))
        result (some #(when (contains? (ks %) ":policy-value") (ks %)) (:entities m))
        payload (entity m "payload")]
    (is (= 7 (get-in m [:summaries :bodies])))
    (is (some #(and (= ":secret" (:key %)) (pos? (:misses %))) (mapcat :keys (:entities m))))
    (is (pos? (get-in m [:summaries :reuses])))
    (is (empty? (:truncated m)))
    (is (= [:map] (result ":policy-value")))
    (is (= [:vector] (result ":obligation-value")))
    (is (= :holding-field (:name-source payload)))
    (is (= :vector (first ((ks payload) ":required-evidence"))))
    (let [contrast (first (:shape-contrasts m))]
      (is (= #{[:map] [:vector]} (set (map :shapes (:entities contrast)))))
      (is (= #{[:map "contracts/rule" :write] [:vector "contracts/obligation" :write]}
             (set (for [e (:entities contrast) s (:examples e)]
                    ((juxt :shape :in :access) s)))))
      (is (every? #(and (= "contracts.clj" (:file %)) (= [":required-evidence"] (:path %))
                        (pos? (:row %)) (pos? (:col %)))
                  (mapcat :examples (:entities contrast)))))))

(deftest mermaid-chart
  (let [chart (schema/mermaid
               {:entities [{:id "e1" :name "a \"<b>\" & #1" :records 1 :keys (for [i (range 14)] {:key (str ":k" i)})}
                           {:id "e2" :name "plan" :records 3 :keys [{:key ":id"}]}]
                :links [{:kind :carries :from "e1" :to "e2" :fields 1 :examples [{:from [":k0"] :to [":id"]}]}
                        {:kind :carries :from "e2" :to "e1" :fields 2 :examples []}
                        {:kind :holds :from "e2" :to "e1" :fields 1 :examples [{:from [] :to [":k1"]}]}]})
        rows (str/split-lines chart)]
    (is (= "flowchart LR" (first rows)))
    (is (str/includes? (rows 1) "<b>a #quot;#lt;b#gt;#quot; #amp; #35;1</b> (1 record)"))
    (is (str/includes? (rows 1) ":k11<br/>+ 2 more"))
    (is (not (str/includes? (rows 1) ":k12")))
    (is (= "  e2[\"<b>plan</b> (3 records)<br/>:id\"]" (rows 2)))
    (is (= ["  e1 -->|\"1 field\"| e2" "  e2 -->|\"2 fields\"| e1" "  e2 -.->|\"in :k1\"| e1"] (subvec rows 3)))))

(deftest surface-and-source-protection
  (let [dir (.toFile (Files/createTempDirectory "schema-input" (make-array FileAttribute 0)))
        out (.toFile (Files/createTempDirectory "schema-output" (make-array FileAttribute 0)))
        file (io/file dir "sample.clj") md (io/file out "model.md") mmd (io/file out "model.mmd")
        source "(ns sample) (defn entry [x] {:id (:id x) :paid? (:paid? x)})"
        run (fn [& args] (cli/run (into ["schema" (str dir)] args)))
        code (fn [& args] (get-in (apply run args) [:answer "error" "code"]))]
    (try
      (spit file source)
      (is (= [1 1] ((juxt #(get % "functions") #(count (get % "entities"))) (:answer (run)))))
      (is (= "usage" (code "--lang" "both")))
      (is (= "usage" (code "--diagram" (str (io/file out "model.png")))))
      (is (= "usage" (code "--limit" "0")))
      (is (= 2 (:exit (cli/run ["schema"]))))
      (is (= "schema_entity" (code "--entity" "e9")))
      (is (= "path_collision" (code "--save" (str file))))
      (is (= "path_collision" (code "--diagram" (str md) "--save" (str md))))
      (is (= source (slurp file)))
      (is (not (.exists md)))
      (is (= (.getCanonicalPath md) (.getCanonicalPath (io/file (get-in (run "--diagram" (str md)) [:answer "diagram"])))))
      (is (re-find #"(?s)^# Data model of .*```mermaid\nflowchart LR\n.*\n```\n$" (slurp md)))
      (run "--diagram" (str mmd))
      (is (str/starts-with? (slurp mmd) "flowchart LR\n  e1["))
      (is (= 2 (count (get-in (run "--entity" "e1") [:answer "records"]))))
      (finally (doseq [f [md mmd out file dir]] (io/delete-file f true))))))

(deftest entity-search-preserves-ids
  (let [root "test/fixtures/flow"
        all (model root {})
        result (model root {:search "LiSTiNG" :limit 1})]
    (is (= [(entity all "listing")] (:entities result)))
    (is (= 4 (get-in result [:hidden :entities])))
    (is (empty? (:links result)))
    (is (empty? (:entities (model root {:search "not an entity"}))))))
