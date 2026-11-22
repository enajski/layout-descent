(ns layout-descent.flow-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [layout-descent.cli :as cli]
            [layout-descent.flow :as flow])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(def root "test/fixtures/flow")

(defn- report [op]
  (flow/analyze root (flow/source-files root) (str "shop.order/" op)))

(defn- fields [result key]
  (filter #(= key (:field %)) (:fields result)))

(defn- uses [result key kind]
  (map (:steps result) (mapcat #(get-in % [:uses kind]) (fields result key))))

(deftest follow-fields-across-calls
  (let [r (report "settle") steps (:steps r)
        amount (first (fields r ":amount"))]
    (is (= :static (:scope r)))
    (is (not (:truncated r)))
    (is (empty? (:unparsed r)))
    (is (every? (fn [{:keys [id row col inputs]}]
                  (and (pos? row) (pos? col) (every? #(< -1 % id) inputs))) steps))
    (is (= ["shop/catalog.cljc"] (map #(get-in steps [(:at %) :file]) (fields r ":sale-price"))))
    (is (= [":sale-price" ":receipt-price"] (map :field (uses r ":price" :carry))))
    (is (= ["clojure.core/*"] (map :via (uses r ":sale-price" :consume))))
    (is (= :produce (:kind (steps (:at amount)))))
    (is (= ["clojure.core/*"] (map (comp :via steps) (:inputs (steps (:at amount))))))
    (is (empty? (uses r ":currency" :consume)))
    (is (empty? (fields r ":inventory-price")))
    (is (= #{:dynamic-key :indirect-call} (set (map :reason (:boundaries r)))))
    (is (= ["sink"] (map :via (uses r ":amount" :boundary))))))

(deftest do-not-unify-by-key-or-by-container
  (let [r (report "alternatives") xs (fields r ":x") choice (first (filter :control (:steps r)))]
    (is (= 2 (count (set (map :at xs)))))
    (is (= "condition" (get-in r [:steps (:control choice) :via]))))
  (let [r (report "only-one")]
    (is (= ["clojure.core/+"] (map :via (uses r ":from-hot" :consume))))
    (is (every? empty? [(uses r ":from-cold" :consume) (uses r ":other" :consume) (fields r ":absent")]))
    (is (some #(and (= ":absent" (:field %)) (= :known-miss (:resolution %))
                    (empty? (:inputs %)) (number? (:container %))) (:steps r)))))

(deftest finite-operation-slices
  (let [r (report "pipeline")]
    (is (empty? (:boundaries r)))
    (is (= ["clojure.core/*"] (map :via (uses r ":unit" :consume))))
    (is (= 2 (count (filter #(= :iterate (:kind %)) (:steps r)))))
    (is (= [:known-slot] (map :resolution (uses r ":amount" :read)))))
  (let [r (report "increment-line")]
    (is (empty? (:boundaries r)))
    (is (= ["clojure.core/inc"] (map :via (uses r ":count" :consume))))
    (is (= [:carry :produce] (map #(get-in r [:steps (:at %) :kind]) (fields r ":qty")))))
  (let [r (report "cycle")]
    (is (= [:recursive-call] (map :reason (:boundaries r))))))

(deftest cljs-branches-and-movement
  (let [files (flow/source-files root)
        r (flow/analyze root files "board.game/act")
        steps (:steps r)
        paths (fn [kind] (set (map :path (filter #(= kind (:kind %)) steps))))
        key (first (filter #(and (= :read (:access %)) (= [":world" "[? ?]"] (:path %))) steps))
        carries (fn [field] (set (map :field (uses r field :carry))))]
    (is (= :cljs (:language r)))
    (is (and (empty? (:unparsed r)) (not (:truncated r))))
    (is (= #{"board/game.cljs" "board/model.cljc"} (set (map :file steps))))
    (is (empty? (fields r ":desktop")))
    (is (= #{":body"} (carries ":browser")))
    (is (= #{":actor"} (carries ":id")))
    (is (= [0 1] (map :index (filter #(= :project (:kind %)) steps))))
    (is (= :dynamic-key (:reason key)))
    (is (= ["vector" 2] (when-let [k (get steps (:key-input key))] [(:via k) (count (:inputs k))])))
    (is (contains? (paths :read) [":world" "[? ?]" ":tile/kind"]))
    (is (contains? (paths :produce) [":entities" "?" ":position"]))
    (is (contains? (paths :produce) [":world" "[? ?]" ":tile/ids"]))
    (is (= ["cljs.core/deref" "cljs.core/swap!"] (map :via (filter #(= :state-read (:kind %)) steps))))
    (is (= ["cljs.core/swap!"] (map :via (filter #(= :state-write (:kind %)) steps))))
    (is (= #{:external-value :dynamic-key} (set (map :reason (:boundaries r)))))
    (testing "select exactly one side of shared code, preserving the old JVM default on mixed trees"
      (doseq [[lang field] [[nil ":desktop"] [:clj ":desktop"] [:cljs ":browser"]]]
        (let [r (flow/analyze root files "board.model/payload" lang)]
          (is (= (or lang :clj) (:language r)))
          (is (= [field] (map :field (:fields r)))))))))

(deftest unknown-addresses-are-not-aliases
  (let [r (flow/analyze root (flow/source-files root) "board.game/reread")
        flag (filter #(= ":flag" (:field %)) (:steps r))]
    (is (= [:produce :read] (map :kind flag)))
    (is (= :open (:resolution (last flag))))
    (is (not-any? #{(:id (first flag))} (:inputs (last flag)))))
  (let [r (flow/analyze root (flow/source-files root) "board.game/separate-snapshots")
        steps (:steps r)
        [swap-read deref-read] (filter #(= :state-read (:kind %)) steps)
        write (first (filter #(= :state-write (:kind %)) steps))
        read (last (filter #(= ":value" (:field %)) steps))]
    (is (< (:id swap-read) (:id write) (:id deref-read)))
    (is (= (:inputs swap-read) (:inputs deref-read)))
    (is (= [:open [(:id deref-read)]] [(:resolution read) (:inputs read)]))))

(deftest surface-and-source-protection
  (let [dir (.toFile (Files/createTempDirectory "flow-input" (make-array FileAttribute 0)))
        file (io/file dir "sample.clj") cljs (io/file dir "front.cljs") bad (io/file dir "broken.clj")
        source "(ns sample) (defn entry [x] (:paid? x))"
        run (fn [& args] (cli/run (into ["flow" (str dir)] args)))]
    (try
      (spit file source)
      (spit bad "(ns broken")
      (spit cljs "(ns front) (defn pick [x] (:seen x))")
      (is (= "cljs" (get-in (run "--operation" "front/pick" "--lang" "cljs") [:answer "language"])))
      (is (= "flow_operation" (get-in (run "--operation" "front/pick" "--lang" "clj") [:answer "error" "code"])))
      (is (= "usage" (get-in (run "--operation" "front/pick" "--lang" "both") [:answer "error" "code"])))
      (is (= "path_collision" (get-in (run "--operation" "front/pick" "--save" (str cljs)) [:answer "error" "code"])))
      (is (= "(ns front) (defn pick [x] (:seen x))" (slurp cljs)))
      (is (= 2 (:exit (run))))
      (is (= "flow_operation" (get-in (run "--operation" "x/missing") [:answer "error" "code"])))
      (is (= ["broken.clj"] (get-in (run "--operation" "sample/entry") [:answer "unparsed"])))
      (is (= "path_collision" (get-in (run "--operation" "sample/entry" "--save" (str file))
                                       [:answer "error" "code"])))
      (is (= source (slurp file)))
      (finally (io/delete-file cljs) (io/delete-file bad) (io/delete-file file) (io/delete-file dir)))))

(defn- form [op]
  (flow/analyze "test/fixtures/forms" (flow/source-files "test/fixtures/forms") (str "desk.forms/" op)))

(defn- vias [result key kind] (map :via (uses result key kind)))

(defn- reasons [result] (map :reason (:boundaries result)))

(deftest branches-bind-and-choose
  (let [r (form "guarded")]
    (is (empty? (:boundaries r)))
    (is (= ["condition"] (vias r ":line" :consume)))
    (is (= [":qty"] (map :field (uses r ":line" :read))))
    (is (= [["clojure.core/if-let"] ["clojure.core/if-let"]] [(vias r ":qty" :choice) (vias r ":fallback" :choice)])))
  (let [r (form "routed")]
    (is (empty? (:boundaries r)))
    (is (= [["condition"] ["condition"]] [(vias r ":urgent?" :consume) (vias r ":stale?" :consume)]))
    (is (every? #(= ["clojure.core/cond"] (vias r % :choice)) [":owner" ":queue" ":id"])))
  (let [r (form "enriched")]
    (is (empty? (:boundaries r)))
    (is (= [":owner"] (map :field (uses r ":name" :carry))))
    (is (= ["clojure.core/cond->"] (vias r ":owner" :choice))))
  (let [r (form "safe")]
    (is (= [:caught :opaque-call] (reasons r)))
    (is (= ["try"] (vias r ":body" :choice)))
    (is (= 1 (count (fields r ":trace"))) "the finally body is read and not returned")
    (is (empty? (vias r ":trace" :return)))))

(deftest collections-keep-their-elements
  (let [r (form "totals")]
    (is (empty? (:boundaries r)))
    (is (= ["clojure.core/+"] (vias r ":amount" :consume))))
  (let [r (form "open-ids")]
    (is (empty? (:boundaries r)))
    (is (= ["condition"] (vias r ":open?" :consume)))
    (is (= ["clojure.core/for"] (vias r ":id" :collect))))
  (let [r (form "paid")]
    (is (empty? (:boundaries r)))
    (is (= ["clojure.core/filter"] (vias r ":paid?" :consume)))
    (is (= ["clojure.core/map"] (vias r ":total" :collect))))
  (let [r (form "head") pick (first (filter #(= :project (:kind %)) (:steps r)))]
    (is (empty? (:boundaries r)))
    (is (= [:open nil] ((juxt :resolution :index) pick)))
    (is (= ["desk.forms/head"] (vias r ":id" :return))))
  (let [r (form "summary")]
    (is (empty? (:boundaries r)))
    (is (= [:read :carry :read :carry] (map #(get-in r [:steps (:at %) :kind]) (:fields r))))
    (is (= #{"clojure.core/select-keys"} (set (vias r ":total" :assemble)))))
  (let [r (form "merged")]
    (is (empty? (:boundaries r)))
    (is (= ["desk.forms/merged"] (vias r ":x" :return))))
  (let [r (form "configured")]
    (is (empty? (:boundaries r)) "a literal def is a known value")
    (is (= [:known-slot] (map :resolution (uses r ":mode" :read))))
    (is (empty? (uses r ":retries" :read))))
  (is (= [:unsupported-macro] (reasons (form "looped")))))

(deftest qualified-map-keys-bind-unqualified-locals
  (let [r (form "qualified")]
    (is (empty? (:boundaries r)))
    (is (= #{[":slot/id"] [":slot/coord"] [":slot/kind"]}
           (set (map :path (filter #(= :read (:kind %)) (:steps r))))))
    (is (= [":id"] (map :field (uses r ":slot/id" :carry))))
    (is (= [":coord"] (map :field (uses r ":slot/coord" :carry))))))

(deftest binding-values-are-local-to-the-scoped-body
  (let [r (form "scoped-binding")
        reads (filter #(= :read (:kind %)) (:steps r))
        resolved (set (map (juxt :field :resolution :in) reads))]
    (is (= [:unmodeled-dynamic-binding] (reasons r)))
    (is (= #{[":inner" :known-slot "desk.forms/scoped-binding"]
             [":outer" :known-slot "desk.forms/scoped-binding"]
             [":outer" :open "desk.forms/read-bound-option"]
             [":local" :known-slot "desk.forms/scoped-binding"]
             [":root" :known-slot "desk.forms/scoped-binding"]}
           resolved))))

(deftest batch-and-cutoff
  (let [run (fn [& flags] (:answer (cli/run (into ["flow" root] flags))))
        batch (run "--operation" "shop.order/settle" "--operation" "missing/entry"
                   "--operation" "shop.order/pipeline")
        [settle missing pipeline] (get batch "traces")
        cut (run "--operation" "shop.order/settle" "--max-steps" "3")]
    (is (= [true false true] (map #(get % "ok") [settle missing pipeline])))
    (is (= "flow_operation" (get-in missing ["error" "code"])))
    (is (= (get settle "steps") (get (run "--operation" "shop.order/settle") "steps")))
    (is (= #{"dynamic-key" "indirect-call"} (set (map #(get % "reason") (get settle "boundaries")))))
    (is (= [21 22] (get settle "boundary_steps")))
    (is (= [true 3] [(get cut "truncated") (count (get cut "steps"))]))
    (is (string? (get-in cut ["cutoff" "in"])))
    (is (= "flow_operation" (get-in (run "--operation" "shop.order/settle" "--exclude" "shop/**")
                                     ["error" "code"])))
    (is (= "usage" (get-in (run "--operation" "shop.order/settle" "--exclude" "[") ["error" "code"])))
    (is (string? (get-in batch ["provenance" 0 "path"])))))

(deftest collection-and-string-coverage
  (let [root "test/fixtures/forms"
        files (flow/source-files root)
        report #(flow/analyze root files (str "desk.collections/" %))
        scan (report "scan")]
    (is (seq (uses scan ":active?" :consume)))
    (is (seq (uses scan ":amount" :consume)))
    (is (seq (uses scan ":value" :collect)))
    (is (empty? (:boundaries (report "labels"))))
    (is (empty? (:boundaries (report "remove-private"))))
    (is (= [:unsupported-transducer] (reasons (report "opaque-transducer"))))
    (is (re-find #"def or defonce holder"
                 (try (report "holder") ""
                      (catch clojure.lang.ExceptionInfo error (ex-message error)))))))
