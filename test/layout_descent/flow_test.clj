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
    (is (= #{:dynamic-key :indirect-call} (set (map (comp :reason steps) (:boundaries r)))))
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
    (is (= [:recursive-call] (map (comp :reason (:steps r)) (:boundaries r))))))

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
    (is (= #{:external-value :dynamic-key} (set (map (comp :reason steps) (:boundaries r)))))
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
