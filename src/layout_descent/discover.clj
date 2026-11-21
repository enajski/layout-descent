(ns layout-descent.discover
  "Find evidence for filtered membership views; never infer runtime heat or safety."
  (:require [layout-descent.flow :as flow]))

(defn- core-op [index file token]
  (let [{:keys [row col]} (meta token)
        [resolved] (get (:calls index) [file row col])]
    (when (and resolved (#{"clojure.core" "cljs.core"} (namespace resolved)))
      (keyword (name resolved)))))

(defn- forms [form]
  (tree-seq #(and (coll? %) (not (and (seq? %) (#{'quote 'comment} (first %))))) seq form))

(defn- site [definition form]
  (merge (select-keys definition [:file :op]) (select-keys (meta form) [:row :col])))

(defn- reader [index {:keys [file] :as definition} form]
  (let [[_ bindings guard] form]
    (when (and (= :doseq (core-op index file (first form))) (= 3 (count form))
               (vector? bindings) (= 2 (count bindings)) (vector? (first bindings))
               (= 2 (count (first bindings))) (symbol? (second bindings))
               (seq? guard) (= :when (core-op index file (first guard))))
      (let [[_ record] (first bindings)
            predicate (second guard)]
        (when (and (symbol? record) (seq? predicate) (= 2 (count predicate))
                   (keyword? (first predicate)) (= record (second predicate)) (seq (nnext guard)))
          {:reader (site definition form) :collection (str (second bindings))
           :field (str (first predicate)) :consumers (mapv pr-str (nnext guard))})))))

(defn- writer [index {:keys [file] :as definition} form]
  (let [[_ owner updater path] form]
    (when (and (= :swap! (core-op index file (first form))) (symbol? owner)
               (#{:assoc-in :update-in} (core-op index file updater)) (vector? path)
               (keyword? (first path)))
      {:site (site definition form) :owner (str owner) :path path})))

(defn- matching-writer [{:keys [collection field]} {:keys [path] :as writer}]
  ;; Name agreement is only a lead: a parameter can alias unrelated state.
  (when (= collection (name (first path)))
    (when-let [effect (case (count path)
                       1 :collection-replacement
                       2 :record-replacement
                       3 (when (= field (str (last path))) :predicate-write)
                       nil)]
      (assoc writer :path (pr-str path) :effect effect))))

(defn analyze [dir files lang]
  (let [index (flow/index dir files nil lang)
        calls (for [[_ definitions] (sort-by key (:defs index)), definition definitions,
                    form (forms (:form definition)) :when (seq? form)] [definition form])
        writers (keep (fn [[definition form]] (writer index definition form)) calls)
        readers (keep (fn [[definition form]] (reader index definition form)) calls)]
    {:lang (:lang index) :files (:file-count index) :unparsed (:unparsed index)
     :opportunities (mapv (fn [scan]
                            (assoc scan :kind :filtered-membership :status :hypothesis
                                   :candidate "Keep the original map; iterate keys whose current record satisfies the predicate."
                                   :possible-writers (vec (keep #(matching-writer scan %) writers)))) readers)
     :boundaries ["Collection ownership is unproven. Writer matching covers only direct swap! assoc-in/update-in paths with a matching root name; other mutations and callers are unresolved."
                  "Preserve consumer order and snapshot identity; test empty, repeated, removed and replaced records and collection rebuilds."]
     :measure ["Fraction of records selected over time" "Reads and membership changes per build"
               "Compare original scan, snapshot view and maintained view on target runtime, including build time, allocations and retained bytes."]}))
