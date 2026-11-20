(ns layout-descent.facts
  "Access facts of a Clojure code base. clj-kondo resolves which symbols are the clojure.core map
  accessors; edamame recovers the argument forms at those positions. The facts say where nested
  maps are read, written and scanned, and with which path shapes. They decide nothing."
  (:require [clj-kondo.core :as kondo]
            [clojure.string :as str]
            [edamame.core :as edamame]))

(def accessors
  "clojure.core functions that count as an access: the argument that holds the map, the argument
  that holds the path or key, and the kind of access."
  '{get-in    {:map 0 :path 1 :kind :read}
    get       {:map 0 :key 1 :kind :read}
    contains? {:map 0 :key 1 :kind :read}
    assoc-in  {:map 0 :path 1 :kind :write}
    update-in {:map 0 :path 1 :kind :write}
    assoc     {:map 0 :key 1 :kind :write}
    update    {:map 0 :key 1 :kind :write}
    dissoc    {:map 0 :key 1 :kind :write}
    keys      {:map 0 :kind :scan}
    vals      {:map 0 :kind :scan}
    reduce-kv {:map 2 :kind :scan}})

;; Functions that apply their function argument to a reference: (swap! state assoc-in path v).
(def ^:private appliers '#{swap! swap-vals! alter commute send send-off update update-in})

(def ^:private parse-opts
  {:all true
   :read-cond :allow
   :features #{:clj}
   :auto-resolve (fn [alias] (if (= :current alias) 'current-ns alias))
   :readers (fn [_tag] identity)})

(defn- clip [form]
  (let [s (pr-str form)]
    (if (> (count s) 60) (str (subs s 0 60) "…") s)))

(defn- index-forms
  "Two maps over every form of a file: [row col] of a list -> the list, and [row col] of a symbol
  that is not the head of its list -> [list index]."
  [forms]
  (let [calls (volatile! {})
        values (volatile! {})]
    (letfn [(visit [form]
              (when (seq? form)
                (when-let [{:keys [row col]} (meta form)]
                  (vswap! calls assoc [row col] form))
                (doseq [[i child] (map-indexed vector form)
                        :when (and (pos? (long i)) (symbol? child) (meta child))]
                  (vswap! values assoc [(:row (meta child)) (:col (meta child))] [form i])))
              (cond
                (map? form) (run! visit (mapcat identity form))
                (coll? form) (run! visit form)))]
      (run! visit forms))
    {:calls @calls :values @values}))

(defn- element [form]
  (cond
    (symbol? form) {:kind :key :text (str "?" form)}
    (or (keyword? form) (string? form) (number? form) (boolean? form) (nil? form))
    {:kind :const :text (pr-str form)}
    :else {:kind :key :text "?"}))

(defn- chain
  "Follow nested literal accesses outwards: (get (get-in db [:a k]) j) is db at [:a ?k ?j].
  Returns {:root form :path [elements] :inner [positions of the absorbed calls]}, with a nil
  :path when a path is not a literal vector."
  [form]
  (let [here (fn [inner own]
               (let [below (chain inner)]
                 (if (and (:path below) own)
                   (assoc below
                          :path (into (:path below) own)
                          :inner (conj (:inner below) ((juxt :row :col) (meta form))))
                   {:root form :path [] :inner []})))]
    (cond
      (and (seq? form) (= 'get-in (first form)) (vector? (nth form 2 nil)))
      (here (second form) (mapv element (nth form 2)))

      (and (seq? form) (= 'get (first form)) (<= 3 (count form) 4))
      (here (second form) [(element (nth form 2))])

      (and (seq? form) (keyword? (first form)) (= 2 (count form)))
      (here (second form) [(element (first form))])

      :else {:root form :path [] :inner []})))

(defn- site
  "One fact from the arguments of an accessor call. threaded? says that the map argument is
  supplied by a threading macro and is not in args."
  [fn-name args threaded?]
  (let [{:keys [path key kind] map-index :map} (get accessors fn-name)
        arg (fn [i] (if threaded? (when (pos? (long i)) (nth args (dec (long i)) nil)) (nth args i nil)))
        below (if threaded? {:root '<threaded> :path [] :inner []} (chain (arg map-index)))
        own (cond
              path (let [form (arg path)] (when (vector? form) (mapv element form)))
              key [(element (arg key))]
              :else [{:kind :scan :text "*"}])]
    {:kind kind
     :target (clip (:root below))
     :inner (:inner below)
     :path (when own (mapv :text (into (:path below) own)))
     :shape (when own (mapv #(if (= :key (:kind %)) "?" (:text %)) (into (:path below) own)))
     :keyed (count (filter #(= :key (:kind %)) (into (:path below) own)))}))

(defn- usage-site [{:keys [calls values]} {:keys [row col arity name]}]
  (if-let [form (get calls [row col])]
    (let [args (vec (rest form))]
      (site name args (and arity (< (count args) (long arity)))))
    (when-let [[parent i] (get values [row col])]
      (when (appliers (first parent))
        (assoc (site name (into ['<reference>] (drop (inc (long i)) parent)) false)
               :target (clip (nth parent 1 nil))
               :via (str (first parent)))))))

(defn- file-index [filename]
  (try
    (index-forms (edamame/parse-string-all (slurp filename) parse-opts))
    (catch Exception _ nil)))

(defn- relevant? [{:keys [shape keyed]}]
  (or (nil? shape) (pos? (long keyed)) (>= (count shape) 2)))

(defn analyze
  "Every access fact under the given source paths. Returns {:files n :unparsed [files] :sites
  [...]}; a site has :file :row :col :fn :in :kind :target :path :shape :keyed and maybe :via.
  A nil :path means the path is not a literal. Accesses of one constant key are left out."
  [paths]
  (let [usages (->> (kondo/run! {:lint (mapv str paths)
                                 :cache false
                                 :skip-lint true
                                 :config {:output {:analysis true}}})
                    :analysis
                    :var-usages
                    (filter #(and ('#{clojure.core cljs.core} (:to %)) (accessors (:name %))))
                    ;; A call inside swap! is reported two times, a .cljc file once per language.
                    (sort-by (juxt :filename :row :col #(if (:arity %) 0 1)))
                    (reduce (fn [[seen out] u]
                              (let [k [(:filename u) (:row u) (:col u)]]
                                (if (seen k) [seen out] [(conj seen k) (conj out u)])))
                            [#{} []])
                    second)
        indexes (into {} (map (fn [f] [f (file-index f)])) (distinct (map :filename usages)))
        sites (for [u usages
                    :let [index (get indexes (:filename u))
                          fact (when index (usage-site index u))]
                    :when fact]
                (array-map
                 :file (:filename u)
                 :row (:row u)
                 :col (:col u)
                 :fn (str (:name u))
                 :in (str (:from u) (when (:from-var u) (str "/" (:from-var u))))
                 :kind (:kind fact)
                 :via (:via fact)
                 :target (:target fact)
                 :path (:path fact)
                 :shape (:shape fact)
                 :keyed (:keyed fact)
                 :inner (:inner fact)))
        absorbed (set (for [s sites, [row col] (:inner s)] [(:file s) row col]))]
    {:files (count indexes)
     :unparsed (vec (sort (keep (fn [[f index]] (when (nil? index) f)) indexes)))
     :sites (->> sites
                 (remove #(absorbed [(:file %) (:row %) (:col %)]))
                 (filter relevant?)
                 (mapv #(dissoc % :inner)))}))

(defn summarize
  "Group sites by path shape, the most keyed shapes first. In a shape, ? is a key the code
  computes, * a scan of every key, and anything else a constant. Each group lists at most limit
  sites."
  [{:keys [files unparsed sites]} limit]
  (let [dynamic (filterv #(nil? (:shape %)) sites)
        groups (->> (remove #(nil? (:shape %)) sites)
                    (group-by :shape)
                    (map (fn [[shape found]]
                           (let [kinds (frequencies (map :kind found))]
                             {:shape shape
                              :keyed (:keyed (first found))
                              :reads (get kinds :read 0)
                              :writes (get kinds :write 0)
                              :scans (get kinds :scan 0)
                              :functions (vec (sort (distinct (map :fn found))))
                              :site-count (count found)
                              :sites (vec (take limit (sort-by (juxt :file :row :col) found)))})))
                    (sort-by (juxt #(- (long (:keyed %))) #(- (long (:site-count %))) #(str/join " " (:shape %))))
                    vec)]
    {:files files
     :unparsed unparsed
     :site-count (count sites)
     :shapes groups
     :dynamic-count (count dynamic)
     :dynamic (vec (take limit (sort-by (juxt :file :row :col) dynamic)))}))
