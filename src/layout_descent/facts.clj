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
    assoc     {:map 0 :key 1 :key-step 2 :kind :write}
    update    {:map 0 :key 1 :kind :write}
    dissoc    {:map 0 :key 1 :key-step 1 :kind :write}
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

(def ^:private thread-first '#{-> some-> cond->})
(def ^:private thread-last '#{->> some->> cond->>})

(defn- position [form]
  (when-let [{:keys [row col]} (meta form)]
    (when row [row col])))

(defn- index-forms
  "Map each list position to its form, and each direct argument position to its parent and slot."
  [forms]
  (let [calls (volatile! {})
        arguments (volatile! {})]
    (letfn [(visit [form]
              (when (seq? form)
                (when-let [pos (position form)]
                  (vswap! calls assoc pos form))
                (doseq [[i child] (map-indexed vector (rest form))
                        :let [pos (position child)]
                        :when pos]
                  (vswap! arguments assoc pos [form (inc (long i))])))
              (cond
                (map? form) (run! visit (mapcat identity form))
                (coll? form) (run! visit form)))]
      (run! visit forms))
    {:calls @calls :arguments @arguments}))

(defn- element [form]
  (cond
    (symbol? form) {:kind :key :text (str "?" form)}
    (or (keyword? form) (string? form) (number? form) (boolean? form) (nil? form))
    {:kind :const :text (pr-str form)}
    :else {:kind :key :text "?"}))

(defn- chain
  "Follow nested literal accesses outwards: (get (get-in db [:a k]) j) is db at [:a ?k ?j].
  Returns {:root form :path [elements] :inner [positions of the absorbed calls]}."
  [resolved form]
  (let [pos (position form)
        accessor (when (seq? form) (resolved pos))
        here (fn [own]
               (let [below (chain resolved (second form))]
                 (assoc below
                        :path (into (:path below) own)
                        :inner (conj (:inner below) pos))))]
    (cond
      (and (= 'get-in accessor) (vector? (nth form 2 nil)))
      (here (mapv element (nth form 2)))

      (and (= 'get accessor) (<= 3 (count form) 4))
      (here [(element (nth form 2))])

      (and (seq? form) (keyword? (first form)) (= 2 (count form)))
      (here [(element (first form))])

      :else {:root form :path [] :inner []})))

(defn- call-sites [resolved fn-name args]
  (let [{:keys [path key key-step kind] map-index :map} (accessors fn-name)
        arg #(nth args % nil)
        below (chain resolved (arg map-index))
        paths (cond
                path [(let [form (arg path)] (when (vector? form) (mapv element form)))]
                key-step (map (comp vector element) (take-nth key-step (drop key args)))
                key [[(element (arg key))]]
                :else [[{:kind :scan :text "*"}]])]
    (for [own paths
          :let [full (into (:path below) own)]]
      {:kind kind
       :target (clip (:root below))
       :inner (:inner below)
       :path (when own (mapv :text full))
       :shape (when own (mapv #(if (= :key (:kind %)) "?" (:text %)) full))
       :keyed (count (filter #(= :key (:kind %)) full))})))

(defn- usage-sites [{:keys [calls arguments]} resolved {:keys [row col arity name]}]
  (let [pos [row col]
        [parent i] (arguments pos)
        parent-name (resolved (position parent))
        step? (and i (<= 2 (long i)))]
    (if-let [form (calls pos)]
      (let [args (vec (rest form))]
        (call-sites resolved name
                    (cond
                      (or (nil? arity) (<= (long arity) (count args))) args
                      (and step? (thread-last parent-name)) (conj args '<threaded>)
                      :else (into ['<threaded>] args))))
      (cond
        (and step? (= 1 arity) (or (thread-first parent-name) (thread-last parent-name)))
        (call-sites resolved name ['<threaded>])

        (appliers (first parent))
        (for [site (call-sites resolved name (into ['<reference>] (drop (inc (long i)) parent)))]
          (assoc site :target (clip (nth parent 1 nil)) :via (str (first parent))))))))

(defn- file-index [filename]
  (try
    (index-forms (edamame/parse-string-all (slurp filename) parse-opts))
    (catch Exception _ nil)))

(defn- relevant? [{:keys [kind shape keyed]}]
  (or (= :scan kind) (nil? shape) (pos? (long keyed)) (>= (count shape) 2)))

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
                    (filter (fn [{:keys [to name]}]
                              (and ('#{clojure.core cljs.core} to)
                                   (or (accessors name) (thread-first name) (thread-last name)))))
                    ;; A call inside swap! is reported two times, a .cljc file once per language.
                    (sort-by (juxt :filename :row :col #(if (:arity %) 0 1)))
                    (reduce (fn [[seen out] u]
                              (let [k [(:filename u) (:row u) (:col u)]]
                                (if (seen k) [seen out] [(conj seen k) (conj out u)])))
                            [#{} []])
                    second)
        resolved (reduce (fn [out {:keys [filename row col name]}]
                           (assoc-in out [filename [row col]] name))
                         {} usages)
        access-usages (filter #(accessors (:name %)) usages)
        indexes (into {} (map (fn [f] [f (file-index f)])) (distinct (map :filename access-usages)))
        sites (for [u access-usages
                    :let [filename (:filename u)
                          index (get indexes filename)]
                    :when index
                    fact (usage-sites index (resolved filename) u)]
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
