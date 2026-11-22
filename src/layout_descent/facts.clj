(ns layout-descent.facts
  "Access facts of a Clojure code base. clj-kondo resolves which symbols are the clojure.core map
  accessors; edamame recovers the argument forms at those positions. The facts say where nested
  maps are read, written and scanned, and with which path shapes. They decide nothing."
  (:require [clj-kondo.core :as kondo]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edamame.core :as edamame]
            [layout-descent.plan :as plan])
  (:import (java.nio.file Path)))

(def accessors
  "clojure.core functions that count as an access: the argument that holds the map, the argument
  that holds the path or key, and the kind of access. A scan that calls a function names the
  argument that holds it (:fn) and the parameter that receives each value (:value)."
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
    reduce-kv {:map 2 :fn 0 :value 2 :kind :scan}})

;; Functions that apply their function argument to a reference: (swap! state assoc-in path v).
(def ^:private appliers '#{swap! swap-vals! alter commute send send-off update update-in})

;; The forms that bind the parameters of a literal function. #() reads as fn*.
(def ^:private binders '#{fn fn*})

(def ^:private parse-opts
  {:all true
   :read-cond :allow
   :features #{:clj}
   :auto-resolve (fn [alias] (if (= :current alias) 'current-ns alias))
   :readers (fn [_tag] identity)})

(defn- clip [form]
  (plan/clip 60 form))

(def ^:private thread-first '#{-> some-> cond->})
(def ^:private thread-last '#{->> some->> cond->>})

(defn- position [form]
  (when-let [{:keys [row col]} (meta form)]
    (when row [row col])))

(defn- reader-fn?
  "True for the (fn* ...) list that #() reads as: the reader gives its head no position."
  [form]
  (and (seq? form) (= 'fn* (first form)) (nil? (position (first form)))))

(defn- index-forms
  "Map each list position to its form, each direct argument position to its parent and slot, and
  each list position inside a #() to the position of that #()."
  [forms]
  (let [calls (volatile! {})
        arguments (volatile! {})
        readers (volatile! {})]
    (letfn [(visit [reader form]
              (let [reader (if (reader-fn? form) (position form) reader)]
                (when (seq? form)
                  (when-let [pos (position form)]
                    (vswap! calls assoc pos form)
                    (when reader (vswap! readers assoc pos reader)))
                  (doseq [[i child] (map-indexed vector (rest form))
                          :let [pos (position child)]
                          :when pos]
                    (vswap! arguments assoc pos [form (inc (long i))])))
                (cond
                  (map? form) (run! #(visit reader %) (mapcat identity form))
                  (coll? form) (run! #(visit reader %) form))))]
      (run! #(visit nil %) forms))
    {:calls @calls :arguments @arguments :readers @readers}))

(defn- part
  "One key form as {:text :shape :keyed}: a constant, or a key that the code computes."
  [form]
  (cond
    (symbol? form) {:text (str "?" form) :shape "?" :keyed 1}
    (or (keyword? form) (string? form) (number? form) (boolean? form) (nil? form))
    {:text (pr-str form) :shape (pr-str form) :keyed 0}
    :else {:text "?" :shape "?" :keyed 1}))

(defn- element
  "One level of a path as {:text :shape :keyed}. A vector literal of two or more parts, of which
  the code computes at least one, is a composite key: one level, written [? :a], that counts one
  computed level for each computed part."
  [form]
  (let [parts (when (and (vector? form) (<= 2 (count form))) (mapv part form))
        join #(str "[" (str/join " " (map % parts)) "]")]
    (if (some (comp pos? :keyed) parts)
      {:text (join :text) :shape (join :shape) :keyed (reduce + (map :keyed parts))}
      (part form))))

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

(defn- rebinds?
  "True when a vector or a map in the body of a function holds sym: a binding may shadow it."
  [form sym]
  (boolean (some #(and (vector? %) (some #{sym} %)) (tree-seq coll? seq (drop 2 form)))))

(defn- value-param
  "The parameter that receives each scanned value when form is a literal function of one arity:
  {:at position-of-the-function :param symbol}. Nil for a destructured or a variadic parameter,
  and for a #() that may bind its parameter again, because no analysis resolves that name."
  [resolved form index]
  (when (and (seq? form) (binders (resolved (position form))))
    (let [params (first (drop-while symbol? (rest form)))
          param (when (vector? params) (nth params index nil))]
      (when (and (symbol? param)
                 (not-any? #{'&} (take (inc (long index)) params))
                 (not (and (reader-fn? form) (rebinds? form param))))
        {:at (position form) :param param}))))

(defn- call-sites [resolved fn-name args]
  (let [{:keys [path key key-step kind value] map-index :map function :fn} (accessors fn-name)
        arg #(nth args % nil)
        below (chain resolved (arg map-index))
        binder (when function (value-param resolved (arg function) value))
        paths (cond
                path [(let [form (arg path)] (when (vector? form) (mapv element form)))]
                key-step (map (comp vector element) (take-nth key-step (drop key args)))
                key [[(element (arg key))]]
                :else [[{:text "*" :shape "*" :keyed 0}]])]
    (for [own paths
          :let [full (into (:path below) own)]]
      {:kind kind
       :target (clip (:root below))
       :root (:root below)
       :binder binder
       :inner (:inner below)
       :path (when own (mapv :text full))
       :shape (when own (mapv :shape full))
       :keyed (reduce + (map :keyed full))})))

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

(defn- symbol-position
  "Where clj-kondo sees a symbol: the end of its form, after a type hint."
  [sym]
  (let [{:keys [end-row end-col]} (meta sym)]
    (when end-row [end-row (- (long end-col) (count (str sym)))])))

(defn- local-bindings
  "Map each use of a local to the position that binds it: {file {[row col] [row col]}}."
  [{:keys [locals local-usages]}]
  (let [id (juxt :filename :lang :id)
        bound (into {} (map (juxt id (juxt :row :col))) locals)]
    (reduce (fn [out {:keys [filename row col name-row name-col] :as usage}]
              (if-let [at (bound (id usage))]
                (assoc-in out [filename [(or name-row row) (or name-col col)]] at)
                out))
            {} local-usages)))

(defn- binder-key
  "The binding that a scan function gives each value: the position of the parameter, or for #()
  the position of the function and the name of the parameter."
  [{:keys [at param]}]
  (when param (or (symbol-position param) [at (name param)])))

(defn- root-key
  "The binding that the root of a path refers to, in the form of binder-key. pos is the access."
  [bindings readers pos root]
  (when (symbol? root)
    (if-let [at (symbol-position root)]
      (get bindings at)
      (when-let [reader (get readers pos)] [reader (name root)]))))

(defn- continues?
  "A read or a scan with a literal path can continue the scan that binds its root."
  [{:keys [kind path bound]}]
  (and bound path (not= :write kind)))

(defn- continue-scans
  "Join each scan to the accesses that its function makes on the scanned value. The scan site gets
  one path per such access, and that access is not a site of its own."
  [sites]
  (let [binds (set (keep :binds sites))
        inner? #(and (continues? %) (contains? binds (:bound %)))
        within (group-by :bound (filter inner? sites))
        extend (fn extend [site]
                 (if-let [inner (seq (get within (:binds site)))]
                   (for [access inner
                         end (extend access)]
                     (assoc site
                            :path (into (:path site) (:path end))
                            :shape (into (:shape site) (:shape end))
                            :keyed (+ (long (:keyed site)) (long (:keyed end)))))
                   [site]))]
    (mapcat extend (remove inner? sites))))

(defn- directory ^Path [path]
  (let [file (.toFile (.normalize (.toAbsolutePath (.toPath (io/file (str path))))))]
    (.toPath (if (.isDirectory file) file (.getParentFile file)))))

(defn- common-root
  "The longest directory that holds every given path."
  ^Path [paths]
  (reduce (fn [^Path root ^Path dir]
            (if (.startsWith dir root) root (recur (.getParent root) dir)))
          (map directory paths)))

(defn- relative [^Path root filename]
  (str (.relativize root (.normalize (.toAbsolutePath (.toPath (io/file (str filename))))))))

(defn- extension [filename]
  (let [file (.getName (io/file (str filename)))
        dot (.lastIndexOf file ".")]
    (if (pos? dot) (keyword (subs file (inc dot))) :other)))

(defn analyze
  "Every access fact under the given source paths. Returns {:root dir :files n :sources {ext n}
  :unparsed [files] :sites [...]}; a site has :file :row :col :fn :in :kind :target :path :shape
  :keyed and maybe :via. :root is the longest common directory of the paths, and each file name
  is relative to it. :files counts the files that hold an access, and :sources counts every
  file that was read, by extension. A nil :path means the path is not a literal. Accesses of one constant key
  are left out. A scan whose literal function reads the scanned value is one site: its path goes
  on into that read. :keyed counts the keys that the code computes: each computed part of a
  vector key is one."
  ([paths] (analyze paths paths))
  ([paths lint-paths]
  (let [root (common-root paths)
        analysis (:analysis (kondo/run! {:lint (mapv str lint-paths)
                                         :cache false
                                         :skip-lint true
                                         :config {:output {:analysis {:locals true}}}}))
        bindings (local-bindings analysis)
        usages (->> (:var-usages analysis)
                    (filter (fn [{:keys [to name]}]
                              (and ('#{clojure.core cljs.core} to)
                                   (or (accessors name) (thread-first name) (thread-last name)
                                       (binders name)))))
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
                          index (get indexes filename)
                          in-file #(when % [filename %])]
                    :when index
                    fact (usage-sites index (resolved filename) u)]
                (array-map
                 :file (relative root filename)
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
                 :inner (:inner fact)
                 :bound (in-file (root-key (get bindings filename) (:readers index)
                                           [(:row u) (:col u)] (:root fact)))
                 :binds (in-file (binder-key (:binder fact)))))
        absorbed (set (for [s sites, [row col] (:inner s)] [(:file s) row col]))]
    {:root (str root)
     :files (count indexes)
     :sources (into (sorted-map :clj 0 :cljc 0 :cljs 0)
                    (frequencies (map extension (distinct (mapcat #(map :filename %) (vals analysis))))))
     :unparsed (vec (sort (keep (fn [[f index]] (when (nil? index) (relative root f))) indexes)))
     :sites (->> sites
                 (remove #(absorbed [(:file %) (:row %) (:col %)]))
                 continue-scans
                 (filter relevant?)
                 (mapv #(dissoc % :inner :bound :binds)))})))

(def summary-defaults
  "What summarize shows without options: shapes of two or more computed levels, and no writes."
  {:min-depth 2 :writes false})

(defn- depth
  "The number of levels that a site computes or scans: its computed keys, each computed part of
  a vector key among them, and its * elements."
  [{:keys [shape keyed]}]
  (+ (long keyed) (count (filter #{"*"} shape))))

(defn- source-warnings [sources]
  (if (pos? (long (:cljs sources 0)))
    [{:code :clojurescript-sources
      :message (str "The analyzed files include ClojureScript ("
                    (str/join ", " (map (fn [[ext n]] (str "." (name ext) " " n)) sources))
                    "). Plans are measured and emitted for the JVM, so the timings do not"
                    " transfer to a JavaScript runtime and the emitted namespace does not load"
                    " there.")}]
    []))

(defn- all-hidden-hint
  "Names only the options that can show something: a lower min-depth when the call asks for more
  than one computed level, and writes when write sites are hidden. Nil when neither can."
  [min-depth hidden-writes]
  (when-let [parts (seq (remove nil? [(when (< 1 (long min-depth)) "min-depth 1 shows the shapes of one computed level")
                                      (when (pos? (long hidden-writes)) "writes true shows the write sites")]))]
    (str "Every shape is hidden: " (str/join ", and " parts) ".")))

(def ^:private constant-only-hint
  (str "Every shape has constant keys only, so the code computes no level to model: min-depth 0"
       " lists these shapes."))

(defn- shape-group [limit [shape found]]
  (let [kinds (frequencies (map :kind found))]
    (array-map
     :shape shape
     :depth (depth (first found))
     :keyed (:keyed (first found))
     :reads (get kinds :read 0)
     :writes (get kinds :write 0)
     :scans (get kinds :scan 0)
     :functions (vec (sort (distinct (map :fn found))))
     :site-count (count found)
     :sites (vec (take limit (sort-by (juxt :file :row :col) found))))))

(defn summarize
  "Group sites by path shape, the most keyed shapes first. In a shape, ? is a key the code
  computes, * a scan of every key, [? :a] one level whose key is a vector, and anything else a
  constant. Each group lists at most limit sites; limit 0 gives the counts only. opts {:min-depth
  n :writes bool} choose what is shown: a shape needs n or more ? and * elements, where each ?
  in a vector key counts, and a write site shows only when :writes is true. :hidden counts what
  is left out: whole shapes, all their sites and the hidden writes together, and the hidden
  writes alone. :site-count counts every site, shown or not. :warnings is a vector of {:code
  :message}; :hint is present only when shapes exist and none is shown."
  ([analysis limit] (summarize analysis limit summary-defaults))
  ([{:keys [root files sources unparsed sites]} limit opts]
   (let [{:keys [min-depth writes]} (merge summary-defaults opts)
         kept (if writes sites (remove #(= :write (:kind %)) sites))
         dynamic (filterv #(nil? (:shape %)) kept)
         groups (->> (remove #(nil? (:shape %)) kept)
                     (group-by :shape)
                     (filter #(<= (long min-depth) (long (depth (first (val %))))))
                     (map #(shape-group limit %))
                     (sort-by (juxt #(- (long (:keyed %))) #(- (long (:site-count %))) #(str/join " " (:shape %))))
                     vec)
         hidden-shapes (- (count (distinct (keep :shape sites))) (count groups))]
     (apply array-map
            (concat
             [:root root
              :files files
              :sources sources
              :unparsed unparsed
              :site-count (count sites)
              :warnings (source-warnings sources)]
             (when (and (empty? groups) (pos? hidden-shapes))
               [:hint (or (all-hidden-hint min-depth (- (count sites) (count kept))) constant-only-hint)])
             [:shapes groups
              :hidden {:shapes hidden-shapes
                       :sites (- (count sites) (count dynamic) (long (reduce + (map :site-count groups))))
                       :writes (- (count sites) (count kept))}
              :dynamic-count (count dynamic)
              :dynamic (vec (take limit (sort-by (juxt :file :row :col) dynamic)))])))))
