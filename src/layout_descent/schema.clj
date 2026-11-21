(ns layout-descent.schema
  "The maps of a code base as records: where each one is built, which fields the code reads,
  writes and scans, and how a field moves from one record into another. One flow trace for each
  entry function supplies the steps. A record is the origin of a value, named by its source site.
  Static evidence: a record is no type, and two records can hold the same data."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [layout-descent.flow :as flow]))

(def ^:private field-kinds #{:produce :carry :write})

(defn- root
  "What a root step is: {:kind :name}, and :file :row :col :in for a root at one source site.
  A reference and a var are one record wherever the code reads them."
  [steps {:keys [kind via file row col in inputs reason]}]
  (let [at {:file file :row row :col col :in in}]
    (case kind
      :input (assoc at :kind :param :name (str in " " via) :label via)
      :assemble (assoc at :kind (if (#{"vector" "set"} via) :value :built) :name (str via " in " in))
      :state-read (let [ref (get steps (first inputs))]
                    (if (= :external-value (:reason ref))
                      {:kind :state :name (:via ref)}
                      (assoc at :kind :state :name (str "reference in " in))))
      :boundary (if (= :external-value reason)
                  {:kind :var :name via}
                  (assoc at :kind :result :name (str "result of " via)))
      (assoc at :kind :value :name (str (name kind) " " via)))))

(defn- below [v k]
  (into #{} (map (fn [[r path]] [r (conj path k)])) (:org v)))

(defn- merge-values [vs]
  {:org (into #{} (mapcat :org) vs) :shapes (into #{} (mapcat :shapes) vs)})

(defn- prefer-site [a b]
  (if (or (nil? a) (neg? (compare b a))) b a))

(defn- record! [state v k kind site shapes]
  (doseq [[r path] (below v k)]
    (vswap! state update-in [:records r path kind] (fnil conj #{}) site)
    (doseq [shape shapes]
      (vswap! state update-in [:shapes r path shape] prefer-site [(if (= kind :write) 0 1) site kind]))))

(defn- store! [state dst k src site]
  (record! state dst k :write site (:shapes src))
  (doseq [[a p] (:org src) [b q] (below dst k) :when (not= [a p] [b q])]
    (vswap! state update :links conj [(if (seq p) :carries :holds) a p b q])))

(defn- boundary! [state s reason]
  (vswap! state update :boundaries conj [reason (:via s) (:file s) (:row s) (:col s)]))

(defn- instantiate
  "Interpret one cached local body against actual argument origins. No nested step log is built."
  [state steps args call]
  (let [site-of (juxt :file :row :col :in)
        owners (reduce (fn [m s] (if (= :assemble (:kind s))
                                   (reduce #(if (contains? %1 %2) %1 (assoc %1 %2 (:id s))) m (:inputs s)) m)) {} steps)]
    (peek
     (reduce
      (fn [vs {:keys [id kind field via inputs container path access reason resolution] :as s}]
        (let [src (get vs (first inputs)) m (get vs container) site (site-of s)
              own (fn [] {:org #{[(root steps s) []]}})
              value (case kind
                      :input (if args (get args id) (own))
                      :call (call s (mapv (fn [{:keys [id shape]}]
                                            (if id (vs id) (when shape {:shapes #{shape}})))
                                          (::flow/args s)))
                      (:pass :return) (cond-> src
                                        (::flow/shape s) (assoc :shapes #{(::flow/shape s)})
                                        (::flow/removed s) (update :slots #(apply dissoc % (::flow/removed s)))
                                        (and (contains? s ::flow/removed) (nil? (::flow/removed s)))
                                        (dissoc :closed :slots))
                      :state-write (get vs (last inputs))
                      (:produce :carry :write) (cond-> src (::flow/shape s) (assoc :shapes #{(::flow/shape s)}))
                      :read (let [known (get (:slots m) field)
                                  miss (or (= :known-miss resolution) (and (:closed m) (not known)))
                                  v (cond miss {} known known (= :known-slot resolution) src :else {:org (below m field)})]
                              (record! state m field (if miss :miss :read) site (:shapes v)) v)
                      :assemble
                      (if (#{"vector" "set"} via)
                        (assoc (own) :shapes #{(keyword via)})
                        (let [onto (get vs (::flow/onto s))
                              dst (assoc (if (seq (:org onto)) onto (own)) :shapes #{:map})
                              slots (into {} (for [i inputs :let [t (steps i)] :when (field-kinds (:kind t))]
                                               [(:field t) (or (vs i) {})]))]
                          (doseq [i inputs :let [t (steps i)] :when (and (field-kinds (:kind t)) (= id (owners i)))]
                            (store! state dst (:field t) (vs i) (site-of t)))
                          (assoc dst :closed (or (:closed onto) (nil? (::flow/onto s))) :slots (merge (:slots onto) slots))))
                      :boundary (do (boundary! state s reason)
                                    (cond (and container (= :read access))
                                          (do (record! state m (peek path) :read site nil) {:org (below m (peek path))})
                                          (and container (= :write access))
                                          (do (store! state m (peek path) (get vs (last inputs)) site) {})
                                          :else (own)))
                      :iterate (if (::flow/element s) src
                                 (do (record! state src "*" :scan site nil)
                                     (or (:element src) {:org (below src "*")})))
                      :project (case resolution :known-slot src :known-miss {}
                                     (or (:element m) {:org (below m "*")}))
                      :collect (let [v (if (::flow/same s) src (assoc (own) :element src))]
                                 (assoc v :shapes #{(case via ("clojure.core/mapv" "cljs.core/mapv" "clojure.core/vec" "cljs.core/vec"
                                                                "clojure.core/filterv" "cljs.core/filterv") :vector
                                                          ("clojure.core/set" "cljs.core/set") :set :collection)}))
                      :choice (merge-values (map vs inputs))
                      (:consume :inspect) {}
                      (own))]
          (conj vs value))) [] steps))))

(defn- collect [index]
  (let [ops (sort-by str (keys (:defs index)))
        templates (into {} (map (fn [op] [(str op) (flow/templates index op)])) ops)
        state (volatile! {:entries [] :visited #{} :truncated #{} :records {} :links #{} :shapes {} :boundaries #{}})
        used (into #{} (map first) (vals (:calls index)))
        cache (volatile! {}) hits (volatile! 0) cuts (volatile! 0)]
    (letfn [(run [op arity args stack site fuel]
              (let [key [op arity args] body (get-in templates [op arity])]
                (cond
                  (or (stack op) (>= (count stack) 32))
                  (do (vswap! cuts inc) (boundary! state site (if (stack op) :recursive-call :call-depth)) {})
                  (contains? @cache key) (do (vswap! hits inc) (get @cache key))
                  (not body) (do (boundary! state site :unsupported-arity) {})
                  (neg? (vswap! fuel - (count (:steps body)))) (throw (ex-info "Schema expansion limit" {::limit true}))
                  :else (let [before @cuts
                              result (instantiate state (:steps body) args
                                                  (fn [s argv] (run (:via s) (count argv) argv (conj stack op) s fuel)))]
                          (vswap! state update :visited conj [op arity])
                          (if (:truncated body)
                            (do (vswap! state update :truncated conj op) {})
                            (do (when (and (= before @cuts) (< (count @cache) 4096)) (vswap! cache assoc key result)) result))))))]
      (doseq [sym (concat (remove used ops) ops) :let [op (str sym)]
              [arity _] (sort-by key (templates op)) :when (not ((:visited @state) [op arity]))]
        (vswap! state update :entries conj op)
        (try (run op arity nil #{} nil (volatile! 100000))
             (catch clojure.lang.ExceptionInfo e
               (if (::limit (ex-data e)) (vswap! state update :truncated conj op) (throw e)))))
      (assoc @state :truncated (vec (sort (:truncated @state)))
             :untraced (vec (sort (for [[op arities] templates :when (empty? arities)] op)))
             :summaries {:bodies (reduce + (map count (vals templates))) :reuses @hits}))))

(def ^:private kind-rank {:state 0 :var 1 :param 2 :result 3 :built 4 :value 5})

(def ^:private count-keys {:read :reads :write :writes :scan :scans :miss :misses})

(defn- structural? [element]
  (or (= "*" element) (= "?" element) (str/starts-with? element "[")))

(defn- named-key
  "The key of a path: its first constant. The keys of a collection or of an index are below its
  * and ? levels."
  [path]
  (first (drop-while structural? path)))

(defn- level
  "The part of a path that ends at its last * or ? level, or nil."
  [path]
  (let [n (count (drop-while (complement structural?) (rseq path)))]
    (when (pos? n) (subvec path 0 n))))

(defn group
  "Group records by their keys into entities. records are {:id :keys #{...}}. The record with the
  most keys comes first. A record joins the entity that shares the most of its keys, when they
  are two or more and at least 60 % of its own keys; otherwise it starts an entity. A record
  with fewer than two keys joins none. Returns [{:keys #{...} :records [id ...]} ...]."
  [records]
  (reduce (fn [entities {:keys [id keys]}]
            (let [scored (keep-indexed (fn [i entity]
                                         (let [shared (count (set/intersection keys (:keys entity)))]
                                           (when (and (<= 2 shared) (<= (* 0.6 (count keys)) shared))
                                             [shared (- (long i)) i])))
                                       entities)]
              (if-let [[_ _ i] (last (sort scored))]
                (update entities i #(-> % (update :keys into keys) (update :records conj id)))
                (conj entities {:keys (set keys) :records [id]}))))
          []
          (sort-by #(- (count (:keys %))) (filter #(<= 2 (count (:keys %))) records))))

(defn- counts [by-kind]
  (into {} (for [[kind k] count-keys] [k (count (get by-kind kind))])))

(defn- field-row [sites-limit [path by-kind]]
  (let [sites (sort (for [[kind sites] by-kind [file row col in] sites] [file row col in kind]))]
    (merge {:path path}
           (counts by-kind)
           {:in (vec (distinct (map #(nth % 3) sites)))
            :sites (mapv (fn [[file row col in kind]] {:file file :row row :col col :in in :access kind})
                         (take sites-limit sites))})))

(defn- boundary-rows [boundaries]
  (->> (group-by first boundaries)
       (map (fn [[reason sites]]
              {:reason reason
               :sites (count sites)
               :via (->> (frequencies (map second sites))
                         (sort-by (juxt (comp - val) key))
                         (take 5)
                         (mapv (fn [[via n]] {:via via :sites n})))}))
       (sort-by (juxt (comp - :sites) (comp str :reason)))
       vec))

(defn- common [xs]
  (ffirst (sort-by (juxt (comp - val) key) (frequencies xs))))

(defn- entity-name [members held]
  (let [labels (remove #(re-matches #"arg\d+" %) (keep :label members))
        label (common labels)
        field (common (mapcat held members))
        {:keys [in name]} (first members)]
    (cond (and label (not (#{"m" "x" "v" "result" "opts"} label))) [label :parameter]
          field [(str/replace-first field #"^:" "") :holding-field]
          label [label :parameter]
          :else [(or in name) :source])))

(defn- distinguish [groups]
  (let [by-name (group-by (comp :name second) groups)]
    (mapv (fn [[members row :as group]]
            (let [peers (by-name (:name row))]
              (if (= 1 (count peers)) group
                (let [others (into #{} (mapcat #(map :key (:keys (second %)))) (remove #{group} peers))
                      k (some #(when-not (others (:key %)) (:key %)) (:keys row))]
                  [members (update row :name str " (" (or k (:name (first members))) ")")])))) groups)))

(defn- entity-row [fields-of shapes held members]
  (let [paths (apply merge-with (partial merge-with into) (map fields-of members))
        sum (fn [rows] (counts (apply merge-with into (map val rows))))
        by-key (group-by (comp named-key key) paths)
        observed (reduce (fn [m [p forms]]
                           (let [[k & nested] (drop-while structural? p)]
                             (if (and k (empty? nested)) (update m k (fnil into #{}) (keys forms)) m)))
                         {} (mapcat shapes members))
        holders (frequencies (mapcat #(distinct (keep named-key (keys (fields-of %)))) members))
        [label source] (entity-name members held)]
    {:name label :name-source source
     :records (count members)
     :kinds (frequencies (map :kind members))
     :functions (count (distinct (for [[_ by-kind] paths [_ sites] by-kind [_ _ _ in] sites] in)))
     :keys (->> (dissoc by-key nil)
                (map (fn [[k rows]] (merge {:key k :records (holders k)
                                            :shapes (vec (sort (observed k)))} (sum rows))))
                (sort-by (juxt (comp - :records) :key))
                vec)
     :levels (->> (group-by (comp level key) paths)
                  (keep (fn [[path rows]] (when path (merge {:path path} (sum rows)))))
                  (sort-by (comp vec :path))
                  vec)}))

(defn- shape-examples [observed members k shapes]
  (let [evidence (reduce (fn [m [path forms]]
                           (if (= [k] (drop-while structural? path))
                             (reduce-kv (fn [m shape [rank site kind]]
                                          (update m shape prefer-site [rank site kind path])) m forms)
                             m)) {} (mapcat observed members))]
    (mapv (fn [shape]
            (let [[_ [file row col in] access path] (evidence shape)]
              {:shape shape :path path :file file :row row :col col :in in :access access})) shapes)))

(defn- contrasts [groups observed]
  (let [members-of (into {} (map (fn [[id members _]] [id members])) groups)
        rows (for [[id _ row] groups k (:keys row)
                   :let [shapes (vec (remove #{:collection :nil} (:shapes k)))] :when (seq shapes)]
               [(:key k) {:entity id :shapes shapes}])]
    (->> (group-by first rows)
         (keep (fn [[k pairs]]
                 (let [evidence (mapv second pairs)]
                   (when (< 1 (count (distinct (map :shapes evidence))))
                     {:key k :entities (mapv (fn [{:keys [entity shapes] :as e}]
                                              (assoc e :examples (shape-examples observed (members-of entity) k shapes)))
                                            evidence)}))))
         (sort-by :key) vec)))

(defn- entity-links
  "The links between two entities: how many fields one carries into the other, with examples."
  [links entity-of]
  (->> (for [[kind from from-path to to-path] links
             :let [a (entity-of from) b (entity-of to)]
             :when (and a b (not= a b))]
         [[kind a b] {:from from-path :to to-path}])
       (group-by first)
       (map (fn [[[kind a b] rows]]
              (let [pairs (sort-by (juxt (comp vec :from) (comp vec :to)) (distinct (map second rows)))]
                {:kind kind :from a :to b :fields (count pairs) :examples (vec (take 3 pairs))})))
       (sort-by (juxt (comp str :kind) :from :to))
       vec))

(defn analyze
  "The data model of the source tree under dir: entities, the links between them and where the
  traces stopped. opts: :limit entities shown, :entity the id of one entity whose records are
  added (or \"loose\" for the records of no entity), :sites the sites listed for each path of
  those records. An entity is a group of records with shared keys (`group`); its :keys count the
  records that have the key and the sites that read and write it, its :levels are the paths that
  end at a scan or at a computed key. A record is the origin of a value at one source site."
  [dir files lang {:keys [limit entity sites]}]
  (let [index (flow/index dir files nil lang)
        {:keys [entries truncated untraced records links boundaries shapes summaries]} (collect index)
        ordered (vec (sort-by (fn [r] [(kind-rank (:kind r)) (:name r) (str (:file r)) (or (:row r) 0) (or (:col r) 0)])
                              (keys records)))
        record-id (into {} (map-indexed (fn [i r] [r (str "r" (inc (long i)))])) ordered)
        held (reduce (fn [m [kind from _ _ p]]
                       (if (and (= :holds kind) (peek p) (not (structural? (peek p)))) (update m from (fnil conj []) (peek p)) m)) {} links)
        grouped (->> (group (for [r ordered] {:id r :keys (into #{} (keep named-key) (keys (records r)))}))
                     (map (fn [{members :records}] [members (entity-row records shapes held members)]))
                     distinguish
                     (sort-by (fn [[_ row]] [(- (long (:records row))) (- (count (:keys row))) (str (:name row))]))
                     (map-indexed (fn [i [members row]] [(str "e" (inc (long i))) members row]))
                     vec)
        entity-of (into {} (for [[id members _] grouped r members] [r id]))
        shown (set (map first (take limit grouped)))
        loose (remove entity-of ordered)
        chosen (cond (= "loose" entity) loose
                     entity (second (first (filter #(= entity (first %)) grouped))))
        member? (set chosen)]
    (when (and entity (nil? chosen))
      (throw (ex-info (str "No entity " entity ".") {:code :schema-entity})))
    (cond->
     {:root (str (.getCanonicalFile (java.io.File. (str dir))))
      :language (:lang index)
      :files (:file-count index)
      :unparsed (:unparsed index)
      :functions (count (:defs index))
      :entries (count (distinct entries)) :summaries summaries
      :truncated truncated
      :untraced untraced
      :note "An entity groups records that share keys; a record is the origin of a value at a source site, not a type. Counts are sites, not executions."
      :entities (vec (for [[id _ row] (take limit grouped)] (into (array-map :id id) row)))
      :shape-contrasts (contrasts (take limit grouped) shapes)
      :links (filterv #(and (shown (:from %)) (shown (:to %))) (entity-links links entity-of))
      :hidden {:entities (max 0 (- (count grouped) (long limit))) :loose-records (count loose)}
      :boundaries (boundary-rows boundaries)}
      entity
      (assoc :entity entity
             :records (vec (for [r chosen]
                             (assoc r :id (record-id r)
                                    :fields (mapv #(assoc (field-row sites %) :shapes (vec (sort (keys (get-in shapes [r (key %)]))))) (sort-by (comp vec key) (records r))))))
             :record-links (->> links
                                (filter (fn [[_ from _ to _]] (or (member? from) (member? to))))
                                (map (fn [[kind from from-path to to-path]]
                                       {:kind kind
                                        :from {:record (record-id from) :entity (entity-of from) :path from-path}
                                        :to {:record (record-id to) :entity (entity-of to) :path to-path}}))
                                (sort-by (juxt (comp str :kind) (comp :record :from) (comp :record :to) (comp vec :path :to)))
                                vec)))))

(defn- escape
  "Text for a quoted Mermaid label."
  [text]
  (reduce (fn [s [from to]] (str/replace s from to))
          (str text)
          [["#" "#35;"] ["&" "#amp;"] ["\"" "#quot;"] ["<" "#lt;"] [">" "#gt;"]]))

(def ^:private keys-drawn 12)

(defn mermaid
  "A Mermaid flowchart of the entities of an answer. A node lists the keys that most records of
  the entity have. A solid arrow carries fields from one entity into another; a dotted arrow
  stores one entity in a field of another."
  [{:keys [entities links]}]
  (let [node (fn [{:keys [id name records keys]}]
               (str "  " id "[\"<b>" (escape name) "</b> (" records (if (= 1 records) " record" " records") ")<br/>"
                    (str/join "<br/>" (map (comp escape :key) (take keys-drawn keys)))
                    (when (< (long keys-drawn) (count keys)) (str "<br/>+ " (- (count keys) (long keys-drawn)) " more"))
                    "\"]"))
        edge (fn [{:keys [kind from to fields examples]}]
               (if (= :holds kind)
                 (str "  " from " -.->|\"in " (escape (peek (:to (first examples)))) "\"| " to)
                 (str "  " from " -->|\"" fields (if (= 1 fields) " field" " fields") "\"| " to)))]
    (str/join "\n" (concat ["flowchart LR"] (map node entities) (map edge links)))))
