(ns layout-descent.schema
  "The maps of a code base as records: where each one is built, which fields the code reads,
  writes and scans, and how a field moves from one record into another. One flow trace for each
  entry function supplies the steps. A record is the origin of a value, named by its source site.
  Static evidence: a record is no type, and two records can hold the same data."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [layout-descent.flow :as flow]))

(def ^:private field-kinds #{:produce :carry :write})

(defn- extend-all [origins element]
  (into #{} (map (fn [[root path]] [root (conj path element)])) origins))

(defn- origins
  "For each step of one trace, in order: where its value comes from, as a set of [root-step-id
  path]. A read of a field is that field of its container; a field that a known map holds is the
  value that was stored; a call passes its origin on; a branch has the origins of all its sides;
  a map that extends a value is that value. Any other step is a root of its own."
  [steps]
  (reduce
   (fn [org {:keys [id kind inputs container field path access control resolution] :as step}]
     (let [of #(get org % #{})
           input (of (first inputs))
           own #{[id []]}]
       (conj org
             (case kind
               :read (case resolution
                       :known-slot input
                       :known-miss #{}
                       (extend-all (of container) field))
               :boundary (cond (and (= :read access) container) (extend-all (of container) (peek path))
                               access #{}
                               :else own)
               (:pass :return :produce :carry :write) input
               :state-write (of (last inputs))
               :choice (into #{} (mapcat of) (remove #{control} inputs))
               :iterate (if (::flow/element step) input (extend-all input "*"))
               :collect (if (::flow/same step) input own)
               :project (case resolution
                          :known-slot input
                          :known-miss #{}
                          (extend-all (of container) "*"))
               :assemble (let [onto (of (::flow/onto step))] (if (seq onto) onto own))
               ;; A computed value, such as a sum or a test, holds no fields.
               (:consume :inspect) #{}
               own))))
   [] steps))

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

(defn- trace-facts
  "The accesses and the links of one trace. An access is [root path kind site], with kind :read,
  :miss (a read of a field that the known map does not have), :write or :scan. A link is
  [kind from-root from-path to-root to-path]: :carries moves a field of one record into a field
  of another, :holds stores a whole record in a field."
  [steps]
  (let [org (origins steps)
        root-of (memoize #(root steps (nth steps %)))
        site (juxt :file :row :col :in)
        ;; A field belongs to the first assemble that takes it.
        owner (reduce (fn [m {:keys [id kind inputs]}]
                        (if (= :assemble kind)
                          (reduce #(if (and (field-kinds (:kind (nth steps %2))) (not (contains? %1 %2)))
                                     (assoc %1 %2 id)
                                     %1)
                                  m inputs)
                          m))
                      {} steps)
        accesses (fn [origins element kind step]
                   (for [[r path] origins] [(root-of r) (conj path element) kind (site step)]))]
    (reduce
     (fn [out {:keys [id kind inputs container field path access resolution] :as step}]
       (cond
         (= :read kind)
         (update out :accesses into
                 (accesses (get org container #{}) field (if (= :known-miss resolution) :miss :read) step))

         (and (= :boundary kind) access container)
         (let [to (get org container #{})
               ;; The inputs of a write under a computed key are the map, the key and the value.
               from (if (and (= :write access) (= 3 (count inputs))) (get org (peek inputs) #{}) #{})]
           (-> out
               (update :accesses into (accesses to (peek path) access step))
               (update :links into (for [[r1 p1] from
                                         [r2 p2] to]
                                     [(if (empty? p1) :holds :carries) (root-of r1) p1 (root-of r2) (conj p2 (peek path))]))))

         (and (= :iterate kind) (not (::flow/element step)))
         (update out :accesses into (accesses (get org (first inputs) #{}) "*" :scan step))

         (and (field-kinds kind) (owner id))
         (let [to (get org (owner id))
               from (get org (first inputs) #{})]
           (-> out
               (update :accesses into (accesses to field :write step))
               (update :links into (for [[r1 p1] from
                                         [r2 p2] to
                                         :let [target (conj p2 field)]
                                         :when (not= [r1 p1] [r2 target])]
                                     [(if (empty? p1) :holds :carries) (root-of r1) p1 (root-of r2) target]))))

         :else out))
     {:accesses [] :links []} steps)))

(defn- collect
  "Trace every function that no other function reaches: first those that the tree never names,
  then each one that no earlier trace entered."
  [index]
  (let [ops (sort-by str (keys (:defs index)))
        used (into #{} (map first) (vals (:calls index)))
        run (fn [state op]
              (if (contains? (:visited state) (str op))
                state
                (if-let [{:keys [steps truncated]} (flow/trace index op)]
                  (let [{:keys [accesses links]} (trace-facts steps)]
                    (-> state
                        (update :entries conj (str op))
                        ;; A trace that hit the step limit did not finish the functions it entered.
                        (update :visited into (if truncated [(str op)] (map :in steps)))
                        (update :truncated into (when truncated [(str op)]))
                        (update :records (fn [records]
                                           (reduce (fn [m [r path kind site]]
                                                     (update-in m [r path kind] (fnil conj #{}) site))
                                                   records accesses)))
                        (update :links into links)
                        (update :boundaries into
                                (for [s steps :when (= :boundary (:kind s))]
                                  [(:reason s) (:via s) (:file s) (:row s) (:col s)]))))
                  (update state :untraced conj (str op)))))
        start {:entries [] :visited #{} :truncated [] :untraced [] :records {} :links #{} :boundaries #{}}]
    (reduce run (reduce run start (remove used ops)) ops)))

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

(defn- entity-name
  "The name that the code gives the entity: the most frequent parameter name of its records, or
  the function of its first record."
  [members]
  (let [labels (remove #(re-matches #"arg\d+" %) (keep :label members))
        {:keys [in name]} (first members)]
    (or (ffirst (sort-by (juxt (comp - val) key) (frequencies labels))) in name)))

(defn- entity-row [fields-of members]
  (let [paths (apply merge-with (partial merge-with into) (map fields-of members))
        sum (fn [rows] (counts (apply merge-with into (map val rows))))
        by-key (group-by (comp named-key key) paths)
        holders (frequencies (mapcat #(distinct (keep named-key (keys (fields-of %)))) members))]
    {:name (entity-name members)
     :records (count members)
     :kinds (frequencies (map :kind members))
     :functions (count (distinct (for [[_ by-kind] paths [_ sites] by-kind [_ _ _ in] sites] in)))
     :keys (->> (dissoc by-key nil)
                (map (fn [[k rows]] (merge {:key k :records (holders k)} (sum rows))))
                (sort-by (juxt (comp - :records) :key))
                vec)
     :levels (->> (group-by (comp level key) paths)
                  (keep (fn [[path rows]] (when path (merge {:path path} (sum rows)))))
                  (sort-by (comp vec :path))
                  vec)}))

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
        {:keys [entries truncated untraced records links boundaries]} (collect index)
        ordered (vec (sort-by (fn [r] [(kind-rank (:kind r)) (:name r) (str (:file r)) (or (:row r) 0) (or (:col r) 0)])
                              (keys records)))
        record-id (into {} (map-indexed (fn [i r] [r (str "r" (inc (long i)))])) ordered)
        grouped (->> (group (for [r ordered] {:id r :keys (into #{} (keep named-key) (keys (records r)))}))
                     (map (fn [{members :records}] [members (entity-row records members)]))
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
      :entries (count entries)
      :truncated truncated
      :untraced untraced
      :note "An entity groups records that share keys; a record is the origin of a value at a source site, not a type. Counts are sites, not executions."
      :entities (vec (for [[id _ row] (take limit grouped)] (into (array-map :id id) row)))
      :links (filterv #(and (shown (:from %)) (shown (:to %))) (entity-links links entity-of))
      :hidden {:entities (max 0 (- (count grouped) (long limit))) :loose-records (count loose)}
      :boundaries (boundary-rows boundaries)}
      entity
      (assoc :entity entity
             :records (vec (for [r chosen]
                             (assoc r :id (record-id r)
                                    :fields (mapv #(field-row sites %) (sort-by (comp vec key) (records r))))))
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
