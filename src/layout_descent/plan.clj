(ns layout-descent.plan
  "The plan: one checked-in EDN description of a layout, its access patterns and its workload."
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import [java.io PushbackReader StringReader]
           [java.nio.charset StandardCharsets]
           [java.nio.file Files Path StandardCopyOption]
           [java.security MessageDigest]))

(def version 1)
(def level-types #{:long :keyword :string})
(def leaf-types #{:double :long :object})
(def formats #{:hash :csf})
(def folds #{:sum :count})
(def max-levels 4)
(def ^:private default-fanout 16)

(def ^:private top-level-keys
  #{:layout/version :name :notes :levels :leaf :patterns :orders :serve :workload :objective :emit})

(def ^:private reserved-names #{"build" "descriptor" "plan-hash"})
(def ^:private id-re #"[a-z][a-z0-9-]*")

(defn- problem [code path message] {:code code :path path :message message})

(defn- id? [x] (and (keyword? x) (nil? (namespace x)) (some? (re-matches id-re (name x)))))

(defn permutations
  "All orderings of the level indices 0..n-1."
  [n]
  (letfn [(perms [xs]
            (if (empty? xs)
              [[]]
              (for [x xs p (perms (remove #{x} xs))] (into [x] p))))]
    (vec (perms (range n)))))

(defn bound-prefix
  "How many leading storage levels of perm the bind vector fixes with a key."
  [perm bind]
  (count (take-while #(= :key (nth bind %)) perm)))

(defn best-order
  "Index of the order that fixes the longest prefix for bind; the lowest index wins a tie."
  [orders bind]
  (->> orders
       (map-indexed (fn [i o] [(- (bound-prefix (:perm o) bind)) i]))
       sort
       first
       second))

;; ---- validation -----------------------------------------------------------

(defn- level-problems [levels]
  (if-not (and (vector? levels) (<= 1 (count levels) max-levels))
    [(problem :bad-levels [:levels] (str ":levels must be a vector of 1 to " max-levels " levels."))]
    (concat
     (keep-indexed
      (fn [i l]
        (when-not (and (map? l) (id? (:id l)) (level-types (:type l)))
          (problem :bad-level [:levels i]
                   (str "A level needs :id (a lower-case keyword) and :type, one of "
                        (vec (sort level-types)) "."))))
      levels)
     (when-not (apply distinct? (map :id levels))
       [(problem :duplicate-level [:levels] "Level ids must be distinct.")]))))

(defn- pattern-problem [n leaf-type i p]
  (let [path [:patterns i]]
    (cond
      (not (and (map? p) (id? (:id p))))
      (problem :bad-pattern-id path "A pattern needs :id, a lower-case keyword.")

      (or (reserved-names (name (:id p)))
          (contains? (ns-publics 'clojure.core) (symbol (name (:id p)))))
      (problem :reserved-pattern-id path
               (str "Pattern id " (:id p) " would shadow an existing name; choose another."))

      (not (and (vector? (:bind p)) (= n (count (:bind p))) (every? #{:key :scan} (:bind p))))
      (problem :bad-bind path (str ":bind must have " n " entries, each :key or :scan."))

      (not (or (nil? (:fold p)) (folds (:fold p))))
      (problem :bad-fold path (str ":fold must be absent or one of " (vec (sort folds)) "."))

      (and (= :sum (:fold p)) (= :object leaf-type))
      (problem :sum-needs-number path ":fold :sum needs a :double or :long leaf."))))

(defn- pattern-problems [patterns n leaf-type]
  (if-not (and (vector? patterns) (seq patterns))
    [(problem :bad-patterns [:patterns] ":patterns must be a non-empty vector.")]
    (concat
     (keep-indexed (partial pattern-problem n leaf-type) patterns)
     (let [ids (keep :id (filter map? patterns))]
       (when (and (seq ids) (not (apply distinct? ids)))
         [(problem :duplicate-pattern [:patterns] "Pattern ids must be distinct.")])))))

(defn- order-problems [orders n]
  (if-not (and (vector? orders) (seq orders))
    [(problem :bad-orders [:orders] ":orders must be a non-empty vector.")]
    (concat
     (keep-indexed
      (fn [i o]
        (when-not (and (map? o)
                       (vector? (:perm o))
                       (every? int? (:perm o))
                       (= (range n) (sort (:perm o)))
                       (formats (:format o)))
          (problem :bad-order [:orders i]
                   (str "An order needs :perm, an ordering of 0.." (dec n)
                        ", and :format, one of " (vec (sort formats)) "."))))
      orders)
     (let [keys (map (juxt :perm :format) (filter map? orders))]
        (when-not (= (count keys) (count (set keys)))
       [(problem :duplicate-order [:orders] "Two orders have the same :perm and :format.")])))))

(defn- serve-problems [serve ids order-count]
  (cond
    (nil? serve) []
    (not (map? serve)) [(problem :bad-serve [:serve] ":serve must map pattern ids to order indices.")]
    :else
    (keep (fn [[id i]]
            (cond
              (not (ids id))
              (problem :unknown-pattern [:serve id] (str ":serve names " id ", which is not a pattern."))
              (not (and (int? i) (< -1 i order-count)))
              (problem :bad-serve-index [:serve id] (str ":serve index must be 0.." (dec order-count) "."))))
          serve)))

(defn- finite-number? [x]
  (and (number? x) (Double/isFinite (double x))))

(defn- fanout? [fanout n]
  (and (vector? fanout) (= n (count fanout)) (every? #(and (pos-int? %) (<= % Integer/MAX_VALUE)) fanout)))

(defn- degree?
  "True when degree has one entry for each level: nil, or a positive integer within the fanout
  of that level. An unusable fanout is reported on its own and sets no limit here."
  [degree fanout n]
  (let [limits (cond (nil? fanout) (repeat n default-fanout)
                     (fanout? fanout n) fanout
                     :else (repeat n Integer/MAX_VALUE))]
    (and (vector? degree)
         (= n (count degree))
         (every? true? (map (fn [d limit] (or (nil? d) (and (pos-int? d) (<= d limit)))) degree limits)))))

(defn- workload-problems [{:keys [fanout fill degree seed weights] :as workload} ids n]
  (cond
    (nil? workload) []
    (not (map? workload)) [(problem :bad-workload [:workload] ":workload must be a map.")]
    :else
    (remove nil?
            [(when-not (or (nil? fanout) (fanout? fanout n))
               (problem :bad-fanout [:workload :fanout] (str ":fanout needs " n " positive integers no larger than " Integer/MAX_VALUE ".")))
             (when-not (or (nil? fill)
                           (and (vector? fill) (= n (count fill)) (every? #(and (number? %) (< 0 % ) (<= % 1)) fill)))
               (problem :bad-fill [:workload :fill] (str ":fill needs " n " numbers in (0, 1].")))
             (when-not (or (nil? degree) (degree? degree fanout n))
               (problem :bad-degree [:workload :degree]
                        (str ":degree needs " n " entries, each nil or a positive integer no larger than the :fanout of its level.")))
             (when-not (or (nil? seed) (int? seed))
               (problem :bad-seed [:workload :seed] ":seed must be an integer."))
             (when-not (or (nil? weights)
                           (and (map? weights) (every? ids (keys weights))
                                (every? #(and (finite-number? %) (pos? (double %))) (vals weights))))
               (problem :bad-weights [:workload :weights]
                        ":weights must map pattern ids to finite positive numbers."))])))

(defn- objective-problems [{:keys [ns-per-byte min-gain] :as objective}]
  (cond
    (nil? objective) []
    (not (map? objective)) [(problem :bad-objective [:objective] ":objective must be a map.")]
    :else
    (remove nil?
            [(when-not (or (nil? ns-per-byte) (and (finite-number? ns-per-byte) (not (neg? ns-per-byte))))
               (problem :bad-ns-per-byte [:objective :ns-per-byte] ":ns-per-byte must be zero or more."))
             (when-not (or (nil? min-gain) (and (finite-number? min-gain) (< 0 (double min-gain) 1)))
               (problem :bad-min-gain [:objective :min-gain] ":min-gain must be between 0 and 1."))])))

(defn- notes-problems [notes]
  (when-not (or (nil? notes) (string? notes) (and (vector? notes) (every? string? notes)))
    [(problem :bad-notes [:notes] ":notes must be a string or a vector of strings.")]))

(defn- emit-problems [emit]
  (when-not (or (nil? emit)
                (and (map? emit) (symbol? (:ns emit))
                     (re-matches #"[A-Za-z_][A-Za-z0-9_-]*(\.[A-Za-z_][A-Za-z0-9_-]*)*" (str (:ns emit)))
                     (string? (:path emit)) (not (str/blank? (:path emit)))))
    [(problem :bad-emit [:emit] ":emit needs a valid namespace symbol and a non-blank path.")]))

(defn problems
  "Errors that make the plan unusable. Empty when the plan is valid."
  [plan]
  (if-not (map? plan)
    [(problem :not-a-map [] "A plan is an EDN map.")]
    (let [base
          (concat
           (for [k (sort-by str (remove top-level-keys (keys plan)))]
             (problem :unknown-key [k] (str k " is not a plan key. The keys are "
                                            (str/join " " (sort-by str top-level-keys)) ".")))
           (when-not (= version (:layout/version plan))
             [(problem :bad-version [:layout/version] (str ":layout/version must be " version "."))])
           (when-not (and (or (symbol? (:name plan)) (string? (:name plan)))
                          (re-matches id-re (str (:name plan))))
             [(problem :bad-name [:name] ":name must be a lower-case symbol such as risk.")])
           (level-problems (:levels plan))
           (when-not (and (map? (:leaf plan)) (leaf-types (:type (:leaf plan))))
             [(problem :bad-leaf [:leaf] (str ":leaf needs :type, one of " (vec (sort leaf-types)) "."))]))]

      (if (seq base)
        (vec base)
        (let [n (count (:levels plan))
              shape (concat (pattern-problems (:patterns plan) n (:type (:leaf plan)))
                            (order-problems (:orders plan) n))]

          (if (seq shape)
            (vec shape)
            (let [ids (set (map :id (:patterns plan)))]
              (vec (concat (serve-problems (:serve plan) ids (count (:orders plan)))
                           (workload-problems (:workload plan) ids n)
                           (objective-problems (:objective plan))
                           (notes-problems (:notes plan))
                           (emit-problems (:emit plan)))))))))))

;; ---- normal form, hash ----------------------------------------------------

(defn- ordered
  "m as an array map: the given keys first, then the others sorted."
  [m ks]
  (let [front (filter #(contains? m %) ks)
        back (sort-by pr-str (remove (set ks) (keys m)))]
    (apply array-map (mapcat (fn [k] [k (get m k)]) (concat front back)))))

(defn normalize
  "The plan with every default filled and every map in one fixed key order. Assumes a valid plan.
  The optional keys :notes and :workload :degree are absent unless the plan gives them, so a plan
  without them keeps its hashes."
  [plan]
  (let [n (count (:levels plan))
        patterns (mapv #(ordered % [:id :bind :fold]) (:patterns plan))
        orders (mapv #(ordered % [:perm :format]) (:orders plan))
        workload (:workload plan)
        degree (:degree workload)]
    ;; One array-map call: assoc would turn a map of more than eight keys into a hash map and lose
    ;; the key order.
    (apply array-map
           (concat
            [:layout/version version
             :name (symbol (str (:name plan)))]
            (when-some [notes (:notes plan)] [:notes notes])
            [:levels (mapv #(ordered % [:id :type]) (:levels plan))
             :leaf (:leaf plan)
             :patterns patterns
             :orders orders
             :serve (into (sorted-map)
                          (map (fn [{:keys [id bind]}]
                                 [id (get (:serve plan) id (best-order orders bind))]))
                          patterns)
             :workload (apply array-map
                              (concat
                               [:fanout (or (:fanout workload) (vec (repeat n default-fanout)))
                                :fill (or (:fill workload) (vec (repeat n 1.0)))]
                               ;; All nil means what no :degree means, so it has the same normal form.
                               (when (some some? degree) [:degree degree])
                               [:seed (or (:seed workload) 1)
                                :weights (into (sorted-map)
                                               (map (fn [{:keys [id]}] [id (get-in workload [:weights id] 1)]))
                                               patterns)]))
             :objective (array-map
                         :ns-per-byte (or (get-in plan [:objective :ns-per-byte]) 0.0)
                         :min-gain (or (get-in plan [:objective :min-gain]) 0.05))]
            (when-let [emit (:emit plan)] [:emit (ordered emit [:ns :path])])))))

(defn layout-key
  "The part of a normal plan that decides the emitted code."
  [plan]
  {:version (:layout/version plan)
   :levels (mapv (juxt :id :type) (:levels plan))
   :leaf (:type (:leaf plan))
   :patterns (mapv (juxt :id :bind :fold) (:patterns plan))
   :orders (mapv (juxt :perm :format) (:orders plan))
   :serve (vec (:serve plan))})

(defn task-key
  "The part of a normal plan that two plans must share before their costs compare."
  [plan]
  (assoc (select-keys (layout-key plan) [:levels :leaf :patterns])
         :workload (:workload plan)))

(defn- short-digest [text]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") (.getBytes ^String text StandardCharsets/UTF_8))]
    (subs (apply str (map #(format "%02x" %) digest)) 0 12)))

(defn plan-hash
  "Twelve hex digits that identify the layout decisions of a plan."
  [plan]
  (short-digest (pr-str (layout-key (normalize plan)))))

(defn task-hash
  "Twelve hex digits that identify the task of a plan: its levels, leaf, patterns and workload."
  [plan]
  (short-digest (pr-str (task-key (normalize plan)))))

(defn warnings
  "Notes about a valid normal plan that do not block it."
  [plan]
  (let [orders (:orders plan)
        used (set (vals (:serve plan)))]
    (vec
     (concat
      (for [i (range (count orders)) :when (not (used i))]
        (problem :unused-order [:orders i] (str "No pattern is served by order " i ".")))
      (for [{:keys [id bind]} (:patterns plan)
            :let [order (nth orders (get (:serve plan) id))
                  prefix (bound-prefix (:perm order) bind)
                  bound (count (filter #{:key} bind))]
            :when (< prefix bound)]
        (problem :partial-index [:patterns id]
                 (str "Pattern " id " fixes " bound " levels, but order " (get (:serve plan) id)
                      " " (:perm order) " reaches only " prefix " of them before it must scan."
                      " A gain from adding an order for this pattern measures the missing index,"
                      " not the layout format.")))
      (when (and (< 1 (count orders)) (zero? (double (get-in plan [:objective :ns-per-byte]))))
        [(problem :memory-not-priced [:objective :ns-per-byte]
                  (str "Memory is free under this objective, so an extra order can only help."
                       " Set :ns-per-byte to make memory count."))])))))

;; ---- search space ---------------------------------------------------------

(defn prune-unused
  "The plan without the orders that serve no pattern."
  [plan]
  (let [plan (normalize plan)
        used (vec (sort (set (vals (:serve plan)))))
        index (zipmap used (range))]
    (assoc plan
           :orders (mapv (:orders plan) used)
           :serve (into (sorted-map) (map (fn [[id i]] [id (index i)])) (:serve plan)))))

(defn- prefix-on [plan id perm]
  (let [bind (:bind (first (filter #(= id (:id %)) (:patterns plan))))]
    (bound-prefix perm bind)))

(defn- add-moves [plan]
  (let [orders (:orders plan)
        present (set (map (juxt :perm :format) orders))]
    (for [perm (permutations (count (:levels plan)))
          :let [gain (vec (for [[id i] (:serve plan)
                                :when (> (prefix-on plan id perm)
                                         (prefix-on plan id (:perm (nth orders i))))]
                            id))]
          :when (seq gain)
          fmt [:csf :hash]
          :when (not (present [perm fmt]))]
      {:move {:op :add-order :perm perm :format fmt :serves gain}
       :why (str "Order " perm " fixes a longer prefix for " (str/join ", " gain) ".")
       :plan (-> plan
                 (update :orders conj (array-map :perm perm :format fmt))
                 (update :serve into (map (fn [id] [id (count orders)])) gain))})))

(defn- format-moves [plan]
  (let [present (set (map (juxt :perm :format) (:orders plan)))]
    (for [[i {:keys [perm format]}] (map-indexed vector (:orders plan))
          :let [fmt (if (= :csf format) :hash :csf)]
          :when (not (present [perm fmt]))]
      {:move {:op :set-format :order i :format fmt}
       :why (str "Store order " i " " perm " as " fmt " instead of " format ".")
       :plan (assoc-in plan [:orders i :format] fmt)})))

(defn- drop-moves [plan]
  (let [orders (:orders plan)]
    (when (> (count orders) 1)
      (for [i (range (count orders))
            :let [kept (vec (concat (subvec orders 0 i) (subvec orders (inc i))))]]
        {:move {:op :drop-order :order i}
         :why (str "Remove order " i " " (:perm (nth orders i)) " and serve its patterns from the rest.")
         :plan (assoc plan
                      :orders kept
                      :serve (into (sorted-map)
                                   (map (fn [{:keys [id bind]}]
                                          (let [s (get (:serve plan) id)]
                                            [id (cond (= s i) (best-order kept bind)
                                                      (> s i) (dec s)
                                                      :else s)])))
                                   (:patterns plan)))}))))

(defn- serve-moves [plan]
  (let [orders (:orders plan)]
    (for [[id s] (:serve plan)
          [i {:keys [perm]}] (map-indexed vector orders)
          :when (and (not= i s)
                     (>= (prefix-on plan id perm) (prefix-on plan id (:perm (nth orders s)))))]
      {:move {:op :serve :pattern id :order i}
       :why (str "Serve " id " from order " i " " perm ", which fixes as long a prefix.")
       :plan (assoc-in plan [:serve id] i)})))

(defn neighbors
  "Every plan one decision away from this one, each with its move and the reason for it."
  [plan]
  (let [plan (normalize plan)
        own (plan-hash plan)]
    (->> (concat (add-moves plan) (format-moves plan) (drop-moves plan) (serve-moves plan))
         (map #(assoc % :hash (plan-hash (:plan %))))
         (remove #(= own (:hash %)))
         (reduce (fn [[seen out] c]
                   (if (seen (:hash c)) [seen out] [(conj seen (:hash c)) (conj out c)]))
                 [#{} []])
         second)))

;; ---- files ----------------------------------------------------------------

(defn- render-value [v pad]
  (if (and (vector? v) (seq v) (or (every? map? v) (every? string? v)))
    (str "[" (str/join (str "\n" pad) (map pr-str v)) "]")
    (pr-str v)))

(defn render
  "The normal plan as EDN text. Equal plans give equal text."
  [plan]
  (str "{"
       (str/join "\n "
                 (map (fn [[k v]]
                        (str k " " (render-value v (apply str (repeat (+ 3 (count (str k))) " ")))))
                      (normalize plan)))
       "}\n"))

(defn read-plan
  "Read a plan file. Throws ex-info with :code :invalid-plan and :problems when it is not usable."
  [path]
  (let [plan (try
               (with-open [r (PushbackReader. (StringReader. (slurp (str path))))]
                  (let [eof (Object.)
                        opts {:eof eof}
                        value (edn/read opts r)]
                    (when (or (identical? eof value) (not (identical? eof (edn/read opts r))))
                      (throw (IllegalArgumentException. "Expected exactly one EDN value.")))
                    value))
               (catch Exception e
                 (throw (ex-info (str "Cannot read plan " path ": " (ex-message e))
                                 {:code :unreadable-plan :path (str path)}))))
        found (problems plan)]
    (when (seq found)
      (throw (ex-info (str "Plan " path " is not valid.")
                      {:code :invalid-plan :path (str path) :problems found})))
    (normalize plan)))

(defn stage-text!
  "Prepare a unique temporary file on the destination filesystem. The caller removes it on failure."
  ^Path [path text]
  (let [target (.toAbsolutePath (Path/of (str path) (make-array String 0)))
        dir (.getParent target)
        attrs (make-array java.nio.file.attribute.FileAttribute 0)]
    (Files/createDirectories dir attrs)
    (let [tmp (Files/createTempFile dir ".layout-descent-" ".tmp" attrs)]
      (try
        (Files/writeString tmp text StandardCharsets/UTF_8 (make-array java.nio.file.OpenOption 0))
        tmp
        (catch Throwable e
          (Files/deleteIfExists tmp)
          (throw e))))))

(defn publish-text! [^Path tmp path]
  (let [target (.toAbsolutePath (Path/of (str path) (make-array String 0)))]
    (Files/move tmp target
                (into-array java.nio.file.CopyOption
                            [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
    (str target)))

(defn write-text!
  "Replace the file at path in one step, with no shared temporary filename."
  [path text]
  (let [tmp (stage-text! path text)]
    (try
      (publish-text! tmp path)
      (finally (Files/deleteIfExists tmp)))))
