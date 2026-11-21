(ns layout-descent.flow
  "Static, entry-scoped information flow. Steps are source evidence, not a runtime trace.
  Values retain known map slots so projecting one field does not read every sibling."
  (:require [clj-kondo.core :as kondo]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [edamame.core :as edamame])
  (:import (java.nio.file Files FileVisitOption Path)))

(defn source-files [dir]
  (with-open [paths (Files/walk (.toPath (.getCanonicalFile (io/file dir)))
                                (make-array FileVisitOption 0))]
    (->> (iterator-seq (.iterator paths)) (map #(.toFile ^Path %))
         (filter #(and (.isFile ^java.io.File %) (re-find #"\.clj[cs]?$" (.getName ^java.io.File %))))
         (map str) sort vec)))

(def ^:private reader-options
  {:all true :read-cond :allow :auto-resolve-ns true
   :readers (fn [tag] #(tagged-literal tag %))})

(defn- pos [form] ((juxt :row :col) (meta form)))

(defn- file-lang [file]
  (cond (str/ends-with? file ".cljs") :cljs (str/ends-with? file ".clj") :clj))

(defn- core-name [q]
  (when (and q (#{"cljs.core" "clojure.core"} (namespace q))) (symbol (name q))))

(defn- source-index [dir files entry requested]
  (let [root (.toPath (.getCanonicalFile (io/file dir)))
        relative #(str (.relativize root (.toPath (io/file %))))
        analysis (:analysis (kondo/run! {:lint files :cache false :skip-lint true
                                         :config {:hooks ^:replace {} :output {:analysis true}}}))
        platform #(or (:lang %) (file-lang (:filename %)))
        qualified #(symbol (str (:ns %)) (str (:name %)))
        defs (filter #(#{'defn 'defn-} (core-name (:defined-by %))) (:var-definitions analysis))
        langs (set (map platform (filter #(= entry (qualified %)) defs)))
        lang (or requested (when (= 1 (count langs)) (first langs))
                 (when (and (some #(= :cljs (file-lang %)) files) (not-any? #(= :clj (file-lang %)) files)) :cljs)
                 :clj)
        selected (filter #(or (nil? (file-lang %)) (= lang (file-lang %))) files)
        definitions (into {} (for [{:keys [filename row col] :as d} defs :when (= lang (platform d))]
                               [[filename row col] (qualified d)]))
        calls (into {} (for [{:keys [filename name-row name-col to name macro] :as u} (:var-usages analysis)
                             :when (and (= lang (platform u)) to name name-row name-col)]
                         [[(relative filename) name-row name-col] [(symbol (str to) (str name)) macro]]))]
    (reduce (fn [index file]
              (let [path (relative file)]
                (try
                  (reduce (fn [index form]
                            (if-let [op (definitions (into [file] (pos form)))]
                              (update-in index [:defs op] (fnil conj []) {:op op :file path :form form})
                              index))
                          index (edamame/parse-string-all (slurp file) (assoc reader-options :features #{lang})))
                  (catch Exception _ (update index :unparsed conj path)))))
            {:defs {} :calls calls :lang lang :file-count (count selected) :unparsed []} selected)))

;; A value has an optional producing step, known slots, and the ids of named facts it carries.
;; Consuming a value starts a NEW value: dependencies remain edges, not aliases of the result.
(def ^:private step-limit 2048)

(defn- tracks [values] (into (sorted-set) (mapcat :tracks) values))

(defn- step! [{:keys [log file op at]} form kind via values & [extra]]
  (let [id (count @log)
        site (if (:row (meta form)) form at)]
    (when (>= id step-limit) (throw (ex-info "Flow step limit" {::limit true})))
    (vswap! log conj (merge {:id id :kind kind :via (str via) :in (str op) :file file
                             :row (:row (meta site)) :col (:col (meta site))
                             :inputs (mapv :id (filter :id values)) ::tracks (tracks values)} extra))
    {:id id}))

(defn- pass! [ctx form kind via v]
  (merge v (step! ctx form kind via [v])))

(defn- boundary! [ctx form reason via vs & [extra]]
  (assoc (step! ctx form :boundary via vs (assoc extra :reason reason)) :unknown true))

(defn- named-key? [x] (or (keyword? x) (string? x)))

(defn- key-shape [k]
  (cond (:items k) (str "[" (str/join " " (map key-shape (:items k))) "]")
        (contains? k :literal) (pr-str (:literal k))
        :else "?"))

(defn- access-path [m k] (conj (vec (:path m)) (key-shape k)))

(defn- uncertain-key [ctx site via m k action values]
  (let [path (access-path m k)]
    (assoc (boundary! ctx site :dynamic-key via values
                      {:access action :path path :container (:id m) :key-input (:id k)})
           :path (if (= :read action) path (:path m)))))

(defn- read-field [ctx site m k]
  (let [known (get (:slots m) k)
        absent? (and (nil? known) (or (:closed m) (:missing m) (contains? m :literal)))
        ;; Only the selected slot supplies its contents. A miss depends on shape, not siblings.
        source (or known (:base m) (cond-> m (:unknown m) (dissoc :tracks)))
        path (access-path m {:literal k})
        info (step! ctx site :read "lookup" (if absent? [] [source])
                    {:field (pr-str k) :path path :container (:id m) ::introduces (not (or known absent?))
                     :resolution (cond known :known-slot absent? :known-miss :else :open)})]
    (assoc (cond known (assoc (merge known info) :present true)
                 absent? (assoc info :literal nil :missing true)
                 :else (assoc info :alias true :tracks #{(:id info)})) :path path)))

(defn- put-field [ctx site k v & [path]]
  (let [kind (cond (:alias v) :carry (:unknown v) :write :else :produce)
        info (step! ctx site kind "field" [v] {:field (pr-str k) :path (or path [(pr-str k)]) ::introduces true})]
    (assoc (merge v info) :alias true :tracks (conj (set (:tracks v)) (:id info)))))

(defn- assemble [ctx form via m slots]
  (let [closed (or (:closed m) (and (contains? m :literal) (nil? (:literal m))))
        base (or (:base m) (when-not closed m))
        parts (cond-> (vec (vals slots)) base (conj base))]
    (assoc (step! ctx form :assemble via parts) :slots slots :closed closed :base base :path (:path m)
           :tracks (tracks parts))))

(defn- read-key [ctx site via m k]
  (if (named-key? (:literal k)) (read-field ctx site m (:literal k))
      (uncertain-key ctx site via m k :read [m k])))

(defn- put-key [ctx site via m k v]
  (if (named-key? (:literal k))
    (assemble ctx site via m (assoc (:slots m) (:literal k)
                                    (put-field ctx site (:literal k) v (access-path m k))))
    ;; An unknown key can overwrite any slot; never install a fictitious slot named '?'.
    (uncertain-key ctx site via m k :write [m k v])))

(defn- edit-path [ctx form via m [k & more] leaf]
  (let [v (if more (edit-path ctx form via (read-key ctx form via m k) more leaf) (leaf m k))]
    (put-key ctx form via m k v)))

(declare trace-expr invoke bind-value)

(defn- body [ctx forms] (last (mapv #(trace-expr ctx %) forms)))

(defn- choice [ctx form via vs control]
  (assoc (step! ctx form :choice via vs (when control {:control (:id control)}))
         :unknown true :tracks (tracks vs)))

(defn- lookup [ctx form via m ks default]
  (let [v (reduce #(read-key ctx form via %1 %2) m ks)]
    (cond (or (empty? ks) (nil? default) (:present v)) v
          (:missing v) (pass! ctx form :pass "lookup default" default)
          :else (choice ctx form "lookup or default" [v default] nil))))

(defn- project [ctx site v i]
  (let [items (:items v)
        selected (get items i)
        miss? (and items (nil? selected))]
    (merge (if miss? {:literal nil} (or selected {:alias true}))
           (step! ctx site :project "nth" (if miss? [] [(or selected v)])
                  {:index i :container (:id v) :resolution (cond selected :known-slot miss? :known-miss :else :open)}))))

(defn- bind-value [ctx binding v]
  (cond
    (symbol? binding) (assoc-in ctx [:env binding] v)
    (vector? binding)
    (let [[positions tail] (split-with #(not (#{'& :as} %)) binding)]
      (when (or (empty? tail) (and (= :as (first tail)) (= 2 (count tail)) (symbol? (second tail))))
        (reduce (fn [c [i b]] (when c (bind-value c b (project c b v i))))
                (if (seq tail) (bind-value ctx (second tail) v) ctx) (map-indexed vector positions))))
    (and (map? binding) (every? #{:keys :as} (keys binding))
         (every? symbol? (:keys binding)))
    (reduce (fn [ctx sym]
              (assoc-in ctx [:env (symbol (name sym))]
                        (read-field ctx sym v (keyword sym))))
            (if-let [sym (:as binding)] (assoc-in ctx [:env sym] v) ctx) (:keys binding))
    :else nil))

(defn- let-body [ctx form]
  (loop [ctx ctx pairs (partition 2 (second form))]
    (if-let [[k expr] (first pairs)]
      (let [v (trace-expr ctx expr)]
        (if-let [inner (bind-value ctx k v)] (recur inner (next pairs))
                (boundary! ctx k :unsupported-binding "let" [v])))
      (body ctx (nnext form)))))

(defn- signatures [form definition?]
  (let [tail (if definition? (nnext form) (rest form))
        tail (cond-> tail (and (not definition?) (symbol? (first tail))) next)
        tail (drop-while #(or (string? %) (map? %)) tail)
        arities (if (vector? (first tail)) [tail] tail)]
    (filter #(vector? (first %)) arities)))

(defn- invoke [ctx site target args]
  (let [{:keys [op file form closure]} target
        id [file (pos form)]
        sig (some #(when (and (not-any? #{'&} (first %)) (= (count args) (count (first %)))) %)
                  (signatures form (not closure)))]
    (cond
      ((:stack ctx) id) (boundary! ctx site :recursive-call op args)
      (>= (count (:stack ctx)) 32) (boundary! ctx site :call-depth op args)
      (nil? sig) (boundary! ctx site :unsupported-arity op args)
      :else
      (let [passed (mapv #(pass! ctx site :pass op %) args)
            inner (assoc ctx :file file :op op :at form :env (or closure {}) :stack (conj (:stack ctx) id))
            bound (reduce (fn [c [param v]] (when c (bind-value c param v))) inner (map vector (first sig) passed))]
        (cond (nil? bound) (boundary! ctx site :unsupported-binding op passed)
              (and (map? (second sig)) (next (next sig)) (some (second sig) [:pre :post]))
              (boundary! ctx site :pre-post-contract op passed)
              :else (pass! inner (last sig) :return op (body bound (rest sig))))))))

(def ^:private consumers
  '#{+ - * / +' -' *' inc dec long int double quot mod rem max min str = not= < <= > >=
     odd? even? pos? neg? zero? nil? some? not boolean compare})

(def ^:private builtins
  (into consumers '#{get get-in assoc assoc-in update update-in deref swap! fnil conj disj}))

(declare apply-value)

(defn- snapshot [ctx site via ref]
  ;; Each deref (and each possible swap callback invocation) starts from an unknown value.
  (assoc (step! ctx site :state-read via [ref] {:resolution :open}) :unknown true))

(defn- apply-builtin [ctx site q args]
  (let [op (core-name q) [m k f & extra] args]
    (case op
      (get get-in) (if-let [ks (if (= op 'get) [k] (:items k))]
                     (lookup ctx site q m ks f)
                     (boundary! ctx site :dynamic-path q args {:access :read}))
      assoc (reduce (fn [m [k v]] (put-key ctx site q m k v)) m (partition 2 (rest args)))
      (assoc-in update update-in)
      (let [ks (if (= op 'update) [k] (:items k))]
        (if (seq ks)
          (edit-path ctx site q m ks (fn [parent key]
                                       (if (= op 'assoc-in) f
                                           (apply-value ctx site q f (cons (read-key ctx site q parent key) extra)))))
          (boundary! ctx site :dynamic-path q args {:access :write})))
      deref (if (= 1 (count args)) (snapshot ctx site q m) (boundary! ctx site :unsupported-arity q args))
      swap! (let [old (snapshot ctx site q m)
                  updated (apply-value ctx site q k (cons old (drop 2 args)))]
              (merge updated (step! ctx site :state-write q [m updated])))
      fnil {:wrapped m :defaults (vec (rest args))}
      (conj disj) (step! ctx site :transform q args)
      (step! ctx site :consume q args))))

(defn- apply-value [ctx site via f args]
  (cond (:fn f) (invoke ctx site (:fn f) args)
        (:builtin f) (apply-builtin ctx site (:builtin f) args)
        (:wrapped f)
        (apply-value ctx site via (:wrapped f)
                     (map-indexed (fn [i v]
                                    (if-let [d (get (:defaults f) i)]
                                      (if (contains? v :literal) (if (nil? (:literal v)) d v)
                                          (choice ctx site "fnil" [v d] nil)) v)) args))
        (and (keyword? (:literal f)) (= 1 (count args))) (read-field ctx site (first args) (:literal f))
        :else (boundary! ctx site :indirect-call via (cons f args))))

(defn- map-call [ctx form via [f coll]]
  (let [item (:element coll)
        ;; Reading a collection is not reading its attributes as fields of each item.
        elem (merge item (step! ctx form :iterate via [(or item coll)]))
        out (apply-value ctx form via f [elem])]
    (assoc (step! ctx form :collect via [out]) :element out :tracks (:tracks out))))

(defn- thread-body [ctx form last?]
  (reduce (fn [v expr]
            (let [[f & args] (if (seq? expr) expr [expr])
                  tmp (gensym "thread")
                  call (if last? (concat [f] args [tmp]) (list* f tmp args))]
              (trace-expr (assoc-in ctx [:env tmp] v) (with-meta (apply list call) (or (meta expr) (meta form))))))
          (trace-expr ctx (second form)) (nnext form)))

(defn- unique-target [ctx q]
  (let [defs (get (:defs ctx) q)] (when (= 1 (count defs)) (first defs))))

(defn- resolved [ctx sym]
  (get (:calls ctx) (into [(:file ctx)] (pos sym))))

(defn- short-circuit [ctx site op [expr & more :as args]]
  (if (empty? args) {:literal (when (= op 'and) true)}
      (let [v (trace-expr ctx expr)]
        (if more (let [c (step! ctx expr :consume "condition" [v])]
                   (choice ctx site op [v (short-circuit ctx site op more)] c)) v))))

(defn- call [ctx form]
  (let [[f & args] form
        ;; The reader expands @x to (clojure.core/deref x) with a positionless head.
        [q macro?] (if (and (= 'clojure.core/deref f) (nil? (:row (meta f))))
                     [(if (= :cljs (:lang ctx)) 'cljs.core/deref 'clojure.core/deref)]
                     (when (symbol? f) (resolved ctx f)))
        core (core-name q)
        ev #(trace-expr ctx %)
        vs #(mapv ev args)
        via (or q (when (symbol? f) f) "call")]
    (cond
      (contains? (:env ctx) f) (apply-value ctx form f (get (:env ctx) f) (vs))
      (keyword? f) (let [[m default] (vs)] (lookup ctx form "lookup" m [{:literal f}] default))
      (= 'quote f) (if (coll? (second form)) (boundary! ctx form :quoted-data f []) {:literal (second form)})
      (= 'do f) (body ctx args)
      (or (= 'if f) (#{'when 'when-not} core))
      (let [[test & branches] args
            control (step! ctx test :consume "condition" [(ev test)])]
        (choice ctx form f (if (= 'if f) (mapv ev branches) [(body ctx branches) {}]) control))
      (= 'case core)
      (let [[test & clauses] args
            c (step! ctx test :consume "case selector" [(ev test)])
            branches (take-nth 2 (rest clauses))]
        (choice ctx form via (mapv ev (cond-> (vec branches) (odd? (count clauses)) (conj (last clauses)))) c))
      (#{'and 'or} core) (short-circuit ctx form core args)
      (= 'as-> core) (let [[init sym & exprs] args]
                       (reduce #(trace-expr (assoc-in ctx [:env sym] %1) %2) (ev init) exprs))
      (or (= 'fn core) (= 'fn* f)) {:fn {:op (:op ctx) :file (:file ctx) :form form :closure (:env ctx)}}
      (= 'let core) (let-body ctx form)
      (#{'-> '->>} core) (thread-body ctx form (= '->> core))
      (builtins core) (apply-builtin ctx form q (vs))
      (and (#{'map 'mapv} core) (= 2 (count args))) (map-call ctx form via (vs))
      (= 'identity core) (pass! ctx form :pass via (ev (first args)))
      (= 'count core) (step! ctx form :inspect via (vs))
      (and (#{'sort 'vec} core) (= 1 (count args)))
      (let [v (ev (first args))]
        (merge (select-keys v [:element :tracks]) (step! ctx form :collect via [v])))
      ;; Mentioned locals may cross an unexpanded macro; this is not a claim of consumption.
      macro? (boundary! ctx form :unsupported-macro via
                        (keep (:env ctx) (distinct (filter symbol? (tree-seq coll? seq args)))))
      (unique-target ctx q) (invoke ctx form (unique-target ctx q) (vs))
      :else (boundary! ctx form (if (symbol? f) :opaque-call :indirect-call) via
                       (cond-> (vs) (not (symbol? f)) (conj (ev f)))))))

(defn- trace-expr [ctx form]
  (let [ctx (if (:row (meta form)) (assoc ctx :at form) ctx)]
    (cond
      (symbol? form)
      (let [q (first (resolved ctx form))]
        (cond (contains? (:env ctx) form) (get (:env ctx) form)
              (unique-target ctx q) {:fn (unique-target ctx q)}
              (builtins (core-name q)) {:builtin q}
              :else (boundary! ctx form :external-value (or q form) [])))
      (seq? form) (call ctx form)
      (map? form) (if (every? named-key? (keys form))
                    (assemble ctx form "map literal" {:closed true}
                              (into {} (for [[k expr] form] [k (put-field ctx expr k (trace-expr ctx expr))])))
                    (boundary! ctx form :dynamic-key "map literal" (mapv #(trace-expr ctx %) (mapcat identity form))))
      (or (vector? form) (set? form)) (let [vs (mapv #(trace-expr ctx %) form)]
                                        (cond-> (assoc (step! ctx form :assemble (if (set? form) "set" "vector") vs)
                                                       :tracks (tracks vs))
                                          (vector? form) (assoc :items vs)))
      (tagged-literal? form) (boundary! ctx form :tagged-literal (:tag form) [])
      :else {:literal form})))

(defn- report [ctx dir op truncated?]
  (let [steps @(:log ctx)
        uses (reduce (fn [m s]
                       (reduce #(update-in %1 [%2 (:kind s)] (fnil conj []) (:id s)) m (::tracks s))) {} steps)
        fields (for [s steps :when (::introduces s)]
                 {:at (:id s) :field (:field s) :uses (get uses (:id s) {})})]
    {:operation (str op) :root (str (.getCanonicalFile (io/file dir))) :scope :static
     :language (:lang ctx) :files (:file-count ctx) :unparsed (:unparsed ctx) :truncated truncated?
     :note "Possible source paths, not execution counts, allocation sizes or proof of safe removal."
     :fields (vec fields) :steps (mapv #(dissoc % ::introduces ::tracks) steps)
     :boundaries (mapv :id (filter #(= :boundary (:kind %)) steps))}))

(defn analyze [dir files operation & [lang]]
  (let [op (symbol operation)
        ctx (assoc (source-index dir files op lang) :log (volatile! []) :stack #{} :env {})
        {:keys [form file] :as target} (unique-target ctx op)]
    (when-not target (throw (ex-info (str "No unique top-level " (name (:lang ctx)) " defn for " operation ".")
                                     {:code :flow-operation :extra {:language (:lang ctx) :unparsed (:unparsed ctx)}})))
    (let [ctx (assoc ctx :file file :op op :at form)
          limited? (try
                     (doseq [[params] (signatures form true)]
                       (let [args (mapv (fn [i p] (assoc (step! ctx p :input (if (symbol? p) p (str "arg" i)) [])
                                                        :alias true)) (range) params)]
                         (invoke ctx form target args)))
                     false
                     (catch clojure.lang.ExceptionInfo e (if (::limit (ex-data e)) true (throw e))))]
      (report ctx dir op limited?))))
