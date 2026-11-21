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
        constants (into {} (for [{:keys [filename row col] :as d} (:var-definitions analysis)
                                 :when (and (#{'def 'defonce} (core-name (:defined-by d))) (= lang (platform d)))]
                             [[filename row col] (qualified d)]))
        calls (into {} (for [{:keys [filename name-row name-col to name macro] :as u} (:var-usages analysis)
                             :when (and (= lang (platform u)) to name name-row name-col)]
                         [[(relative filename) name-row name-col] [(symbol (str to) (str name)) macro]]))]
    (reduce (fn [index file]
              (let [path (relative file)]
                (try
                  (reduce (fn [index form]
                            (let [at (into [file] (pos form))
                                  init (when (and (seq? form) (<= 3 (count form))) (last form))]
                              (cond (definitions at)
                                    (update-in index [:defs (definitions at)] (fnil conj [])
                                               {:op (definitions at) :file path :form form})
                                    ;; A def of a literal collection is a value that a trace can read.
                                    (and (constants at) (or (map? init) (vector? init) (set? init)))
                                    (assoc-in index [:vars (constants at)] {:op (constants at) :file path :form init})
                                    :else index)))
                          index (edamame/parse-string-all (slurp file) (assoc reader-options :features #{lang})))
                  (catch Exception _ (update index :unparsed conj path)))))
            {:defs {} :vars {} :calls calls :lang lang :file-count (count selected) :unparsed []} selected)))

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

(defn- literal-shape [v]
  (let [x (:literal v)]
    (when (contains? v :literal)
      (cond (nil? x) :nil (string? x) :string (keyword? x) :keyword
            (number? x) :number (boolean? x) :boolean))))

(defn- pass! [ctx form kind via v]
  (merge v (step! ctx form kind via [v] {::shape (literal-shape v)})))

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
        info (step! ctx site kind "field" [v] {:field (pr-str k) :path (or path [(pr-str k)]) ::introduces true
                                                             ::shape (literal-shape v)})]
    (assoc (merge v info) :alias true :tracks (conj (set (:tracks v)) (:id info)))))

(defn- assemble [ctx form via m slots]
  (let [closed (or (:closed m) (and (contains? m :literal) (nil? (:literal m))))
        base (or (:base m) (when-not closed m))
        parts (cond-> (vec (vals slots)) base (conj base))]
    (assoc (step! ctx form :assemble via parts {::onto (:id m)}) :slots slots :closed closed :base base :path (:path m)
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
    (and (map? binding)
         (every? (fn [[k xs]]
                   (or (= :as k) (and (keyword? k) (= "keys" (name k)) (every? symbol? xs)))) binding))
    (reduce (fn [ctx [directive syms]]
              (reduce (fn [ctx sym]
                        (let [local (name sym)
                              field (keyword (or (namespace directive) (namespace sym)) local)]
                          (assoc-in ctx [:env (symbol local)] (read-field ctx sym v field))))
                      ctx syms))
            (if-let [sym (:as binding)] (assoc-in ctx [:env sym] v) ctx) (dissoc binding :as))
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
      (and (:local ctx) (seq (:dynamic ctx)) (not closure) sig)
      (boundary! ctx site :unmodeled-dynamic-binding op args)
      (and (:local ctx) (seq (:stack ctx)) (not closure) sig)
      (assoc (step! ctx site :call op args {::args (mapv #(hash-map :id (:id %) :shape (literal-shape %)) args)}) :unknown true)
      ((:stack ctx) id) (boundary! ctx site :recursive-call op args)
      (>= (count (:stack ctx)) 32) (boundary! ctx site :call-depth op args)
      (nil? sig) (boundary! ctx site :unsupported-arity op args)
      :else
      (let [passed (mapv #(pass! ctx site :pass op %) args)
            inner (-> ctx
                      (assoc :file file :op op :at form :env (or closure {}) :stack (conj (:stack ctx) id))
                      (update :unmodeled-dynamic (fnil into #{}) (keys (:dynamic ctx)))
                      (dissoc :dynamic))
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

(declare each)

(defn- map-call [ctx form via [f coll]]
  (let [out (apply-value ctx form via f [(each ctx form via coll)])]
    (assoc (step! ctx form :collect via [out]) :element out :tracks (:tracks out))))

;; Functions of one collection whose result holds the same elements.
(def ^:private copies '#{seq sort vec set reverse distinct rest next butlast doall not-empty shuffle vals})

;; Functions of [f coll] that call f with an element and return the same elements.
(def ^:private tests '#{filter filterv remove take-while drop-while sort-by})

;; Functions of [n coll] whose result holds the same elements.
(def ^:private slices '#{take drop take-last drop-last take-nth})

(def ^:private picks '#{first second last peek rand-nth})

(defn- thread-step [ctx form v expr last?]
  (let [[f & args] (if (seq? expr) expr [expr])
        tmp (gensym "thread")
        call (if last? (concat [f] args [tmp]) (list* f tmp args))]
    (trace-expr (assoc-in ctx [:env tmp] v) (with-meta (apply list call) (or (meta expr) (meta form))))))

(defn- thread-body [ctx form last?]
  (reduce #(thread-step ctx form %1 %2 last?) (trace-expr ctx (second form)) (nnext form)))

(defn- conditional-thread
  "cond-> and cond->>: every step may run or not, so its result is a choice with the value before."
  [ctx form via last?]
  (let [[init & clauses] (rest form)]
    (reduce (fn [v [test expr]]
              (let [control (step! ctx test :consume "condition" [(trace-expr ctx test)])]
                (choice ctx expr via [(thread-step ctx form v expr last?) v] control)))
            (trace-expr ctx init) (partition 2 clauses))))

(defn- bind-test
  "if-let, when-let, if-some and when-some: the branch that sees the binding, or the other one."
  [ctx form via two-sided?]
  (let [[[binding expr] & branches] (rest form)
        v (trace-expr ctx expr)
        control (step! ctx expr :consume "condition" [v])
        inner (bind-value ctx binding v)]
    (cond (nil? inner) (boundary! ctx binding :unsupported-binding via [v])
          two-sided? (choice ctx form via [(trace-expr inner (first branches))
                                           (if (next branches) (trace-expr ctx (second branches)) {})] control)
          :else (choice ctx form via [(body inner branches) {}] control))))

(defn- try-body
  "The value of a try: its body or one of its handlers. A caught exception is an unknown value."
  [ctx form]
  (let [clause? #(and (seq? %) ('#{catch finally} (first %)))
        main (or (body ctx (remove clause? (rest form))) {})
        handlers (vec (for [[kind & more :as clause] (filter clause? (rest form))
                            :let [[_ sym & forms] more
                                  v (if (= 'catch kind)
                                      (body (assoc-in ctx [:env sym] (boundary! ctx clause :caught "catch" [])) forms)
                                      (do (body ctx more) nil))]
                            :when v]
                        v))]
    (if (seq handlers) (choice ctx form "try" (into [main] handlers) nil) main)))

(defn- each
  "The element of a collection: the known one, or a step that stands for every element."
  [ctx form via coll]
  (let [item (:element coll)]
    ;; Reading a collection is not reading its attributes as fields of each item.
    (merge item (step! ctx form :iterate via [(or item coll)] {::element (some? item)}))))

(defn- same-items
  "A collection with the elements of another one: sorted, filtered, shortened or copied."
  [ctx form via coll]
  (merge (select-keys coll [:element :tracks]) (step! ctx form :collect via [coll] {::same true})))

(defn- pick
  "One element of a collection, such as first or last."
  [ctx form via coll]
  (let [item (:element coll)]
    (merge (or item {:alias true})
           (step! ctx form :project via [(or item coll)]
                  {:container (:id coll) :resolution (if item :known-slot :open)}))))

(defn- comprehension
  "doseq and for: every binding is an element of its collection. :let binds, :when and :while test."
  [ctx form via collect?]
  (let [[bindings & forms] (rest form)
        inner (reduce (fn [c [b expr]]
                        (when c
                          (cond (= :let b) (reduce (fn [c [k e]] (when c (bind-value c k (trace-expr c e))))
                                                   c (partition 2 expr))
                                (keyword? b) (do (step! c expr :consume "condition" [(trace-expr c expr)]) c)
                                :else (bind-value c b (each c expr via (trace-expr c expr))))))
                      ctx (partition 2 bindings))]
    (if (nil? inner)
      (boundary! ctx form :unsupported-binding via [])
      (let [out (body inner forms)]
        (if collect?
          (assoc (step! ctx form :collect via [out]) :element out :tracks (:tracks out))
          {:literal nil})))))

(defn- unique-target [ctx q]
  (let [defs (get (:defs ctx) q)] (when (= 1 (count defs)) (first defs))))

(defn- resolved [ctx sym]
  (get (:calls ctx) (into [(:file ctx)] (pos sym))))

(defn- binding-body [ctx form via]
  (let [[bindings & forms] (rest form)]
    (if (and (vector? bindings) (even? (count bindings)))
      (let [pairs (partition 2 bindings)
            targets (mapv (fn [[target]]
                            (when (symbol? target) (first (resolved ctx target)))) pairs)
            ;; All initializers see the outer bindings, even those after a rebinding.
            values (mapv (fn [[_ init]] (trace-expr ctx init)) pairs)]
        (if (every? #(and % (namespace %) (not= "<unknown-namespace>" (namespace %))) targets)
          (body (update ctx :dynamic (fnil into {}) (map vector targets values)) forms)
          (boundary! ctx form :unsupported-binding via values)))
      (boundary! ctx form :unsupported-binding via []))))

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
      (contains? (:dynamic ctx) q) (apply-value ctx form via (get (:dynamic ctx) q) (vs))
      (contains? (:unmodeled-dynamic ctx) q) (boundary! ctx form :unmodeled-dynamic-binding via (vs))
      (keyword? f) (let [[m default] (vs)] (lookup ctx form "lookup" m [{:literal f}] default))
      (= 'quote f) (if (coll? (second form)) (boundary! ctx form :quoted-data f []) {:literal (second form)})
      (= 'do f) (body ctx args)
      (or (= 'if f) (#{'if-not 'when 'when-not} core))
      (let [[test & branches] args
            control (step! ctx test :consume "condition" [(ev test)])]
        (choice ctx form f (if (or (= 'if f) (= 'if-not core)) (mapv ev branches) [(body ctx branches) {}]) control))
      (#{'if-let 'if-some} core) (bind-test ctx form via true)
      (#{'when-let 'when-some} core) (bind-test ctx form via false)
      (= 'cond core)
      (let [branches (mapv (fn [[test expr]]
                             [(when-not (keyword? test) (step! ctx test :consume "condition" [(ev test)])) (ev expr)])
                           (partition 2 args))]
        (choice ctx form via (mapv second branches) (first (keep first branches))))
      (#{'cond-> 'cond->>} core) (conditional-thread ctx form via (= 'cond->> core))
      (or (= 'throw f) (= 'throw core)) (step! ctx form :consume "throw" (vs))
      (= 'try f) (try-body ctx form)
      (#{'doseq 'for} core) (comprehension ctx form via (= 'for core))
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
      (= 'binding core) (binding-body ctx form via)
      (#{'-> '->> 'some-> 'some->>} core) (thread-body ctx form (boolean (#{'->> 'some->>} core)))
      (builtins core) (apply-builtin ctx form q (vs))
      (and (#{'map 'mapv 'keep} core) (= 2 (count args))) (map-call ctx form via (vs))
      (and (tests core) (= 2 (count args)))
      (let [[f coll] (vs)]
        (step! ctx form :consume via [(apply-value ctx form via f [(each ctx form via coll)])])
        (same-items ctx form via coll))
      (and (= 'mapcat core) (= 2 (count args)))
      (let [[f coll] (vs)
            out (apply-value ctx form via f [(each ctx form via coll)])]
        (cond-> (assoc (step! ctx form :collect via [out]) :tracks (:tracks out))
          (:element out) (assoc :element (:element out))))
      (and (= 'some core) (= 2 (count args)))
      (let [[f coll] (vs)] (pass! ctx form :pass via (apply-value ctx form via f [(each ctx form via coll)])))
      (and (#{'every? 'not-any? 'not-every?} core) (= 2 (count args)))
      (let [[f coll] (vs)] (step! ctx form :consume via [(apply-value ctx form via f [(each ctx form via coll)])]))
      (and (= 'reduce core) (#{2 3} (count args)))
      (let [[f a b] (vs)
            [init coll] (if b [a b] [nil a])
            item (each ctx form via coll)
            ;; The function runs once here: its accumulator is the start value, not an earlier result.
            out (apply-value ctx form via f [(or init item) item])]
        (choice ctx form via (cond-> [out] init (conj init)) nil))
      (and (#{'merge 'into} core) (seq args) (<= (count args) (if (= 'into core) 2 64)))
      (choice ctx form via (vs) nil)
      (and (= 'select-keys core) (= 2 (count args)) (vector? (second args)) (every? named-key? (second args)))
      (let [m (ev (first args))]
        (assemble ctx form via {:closed true}
                  (into {} (for [k (second args)] [k (put-field ctx form k (read-field ctx form m k))]))))
      (and (= 'dissoc core) (seq args))
      (let [[m & ks] (vs)]
        (merge (cond-> m (:slots m) (update :slots #(apply dissoc % (keep :literal ks))))
               (step! ctx form :pass via [m] {::removed (when (every? #(contains? % :literal) ks)
                                                         (mapv (comp pr-str :literal) ks))})))
      (and (= 'contains? core) (= 2 (count args)))
      (let [[m k] (vs)] (step! ctx form :consume via [(read-key ctx form via m k)]))
      (and (picks core) (= 1 (count args)))
      (let [[coll] (vs)
            index ({'first 0 'second 1} core)]
        (if (and index (:items coll)) (project ctx form coll index) (pick ctx form via coll)))
      (and (slices core) (= 2 (count args))) (same-items ctx form via (second (vs)))
      (= 'identity core) (pass! ctx form :pass via (ev (first args)))
      (= 'count core) (step! ctx form :inspect via (vs))
      (and (copies core) (= 1 (count args))) (same-items ctx form via (ev (first args)))
      ;; Mentioned locals may cross an unexpanded macro; this is not a claim of consumption.
      macro? (boundary! ctx form :unsupported-macro via
                        (keep (:env ctx) (distinct (filter symbol? (tree-seq coll? seq args)))))
      (unique-target ctx q) (invoke ctx form (unique-target ctx q) (vs))
      :else (boundary! ctx form (if (symbol? f) :opaque-call :indirect-call) via
                       (cond-> (vs) (not (symbol? f)) (conj (ev f)))))))

(defn- constant
  "The value of a def whose init is a literal collection. One trace reads the literal once."
  [ctx q]
  (or (get @(:constants ctx) q)
      (let [{:keys [op file form]} (get (:vars ctx) q)
            v (trace-expr (-> ctx (assoc :file file :op op :at form :env {})
                              (dissoc :dynamic :unmodeled-dynamic)) form)]
        (vswap! (:constants ctx) assoc q v)
        v)))

(defn- trace-expr [ctx form]
  (let [ctx (if (:row (meta form)) (assoc ctx :at form) ctx)]
    (cond
      (symbol? form)
      (let [q (first (resolved ctx form))]
        (cond (contains? (:env ctx) form) (get (:env ctx) form)
              (contains? (:dynamic ctx) q) (get (:dynamic ctx) q)
              (contains? (:unmodeled-dynamic ctx) q) (boundary! ctx form :unmodeled-dynamic-binding q [])
              (unique-target ctx q) {:fn (unique-target ctx q)}
              (builtins (core-name q)) {:builtin q}
              (get (:vars ctx) q) (constant ctx q)
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

(defn- report [ctx dir op steps truncated?]
  (let [uses (reduce (fn [m s]
                       (reduce #(update-in %1 [%2 (:kind s)] (fnil conj []) (:id s)) m (::tracks s))) {} steps)
        fields (for [s steps :when (::introduces s)]
                 {:at (:id s) :field (:field s) :uses (get uses (:id s) {})})]
    {:operation (str op) :root (str (.getCanonicalFile (io/file dir))) :scope :static
     :language (:lang ctx) :files (:file-count ctx) :unparsed (:unparsed ctx) :truncated truncated?
     :note "Possible source paths, not execution counts, allocation sizes or proof of safe removal."
     :fields (vec fields) :steps (mapv #(dissoc % ::introduces ::tracks ::onto ::element ::same ::shape ::removed) steps)
     :boundaries (mapv :id (filter #(= :boundary (:kind %)) steps))}))

(defn index
  "The definitions and the resolved calls of a source tree in one language, for any number of
  traces. entry, a qualified symbol or nil, chooses the language when lang is nil."
  [dir files entry lang]
  (source-index dir files entry lang))

(defn trace
  "The steps of one operation as {:steps [...] :truncated bool}, or nil when op is not one
  top-level defn of the index. A step keeps the keys that the answer of analyze leaves out:
  ::tracks, ::introduces, ::onto (the value that an assemble extends) and ::element (an iterate
  whose input is the element and not the collection)."
  [index op]
  (let [ctx (assoc index :log (volatile! []) :constants (volatile! {}) :stack #{} :env {})]
    (when-let [{:keys [form file] :as target} (unique-target ctx op)]
      (let [ctx (assoc ctx :file file :op op :at form)
            limited? (try
                       (doseq [[params] (signatures form true)]
                         (let [args (mapv (fn [i p] (assoc (step! ctx p :input (if (symbol? p) p (str "arg" i)) [])
                                                          :alias true)) (range) params)]
                           (invoke ctx form target args)))
                       false
                       (catch clojure.lang.ExceptionInfo e (if (::limit (ex-data e)) true (throw e))))]
        {:steps @(:log ctx) :truncated limited?}))))

(defn templates
  "Build each fixed arity once. Named nested calls are slots in the template, not expanded bodies."
  [index op]
  (when-let [{:keys [file form] :as target} (unique-target index op)]
    (into {} (for [[params] (signatures form true)
                   :let [ctx (assoc index :log (volatile! []) :constants (volatile! {}) :stack #{} :env {}
                                    :file file :op op :at form :local true)
                         limited? (try
                                    (invoke ctx form target
                                            (mapv (fn [i p] (assoc (step! ctx p :input (if (symbol? p) p (str "arg" i)) [])
                                                                    :alias true)) (range) params))
                                    false
                                    (catch clojure.lang.ExceptionInfo e (if (::limit (ex-data e)) true (throw e))))]]
               [(count params) {:steps @(:log ctx) :truncated limited?}]))))

(defn analyze [dir files operation & [lang]]
  (let [op (symbol operation)
        index (source-index dir files op lang)
        {:keys [steps truncated]} (trace index op)]
    (when-not steps (throw (ex-info (str "No unique top-level " (name (:lang index)) " defn for " operation ".")
                                    {:code :flow-operation :extra {:language (:lang index) :unparsed (:unparsed index)}})))
    (report index dir op steps truncated)))
