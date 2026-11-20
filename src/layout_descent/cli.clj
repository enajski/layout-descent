(ns layout-descent.cli
  "The command surface, made for a program or a language model to drive. Every command prints one
  JSON document on stdout and nothing else there. Exit 0: done. Exit 1: refused or failed, with
  error.code and error.hint. Exit 2: wrong usage or an internal error."
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [layout-descent.bench :as bench]
            [layout-descent.emit :as emit]
            [layout-descent.gate :as gate]
            [layout-descent.plan :as plan])
  (:import (java.io File)
           (java.util LinkedHashMap)))

(def commands
  "Every command: its usage, what it changes, and what it is for."
  [{:name "help" :usage "help" :writes "nothing"
    :doc "List the commands."}
   {:name "facts" :usage "facts <src-dir>... [--limit N]" :writes "nothing"
    :doc "Where a code base reads, writes and scans nested maps, grouped by path shape."}
   {:name "check" :usage "check <plan>" :writes "nothing"
    :doc "Validate a plan file. Gives its hash, task and warnings, or its problems."}
   {:name "status" :usage "status <plan>" :writes "nothing"
    :doc "The plan in full: which order serves each pattern, the emitted functions, the journal."}
   {:name "verify" :usage "verify <plan> [--source <file>]" :writes "nothing"
    :doc "Run the gate: compile the emitted code and compare every pattern with the model."}
   {:name "neighbors" :usage "neighbors <plan> [--out-dir <dir>]" :writes "candidate files in --out-dir"
    :doc "Every plan one decision away. A starting set of candidates; any valid plan of the task may be one."}
   {:name "evaluate" :usage "evaluate <candidate> --against <plan> [--rounds N] [--warm-ms N]"
    :writes "one line in <plan>.journal"
    :doc "Gate both plans, time them on the same data, and give a verdict: better, worse or no-change."}
   {:name "descend" :usage "descend <plan> [--max-steps N] [--out <file>] [--rounds N] [--warm-ms N]"
    :writes "one candidate file, lines in <plan>.journal"
    :doc "Greedy search over neighbors. A baseline for the search, and a candidate to accept."}
   {:name "accept" :usage "accept <candidate> --into <plan> [--force <reason>]"
    :writes "<plan>, its emitted file, one line in <plan>.journal"
    :doc "Move the orders of a candidate into the plan. Needs a better verdict in the journal."}
   {:name "emit" :usage "emit <plan> [--out <file>]" :writes "the emitted source file"
    :doc "Gate the plan, then write its source to --out or to :emit :path. With neither, return it."}])

(def ^:private flags-of
  {"help" #{} "facts" #{:limit} "check" #{} "status" #{} "verify" #{:source} "neighbors" #{:out-dir}
   "evaluate" #{:against :rounds :warm-ms} "descend" #{:max-steps :out :rounds :warm-ms}
   "accept" #{:into :force} "emit" #{:out}})

(def ^:private hints
  {:usage "Run `help` for the commands and their arguments."
   :unreadable-plan "The file must hold one EDN map. See examples/risk/plan.edn."
   :invalid-plan "Fix every entry of `problems`; `path` is the place in the plan."
   :task-mismatch "A candidate keeps :levels, :leaf, :patterns and :workload of the plan. Change only :orders and :serve."
   :same-plan "The candidate makes the same layout decisions as the plan. Change :orders or :serve."
   :gate-failed "The emitted code is wrong or compiles with warnings. Do not accept this plan. For source that layout-descent emitted, this is a defect of layout-descent: report `mismatches`."
   :not-measured "Run `evaluate <candidate> --against <plan>` first. accept needs a `better` verdict for these two hashes on this task."
   :no-target "Give --out <file>, or add :emit {:ns some.ns :path \"src/some/ns.clj\"} to the plan."
   :internal "An unexpected error. Read `message`; the command changed nothing that it did not report."})

(defn- fail
  "Stop the command with an error code. extra is merged into the top level of the answer."
  ([code message] (fail code message nil))
  ([code message extra] (throw (ex-info message {:code code :extra extra}))))

;; ---- arguments ------------------------------------------------------------

(defn- parse-args [command args]
  (let [allowed (get flags-of command)]
    (loop [args args
           positional []
           flags {}]
      (if-let [[arg & more] (seq args)]
        (if (str/starts-with? arg "--")
          (let [flag (keyword (subs arg 2))]
            (cond
              (not (contains? allowed flag))
              (fail :usage (str "Unknown flag " arg " for " command ". Allowed: "
                                (str/join " " (map #(str "--" (name %)) (sort allowed))) "."))
              (empty? more) (fail :usage (str "Flag " arg " needs a value."))
              :else (recur (rest more) positional (assoc flags flag (first more)))))
          (recur more (conj positional arg) flags))
        {:positional positional :flags flags}))))

(defn- one-path [command {:keys [positional]}]
  (when-not (= 1 (count positional))
    (fail :usage (str command " takes one plan file. Usage: "
                      (:usage (first (filter #(= command (:name %)) commands))))))
  (first positional))

(defn- int-flag [flags k default low]
  (let [v (get flags k)
        n (if v (parse-long v) default)]
    (when (or (nil? n) (< (long n) (long low)))
      (fail :usage (str "--" (name k) " must be a whole number of at least " low ".")))
    n))

(defn- bench-opts [flags]
  {:rounds (int-flag flags :rounds (:rounds bench/defaults) 5)
   :warm-ms (int-flag flags :warm-ms (:warm-ms bench/defaults) 10)})

(defn- absolute [path]
  (.getPath (.getAbsoluteFile (io/file path))))

(defn- beside
  "A path that the plan file names, resolved against the directory of the plan file."
  [plan-path path]
  (let [f (io/file path)]
    (absolute (if (.isAbsolute f) f (io/file (.getParentFile (io/file (absolute plan-path))) path)))))

;; ---- journal --------------------------------------------------------------

(defn- journal-path [plan-path]
  (str (absolute plan-path) ".journal"))

(defn- journal
  "The records of a plan's journal. A line that does not read, such as a torn last line, is skipped."
  [plan-path]
  (let [f (io/file (journal-path plan-path))]
    (if (.exists f)
      (vec (keep (fn [line]
                   (try (let [r (edn/read-string line)] (when (map? r) r))
                        (catch Exception _ nil)))
                 (str/split-lines (slurp f))))
      [])))

(defn- append-journal! [plan-path record]
  (spit (journal-path plan-path)
        (str (pr-str (assoc record :at (str (java.time.Instant/now)))) "\n")
        :append true))

;; ---- shared steps ---------------------------------------------------------

(defn- gate!
  "Verify a plan, or stop with :gate-failed."
  [plan role]
  (let [result (gate/verify plan)]
    (when-not (:ok result)
      (fail :gate-failed (str "The " role " " (:hash result) " does not pass the gate.") {:gate result}))
    result))

(defn- brief [summary]
  (select-keys summary [:hash :cost :time-ns :noise :bytes :entries :seed :patterns]))

(defn- compare-plans
  "Measure a candidate against a baseline. A better verdict must hold again on holdout data, which
  no step of a search has seen."
  [base candidate opts]
  (let [[b c] (bench/measure [base candidate] opts)
        first-look (bench/verdict (:objective base) b c)
        holdout (when (= :better (:verdict first-look))
                  (let [[hb hc] (bench/measure [base candidate] (assoc opts :seed-offset 1))]
                    (assoc (bench/verdict (:objective base) hb hc) :seed (:seed hb))))
        confirmed? (or (nil? holdout) (= :better (:verdict holdout)))]
    (array-map
     :verdict (if confirmed? (:verdict first-look) :no-change)
     :reason (cond
               (not confirmed?) "Better on the workload seed, but not on the holdout seed."
               (= :no-change (:verdict first-look))
               "The difference is below :min-gain or within three times the noise.")
     :gain (:gain first-look)
     :delta-ns (:delta-ns first-look)
     :noise-ns (:noise-ns first-look)
     :holdout holdout
     :baseline (brief b)
     :candidate (brief c))))

(defn- journal-evaluation! [plan-path base candidate result opts]
  (append-journal! plan-path
                   {:op :evaluate
                    :baseline (plan/plan-hash base)
                    :candidate (plan/plan-hash candidate)
                    :task (plan/task-hash base)
                    :verdict (:verdict result)
                    :gain (:gain result)
                    :baseline-cost (:cost (:baseline result))
                    :candidate-cost (:cost (:candidate result))
                    :holdout (:verdict (:holdout result))
                    :rounds (:rounds opts)}))

(defn- same-task! [base candidate]
  (when-not (= (plan/task-key base) (plan/task-key candidate))
    (fail :task-mismatch "The candidate and the plan describe different tasks."
          {:tasks {:plan (plan/task-hash base) :candidate (plan/task-hash candidate)}})))

(defn- emit-target [plan-path plan out]
  (cond
    out (absolute out)
    (:emit plan) (beside plan-path (:path (:emit plan)))))

(defn- emit-state [plan target]
  (cond
    (nil? target) :not-configured
    (not (.exists (io/file ^String target))) :missing
    (= (slurp target) (emit/source plan)) :current
    :else :stale))

(defn- served [plan]
  (mapv (fn [pattern]
          (let [index (get (:serve plan) (:id pattern))
                order (nth (:orders plan) index)
                sig (emit/signature plan pattern)]
            (array-map
             :pattern (:id pattern)
             :bind (:bind pattern)
             :fold (:fold pattern)
             :weight (get-in plan [:workload :weights (:id pattern)])
             :order index
             :perm (:perm order)
             :format (:format order)
             :bound-levels (count (filter #{:key} (:bind pattern)))
             :indexed-levels (plan/bound-prefix (:perm order) (:bind pattern))
             :call (str "(" (:fn sig) " " (str/join " " (:args sig)) ")")
             :returns (:returns sig))))
        (:patterns plan)))

;; ---- commands -------------------------------------------------------------

(defmulti ^:private command (fn [name _args] name))

(defmethod command "help" [_ _]
  {:commands commands
   :exit-codes {:done 0 :refused-or-failed 1 :usage-or-internal 2}
   :loop ["facts <src-dir> to see the access shapes of the code"
          "write a plan: levels, leaf, patterns, one order, a workload"
          "check, then verify"
          "neighbors, or write a candidate that keeps the task"
          "evaluate <candidate> --against <plan>"
          "accept <candidate> --into <plan> when the verdict is better"]})

(defmethod command "facts" [_ {:keys [positional flags]}]
  (when (empty? positional)
    (fail :usage "facts needs at least one source directory."))
  (doseq [path positional :when (not (.exists (io/file ^String path)))]
    (fail :usage (str "No such path: " path)))
  (let [analyze (requiring-resolve 'layout-descent.facts/analyze)
        summarize (requiring-resolve 'layout-descent.facts/summarize)]
    (assoc (summarize (analyze positional) (int-flag flags :limit 5 1))
           :legend {:? "a key that the code computes: a :key level"
                    :* "every key: a :scan level"
                    :other "a constant: part of the schema, not a level"})))

(defmethod command "check" [_ args]
  (let [path (one-path "check" args)
        plan (plan/read-plan path)]
    {:plan (absolute path)
     :hash (plan/plan-hash plan)
     :task (plan/task-hash plan)
     :warnings (plan/warnings plan)}))

(defmethod command "status" [_ args]
  (let [path (one-path "status" args)
        plan (plan/read-plan path)
        target (emit-target path plan nil)
        records (journal path)]
    (array-map
     :plan (absolute path)
     :name (:name plan)
     :hash (plan/plan-hash plan)
     :task (plan/task-hash plan)
     :levels (:levels plan)
     :leaf (:type (:leaf plan))
     :orders (vec (map-indexed (fn [i order] (assoc order :order i)) (:orders plan)))
     :patterns (served plan)
     :warnings (plan/warnings plan)
     :objective (:objective plan)
     :emitted {:path target :ns (emit/ns-symbol plan) :state (emit-state plan target)}
     :journal {:path (journal-path path)
               :records (count records)
               :last (vec (take-last 5 records))})))

(defmethod command "verify" [_ {:keys [flags] :as args}]
  (let [path (one-path "verify" args)
        plan (plan/read-plan path)
        result (if-let [source (:source flags)]
                 (gate/verify-source plan (slurp source))
                 (gate/verify plan))]
    (when-not (:ok result)
      (fail :gate-failed (str "Plan " (:hash result) " does not pass the gate.") {:gate result}))
    {:gate result}))

(defmethod command "neighbors" [_ {:keys [flags] :as args}]
  (let [path (one-path "neighbors" args)
        plan (plan/read-plan path)
        dir (some-> (:out-dir flags) absolute)]
    {:plan (absolute path)
     :hash (plan/plan-hash plan)
     :neighbors (mapv (fn [{:keys [move why hash] candidate :plan}]
                        (cond-> {:hash hash
                                 :move move
                                 :why why
                                 :warnings (mapv :code (plan/warnings candidate))}
                          dir (assoc :path (plan/write-text! (io/file dir (str hash ".edn"))
                                                             (plan/render candidate)))))
                      (plan/neighbors plan))}))

(defmethod command "evaluate" [_ {:keys [flags] :as args}]
  (let [candidate-path (one-path "evaluate" args)
        base-path (or (:against flags) (fail :usage "evaluate needs --against <plan>."))
        base (plan/read-plan base-path)
        candidate (plan/read-plan candidate-path)
        opts (bench-opts flags)]
    (same-task! base candidate)
    (when (= (plan/plan-hash base) (plan/plan-hash candidate))
      (fail :same-plan "The candidate equals the plan."))
    (gate! base "plan")
    (gate! candidate "candidate")
    (let [result (compare-plans base candidate opts)]
      (journal-evaluation! base-path base candidate result opts)
      (concat result
              [[:journal (journal-path base-path)]
               [:next (if (= :better (:verdict result))
                        (str "accept " candidate-path " --into " base-path)
                        "Keep the plan. Try another candidate.")]]))))

(defn- step-candidates
  "The neighbors of a plan that are worth timing: unused orders removed, duplicates removed, and
  the gate passed."
  [current]
  (let [own (plan/plan-hash current)]
    (->> (plan/neighbors current)
         (map (fn [c] (let [pruned (plan/prune-unused (:plan c))]
                        (assoc c :plan pruned :hash (plan/plan-hash pruned)))))
         (remove #(= own (:hash %)))
         (reduce (fn [[seen out] c]
                   (if (seen (:hash c)) [seen out] [(conj seen (:hash c)) (conj out c)]))
                 [#{} []])
         second
         (mapv #(assoc % :gate-ok (:ok (gate/verify (:plan %))))))))

(defn- descend-step
  "Time every candidate with the current plan, then measure the cheapest again on its own. The
  second measurement removes the luck of having picked the minimum of many noisy numbers."
  [plan-path current opts]
  (let [candidates (step-candidates current)
        timed (filterv :gate-ok candidates)
        [base & summaries] (when (seq timed) (bench/measure (into [current] (map :plan timed)) opts))
        scored (mapv (fn [c s] (assoc c :cost (:cost s) :verdict (:verdict (bench/verdict (:objective current) base s))))
                     timed summaries)
        best (first (sort-by :cost (filter #(= :better (:verdict %)) scored)))
        confirmed (when best (compare-plans current (:plan best) opts))]
    (when confirmed
      (journal-evaluation! plan-path current (:plan best) confirmed opts))
    {:from (plan/plan-hash current)
     :cost (:cost base)
     :candidates (into (mapv #(select-keys % [:hash :move :cost :verdict]) scored)
                       (map #(assoc (select-keys % [:hash :move]) :verdict :gate-failed))
                       (remove :gate-ok candidates))
     :chosen (when (= :better (:verdict confirmed))
               {:hash (:hash best)
                :move (:move best)
                :why (:why best)
                :gain (:gain confirmed)
                :cost (:cost (:candidate confirmed))
                :plan (:plan best)})
     :stopped (cond
                (nil? best) "No neighbor is better."
                (not= :better (:verdict confirmed)) "The best neighbor was not better when measured again.")}))

(defmethod command "descend" [_ {:keys [flags] :as args}]
  (let [path (one-path "descend" args)
        start (plan/read-plan path)
        max-steps (int-flag flags :max-steps 4 1)
        opts (bench-opts flags)]
    (gate! start "plan")
    (let [[final steps]
          (loop [current start
                 steps []]
            (if (= (count steps) max-steps)
              [current steps]
              (let [step (descend-step path current opts)
                    steps (conj steps (update step :chosen #(some-> % (dissoc :plan))))]
                (if-let [chosen (:chosen step)]
                  (recur (:plan chosen) steps)
                  [current steps]))))
          improved? (not= (plan/plan-hash start) (plan/plan-hash final))
          taken (count (filter :chosen steps))
          ;; accept wants one record from the plan to the candidate; a walk of two or more steps has none.
          overall (when (> taken 1)
                    (let [result (compare-plans start final opts)]
                      (journal-evaluation! path start final result opts)
                      result))
          verdict (cond (not improved?) :no-change
                        overall (:verdict overall)
                        :else :better)
          out (when improved?
                (plan/write-text! (or (some-> (:out flags) absolute)
                                      (beside path (str "candidates/" (plan/plan-hash final) ".edn")))
                                  (plan/render final)))]
      (array-map
       :start (plan/plan-hash start)
       :final (plan/plan-hash final)
       :verdict verdict
       :gain (if overall (:gain overall) (:gain (:chosen (first (filter :chosen steps)))))
       :steps steps
       :stopped (or (:stopped (last steps)) "The step limit was reached.")
       :candidate out
       :journal (journal-path path)
       :next (if (= :better verdict)
               (str "accept " out " --into " path)
               "Keep the plan. A hand-written candidate may still be better: neighbors are one decision away only.")))))

(defmethod command "accept" [_ {:keys [flags] :as args}]
  (let [candidate-path (one-path "accept" args)
        into-path (or (:into flags) (fail :usage "accept needs --into <plan>."))
        target (plan/read-plan into-path)
        candidate (plan/read-plan candidate-path)
        _ (same-task! target candidate)
        from (plan/plan-hash target)
        to (plan/plan-hash candidate)
        ;; Only the layout decisions move. The name, workload, objective and :emit stay the plan's.
        accepted (assoc target :orders (:orders candidate) :serve (:serve candidate))
        changed? (not= from to)
        records (journal into-path)
        evidence (last (filter #(and (= :evaluate (:op %)) (= :better (:verdict %))
                                     (= to (:candidate %)) (= (plan/task-hash target) (:task %))
                                     (or (not changed?) (= from (:baseline %))))
                               records))]
    (when (and changed? (nil? evidence) (nil? (:force flags)))
      (fail :not-measured (str "No better verdict for candidate " to " against plan " from " is in the journal.")
            {:journal (journal-path into-path)}))
    (let [emit-path (emit-target into-path accepted nil)
          stale? (and emit-path (not= :current (emit-state accepted emit-path)))]
      (when (or changed? stale?)
        (gate! accepted "candidate"))
      (when changed?
        (plan/write-text! into-path (plan/render accepted)))
      (when stale?
        (plan/write-text! emit-path (emit/source accepted)))
      ;; A run that was interrupted after the plan was written finds no record here and adds it.
      (when (and (or changed? evidence)
                 (not= to (:to (last (filter #(= :accept (:op %)) records)))))
        (append-journal! into-path (cond-> {:op :accept :from (if changed? from (:baseline evidence)) :to to}
                                     (:force flags) (assoc :forced (:force flags))
                                     evidence (assoc :gain (:gain evidence)))))
      {:plan (absolute into-path)
       :changed changed?
       :from from
       :hash to
       :forced (boolean (and changed? (nil? evidence)))
       :emitted (when emit-path {:path emit-path :written (boolean stale?)})
       :journal (journal-path into-path)})))

(defmethod command "emit" [_ {:keys [flags] :as args}]
  (let [path (one-path "emit" args)
        plan (plan/read-plan path)
        target (emit-target path plan (:out flags))
        text (emit/source plan)
        gate (gate! plan "plan")
        base {:hash (plan/plan-hash plan)
              :ns (emit/ns-symbol plan)
              :checks (:checks gate)
              :functions (mapv #(select-keys % [:pattern :call :returns]) (served plan))}]
    (if target
      (assoc base :path (plan/write-text! target text))
      (assoc base :source text))))

(defmethod command :default [name _]
  (fail :usage (str "Unknown command " (pr-str name) ".")))

;; ---- output ---------------------------------------------------------------

(defn- json-key [k]
  (if (keyword? k) (str/replace (name k) "-" "_") (str k)))

(defn- jsonable
  "Clojure data as JSON data: snake_case keys, keywords and symbols as their text."
  [x]
  (cond
    (map? x) (let [m (LinkedHashMap.)]
               (doseq [[k v] x] (.put m (json-key k) (jsonable v)))
               m)
    (keyword? x) (subs (str x) 1)
    (symbol? x) (str x)
    (set? x) (mapv jsonable (sort-by str x))
    (sequential? x) (mapv jsonable x)
    (double? x) (if (Double/isFinite x) (/ (Math/round (* 1000.0 (double x))) 1000.0) nil)
    (instance? File x) (str x)
    :else x))

(defn- in-order
  "One map from several maps or sequences of pairs, with the keys in the order given."
  [& parts]
  (apply array-map (mapcat identity (apply concat parts))))

(defn run
  "Run one command line. Returns {:exit n :answer map}; the answer is what -main prints as JSON."
  [args]
  (try
    (let [[name & more] args]
      (when (nil? name)
        (fail :usage "No command."))
      (when-not (contains? flags-of name)
        (fail :usage (str "Unknown command " (pr-str name) ".")))
      {:exit 0
       :answer (jsonable (in-order [[:ok true] [:command name]] (command name (parse-args name more))))})
    (catch clojure.lang.ExceptionInfo e
      (let [{:keys [code extra problems]} (ex-data e)
            code (or code :internal)]
        {:exit (if (#{:usage :internal} code) 2 1)
         :answer (jsonable (in-order [[:ok false]
                                      [:error {:code (str/replace (name code) "-" "_")
                                               :message (ex-message e)
                                               :hint (get hints code (:internal hints))}]]
                                     (when problems {:problems problems})
                                     extra))}))
    (catch Throwable e
      {:exit 2
       :answer (jsonable {:ok false
                          :error {:code "internal"
                                  :message (str (.getSimpleName (class e)) ": " (ex-message e))
                                  :hint (:internal hints)}})})))

(defn -main [& args]
  (let [out *out*
        ;; Nothing but the answer may reach stdout, whatever a library prints.
        {:keys [exit answer]} (binding [*out* *err*] (run args))]
    (binding [*out* out]
      (println (json/write-str answer :escape-slash false))
      (flush))
    (shutdown-agents)
    (System/exit exit)))
