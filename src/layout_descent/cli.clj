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
           (java.nio.channels FileChannel)
           (java.nio.file Files LinkOption OpenOption Path StandardOpenOption)
           (java.util LinkedHashMap UUID)))

(def commands
  "Every command: its usage, what it changes, and what it is for."
  [{:name "help" :usage "help" :writes "nothing"
    :doc "List the commands."}
   {:name "facts" :usage "facts <src-dir>... [--limit N] [--min-depth N] [--writes true|false]" :writes "nothing"
    :doc "Where a code base reads and scans nested maps, grouped by path shape. Shapes with fewer than --min-depth (2) computed levels and write sites are counted in `hidden`. --limit 0 gives counts only."}
   {:name "flow" :usage "flow <src-dir> --operation <namespace/function> [--lang clj|cljs]" :writes "nothing"
    :doc "Follow named map fields through one Clojure operation and its resolved callees. Steps, uses and unresolved boundaries are static evidence, not timings."}
   {:name "discover" :usage "discover <src-dir> [--lang clj|cljs]" :writes "nothing"
    :doc "Find filtered entry traversals and possible membership-index maintenance sites; hypotheses only."}
   {:name "schema" :usage "schema <src-dir> [--lang clj|cljs] [--limit N] [--entity <id>|loose] [--sites N] [--diagram <file.md|file.mmd>]"
    :writes "the file of --diagram"
    :doc "The data model of a code base: entities (records that share keys), the fields that one entity carries into another, and where the traces stopped. --entity adds the records of one entity with their paths and sites. --diagram writes a Mermaid flowchart. Static evidence, not timings."}
   {:name "check" :usage "check <plan>" :writes "nothing"
    :doc "Validate a plan file. Gives its hash, task and warnings, or its problems."}
   {:name "status" :usage "status <plan>" :writes "nothing"
    :doc "The plan in full: its workload, which order serves each pattern, the emitted functions and why they are stale, the last journal records."}
   {:name "report" :usage "report <plan>" :writes "nothing"
    :doc "Everything for a report in one answer: the fields of status, the text of the plan file, the source that emit would write, every journal record, and the candidate plans kept under candidates/."}
   {:name "journal" :usage "journal <plan> [--last N]" :writes "nothing"
    :doc "Every record of <plan>.journal as JSON: evaluations with per-pattern numbers, acceptances."}
   {:name "measure" :usage "measure <plan>... [--rounds N] [--warm-ms N]" :writes "nothing"
    :doc "Time plans of one task on the same data: cost, bytes and ns of each pattern. No verdict and no journal record. Numbers compare inside one call only."}
   {:name "verify" :usage "verify <plan> [--source <trusted-clojure-file>]" :writes "nothing"
    :doc "Run the gate. --source executes the supplied Clojure file; use only trusted code."}
   {:name "neighbors" :usage "neighbors <plan> [--out-dir <dir>]" :writes "candidate files in --out-dir"
    :doc "Every plan one decision away. A starting set of candidates; any valid plan of the task may be one."}
   {:name "evaluate" :usage "evaluate <candidate> --against <plan> [--rounds N] [--warm-ms N]"
    :writes "one line in <plan>.journal"
    :doc "Gate both plans, time them on the same data, and give a verdict: better, worse or no-change. `sensitivity` says whether the verdict depends on :weights."}
   {:name "descend" :usage "descend <plan> [--max-steps N] [--out <file>] [--rounds N] [--warm-ms N]"
    :writes "the chosen plan of every step under candidates/ beside the plan, the final one also at --out, lines in <plan>.journal"
    :doc "Greedy search over neighbors. Every step gives cost, bytes and per-pattern ns of the plan and of each neighbor. When a neighbor is faster on one pattern and slower on another, the step also times a `split` that keeps the orders of both. `reach` says how many candidates were timed and what the search does not try."}
   {:name "accept" :usage "accept <candidate> --into <plan> [--force <reason>]"
    :writes "<plan>, its emitted file, one line in <plan>.journal"
    :doc "Move the orders of a candidate into the plan. Needs a better verdict in the journal."}
   {:name "emit" :usage "emit <plan> [--out <file>]" :writes "an optional preview file"
    :doc "Gate and return source, or write a preview to --out. Only accept writes the configured emitted file."}])

(def ^:private flags-of
  {"help" #{} "facts" #{:limit :min-depth :writes} "flow" #{:operation :lang} "discover" #{:lang} "schema" #{:lang :limit :entity :sites :diagram} "check" #{} "status" #{} "report" #{} "journal" #{:last}
   "measure" #{:rounds :warm-ms} "verify" #{:source} "neighbors" #{:out-dir}
   "evaluate" #{:against :rounds :warm-ms} "descend" #{:max-steps :out :rounds :warm-ms}
   "accept" #{:into :force} "emit" #{:out}})

(def ^:private hints
  {:usage "Run `help` for the commands and their arguments."
   :flow-operation "Choose a unique top-level defn in the selected language; use --lang clj or --lang cljs for .cljc sources. See unparsed for unreadable files."
   :schema-entity "Take the id of an entity from `entities` of the same command without --entity, or `loose`. Raise --limit to see more entities."
   :unreadable-plan "The file must hold one EDN map. See examples/risk/plan.edn."
   :invalid-plan "Fix every entry of `problems`; `path` is the place in the plan."
   :task-mismatch "A candidate keeps :levels, :leaf, :patterns and :workload of the plan. Change only :orders and :serve."
   :same-plan "The candidate makes the same layout decisions as the plan. Change :orders or :serve."
   :gate-failed "The emitted code is wrong or compiles with warnings. Do not accept this plan. For source that layout-descent emitted, this is a defect of layout-descent: report `mismatches`."
   :not-measured "Run `evaluate <candidate> --against <plan>` first. accept needs a `better` verdict for these two hashes on this task."
   :path-collision "Choose separate files for the plan, generated source, journal and candidate."
   :accept-incomplete "Fix the reported write problem and rerun accept to resume."
   :internal "An unexpected error. Read `message` for the cause."})

(defn- fail
  "Stop the command with an error code. extra is merged into the top level of the answer."
  ([code message] (fail code message nil))
  ([code message extra] (throw (ex-info message {:code code :extra extra}))))

;; ---- arguments ------------------------------------------------------------

(defn- parse-args [command args]
  (let [allowed (conj (get flags-of command) :save)]
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

;; ---- files and journal -----------------------------------------------------

(defn- canonical [path]
  (.getCanonicalPath (io/file path)))

(defn- journal-path [plan-path] (str (canonical plan-path) ".journal"))
(defn- lock-path [plan-path] (str (canonical plan-path) ".lock"))
(defn- pending-path [plan-path] (str (canonical plan-path) ".accept-pending"))

(def ^:private local-locks (vec (repeatedly 64 #(Object.))))

(defn- with-plan-lock [path f]
  (let [path (canonical path)
        lock-file (lock-path path)]
    (locking (nth local-locks (mod (hash path) (count local-locks)))
      (with-open [channel (FileChannel/open (Path/of lock-file (make-array String 0))
                                          (into-array OpenOption [StandardOpenOption/CREATE StandardOpenOption/WRITE]))
                  lock (.lock channel)]
        (f path)))))

(defn- same-file? [a b]
  (and a b
       (or (= (canonical a) (canonical b))
           (let [a (.toPath (io/file a)) b (.toPath (io/file b))]
             (and (Files/exists a (make-array LinkOption 0))
                  (Files/exists b (make-array LinkOption 0))
                  (Files/isSameFile a b))))))

(defn- separate! [out protected]
  (when (and out (some #(same-file? out %) protected))
    (fail :path-collision (str "Output file aliases a protected file: " out))))

(defn- plan-files [path]
  [path (journal-path path) (lock-path path) (pending-path path)])

(defn- candidate-file [root plan]
  (io/file root (plan/task-hash plan) (str (plan/plan-hash plan) ".edn")))

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
     :sensitivity (bench/weight-sensitivity (:objective base) b c)
     :baseline (brief b)
     :candidate (brief c))))

(defn- journal-evaluation! [plan-path base candidate result opts]
  (with-plan-lock plan-path #(append-journal! %
                   {:op :evaluate
                    :baseline (plan/plan-hash base)
                    :candidate (plan/plan-hash candidate)
                    :task (plan/task-hash base)
                    :objective (:objective base)
                    :verdict (:verdict result)
                    :gain (:gain result)
                    :baseline-cost (:cost (:baseline result))
                    :candidate-cost (:cost (:candidate result))
                    :holdout (:verdict (:holdout result))
                    :baseline-bytes (:bytes (:baseline result))
                    :candidate-bytes (:bytes (:candidate result))
                    :patterns (mapv (fn [b c] {:pattern (:pattern b) :baseline-ns (:ns b) :candidate-ns (:ns c)})
                                    (:patterns (:baseline result)) (:patterns (:candidate result)))
                    :rounds (:rounds opts)})))

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

(defn- text-digest [^String text]
  (plan/text-digest text 16))

(def ^:private stale-causes
  {:plan-changed "The layout decisions of the plan file changed outside accept."
   :plan-renamed "The :name or the :emit :ns of the plan changed after its last accept."
   :file-edited "The emitted file was edited after accept wrote it."
   :emitter-changed "layout-descent emits other source for this plan than at the last accept."
   :file-or-emitter-changed "The emitted file or the emitter changed; the journal comes from a version that kept no digest of the written source."})

(defn- source-names
  "What the emitted source takes from a plan beyond the decisions that the plan hash covers."
  [plan]
  {:name (:name plan) :ns (emit/ns-symbol plan)})

(defn- stale-cause
  "Why an emitted file differs from the source of its plan. The plan hash in the file tells a
  change of the decisions; the last record of a written source tells the rest."
  [plan text records]
  (let [written (last (filter :source-digest records))]
    (cond
      (not= (plan/plan-hash plan) (second (re-find #"\(def plan-hash \"([0-9a-f]+)\"\)" text))) :plan-changed
      (nil? written) :file-or-emitter-changed
      (not= (:source-digest written) (text-digest text)) :file-edited
      (and (:source-names written) (not= (:source-names written) (source-names plan))) :plan-renamed
      :else :emitter-changed)))

(defn- emitted-status [path plan target records]
  (let [state (emit-state plan target)
        text (when (= :stale state) (slurp target))
        cause (when text (stale-cause plan text records))]
    (cond-> (array-map :path target :ns (emit/ns-symbol plan) :state state)
      cause (assoc :cause cause
                   :why (stale-causes cause)
                   :fix (if (= :plan-changed cause)
                          (str "Put the accepted plan "
                               (or (second (re-find #"\(def plan-hash \"([0-9a-f]+)\"\)" text)) "of the emitted file")
                               " back, save the edit as a candidate, then evaluate and accept it.")
                          (str "accept " path " --into " path " gates the plan and writes the file again."))))))

(defn- pattern-ns [summary]
  (mapv #(select-keys % [:pattern :ns]) (:patterns summary)))

(defn- keep-candidate!
  "Write a candidate under candidates/<task>/ beside the plan. Returns its path."
  [plan-path plan candidate]
  (let [out (candidate-file (beside plan-path "candidates") candidate)]
    (separate! out (conj (plan-files plan-path) (emit-target plan-path plan nil)))
    (plan/write-text! out (plan/render candidate))))

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
   :every-command {:save "--save <file> also writes the answer, the same JSON document, to the file and names it in `saved`. A refusal or a failure is not saved and leaves the file as it was. The file cannot be a plan, its journal or its emitted file, a source file that the command reads, or a file below candidates/ or --out-dir."}
   :loop ["facts <src-dir> to see the access shapes of the code"
          "write a plan: levels, leaf, patterns, one order, a workload"
          "check, then verify"
          "descend <plan>, or neighbors, or write a candidate that keeps the task"
          "evaluate <candidate> --against <plan>"
          "accept <candidate> --into <plan> when the verdict is better"
          "for a report: descend --save <file>, measure <plan>..., report <plan>"]})

(defmethod command "facts" [_ {:keys [positional flags]}]
  (when (empty? positional)
    (fail :usage "facts needs at least one source directory."))
  (doseq [path positional :when (not (.exists (io/file ^String path)))]
    (fail :usage (str "No such path: " path)))
  (let [analyze (requiring-resolve 'layout-descent.facts/analyze)
        summarize (requiring-resolve 'layout-descent.facts/summarize)]
    (assoc (summarize (analyze positional)
                      (int-flag flags :limit 5 0)
                      {:min-depth (int-flag flags :min-depth 2 0)
                       :writes (case (:writes flags "false")
                                 "true" true
                                 "false" false
                                 (fail :usage "--writes must be true or false."))})
           :legend {:? "a key that the code computes: a :key level"
                    :* "every key: a :scan level"
                    "[? ?]" "one key that is a vector: model each computed part as a level; a constant part is part of the schema"
                    :other "a constant: part of the schema, not a level"})))

(defmethod command "flow" [_ {:keys [positional flags]}]
  (let [[dir] positional
        op (:operation flags)
        lang (some-> (:lang flags) keyword)]
    (when (and lang (not (#{:clj :cljs} lang))) (fail :usage "--lang must be clj or cljs."))
    (when-not (and (= 1 (count positional)) (.isDirectory (io/file ^String dir))
                   op (re-matches #"[^/\s]+/[^/\s]+" op))
      (fail :usage "Usage: flow <src-dir> --operation <namespace/function> [--lang clj|cljs]."))
    (let [files ((requiring-resolve 'layout-descent.flow/source-files) dir)]
      (separate! (:save flags) files)
      ((requiring-resolve 'layout-descent.flow/analyze) dir files op lang))))

(defmethod command "discover" [_ {:keys [positional flags]}]
  (let [[dir] positional
        lang (some-> (:lang flags) keyword)]
    (when-not (and (= 1 (count positional)) (.isDirectory (io/file ^String dir))
                   (or (nil? lang) (#{:clj :cljs} lang)))
      (fail :usage "Usage: discover <src-dir> [--lang clj|cljs]."))
    (let [files ((requiring-resolve 'layout-descent.flow/source-files) dir)]
      (separate! (:save flags) files)
      ((requiring-resolve 'layout-descent.discover/analyze) dir files lang))))

(defmethod command "schema" [_ {:keys [positional flags]}]
  (let [[dir] positional
        lang (some-> (:lang flags) keyword)
        {:keys [diagram save]} flags]
    (when (and lang (not (#{:clj :cljs} lang))) (fail :usage "--lang must be clj or cljs."))
    (when-not (and (= 1 (count positional)) (.isDirectory (io/file ^String dir)))
      (fail :usage "Usage: schema <src-dir> [--lang clj|cljs] [--limit N] [--entity <id>|loose] [--sites N] [--diagram <file.md|file.mmd>]."))
    (when (and diagram (not (re-find #"(?i)\.(md|mmd)$" diagram)))
      (fail :usage "--diagram needs a .md or a .mmd file."))
    (when (and diagram save (same-file? diagram save))
      (fail :path-collision "--diagram and --save name the same file."))
    (let [files ((requiring-resolve 'layout-descent.flow/source-files) dir)
          _ (separate! save files)
          answer ((requiring-resolve 'layout-descent.schema/analyze)
                  dir files lang {:limit (int-flag flags :limit 30 1)
                                  :entity (:entity flags)
                                  :sites (int-flag flags :sites 3 0)})]
      (if diagram
        (let [chart ((requiring-resolve 'layout-descent.schema/mermaid) answer)
              target (absolute diagram)]
          (plan/write-text! target (if (re-find #"(?i)\.md$" diagram)
                                     (str "# Data model of " (:root answer) "\n\n" (:note answer)
                                          "\n\n```mermaid\n" chart "\n```\n")
                                     (str chart "\n")))
          (assoc answer :diagram target))
        answer))))

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
     :notes (:notes plan)
     :hash (plan/plan-hash plan)
     :task (plan/task-hash plan)
     :levels (:levels plan)
     :leaf (:type (:leaf plan))
     ;; The weights are in :patterns: a pattern id as a JSON key would lose its hyphens.
     ;; bigdec: the answer rounds a double to three places, and a fill must read back exactly.
     :workload (-> (:workload plan)
                   (dissoc :weights)
                   (update :fill (partial mapv bigdec)))
     :orders (vec (map-indexed (fn [i order] (assoc order :order i)) (:orders plan)))
     :patterns (served plan)
     :warnings (plan/warnings plan)
     :objective (:objective plan)
     :emitted (emitted-status path plan target records)
     :journal {:path (journal-path path)
               :accept-pending (.exists (io/file (pending-path path)))
               :records (count records)
               :last (vec (take-last 5 records))})))

(defmethod command "report" [_ args]
  (let [path (one-path "report" args)
        status (command "status" args)
        plan (plan/read-plan path)
        records (journal path)
        kept (io/file (beside path "candidates") (plan/task-hash plan))]
    (concat (dissoc status :journal)
            [[:plan-text (slurp path)]
             [:source (emit/source plan)]
             [:journal (-> (:journal status) (dissoc :last) (assoc :entries (vec records)))]
             [:candidates (vec (for [^File file (sort (.listFiles kept))
                                     :when (str/ends-with? (.getName file) ".edn")
                                     :let [candidate (try (plan/read-plan (str file)) (catch Exception _ nil))]]
                                 (cond
                                   (nil? candidate) {:path (str file) :unreadable true}
                                   (not= (plan/task-hash plan) (plan/task-hash candidate))
                                   {:path (str file) :other-task true}
                                   :else
                                   (array-map :hash (plan/plan-hash candidate)
                                              :path (str file)
                                              :orders (:orders candidate)
                                              :serve (mapv (fn [[id i]] {:pattern id :order i}) (:serve candidate))))))]])))

(defmethod command "journal" [_ {:keys [flags] :as args}]
  (let [path (one-path "journal" args)
        _ (when-not (.exists (io/file ^String path))
            (fail :usage (str "No such plan: " path)))
        records (journal path)
        file (io/file (journal-path path))
        lines (if (.exists file) (count (remove str/blank? (str/split-lines (slurp file)))) 0)]
    (array-map
     :path (journal-path path)
     :records (count records)
     ;; A torn or hand-edited line does not read as a record.
     :unreadable-lines (- lines (count records))
     :entries (vec (take-last (int-flag flags :last (max 1 (count records)) 1) records)))))

(defmethod command "measure" [_ {:keys [positional flags]}]
  (when (empty? positional)
    (fail :usage "measure needs at least one plan file."))
  (let [plans (mapv plan/read-plan positional)]
    (doseq [p (rest plans)] (same-task! (first plans) p))
    (when-not (apply distinct? (map plan/plan-hash plans))
      (fail :same-plan "Two of the plans make the same layout decisions."))
    (doseq [p plans] (gate! p "plan"))
    (let [summaries (bench/measure plans (bench-opts flags))]
      (array-map
       :task (plan/task-hash (first plans))
       :entries (:entries (first summaries))
       :seed (:seed (first summaries))
       :objective (:objective (first plans))
       :plans (mapv (fn [path s]
                      (into (array-map :path (absolute path))
                            (map (fn [k] [k (get s k)]))
                            [:hash :cost :time-ns :noise :bytes :build-ms :patterns]))
                    positional summaries)))))

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
                          dir (assoc :path (let [out (candidate-file dir candidate)]
                                             (separate! out (conj (plan-files path) (emit-target path plan nil)))
                                             (plan/write-text! out (plan/render candidate))))))
                      (plan/neighbors plan))}))

(defmethod command "evaluate" [_ {:keys [flags] :as args}]
  (let [candidate-path (one-path "evaluate" args)
        base-path (or (:against flags) (fail :usage "evaluate needs --against <plan>."))
        base (plan/read-plan base-path)
        candidate (plan/read-plan candidate-path)
        opts (bench-opts flags)]
    (separate! (journal-path base-path)
               [base-path candidate-path (emit-target base-path base nil)])
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

(defn- score
  "Time candidates with the current plan in one run. Returns the summary of the current plan and
  the candidates with their numbers. :faster and :slower name the patterns that a candidate serves
  faster or slower than the plan, by time alone and beyond three times the noise. A run times the
  candidates in batches, each with the plan again, so :gain and :verdict compare a candidate with
  the plan of its own batch. Costs of two batches do not compare; gains do."
  [current candidates opts]
  (let [[base & summaries] (when (seq candidates) (bench/measure (into [current] (map :plan candidates)) opts))]
    [base
     (mapv (fn [c s]
             (let [b (or (:batch-baseline s) base)
                   rows (:patterns (bench/weight-sensitivity {:ns-per-byte 0.0} b s))
                   with-effect (fn [effect] (mapv :pattern (filter #(= effect (:effect %)) rows)))
                   compared (bench/verdict (:objective current) b s)]
               (assoc c
                      :cost (:cost s) :bytes (:bytes s) :patterns (pattern-ns s)
                      :faster (with-effect :better) :slower (with-effect :worse)
                      :gain (:gain compared)
                      :verdict (:verdict compared))))
           candidates summaries)]))

(defn- split-candidates
  "For every timed neighbor that is faster on one pattern and slower on another: the plan that
  keeps the orders of both and serves each pattern from the faster one. A change of format takes
  the old layout away, so this plan is two moves from the current one and no neighbor."
  [current scored]
  (let [taken (into #{(plan/plan-hash current)} (map :hash) scored)]
    (->> scored
         (keep (fn [{:keys [hash faster slower] neighbor :plan}]
                 (when (and (seq faster) (seq slower))
                   (let [both (plan/split current neighbor faster)]
                     {:move {:op :split :with hash :serves faster}
                      :why (str "Neighbor " hash " is faster on " (str/join ", " faster) " and slower on "
                                (str/join ", " slower) ". This plan keeps the orders of both and serves"
                                " each pattern from the faster one.")
                      :plan both
                      :hash (plan/plan-hash both)}))))
         (reduce (fn [[seen out] c]
                   (if (seen (:hash c)) [seen out] [(conj seen (:hash c)) (conj out c)]))
                 [taken []])
         second
         (mapv #(assoc % :gate-ok (:ok (gate/verify (:plan %))))))))

(defn- descend-step
  "Time every neighbor with the current plan, then the splits that their numbers suggest, then
  measure the better candidate with the largest gain again on its own. The last measurement removes
  the luck of having picked the best of many noisy numbers. The numbers of two runs do not compare:
  two runs in one JVM can time the same layout far apart. So when there are splits, one more run
  times the plan, the neighbors and the splits together, and the step reports that run alone."
  [plan-path current opts]
  (let [neighbors (step-candidates current)
        [base timed] (score current (filterv :gate-ok neighbors) opts)
        splits (split-candidates current timed)
        [base scored] (if-let [timed-splits (seq (filter :gate-ok splits))]
                        (score current (into timed timed-splits) opts)
                        [base timed])
        candidates (into neighbors splits)
        best (first (sort-by #(- (double (:gain %))) (filter #(= :better (:verdict %)) scored)))
        confirmed (when best (compare-plans current (:plan best) opts))]
    (when confirmed
      (journal-evaluation! plan-path current (:plan best) confirmed opts))
    {:from (plan/plan-hash current)
     :cost (:cost base)
     :bytes (:bytes base)
     :patterns (some-> base pattern-ns)
     :candidates (into (mapv #(select-keys % [:hash :move :cost :bytes :gain :verdict :patterns]) scored)
                       (map #(assoc (select-keys % [:hash :move]) :verdict :gate-failed))
                       (remove :gate-ok candidates))
     :chosen (when (= :better (:verdict confirmed))
               {:hash (:hash best)
                :move (:move best)
                :why (:why best)
                :gain (:gain confirmed)
                :cost (:cost (:candidate confirmed))
                :measured (select-keys confirmed [:sensitivity :baseline :candidate])
                :plan (:plan best)})
     :stopped (cond
                (nil? best) "No candidate is better."
                (not= :better (:verdict confirmed)) "The best candidate was not better when measured again.")}))

(defn- reach
  "What a walk timed, and what it cannot reach. A result holds inside this reach only."
  [steps]
  (let [candidates (mapcat :candidates steps)
        failed? #(= :gate-failed (:verdict %))]
    (array-map
     :timed (count (distinct (map :hash (remove failed? candidates))))
     :gate-failed (count (distinct (map :hash (filter failed? candidates))))
     :moves (mapv (fn [[op n]] {:op op :candidates n})
                  (sort-by (comp str key) (frequencies (map (comp :op :move) candidates))))
     :not-tried ["A plan two or more moves from the plan of a step. The one exception is a split."
                 "An added order that fixes no longer prefix for any pattern."
                 "The plans behind a better candidate that did not have the largest gain of its step."])))

(defmethod command "descend" [_ {:keys [flags] :as args}]
  (let [path (one-path "descend" args)
        start (plan/read-plan path)
        max-steps (int-flag flags :max-steps 4 1)
        opts (bench-opts flags)]
    (separate! (:out flags) (conj (plan-files path) (emit-target path start nil)))
    (separate! (journal-path path) [path (emit-target path start nil)])
    (gate! start "plan")
    (let [[final steps]
          (loop [current start
                 steps []]
            (if (= (count steps) max-steps)
              [current steps]
              (let [step (descend-step path current opts)
                    kept (some->> (:plan (:chosen step)) (keep-candidate! path start))
                    steps (conj steps (update step :chosen #(some-> % (dissoc :plan) (assoc :path kept))))]
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
                (let [out (or (some-> (:out flags) absolute)
                              (candidate-file (beside path "candidates") final))]
                  (separate! out (conj (plan-files path) (emit-target path start nil)))
                  (plan/write-text! out (plan/render final))))]
      (array-map
       :start (plan/plan-hash start)
       :final (plan/plan-hash final)
       :verdict verdict
       :gain (if overall (:gain overall) (:gain (:chosen (first (filter :chosen steps)))))
       ;; Start and final from one run: the numbers to report.
       :measured (when improved?
                   (or (some-> overall (select-keys [:sensitivity :baseline :candidate]))
                       (:measured (:chosen (first (filter :chosen steps))))))
       :steps steps
       ;; For example :memory-not-priced: under such an objective a split, which adds an order, costs nothing.
       :warnings (plan/warnings final)
       :reach (reach steps)
       :stopped (or (:stopped (last steps)) "The step limit was reached.")
       :candidate out
       :journal (journal-path path)
       :next (if (= :better verdict)
               (str "accept " out " --into " path)
               "Keep the plan. `reach` says how far the search looked; a hand-written candidate outside it may still be better.")))))

(defn- latest-measurement [records target from to]
  (last (filter #(and (= :evaluate (:op %))
                      (= from (:baseline %)) (= to (:candidate %))
                      (= (plan/task-hash target) (:task %))
                      (= (:objective target) (:objective %)))
                records)))

(defn- acceptance-writes [path {:keys [accepted changed? source? record]}]
  (let [out (emit-target path accepted nil)
        records (when record (journal path))]
    (separate! out (plan-files path))
    (cond-> []
      source? (conj [out (emit/source accepted)])
      changed? (conj [path (plan/render accepted)])
      (and record (not-any? #(= (:transaction record) (:transaction %)) records))
      (conj [(journal-path path)
             (let [file (io/file (journal-path path))
                   old (when (.exists file) (slurp file))]
               (str old (when (and (seq old) (not (str/ends-with? old "\n"))) "\n")
                    (pr-str record) "\n"))]))))

(defn- publish-acceptance! [path transaction resume?]
  (let [pending (pending-path path)
        staged (volatile! [])]
    (try
      (doseq [[dest text] (acceptance-writes path transaction)]
        (when (.isDirectory (io/file dest))
          (throw (java.io.IOException. (str "Destination is a directory: " dest))))
        (vswap! staged conj [(plan/stage-text! dest text) dest]))
      (when-not resume? (plan/write-text! pending (pr-str transaction)))
      (doseq [[tmp dest] @staged] (plan/publish-text! tmp dest))
      (Files/deleteIfExists (.toPath (io/file pending)))
      (catch Exception e
        (if (.exists (io/file pending))
          (fail :accept-incomplete (str "Acceptance is pending: " (ex-message e)) {:pending pending})
          (throw e)))
      (finally
        (doseq [[tmp _] @staged] (Files/deleteIfExists tmp))))))

(defmethod command "accept" [_ {:keys [flags] :as args}]
  (let [candidate-path (one-path "accept" args)
        into-path (or (:into flags) (fail :usage "accept needs --into <plan>."))]
    (with-plan-lock
      into-path
      (fn [path]
        (let [pending (io/file (pending-path path))
              recovered? (.exists pending)
              previous (when recovered? (edn/read-string (slurp pending)))
              _ (when previous (publish-acceptance! path previous true))
              candidate (plan/read-plan candidate-path)
              target (plan/read-plan path)
              _ (same-task! target candidate)
              from (plan/plan-hash target)
              to (plan/plan-hash candidate)
              changed? (not= from to)
              accepted (assoc target :orders (:orders candidate) :serve (:serve candidate))
              out (emit-target path accepted nil)
              _ (separate! out (conj (plan-files path) candidate-path))
              _ (doseq [p (rest (plan-files path))] (separate! p [path candidate-path]))
              source? (boolean (and out (not= :current (emit-state accepted out))))
              evidence (when changed? (latest-measurement (journal path) target from to))
              forced? (and changed? (not= :better (:verdict evidence)) (some? (:force flags)))]
          (when (and changed? (not= :better (:verdict evidence)) (not forced?))
            (fail :not-measured (str "No current better verdict for candidate " to " against plan " from ".")
                  {:journal (journal-path path)}))
          (when (or changed? source?)
            (gate! accepted "candidate")
            (publish-acceptance!
             path {:accepted accepted :changed? changed? :source? source?
                   ;; A write of the source alone is recorded too: status compares the file with :source-digest.
                   :record (cond-> {:op (if changed? :accept :emit) :from from :to to
                                    :at (str (java.time.Instant/now))
                                    :transaction (str (UUID/randomUUID))}
                             out (assoc :source-digest (text-digest (emit/source accepted))
                                        :source-names (source-names accepted))
                             forced? (assoc :forced (:force flags))
                             (= :better (:verdict evidence)) (assoc :gain (:gain evidence)))}
             false))
          {:plan path :changed changed? :from from :hash to :forced (boolean forced?)
           :recovered recovered?
           :emitted (when out {:path out :written (or source? (boolean (:source? previous)))})
           :journal (journal-path path)})))))

(defmethod command "emit" [_ {:keys [flags] :as args}]
  (let [path (one-path "emit" args)
        plan (plan/read-plan path)
        target (some-> (:out flags) absolute)
        _ (separate! target (conj (plan-files path) (emit-target path plan nil)))
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

(defn- exact
  "Prices and weights read back exactly: bigdec keeps a small double from rounding to 0.0."
  [x]
  (cond
    (double? x) (when (Double/isFinite x) (bigdec x))
    (map? x) (update-vals x exact)
    (sequential? x) (mapv exact x)
    :else x))

(defn- jsonable
  "Clojure data as JSON data: snake_case keys, keywords and symbols as their text. Doubles round
  to three places, except under :objective, :weights and :weight, which must read back exactly."
  [x]
  (cond
    (map? x) (let [m (LinkedHashMap.)]
               (doseq [[k v] x] (.put m (json-key k) (jsonable (if (#{:objective :weights :weight} k) (exact v) v))))
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

(defn- inside?
  "True when the file is the directory or is below it, under any name of the directory. A
  directory that does not exist yet has no other name in the file system, so letter case is
  ignored: that refuses too much on a file system that keeps case apart, and never too little."
  [file dir]
  (let [dir-name ^String (canonical dir)]
    (->> (iterate (fn [^String p] (when p (.getParent (io/file p)))) (canonical file))
         (take-while some?)
         (some (fn [^String p] (or (same-file? p dir) (.equalsIgnoreCase p dir-name)))))))

(defn- save-target!
  "The absolute file of --save, or nil. Stops before the command runs when that file is one that
  the command reads or writes: a plan, its journal, its lock or its emitted file, a source file
  below a directory that the command analyzes, or any file below a directory where the command
  writes candidates. The names of those candidates are not known before the command runs."
  [{:keys [positional flags]}]
  (when-let [save (:save flags)]
    (let [named (concat positional (keep flags [:against :into :out :source]))
          directory? #(.isDirectory (io/file ^String %))
          written (concat (keep flags [:out-dir])
                          (map #(beside % "candidates")
                               (remove directory? (concat positional (keep flags [:against :into])))))]
      (when (directory? save)
        (fail :usage (str "--save needs a file, not a directory: " save)))
      (when (and (re-find #"(?i)\.clj[cs]?$" save) (some #(inside? save %) (filter directory? positional)))
        (fail :path-collision (str "--save names a source file below a directory that the command reads: " save)))
      ;; Both ways: the file may also not be a name that the command needs as a directory.
      (when-let [dir (first (filter #(or (inside? save %) (inside? % save)) written))]
        (fail :path-collision (str "--save is, or is below, a directory where the command writes candidates: " dir)))
      (separate! save
                 (concat named
                         (mapcat (fn [path]
                                   (concat (rest (plan-files path))
                                           (when-let [plan (try (plan/read-plan path) (catch Exception _ nil))]
                                             (some-> (emit-target path plan nil) vector))))
                                 named)))
      (absolute save))))

(defn run
  "Run one command line. Returns {:exit n :answer map}; the answer is what -main prints as JSON."
  [args]
  (try
    (let [[name & more] args]
      (when (nil? name)
        (fail :usage "No command."))
      (when-not (contains? flags-of name)
        (fail :usage (str "Unknown command " (pr-str name) ".")))
      (let [parsed (parse-args name more)
            save (save-target! parsed)
            answer (jsonable (in-order [[:ok true] [:command name]]
                                       (command name parsed)
                                       (when save [[:saved save]])))]
        (when save
          (plan/write-text! save (str (json/write-str answer :escape-slash false) "\n")))
        {:exit 0 :answer answer}))
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
