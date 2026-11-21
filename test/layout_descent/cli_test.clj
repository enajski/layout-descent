(ns layout-descent.cli-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [layout-descent.cli :as cli]
            [layout-descent.emit :as emit]
            [layout-descent.fixtures :as fx]
            [layout-descent.gen]
            [layout-descent.plan :as plan])
  (:import (java.nio.file Files)
           (java.nio.file.attribute FileAttribute)))

(defn- temp-dir []
  (str (Files/createTempDirectory "layout-descent-test" (make-array FileAttribute 0))))

(defn- run [& args]
  (let [{:keys [exit answer]} (cli/run (mapv str args))]
    ;; Every answer must be one JSON document.
    (assoc (json/read-str (json/write-str answer)) "exit" exit)))

(def ^:private fast ["--rounds" "5" "--warm-ms" "30"])

(deftest usage-and-validation
  (is (= 2 (get (run) "exit")))
  (is (= 2 (get (run "bogus") "exit")))
  (is (= "usage" (get-in (run "check" "a.edn" "--nope" "1") ["error" "code"])))
  (is (= 1 (get (run "check" "no-such-plan.edn") "exit")))
  (is (some #(= "accept" (get % "name")) (get (run "help") "commands")))
  (let [dir (temp-dir)
        bad (str (io/file dir "bad.edn"))]
    (spit bad (pr-str (assoc fx/small-risk :orders [{:perm [0 0 1] :format :csf}])))
    (let [answer (run "check" bad)]
      (is (= [1 false "invalid_plan"] [(get answer "exit") (get answer "ok") (get-in answer ["error" "code"])]))
      (is (seq (get answer "problems")))
      (is (string? (get-in answer ["error" "hint"]))))))

(deftest the-loop-from-plan-to-accepted-plan
  (let [dir (temp-dir)
        path (str (io/file dir "plan.edn"))
        emitted (io/file dir "gen/demo/layout.clj")
        _ (spit path (pr-str (assoc fx/small-risk :emit '{:ns demo.layout :path "gen/demo/layout.clj"})))
        start (get (run "check" path) "hash")
        listed (run "neighbors" path "--out-dir" (io/file dir "candidates"))
        pick (first (filter #(= {"op" "add-order" "perm" [2 0 1] "format" "csf" "serves" ["feature-total"]}
                                (get % "move"))
                            (get listed "neighbors")))
        candidate (get pick "path")]
    (is (= 0 (get listed "exit")))
    (is (.exists (io/file candidate)))
    (is (= "missing" (get-in (run "status" path) ["emitted" "state"])))

    (testing "accept refuses a candidate that was never measured, and changes nothing"
      (let [answer (run "accept" candidate "--into" path)]
        (is (= [1 "not_measured"] [(get answer "exit") (get-in answer ["error" "code"])]))
        (is (= start (get (run "check" path) "hash")))
        (is (not (.exists emitted)))))

    (testing "evaluate gives a verdict and writes the journal"
      (let [answer (apply run "evaluate" candidate "--against" path fast)]
        (is (= [0 "better" "better"]
               [(get answer "exit") (get answer "verdict") (get-in answer ["holdout" "verdict"])])
            (pr-str (select-keys answer ["error" "verdict" "reason" "gain"])))
        (is (str/starts-with? (get answer "next") "accept "))
        (is (boolean? (get-in answer ["sensitivity" "depends_on_weights"])))
        (is (= (mapv #(get % "pattern") (get-in answer ["baseline" "patterns"]))
               (mapv #(get % "pattern") (get-in answer ["sensitivity" "patterns"]))))
        (is (= 1 (get-in (run "status" path) ["journal" "records"])))))

    (testing "accept moves the orders, keeps the rest, and emits"
      (let [answer (run "accept" candidate "--into" path)
            accepted (plan/read-plan path)]
        (is (= [0 true start (get pick "hash")]
               [(get answer "exit") (get answer "changed") (get answer "from") (get answer "hash")]))
        (is (= (get pick "hash") (plan/plan-hash accepted)))
        (is (= 'demo.layout (get-in accepted [:emit :ns])) ":emit stays the plan's")
        (is (str/includes? (slurp emitted) (str "(def plan-hash \"" (get pick "hash") "\")")))
        (is (= "current" (get-in (run "status" path) ["emitted" "state"])))))

    (testing "accept again changes nothing"
      (let [before [(slurp path) (slurp emitted) (slurp (str path ".journal"))]
            answer (run "accept" candidate "--into" path)]
        (is (= [0 false false] [(get answer "exit") (get answer "changed") (get-in answer ["emitted" "written"])]))
        (is (= before [(slurp path) (slurp emitted) (slurp (str path ".journal"))]))))

    (testing "an emitted file that was edited is stale, and accept writes it again"
      (spit emitted (str (slurp emitted) "\n;; edited by hand\n"))
      (is (= ["stale" "file-edited"] (mapv (get (run "status" path) "emitted") ["state" "cause"])))
      (is (true? (get-in (run "accept" candidate "--into" path) ["emitted" "written"])))
      (is (= "current" (get-in (run "status" path) ["emitted" "state"]))))

    (testing "status tells an emitter that changed from a plan that changed"
      (let [source emit/source]
        (with-redefs [emit/source (fn newer
                                    ([p] (newer p nil))
                                    ([p opts] (str (source p opts) "\n;; a newer emitter\n")))]
          (let [emitted-now (get (run "status" path) "emitted")]
            (is (= ["stale" "emitter-changed"] (mapv emitted-now ["state" "cause"])))
            (is (str/includes? (get emitted-now "fix") "accept ")))))
      (let [accepted (slurp path)]
        (spit path (plan/render (fx/normal (assoc fx/small-risk :emit '{:ns demo.layout :path "gen/demo/layout.clj"}))))
        (let [emitted-now (get (run "status" path) "emitted")]
          (is (= "plan-changed" (get emitted-now "cause")))
          (is (str/includes? (get emitted-now "fix") (get pick "hash")) "the fix names the accepted plan")
          (is (not (str/includes? (get emitted-now "fix") "--into"))))
        (testing "another namespace is a change of the plan, not of the emitter"
          (spit path (str/replace accepted "demo.layout" "demo.other"))
          (is (= ["stale" "plan-renamed"] (mapv (get (run "status" path) "emitted") ["state" "cause"]))))
        (spit path accepted)
        (is (= "current" (get-in (run "status" path) ["emitted" "state"])))))

    (testing "the emitted file passes the gate, and a wrong one does not"
      (is (= 0 (get (run "verify" path "--source" emitted) "exit")))
      (let [wrong (io/file dir "wrong.clj")]
        (spit wrong (str/replace-first (slurp emitted) "(aget ptrs1 b1)" "(dec (aget ptrs1 b1))"))
        (is (not= (slurp wrong) (slurp emitted)))
        (let [answer (run "verify" path "--source" wrong)]
          (is (= [1 "gate_failed"] [(get answer "exit") (get-in answer ["error" "code"])]))
          (is (seq (get-in answer ["gate" "mismatches"]))))))

    (testing "a candidate of another task is refused, even when forced"
      (let [other (str (io/file dir "other.edn"))]
        (spit other (pr-str (assoc-in fx/small-risk [:workload :fanout] [2 2 2])))
        (is (= "task_mismatch" (get-in (run "accept" other "--into" path "--force" "because") ["error" "code"])))
        (is (= "task_mismatch" (get-in (run "evaluate" other "--against" path) ["error" "code"])))))

    (testing "--force accepts without a measurement and says so in the journal"
      (let [back (str (io/file dir "back.edn"))
            _ (spit back (pr-str fx/small-risk))
            answer (run "accept" back "--into" path "--force" "roll back for the test")]
        (is (= [0 true true] [(get answer "exit") (get answer "changed") (get answer "forced")]))
        (is (= start (get (run "check" path) "hash")))
        (is (str/includes? (slurp (str path ".journal")) ":forced \"roll back for the test\""))))))

(deftest descend-finds-the-indexed-order
  (let [dir (temp-dir)
        path (str (io/file dir "plan.edn"))
        _ (spit path (pr-str fx/small-risk))
        answer (apply run "descend" path "--max-steps" "1" fast)]
    (is (= [0 "better"] [(get answer "exit") (get answer "verdict")]) (pr-str (get answer "error")))
    (is (= 1 (count (get answer "steps"))))
    (is (= "add-order" (get-in answer ["steps" 0 "chosen" "move" "op"])))
    (is (= (get answer "final") (plan/plan-hash (plan/read-plan (get answer "candidate")))))
    (testing "every step carries the numbers of a report, and the chosen plan is on disk"
      (let [step (get-in answer ["steps" 0])
            chosen (get step "chosen")
            ids (mapv name (map :id (:patterns fx/small-risk)))]
        (is (pos? (long (get step "bytes"))))
        (is (= ids (mapv #(get % "pattern") (get step "patterns"))))
        (is (every? #(and (pos? (long (get % "bytes"))) (= (count ids) (count (get % "patterns"))))
                    (filter #(get % "cost") (get step "candidates"))))
        (is (= (get chosen "hash") (plan/plan-hash (plan/read-plan (get chosen "path")))))
        (is (= [(get answer "start") (get answer "final")]
               (mapv #(get-in answer ["measured" % "hash"]) ["baseline" "candidate"])))
        (is (= ids (mapv #(get % "pattern") (get-in answer ["measured" "candidate" "patterns"]))))))
    (testing "the journal reads as JSON, with the numbers of each pattern"
      (let [listed (run "journal" path)
            entry (first (get listed "entries"))]
        (is (= [0 1 1] [(get listed "exit") (get listed "records") (count (get listed "entries"))]))
        (is (= ["evaluate" (get answer "start") (get answer "final")]
               (mapv entry ["op" "baseline" "candidate"])))
        (is (every? #(and (number? (get % "baseline_ns")) (number? (get % "candidate_ns"))) (get entry "patterns")))
        (is (pos? (long (get entry "candidate_bytes"))))
        (is (= 2 (get (run "journal" path "--last" "0") "exit")))
        (is (= 2 (get (run "journal" (io/file dir "no-such-plan.edn")) "exit")) "a wrong path is not an empty journal")
        (is (= 0 (get listed "unreadable_lines")))
        (let [journal-file (str path ".journal")
              whole (slurp journal-file)]
          (spit journal-file (str whole "{:op :evaluate :verdict"))
          (is (= [1 1] (mapv (run "journal" path) ["records" "unreadable_lines"])))
          (spit journal-file whole))))
    (testing "measure times plans of one task and writes nothing"
      (let [before (slurp (str path ".journal"))
            measured (apply run "measure" path (get answer "candidate") fast)
            [a b] (get measured "plans")]
        (is (= 0 (get measured "exit")) (pr-str (get measured "error")))
        (is (= [(get answer "start") (get answer "final")] [(get a "hash") (get b "hash")]))
        (is (every? #(and (number? (get % "cost")) (pos? (long (get % "bytes"))) (seq (get % "patterns"))) [a b]))
        (is (= before (slurp (str path ".journal"))))
        (is (= "same_plan" (get-in (run "measure" path path) ["error" "code"])))
        (is (= 2 (get (run "measure") "exit")))))
    (testing "reach counts what the walk timed and says what it does not try"
      (let [reach (get answer "reach")
            candidates (mapcat #(get % "candidates") (get answer "steps"))]
        (is (= (count (distinct (map #(get % "hash") (filter #(get % "cost") candidates)))) (get reach "timed")))
        (is (= (count candidates) (reduce + (map #(get % "candidates") (get reach "moves")))))
        (is (every? string? (get reach "not_tried")))
        (is (seq (get reach "not_tried")))
        (is (= ["memory-not-priced"] (mapv #(get % "code") (get answer "warnings")))
            "the final plan has two orders and the objective does not price memory")))
    (testing "a step takes the better candidate with the largest gain, not the lowest cost of any batch"
      (doseq [step (get answer "steps")
              :let [timed (filter #(get % "cost") (get step "candidates"))
                    better (filter #(= "better" (get % "verdict")) timed)
                    chosen (get-in step ["chosen" "hash"])]]
        (is (every? #(number? (get % "gain")) timed))
        (when chosen
          (is (= (apply max (map #(get % "gain") better))
                 (get (first (filter #(= chosen (get % "hash")) better)) "gain"))))))
    (testing "--save writes the answer, and never over a file of the plan"
      (let [file (str (io/file dir "out" "status.json"))
            answer (run "status" path "--save" file)
            files #(set (map str (file-seq (io/file dir))))
            before (files)]
        (is (= [0 file] [(get answer "exit") (get answer "saved")]))
        (is (= (dissoc answer "exit") (json/read-str (slurp file))))
        (doseq [protected [path (str path ".journal")]
                :let [text (slurp protected)]]
          (is (= "path_collision" (get-in (run "status" path "--save" protected) ["error" "code"])))
          (is (= "path_collision" (get-in (run "measure" path "--save" protected) ["error" "code"])))
          (is (= text (slurp protected))))
        (is (= 2 (get (run "status" path "--save" dir) "exit")))
        (is (= before (files)))))
    (testing "--save never names a file where the command writes or a source file that it reads"
      (let [refused #(get-in (apply run %&) ["error" "code"])
            kept (str (io/file dir "candidates" "any" "trace.edn"))
            out-dir (str (io/file dir "nb"))
            sources (io/file dir "sources")
            source (str (io/file sources "sample" "core.clj"))
            text "(ns sample.core)\n(defn value [m a b] (get-in m [a b]))\n"]
        (io/make-parents source)
        (spit source text)
        (is (= "path_collision" (refused "descend" path "--max-steps" "1" "--save" kept)))
        (is (= "path_collision" (refused "descend" path "--max-steps" "1" "--save"
                                         (str (io/file dir "CANDIDATES" "any" "trace.json"))))
            "candidates/ does not exist yet, so its letter case cannot be asked of the file system")
        (is (= "path_collision" (refused "neighbors" path "--out-dir" out-dir "--save" (str (io/file out-dir "x" "y.edn")))))
        (is (= "path_collision" (refused "neighbors" path "--out-dir" out-dir "--save" out-dir)))
        (is (not (.exists (io/file out-dir))) "refused before the command wrote")
        (is (= "path_collision" (refused "facts" (str sources) "--save" source)))
        (is (= "path_collision" (refused "facts" (str sources) "--save" (str (io/file sources "sample" "CORE.CLJ")))))
        (is (= text (slurp source)))))
    (testing "status gives the workload as the plan states it"
      (is (= {"fanout" [4 60 8] "fill" [1.0 0.9 0.6] "seed" 7} (get (run "status" path) "workload"))))
    (testing "report holds what a report needs and writes nothing"
      (let [files #(set (map str (file-seq (io/file dir))))
            before (files)
            report (run "report" path)
            status (run "status" path)]
        (is (= 0 (get report "exit")) (pr-str (get report "error")))
        (is (= before (files)))
        (is (= (dissoc status "journal" "command") (select-keys report (keys (dissoc status "journal" "command")))))
        (is (= (slurp path) (get report "plan_text")))
        (is (= (emit/source (plan/read-plan path)) (get report "source")))
        (is (= (get (run "journal" path) "entries") (get-in report ["journal" "entries"])))
        (is (= (dissoc (get status "journal") "last") (dissoc (get report "journal") "entries")))
        (is (= [(get answer "final")] (mapv #(get % "hash") (get report "candidates"))))
        (is (= (get-in answer ["steps" 0 "chosen" "path"]) (get-in report ["candidates" 0 "path"])))
        (is (every? #(and (get % "pattern") (number? (get % "order"))) (get-in report ["candidates" 0 "serve"])))))
    (testing "descend does not write the plan; accept does"
      (is (= (get answer "start") (get (run "check" path) "hash")))
      (is (= 0 (get (run "accept" (get answer "candidate") "--into" path) "exit")))
      (is (= (get answer "final") (get (run "check" path) "hash"))))))

(deftest a-split-is-offered-for-a-neighbor-that-wins-and-loses
  (let [base (fx/normal fx/small-risk)
        csf (assoc-in base [:orders 0 :format] :csf)
        row (fn [faster slower] {:hash (plan/plan-hash csf) :plan csf :faster faster :slower slower})
        splits #(#'cli/split-candidates base [%])
        [split :as found] (splits (row [:tenant-size] [:value]))]
    (is (= 1 (count found)))
    (is (= {:op :split :with (plan/plan-hash csf) :serves [:tenant-size]} (:move split)))
    (is (= (plan/plan-hash (plan/split base csf [:tenant-size])) (:hash split)))
    (is (:gate-ok split))
    (is (= 2 (count (:orders (:plan split)))))
    (testing "a neighbor that only wins, or only loses, gives no split"
      (is (= [] (splits (row [:tenant-size] []))))
      (is (= [] (splits (row [] [:value])))))
    (testing "a split that equals a timed candidate is not timed twice"
      (is (= [] (#'cli/split-candidates base [(row [:tenant-size] [:value])
                                              (assoc (row [] []) :hash (:hash split))]))))))

(deftest notes-and-degree-pass-through-the-commands
  (let [dir (temp-dir)
        path (str (io/file dir "plan.edn"))
        notes ["Sizes from bench_data.clj." "Leaf simplified to one double."]
        input (-> fx/small-risk
                  (assoc :notes notes :emit '{:ns demo.layout :path "gen/demo/layout.clj"})
                  (assoc-in [:workload :degree] [nil nil 3]))
        candidate (str (io/file dir "candidate.edn"))]
    (spit path (str ";; a comment that accept drops\n" (pr-str input)))
    (spit candidate (plan/render (fx/with-order input [2 0 1] :csf [:feature-total])))
    (is (= 0 (get (run "accept" candidate "--into" path "--force" "test") "exit")))
    (is (= notes (:notes (plan/read-plan path))))
    (is (= [nil nil 3] (get-in (plan/read-plan path) [:workload :degree])))
    (is (not (str/includes? (slurp path) "a comment")))
    (is (= notes (get (run "status" path) "notes")))
    (testing "two orders with free memory are a warning of check"
      (is (some #(= "memory-not-priced" (get % "code")) (get (run "check" path) "warnings"))))
    (testing "a candidate with another degree is another task"
      (let [other (str (io/file dir "other.edn"))]
        (spit other (pr-str (assoc-in input [:workload :degree] [nil nil 4])))
        (is (= "task_mismatch" (get-in (run "evaluate" other "--against" path) ["error" "code"])))))
    (testing "every entity has exactly three features in the measured data"
      (let [measured (apply run "measure" path fast)
            entities (count (for [[_ es] (layout-descent.gen/data (plan/read-plan path)) e es] e))]
        (is (= 0 (get measured "exit")) (pr-str (get measured "error")))
        (is (= (* 3 entities) (get measured "entries")))))))

(deftest facts-command
  (let [answer (run "facts" "test/fixtures/sample/src" "--limit" "1")]
    (is (= 0 (get answer "exit")))
    (is (= [":scores" "?" "?" ":risk"] (get-in answer ["shapes" 0 "shape"])))
    (testing "shallow shapes and writes are counted, and shown on request"
      (let [all (run "facts" "test/fixtures/sample/src" "--limit" "0" "--min-depth" "0" "--writes" "true")]
        (is (pos? (long (get-in answer ["hidden" "sites"]))))
        (is (pos? (long (get-in answer ["hidden" "writes"]))))
        (is (= [0 0] (mapv #(get-in all ["hidden" %]) ["sites" "writes"])))
        (is (< (count (get answer "shapes")) (count (get all "shapes"))))
        (is (every? #(empty? (get % "sites")) (get all "shapes")))
        (is (not (str/starts-with? (get-in answer ["shapes" 0 "sites" 0 "file"]) "/")))
        (is (= 2 (get (run "facts" "test/fixtures/sample/src" "--writes" "yes") "exit")))))
    (is (= 2 (get (run "facts" "no/such/dir") "exit")))))

(deftest acceptance-writes-and-recovery
  (let [dir (temp-dir)
        base (io/file dir "plan.edn")
        candidate (io/file dir "candidate.edn")
        blocked (io/file dir "blocked")
        out (io/file blocked "layout.clj")
        input (assoc fx/small-risk :emit '{:ns demo.layout :path "blocked/layout.clj"})
        better (fx/with-order input [2 0 1] :csf [:feature-total])
        accept #(run "accept" candidate "--into" base "--force" "keep this reason")
        pending (io/file (str base ".accept-pending"))
        journal (io/file (str base ".journal"))]
    (spit base (plan/render input))
    (spit candidate (plan/render better))
    (spit blocked "not a directory")
    (is (= 2 (get (accept) "exit")))
    (is (= (plan/render input) (slurp base)))
    (is (not (.exists pending)))
    (is (not (.exists journal)))
    (is (.delete blocked))
    (let [publish! plan/publish-text!]
      (with-redefs [plan/publish-text! (fn [tmp dest]
                                       (if (= (.getCanonicalPath base) (str dest))
                                         (throw (java.io.IOException. "interrupted before plan publication"))
                                         (publish! tmp dest)))]
        (is (= "accept_incomplete" (get-in (accept) ["error" "code"])))))
    (is (= (plan/render input) (slurp base)))
    (is (.exists out))
    (is (.exists pending))
    (is (true? (get (run "accept" candidate "--into" base) "recovered")))
    (is (= (plan/render better) (slurp base)))
    (is (not (.exists pending)))
    (is (= ["keep this reason"] (mapv #(get % "forced") (get-in (run "status" base) ["journal" "last"]))))))

(deftest acceptance-needs-the-latest-verdict-for-the-current-objective
  (let [dir (temp-dir)
        base (io/file dir "plan.edn")
        candidate (io/file dir "candidate.edn")
        journal (io/file (str base ".journal"))
        input (fx/normal fx/small-risk)
        next-plan (fx/with-order input [2 0 1] :csf [:feature-total])
        common {:op :evaluate :baseline (plan/plan-hash input) :candidate (plan/plan-hash next-plan)
                :task (plan/task-hash input)}
        accept #(run "accept" candidate "--into" base)]
    (spit base (plan/render input))
    (spit candidate (plan/render next-plan))
    (doseq [rows [[(assoc common :verdict :better)]
                  [(assoc common :verdict :better :objective {:ns-per-byte 0.0 :min-gain 0.1})]
                  (mapv #(assoc common :objective (:objective input) :verdict %) [:better :worse])]]
      (spit journal (str/join "\n" (map pr-str rows)))
      (is (= "not_measured" (get-in (accept) ["error" "code"]))))
    (spit journal (str (pr-str (assoc common :objective (:objective input) :verdict :better)) "\n"))
    (is (true? (get (accept) "changed")))))

(deftest outputs-cannot-replace-protected-files
  (let [dir (temp-dir)
        base (io/file dir "plan.edn")
        alias (io/file dir "alias.edn")
        candidate (io/file dir "candidate.edn")
        input (assoc fx/small-risk :emit '{:ns demo.layout :path "./plan.edn"})]
    (spit base (plan/render input))
    (spit candidate (plan/render (fx/with-order input [2 0 1] :csf [:feature-total])))
    (Files/createLink (.toPath alias) (.toPath base))
    (doseq [answer [(run "emit" base "--out" alias)
                    (run "descend" base "--out" (str base ".journal"))
                    (run "accept" candidate "--into" base "--force" "test")]]
      (is (= "path_collision" (get-in answer ["error" "code"]))))
    (is (= (plan/render input) (slurp base)))))

(deftest different-tasks-get-separate-candidate-paths
  (let [dir (temp-dir)
        a (io/file dir "a.edn")
        b (io/file dir "b.edn")
        out (io/file dir "candidates")]
    (spit a (plan/render fx/small-risk))
    (spit b (plan/render (assoc-in fx/small-risk [:workload :seed] 121)))
    (let [paths (fn [p] (set (map #(get % "path") (get (run "neighbors" p "--out-dir" out) "neighbors"))))
          as (paths a)
          bs (paths b)]
      (is (seq as))
      (is (= (count as) (count bs)))
      (is (not-any? as bs)))))
