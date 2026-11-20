(ns layout-descent.cli-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [layout-descent.cli :as cli]
            [layout-descent.fixtures :as fx]
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
      (is (= "stale" (get-in (run "status" path) ["emitted" "state"])))
      (is (true? (get-in (run "accept" candidate "--into" path) ["emitted" "written"])))
      (is (= "current" (get-in (run "status" path) ["emitted" "state"]))))

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
    (testing "descend does not write the plan; accept does"
      (is (= (get answer "start") (get (run "check" path) "hash")))
      (is (= 0 (get (run "accept" (get answer "candidate") "--into" path) "exit")))
      (is (= (get answer "final") (get (run "check" path) "hash"))))))

(deftest facts-command
  (let [answer (run "facts" "test/fixtures/sample/src" "--limit" "1")]
    (is (= 0 (get answer "exit")))
    (is (= [":scores" "?" "?" ":risk"] (get-in answer ["shapes" 0 "shape"])))
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
