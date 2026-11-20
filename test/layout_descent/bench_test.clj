(ns layout-descent.bench-test
  (:require [clojure.test :refer [deftest is testing]]
            [layout-descent.bench :as bench]
            [layout-descent.fixtures :as fx]))

(def ^:private quick {:rounds 7 :warm-ms 40 :target-ms 2 :args 16})

(deftest an-indexed-order-beats-a-scan
  (let [base (fx/normal fx/small-risk)
        indexed (fx/with-order fx/small-risk [2 0 1] :csf [:feature-total])
        [b c] (bench/measure [base indexed] quick)
        ns-of (fn [summary id] (:ns (first (filter #(= id (:pattern %)) (:patterns summary)))))]
    (is (= (:entries b) (:entries c)) "both plans hold the same data")
    (is (< (double (ns-of c :feature-total)) (double (ns-of b :feature-total))))
    (is (> (long (:bytes c)) (long (:bytes b))) "the second order costs memory")
    (is (= :better (:verdict (bench/verdict (:objective base) b c))))
    (is (= :worse (:verdict (bench/verdict (:objective base) c b))))
    (testing "a high price of memory turns the verdict"
      (let [priced (fn [s] (assoc s :cost (+ (double (:time-ns s)) (* 1.0 (double (:bytes s))))))]
        (is (= :worse (:verdict (bench/verdict (:objective base) (priced b) (priced c)))))))))

(deftest plans-of-different-tasks-do-not-compare
  (let [base (fx/normal fx/small-risk)
        other (fx/normal (assoc-in fx/small-risk [:workload :seed] 8))]
    (is (= :task-mismatch
           (try (bench/measure [base other] quick)
                (catch clojure.lang.ExceptionInfo e (:code (ex-data e))))))))

(deftest a-verdict-needs-a-clear-difference
  (let [objective {:min-gain 0.05}
        verdict #(:verdict (bench/verdict objective {:cost %1 :noise %2} {:cost %3 :noise %4}))]
    (is (= :better (verdict 100.0 1.0 80.0 1.0)))
    (is (= :worse (verdict 80.0 1.0 100.0 1.0)))
    (is (= :no-change (verdict 100.0 1.0 97.0 0.1)) "below :min-gain")
    (is (= :no-change (verdict 100.0 4.0 80.0 4.0)) "within three times the noise")
    (is (= :no-change (verdict 100.0 0.0 100.0 0.0)))))
