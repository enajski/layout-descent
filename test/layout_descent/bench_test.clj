(ns layout-descent.bench-test
  (:require [clojure.test :refer [deftest is testing]]
            [layout-descent.bench :as bench]
            [layout-descent.gen :as gen]
            [layout-descent.plan :as plan]
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

(deftest the-time-is-the-fastest-generation
  (let [center @(ns-resolve 'layout-descent.bench 'center)]
    (testing "one slow compilation moves neither the time nor the spread"
      (is (= {:ns 10.0 :spread 0.0} (center [[10.0 10.0 10.0] [14.0 14.0 14.0] [10.0 10.0 10.0]]))))
    (testing "generations that disagree raise the spread"
      (is (= {:ns 10.0 :spread 2.0} (center [[10.0 10.0 10.0] [12.0 12.0 12.0] [15.0 15.0 15.0]]))))))

(deftest every-generation-compiles-again-and-warms-another-plan-first
  (let [base (fx/normal fx/small-risk)
        indexed (fx/with-order fx/small-risk [2 0 1] :csf [:feature-total])
        prepare-var (ns-resolve 'layout-descent.bench 'prepare)
        warm-var (ns-resolve 'layout-descent.bench 'warm)
        prepare @prepare-var
        warm @warm-var
        calls (atom 0)
        warmed (atom [])]
    (with-redefs-fn {prepare-var (fn [plan data samples]
                                   (let [p (prepare plan data samples)
                                         owner {:hash (:hash p) :generation (quot (dec (long (swap! calls inc))) 2)}]
                                     (update p :cases (fn [cases] (mapv #(assoc % :owner owner) cases)))))
                     warm-var (fn [case opts]
                                (swap! warmed conj (assoc (:owner case) :loop (class ((:loop case)))))
                                (warm case opts))}
      (fn []
        (let [[b c] (bench/measure [base indexed] {:rounds 6 :generations 3 :warm-ms 10 :target-ms 1 :args 8})
              firsts (mapv (comp :hash first) (partition-by :generation @warmed))]
          (is (= 6 @calls) "two plans, three generations")
          (is (= [(:hash b) (:hash c) (:hash b)] firsts))
          (is (apply distinct? (map :loop @warmed)) "every loop is a class of its own")
          (is (every? #(pos? (double (:ns %))) (mapcat :patterns [b c]))))))))

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

(deftest batches-share-paths-and-price-but-keep-paired-baselines
  (let [base (assoc-in (fx/normal fx/small-risk) [:objective :ns-per-byte] 1.0)
        candidates (mapv #(assoc-in (:plan %) [:objective :ns-per-byte] 0.0)
                         (take 5 (plan/neighbors base)))
        plans (into [base] candidates)
        paths gen/paths
        walks (atom 0)
        batches (atom [])
        measure-batch (ns-resolve 'layout-descent.bench 'measure-batch)
        batch (fn [ps _ _ _ entries seed]
                (swap! batches conj ps)
                (mapv #(hash-map :hash (plan/plan-hash %) :entries entries :seed seed
                                 :objective (:objective %) :batch (count @batches)) ps))]
    (with-redefs-fn {measure-batch batch
                     #'gen/paths (fn [data n] (when (= 3 n) (swap! walks inc)) (paths data n))}
      (fn []
        (let [[b & cs :as results] (bench/measure plans quick)]
          (is (= (mapv plan/plan-hash plans) (mapv :hash results)))
          (is (= 1 @walks))
          (is (= [5 2] (mapv count @batches)))
          (is (every? #(= (:objective base) (:objective %)) results))
          (is (every? #(= (:batch %) (get-in % [:batch-baseline :batch])) cs))
          (is (not= (:batch b) (:batch (last cs)))))))))

(deftest sensitivity-says-when-the-weights-decide
  (let [summary (fn [bytes & ns] {:bytes bytes
                                  :patterns (mapv (fn [id n] {:pattern id :ns n :spread 0.1 :weight 1}) [:a :b] ns)})
        sensitivity #(bench/weight-sensitivity {:ns-per-byte %1} %2 %3)
        mixed (sensitivity 0.0 (summary 100 10.0 100.0) (summary 100 20.0 50.0))]
    (is (true? (:depends-on-weights mixed)))
    (is (= [:worse :better] (mapv :effect (:patterns mixed))))
    (testing "five times the weight of the slower pattern cancels the gain of the other"
      (is (= [5.0 0.2] (mapv :flip-factor (:patterns mixed)))))
    (testing "a gain on every pattern holds for any weights"
      (let [clear (sensitivity 0.0 (summary 100 10.0 100.0) (summary 100 5.0 50.0))]
        (is (false? (:depends-on-weights clear)))
        (is (= [nil nil] (mapv :flip-factor (:patterns clear))))))
    (testing "a difference inside the noise is no effect"
      (is (= [:same :same] (mapv :effect (:patterns (sensitivity 0.0 (summary 100 10.0 100.0) (summary 100 10.2 99.9)))))))
    (testing "priced memory is charged to every pattern"
      (let [priced (sensitivity 1.0 (summary 100 10.0 100.0) (summary 110 5.0 50.0))]
        (is (= -10.0 (:memory-ns priced)))
        (is (= [-5.0 40.0] (mapv :net-ns (:patterns priced))))
        (is (= [:worse :better] (mapv :effect (:patterns priced))))
        (is (= [8.0 0.125] (mapv :flip-factor (:patterns priced))))
        (is (true? (:depends-on-weights priced)))))
    (testing "memory that costs more than the largest gain loses for any weights"
      (let [dear (sensitivity 1.0 (summary 100 10.0 100.0) (summary 200 5.0 50.0))]
        (is (= [:worse :worse] (mapv :effect (:patterns dear))))
        (is (false? (:depends-on-weights dear)))
        (is (= [nil nil] (mapv :flip-factor (:patterns dear))))))))
