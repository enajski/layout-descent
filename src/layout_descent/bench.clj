(ns layout-descent.bench
  "Measurement. Plans of one task are built from the same data, called with the same arguments and
  timed in interleaved rounds, so drift of the machine falls on all of them alike."
  (:require [layout-descent.gate :as gate]
            [layout-descent.gen :as gen]
            [layout-descent.plan :as plan]
            [layout-descent.rt :as rt]))

(set! *warn-on-reflection* true)

(def defaults
  "Arguments per pattern, timed rounds, compilations that share the rounds, warm-up per pattern
  and compilation, target time of one sample, and the share of arguments that miss."
  {:args 64 :rounds 15 :generations 3 :warm-ms 150 :target-ms 4 :miss 0.1})

;; The reducing function of a timed scan: fixed arities, no allocation.
(def ^:private keep-leaf
  (fn
    ([_ _ leaf] leaf)
    ([_ _ _ leaf] leaf)
    ([_ _ _ _ leaf] leaf)
    ([_ _ _ _ _ leaf] leaf)))

;; The reducing function of a timed scan is passed to the loop, so no call reads a var.

(defn- loop-form
  "The source of a timing loop for one arity: a function of [f keep sink] that returns a function
  of [ds args reps]. That one calls f with every argument vector reps times. The results feed the
  sink, so no call is dead code."
  [reduce? arity]
  (let [call (concat '(f ds) (when reduce? '(keep nil)) (map #(list 'aget 'a %) (range arity)))]
    (list 'fn '[f keep ^longs sink]
          (list 'fn '[ds ^objects args ^long reps]
                (list 'let '[n (alength args)]
                      (list 'loop '[r 0 i 0 acc 0]
                            (list 'cond
                                  '(= r reps) '(aset sink 0 (unchecked-add (aget sink 0) acc))
                                  '(= i n) '(recur (inc r) 0 acc)
                                  :else (list 'let ['^objects a '(aget args i)
                                                    'x call]
                                              '(recur r (inc i)
                                                      (if (nil? x) acc (unchecked-add acc (long (.hashCode ^Object x)))))))))))))

(def ^:private ^"[J" sink (long-array 1))

(defn- timing-loop
  "Compile a timing loop for f alone. A loop that all plans shared was one call site for the
  functions of all of them, and the JIT compiled it for the first plans that it saw: has-value of
  one layout then differed by 3 to 15 % with its position among four plans. A class of its own
  gives every case a call site that sees one function, as a call site of an application does."
  [f reduce? arity]
  (let [make (binding [*ns* (the-ns 'layout-descent.bench)]
               (eval (loop-form reduce? arity)))]
    (make f keep-leaf sink)))

(defn- sample
  "Nanoseconds per call of one batch."
  ^double [{:keys [run ds args reps]}]
  (let [^objects args args
        reps (long reps)
        start (System/nanoTime)]
    (run ds args reps)
    (/ (double (- (System/nanoTime) start)) (double (* reps (alength args))))))

(defn- spin
  "Call a case for ms, three times at least."
  [case ms]
  (let [deadline (+ (System/nanoTime) (* 1000000 (long ms)))]
    (loop [runs 0]
      (sample case)
      (when (or (< runs 2) (< (System/nanoTime) deadline))
        (recur (inc runs))))))

(def ^:private timed-ns
  "The shortest batch that sets reps: about 2,400 steps of System/nanoTime on macOS."
  100000.0)

(defn- reps-for
  "Repetitions that make one batch last target-ns. batch-ns gives the time of a batch of reps.
  Reps grow tenfold until a batch lasts timed-ns. A scan with no key has one argument vector, so
  its batch of one call is shorter than a step of the timer: it read 0 or 42 ns, and reps came out
  at 4,000,000 or 95,238 where 216,000 fill the target."
  ^long [batch-ns ^double target-ns]
  (loop [reps 1]
    (let [ns (double (batch-ns reps))]
      (if (< ns (double timed-ns))
        (recur (* 10 reps))
        (max 1 (long (/ (* target-ns reps) ns)))))))

(defn- warm
  "Warm a case for warm-ms, then set reps so one sample lasts target-ms. The first third runs in a
  loop that is thrown away, so the JIT compiles the function by itself before it compiles the loop
  that is timed. When the two compilations raced, the loop sometimes got its own copy of the
  function: has-value of one plan then took 13.7 ns in place of 11.9 ns."
  [case {:keys [warm-ms target-ms]}]
  (let [first-ms (quot (long warm-ms) 3)
        _ (spin (assoc case :run ((:loop case)) :reps 1) first-ms)
        one (assoc case :run ((:loop case)) :reps 1)
        _ (spin one (- (long warm-ms) first-ms))
        n (alength ^objects (:args case))
        batch-ns (fn [reps]
                   (* (double (apply min (repeatedly 3 #(sample (assoc one :reps reps))))) (long reps) n))]
    (assoc one :reps (reps-for batch-ns (* 1000000.0 (long target-ms))))))

(defn- median [xs]
  (let [v (vec (sort xs))
        n (count v)]
    (if (odd? n)
      (double (nth v (quot n 2)))
      (/ (+ (double (nth v (quot n 2))) (double (nth v (dec (quot n 2))))) 2.0))))

(defn- check-sample
  "Compare the first arguments of every pattern with the model before any timing."
  [plan layout data ds samples]
  (gate/check-built plan layout "benchmark" data ds
                    (into {} (map (fn [[id args]] [id (take 3 args)])) samples)))

(defn- prepare [plan data samples]
  (let [layout (gate/load-layout plan)
        _ (when (seq (:warnings layout))
            (throw (ex-info (str "Plan " (:hash layout) " compiles with warnings.")
                            {:code :gate-failed :warnings (:warnings layout)})))
        start (System/nanoTime)
        ds ((:build layout) data)
        build-ms (/ (double (- (System/nanoTime) start)) 1e6)
        checked (check-sample plan layout data ds samples)]
    (when (seq (:mismatches checked))
      (throw (ex-info (str "Plan " (:hash layout) " disagrees with the model on the benchmark data.")
                      {:code :gate-failed :mismatches (:mismatches checked)})))
    {:plan plan
     :hash (:hash layout)
     :bytes (rt/estimate-bytes ds (count (:levels plan)))
     :build-ms build-ms
     :cases (mapv (fn [pattern]
                    (let [id (:id pattern)
                          args (get samples id)]
                      {:id id
                       :ds ds
                       :args (object-array (map object-array args))
                       :loop (let [f (get (:fns layout) id)
                                   reduce? (plan/reduce? pattern)
                                   arity (count (first args))]
                               #(timing-loop f reduce? arity))}))
                  (:patterns plan))}))

(defn- center
  "The time and the spread of one case from the samples of each generation. The time is the lowest
  median of a generation: a compilation can make the code slower than it is, not faster. The
  spread is the median distance of a sample from that time, so it grows when the generations
  disagree."
  [generations]
  (let [medians (mapv median generations)
        ns (double (apply min medians))]
    {:ns ns
     :spread (median (map #(Math/abs (- (double %) ns)) (apply concat generations)))}))

(defn- summarize [plan prepared times]
  (let [weights (:weights (:workload plan))
        total (double (reduce + 0 (vals weights)))
        rows (mapv (fn [{:keys [id reps]}]
                     (let [{:keys [ns spread]} (center (get times [(:hash prepared) id]))]
                       {:pattern id
                        :ns ns
                        :spread spread
                        :weight (get weights id)
                        :order (get (:serve plan) id)
                        :reps reps}))
                   (:cases prepared))
        weighted (fn [k] (/ (double (reduce + 0.0 (map #(* (double (:weight %)) (double (k %))) rows))) total))
        time-ns (weighted :ns)
        byte-ns (* (double (:ns-per-byte (:objective plan))) (double (:bytes prepared)))]
    {:hash (:hash prepared)
     :cost (+ time-ns byte-ns)
     :time-ns time-ns
     :noise (weighted :spread)
     :bytes (:bytes prepared)
     :build-ms (:build-ms prepared)
     :patterns rows}))

(def ^:private candidate-batch-size 4)

(defn- generation
  "Compile every plan again, warm its cases and take `rounds` samples of each in interleaved
  rounds. Plan `g` is warmed first. Returns the plans without their data and the samples by
  [hash pattern]."
  [plans data samples opts g rounds]
  (let [prepared (mapv #(prepare % data samples) plans)
        _ (System/gc)
        n (count plans)
        warmed (reduce (fn [ps i] (update-in ps [i :cases] (fn [cases] (mapv #(warm % opts) cases))))
                       prepared
                       (map #(mod (+ (long g) (long %)) n) (range n)))
        times (reduce (fn [acc [p case]]
                        (update acc [(:hash p) (:id case)] (fnil conj []) (sample case)))
                      {}
                      (for [_ (range rounds) p warmed case (:cases p)] [p case]))]
    {:prepared (mapv (fn [p] (update p :cases (fn [cases] (mapv #(select-keys % [:id :reps]) cases)))) warmed)
     :times times}))

(defn- measure-batch
  "Time the plans over :generations compilations. One compilation of one layout can be slower than
  another of the same layout, and the first case that a compilation warms is so most often. So
  each generation gets new classes and warms another plan first, and `center` takes the fastest."
  [plans data samples opts entries seed]
  (let [k (max 1 (long (:generations opts)))
        rounds (max 2 (long (Math/ceil (/ (double (:rounds opts)) k))))
        generations (mapv #(generation plans data samples opts % rounds) (range k))
        times (apply merge-with into (map (fn [g] (update-vals (:times g) vector)) generations))]
    (mapv (fn [plan p] (assoc (summarize plan p times) :entries entries :seed seed))
          plans (:prepared (first generations)))))

(defn measure
  "Time normal plans of one task. All layouts use the first plan's objective. Each candidate is
   measured alongside the baseline in its own batch. Returns summaries in input order; a candidate
   summary carries the baseline of its batch as :batch-baseline."
  [plans opts]
  (let [opts (merge defaults opts)
        base (first plans)]
    (when-not (apply = (map plan/task-key plans))
      (throw (ex-info "The plans do not share one task, so their costs do not compare."
                      {:code :task-mismatch :tasks (mapv plan/task-hash plans)})))
    (let [seed (unchecked-add (long (get-in base [:workload :seed])) (long (:seed-offset opts 0)))
          data (gen/data base {:seed seed})
          key-paths (gen/paths data (count (:levels base)))
          entries (count key-paths)
          samples (into {}
                        (map (fn [pattern]
                               [(:id pattern)
                                (gen/arg-samples base pattern data
                                                 {:n (:args opts) :seed (unchecked-add seed 101)
                                                  :miss (:miss opts) :key-paths key-paths})]))
                        (:patterns base))
          batches (or (seq (partition-all candidate-batch-size (rest plans))) [[]])]
      (reduce (fn [out candidates]
                (let [[b & cs] (measure-batch (into [base] (map #(assoc % :objective (:objective base))) candidates)
                                              data samples opts entries seed)]
                  (into (if (seq out) out [b]) (map #(assoc % :batch-baseline b)) cs)))
              [] batches))))

(defn verdict
  "Compare a candidate summary with a baseline summary. :better and :worse need a difference of
  at least :min-gain of the baseline and of more than three times the summed noise."
  [objective base candidate]
  (let [delta (- (double (:cost base)) (double (:cost candidate)))
        noise (* 3.0 (+ (double (:noise base)) (double (:noise candidate))))
        gain (if (pos? (double (:cost base))) (/ delta (double (:cost base))) 0.0)
        clear? (and (> (Math/abs delta) noise) (>= (Math/abs gain) (double (:min-gain objective))))]
    {:verdict (cond (and clear? (pos? delta)) :better
                    (and clear? (neg? delta)) :worse
                    :else :no-change)
     :gain gain
     :delta-ns delta
     :noise-ns noise}))

(defn weight-sensitivity
  "How the comparison of two summaries depends on :weights. The cost difference is the weighted
  mean of :net-ns, the time a pattern gains plus the memory charge that every pattern carries. A
  pattern is :better, :worse or :same for the candidate by :net-ns against three times its noise;
  the weights decide only when both :better and :worse occur. :flip-factor is the multiple of that
  pattern's weight at which the difference changes sign with the other weights fixed; it is nil
  for a :same pattern and when no positive multiple does that."
  [objective base candidate]
  (let [memory (* (double (:ns-per-byte objective)) (- (double (:bytes base)) (double (:bytes candidate))))
        rows (mapv (fn [b c]
                     (let [delta (- (double (:ns b)) (double (:ns c)))
                           net (+ delta memory)
                           noise (* 3.0 (+ (double (:spread b)) (double (:spread c))))]
                       {:pattern (:pattern b)
                        :weight (:weight b)
                        :delta-ns delta
                        :net-ns net
                        :noise-ns noise
                        :effect (cond (> net noise) :better (< net (- noise)) :worse :else :same)}))
                   (:patterns base) (:patterns candidate))
        sum (double (reduce + 0.0 (map #(* (double (:weight %)) (double (:net-ns %))) rows)))
        flip (fn [{:keys [weight net-ns effect]}]
               (let [own (* (double weight) (double net-ns))
                     m (when-not (or (= :same effect) (zero? own))
                         (/ (- own sum) own))]
                 (when (and m (pos? (double m)) (Double/isFinite (double m))) m)))
        rows (mapv #(assoc % :flip-factor (flip %)) rows)
        effects (set (map :effect rows))
        mixed? (boolean (and (effects :better) (effects :worse)))]
    {:depends-on-weights mixed?
     :summary (cond
                mixed? "The candidate wins on some patterns and loses on others: the verdict depends on :weights. flip_factor is the multiple of one weight that reverses the sign of the difference."
                (effects :better) "The candidate loses on no pattern beyond its noise: the direction holds for any weights."
                (effects :worse) "The candidate wins on no pattern beyond its noise: the direction holds for any weights."
                :else "No pattern differs beyond its noise: no weights give a clear verdict.")
     :memory-ns memory
     :patterns rows}))
