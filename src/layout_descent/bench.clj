(ns layout-descent.bench
  "Measurement. Plans of one task are built from the same data, called with the same arguments and
  timed in interleaved rounds, so drift of the machine falls on all of them alike."
  (:require [layout-descent.gate :as gate]
            [layout-descent.gen :as gen]
            [layout-descent.plan :as plan]
            [layout-descent.rt :as rt]))

(set! *warn-on-reflection* true)

(def defaults
  "Arguments per pattern, timed rounds, warm-up per pattern, target time of one sample, and the
  share of arguments that miss."
  {:args 64 :rounds 15 :warm-ms 150 :target-ms 4 :miss 0.1})

;; The reducing function of a timed scan: fixed arities, no allocation.
(def ^:private keep-leaf
  (fn
    ([_ _ leaf] leaf)
    ([_ _ _ leaf] leaf)
    ([_ _ _ _ leaf] leaf)
    ([_ _ _ _ _ leaf] leaf)))

(defn- invoker
  "A function of [ds args] that calls f at its real arity."
  [f reduce? arity]
  (if reduce?
    (case (int arity)
      0 (fn [ds ^objects a] (f ds keep-leaf nil))
      1 (fn [ds ^objects a] (f ds keep-leaf nil (aget a 0)))
      2 (fn [ds ^objects a] (f ds keep-leaf nil (aget a 0) (aget a 1)))
      3 (fn [ds ^objects a] (f ds keep-leaf nil (aget a 0) (aget a 1) (aget a 2))))
    (case (int arity)
      0 (fn [ds ^objects a] (f ds))
      1 (fn [ds ^objects a] (f ds (aget a 0)))
      2 (fn [ds ^objects a] (f ds (aget a 0) (aget a 1)))
      3 (fn [ds ^objects a] (f ds (aget a 0) (aget a 1) (aget a 2)))
      4 (fn [ds ^objects a] (f ds (aget a 0) (aget a 1) (aget a 2) (aget a 3))))))

(def ^:private ^"[J" sink (long-array 1))

(defn- run-batch
  "Call every argument vector reps times. The results feed a sink, so no call is dead code."
  [call ds ^objects args ^long reps]
  (let [n (alength args)]
    (loop [r 0
           i 0
           acc 0]
      (cond
        (= r reps) (aset sink 0 (unchecked-add (aget sink 0) acc))
        (= i n) (recur (inc r) 0 acc)
        :else (let [x (call ds (aget args i))]
                (recur r (inc i) (if (nil? x) acc (unchecked-add acc (long (.hashCode ^Object x))))))))))

(defn- sample
  "Nanoseconds per call of one batch."
  ^double [{:keys [call ds args reps]}]
  (let [^objects args args
        reps (long reps)
        start (System/nanoTime)]
    (run-batch call ds args reps)
    (/ (double (- (System/nanoTime) start)) (double (* reps (alength args))))))

(defn- warm
  "Run a case until the JIT has had warm-ms of it, then set reps so one sample lasts target-ms."
  [case {:keys [warm-ms target-ms]}]
  (let [deadline (+ (System/nanoTime) (* 1000000 (long warm-ms)))
        one (assoc case :reps 1)]
    (loop [runs 0]
      (sample one)
      (when (or (< runs 2) (< (System/nanoTime) deadline))
        (recur (inc runs))))
    (let [batch-ns (* (double (apply min (repeatedly 3 #(sample one)))) (alength ^objects (:args case)))]
      (assoc case :reps (max 1 (long (/ (* 1000000.0 (long target-ms)) (max 1.0 batch-ns))))))))

(defn- median [xs]
  (let [v (vec (sort xs))
        n (count v)]
    (if (odd? n)
      (double (nth v (quot n 2)))
      (/ (+ (double (nth v (quot n 2))) (double (nth v (dec (quot n 2))))) 2.0))))

(defn- spread
  "The median absolute deviation."
  [xs]
  (let [m (median xs)]
    (median (map #(Math/abs (- (double %) m)) xs))))

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
                       :call (invoker (get (:fns layout) id)
                                      (and (nil? (:fold pattern)) (some? (some #{:scan} (:bind pattern))))
                                      (count (first args)))}))
                  (:patterns plan))}))

(defn- summarize [plan prepared times]
  (let [weights (:weights (:workload plan))
        total (double (reduce + 0 (vals weights)))
        rows (mapv (fn [{:keys [id reps]}]
                     (let [xs (get times [(:hash prepared) id])]
                       {:pattern id
                        :ns (median xs)
                        :spread (spread xs)
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

(defn measure
  "Time normal plans of one task. Returns one summary per plan, in the given order: :cost is the
  weighted mean ns per call plus the memory charge, :noise the weighted spread of the samples.
  opts overrides `defaults`; :seed-offset moves the data and the arguments to a holdout seed."
  [plans opts]
  (let [opts (merge defaults opts)
        first-plan (first plans)]
    (when-not (apply = (map plan/task-key plans))
      (throw (ex-info "The plans do not share one task, so their costs do not compare."
                      {:code :task-mismatch :tasks (mapv plan/task-hash plans)})))
    (let [seed (+ (long (:seed (:workload first-plan))) (long (:seed-offset opts 0)))
          data (gen/data first-plan {:seed seed})
          samples (into {}
                        (map (fn [pattern]
                               [(:id pattern)
                                (gen/arg-samples first-plan pattern data
                                                 {:n (:args opts) :seed (+ seed 101) :miss (:miss opts)})]))
                        (:patterns first-plan))
          prepared (mapv #(prepare % data samples) plans)
          _ (System/gc)
          warmed (mapv (fn [p] (update p :cases (fn [cases] (mapv #(warm % opts) cases)))) prepared)
          times (reduce (fn [acc [p case]]
                          (update acc [(:hash p) (:id case)] (fnil conj []) (sample case)))
                        {}
                        (for [_ (range (:rounds opts))
                              p warmed
                              case (:cases p)]
                          [p case]))]
      (mapv (fn [plan p] (assoc (summarize plan p times)
                                :entries (count (gen/paths data (count (:levels plan))))
                                :seed seed))
            plans warmed))))

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
