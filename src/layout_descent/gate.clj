(ns layout-descent.gate
  "The differential gate. An emitted layout passes only when it compiles with no warning and
  answers every pattern as the reference model does."
  (:require [clojure.string :as str]
            [layout-descent.emit :as emit]
            [layout-descent.gen :as gen]
            [layout-descent.model :as model]
            [layout-descent.plan :as plan]
            ;; Loaded here, so that its own compilation is never charged to an emitted source.
            [layout-descent.rt])
  (:import (java.io StringWriter)))

(defn eval-ns
  "The scratch namespace in which a plan is loaded for checks and measurements."
  [plan]
  (symbol (str "layout-descent.eval.p" (plan/plan-hash plan))))

(defn source-ns
  "The namespace that source text declares."
  [text]
  (some-> (re-find #"\(ns\s+([^\s()]+)" text) second symbol))

(defn- load-text [text boxed-math-warnings?]
  (let [err (StringWriter.)]
    (binding [*err* err
              *warn-on-reflection* true
              *unchecked-math* (if boxed-math-warnings? :warn-on-boxed false)]
      (load-string text))
    (remove str/blank? (str/split-lines (str err)))))

(defn load-source
  "Compile source text and return {:ns :build :fns :warnings}. The text is loaded two times: with
  boxed-math warnings on, then as a consumer loads it, so the functions that remain are the ones a
  consumer gets. Throws ex-info with :code :compile-failed."
  [plan text]
  (let [ns-sym (source-ns text)
        warnings (try
                   (vec (distinct (concat (load-text text true) (load-text text false))))
                   (catch Throwable e
                     (throw (ex-info (str "The emitted source does not compile: " (ex-message e))
                                     {:code :compile-failed :ns ns-sym}
                                     e))))
        public #(some-> (ns-resolve (the-ns ns-sym) (symbol %)) deref)]
    {:ns ns-sym
     :hash (plan/plan-hash plan)
     :warnings warnings
     :build (public "build")
     :fns (into {} (map (fn [{:keys [id]}] [id (public (name id))])) (:patterns plan))}))

(defn load-layout
  "Emit a normal plan into its scratch namespace and compile it."
  [plan]
  (load-source plan (emit/source plan {:ns (eval-ns plan)})))

(defn- count-row [acc & scanned-and-leaf]
  (update acc (vec scanned-and-leaf) (fnil inc 0)))

(defn- stop-after-one [call]
  (let [calls (volatile! 0)
        result (call (fn [& _] (vswap! calls inc) (reduced :done)) :empty)]
    [result @calls]))

(defn- reduce? [pattern]
  (and (nil? (:fold pattern)) (some #{:scan} (:bind pattern))))

(defn- answer
  "What a pattern returns, in a form that compares with =."
  [pattern call]
  (if (reduce? pattern)
    [(call count-row {}) (stop-after-one call)]
    (let [x (call)]
      [(some-> x class .getSimpleName) x])))

(defn- threw [e]
  [:threw (str (.getSimpleName (class e)) ": " (ex-message e))])

(defn- clip [x]
  (let [s (pr-str x)]
    (if (> (count s) 240) (str (subs s 0 240) "…") s)))

(defn check-built
  "The disagreements between a built dataset and the model on one dataset. samples maps a pattern
  id to its argument vectors. Returns {:checks n :mismatches [...]}."
  [plan layout label data ds samples]
  (let [results
        (for [pattern (:patterns plan)
              args (get samples (:id pattern))
              :let [f (get (:fns layout) (:id pattern))
                    expected (try
                                (answer pattern (fn [& extra] (apply model/run plan pattern data args extra)))
                                ;; Checked long addition can overflow on valid input.
                                (catch ArithmeticException e (threw e)))
                    actual (try
                             (answer pattern (fn [& extra] (apply f ds (concat extra args))))
                             (catch Throwable e (threw e)))]]
          (when (not= expected actual)
            {:dataset label
             :pattern (:id pattern)
             :args (clip args)
             :expected (clip expected)
             :actual (clip actual)}))]
    {:checks (count results)
     :mismatches (vec (distinct (remove nil? results)))}))

(defn- edge-queries [types bind path]
  (when path
    (let [bound (filterv #(= :key (nth bind %)) (range (count types)))
          args (mapv path bound)]
      (for [[i level] (map-indexed vector bound)
            :when (= :long (nth types level))
            :let [k (nth args i)]
            query [nil (+ 0.5 k) (double k) (bigint k) (biginteger k) (inc' Long/MAX_VALUE)]]
        (assoc args i query)))))

(defn- check-dataset [plan layout {:keys [name data]}]
  (let [types (mapv :type (:levels plan))
        paths (gen/paths data (count types))
        opts {:n 48 :seed 5 :miss 0.4 :boxed-ints true :key-paths paths}
        samples (into {}
                      (map (fn [pattern]
                             [(:id pattern)
                              (into (gen/arg-samples plan pattern data opts)
                                    (edge-queries types (:bind pattern) (first paths)))])
                           (:patterns plan)))]
    (try
      (assoc (check-built plan layout name data ((:build layout) data) samples)
             :entries (count paths))
      (catch Throwable e
        {:checks 1
         :entries 0
         :mismatches [{:dataset name
                       :pattern nil
                       :args "[]"
                       :expected "a dataset"
                       :actual (str "build threw " (.getSimpleName (class e)) ": " (ex-message e))}]}))))

(defn- compile-failure [plan ns-sym message]
  {:ok false
   :hash (plan/plan-hash plan)
   :ns ns-sym
   :checks 0
   :warnings [message]
   :mismatch-count 0
   :mismatches []
   :datasets []})

(defn- check-source [plan text]
  (try
    (let [layout (load-source plan text)
          datasets (gen/edge-datasets plan)
          results (mapv #(check-dataset plan layout %) datasets)
          mismatches (vec (mapcat :mismatches results))]
      {:ok (and (empty? mismatches) (empty? (:warnings layout)))
       :hash (:hash layout)
       :ns (:ns layout)
       :checks (reduce + 0 (map :checks results))
       :warnings (:warnings layout)
       :mismatch-count (count mismatches)
       :mismatches (vec (take 5 mismatches))
       :datasets (mapv (fn [d r] {:name (:name d) :entries (:entries r) :checks (:checks r)}) datasets results)})
    (catch clojure.lang.ExceptionInfo e
      (if (= :compile-failed (:code (ex-data e)))
        (compile-failure plan (:ns (ex-data e)) (ex-message e))
        (throw e)))))

(def ^:private verify-lock (Object.))

(defn verify-source
  "Execute trusted source text and check it against the model of a normal plan. The result has
  :ok, :checks, :warnings, the first :mismatches and the :datasets used. A warning fails the gate.
  The declared namespace must not already be loaded and is removed afterward."
  [plan text]
  (let [ns-sym (source-ns text)]
    (locking verify-lock
      (cond
        (nil? ns-sym) (compile-failure plan nil "Source has no namespace declaration.")
        (find-ns ns-sym) (compile-failure plan ns-sym (str "Namespace " ns-sym " is already loaded."))
        :else (try
                (check-source plan text)
                (finally
                  (remove-ns ns-sym)))))))

(defn verify
  "Compile the delivered source in its declared namespace and check it against the model."
  [plan]
  (verify-source plan (emit/source plan)))
