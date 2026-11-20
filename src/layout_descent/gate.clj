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

(defn- collect [acc & scanned-and-leaf]
  (conj acc (vec scanned-and-leaf)))

(defn- reduce? [pattern]
  (and (nil? (:fold pattern)) (some #{:scan} (:bind pattern))))

(defn- answer
  "What a pattern returns, in a form that compares with =."
  [pattern call]
  (if (reduce? pattern)
    (frequencies (call collect []))
    (let [x (call)]
      [(some-> x class .getSimpleName) x])))

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
                    expected (answer pattern (fn [& extra] (apply model/run plan pattern data args extra)))
                    actual (try
                             (answer pattern (fn [& extra] (apply f ds (concat extra args))))
                             (catch Throwable e [:threw (str (.getSimpleName (class e)) ": " (ex-message e))]))]]
          (when (not= expected actual)
            {:dataset label
             :pattern (:id pattern)
             :args (clip args)
             :expected (clip expected)
             :actual (clip actual)}))]
    {:checks (count results)
     :mismatches (vec (distinct (remove nil? results)))}))

(defn- check-dataset [plan layout {:keys [name data]}]
  (let [samples (into {}
                      (map (fn [pattern]
                             [(:id pattern)
                              (gen/arg-samples plan pattern data {:n 48 :seed 5 :miss 0.4 :boxed-ints true})]))
                      (:patterns plan))]
    (try
      (assoc (check-built plan layout name data ((:build layout) data) samples)
             :entries (count (gen/paths data (count (:levels plan)))))
      (catch Throwable e
        {:checks 1
         :entries 0
         :mismatches [{:dataset name
                       :pattern nil
                       :args "[]"
                       :expected "a dataset"
                       :actual (str "build threw " (.getSimpleName (class e)) ": " (ex-message e))}]}))))

(defn verify-source
  "Check source text against the model of a normal plan. The result has :ok, :checks, :warnings,
  the first :mismatches and the :datasets used. A warning fails the gate."
  [plan text]
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
        {:ok false
         :hash (plan/plan-hash plan)
         :ns (:ns (ex-data e))
         :checks 0
         :warnings [(ex-message e)]
         :mismatch-count 0
         :mismatches []
         :datasets []}
        (throw e)))))

(defn verify
  "Emit a normal plan and check it against the model."
  [plan]
  (verify-source plan (emit/source plan {:ns (eval-ns plan)})))
