(ns layout-descent.test-runner
  "Runs the test namespaces of test/layout_descent: all of them, or the ones named as arguments
  (`clojure -M:cli:test facts plan` runs facts-test and plan-test)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t]))

(defn- all-names []
  (->> (.listFiles (io/file "test/layout_descent"))
       (map #(.getName ^java.io.File %))
       (filter #(str/ends-with? % "_test.clj"))
       (map #(str/replace % #"_test\.clj$" ""))
       sort))

(defn -main [& names]
  (let [namespaces (mapv #(symbol (str "layout-descent." (str/replace % "_" "-") "-test"))
                         (or (seq names) (all-names)))
        _ (apply require namespaces)
        result (apply t/run-tests namespaces)]
    (shutdown-agents)
    (System/exit (if (zero? (+ (long (:fail result)) (long (:error result)))) 0 1))))
