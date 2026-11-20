(ns layout-descent.test-runner
  (:require [clojure.test :as t]
            [layout-descent.bench-test]
            [layout-descent.cli-test]
            [layout-descent.facts-test]
            [layout-descent.gate-test]
            [layout-descent.plan-test]))

(defn -main [& _]
  (let [result (t/run-tests 'layout-descent.plan-test
                            'layout-descent.gate-test
                            'layout-descent.bench-test
                            'layout-descent.facts-test
                            'layout-descent.cli-test)]
    (shutdown-agents)
    (System/exit (if (zero? (+ (long (:fail result)) (long (:error result)))) 0 1))))
