(ns layout-descent.rt-test
  (:require [clojure.test :refer [deftest is testing]]
            [layout-descent.rt :as rt]))

(deftest estimate-bytes-counts-the-dictionaries-and-leaf-values
  (let [n 1000
        data (into {} (map (fn [i] [i (double i)])) (range n))
        build (fn [format]
                (rt/build-dataset {:types [:long] :leaf :double
                                   :orders [{:perm [0] :format format}]}
                                  data))]
    (testing "a CSF level holds a HashMap dict beside ids, leaves and labels"
      ;; ids 4n, double leaves 8n, labels 64n: the estimate must exceed these counted arrays.
      (is (> (long (rt/estimate-bytes (build :csf) 1)) (+ (* 4 n) (* 8 n) (* 64 n)))))
    (testing "a hash leaf entry holds a boxed value beside the Node and the key"
      (is (> (long (rt/estimate-bytes (build :hash) 1)) (+ 48 (* 56 n)))))))
