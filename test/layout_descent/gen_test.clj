(ns layout-descent.gen-test
  (:require [clojure.test :refer [deftest is testing]]
            [layout-descent.fixtures :as fx]
            [layout-descent.gen :as gen])
  (:import (java.util Random)))

(def base (fx/normal fx/small-risk))

(defn- with-degree [degree]
  (fx/normal (assoc-in fx/small-risk [:workload :degree] degree)))

(defn- maps-at
  "Every map that is depth levels below the root of data."
  [data depth]
  (if (zero? depth) [data] (mapcat #(maps-at % (dec depth)) (vals data))))

(defn- fill-only
  "The generator as it was before :degree: one nextDouble for each key of the fanout, in key order,
  and one nextInt for each leaf."
  [plan]
  (let [types (mapv :type (:levels plan))
        {:keys [fanout fill seed]} (:workload plan)
        r (Random. (long seed))]
    ((fn node [depth]
       (if (= depth (count types))
         (* 0.25 (.nextInt r 4000))
         (reduce (fn [m i]
                   (if (< (.nextDouble r) (double (nth fill depth)))
                     (assoc m (gen/level-key (nth types depth) i) (node (inc depth)))
                     m))
                 {}
                 (range (nth fanout depth)))))
     0)))

(deftest data-without-degree-is-unchanged
  (let [data (gen/data base)
        leaves (map #(get-in data %) (gen/paths data 3))]
    (is (= (fill-only base) data))
    (testing "measured before :degree existed"
      (is (= 1024 (count leaves)))
      (is (= 518776.25 (reduce + leaves))))
    (testing "an all-nil degree draws the same numbers"
      (is (= data (gen/data base {:degree [nil nil nil]}))))))

(deftest degree-gives-every-parent-exactly-that-many-keys
  (let [plan (with-degree [nil 12 3])
        data (gen/data plan)]
    (is (= 4 (count data)) "fill 1.0 still decides the first level")
    (is (= #{12} (set (map count (maps-at data 1)))))
    (is (= #{3} (set (map count (maps-at data 2)))))
    (is (= (* 4 12 3) (count (gen/paths data 3))))
    (testing "keys are keys of the level, inside its fanout"
      (is (every? (set (map #(gen/level-key :long %) (range 60))) (mapcat keys (maps-at data 1))))
      (is (every? (set (map #(gen/level-key :keyword %) (range 8))) (mapcat keys (maps-at data 2)))))
    (testing "parents do not all draw the same keys"
      (is (< 1 (count (set (map (comp set keys) (maps-at data 1)))))))
    (testing "a level without a degree keeps its fill"
      (let [mixed (gen/data (with-degree [nil 12 nil]))]
        (is (= #{12} (set (map count (maps-at mixed 1)))))
        (is (< 1 (count (set (map count (maps-at mixed 2))))))))))

(deftest degree-at-the-bounds
  (testing "a degree equal to the fanout holds every key"
    (let [data (gen/data (with-degree [4 60 8]))]
      (is (= (* 4 60 8) (count (gen/paths data 3))))))
  (testing "a degree of one"
    (let [data (gen/data (with-degree [1 1 1]))]
      (is (= 1 (count (gen/paths data 3))))))
  (testing "a fanout that opts make smaller caps the degree"
    (let [data (gen/data (with-degree [nil 12 3]) {:fanout [4 5 2]})]
      (is (= #{5} (set (map count (maps-at data 1)))))
      (is (= #{2} (set (map count (maps-at data 2))))))))

(deftest degree-data-is-deterministic
  (let [plan (with-degree [nil 12 3])]
    (is (= (gen/data plan) (gen/data plan)))
    (is (= (gen/data plan {:seed 8}) (gen/data plan {:seed 8})))
    (is (not= (gen/data plan) (gen/data plan {:seed 8})))
    (is (not= (gen/data plan) (gen/data base)))))

(deftest edge-datasets-are-valid-with-a-degree
  (let [plan (with-degree [4 60 8])
        datasets (gen/edge-datasets plan)]
    (is (= ["empty" "single" "empty-inner" "sparse" "dense"] (mapv :name datasets)))
    (testing "the degree does not reach the edge datasets"
      (is (= (gen/edge-datasets base) datasets)))
    (testing "the sparse sample stays sparse and inside its small fanout"
      (let [sparse (:data (nth datasets 3))]
        (is (< 0 (count (gen/paths sparse 3)) (* 4 6 6)))
        (is (every? #(<= (count %) 6) (maps-at sparse 1)))))
    (testing "argument samples work on degree data"
      (let [data (gen/data (with-degree [nil 12 3]))
            pattern (first (:patterns plan))]
        (is (= 64 (count (gen/arg-samples (with-degree [nil 12 3]) pattern data {}))))))))
