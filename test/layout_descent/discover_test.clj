(ns layout-descent.discover-test
  (:require [clojure.test :refer [deftest is]]
            [layout-descent.discover :as discover]
            [layout-descent.flow :as flow]))

(deftest finds-guarded-scan-without-a-plan
  (let [dir "case-studies/rotten/flow-src"
        {:keys [opportunities unparsed]} (discover/analyze dir (flow/source-files dir) nil)
        [scan] opportunities]
    (is (empty? unparsed))
    (is (= ['rotten.core/display-seen] (mapv #(get-in % [:reader :op]) opportunities)))
    (is (= ["world" ":tile/seen?"] ((juxt :collection :field) scan)))
    (is (= #{['rotten.db/mark-tile-as-seen :predicate-write]
             ['rotten.db/create-and-place-tile :record-replacement]}
           (set (map (juxt #(get-in % [:site :op]) :effect) (:possible-writers scan)))))))
