(ns periscope.benchmarks
  (:require [criterium.core :as bench]
            [periscope.core :as c]
            [periscope.xforms :as x]
            [com.rpl.specter :as s]
            [com.rpl.specter.transients]
            [com.rpl.specter.impl :as i]))

(defn pretty-float3 [anum]
  (format "%.3g" anum))

(defn mean [a-fn]
  (-> a-fn (bench/benchmark* {}) :mean first (* 1000000)))

(defn compare-benchmark [afn-map]
  (let [results (s/transform s/MAP-VALS mean afn-map)
        [[_ best-time] & _ :as sorted] (sort-by last results)]
    (println "\nMean(us)\tvs best\t\tCode")
    (doseq [[k t] sorted]
      (println (pretty-float3 t) "\t\t" (pretty-float3 (/ t best-time 1.0)) "\t\t" k))))

(defmacro run-benchmark [name & exprs]
  (let [only-benchmarks (set (filter some? *command-line-args*))
        all-benchmarks? (empty? only-benchmarks)]
    (if (or all-benchmarks? (contains? only-benchmarks name))
      (let [afn-map (->> exprs shuffle (map (fn [e] [`(quote ~e) `(fn [] ~e)])) (into {}))]
        `(do
           (println "Benchmark:" ~name)
           (compare-benchmark ~afn-map)
           (println "\n********************************\n"))))))

(let [data {:a {:b {:c 1}}}
      p (s/comp-paths :a :b :c)]
  (run-benchmark "get value in nested map"
                 (get-in data [:a :b :c])
                 (s/select-first [:a :b :c] data)
                 (s/compiled-select-any p data)
                 (c/get data (c/in [:a :b :c]))))

(let [data {:a {:b {:c 1}}}]
  (run-benchmark "set value in nested map"
                 (assoc-in data [:a :b :c] 1)
                 (s/setval [:a :b :c] 1 data)
                 (c/assoc data (c/in [:a :b :c]) 1)))
