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

 (let [data {:a {:b {:c 1}}}]
   (run-benchmark "update value in nested map"
                  (update-in data [:a :b :c] inc)
                  (s/transform [:a :b :c] inc data)
                  (c/update data (c/in [:a :b :c]) inc)))

 (let [data '(1 2 3 4 5)]
   (run-benchmark "transform values of a list"
                  (s/transform s/ALL inc data)
                  (doall (sequence (map inc) data))
                  (reverse (into '() (map inc) data))
                  (c/update data c/all inc)))

(let [data {:a 1 :b 2 :c 3 :d 4}]
  (run-benchmark "transform values of a small map"
                 (into {} (for [[k v] data] [k (inc v)]))
                 (reduce-kv (fn [m k v] (assoc m k (inc v))) {} data)
                 (persistent! (reduce-kv (fn [m k v] (assoc! m k (inc v))) (transient {}) data))
                 (reduce-kv (fn [m k v] (assoc m k (inc v))) (empty data) data)
                 (s/transform [s/ALL s/LAST] inc data)
                 (s/transform s/MAP-VALS inc data)
                 (zipmap (keys data) (map inc (vals data)))
                 (into {} (map (fn [e] [(key e) (inc (val e))]) data))
                 (into {} (map (fn [e] [(key e) (inc (val e))])) data)
                 (c/update data c/vals inc)))

(let [data (->> (for [i (range 1000)] [i i]) (into {}))]
  (run-benchmark "transform values of large map"
                 (into {} (for [[k v] data] [k (inc v)]))
                 (reduce-kv (fn [m k v] (assoc m k (inc v))) {} data)
                 (persistent! (reduce-kv (fn [m k v] (assoc! m k (inc v))) (transient {}) data))
                 (persistent! (reduce-kv (fn [m k v] (assoc! m k (inc v))) (transient clojure.lang.PersistentHashMap/EMPTY) data))
                 (reduce-kv (fn [m k v] (assoc m k (inc v))) (empty data) data)
                 (s/transform [s/ALL s/LAST] inc data)
                 (s/transform s/MAP-VALS inc data)
                 (zipmap (keys data) (map inc (vals data)))
                 (into {} (map (fn [e] [(key e) (inc (val e))]) data))
                 (into {} (map (fn [e] [(key e) (inc (val e))])) data)
                 (c/update data c/vals inc)))
