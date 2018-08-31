(ns ductors.core-test
  (:require [clojure.test :refer :all]
            [ductors.core :refer :all]
            [ductors.ductors :as d]
            [ductors.reducers :as r]))

(deftest ducts-can-be-nested
  (is (= [9 18]
         (deduce (comp (d/map (duct conj))
                       (d/map (duct (map inc) +)))
                 [[1 2 3] [4 5 6]]))))

(deftest ducts-can-be-defined-with-letduct
  (let [d (d/map (letduct [total (duct +)
                           result (duct (map #(/ % total)) conj)]
                   result))]
    (is (= [0 1/3 2/3]
           (deduce d [0 1 2])))))

(deftest it-transforms-data-structures
  (testing "incrementing vals in map of maps"
    (let [d (comp (d/map-vals (duct r/assoc))
                  (d/map-vals (duct r/assoc)))]
      (is (= {:a {:aa 2}, :b {:ba 0, :bb 3}}
             (deduce d inc {:a {:aa 1} :b {:ba -1 :bb 2}})))))
  
  (testing "increments all even vals for :a in sequence of maps"
    (let [d (d/map (duct conj))]
      (is (= [{:a 1} {:a 3} {:a 5} {:a 3}]
             (deduce d (fn [x] (update x :a #(cond-> % (even? %) inc)))
                     [{:a 1} {:a 2} {:a 4} {:a 3}])))))

  (testing "retrieve every number divisible by 3 out of a sequence of sequences"
    (let [d (comp (d/map (duct r/concat))
                  (d/map (duct (filter #(zero? (mod % 3))) conj)))]
      (is (= [3 3 18 6 12]
             (deduce d [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]])))))

  (testing "remove nils from a nested sequence"
    (let [d (comp (d/map-vals (duct r/assoc))
                  (d/map (duct (remove nil?) conj)))]
      (is (= {:a [1 2 3]}
             (deduce d {:a [1 2 nil 3 nil]})))))

  (testing "transducers can filter values before reduction"
    (let [d (comp (d/map (remove keyword?) (duct conj))
                  (d/map (duct +)))]
      (is (= [6 15]
             (deduce d [:a [1 2 3] :b [4 5 6]])))))

  (testing "append [:c :d] to every subsequence that has at least two even numbers"
    (let [d (comp (d/map (duct conj))
                  (d/when #(>= (count (filter even? %)) 2)))]
      (is (= [[1 2 3 4 5 6 :c :d] [7 0 -1] [8 8 :c :d] []]
             (deduce d #(concat % [:c :d]) [[1 2 3 4 5 6] [7 0 -1] [8 8] []]))))))

(deftest assemblies-can-target-their-execution
  (testing "increment only numbers in even length arrays"
    (let [d (comp (d/map (duct conj))
                  (d/when #(even? (count %)))
                  (d/map (duct conj))
                  (d/when number?))]
      (is (= [[1 false 2] [4 5] [6 nil] [7 8 9 10]]
             (deduce d inc [[1 false 2] [3 4] [5 nil] [6 7 8 9]])))))

  (testing "predicates only affect whether bottom is applied"
    (let [d (comp (d/map (duct concat))
                  (d/when #(contains? % :a))
                  (d/vals (duct concat))
                  (d/map (duct conj)))]
      (is (= [2 3 4 5 6 7 8 9 10 11 12 13]
             (deduce d inc [{:a [1 2 3] :b [4 5 6]} {:c [8 9 10] :d [11 12 13]}]))))))

