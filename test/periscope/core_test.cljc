(ns periscope.core-test
  (:require [clojure.test :refer :all]
            [periscope.core :refer [get update assoc first second nth last all vals in]])
  (:refer-clojure :exclude [get update assoc first second nth last vals]))

(deftest update-scope
  (is (= [2 3 4]
         (update all inc [1 2 3])))
  (is (= {:a 2 :b 3 :c 4}
         (update vals inc {:a 1 :b 2 :c 3})))
  (is (= [{:a 2 :b 3} {:c 4 :d 5}]
         (update (comp all vals) inc
                 [{:a 1 :b 2} {:c 3 :d 4}])))
  (is (= {:a [2 3 4] :b [2 3 4]}
         (update (comp (in [:a]) all) inc
                 {:a [1 2 3] :b [2 3 4]}))))

(deftest get-scope
  (is (= [1 2 3]
         (get all [1 2 3])))
  (is (= '(1 2 3)
         (get vals {:a 1 :b 2 :c 3})))
  (is (= ['(1 2) '(3 4)]
         (get (comp all vals)
              [{:a 1 :b 2} {:c 3 :d 4}])))
  (is (= [1 2 3]
         (get (comp (in [:a]) all)
              {:a [1 2 3] :b [2 3 4]}))))

(deftest nth-test
  (is (= 0 (get first [0 1 2])))
  (is (= 1 (get second [0 1 2])))
  (is (= 2 (get (nth 2) [0 1 2])))
  (is (= [0 2 2]
         (update second inc [0 1 2])))
  (is (= [2 1 2]
         (assoc first 2 [0 1 2])))
  (is (= 2 (get last [0 1 2])))
  (is (= [0 1 0]
         (assoc last 0 [0 1 2]))))
