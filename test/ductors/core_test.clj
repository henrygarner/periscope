(ns ductors.core-test
  (:require [clojure.test :refer :all]
            [ductors.core :as sut]
            [ductors.reducers :as r]))

(deftest update-lens
  (is (= [2 3 4]
         (sut/update sut/all inc [1 2 3])))
  (is (= {:a 2 :b 3 :c 4}
         (sut/update sut/vals inc {:a 1 :b 2 :c 3})))
  (is (= [{:a 2 :b 3} {:c 4 :d 5}]
         (sut/update (comp sut/all sut/vals) inc
                   [{:a 1 :b 2} {:c 3 :d 4}])))
  (is (= {:a [2 3 4] :b [2 3 4]}
         (sut/update (comp (sut/in [:a]) sut/all) inc
                   {:a [1 2 3] :b [2 3 4]}))))

(deftest get-lens
  (is (= [1 2 3]
         (sut/get sut/all [1 2 3])))
  (is (= '(1 2 3)
         (sut/get sut/vals {:a 1 :b 2 :c 3})))
  (is (= ['(1 2) '(3 4)]
         (sut/get (comp sut/all sut/vals)
                  [{:a 1 :b 2} {:c 3 :d 4}])))
  (is (= [1 2 3]
         (sut/get (comp (sut/in [:a]) sut/all)
                  {:a [1 2 3] :b [2 3 4]}))))

(deftest get-duct
  (is (= '(1 3 5 7 9)
         (sut/get (sut/duct (sut/filter odd?)) (range 10))))
  (is (= ['(0 2) '(0 2 4)]
         (sut/get (comp (sut/duct (sut/filter (comp odd? count)))
                        (sut/duct (sut/filter even?)))
                  [(range 2) (range 3) (range 4) (range 5)]))))

(deftest lens-xforms-are-compatible
  (is (= '(1 3 5 7 9)
         (sequence (sut/filter odd?) (range 10))))
  (is (= '(1 2 3 4 5 6 7 8 9 10)
         (sequence (sut/map inc) (range 10)))))

(deftest update-duct
  (is (= '(0 2 2 4 4 6 6 8 8 10)
         (sut/update (sut/duct (sut/filter odd?)) inc (range 10))))
  (is (= ['(0 1) '(1 1 3) '(0 1 2 3) '(1 1 3 3 5)]
         (sut/update (comp (sut/duct (sut/filter (comp odd? count)))
                           (sut/duct (sut/filter even?)))
                     inc
                     [(range 2) (range 3) (range 4) (range 5)]))))

(deftest map-duct
  (is (= [2 3 4 5 6]
         (sut/get (sut/duct (sut/map inc)) [1 2 3 4 5])))
  (is (= [3 4 5 6 7]
         (sut/update (sut/duct (sut/map inc)) inc [1 2 3 4 5])))
  (is (= [1 1 1 1 1]
         (sut/assoc (sut/duct (sut/map inc)) 1 [1 2 3 4 5]))))

(deftest nth
  (is (= 0 (sut/get sut/first [0 1 2])))
  (is (= 1 (sut/get sut/second [0 1 2])))
  (is (= 2 (sut/get (sut/nth 2) [0 1 2])))
  (is (= [0 2 2] (sut/update sut/second inc [0 1 2])))
  (is (= [2 1 2] (sut/assoc sut/first 2 [0 1 2])))
  (is (= 2 (sut/get sut/last [0 1 2])))
  (is (= [0 1 0] (sut/assoc sut/last 0 [0 1 2]))))

(deftest take
  (is (= '(0 1 2) (sut/get (sut/duct (sut/take 3)) (range 10))))
  (is (= '(1 2 3 3 4 5 6 7 8 9) (sut/update (sut/duct (sut/take 3)) inc (range 10))))
  (is (= '(0 0 0 3 4 5 6 7 8 9) (sut/assoc (sut/duct (sut/take 3)) 0 (range 10)))))

(deftest duct-composition
  (is (= '(0 42 2 42 4 42 6 7 8 9) (sut/assoc (sut/duct (comp (sut/filter odd?) (sut/take 3))) 42 (range 10))))
  (is (= '(0 42 2 3 4 5 6 7 8 9) (sut/assoc (sut/duct (comp (sut/take 3) (sut/filter odd?))) 42 (range 10)))))
