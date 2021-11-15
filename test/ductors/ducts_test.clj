(ns ductors.ducts-test
  (:require [ductors.ducts :as sut]
            [ductors.core :refer [get update assoc]]
            [clojure.test :refer :all]))

(deftest get-duct
  (is (= '(1 3 5 7 9)
         (get (sut/duct (sut/filter odd?)) (range 10))))
  (is (= ['(0 2) '(0 2 4)]
         (get (comp (sut/duct (sut/filter (comp odd? count)))
                        (sut/duct (sut/filter even?)))
              [(range 2) (range 3) (range 4) (range 5)]))))

(deftest lens-xforms-are-compatible
  (is (= '(1 3 5 7 9)
         (sequence (sut/filter odd?) (range 10))))
  (is (= '(1 2 3 4 5 6 7 8 9 10)
         (sequence (sut/map inc) (range 10)))))

(deftest update-duct
  (is (= '(0 2 2 4 4 6 6 8 8 10)
         (update (sut/duct (sut/filter odd?)) inc (range 10))))
  (is (= ['(0 1) '(1 1 3) '(0 1 2 3) '(1 1 3 3 5)]
         (update (comp (sut/duct (sut/filter (comp odd? count)))
                       (sut/duct (sut/filter even?)))
                 inc
                 [(range 2) (range 3) (range 4) (range 5)]))))

(deftest map-duct
  (is (= [2 3 4 5 6]
         (get (sut/duct (sut/map inc)) [1 2 3 4 5])))
  (is (= [3 4 5 6 7]
         (update (sut/duct (sut/map inc)) inc [1 2 3 4 5])))
  (is (= [1 1 1 1 1]
         (assoc (sut/duct (sut/map inc)) 1 [1 2 3 4 5]))))

(deftest take
  (is (= '(0 1 2) (get (sut/duct (sut/take 3)) (range 10))))
  (is (= '(1 2 3 3 4 5 6 7 8 9) (update (sut/duct (sut/take 3)) inc (range 10))))
  (is (= '(0 0 0 3 4 5 6 7 8 9) (assoc (sut/duct (sut/take 3)) 0 (range 10)))))

(deftest duct-composition
  (is (= '(0 42 2 42 4 42 6 7 8 9) (assoc (sut/duct (comp (sut/filter odd?) (sut/take 3))) 42 (range 10))))
  (is (= '(0 42 2 3 4 5 6 7 8 9) (assoc (sut/duct (comp (sut/take 3) (sut/filter odd?))) 42 (range 10))))
  (is (= '(0 1 2 3 4 5 6 7 42 9) (assoc (sut/duct (comp (sut/filter even?) sut/last)) 42 (range 10)))))
