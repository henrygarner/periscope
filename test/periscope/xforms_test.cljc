(ns periscope.xforms-test
  (:require [periscope.xforms :refer [*>> map take drop filter last butlast]]
            [periscope.core :refer [get update assoc]]
            [clojure.test :refer :all])
  (:refer-clojure :exclude [get update assoc map take drop filter last butlast]))

(deftest get-periscope
  (is (= '(1 3 5 7 9)
         (get (range 10) (*>> (filter odd?)))))
  (is (= ['(0 2) '(0 2 4)]
         (get [(range 2) (range 3) (range 4) (range 5)]
              (comp (*>> (filter (comp odd? count)))
                    (*>> (filter even?)))))))

(deftest pforms-are-compatible
  (is (= '(1 3 5 7 9)
         (sequence (filter odd?) (range 10))))
  (is (= '(1 2 3 4 5 6 7 8 9 10)
         (sequence (map inc) (range 10)))))

(deftest update-periscope
  (is (= '(0 2 2 4 4 6 6 8 8 10)
         (update (range 10) (*>> (filter odd?)) inc)))
  (is (= ['(0 1) '(1 1 3) '(0 1 2 3) '(1 1 3 3 5)]
         (update [(range 2) (range 3) (range 4) (range 5)]
                 (comp (*>> (filter (comp odd? count)))
                       (*>> (filter even?)))
                 inc))))

(deftest map-periscope
  (is (= [2 3 4 5 6]
         (get [1 2 3 4 5] (*>> (map inc)))))
  (is (= [3 4 5 6 7]
         (update [1 2 3 4 5] (*>> (map inc)) inc)))
  (is (= [1 1 1 1 1]
         (assoc [1 2 3 4 5] (*>> (map inc)) 1))))

(deftest take-test
  (is (= '(0 1 2)
         (get (range 10) (*>> (take 3)))))
  (is (= '(1 2 3 3 4 5 6 7 8 9)
         (update (range 10) (*>> (take 3)) inc)))
  (is (= '(0 0 0 3 4 5 6 7 8 9)
         (assoc (range 10) (*>> (take 3)) 0))))

(deftest drop-test
  (is (= '(3 4 5 6 7 8 9)
         (get (range 10) (*>> (drop 3)))))
  (is (= '(0 1 2 4 5 6 7 8 9 10)
         (update (range 10) (*>> (drop 3)) inc)))
  (is (= '(0 1 2 0 0 0 0 0 0 0)
         (assoc (range 10) (*>> (drop 3)) 0))))

(deftest periscope-composition
  (is (= '(0 42 2 42 4 42 6 7 8 9)
         (assoc (range 10) (*>> (filter odd?) (take 3)) 42)))
  (is (= '(0 42 2 3 4 5 6 7 8 9)
         (assoc (range 10) (*>> (take 3) (filter odd?)) 42)))
  (is (= '(0 1 2 3 4 5 6 7 42 9)
         (assoc (range 10) (*>> (filter even?) last) 42))))

(deftest butlast-test
  (is (= [1 1 1 4]
         (assoc [1 2 3 4] (*>> butlast) 1)))
  (is (= [2 3 4 4]
         (update [1 2 3 4] (*>> butlast) inc)))
  (is (= [1 2 3]
         (get [1 2 3 4] (*>> butlast))))
  (is (= '(0 1 2 0 0 0 0 0 0 9)
         (assoc (range 10) (*>> (drop 3) butlast) 0))))
