(ns periscope.core-test
  (:require [clojure.test :refer :all]
            [periscope.core :as sut]))

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

(deftest nth-test
  (is (= 0 (sut/get sut/first [0 1 2])))
  (is (= 1 (sut/get sut/second [0 1 2])))
  (is (= 2 (sut/get (sut/nth 2) [0 1 2])))
  (is (= [0 2 2] (sut/update sut/second inc [0 1 2])))
  (is (= [2 1 2] (sut/assoc sut/first 2 [0 1 2])))
  (is (= 2 (sut/get sut/last [0 1 2])))
  (is (= [0 1 0] (sut/assoc sut/last 0 [0 1 2]))))
