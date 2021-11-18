(ns periscope.api-test
  (:require [clojure.test :refer :all]
            [periscope.core :as c]
            [periscope.xforms :as x]))

(deftest api-test []
  (testing "Increment every even number nested within map of vector of maps"
    (let [data {:a [{:aa 1 :bb 2}
                    {:cc 3}]
                :b [{:dd 4}]}]
      (is (= {:a [{:aa 1 :bb 3} {:cc 3}] :b [{:dd 5}]}
             (c/update data (comp c/vals c/all c/vals)
                       #(cond-> % (even? %) inc))))))
  (testing "Append a sequence of elements to a nested vector"
    (let [data {:a [1 2 3]}]
      (is (= {:a [1 2 3 4 5]}
             (c/update data (c/in [:a]) into [4 5])))))
  (testing "Increment the last odd number in a sequence"
    (let [data [1 2 3 4 5 6 7 8]]
      (is (= [1 2 3 4 5 6 8 8]
             (c/update data (x/*>> (x/filter odd?) x/last) inc)))))
  (testing "Map a function over a sequence without changing the type or order of the sequence"
    (is (vector? (c/update [1 2 3 4 5] c/all inc)))
    (is (seq? (c/update '(1 2 3 4 5) c/all inc)))
    (is (map? (c/update {:a 1 :b 2} c/all identity))))
  (testing "Increment all the values in maps of maps"
    (is (= {:a {:aa 2}, :b {:ba 0, :bb 3}}
           (c/update {:a {:aa 1} :b {:ba -1 :bb 2}} (comp c/vals c/vals) inc))))
  (testing "Increment all the even values for :a keys in a sequence of maps"
    (is (= [{:a 1} {:a 3} {:a 5} {:a 3}]
           (c/update [{:a 1} {:a 2} {:a 4} {:a 3}]
                     (comp c/all (c/in [:a]))
                     #(cond-> % (even? %) inc)))))
  (testing "Retrieve every number divisible by 3 out of a sequence of sequences"
    (is (= [3 3 18 6 12]
           (c/get [[1 2 3 4] [] [5 3 2 18] [2 4 6] [12]]
                  (comp (x/*>> x/cat (x/filter #(zero? (mod % 3)))))))))
  (testing "Increment the last odd number in a sequence"
    (is (= [2 1 3 6 10 4 8]
           (c/update [2 1 3 6 9 4 8] (x/*>> (x/filter odd?) x/last) inc))))
  (testing "Remove nils from a nested sequence"
    (is (= {:a [1 2 3]}
           (c/update {:a [1 2 nil 3 nil]}
                     (comp (c/in [:a]))
                     #(into (empty %) (remove nil?) %)))))
  (testing "Remove key/value pair from nested map"
    (is (= {:a {:b {}}}
           (c/update {:a {:b {:c 1}}} (c/in [:a :b]) dissoc :c)))))



