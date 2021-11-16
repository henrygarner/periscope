(ns periscope.core-test
  (:require [clojure.test :refer :all]
            [clojure.core :as core]
            [periscope.core :refer [get update assoc first second nth last all vals in rest]])
  (:refer-clojure :exclude [get update assoc first second nth last vals rest]))

(deftest update-scope
  (is (= [2 3 4]
         (update [1 2 3] all inc)))
  (is (= {:a 2 :b 3 :c 4}
         (update {:a 1 :b 2 :c 3} vals inc)))
  (is (= [{:a 2 :b 3} {:c 4 :d 5}]
         (update [{:a 1 :b 2} {:c 3 :d 4}] (comp all vals) inc)))
  (is (= {:a [2 3 4] :b [2 3 4]}
         (update {:a [1 2 3] :b [2 3 4]} (comp (in [:a]) all) inc))))

(deftest get-scope
  (is (= [1 2 3]
         (get [1 2 3] all)))
  (is (= '(1 2 3)
         (get {:a 1 :b 2 :c 3} vals)))
  (is (= ['(1 2) '(3 4)]
         (get [{:a 1 :b 2} {:c 3 :d 4}] (comp all vals))))
  (is (= [1 2 3]
         (get {:a [1 2 3] :b [2 3 4]} (comp (in [:a]) all)))))

(deftest nth-test
  (is (= 0 (get [0 1 2] first)))
  (is (= 1 (get [0 1 2] second)))
  (is (= 2 (get [0 1 2] (nth 2))))
  (is (= [0 2 2]
         (update [0 1 2] second inc)))
  (is (= [2 1 2]
         (assoc [0 1 2] first 2)))
  (is (= 2 (get [0 1 2] last)))
  (is (= [0 1 0]
         (assoc [0 1 2] last 0))))

(deftest graph-spec-test
  (let [spec [[:grid nil {:position [0 2]}]
              [:line [[0 1] [2 3]] {:stroke {:size 1}}]
              [:ci [[0 1] [2 3]] {:colour "blue"}]
              [:ci [[0 1] [2 3]] {:colour "blue"}]]]
    (is (= [[:grid nil {:position [0 2]}]
            [:line [[0 1] [2 3]] {:stroke {:size 1} :position [0 1]}]
            [:ci [[0 1] [2 3]] {:colour "blue" :position [0 1]}]
            [:ci [[0 1] [2 3]] {:colour "blue" :position [0 1]}]]
           (update spec (comp rest last) core/assoc :position [0 1])))))
