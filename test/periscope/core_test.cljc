(ns periscope.core-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.core :as core]
            #?(:clj [periscope.core :refer [get update assoc first second nth last all vals rest drop take butlast in]])
            #?(:cljs [periscope.core :refer [get update assoc first second nth last all vals rest drop take butlast]
                      :refer-macros [in]]))
  (:refer-clojure :exclude [get update assoc first second nth last vals rest drop take butlast]))

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

(deftest drop-test
  (is (= '(2 3 4) (get (range 5) (drop 2))))
  (is (= '(0 1 3 4 5) (update (range 5) (drop 2) inc)))
  (is (= '(0 1 0 0 0) (assoc  (range 5) (drop 2) 0))))

(deftest take-test
  (is (= '(0 1) (get (range 5) (take 2))))
  (is (= '(1 2 2 3 4) (update (range 5) (take 2) inc)))
  (is (= '(0 0 2 3 4) (assoc  (range 5) (take 2) 0))))

(deftest butlast-test
  (is (= '(0 1 2 3) (get (range 5) butlast)))
  (is (= '(1 2 3 4 4) (update (range 5) butlast inc)))
  (is (= '(0 0 0 0 4) (assoc  (range 5) butlast 0))))

(deftest last-test
  (is (= 4 (get (range 5) last)))
  (is (= '(0 1 2 3 5) (update (range 5) last inc)))
  (is (= '(0 1 2 3 0) (assoc  (range 5) last 0))))

(deftest rest-test
  (is (= '(1 2 3 4) (get (range 5) rest)))
  (is (= '(0 2 3 4 5) (update (range 5) rest inc)))
  (is (= '(0 1 1 1 1) (assoc  (range 5) rest 1))))

(deftest all-test
  (is (= '(0 1 2 3 4) (get (range 5) all)))
  (is (= '(1 2 3 4 5) (update (range 5) all inc)))
  (is (= '(0 0 0 0 0) (assoc  (range 5) all 0))))

(deftest vals-test
  (is (= '(1 2 3) (get {:a 1 :b 2 :c 3} vals)))
  (is (= {:a 2 :b 3 :c 4} (update {:a 1 :b 2 :c 3} vals inc)))
  (is (= {:a 0 :b 0 :c 0} (assoc {:a 1 :b 2 :c 3} vals 0))))

(deftest in-test
  (is (= 1 (get {:a {:b {:c 1}}} (in [:a :b :c]))))
  (is (= {:a {:b {:c 2}}} (update {:a {:b {:c 1}}} (in [:a :b :c]) inc)))
  (is (= {:a {:b {:c 0}}} (assoc {:a {:b {:c 1}}} (in [:a :b :c]) 0))))
