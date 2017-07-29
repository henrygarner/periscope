(ns duxi.core-test
  (:require [clojure.test :refer :all]
            [duxi.core :refer :all]
            [kixi.stats.core :as kixi]
            [redux.core :as r]))

(def minmax-rf
  (let [min' (fn [a b] (if (nil? a) b (min a b)))
        max' (fn [a b] (if (nil? a) b (max a b)))]
    (fn
      ([] [])
      ([[mn mx] x] [(min' mn x) (max' mx x)])
      ([[mn mx]] [mn mx]))))

(defn bin [n]
  (ducts
   (map->Conduct {:rf minmax-rf})
   (fn [[mn mx]]
     (map->Conduct
      {:xform (let [var (- mx mn)]
                (map (fn [x]
                       (if (= x mx)
                         (dec n)
                         (int (*  n (/ (- x mn) var)))))))}))))

(def normalize
  (ducts
   (map->Conduct {:rf (r/fuse {:mean kixi/mean :sd kixi/standard-deviation})})
   (fn [{:keys [mean sd]}]
     (map->Conduct
      {:xform (map (fn [x]
                     (cond-> (- x mean)
                       (pos? sd)
                       (/ sd))))}))))


(deftest conduce-test []
  (is (= (conduce (bin 2) [1 4 3 2 5 6])
         '(0 1 0 0 1 1)))
  
  (is (= (conduce normalize [0 5 10])
         [-1.0 0.0 1.0])))

