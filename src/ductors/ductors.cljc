(ns ductors.ductors
  (:require [ductors.xforms :as xf])
  (:refer-clojure :exclude [filter map keys vals when]))

(defn nest
  [xf]
  (fn nest*
    ([duct]
     (nest* identity duct))
    ([xform' duct]
     (fn [f]
       (fn [state coll]
         (duct (fn [xform rf] (transduce (comp xform' (xf (partial f state)) xform) rf coll))))))))

(defn when
  [pred?]
  (fn [f]
    (fn [state coll]
      (f (and state (pred? coll)) coll))))

(defn link
  [duct]
  (fn [f]
    (fn [state coll]
      (f state (duct (fn [xform rf] (transduce xform rf coll)))))))

(defn make-ductor
  [duct]
  (fn [f]
    (fn [state coll]
      (f state (duct (fn [xform rf] (transduce xform rf coll)))))))

(def map (nest xf/map))

(def map-vals (nest xf/map-vals))

(def map-keys (nest xf/map-keys))

(def keys (nest xf/keys))

(def vals (nest xf/vals))

(def filter (nest clojure.core/filter))
