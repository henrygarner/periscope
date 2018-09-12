(ns ductors.protocols)

(defprotocol IdentityRf
  (identity-rf [this] "Returns a reducing function for use with"))

(defn post-complete
  [rf r]
  (completing rf (comp r rf)))

(extend-protocol IdentityRf
  clojure.lang.PersistentVector
  (identity-rf [this] conj)
  clojure.lang.PersistentList
  (identity-rf [this] (post-complete conj #(apply list %)))
  clojure.lang.PersistentHashMap
  (identity-rf [this] (completing
                       (fn
                         ([] (empty this))
                         ([acc [k v]] (assoc acc k v)))))
  clojure.lang.PersistentArrayMap
  (identity-rf [this] (completing
                       (fn
                         ([] (empty this))
                         ([acc [k v]] (assoc acc k v))))))

(defn f
  [xform rf coll]
  (transduce xform (rf coll) coll))

(f (map inc) identity-rf [1 2 3 4])

(f (map inc) identity-rf '(1 2 3 4))

(f (map #(update % 1 inc)) identity-rf {:a 1 :b 2})
