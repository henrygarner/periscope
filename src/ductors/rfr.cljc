(ns duxi.rfr)

(defn >seq
  "Return a sequence like the one being reduced"
  [coll]
  (fn
    ([] (empty coll))
    ([acc x]
     (conj acc x))
    ([acc]
     (if (list? acc)
       (reverse acc)
       acc))))
