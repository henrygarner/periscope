(ns duxi.core)

(defn ducts
  "Chains a duct with a sequence of duct generators"
  [duct & ducts]
  (let [rf' (reduce
             (fn [a b]
               (fn [acc]
                 (let [duct (b acc)]
                   (update duct :rf #(completing %
                                                 (fn [acc]
                                                   (a (% acc))))))))
             (reverse ducts))]
    (update duct :rf (fn [rf] (completing rf (fn [acc] (rf' (rf acc))))))))

(defrecord Conduct [xform rf])

(defn unconduced? [x]
  (instance? Conduct x))

(def conduced? (complement unconduced?))

(defn conduce
  [{:keys [xform rf]} data]
  (let [xform (or xform identity)]
    (if rf
      (let [res (transduce xform rf data)]
        (if (conduced? res)
          res
          (recur res data)))
      (sequence xform data))))
