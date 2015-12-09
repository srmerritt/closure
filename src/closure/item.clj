(ns closure.item)

;; Containers are backed by vectors at the moment, so that the order of
;; their contents is consistent when listed.

(defn new-bag
  [& coll]
  (ref (vec coll)))

(defn put-bag
  [bag thing]
  (dosync
    (commute bag conj thing)))

(defn vec-remove
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn take-bag
  [bag pos]
  (let [thing (get bag pos)]
    (cond
      (nil? thing) nil
      :default     (dosync
                     (commute bag vec-remove pos)
                     thing))))
