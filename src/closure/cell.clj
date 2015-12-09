(ns closure.cell)

;; Single cell, i.e. tile, of the game. Modeled as a ref to a set containing
;; entities present in the cell.

(defn new
  [& coll]
  (ref (set coll)))

(defn enter
  [c ent]
  (dosync
    (commute c conj ent)))

(defn exit
  [c ent]
  (dosync
    (commute c disj ent)))

(defn has?
  [c kind]
  (some #(= kind (:kind %)) @c))
