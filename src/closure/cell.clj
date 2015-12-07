(ns closure.cell)

;; Single cell, i.e. tile, of the game. Modeled as a ref to a set containing
;; entities present in the cell.

(defn +
  []
  (ref #{}))

(defn enter
  [c ent]
  (dosync
          (commute c conj ent)))

(defn exit
  [c ent]
  (dosync
          (commute c disj ent)))

;; Won't be consistent (wrt concurrency) in current version
(defn status
  [c]
  (cond
      :default                   :empty))

