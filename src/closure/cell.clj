(ns closure.cell
  (:require [closure.item :as item]))

;; Single cell, i.e. tile, of the game. Modeled as a ref to a set containing
;; entities present in the cell.

(defn new
  [& coll]
  {:entities (ref (set (filter #(#{:prot} (:kind %)) coll)))
   :bag (apply item/new-bag (filter #(#{:item} (:kind %)) coll))})

(defn enter
  [c ent]
  (dosync
    (commute (:entities c) conj ent)))

(defn exit
  [c ent]
  (dosync
    (commute (:entities c) disj ent)))

(defn has?
  [c kind]
  (or (some #(= kind (:kind %)) (deref (:entities c)))
      (some #(= kind (:kind %)) (item/iter-bag (:bag c)))))
