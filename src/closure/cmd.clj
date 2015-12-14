(ns closure.cmd
  (:require [closure.cell :as cell]
            [closure.item :as item]
            [closure.ui :as ui]))

;; ------- Overworld / movement
;; Gross: indexing into a 2d array yields [y, x] ordering
(def directions {:left [0 -1],
                 :right [0 1],
                 :up [-1 0],
                 :down [1 0]})

(defn inbounds?
  [grid  [x y]]
  (not (contains? #{nil :oob} (get-in grid [x y]))))

(defn current-cell
  [state]
  (get-in (:grid state) (:pos state)))

(defn move
  [state dir]
  (let [grid (:grid state)
        pos (:pos state)
        prot (:prot state)
        newpos (mapv + dir pos)
        oldcell (get-in grid pos)
        newcell (get-in grid newpos)]
    (cond
      (inbounds? grid newpos) (do
                                (cell/exit oldcell prot)
                                (cell/enter newcell prot)
                                (assoc state :pos newpos))
      :default                state)))

(defn left [state] (move state (:left directions)))
(defn right [state] (move state (:right directions)))
(defn up [state] (move state (:up directions)))
(defn down [state] (move state (:down directions)))

;; ------- Overworld / items
(defn inventory
  [state]
  (doseq [itm (item/iter-bag (:bag (:prot state)))]
    (ui/put-text (:name itm)))
  state)

(defn get-room
  [state]
  (let [cell (current-cell state)
        itm (item/take-bag (:bag cell) 0)]
    (item/put-bag (:bag (:prot state)) itm))
  state)
