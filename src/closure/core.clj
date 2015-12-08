(ns closure.core
  (:require [closure.cell :as cell]
            [closure.ui :as ui]))

;; Gross: indexing into a 2d array yields [y, x] ordering
(def directions {:left [0 -1],
                 :right [0 1],
                 :up [-1 0],
                 :down [1 0]})

(defn inbounds?
  [grid  [x y]]
  (not (contains? #{nil :oob} (get-in grid [x y]))))

;; Main overworld
(def mploc [1 1])
(def mprot {:kind :prot})
(def mgrid [[:oob (cell/new) (cell/new) (cell/new) :oob]
            [(cell/new) (cell/new mprot) (cell/new) (cell/new) (cell/new)]
            [:oob (cell/new) (cell/new) (cell/new) :oob]])

;; Returns the new loc
(defn move
  [dir grid loc ent]
  (let [newloc (mapv + (dir directions) loc)
        oldcell (get-in grid loc)
        newcell (get-in grid newloc)]
    (cond
      (inbounds? grid newloc) (do
                                (cell/exit oldcell ent)
                                (cell/enter newcell ent)
                                newloc)
      :default                loc)))

(defn main
  [screen-type]
  (closure.ui/init screen-type)
  (loop [[grid ploc :as state] [mgrid mploc]]
    (closure.ui/redraw [[grid [0 1]]])
    (let [c (closure.ui/getch)]
      (cond
        (= c :escape)            (closure.ui/quit)
        (contains? directions c) (recur [grid (move c grid ploc mprot)])
        :else                    (recur state)))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
