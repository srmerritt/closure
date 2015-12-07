(ns closure.core
  (:require [lanterna.screen :as s]
            [clojure.tools.logging :as log]
            [closure.cell :as cell]))

(def sigil {:oob   " ",
            :empty ".",
            :prot  "@"})

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

(defmulti cell-status identity)
(defmethod cell-status :oob [_]
  :oob)
(defmethod cell-status :default [c]
  (cond
    (cell/has? c :prot) :prot
    :default            :empty))

(defn draw
  [grid screen off]
  (let [rows (map vector (iterate inc 0) grid)
        [x y] off]
    (doseq [[offset row] rows]
      (s/put-string screen
                    x (+ offset y)
                    (apply str (map (comp sigil cell-status) row))))))

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

(defn getch [screen]
  (let [c (s/get-key-blocking screen)]
    (log/info "got char " c)
    c))

(defn quit
  [screen]
  (s/put-string screen 0 2 "BYE")
  (s/redraw screen)
  (Thread/sleep 1000))

(defn main
  [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (loop [[grid ploc :as state] [mgrid mploc]]
                   (draw grid screen [0 0])
                   (s/redraw screen)
                   (let [c (getch screen)]
                     (cond
                       (= c :escape)            (quit screen)
                       (contains? directions c) (recur [grid (move c grid ploc mprot)])
                       :else                    (recur state)))))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
