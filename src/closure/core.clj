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

(defn protagonist?
  [p]
  false)

;; Main overworld
(def mploc [1 1])
(def mgrid [[:oob (cell/+) (cell/+) (cell/+) :oob]
            [(cell/+) (cell/+) (cell/+) (cell/+) (cell/+)]
            [:oob (cell/+) (cell/+) (cell/+) :oob]])

(defmulti cell-status identity)
(defmethod cell-status :oob [_]
  :oob)
(defmethod cell-status :default [c]
  (cell/status c))

(defn draw
  [grid screen [px py] off]
  (let [rows (map vector (iterate inc 0) grid)
        [x y] off]
    (doseq [[offset row] rows]
      (s/put-string screen
                    x (+ offset y)
                    (apply str (map (comp sigil cell-status) row))))
    ;; XXX protagonist drawn on top
    (s/put-string screen
                  (+ py x) (+ px y) "@")))

;; Returns the new grid, new ploc
(defn move
  [dir grid loc]
  (let [newloc (mapv + (dir directions) loc)]
    (cond
      (inbounds? grid newloc) [grid
                               newloc]
      :default                [grid
                               loc])))

(defn getch [screen]
  (let [c (s/get-key-blocking screen)]
    (log/info "got char " c)
    c))

(defn quit
  [screen]
  (s/put-string screen 0 2 "BYE")
  (s/redraw screen)
  (Thread/sleep 1000))


(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (loop [[grid ploc :as state] [mgrid mploc]]
                   (draw grid screen ploc [0 0])
                   (s/redraw screen)
                   (let [c (getch screen)]
                     (cond
                       (= c :escape)            (quit screen)
                       (contains? directions c) (recur (move c grid ploc))
                       :else                    (recur state)))))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
