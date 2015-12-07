(ns closure.core
  (:require [lanterna.screen :as s]
            [clojure.tools.logging :as log]))

(defn getch [screen]
  (let [c (s/get-key-blocking screen)]
    (log/info "got char " c)
    c))

(def sigil {:oob   " ",
            :empty ".",
            :prot  "@"})

;; Gross: indexing into a 2d array yields [y, x] ordering
(def directions {:left [0 -1],
                 :right [0 1],
                 :up [-1 0],
                 :down [1 0]})

(defn inbounds?
  [grid [mx my] [x y]]
  (and (< x mx) (>= x 0) (< y my) (>= y 0)
       (not= :oob (get-in grid [x y]))))

;; Main overworld
(def mploc [1 1])
(def mbounds [3 5])
(def mgrid [[:oob :empty :empty :empty :oob]
            [:empty :empty :empty :empty :empty]
            [:oob :empty :empty :empty :oob]])

(defn quit
  [screen]
  (s/put-string screen 0 2 "BYE")
  (s/redraw screen)
  (Thread/sleep 1000))

(defn draw
  [grid screen [px py] off]
  (let [rows (map vector (iterate inc 0) grid)
        [x y] off]
    (doseq [[offset row] rows]
      (s/put-string screen
                    x (+ offset y)
                    (apply str (map sigil row))))
    ;; XXX protagonist drawn on top
    (s/put-string screen
                  (+ py x) (+ px y) "@")))

;; Returns the new grid, new ploc
(defn move
  [dir grid bounds loc]
  (let [newloc (mapv + (dir directions) loc)]
    (cond
      (inbounds? grid bounds newloc) [grid
                                      newloc]
      :default                       [grid
                                      loc])))


(defn main [screen-type]
  (let [screen (s/get-screen screen-type)]
    (s/in-screen screen
                 (loop [[grid ploc :as state] [mgrid mploc]]
                   (draw grid screen ploc [0 0])
                   (s/redraw screen)
                   (let [c (getch screen)]
                     (cond
                       (= c :escape) (quit screen)
                       (contains? directions c) (recur (move c grid mbounds ploc))
                       :else (recur state)))))))

(defn -main [& args]
  (let [args (set args)
        screen-type (cond
                      (args ":swing") :swing
                      (args ":text")  :text
                      :else           :auto)]
    (main screen-type)))
