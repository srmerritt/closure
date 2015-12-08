(ns closure.ui
  (:require [lanterna.screen :as s]
            [closure.cell :as cell]))

(def num-cols 80)
(def context-cols 24)
(def top-rows 24)
(def num-rows 34)
(def num-text-rows 5)

(def scr)

;; Text buffer.
;; put-text writes to the text buffer displayed under the world grid.
(def textbuf (ref []))
(defn put-text
  [line]
  (dosync
    (commute textbuf conj line)))

(defn init
  [screen-type]
  (def scr (s/get-screen screen-type))
  (s/start scr)
  ;; Hack recommended in a clojure-lanterna issue https://github.com/sjl/clojure-lanterna/issues/7
  (s/get-key scr)
  (s/redraw scr))

;; Util: [x y z] -> [[0 x] [1 y] [2 z]]
(defn with-ids
  [coll]
  (map vector (iterate inc 0) coll))

;; Cell drawing
(def sigil {:oob   " ",
            :empty ".",
            :prot  "@"})

(defmulti cell-status identity)
(defmethod cell-status :oob [_]
  :oob)
(defmethod cell-status :default [c]
  (cond
    (cell/has? c :prot) :prot
    :default            :empty))

;; Draw overworld. Offset here is relative to the overworld window position,
;; which is managed internally by closure.ui.
(defn draw-grid
  [grid off]
  (let [rows (with-ids grid)
        [x y] off]
    (doseq [[offset row] rows]
      (s/put-string scr
                    (+ context-cols x) (+ offset y)
                    (apply str (map (comp sigil cell-status) row))))))

(defn draw-text
  []
  (doseq [[i line] (with-ids (take-last num-text-rows @textbuf))]
    (s/put-string scr 1 (+ top-rows i 1) line)))

(defn draw-border
  []
  ;; Top bar
  (s/put-string scr 0 0
                (str "."
                     (apply str (repeat (- num-cols 2) "-"))
                     "."))
  ;; Text box separator bar
  (s/put-string scr 1 top-rows
                (apply str (repeat (- num-cols 2) "-")))
  ;; Bottom bar
  (s/put-string scr 0 (- num-rows 1)
                (str "'"
                     (apply str (repeat (- num-cols 2) "-"))
                     "'"))
  ;; Verticals
  (doseq [row (range 1 (- num-rows 1))]
    (s/put-string scr 0 row "|")
    (when (< row top-rows)
      (s/put-string scr (- context-cols 1) row "|"))
    (s/put-string scr (- num-cols 1) row "|")))

;; Main function for redrawing everything. Expects a seq of input to draw-grid,
;; i.e. for rendering multiple grids to the screen.
(defn redraw
  [grids]
  (s/clear scr)
  (draw-border)
  (draw-text)
  (doseq [g grids] (apply draw-grid g))
  (s/redraw scr))

(defn getch
  []
  (let [c (s/get-key-blocking scr)]
    c))

(defn quit
  []
  (put-text "come")
  (put-text "again")
  (put-text "soon")
  (draw-text)
  (s/redraw scr)
  (Thread/sleep 1000)
  (s/clear scr)
  (s/redraw scr)
  (s/stop scr))
