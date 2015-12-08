(ns closure.ui
  (:require [lanterna.screen :as s]
            [closure.cell :as cell]))

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

(def num-cols 80)
(def context-cols 24)
(def top-rows 24)
(def num-rows 34)

(def scr)

(defn init
  [screen-type]
  (def scr (s/get-screen screen-type))
  (s/start scr)
  (s/get-key scr)
  (s/redraw scr))

(defn quit
  []
  (s/put-string scr 0 2 "BYE")
  (s/redraw scr)
  (Thread/sleep 1000)
  (s/stop scr))

(defn redraw [] (s/redraw scr))

(defn clear-screen
  []
  (let [blank (apply str (repeat num-cols \space))]
    (doseq [row (range num-rows)]
      (s/put-string scr 0 row blank))))

;; Draw overworld. Offset here is relative to the overworld window position,
;; which is managed internally by closure.ui.
(defn draw-grid
  [grid off]
  (let [rows (map vector (iterate inc 0) grid)
        [x y] off]
    (doseq [[offset row] rows]
      (s/put-string scr
                    (+ context-cols x) (+ offset y)
                    (apply str (map (comp sigil cell-status) row))))))

(defn draw-border
  []
  ;; Top bar
  (s/put-string scr 0 0
                (str "."
                     (apply str (repeat (- num-cols 2) "-"))
                     "."))
  ;; Text box separator bar
  (s/put-string scr 1 num-rows
                (apply str (repeat (- num-cols 2) "-")))
  ;; Bottom bar
  (s/put-string scr 0 (- top-rows 1)
                (str "'"
                     (apply str (repeat (- num-cols 2) "-"))
                     "'"))
  ;; Verticals
  (doseq [row (range 1 (- num-rows 1))]
    (s/put-string scr 0 row "|")
    (when (< row top-rows)
      (s/put-string scr (- context-cols 1) row "|"))
    (s/put-string scr (- num-cols 1) row "|")))

(defn getch []
  (let [c (s/get-key-blocking scr)]
    c))
