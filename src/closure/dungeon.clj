(ns closure.dungeon
  (:require [closure.cell :as cell]))

(defn- get-adjacencies [row col]
  [[(inc row) col] [(dec row) col]
   [row (inc col)] [row (dec col)]])

(defn- get-next-cell [dungeon filled-indices]
  (let 
    [available-cells (set
                       (apply concat
                        (for [[row col] filled-indices
                              :let [adjacencies (get-adjacencies row col)]]
                          (filter #(= :oob (get-in dungeon %)) adjacencies))))]
     (rand-nth (seq available-cells))))

(defn generate-dungeon [height width]
  (let 
    [empty-dungeon (vec (repeat height (vec (repeat width :oob))))
     seed [(rand-nth (range width)) (rand-nth (range height))]
     starting-dungeon (assoc-in empty-dungeon seed (cell/new))]
    (loop [dungeon starting-dungeon
           filled #{seed}]
      (if (> (count filled) (* 0.75 height width))
        [filled dungeon]
        (let [cell (get-next-cell dungeon filled)
              new-dungeon (assoc-in dungeon cell (cell/new))]
          (recur new-dungeon (conj filled cell)))))))
