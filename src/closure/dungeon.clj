(ns closure.dungeon
  (:require [closure.cell :as cell]))

(defn- get-adjacencies [[row col]]
  [[(inc row) col] [(dec row) col]
   [row (inc col)] [row (dec col)]])

(defn- get-next-cell [dungeon filled-indices]
  (loop [[index & indices] (shuffle filled-indices)]
    (let [is-oob? (fn [idx] (= :oob (get-in dungeon idx)))
          valid-adjacencies (filter is-oob? (get-adjacencies index))]
      (if (seq valid-adjacencies) (rand-nth valid-adjacencies)
        (recur indices)))))
      
(defn generate-dungeon [height width]
  (let 
    [empty-dungeon (vec (repeat height (vec (repeat width :oob))))
     seed [(rand-nth (range height)) (rand-nth (range width))]
     seeded-dungeon (assoc-in empty-dungeon seed (cell/new))]
    (loop [dungeon seeded-dungeon
           filled #{seed}]
      (if (> (count filled) (* 0.75 height width))
        [filled dungeon]
        (let [cell (get-next-cell dungeon filled)
              new-dungeon (assoc-in dungeon cell (cell/new))]
          (recur new-dungeon (conj filled cell)))))))
