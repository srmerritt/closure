(ns closure.dungeon
  (:require [closure.cell :as cell]))

(defn room-size []
  [(rand-nth (range 3 8)) (rand-nth (range 3 8))])

(defn transpose [shape offset]
  (map #(mapv + offset %) shape))

(defn int-avg [x y]
  (int (/ (+ x y) 2)))

(defn get-center [shape]
  (let [[[r1 c1] [r2 c2]] shape]
    [(int-avg r1 r2) (int-avg c1 c2)]))

(defn overlap? [shape-1 shape-2]
  (let [[[s1r1 s1c1] [s1r2 s1c2]] shape-1
        [[s2r1 s2c1] [s2r2 s2c2]] shape-2
        disjoint-r (or (< s1r1 s1r2 s2r1 s2r2)
                       (< s2r1 s2r2 s1r1 s1r2))
        disjoint-c (or (< s1c1 s1c2 s2c1 s2c2)
                       (< s2c1 s2c2 s1c1 s1c2))]
        (not (or disjoint-r disjoint-c))))

(defn inside? [bigger smaller]
  (let [[[bigr1 bigc1] [bigr2 bigc2]] bigger 
        [[smallr1 smallc1] [smallr2 smallc2]] smaller]
    (and (<= bigr1 smallr1 smallr2 bigr2)
         (<= bigc1 smallc1 smallc2 bigc2))))

(defn expand [[upper-left lower-right]]
  [(map dec upper-left) (map inc lower-right)])

(defn half [x] (int (/ x 2)))

(defn adjacent-rooms [size this-room]
  (let [[height width] size
        [[r1 c1] [r2 c2]] this-room
        this-room-height (- r2 r1)
        this-room-width (- c2 c1)
        base-room [[0 0] size]
        center (get-center this-room)
        hallway-length (rand-nth (range 3 8))
        left-disp [(- (half height)) (- (+ hallway-length (half this-room-width) width))]
        right-disp [(- (half height)) (+ hallway-length (half this-room-width))]
        up-disp   [(- (+ hallway-length (half this-room-height) height)) (- (half width))]
        down-disp  [(+ hallway-length (half this-room-height)) (- (half width))]
        adjacent-centers (map #(mapv + center %) [left-disp right-disp up-disp down-disp])]
    (map #(transpose base-room %) adjacent-centers)))

(defn find-common-index [[min-1 max-1] [min-2 max-2]]
  (first (drop-while (fn [x] (not-any? (partial = x) (range min-1 (inc max-1))))
                                                     (range min-2 (inc max-2)))))
  
(defn get-index-range [[min-1 max-1] [min-2 max-2]]
  (range (min max-1 max-2) (inc (max min-1 min-2))))

(defn get-hallway-indices [this-room next-room]
  (let [[[this-r1 this-c1] [this-r2 this-c2]] this-room
        [[next-r1 next-c1] [next-r2 next-c2]] next-room
        disjoint-row? (or (< this-r1 this-r2 next-r1 next-r2)
                          (< next-r1 next-r2 this-r1 this-r2))
        disjoint-index (if disjoint-row? 0 1)
        overlap-index (if disjoint-row? 1 0)
        common-index (find-common-index (map #(nth % overlap-index) this-room)
                                        (map #(nth % overlap-index) next-room))
        index-range (get-index-range (map #(nth % disjoint-index) this-room)
                                     (map #(nth % disjoint-index) next-room))]
    (for [idx index-range]
      (if disjoint-row? [idx common-index] [common-index idx]))))


(defn get-next-room [grid-dimensions rooms]
  (let [next-room-size (room-size)
        grid [[0 0] grid-dimensions]]
    (loop [[[room-index room] & other-rooms] (shuffle rooms)]
      (let [candidates (adjacent-rooms next-room-size room)
            valid-candidates (filter (fn [room]
                                       (and (inside? grid room)
                                            (not-any? (partial overlap? (expand room)) (map second rooms)))) candidates)]
            (if (seq valid-candidates) [room-index (rand-nth valid-candidates)]
                (recur other-rooms))))))

(defn generate-dungeon-rooms [height width num-rooms]
  (let 
    [empty-dungeon (vec (repeat height (vec (repeat width :oob))))
     initial-room [[0 0] [5 5]]
     [rooms hallways] (loop [index 1
                             hallways []
                             rooms [[0 initial-room]]]
                        (if (>= (count rooms) num-rooms) [rooms hallways]
                          (let [[prev-room-index next-room] (get-next-room [height width] rooms)]
                                (recur (inc index) (conj hallways [prev-room-index index]) (conj rooms [index next-room])))))
     indices (for [[index [[r1 c1] [r2 c2]]] rooms
                   row (range r1 (inc r2))
                   col (range c1 (inc c2))]
               [row col])
     hall-indices (for [[source-room-index target-room-index] hallways
                        index (get-hallway-indices (second (nth rooms source-room-index))
                                                   (second (nth rooms target-room-index)))]
                    index)
     dungeon (reduce (fn [dungeon index]
              (assoc-in dungeon index (cell/new))) empty-dungeon (concat indices hall-indices))]
    [indices dungeon]))

;; Old stuff - unused (this naively builds a map from a starting cell and generates adjacencies)

(defn- get-adjacencies [[row col]]
  [[(inc row) col] [(dec row) col]
   [row (inc col)] [row (dec col)]])

(defn- get-next-cell [dungeon filled-indices]
  (let [filled-adj (seq filled-indices)]
    (loop []
      (let [is-oob? (fn [idx] (= :oob (get-in dungeon idx)))
            index (rand-nth filled-adj)
            valid-adjacencies (filter is-oob? (get-adjacencies index))]
        (if (seq valid-adjacencies) (rand-nth valid-adjacencies)
          (recur))))))
  
(defn generate-dungeon-old [height width fullness]
  (let
    [empty-dungeon (vec (repeat height (vec (repeat width :oob))))
     seed [(rand-nth (range height)) (rand-nth (range width))]
     seeded-dungeon (assoc-in empty-dungeon seed (cell/new))]
    (loop [dungeon seeded-dungeon
           filled #{seed}]
      (if (> (count filled) (* fullness height width))
        [filled dungeon]
        (let [cell (get-next-cell dungeon filled)
              new-dungeon (assoc-in dungeon cell (cell/new))]
          (recur new-dungeon (conj filled cell)))))))
