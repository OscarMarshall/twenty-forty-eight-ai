(ns twenty-forty-eight-ai.ai
  (:require [twenty-forty-eight-ai.board :refer :all]))

(defn safe-direction?
  [[grid score] moves]
  (some (if (or (zero? moves) (> (count (open-posns grid)) moves))
          #(when-not (empty? (direction-possibilities [grid score] %)) %)
          (fn [direction]
            (let [possibilities
                  (direction-possibilities [grid score] direction)]
              (and (not (empty? possibilities))
                   (every? #(safe-direction? % (dec moves)) possibilities)))))
        (range 0 4)))

(defn safe-directions
  [[grid score] moves]
  (filter (comp not nil?)
          (pmap (fn [direction]
                  (let [possibilities
                        (direction-possibilities [grid score] direction)]
                    (when (and (not (empty? possibilities))
                               (every? #(safe-direction? % (dec moves))
                                       possibilities))
                      direction)))
                (range 0 4))))

(defn flow-penalty-up
  [grid]
  (apply + (map #(loop [x % penalty 0]
                   (cond
                     (nil? (second x))
                     penalty

                     (< (first x) (second x))
                     (recur (rest x) (+ penalty (- (second x) (first x))))

                     :else
                     (recur (rest x) penalty)))
                grid)))

(defn flow-penalty
  [grid]
  (apply min (map #(flow-penalty-up (rotate-cw grid %)) (range 0 4))))

(defn chain-score
  ([grid posn]
   (let [current            (get-in grid posn)
         neighbors          (neighbor-posns posn)
         lte-neighbors (filter #(<= (get-in grid %) current) neighbors)]
     (if (empty? lte-neighbors)
       0
       (let [max-neighbor-value (apply max (map (partial get-in grid)
                                                lte-neighbors))]
         (cond
           (zero? max-neighbor-value)
           current

           (== max-neighbor-value current)
           (* current 2)

           :else
           (let [max-neighbors (filter #(== (get-in grid %)
                                            max-neighbor-value)
                                       neighbors)]
             (+ current (apply max (map (partial chain-score grid)
                                        max-neighbors)))))))))
  ([grid]
   (let [max-tile (max-tile-value grid)]
     (apply max (for [x (range 0 4), y (range 0 4)]
                  (if-not (== (get-in grid [x y]) max-tile)
                    0
                    (chain-score grid [x y])))))))

(defn max-corner
  [grid]
  (apply max (for [x [0 3] y [0 3]] (get-in grid [x y]))))

(def zigzag-path
  (apply concat
         (let [path (map (fn [x] (map (fn [y] [x y]) (range 0 4))) (range 0 4))]
           (for [x (range 0 4)]
             (let [col (nth path x)]
               (if (odd? x)
                 (reverse col)
                 col))))))

(defn zigzag-score-tl-down
  [grid]
  (loop [path zigzag-path
         score 0]
    (let [posn (first path)
          this (get-in grid posn)]
      (if (== (count path) 1)
        (+ score this)
        (let [that (get-in grid (second path))]
          (cond
           (== this that) (+ score (* this 2))
           (< this that)  (+ score (quot (chain-score grid posn) 2))
           :else          (recur (rest path) (+ score this))))))))

(defn zigzag-score
  [grid]
  (let [grid
        (let [[x y] (apply max-key
                           (partial get-in grid)
                           (for [x [0 3] y [0 3]] [x y]))]
          (rotate-cw grid (case [x y]
                            [0 0] 0
                            [0 3] 1
                            [3 3] 2
                            [3 0] 3)))

        grid2
        (into [] (map #(into [] (reverse %)) (rotate-cw grid 3)))]
    (max (zigzag-score-tl-down grid) (zigzag-score-tl-down grid2))))

(defn board-score
  [[grid score]]
  (if (game-over? grid)
    0
    (-> (max-corner grid)
        (+ (zigzag-score grid))
        (- (flow-penalty grid)))))

(defn board-score-lower-bound
  [[grid score] moves]
  (cond
    (game-over? grid)
    0

    (zero? moves)
    (board-score [grid score])

    :else
    (apply max (pmap (fn [direction]
                       (if (empty? direction)
                         0
                         (apply min (pmap #(board-score-lower-bound %
                                                                    (dec moves))
                                          direction))))
                     (all-possibilities [grid score])))))

(defn direction-min-board-score
  [[grid score] moves direction]
  (apply min (pmap #(board-score-lower-bound % (dec moves))
                   (direction-possibilities [grid score] direction))))

(defn pick-direction
  [[grid score] moves safe]
  (apply max-key #(direction-min-board-score [grid score] moves %) safe))
