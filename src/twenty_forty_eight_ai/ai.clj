(ns twenty-forty-eight-ai.ai
  (:require [twenty-forty-eight-ai.board :refer :all]))

(defn safe-direction?
  [[grid score] moves]
  (cond
    (game-over? grid)
    false

    (or (zero? moves) (> (count (open-posns grid)) moves))
    (some #(when-not (empty? (direction-possibilities [grid score] %)) %)
          (range 0 4))

    :else
    (some (fn [direction]
            (let [possibilities
                  (direction-possibilities [grid score] direction)]
              (and (not (nil? possibilities))
                   (every? #(safe-direction? % (dec moves)) possibilities))))
          (range 0 4))))

(defn safe-directions
  [[grid score] moves]
  (filter (fn [direction]
            (let [possibilities
                  (direction-possibilities [grid score] direction)]
              (and (not (empty? possibilities))
                   (if (zero? moves)
                     true
                     (every? #(safe-direction? % (dec moves)) possibilities)))))
          (range 0 4)))

(defn flow-penalty-up
  [grid]
  (apply + (map #(loop [x % penalty 0]
                   (cond
                     (or (nil? (second x)) (zero? (first x)))
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
   (let [current            (get-tile-value grid posn)
         neighbors          (neighbor-posns posn)
         lte-neighbors (filter #(<= (get-tile-value grid %) current) neighbors)]
     (if (empty? lte-neighbors)
       0
       (let [max-neighbor-value (apply max (map (partial get-tile-value grid)
                                                lte-neighbors))]
         (cond
           (zero? max-neighbor-value)
           current

           (== max-neighbor-value current)
           (* current 2)

           :else
           (let [max-neighbors (filter #(== (get-tile-value grid %)
                                            max-neighbor-value)
                                       neighbors)]
             (+ current
                (apply max (map (partial chain-score grid) max-neighbors)))))))))
  ([grid]
   (let [max-tile (max-tile-value grid)]
     (apply max (for [x (range 0 4), y (range 0 4)]
                  (if-not (== (get-tile-value grid [x y]) max-tile)
                    0
                    (chain-score grid [x y])))))))

(defn max-corner
  [grid]
  (apply max (for [x [0 3] y [0 3]] (get-tile-value grid [x y]))))

(defn board-score
  [[grid score]]
  (if (game-over? grid)
    0
    (-> (max-corner grid)
        (+ (chain-score grid))
        (- (* (flow-penalty grid) 4)))))

(defn board-score-lower-bound
  [[grid score] moves]
  (cond
    (game-over? grid)
    0

    (zero? moves)
    (board-score [grid score])

    :else
    (apply max (map (fn [direction]
                      (if (nil? direction)
                        0
                        (apply min (map #(board-score-lower-bound % (dec moves))
                                        direction))))
                    (all-possibilities [grid score])))))

(defn direction-min-board-score
  [[grid score] moves direction]
  (apply min (map #(board-score-lower-bound % (dec moves))
                  (direction-possibilities [grid score] direction))))

(defn pick-direction
  [[grid score] moves safe]
  (if (zero? moves)
    (first safe)
    (apply max-key #(direction-min-board-score [grid score] moves %) safe)))
