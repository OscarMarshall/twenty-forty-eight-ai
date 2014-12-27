(ns twenty-forty-eight-ai.ai
  (:require
    [clojure.core.memoize        :as memoize]
    [twenty-forty-eight-ai.board :refer :all]))

#_(def safe-dirs
  (memoize/lru
    (fn [grid moves]
      (cond
        (or (zero? moves) (> (count (open-posns grid)) moves))
        (let [results (all-possibilities grid)]
          (filter (comp seq flatten (partial nth results)) (range 4)))

        :else
        (let [results
              (pmap (fn [dir]
                      (every? (fn [tile]
                                (and (seq tile)
                                     (every? #(seq (safe-dirs % (dec moves)))
                                             tile)))
                              dir))
                    (all-possibilities grid))]
          (filter (partial nth results) (range 4)))))
    :lru/threshold 4096))

(defn flow-penalty-up
  [grid]
  (apply + (map #(loop [x %, penalty 0]
                   (cond
                     (nil? (second x))
                     penalty

                     (< (first x) (second x))
                     (recur (rest x) (+ penalty (- (second x) (first x))))

                     :else
                     (recur (rest x) penalty)))
                grid)))

(defn max-corner
  [grid]
  (apply max (for [x [0 3] y [0 3]] (get-in grid [x y]))))

(def zigzag-path
  (apply concat
         (let [path (map (fn [y] (map #(vector % y) (range 4))) (range 4))]
           (for [x (range 4)]
             (let [col (nth path x)]
               (if (odd? x)
                 (reverse col)
                 col))))))

(defn zigzag-score-tl-up
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
            (< this that)  score
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

        grid*
        (vec (map #(vec (reverse %)) (rotate-cw grid 3)))]
    (max (- (zigzag-score-tl-up grid) (flow-penalty-up grid))
         (- (zigzag-score-tl-up grid*) (flow-penalty-up grid*)))))

(defn board-score
  [grid]
  (if (game-over? grid)
    0
    (zigzag-score grid)))

(def expected-board-scores
  (memoize/lru
    (fn [grid moves]
      (cond
        (game-over? grid)
        [0 0 0 0]

        (zero? moves)
        (let [result (board-score grid)]
          [result result result result])

        :else
        (map (fn [dir]
               (let [tiles
                     (map (fn [tile]
                            (if (empty? tile)
                              0
                              (let [results
                                    (map #(apply max (expected-board-scores
                                                       % (dec moves)))
                                         tile)]
                                (/ (apply + results) (count results)))))
                          dir)]
                 (+ (* (first tiles) 9/10) (* (second tiles) 1/10))))
             (all-possibilities grid))))
    :lru/threshold 2097152))

(defn pick-dir
  [grid moves]
  (apply max-key (partial nth (expected-board-scores grid moves)) (range 4)))
