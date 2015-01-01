(ns twenty-forty-eight-ai.ai
  (:require
    [clojure.core.memoize        :as memoize]
    [twenty-forty-eight-ai.board :refer :all]))

(defn sum-number-lists [& lists]
  (cond
    (zero? (count lists)) nil
    (every? empty? lists) nil
    (== (count lists) 1)  (first lists)
    :default              (cons (apply + (filter identity (map first lists)))
                                (apply sum-number-lists (map rest lists)))))

(defn max-number-list [& lists]
  {:pre [(every? (partial every? number?) lists)]}
  (case (count lists)
    0 nil
    1 (first lists)
    (recur (cons (loop [x (first lists), y (second lists)]
                   (cond
                     (empty? y)              (first lists)
                     (empty? x)              (second lists)
                     (> (first x) (first y)) (first lists)
                     (< (first x) (first y)) (second lists)
                     :else                   (recur (rest x) (rest y))))
                 (drop 2 lists)))))

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

#_(defn flow-penalty-up
  [grid]
  (apply + (map #(loop [x (filter pos? %), penalty 0]
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
  ([grid] (zigzag-score-tl-up grid zigzag-path))
  ([grid path]
   {:post [(seq? %)]}
   (let [path-count (count path)]
     (if (zero? path-count)
       (list)
       (let [posn (first path)
             this (get-in grid posn)
             this (if (zero? (second posn))
                    this
                    (min this (get-in grid (map + posn [0 -1]))))]
         (if (== path-count 1)
           (list this)
           (let [that (get-in grid (second path))]
             (cond
               (< this that)
               (cons this (zigzag-score-tl-up grid (rest path)))

               (== this that)
               (cons (* this 2) (zigzag-score-tl-up grid (drop 2 path)))

               (> this that)
               (let [results (zigzag-score-tl-up grid (rest path))]
                 (cons (+ this (first results)) (rest results)))))))))))

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
    (max-number-list (zigzag-score-tl-up grid) (zigzag-score-tl-up grid*))))

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
        ['() '() '() '()]

        (zero? moves)
        (let [result (board-score grid)]
          [result result result result])

        :else
        (map (fn [dir]
               (let [tiles
                     (map (fn [tile]
                            (if (empty? tile)
                              (list)
                              (let [results
                                    (map #(apply max-number-list
                                                 (expected-board-scores
                                                   % (dec moves)))
                                         tile)
                                    results-count (count results)]
                                (map #(/ % results-count)
                                     (apply sum-number-lists results)))))
                          dir)]
                 (sum-number-lists (map (partial * 9/10) (first tiles))
                                   (map (partial * 1/10) (second tiles)))))
             (all-possibilities grid))))
    :lru/threshold 65536))

(defn pick-dir
  [grid moves]
  (let [results (expected-board-scores grid moves)]
    (.indexOf results (apply max-number-list results))))
