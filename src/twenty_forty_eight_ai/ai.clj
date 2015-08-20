(ns twenty-forty-eight-ai.ai
  (:require [clojure.math.numeric-tower :refer [abs expt]]
            [twenty-forty-eight-ai.board :as board]))

(defrecord Possibility [probability board])

(defn possibilities [board dir]
  (let [new-board (board/shift board dir)]
    (when (not= new-board board)
      (let [new-board-open-posns (board/open-posns new-board)
            posn-probability (/ 1 (count new-board-open-posns))
            four-tile-probability (* posn-probability (/ 1 10))
            two-tile-probability (* posn-probability (/ 9 10))]
        (mapcat (fn [posn]
                  [(Possibility. four-tile-probability (assoc-in new-board posn 4))
                   (Possibility. two-tile-probability (assoc-in new-board posn 2))])
                new-board-open-posns)))))

(defn possible-dirs
  [board]
  (filter (fn [dir]
            (let [old-board board
                  new-board (board/shift board dir)]
              (not= new-board old-board)))
          (range 4)))

(defn score-trees [score-fn board moves]
  (if (or (zero? moves) (board/game-over? board))
    [(score-fn board)]
    (->> (range 4)
      (map (partial possibilities board))
      (map (partial pmap #(* (:probability %)
                             (apply max (score-trees score-fn
                                                     (:board %)
                                                     (dec moves))))))
      (map (partial apply +)))))

(defn permutations [board]
  (mapcat #(let [rotated-board (board/rotate-cw board %)]
             [rotated-board (board/flip rotated-board)])
          (range 4)))

(def zig-zag-path
  (mapcat #(if (odd? %1) (reverse %2) %2)
          (range)
          (partition 4 (for [x (range 4) y (range 4)] [x y]))))

(defn zig-zag-score [board]
  (let [log2 (fn [n] (/ (Math/log n) (Math/log 2)))]
    (if (board/game-over? board)
      0
      (let [sub-path-score
            (fn sub-path-score [values]
              (let [[pre values]
                    (split-with (partial not= (apply max values)) values)]
                (/ (loop [values values, score 0]
                     (let [[first-value second-value] values
                           score (+ score first-value)]
                       (cond
                         (nil? second-value)
                         score

                         (< first-value second-value)
                         (+ score (log2 (sub-path-score (rest values))))

                         :default
                         (recur (rest values) score))))
                   (expt 2 (count pre)))))]
        (->> board
          permutations
          (map #(map (partial get-in %) zig-zag-path))
          (map sub-path-score)
          (apply max))))))

(defn monotonic-score [board]
  (if (board/game-over? board)
    0
    (loop [board board, previous (expt 2 18), score 0]
      (if (empty? board)
        (/ score (expt 2 3))
        (recur
          (rest board)
          (let [new-previous (apply min (first board))]
            (if (> new-previous previous) 0 new-previous))
          (let [row (first board)]
            (+ (* score 2)
               (max (- (min (apply + (first (split-with (partial > previous)
                                                        row)))
                            previous)
                       (apply + (map #(if (> %1 %2) (- %1 %2) 0)
                                     row
                                     (rest row))))
                    0))))))))

(defn resiliency-score [board]
  (if (board/game-over? board)
    0
    (let [score
          (fn [board]
            (loop [board board, last-tile 262144, score 0, penalty 0]
              (if (empty? board)
                score
                (let [differences
                      (fn [board]
                        (apply + (map-indexed (fn [index row]
                                                (/ (apply +
                                                     (map (comp abs -)
                                                          row
                                                          (rest row)))
                                                   (expt 2 index)))
                                              board)))
                      row (first board)]
                  (cond
                    (> (apply max row) last-tile)
                    (+ score (/ (- (loop [row row, previous 0, subscore 0]
                                     (let [current (first row)]
                                       (if (or (nil? current)
                                               (< current previous)
                                               (>= current last-tile))
                                         (apply -
                                           subscore
                                           (map (comp abs -) row (rest row)))
                                         (recur
                                           (rest row)
                                           current
                                           (+ subscore current)))))
                                   (/ (differences (rest board)) 2))
                                (expt 2 penalty)))

                    (not (every? identity (map > (cons last-tile row) row)))
                    (+ score (/ (- (loop [row row, score 0]
                                     (let [[first-value second-value] row
                                           score (+ score first-value)]
                                       (cond
                                         (or (nil? second-value)
                                             (< first-value second-value))
                                         (apply -
                                           score
                                           (map (comp abs -)
                                                (rest row)
                                                (rest (rest row))))

                                         :default
                                         (recur (rest row) score))))
                                   (/ (differences (rest board)) 2))
                                (expt 2 penalty)))

                    :default
                    (recur
                      (map reverse (rest board))
                      (last row)
                      (+ score (/ (apply + row) (expt 2 penalty)))
                      (inc penalty)))))))]
      (->> board
        permutations
        (map score)
        (apply max)))))

(defn safe? [board moves]
  (and (not (board/game-over? board))
       (or (zero? moves)
           (> (count (board/open-posns board)) moves)
           (->> 4
             range
             (map (partial possibilities board))
             (some (fn [dir]
                     (and (seq dir)
                          (every? #(safe? (:board %) (dec moves)) dir))))))))

(defn safe-dirs [board moves]
  (if (or (zero? moves) (> (count (board/open-posns board)) moves))
    (possible-dirs board)
    (let [results (->> 4
                    range
                    (map (partial possibilities board))
                    (pmap (fn [dir]
                            (and (seq dir)
                                 (every? #(safe? (:board %) (dec moves))
                                         dir)))))]
      (filter (partial nth results) (range 4)))))

(defn pick-dir [board]
  (let [possible (set (possible-dirs board))
        safe (set (safe-dirs board 6))
        allowed (set (or (seq (intersection possible safe)) possible))]
    (->> (score-trees monotonic-score board 3)
      (map vector (range))
      (filter (comp allowed first))
      (reduce (partial max-key second))
      first)))
