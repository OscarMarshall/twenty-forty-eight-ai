(ns twenty-forty-eight-ai.ai
  (:require
   [clojure.math.numeric-tower :refer [expt]]
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
                  [(Possibility. four-tile-probability (assoc-in board posn 4))
                   (Possibility. two-tile-probability (assoc-in board posn 2))])
                new-board-open-posns)))))

(defn possible-dirs
  [board]
  (filter (fn [dir]
            (let [old-board board
                  new-board (board/shift board dir)]
              (not= new-board old-board)))
          (range 0 4)))

(defn walk-possibility-tree [score-fn board moves]
  (if (or (zero? moves) (board/game-over? board))
    (score-fn board)
    (->> (map (partial possibilities board) (range 0 4))
         (map (partial map #(* (:probability %)
                               (walk-possibility-tree score-fn
                                                      (:board %)
                                                      (dec moves)))))
         (map (partial apply +))
         sort
         last)))

(defn permutations [board]
  (mapcat #(let [rotated-board (board/rotate-cw board %)]
             [rotated-board (board/flip rotated-board)])
          (range 0 4)))

(def zig-zag-path
  (mapcat #(if (odd? %1) (reverse %2) %2)
          (range)
          (partition 4 (for [x (range 0 4) y (range 0 4)] [x y]))))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn zig-zag-score [board]
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
           sort
           last))))

(defn pick-dir [board]
  (->> board
       possible-dirs
       (map #(vector (walk-possibility-tree zig-zag-score
                                            (board/shift board %)
                                            2)
                     %))
       sort
       last
       second))
