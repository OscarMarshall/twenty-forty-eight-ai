(ns twenty-forty-eight-ai.ai
  (:require [clojure.set :refer [intersection]]
            [clojure.math.numeric-tower :refer [abs expt]]
            [twenty-forty-eight-ai.board :as board]))

(defrecord Possibility [probability board])

(defn possibilities [board dir]
  (when-let [board (board/shift board dir)]
    (let [board-open-posns      (board/open-posns board)
          denominator           (* (count board-open-posns) 10)
          four-tile-probability (/ 1 denominator)
          two-tile-probability  (/ 9 denominator)]
      (mapcat (fn [posn]
                [(Possibility. four-tile-probability (assoc-in board posn 4))
                 (Possibility. two-tile-probability (assoc-in board posn 2))])
              board-open-posns))))

(defn possible-dirs [board] (filter (partial board/shift board) (range 4)))

(defn long-view [score-fn board ^long moves]
  (let [score (if (> moves 0) 0 (score-fn board))]
    (if (or (every? nil? (flatten board)) (board/game-over? board))
      [score]
      (->> (range 4)
           (map (partial board/shift board))
           (map (partial mapv (partial replace {0 nil})))
           (map #(when (seq %) (->> (long-view score-fn % (dec moves))
                                    (apply max score))))
           (replace {nil 0})))))

(defn score-trees [score-fn board moves]
  (if (or (zero? moves) (board/game-over? board))
    (score-fn board)
    (->> (range 4)
         (map (partial possibilities board))
         (map (partial pmap #(* (:probability %)
                                (apply max (score-trees score-fn
                                                        (:board %)
                                                        (dec moves))))))
         (map (partial apply +)))))

(defn monotonic-score [board]
  (if (board/game-over? board)
    0
    (->> board
         (mapv (partial replace {nil 0}))
         (map-indexed (fn [index column]
                        (-> (apply + column)
                            (- (apply + (map (fn [x y]
                                                  (if (> x y)
                                                    (- x y)
                                                    0))
                                                column
                                                (rest column))))
                            (bit-shift-left (- (* (- 3 index) 15) 1)))))
         (apply +))))

(def dir-str ["Up" "Right" "Down" "Left"])

(defn pick-dir [board]
  (let [possible (set (possible-dirs board))
        _        (apply println "  Possible:" (map dir-str possible))
        scores   (map max
                      (score-trees #(long-view monotonic-score % 1) board 2)
                      (score-trees #(vector (monotonic-score %)) board 3))
        _        (println "  Scores:  " (map #(bit-shift-right (long %) 44)
                                             scores))
        pick     (->> scores
                      (map-indexed vector)
                      (filter (comp possible first))
                      (apply max-key second)
                      first)
        _        (println "  Pick:    " (dir-str pick))]
    pick))
