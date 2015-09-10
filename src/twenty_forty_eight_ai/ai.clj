(ns twenty-forty-eight-ai.ai
  (:require [clojure.set :refer [intersection]]
            [clojure.math.numeric-tower :refer [abs expt]]
            [twenty-forty-eight-ai.board :as board]))

(defrecord Possibility [probability board])

(defn possibilities [board dir]
  (when-let [board (board/shift board dir)]
    (let [board-open-posns (board/open-posns board)
          posn-probability (/ 1 (count board-open-posns))
          four-tile-probability (* posn-probability (/ 1 10))
          two-tile-probability (* posn-probability (/ 9 10))]
      (mapcat (fn [posn]
                [(Possibility. four-tile-probability (assoc-in board posn 4))
                 (Possibility. two-tile-probability (assoc-in board posn 2))])
              board-open-posns))))

(defn possible-dirs [board] (filter (partial board/shift board) (range 4)))

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

(defn long-view [score-fn board moves]
  (let [score (if (pos? moves) 0 (score-fn board))]
    (if (or (every? nil? (flatten board)) (board/game-over? board))
      [score]
      (->> (range 4)
           (map (partial board/shift board))
           (map (partial mapv (partial replace {0 nil})))
           (map #(when (seq %) (apply max score (long-view score-fn % (dec moves)))))
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
                     (bit-shift-left (- (apply + column)
                                        (apply + (map #(if (> %1 %2)
                                                         (- %1 %2)
                                                         0)
                                                      column
                                                      (rest column))))
                                     (- (* (- 3 index) 15) 1))))
      (apply +))))

(def dir-str ["Up" "Right" "Down" "Left"])

(defn pick-dir [board]
  (let [possible (set (possible-dirs board))
        _ (apply println "  Possible:" (map dir-str possible))
        ; safe (set (safe-dirs board 6))
        ; _ (apply println "  Safe:    " (map dir-str safe))
        ; allowed (set (or (seq (intersection possible safe)) possible))
        ; _ (apply println "  Allowed: " (map dir-str allowed))
        scores (map max
                    (score-trees #(long-view monotonic-score % 1) board 2)
                    (score-trees #(vector (monotonic-score %)) board 3))
        _ (println "  Scores:  " (map #(bit-shift-right (long %) 44) scores))
        pick (->> scores
               (map-indexed vector)
              ;  (filter (comp allowed first))
               (apply max-key second)
               first)
        _ (println "  Pick:    " (dir-str pick))]
    pick))
