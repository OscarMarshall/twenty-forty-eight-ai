(ns twenty-forty-eight-ai.ai
  (:require [clojure.set :refer [intersection]]
            [clojure.math.numeric-tower :refer [abs expt]]
            [clojure.core.reducers :as r]
            [twenty-forty-eight-ai.board :as board]
            [clojure.core.async :as a]
            [clojure.core.async.impl.protocols :as ap]
            [schema.core :as s]))

(defrecord Possibility [probability board])

(defn possibilities [board dir]
  (when-let [board (board/shift board dir)]
    (let [board-open-posns      (board/open-posns board)
          denominator           (* (count board-open-posns) 10)
          four-tile-probability (/ 1 denominator)
          two-tile-probability  (/ 9 denominator)]
      (r/mapcat (fn [posn]
                  [(Possibility. four-tile-probability (assoc-in board posn 4))
                   (Possibility. two-tile-probability (assoc-in board posn 2))])
                board-open-posns))))

(defn possible-dirs [board] (filter (partial board/shift board) (range 4)))

(defn long-view [score-fn board ^long moves]
  (let [score (if (> moves 0) 0 (score-fn board))]
    (if (or (every? nil? (flatten board)) (board/game-over? board))
      [score]
      (->> (range 4)
           (r/map (partial board/shift board))
           (r/map (partial mapv (partial replace {0 nil})))
           (r/map #(when (seq %) (->> (long-view score-fn % (dec moves))
                                      (apply max score))))
           (into [])
           (replace {nil 0})))))

(defn score-trees [score-fn board moves]
  (if (or (zero? moves) (board/game-over? board))
    (score-fn board)
    (->> (range 4)
         (pmap (fn [direction]
                 (->> (possibilities board direction)
                      (r/map #(* (:probability %)
                                 (apply max (score-trees score-fn
                                                         (:board %)
                                                         (dec moves)))))
                      (into [])
                      (apply +)))))))

(s/defn score-trees-async :- [(s/protocol ap/ReadPort)
                              {[[s/Num]] [(s/protocol ap/ReadPort)]}]
  [board :- [[s/Num]], moves :- s/Num]
  (cond
    (board/game-over? board) [(a/go 0) {}]
    (zero? moves) (let [result (a/chan)]
                    [result {board [result]}])
    :else (->> (range 4)
               (map (fn [direction]
                      (->> (possibilities board direction)
                           (r/map (fn [{:keys [board probability]}]
                                    (a/go (* probability
                                             (apply max
                                                    (a/<! (score-trees-async board (dec moves))))))))
                           (into [])))))))

(defn monotonic-score [board]
  (if (board/game-over? board)
    0
    (->> board
         (map-indexed (fn [index column]
                        (let [column (replace {nil 0} column)]
                          (-> (apply + column)
                              (- (apply + (map (fn [x y]
                                                 (if (> x y)
                                                   (- x y)
                                                   0))
                                               column
                                               (rest column))))
                              (bit-shift-left (- (* (- 3 index) 15) 1))))))
         (apply +))))

(def dir-str ["Up" "Right" "Down" "Left"])

(defn pick-dir [board]
  (time (let [possible (set (possible-dirs board))
              _        (apply println "  Possible:" (map dir-str possible))
              scores   (score-trees #(long-view monotonic-score % 1) board 2)
              _        (println "  Scores:  "
                                (map #(bit-shift-right (long %) 44) scores))
              pick     (->> scores
                            (map-indexed vector)
                            (filter (comp possible first))
                            (apply max-key second)
                            first)
              _        (println "  Pick:    " (dir-str pick))]
          pick)))
