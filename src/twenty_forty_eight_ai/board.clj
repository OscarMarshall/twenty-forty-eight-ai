(ns twenty-forty-eight-ai.board
  (:require [clojure.string :refer [join]]))

(def neighbor-posns
  (memoize (fn [[x y] dirs]
             (filter (fn [[x y]] (and (<= 0 x 3) (<= 0 y 3)))
                     (for [dir dirs]
                       (case dir
                         0 [x (dec y)]
                         1 [(inc x) y]
                         2 [x (inc y)]
                         3 [(dec x) y]))))))

(defn max-tile [board]
  (apply max (flatten board)))

(defn game-over?
  [board]
  (every? (fn [x]
            (every? (fn [y]
                      (let [value (get-in board [x y])]
                        (and (not (or (nil? value) (zero? value)))
                             (every? #(not= (get-in board %) value)
                                     (neighbor-posns [x y] [0 3])))))
                    (range 4)))
          (range 4)))

(defn rotate-cw
  [board turns]
  (case (mod turns 4)
    0 board
    1 (mapv (fn [y] (mapv #(get-in board [% y]) (range 4)))
            (reverse (range 4)))
    2 (vec (reverse (map #(vec (reverse %)) board)))
    3 (mapv (fn [y] (mapv #(get-in board [% y]) (reverse (range 4))))
            (range 4))))

(defn shift-up
  [board]
  (let [new-board (mapv (fn [column]
                          (loop [column (filter #(or (nil? %) (not (zero? %)))
                                                column)
                                 result []]
                            (if (empty? column)
                              (vec (take 4 (concat result (repeat 0))))
                              (let [[first-value second-value] column]
                                (cond
                                  (nil? first-value)
                                  (vec (take 4 (concat result (repeat nil))))

                                  (= first-value second-value)
                                  (recur
                                    (drop 2 column)
                                    (conj result (* first-value 2)))

                                  :else
                                  (recur
                                    (drop 1 column)
                                    (conj result first-value)))))))
                        board)]
    (when-not (= new-board board) new-board)))

(defn shift
  [board dir]
  (when-let [board (shift-up (rotate-cw board (- 4 dir)))]
    (rotate-cw board dir)))

(def all-posns (for [x (range 4) y (range 4)] [x y]))

(defn open-posns
  [board]
  (filter #(zero? (get-in board %)) all-posns))

(defn board-ln
  [cells head-sep body-sep tail-sep]
  (join (list head-sep (join body-sep cells) tail-sep (format "%n"))))

(defn board-str
  [board]
  (join (list (board-ln (repeat 4 (join (repeat 6 \─))) \┌ \┬ \┐)
              (join (board-ln (repeat 4 (join (repeat 6 \─))) \├ \┼ \┤)
                    (map #(board-ln % \│ \│ \│)
                         (map (fn [y]
                                (map #(let [value (nth % y)]
                                        (if (zero? value)
                                          (join (repeat 6 \space))
                                          (format "%6d" value)))
                                     board))
                              (range 4))))
              (board-ln (repeat 4 (join (repeat 6 \─))) \└ \┴ \┘))))
