(ns twenty-forty-eight-ai.board
  (:require [clojure.string :refer [join]]))

(defn max-tile [board]
  (apply max (flatten board)))

(def neighbor-posns
  (memoize (fn [[x y] dirs]
             (filter (fn [[x y]] (and (<= 0 x 3) (<= 0 y 3)))
                     (for [dir dirs]
                       (case dir
                         0 [x (dec y)]
                         1 [(inc x) y]
                         2 [x (inc y)]
                         3 [(dec x) y]))))))

(defn game-over?
  [board]
  (every? (fn [x]
            (every? (fn [y]
                      (let [value (get-in board [x y])]
                        (and (not (zero? value))
                             (every? #(not= (get-in board %) value)
                                     (neighbor-posns [x y] [0 3])))))
                    (range 0 4)))
          (range 0 4)))

(defn rotate-cw
  [board turns]
  (case (mod turns 4)
    0 board
    1 (mapv (fn [y] (mapv #(get-in board [% y]) (range 0 4))) (range 3 -1 -1))
    2 (vec (reverse (map #(vec (reverse %)) board)))
    3 (mapv (fn [y] (mapv #(get-in board [% y]) (range 3 -1 -1))) (range 0 4))))

(defn shift-up
  [board]
  (reduce conj
          []
          (map #(loop [x (filter pos? %)
                       column []]
                  (if (empty? x)
                    (vec (concat column (repeat (- 4 (count column)) 0)))
                    (let [current (first x)]
                      (if (= (second x) current)
                        (recur (drop 2 x) (conj column (* current 2)))
                        (recur (drop 1 x) (conj column current))))))
               board)))

(defn shift
  [board dir]
  {:pre [(not (neg? dir))]}
  (let [board (shift-up (rotate-cw board (- 4 dir)))]
    (rotate-cw board dir)))

(defn flip [board]
  (mapv (comp vec reverse) board))

(def all-posns (for [x (range 0 4) y (range 0 4)] [x y]))

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
                              (range 0 4))))
              (board-ln (repeat 4 (join (repeat 6 \─))) \└ \┴ \┘))))
