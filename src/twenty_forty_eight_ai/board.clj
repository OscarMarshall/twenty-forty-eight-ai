(ns twenty-forty-eight-ai.board
  (:require [clojure.string :refer [join]]))

(defn max-tile
  [grid]
  (apply max (flatten grid)))

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
  [grid]
  (every? (fn [x]
            (every? (fn [y]
                      (let [value (get-in grid [x y])]
                        (and (not (zero? value))
                             (every? #(not= (get-in grid %) value)
                                     (neighbor-posns [x y] [0 3])))))
                    (range 0 4)))
          (range 0 4)))

(defn rotate-cw
  [grid turns]
  {:pre [(not (neg? turns))]}
  (case (rem turns 4)
    0 grid
    1 (vec (map (fn [y] (vec (map #(get-in grid [% y]) (range 0 4))))
                (range 3 -1 -1)))
    2 (vec (reverse (map #(vec (reverse %)) grid)))
    3 (vec (map (fn [y] (vec (map #(get-in grid [% y]) (range 3 -1 -1))))
                (range 0 4)))))

(defn shift-up
  [grid]
  (reduce conj
          []
          (map #(loop [x      (filter pos? %)
                       column []]
                  (if (empty? x)
                    (vec (concat column (repeat (- 4 (count column)) 0)))
                    (let [current (first x)]
                      (if (= (second x) current)
                        (recur (drop 2 x) (conj column (* current 2)))
                        (recur (drop 1 x) (conj column current))))))
               grid)))

(defn shift
  [grid dir]
  {:pre [(not (neg? dir))]}
  (let [grid (shift-up (rotate-cw grid (- 4 dir)))]
    (rotate-cw grid dir)))

(defn open-posns
  [grid]
  (filter #(zero? (get-in grid %)) (for [x (range 0 4) y (range 0 4)] [x y])))

(defn dir-possibilities
  [grid dir tile]
  (let [new-grid (shift grid dir)]
    (for [posn  (when (not= new-grid grid) (open-posns new-grid))]
      (assoc-in new-grid posn tile))))

(defn all-possibilities
  [grid]
  (map (fn [dir]
         (map #(dir-possibilities grid dir %)
              '(2 4)))
       (range 0 4)))

(defn possible-dirs
  [grid]
  (filter (fn [dir]
            (let [old-grid grid
                  new-grid (shift grid dir)]
              (not= new-grid old-grid)))
          (range 0 4)))

(defn grid-ln
  [cells head-sep body-sep tail-sep]
  (join (list head-sep (join body-sep cells) tail-sep (format "%n"))))

(defn grid-str
  [grid]
  (join (list (grid-ln (repeat 4 (join (repeat 6 \─))) \┌ \┬ \┐)
              (join (grid-ln (repeat 4 (join (repeat 6 \─))) \├ \┼ \┤)
                    (map #(grid-ln % \│ \│ \│)
                         (map (fn [y]
                                (map #(let [value (nth % y)]
                                        (if (zero? value)
                                          (join (repeat 6 \space))
                                          (format "%6d" value)))
                                     grid))
                              (range 0 4))))
              (grid-ln (repeat 4 (join (repeat 6 \─))) \└ \┴ \┘))))
