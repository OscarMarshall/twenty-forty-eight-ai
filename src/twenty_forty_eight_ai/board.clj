(ns twenty-forty-eight-ai.board)

; A value is a number #(>= 0 %) #(== (.pow 2 x) %)

; A level is a number #(>= 0 %)

; A score is a number #(>= 0 %)

; A grid is a
;   [[level level level level]
;    [level level level level]
;    [level level level level]
;    [level level level level]]

; A board is a [grid score]

(def level->value
  "Takes a level and converts it to a value"
  (memoize (fn [level]
             (assert (>= level 0))
             (case level
               0 0
               1 2
               (* (level->value (dec level)) 2)))))

(def value->level
  "Takes a value and converts it to a level"
  (memoize (fn [value]
             (assert (zero? (rem value 2)))
             (case value
               0 0
               2 1
               (inc (value->level (quot value 2)))))))

(defn set-tile-value
  "Returns the grid with the tile at posn set to value"
  [grid posn value]
  (assoc-in grid posn (value->level value)))

(defn get-tile-value
  [grid posn]
  (level->value (get-in grid posn)))

(defn value-grid->level-grid
  [grid]
  (into [] (map #(into [] (map value->level %)) grid)))

(defn max-tile-value
  [grid]
  (level->value (reduce max (map #(reduce max %) grid))))

(def neighbor-posns
  (memoize (fn [[x y]]
             (filter (fn [[x y]] (and (>= x 0) (< x 4) (>= y 0) (< y 4)))
                     [[x (- y 1)] [(+ x 1) y] [x (+ y 1)] [(- x 1) y]]))))

(defn game-over?
  [grid]
  (every? (fn [x] (every? (fn [y] (let [level (get-in grid [x y])]
                                    (and (not (zero? level))
                                         (every? #(not= (get-in grid %) level)
                                                 (neighbor-posns [x y])))))
                          (range 0 4)))
          (range 0 4)))

(defn rotate-cw
  [grid turns]
  (assert (not (neg? turns)))
  (case (rem turns 4)
    0 grid
    1 (into [] (map (fn [y] (into [] (map #(get-in grid [% y])
                                          (range 0 4))))
                    (range 3 -1 -1)))
    2 (into [] (reverse (map #(into [] (reverse %)) grid)))
    3 (into [] (map (fn [y] (into [] (map #(get-in grid [% y])
                                          (range 3 -1 -1))))
                    (range 0 4)))))

(defn shift-up
  [[grid score]]
  (reduce (fn [[grid score] [column subscore]]
            [(conj grid column) (+ score subscore)])
          [[] score]
          (map #(loop [x (filter pos? %)
                       column []
                       score 0]
                  (if (empty? x)
                    [(into [] (concat column (repeat (- 4 (count column)) 0)))
                     score]
                    (let [current (first x)]
                      (if (= (second x) current)
                        (let [current (int current)]
                          (recur (drop 2 x)
                                 (conj column current)
                                 (+ score (level->value current))))
                        (recur (drop 1 x) (conj column current) score)))))
               grid)))

(defn shift
  [[grid score] direction]
  (assert (not (neg? direction)))
  (if (zero? direction)
    (shift-up [grid score])
    (let [[grid score] (shift-up [(rotate-cw grid (- 4 direction)) score])]
        [(rotate-cw grid direction) score])))

(defn open-posns
  [grid]
  (filter #(== (get-in grid %) 0) (for [x (range 0 4) y (range 0 4)] [x y])))

(defn possibilities
  [[grid score]]
  (map (fn [direction]
         (let [[new-grid score] (shift [grid score] direction)
               old-grid         grid
               open-posns       (open-posns new-grid)]
           (when (not= new-grid old-grid)
             (map (fn [value]
                    (map (fn [posn]
                           [(set-tile-value new-grid posn value) score])
                         open-posns))
                  (range 2 5 2)))))
       (range 0 4)))

(defn board-line
  [items head-separator body-separator tail-separator]
  (apply str (concat [head-separator]
                     (interpose body-separator items)
                     [tail-separator (format "%n")])))

(defn board-str
  [[grid score]]
  (apply str (concat [(board-line (repeat 4 (apply str (repeat 6 \u2500)))
                                  \u250c
                                  \u252c
                                  \u2510)]
                     (interpose
                       (board-line (repeat 4 (apply str (repeat 6 \u2500)))
                                   \u251c
                                   \u253c
                                   \u2524)
                       (map #(board-line % \u2502 \u2502 \u2502)
                            (map (fn [y] (map #(format "%6d" (nth % y)) grid))
                                 (range 0 4))))
                     [(board-line (repeat 4 (apply str (repeat 6 \u2500)))
                                  \u2514
                                  \u2534
                                  \u2518)])))
