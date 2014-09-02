(ns twenty-forty-eight-ai.board)

(defn max-tile-value
  [grid]
  (apply max (flatten grid)))

(def neighbor-posns
  (memoize (fn [[x y]]
             (filter (fn [[x y]] (and (>= x 0) (< x 4) (>= y 0) (< y 4)))
                     [[x (- y 1)] [(+ x 1) y] [x (+ y 1)] [(- x 1) y]]))))

(defn game-over?
  [grid]
  (every? (fn [x] (every? (fn [y] (let [value (get-in grid [x y])]
                                    (and (not (zero? value))
                                         (every? #(not= (get-in grid %) value)
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
          (map #(loop [x      (filter pos? %)
                       column []
                       score  0]
                  (if (empty? x)
                    [(into [] (concat column (repeat (- 4 (count column)) 0)))
                     score]
                    (let [current (first x)]
                      (if (= (second x) current)
                        (let [current (* current 2)]
                          (recur (drop 2 x)
                                 (conj column current)
                                 (+ score current)))
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

(defn direction-possibilities
  [[grid score] direction]
  (let [[new-grid score] (shift [grid score] direction)]
    (for [posn  (if (not= new-grid grid) (open-posns new-grid) [])
          value [2 4]]
      [(assoc-in new-grid posn value) score])))

(defn all-possibilities
  [[grid score]]
  (map #(direction-possibilities [grid score] %) (range 0 4)))

(defn possible-directions
  [[grid score]]
  (filter (fn [direction]
            (let [old-grid         grid
                  [new-grid score] (shift [grid score] direction)]
              (not= new-grid old-grid)))
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
                            (map (fn [y]
                                   (map #(let [value (nth % y)]
                                           (if (zero? value)
                                             (apply str (repeat 6 \space))
                                             (format "%6d" value)))
                                        grid))
                                 (range 0 4))))
                     [(board-line (repeat 4 (apply str (repeat 6 \u2500)))
                                  \u2514
                                  \u2534
                                  \u2518)])))
