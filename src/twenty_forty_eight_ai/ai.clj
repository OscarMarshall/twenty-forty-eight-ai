(ns twenty-forty-eight-ai.ai
  (:require [twenty-forty-eight-ai.board :refer :all]))

(defn safe-direction
  [[grid score] moves]
  (cond
    (game-over? grid) false
    (zero? moves) true
    (> (count (open-posns grid)) moves) true
    :else (let [possibilities (possibilities [grid score])]
            (some (fn [direction]
                      (when (every? (fn [value]
                                      (when (pos? (count value))
                                        (every? #(safe-direction % (dec moves))
                                                value)))
                                    (get possibilities direction))
                        direction))
                  (range 0 4)))))
