(ns twenty-forty-eight-ai.core
  (:require [clojure.math.numeric-tower :refer :all]
            [clj-webdriver.core :refer :all]
            [twenty-forty-eight-ai.board :refer :all]
            [twenty-forty-eight-ai.ai :refer :all])
  (:gen-class))

(defn -main
  "Sets up, runs, and tears down the main AI loop."
  [& args]
  (let [driver (start {:browser :chrome}
                      "http://gabrielecirulli.github.io/2048/")]
    (loop [moves 0]
      (let [board [(value-grid->level-grid
                     (map (fn [x]
                            (map (fn [y]
                                   (let [selector
                                         (str ".tile-position-" x "-" y)
                                         elements
                                         (find-elements driver {:css selector})]
                                     (if (empty? elements)
                                       0
                                       (->
                                         elements
                                         last
                                         text
                                         Integer/parseInt))))
                                 (range 1 5)))
                          (range 1 5)))
                   0]]
        (print (board-str board))
        (if (game-over? (first board))
          (print "Game Over")
          (do
            (print (str "Move " (inc moves) ": "))
            (flush)
            (let [safe      (safe-directions board 6)
                  direction (pick-direction board
                                            2
                                            (if (empty? safe)
                                              (possible-directions board)
                                              safe))]
              (println (nth ["Up" "Right" "Down" "Left"] direction))
              (println safe)
              (send-keys (find-element driver {:css "body"})
                         (nth [(key-code :arrow_up)
                               (key-code :arrow_right)
                               (key-code :arrow_down)
                               (key-code :arrow_left)]
                              direction)))
            (Thread/sleep 128)
            (recur (inc moves))))))))
