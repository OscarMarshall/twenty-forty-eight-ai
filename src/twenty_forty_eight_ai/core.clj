(ns twenty-forty-eight-ai.core
  (:require [clojure.math.numeric-tower :refer :all]
            [clj-webdriver.core :refer :all]
            [twenty-forty-eight-ai.board :refer :all]
            [twenty-forty-eight-ai.ai :refer :all])
  (:gen-class))

(defn now [] (java.util.Date.))

(defn dir-num->dir-str [dir-num] (nth ["Up" "Right" "Down" "Left"] dir-num))

(defn -main
  "Sets up, runs, and tears down the main AI loop."
  [& args]
  (let [driver (start {:browser :chrome}
                      "http://gabrielecirulli.github.io/2048/")]
    (loop [moves 0]
      (let [grid
            (into [] (map (fn [x]
                            (into []
                                  (map (fn [y]
                                         (let [selector
                                               (str ".tile-position-" x "-" y)

                                               elements
                                               (find-elements driver
                                                              {:css selector})]
                                           (if (empty? elements)
                                             0
                                             (->
                                              elements
                                              last
                                              text
                                              Integer/parseInt))))
                                       (range 1 5))))
                          (range 1 5)))

            score
            (Integer/parseInt (text (find-element driver {:css ".best-container"})))

            board
            [grid score]]
        (println)
        (print (board-str board))
        (let [keep-playing (find-element driver {:css ".keep-playing-button"})]
          (when (displayed? keep-playing)
            (click keep-playing)))
        (if (game-over? (first board))
          (println "Game Over")
          (do
            (println (str "Move " (inc moves) ":"))
            (println (str (now)))
            (let [start (now)
                  safe  (safe-directions board 6)]
              (println (str "Safe:      "
                            (apply str
                                   (interpose ", " (map dir-num->dir-str safe)))
                            " ("
                            (- (.getTime (now)) (.getTime start))
                            "ms)"))
              (let [start (now)
                    dir   (pick-direction board
                                          2
                                          (if (empty? safe)
                                            (possible-directions board)
                                            safe))]
                (println (str "Direction: "
                              (dir-num->dir-str dir)
                              " ("
                              (- (.getTime (now)) (.getTime start))
                              "ms)"))
                (send-keys (find-element driver {:css "body"})
                           (nth [(key-code :arrow_up)
                                 (key-code :arrow_right)
                                 (key-code :arrow_down)
                                 (key-code :arrow_left)]
                                dir)))
              (Thread/sleep 128)
              (recur (inc moves)))))))))
