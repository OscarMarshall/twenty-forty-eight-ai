(ns twenty-forty-eight-ai.core
  (:require
    [clojure.math.numeric-tower  :refer :all]
    [clojure.string              :refer [join]]
    [clj-webdriver.core          :refer :all]
    [twenty-forty-eight-ai.board :refer :all]
    [twenty-forty-eight-ai.ai    :refer :all])
  (:gen-class))

(defn now [] (java.util.Date.))

(def dir-str ["Up" "Right" "Down" "Left"])

(defn read-grid [driver]
  (vec (map (fn [x]
              (vec (map (fn [y]
                          (let [selector (str ".tile-position-" x "-" y)
                                elements (find-elements driver {:css selector})]
                            (if (empty? elements)
                              0
                              (->
                                elements
                                last
                                text
                                Integer/parseInt))))
                        (range 1 5))))
            (range 1 5))))

(defn read-score [driver]
  (Integer/parseInt (text (find-element driver {:css ".best-container"}))))

(defn -main
  "Sets up, runs, and tears down the main AI loop."
  [& args]
  (let [driver (start {:browser :chrome}
                      "http://gabrielecirulli.github.io/2048/")]
    (loop [moves 0
           times nil]
      (let [grid (read-grid driver)
            score (read-score driver)
            board [grid score]]
        (print (grid-str grid))
        (let [keep-playing (find-element driver {:css ".keep-playing-button"})]
          (when (displayed? keep-playing) (click keep-playing)))
        (if (game-over? grid)
          (println "Game Over")
          (do
            (println (str "Move " (inc moves) ":"))
            (println (str (now)))
            (let [start (now)
                  dir   (pick-dir grid 3)
                  time  (- (.getTime (now)) (.getTime start))
                  times (cons time times)]
              (print (str "Direction: " (dir-str dir) (format "%n")
                          "Time:" (format "%n")
                          "     Last: " time "ms" (format "%n")
                          "  Average: " (quot (apply + times) (count times))
                            "ms" (format "%n")
                          "      Max: " (apply max times) "ms" (format "%n")))
              (println)
              (send-keys (find-element driver {:css "body"})
                         (nth [(key-code :arrow_up)
                               (key-code :arrow_right)
                               (key-code :arrow_down)
                               (key-code :arrow_left)]
                              dir))
              (Thread/sleep 128)
              (recur (inc moves) times))))))))
