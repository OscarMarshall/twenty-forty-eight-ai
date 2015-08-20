(ns twenty-forty-eight-ai.core
  (:require [clj-webdriver.core :as web]
            [clojure.string :as str]
            [twenty-forty-eight-ai.ai :as ai]
            [twenty-forty-eight-ai.board :as board])
  (:gen-class))

(defn now [] (java.util.Date.))

(def dir-str ["Up" "Right" "Down" "Left"])

(def dir-button
  (vec (map web/key-code [:arrow_up :arrow_right :arrow_down :arrow_left])))

(defn read-board* [driver]
  (fn []
    (mapv (fn [x]
            (mapv (fn [y]
                    (let [elements (web/find-elements
                                    driver
                                    {:css (format ".tile-position-%d-%d" x y)})]
                      (if (empty? elements)
                        0
                        (-> elements
                            last
                            web/text
                            Integer/parseInt))))
                  (range 1 5)))
          (range 1 5))))

(defn read-score* [driver]
  (let [score (web/find-element driver {:css ".best-container"})]
    #(Integer/parseInt (web/text score))))

(defn keep-playing* [driver]
  (let [keep-playing (web/find-element driver {:css ".keep-playing-button"})]
    #(when (web/displayed? keep-playing) (web/click keep-playing))))

(def divider (str/join (repeat 80 \─)))

(defn -main
  "Sets up, runs, and tears down the main AI loop."
  [& args]
  (let [driver (web/start {:browser :chrome}
                          (if (pos? (count args))
                            (first args)
                            "http://gabrielecirulli.github.io/2048/"))
        read-board (read-board* driver)
        read-score (read-score* driver)
        keep-playing (keep-playing* driver)
        body (web/find-element driver {:css "body"})]
    (loop [moves 0]
      (Thread/sleep 128)
      (println divider)
      (println (str (now)))
      (println "Score:" (read-score))
      (let [board (read-board)]
        (print (board/board-str board))
        (println "Rating:" (ai/monotonic-score board))
        (if (board/game-over? board)
          (println "Game Over")
          (do
            (print (format "Move %d: " (inc moves)))
            (flush)
            (let [dir (ai/pick-dir board)]
              (println (dir-str dir))
              (web/send-keys body (dir-button dir))
              (keep-playing)
              (recur (inc moves)))))))))
