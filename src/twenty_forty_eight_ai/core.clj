(ns twenty-forty-eight-ai.core
  (:require
    [clojure.string :as str]
    [clj-webdriver.core :as web]
    [twenty-forty-eight-ai.board :as board]
    [twenty-forty-eight-ai.ai :as ai])
  (:gen-class))

(defn now [] (java.util.Date.))

(def dir-str ["Up" "Right" "Down" "Left"])

(def dir-button
  (vec (map web/key-code [:arrow_up :arrow_right :arrow_down :arrow_left])))

(defn read-grid* [driver]
  (fn []
    (mapv (fn [x]
            (mapv (fn [y]
                    (let [elements
                          (->> {:css (format ".tile-position-%d-%d" x y)}
                               (web/find-elements driver))]
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
        read-grid (read-grid* driver)
        read-score (read-score* driver)
        keep-playing (keep-playing* driver)
        body (web/find-element driver {:css "body"})]
    (loop [moves 0]
      (Thread/sleep 128)
      (println divider)
      (println (str (now)))
      (println (format "Score: %d" (read-score)))
      (let [grid (read-grid)]
        (print (board/board-str grid))
        (if (board/game-over? grid)
          (println "Game Over")
          (do
            (print (format "Move %d: " (inc moves)))
            (flush)
            (let [dir (ai/pick-dir grid)]
              (println (dir-str dir))
              (web/send-keys body (dir-button dir))
              (keep-playing)
              (recur (inc moves)))))))))
