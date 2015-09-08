(ns twenty-forty-eight-ai.core
  (:require [clj-webdriver.core :as web]
            [clojure.string :as str]
            [twenty-forty-eight-ai.ai :as ai]
            [twenty-forty-eight-ai.board :as board])
  (:gen-class))

(defn now [] (java.util.Date.))

(def dir-button
  (vec (map web/key-code [:arrow_up :arrow_right :arrow_down :arrow_left])))

(defn read-board [driver]
  (loop []
    (or (try
          (mapv (fn [x]
                  (mapv (fn [y]
                          (let [elements (web/find-elements
                                          driver
                                          {:css (format ".tile-position-%d-%d"
                                                        x
                                                        y)})]
                            (if (empty? elements)
                              0
                              (let [text (-> elements
                                             last
                                             web/text)]
                                (if (= text "")
                                  0
                                  (Integer/parseInt text))))))
                        (range 1 5)))
                (range 1 5))
          (catch Exception _ nil))
        (recur))))

(defn read-score [driver]
  (Integer/parseInt (web/text (web/find-element driver
                                                {:css ".best-container"}))))

(defn find-keep-playing-button [driver]
  (web/find-element driver {:css ".keep-playing-button"}))

(defn wait-for-board [driver board dir]
  (let [shifted-board (board/shift board dir)]
    (if (and (= (board/max-tile board) 1024)
             (= (board/max-tile shifted-board) 2048))
      (let [keep-playing (web/find-element driver
                                           {:css ".keep-playing-button"})]
        (while (not (web/displayed? keep-playing)))
        (while (web/displayed? keep-playing) (web/click keep-playing)))
      (loop []
        (let [board (read-board driver)
              differences (->>
                           (for [x (range 4), y (range 4)]
                             (let [expected-value (get-in shifted-board [x y])
                                   actual-value (get-in board [x y])]
                               (if (= actual-value expected-value)
                                 [[x y] nil]
                                 (if (zero? expected-value)
                                   [[x y] actual-value]
                                   [[x y] -1]))))
                           (filter second))
              difference (first differences)]
          (if (and (= (count differences) 1) (pos? (second difference)))
            difference
            (recur)))))))

(def divider (str/join (repeat 80 \─)))

(defn -main
  "Sets up, runs, and tears down the main AI loop."
  [& args]
  (let [driver (web/start {:browser :chrome}
                          (if (pos? (count args))
                            (first args)
                            "http://gabrielecirulli.github.io/2048/"))
        body (web/find-element driver {:css "body"})]
    (loop [moves 0]
      (println divider)
      (println (str (now)))
      (println "Score:" (read-score driver))
      (let [board (read-board driver)]
        (print (board/board-str board))
        (println "Rating:" (bit-shift-right (long (ai/monotonic-score board))
                                            44))
        (if (board/game-over? board)
          (println "Game Over")
          (let [_ (printf "Move %d:%n" (inc moves))
                dir (ai/pick-dir board)]
            (web/send-keys body (dir-button dir))
            (wait-for-board driver board dir)
            (recur (inc moves))))))))
