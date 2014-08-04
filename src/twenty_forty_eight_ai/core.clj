(ns twenty-forty-eight-ai.core
  (:require [clj-webdriver.core :refer :all]
            [twenty-forty-eight-ai.board :refer :all])
  (:gen-class))

(defn -main
  "Sets up, runs, and tears down the main AI loop."
  [& args]
  (let [driver (start {:browser :chrome}
                      "http://gabrielecirulli.github.io/2048/")]
    (println (System/getProperty "file.encoding"))
    (loop [moves 0]
      (print (board-str
               [(value-grid->level-grid
                  (map (fn [x]
                         (map (fn [y]
                                (let [selector (str ".tile-position-" x "-" y)
                                      elements (find-elements driver
                                                              {:css selector})]
                                  (if (empty? elements)
                                    0
                                    (Integer/parseInt (text (last elements))))))
                              (range 1 5)))
                       (range 1 5)))
                0]))
      (flush)
      (send-keys (find-element driver {:css "body"}) (key-code :arrow_up))
      (Thread/sleep 128)
      (if (== moves 10)
        0
        (recur (inc moves)))))
  (System/exit 0))
