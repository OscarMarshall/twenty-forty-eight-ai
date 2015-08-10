(defproject twenty-forty-eight-ai "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [clj-webdriver "0.6.1"]]
  :main ^:skip-aot twenty-forty-eight-ai.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[expectations "2.1.2"]]
                   :plugins [[lein-autoexpect "1.6.0"]]}})
