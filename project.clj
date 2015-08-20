(defproject twenty-forty-eight-ai "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [clj-webdriver "0.7.2"]
                 [org.seleniumhq.selenium/selenium-java "2.47.1"]]
  :main ^:skip-aot twenty-forty-eight-ai.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
