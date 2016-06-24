(defproject twenty-forty-eight-ai "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[clj-webdriver "0.7.2"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.seleniumhq.selenium/selenium-java "2.53.0"]
                 [prismatic/schema "1.1.0"]]
  :main ^:skip-aot twenty-forty-eight-ai.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
