(defproject closure "0.1.0-SNAPSHOT"
  :description "Closure: a Game"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [clojure-lanterna "0.9.4"]
                 [org.clojure/tools.logging "0.3.1"]]
  :main ^:skip-aot closure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
