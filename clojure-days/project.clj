(defproject clojure-days "0.1.0-SNAPSHOT"
  :description "The Clojure part of the book 'Seven Languages in Seven Weeks'"
  :url "http://florianhartl.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot clojure-days.day-1
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
