(defproject observatory "0.1.0-SNAPSHOT"
  :description "observatory"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-time "0.13.0"]]
  :source-paths ["src/main/clj/"]
  :test-paths ["src/test/clj"]
  :resource-paths ["src/resources/"]
  :main ^:skip-aot observatory.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})