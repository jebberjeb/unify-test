(defproject unify-test "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.unify "0.5.5"]
                 [com.taoensso/timbre "3.2.1"]]
  :main ^:skip-aot unify-test.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
