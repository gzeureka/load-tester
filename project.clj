(defproject load-tester "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/core.async "1.2.603"]
                 [http-kit "2.3.0"]
                 ]
  :main ^:skip-aot load-tester.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
