(defproject aoc2022 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [instaparse "1.4.12"]
                 [zprint "1.2.3"]
                 [aysylu/loom "1.0.2"]]
  :main ^:skip-aot aoc2022.core
  :target-path "target/%s"
  :plugins [[mvxcvi/whidbey "2.2.1"]]
  :middleware [whidbey.plugin/repl-pprint]
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
