(defproject myclojure "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter "1.5.6"]
                 [digest "1.4.4"]]
  :jvm-opts ["-Xmx4g" "-XX:+UseConcMarkSweepGC"] ; cljsbuild eats memory
  :main ^:skip-aot myclojure.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  )
