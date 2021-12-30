(defproject org.clojars.henrygarner/periscope "0.1.0"
  :description "Fast "
  :url "https://github.com/henrygarner/periscope"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :source-paths ["src"]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.3"]
                                  [com.rpl/specter "1.1.3"]]}
             :bench {:dependencies [[org.clojure/clojure "1.10.3"]
                                    [com.rpl/specter "1.1.3"]
                                    [criterium "0.4.6"]]}})
