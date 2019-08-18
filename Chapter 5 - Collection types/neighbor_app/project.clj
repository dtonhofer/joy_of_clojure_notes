(defproject neighbor_app "0.1.0-SNAPSHOT"
  :description "running the neighbor code"
  :url "http://example.com/FIXME"
  :license {:name "UNLICENSE"
            :url "http://unlicense.org/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/test.check "0.9.0"]]
  :repl-options {:init-ns neighbor-app.core}
  :main neighbor-app.core/-main)
