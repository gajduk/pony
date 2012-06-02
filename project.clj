(defproject pony "0.1.0"
  :description "FIXME: write description"
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :dev-dependencies [[org.clojure/clojure "1.3.0" :classifier "sources"]
                     [org.clojure/tools.trace "0.7.1"]
                     [lein-marginalia "0.7.0"]]
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [pony/parallelcolt "0.9.4"]]
  :repositories {"releases" ~(str (.toURI (java.io.File. "~/.m2/repository")))})
