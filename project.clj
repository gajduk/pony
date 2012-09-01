(defproject pony "0.1.0"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.nrepl "0.2.0-beta9"]
                 [pony/parallelcolt "0.9.4"]]
  :repositories {"releases" ~(str (.toURI (java.io.File. "~/.m2/repository")))})
