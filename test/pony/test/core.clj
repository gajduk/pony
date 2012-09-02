(ns pony.test.core
  (:use [pony.core])
  (:use [clojure.test]))

(deftest assign-test
  (let [m (make-matrix 4 3)
        assigned-m
        (-> m
            transient
            (assign 1.0)
            persistent!)
        matrix (. assigned-m m)]
    (is
     (every? #(= %1 1.0)
             (for [r (range 4) c (range 3)] (.get matrix r c))))))
