(ns pony.test.core
  (:use [pony.core])
  (:use [clojure.test]))

(deftest assign-test
  (let [m (make-matrix 4 3)
        assigned-m
        (-> m
            transient
            (assign 1.0)
            persistent!)]
    (is (every? #(= %1 1.0) (flatten (seq assigned-m))))))
