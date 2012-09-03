(ns pony.test.core
  (:use [pony.core])
  (:use [clojure.test]))

;;; Tests for editable types
;;; TODO: Check memory addresses to make sure copies are done when
;;; expected, and not done when not expected.
;;; TODO: Check owner thread and make sure that it is set to nil or to
;;; the appropriate thread.
(defn is-edit-lock [m val]
  (is (= @(. m edit-lock) val)))

(defmacro has-editable-flags [m {:keys [readable editable]}]
  `(do
    ~(when editable `(is-edit-lock ~m (Thread/currentThread)))
    ~(when (and readable (not editable)) `(is-edit-lock ~m nil))
    ~(if readable
      `(is (ensure-readable ~m))
      `(is (thrown? IllegalAccessError (ensure-readable ~m))))

    ~(if editable
      `(is (ensure-editable ~m))
      `(is (thrown? IllegalAccessError (ensure-editable ~m))))))

(deftest editable-single-thread-test
  (let [m (make-matrix 4 3)]
    (is-edit-lock m nil)
    (is (ensure-readable m))
    (is (thrown? IllegalAccessError (ensure-editable m)))
    (has-editable-flags m {:readable true :editable false}))
  (let [m (-> (make-matrix 4 3) transient)]
    (has-editable-flags m {:readable true :editable true}))
  (let [m (-> (make-matrix 4 3) transient (assign! 1.0) persistent!)]
    (has-editable-flags m {:readable true :editable false}))

  (let [m (make-matrix 4 3)
        pm (persistent! m)]
    (has-editable-flags m {:readable true :editable false})
    (has-editable-flags pm {:readable true :editable false}))

  (let [m (make-matrix 4 3)
        tm (transient m)]
    (has-editable-flags m {:readable true :editable false})
    (has-editable-flags tm {:readable true :editable true}))

  (let [tm (transient (make-matrix 4 3))
        ttm (transient tm)]
    (has-editable-flags tm {:readable true :editable true})
    (has-editable-flags ttm {:readable true :editable true})))

(deftest editable-multi-thread-test
  (is true))

;;; Tests for Matrix protocol
;; Use extenders function to test every extender of the protocol
(deftest assign!-test
  (is
   (let [m (make-matrix 4 3)
         assigned-m
         (-> m
             transient
             (assign! 1.0)
             persistent!)]
     (every? #(= %1 1.0) (flatten (seq assigned-m))))
   "assign! should set every element of the matrix to the specified value"))

;;; Tests for Matrix1D protocol

;;; Tests for Matrix2D protocol
