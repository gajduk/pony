;; ## A thin wrapper around ParallelColt
;; Pony is a wrapper around the ParallelColt library
;; that is intended to be as close as possible to the original library
;; (to allow full usage of it) while being as close to Clojure as possible.

(ns pony.core
  (:import [cern.colt.matrix AbstractMatrix AbstractMatrix2D]
           [cern.colt.matrix.tdouble DoubleMatrix2D]
           [cern.colt.matrix.tdouble.impl DenseDoubleMatrix2D]
           [cern.colt.matrix.tfloat FloatMatrix2D]
           [cern.colt.matrix.tfloat.impl DenseFloatMatrix2D]))

(defn zeros [^long rows ^long cols]
  (DenseDoubleMatrix2D. rows cols))

(defn ones [^long rows ^long cols]
  (.assign (DenseDoubleMatrix2D. rows cols) 1.0))

(defn row-view [^AbstractMatrix2D m ^long row]
  (.viewRow m row))

(defn assign [^AbstractMatrix2D m vals]
  {:pre [(let [rows (.rows m) cols (.columns m)]
           (= (* rows cols) (count vals)))]}
  (let [rows (.rows m) cols (.columns m)
        vss (partition cols vals)]
    (dorun
     (map
      (fn [r vs]
        (.assign
         (row-view m r)
         (double-array vs)))
      (range rows) vss))
    m))

;; ## Reconciling the difference between a mutable Java library and an
;; ## immutable Clojure style?
;; How exactly can a Clojure-like wrapper be built when ParallelColt
;; matrices are made up of operations that are meant to destructively
;; modify the operand? Ideally, the matrix would be implemented as a
;; persistent collection with transient support for performing a chain
;; of operations. A Clojure group thread called "How to add a new
;; type of collection?" talks about why it is not possible to do this
;; in Clojure right now.

;; These are just thin wrappers around ParallelColt functions.
(defn copy [^AbstractMatrix m] (.copy m))

(defn arg-matmin [^AbstractMatrix m]
  (let [[min & coords] (.getMinLocation m)]
    [min (map long coords)]))
(defn matmin [^AbstractMatrix m]
  (first (.getMinLocation m)))

(defn arg-matmax [^AbstractMatrix m]
  (let [[max & coords] (.getMaxLocation m)]
    [max (map long coords)]))
(defn matmax [^AbstractMatrix m]
  (first (.getMaxLocation m)))

(defn trans [^AbstractMatrix2D m]
  (.viewDice m))

;;  These macros copy the ParallelColt matrix m, do the equivalent of
;;  -> or ->> with it and return the result. They try to approximate
;;  the (do (transient) (do ops) (persistent!)) semantics of transient
;;  collections.
(defmacro mat->
  [^AbstractMatrix2D m ops]
  (let [copy-m (gensym)]
    `(let [~copy-m (.copy ~m)]
       (-> ~copy-m ~@ops))))
(defmacro mat->>
  [^AbstractMatrix2D m ops]
  (let [copy-m (gensym)]
    `(let [~copy-m (.copy ~m)]
       (->> ~copy-m ~@ops))))

;; These macros are equivalent to the ones above, except no copy is
;; done and hence they are not safe.
(defmacro mat->!
  [^AbstractMatrix2D m ops]
  `(-> ~m ~@ops))
(defmacro mat->>!
  [^AbstractMatrix2D m ops]
  `(->> ~m ~@ops))

;; ## Being conscious of speed
;; Reflection adds overhead that we don't really need, since we
;; already know the types of the matrix elements. We can get quite far
;; by avoiding reflection with typehints and simple functions.
(set! *warn-on-reflection* true)
