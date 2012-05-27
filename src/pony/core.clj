;; ## A thin wrapper around ParallelColt
;; Pony is a wrapper around the ParallelColt library
;; that is intended to be as close as possible to the original library
;; (to allow full usage of it) while being as close to Clojure as possible.

(ns pony.core
  (:import
   [clojure.lang IPersistentCollection IEditableCollection ITransientCollection]
   [cern.colt.matrix AbstractMatrix AbstractMatrix2D]
   [cern.colt.matrix.tdouble DoubleMatrix2D]
   [cern.colt.matrix.tdouble.impl DenseDoubleMatrix2D]
   [cern.colt.matrix.tfloat FloatMatrix2D]
   [cern.colt.matrix.tfloat.impl DenseFloatMatrix2D]))

(defn make-matrix [^long rows ^long cols]
  (let [edit (atom nil)
        ensure-editable
        (fn []
          (condp = @edit
            (Thread/currentThread) true
            nil
            (throw (IllegalAccessError.
                    "Transient used after persistent! call"))
            (throw (IllegalAccessError.
                    "Transient used by non-owner thread"))))]
    [(proxy [DenseDoubleMatrix2D
             IPersistentCollection
             IEditableCollection
             ITransientCollection]
         [rows cols]
       (persistent []
         (do (swap! edit (fn [_] nil))
             this))
       ;; conj is no-op
       (conj [] this)
       (asTransient []
         (do (swap! edit (fn [_] (Thread/currentThread)))
             this))
       (count []
         (let [rows (.rows this) cols (.columns this)]
           (* rows cols)))
       ;; cons is no-op
       (cons [o] this)
       (empty [] nil)
       (equiv [o]
         (when (instance? AbstractMatrix2D o)
           (and (= (.rows this) (.rows o))
                (= (.columns this) (.columns o))
                (every?
                 (fn [[r c]]
                   (= (.get this r c)
                      (.get o r c)))
                 (for [r (range (.rows this))
                       c (range (.columns this))]
                   [r c]))))))
     ensure-editable]))

(let [[m ensure-editable] (make-matrix 4 3)]
  (do
    (println (ensure-editable))
    (.asTransient m)
    (println (ensure-editable))
    (.persistent m)
    (println (ensure-editable))))

(defprotocol IMatrix
  "Matrices,dense or sparse and float or double and 1D, 2D or 3D."
  (zeros [dims] "Return a matrix of the same type, filled with zeros")
  (ones [dims] "Return a matrix of the same type, filled with ones")
  )

;; ## A deftype-based solution
;; Which protocols should be implemented?
;; - IPersistentMap
;; - ITransientCollection
;; Look at (ancestors (class {}))
;; The best thing to do would be to implement (transient!) and
;; (persistent) such that when the matrix is transient,
;; modification operations are allowed and when it is persistent,
;; those modification operations are not available.

;; ## Functions for creating matrices
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
