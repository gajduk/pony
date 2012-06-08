;; ## A thin wrapper around ParallelColt
;; Pony is a wrapper around the ParallelColt library
;; that is intended to be as close as possible to the original library
;; (to allow full usage of it) while being as close to Clojure as possible.

(ns pony.core
  (:import
   [clojure.lang Counted IPersistentCollection IEditableCollection ITransientCollection]
   [cern.colt.matrix AbstractMatrix AbstractMatrix2D]
   [cern.colt.matrix.tdouble DoubleMatrix2D]
   [cern.colt.matrix.tdouble.impl DenseDoubleMatrix2D]
   [cern.colt.matrix.tfloat FloatMatrix2D]
   [cern.colt.matrix.tfloat.impl DenseFloatMatrix2D]))

;; TODO: Make a protocol for matrices
;; TODO: Reify the output of make-matrix into a type that satifies the
;; TODO: protocol for matrices.

(defprotocol IEditable
  (make-uneditable [this])
  (make-editable [this])
  (ensure-editable [this]))

(defprotocol IMatrix
  "Matrices,dense or sparse and float or double and 1D, 2D or 3D."
  (assign [this value] "Assign value to all cells in matrix")
  (to-string [this] "Print the matrix")
  (copy [this] "Make a deep copy of the matrix")
  (arg-matmax [this]))

(declare make-matrix-from-matrix)

(deftype PonyMatrix2D [m edit-lock]
  IEditable
  (make-uneditable [_]
    (swap! edit-lock
           (fn [owner-thread]
             (let [curr-thread (Thread/currentThread)]
               (condp = @edit-lock
                 curr-thread nil
                 nil nil
                 (throw (IllegalAccessError.
                         "make-uneditable called by non-owner thread")))))))
  (make-editable [_]
    (swap! edit-lock
           (fn [owner-thread]
             (let [curr-thread (Thread/currentThread)]
               (condp = @edit-lock
                 curr-thread curr-thread
                 nil curr-thread
                 (throw (IllegalAccessError.
                         "Transient used by non-owner thread")))))))
  (ensure-editable [_]
    (condp = @edit-lock
      (Thread/currentThread) true
      nil
      (throw (IllegalAccessError.
              "Transient method called on persistent matrix"))
      (throw (IllegalAccessError.
              "Transient used by non-owner thread"))))
  IMatrix
  (assign [this value]
    (do (ensure-editable this)
        (.assign m value)
        this))
  (to-string [_]
    (.toString m))
  (copy [_]
    (make-matrix-from-matrix (.copy m)))
  (arg-matmax [_]
    (let [[max & coords] (.getMaxLocation m)]
      [max (map long coords)]))

  Counted
  (count [_]
    (let [rows (.rows m) cols (.columns m)]
      (* rows cols)))

  IPersistentCollection
  ;; cons is no-op
  (cons [this o] this)
  (empty [_] nil)
  (equiv [_ o]
    (when (instance? AbstractMatrix2D o)
      (and (= (.rows m) (.rows o))
           (= (.columns m) (.columns o))
           (every?
            (fn [[r c]]
              (= (.get m r c)
                 (.get o r c)))
            (for [r (range (.rows m))
                  c (range (.columns m))]
              [r c])))))

  IEditableCollection
  (asTransient [this]
    (do (make-editable this)
        this))

  ITransientCollection
  (persistent [this]
    (do (make-uneditable this)
        this))
  ;; conj is no-op
  (conj [this o] this))

(defn make-matrix-from-matrix [^AbstractMatrix2D m]
  (PonyMatrix2D. m (atom nil)))

(defn make-matrix [^long rows ^long cols]
  (let [m (DenseDoubleMatrix2D. rows cols)]
    (make-matrix-from-matrix m)))

(let [m (make-matrix 4 3)
      mc (copy m)]
  (-> m
      transient
      (assign 1.0)
      persistent!)
  (println (to-string m))
  (println (to-string mc)))

(set! *warn-on-reflection* true)
