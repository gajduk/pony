;; ## A thin wrapper around ParallelColt
;; Pony is a wrapper around the ParallelColt library
;; that is intended to be as close as possible to the original library
;; (to allow full usage of it) while being as close to Clojure as possible.

(ns pony.core
  (:import
   [clojure.lang Counted IPersistentCollection IEditableCollection ITransientCollection]
   [cern.colt.matrix AbstractMatrix AbstractMatrix1D AbstractMatrix2D]
   [cern.colt.matrix.tdouble DoubleMatrix2D]
   [cern.colt.matrix.tdouble.impl DenseDoubleMatrix1D DenseDoubleMatrix2D]
   [cern.colt.matrix.tfloat FloatMatrix2D]
   [cern.colt.matrix.tfloat.impl DenseFloatMatrix2D]))

;; TODO: Make a protocol for matrices
;; TODO: Reify the output of make-matrix into a type that satifies the
;; TODO: protocol for matrices.

(defprotocol IEditable
  (make-uneditable [this])
  (make-editable [this])
  (ensure-editable [this]))

(defmacro make-editable-type-fns [edit-lock-sym]
  `{:make-uneditable
    (fn [this#]
      (swap!
       (. this# ~edit-lock-sym)
       (fn [owner-thread#]
         (let [curr-thread# (Thread/currentThread)]
           (condp = owner-thread#
             curr-thread# nil
             nil nil
             (throw
              (IllegalAccessError.
               "make-uneditable called by non-owner thread")))))))
    :make-editable
    (fn [this#]
      (swap!
       (. this# ~edit-lock-sym)
       (fn [owner-thread#]
         (let [curr-thread# (Thread/currentThread)]
           (condp = owner-thread#
             curr-thread# curr-thread#
             nil curr-thread#
             (throw (IllegalAccessError.
                     "Transient used by non-owner thread")))))))
    :ensure-editable
    (fn [this#]
      (condp = @(. this# ~edit-lock-sym)
        (Thread/currentThread) true
        nil
        (throw (IllegalAccessError.
                "Transient method called on persistent matrix"))
        (throw (IllegalAccessError.
                "Transient used by non-owner thread"))))})

(declare make-matrix-from-matrix)

(defmacro def-lockable-type
  [name locked-member persistent-col-fns ifaces & body]
  `(do
     (deftype ~name [~locked-member edit-lock#]
       IEditableCollection
       (asTransient [this#]
         (do (make-editable this#)
             this#))
       ITransientCollection
       (persistent [this#]
         (do (make-uneditable this#)
             this#))
       ;; conj is no-op
       (conj [this# _#] this#)
       IPersistentCollection
       ~@persistent-col-fns
       ~@ifaces)
     (extend ~name IEditable
             (make-editable-type-fns edit-lock#))
     ~(when body `(extend-type ~name ~@body))
     ~name))

(defn type-hinted-param [cls x]
  (with-meta x {:tag cls}))

(defprotocol Matrix
  "Matrices,dense or sparse and float or double and 1D, 2D or 3D."
  (assign [this value] "Assign value to all cells in matrix")
  (to-string [this] "Print the matrix")
  (copy [this] "Make a deep copy of the matrix")
  (arg-matmax [this]))

(def matrix-impl
  {:assign
   (fn [this value]
     (do (ensure-editable this)
         (.assign (.m this) value)
         this))
   :to-string
   (fn [this] (.toString (.m this)))
   :copy
   (fn [this] (.copy (.m this)))
   :arg-matmax
   (fn [this]
     (let [[max & coords] (.getMaxLocation (.m this))]
       [max (map long coords)]))})

(defprotocol Matrix1D
  (dot-prod [this ^Matrix1D that])
  (sum [this]))

(def-lockable-type PonyMatrix1D m
  [;; cons is no-op
   (cons [this o] this)
   (empty [_] nil)
   (equiv [this o]
          (let [matrix (.m this)]
            (when (instance? AbstractMatrix1D o)
              (and (= (.size matrix) (.size o))
                   (every?
                    (fn [i]
                      (= (.index matrix i) (.index o i)))
                    (range (.size matrix)))))))]
  [Counted (count [this] (.size (.m this)))]
  Matrix1D
  (dot-prod [this that] (.zDotProduct (.m this) that))
  (sum [this] (.zSum (.m this))))
(extend PonyMatrix1D Matrix matrix-impl)

;; (defprotocol Matrix2D [])
(def-lockable-type PonyMatrix2D m
  [ ;; cons is no-op
   (cons [this o] this)
   (empty [_] nil)
   (equiv [this o]
          (let [matrix (.m this)]
            (when (instance? AbstractMatrix2D o)
              (and (= (.rows matrix) (.rows o))
                   (= (.columns matrix) (.columns o))
                   (every?
                    (fn [[r c]]
                      (= (.get matrix r c)
                         (.get o r c)))
                    (for [r (range (.rows matrix))
                          c (range (.columns matrix))]
                      [r c]))))))]
  [Counted
   (count [this]
          (let [matrix (.m this)
                rows (.rows matrix) cols (.columns matrix)]
            (* rows cols)))])
(extend PonyMatrix2D Matrix matrix-impl)

(defn make-matrix-from-matrix [^AbstractMatrix2D m]
  (PonyMatrix2D. m (atom nil)))

(defn make-matrix [^long rows ^long cols]
  (let [m (DenseDoubleMatrix2D. rows cols)]
    (make-matrix-from-matrix m)))

(set! *warn-on-reflection* true)

(let [m (make-matrix 4 3)
      mc (make-matrix-from-matrix  (copy m))]
  (-> m
      transient
      (assign 1.0)
      persistent!)
  (println (to-string m))
  (println (to-string mc)))
