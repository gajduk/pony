;; ## A thin wrapper around ParallelColt
;; Pony is a wrapper around the ParallelColt library
;; that is intended to be as close as possible to the original library
;; (to allow full usage of it) while being as close to Clojure idioms
;; as possible.
;; TODO: Need to figure out how to use/handle views.

(ns pony.core
  (:import
   [clojure.lang Counted
    IPersistentCollection IEditableCollection ITransientCollection]
   [cern.colt.matrix AbstractMatrix AbstractMatrix1D AbstractMatrix2D]
   [cern.colt.matrix.tdouble DoubleMatrix2D]
   [cern.colt.matrix.tdouble.impl
    DenseDoubleMatrix1D DenseDoubleMatrix2D
    SparseDoubleMatrix1D SparseDoubleMatrix2D]
   [cern.colt.matrix.tfloat FloatMatrix2D]
   [cern.colt.matrix.tfloat.impl DenseFloatMatrix2D]))

;; ## IEditable is a protocol for mutable data structures
;; IEditables are immutable, but locked mutable copies can be made of
;; them. These mutable copies can only be read from or written to by
;; the thread that created them.
;; IEditable can be locked to a particular thread by calling
;; copy-for-current-thread. Read/write operations can then be done on
;; it. It can be unlocked by calling make-uneditable. Write operations
;; then will fail and read operations will only pass if the IEditable
;; is not writeable by anyone else. All read or write operations must
;; be guarded by ensure-readable and ensure-editable, respectively.
(defprotocol IEditable
  (make-uneditable [this]
    "Release the object for editing by setting edit-thread atomically
  to nil")
  (ensure-editable [this]
    "All write operations should ensure-editable: that the edit-thread
  is set to current thread, so no one else is reading/writing.")
  (ensure-readable [this]
    "All read operations should ensure-readaoble: that edit-thread is
  set to nil or to current thread, so there are no phantom reads.")
  (copy [this] "Make a deep copy and create a new lock set to nil")
  (copy-for-current-thread [this]
    "Make a deep copy and create a new lock set to current thread"))

(defmacro make-editable-type-fns
  "Builds unlock and check-lock functions for IEditable"
  [edit-lock-sym]
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
               "make-uneditable called by non-owner thread"))))))
      this#)
    :ensure-editable
    (fn [this#]
      (condp = @(. this# ~edit-lock-sym)
        (Thread/currentThread) true
        nil
        (throw (IllegalAccessError.
                (format "Transient method called %s or %s"
                        "on transient after call to persistent!"
                        "on persistent data")))
        (throw (IllegalAccessError.
                "Transient used by non-owner thread"))))
    :ensure-readable
    (fn [this#]
      (condp = @(. this# ~edit-lock-sym)
        (Thread/currentThread) true
        nil true
        (throw (IllegalAccessError.
                "Transient is not readable"))))})

(defmacro def-editable-type
  "Builds code to define an editable type that locks the specified
  member, implementing all required interfaces and allowing the caller
  to specific any other desired interfaces. Type-specific functions
  for IPersistentCollection must also be provided. Additional
  protocols to extend this type are specified in the body."
  [name locked-member
   persistent-col-fns
   ifaces & body]
  `(do
     (deftype ~name [~locked-member ~'edit-lock]
       IEditableCollection
       (asTransient [this#]
         (copy-for-current-thread this#))
       ITransientCollection
       (persistent [this#]
         (make-uneditable this#))
       ;; conj is no-op
       (conj [this# _#] this#)
       IPersistentCollection
       ~@persistent-col-fns
       ~@ifaces)
     (extend
         ~name IEditable
         (merge
          (make-editable-type-fns ~'edit-lock)
          ;; Generate functions to perform deep copies of editable
          ;; type, including a function that performs a deep copy and
          ;; then sets the edit thread to the current thread. This is
          ;; how mutable copies of the editable type are created.
          (letfn
              [(copy-for-thread# [editable# thread#]
                 (let [locked-member-deep-copy# (deep-copy editable#)]
                   (new ~name locked-member-deep-copy#
                        (atom thread#))))]
            {:copy (fn [editable#] (copy-for-thread# editable# nil))
             :copy-for-current-thread
             (fn [editable#] (copy-for-thread# editable# (Thread/currentThread)))})))
     ~(when body `(extend-type ~name ~@body))
     ~name))

(defmacro with-editable [o & body]
  `(do (ensure-editable ~o)
       ~@body))
(defmacro with-readable [o & body]
  `(do (ensure-readable ~o)
       ~@body))

(defn type-hinted-param [cls x]
  (with-meta x {:tag cls}))

(defprotocol Matrix
  "Matrices,dense or sparse and float or double and 1D, 2D or 3D."
  (assign! [this value] "Assign value to all cells in matrix")
  (to-string [this] "Print the matrix")
  (as-array [this] "Copy the matrix into an array")
  (deep-copy [this] "Make a deep copy of the underlying matrix")
  (arg-matmax [this])
  (normalize! [this]))

(def matrix-impl
  {:assign!
   (fn [this value]
     (with-editable this
       (.assign (.m this) value)
       this))
   :to-string
   (fn [this]
     (with-readable this
       (.toString (.m this))))
   :deep-copy
   (fn [this]
     (with-readable this
       (.copy (.m this))))
   :arg-matmax
   (fn [this]
     (with-readable this
       (let [[max & coords] (.getMaxLocation (.m this))]
         [max (map long coords)])))})

(defprotocol Matrix1D
  (dot-prod [this ^Matrix1D that])
  (sum [this]))

(def-editable-type PonyMatrix1D ^AbstractMatrix1D m
  [;; cons is no-op
   (cons [this o]
         (with-readable this
           this))
   (empty [_] nil)
   (equiv [this o]
          (with-readable this
            (let [matrix (.m this)]
              (when (instance? AbstractMatrix1D o)
                (with-readable o
                  (and (= (.size matrix) (.size o))
                       (every?
                        (fn [i]
                          (= (.index matrix i) (.index o i)))
                        (range (.size matrix)))))))))]
  [Counted
   (count [this] (with-readable this (.size (.m this))))
   clojure.lang.Seqable
   (seq [this] (seq (as-array this)))]
  Matrix1D
  (dot-prod [this that] (with-readable (.zDotProduct (.m this) that)))
  (sum [this] (with-readable (.zSum (.m this)))))
(extend PonyMatrix1D Matrix
        (merge
         {:as-array
          (fn [this]
            (with-readable this
              (-> this .m .toArray)))}
         matrix-impl))

;; (defprotocol Matrix2D [])
(def-editable-type PonyMatrix2D ^AbstractMatrix2D m
  [ ;; cons is no-op
   (cons [this o] (with-readable this this))
   (empty [_] nil)
   (equiv [this o]
          (with-readable this
            (let [matrix (.m this)]
              (when (instance? AbstractMatrix2D o)
                (with-readable o
                  (and (= (.rows matrix) (.rows o))
                       (= (.columns matrix) (.columns o))
                       (every?
                        (fn [[r c]]
                          (= (.get matrix r c)
                             (.get o r c)))
                        (for [r (range (.rows matrix))
                              c (range (.columns matrix))]
                          [r c]))))))))]
  [Counted
   (count [this]
          (let [matrix (.m this)
                rows (.rows matrix) cols (.columns matrix)]
            (* rows cols)))
   clojure.lang.Seqable
   (seq [this] (map seq (seq (as-array this))))])
(extend PonyMatrix2D Matrix
        (merge
         {:as-array
          (fn [this]
            (with-readable this
              (-> this .m .toArray)))}
         matrix-impl))

(defprotocol ColtMatrix
  (wrap-colt-matrix [^AbstractMatrix colt-matrix]
    "Wrap colt matrix in a pony matrix"))

(extend-type AbstractMatrix1D
  ColtMatrix
  (wrap-colt-matrix [colt-matrix]
    (PonyMatrix1D. colt-matrix (atom nil))))

(extend-type AbstractMatrix2D
  ColtMatrix
  (wrap-colt-matrix [colt-matrix]
    (PonyMatrix2D. colt-matrix (atom nil))))

;;; Matrix constructor functions
(defn make-dense-matrix
  ([^long size] (-> size DenseDoubleMatrix1D. wrap-colt-matrix))
  ([^long rows ^long cols]
     (-> (DenseDoubleMatrix2D. rows cols) wrap-colt-matrix)))

(defn make-sparse-matrix
  ([^long size] (-> size SparseDoubleMatrix1D. wrap-colt-matrix))
  ([^long rows ^long cols]
     (-> (SparseDoubleMatrix2D. rows cols) wrap-colt-matrix)))

(set! *warn-on-reflection* true)
