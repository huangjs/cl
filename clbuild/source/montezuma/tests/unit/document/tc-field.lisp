(in-package #:montezuma)

(deftestfun test-standard-field
  (let ((f (make-field "name" "value" :stored :compress :index :tokenized)))
    (test standard-field-1 (field-name f) "name")
    (test standard-field-2 (field-data f) "value")
    (test standard-field-3 (field-stored-p f)     T)
    (test standard-field-4 (field-compressed-p f) T)
    (test standard-field-5 (field-indexed-p f)    T)
    (test standard-field-6 (field-tokenized-p f)  T)
    (test standard-field-7 (field-store-term-vector-p f)  NIL)
    (test standard-field-8 (field-store-offsets-p f)  NIL)
    (test standard-field-9 (field-store-positions-p f)  NIL)
    (test standard-field-10 (field-omit-norms-p f)  NIL)
    (test standard-field-11 (field-binary-p f)  NIL)))

(deftestfun test-field-set-store
  (let ((f (make-field "name" nil :stored :compress :index :tokenized)))
    (setf (field-stored f) NIL)
    (test set-store-1 (field-stored-p f) NIL)
    (test set-store-2 (field-compressed-p f) NIL)))


(deftestfun test-field-set-index
  (let ((f (make-field "name" "value" :stored :compress :index :tokenized)))
    (setf (field-index f) NIL)
    (test set-index-1 (field-indexed-p f) NIL)
    (test set-index-2 (field-tokenized-p f) NIL)
    (test set-index-3 (field-omit-norms-p f) NIL)
    (setf (field-index f) :no-norms)
    (test set-index-4 (field-indexed-p f) T)
    (test set-index-5 (field-tokenized-p f) NIL)
    (test set-index-6 (field-omit-norms-p f) T)))

(deftestfun test-set-field-term-vector
  (let ((f (make-field "name" "value" :stored :compress :index :tokenized)))
    (setf (field-store-term-vector f) :with-positions-offsets)
    (test set-term-vector-1 (field-store-term-vector-p f) T)
    (test set-term-vector-2 (field-store-offsets-p f) T)
    (test set-term-vector-3 (field-store-positions-p f) T)))

(deftestfun test-field-new-binary-field
  (let ((bin (make-array (list 256) :element-type '(unsigned-byte 8))))
    (dotimes (i 256)
      (setf (aref bin i) i))
    (let ((f (make-binary-field "name" bin T)))
      (test new-binary-field-1 (field-name f) "name" #'string=)
      (test new-binary-field-2 (field-data f) bin)
      (test new-binary-field-3 (field-stored-p f) T)
      (test new-binary-field-4 (field-compressed-p f) NIL)
      (test new-binary-field-5 (field-indexed-p f) NIL)
      (test new-binary-field-6 (field-store-term-vector-p f) NIL)
      (test new-binary-field-7 (field-store-offsets-p f) NIL)
      (test new-binary-field-8 (field-store-positions-p f) NIL)
      (test new-binary-field-9 (field-omit-norms-p f) NIL)
      (test new-binary-field-10 (field-binary-p f) T))))


