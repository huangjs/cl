(defpackage :poker
  (:use :common-lisp :lisp-unit)
  )

(in-package :poker)

(define-test three-of-a-kind
  (assert-equal 'three-of-a-kind
                (get-hand-rank '((k h) (k c) (k d) (2 s) (3 d))))
  (assert-equal 'three-of-a-kind
                (get-hand-rank '((2 s) (k h) (3 d) (k c) (k d))))
  )
