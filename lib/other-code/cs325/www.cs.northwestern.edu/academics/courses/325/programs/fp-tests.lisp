
(defpackage #:fp-tests
  (:use #:common-lisp #:lisp-unit #:fp)
  )

(in-package #:fp-tests)

(define-test fp-unary
  (assert-equal 3 (funcall (fp 1+ 1+ 1+) 0))
  (assert-equal 1 (funcall (fp 1+ car) '(0 1)))
  (assert-equal 2 (funcall (fp) 2))
  )

(define-test fp-binary
  (assert-true (funcall (fp (> 0)) 1))
  (assert-false (funcall (fp not (> 0)) 1))
  (assert-equal '(t t nil)
                (mapcar (fp (> 0) car) '((1 a) (2 b) (-1 c))))
  (assert-equal '(nil nil t)
                (mapcar (fp not (> 0) car) '((1 a) (2 b) (-1 c))))

  (assert-equal '(t t nil)
                (let ((x 4))
                  (mapcar (fp not (> x) car)
                          '((1 a) (2 b) (6 c)))))
  )
    
  
