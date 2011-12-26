(in-package :glist-tests)

;;; Test cases for the GLIST exercises.
;;;
;;; Updates:
;;; 01/16/06 File created [CKR]

;;; The tests

(define-test gnth
  (assert-equal 1 (gnth 0 (gintegers 1)))
  (assert-equal 5 (gnth 4 (gintegers 1)))
  (assert-equal nil (gnth 10 (delay '(a b c))))
  )
  
(define-test gmap
  (assert-equal '(2 3 4 5) (force (gmap #'1+ (delay '(1 2 3 4))) 4))
  (assert-equal '(2 3 4 5) (force (gmap #'1+ (gintegers 1)) 4))
  )

(define-test gfilter
  (assert-equal '(1 3 5) (force (gfilter #'oddp (delay '(1 2 3 4 5))) 4))
  (assert-equal '(1 3 5 7) (force (gfilter #'oddp (gintegers 1)) 4))
  (assert-equal nil (force (gfilter #'oddp (delay '(2 4 6 8 10 12 14))) 4))
  )

