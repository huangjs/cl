(defpackage #:glist-tests
  (:use #:common-lisp #:lisp-unit #:glist)
  )

(in-package :glist-tests)

;;; Examples of how to use the GLIST generated lists
;;; package.
;;;
;;; Updates:
;;; 01/18/05 Updated tests for new lisp-unit [CKR]
;;; 01/15/05 Added FORCE utility [CKR]
;;; 01/15/05 Replaced printing code with unit tests [CKR]
;;; 05/03/04 ReplaCed GPOP with GLIST:GPOP in SILLY-EXAMPLE [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test a list generator made from a list.

(define-test delay
  (assert-equal '(a b c) (force (delay '(a b c))))
  (assert-true (null (force (delay nil))))
  )

;;; Test a list generator that "flattens" a list.

(define-test gflatten
  (assert-equal '(a b c d e f) (force (gflatten '(((a b) c d) e f))))
  (assert-true (null (force (gflatten nil))))
  )

;;; Test a list generator that generates an infinite list of integers.

(define-test gintegers
  (assert-equal '(0 1 2 3 4) (force (gintegers) 5))
  (assert-equal '(3 4 5 6 7) (force (gintegers 3) 5))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (FORCE glist [n]) => list
;;;   Returns a list of the first M elements of glist, where M
;;;   is the minimum of N and the length of glist. If N is nil,
;;;   returns all the elements of glist.

(defun force (gl &optional n)
  (do ((l nil (cons (gpop gl) l)))
      ((or (gnull gl)
           (and (not (null n))
                (= -1 (decf n))))
       (nreverse l))))

;;; (GFLATTEN x) => list generator
;;;   Returns a list generator that will return
;;;   the atoms from a nested list one at a time. 

(defun gflatten (x)
  (cond ((null x) nil)
        ((atom x) (list x))
        (t (glist:gappend (gflatten (car x))
                          (gflatten (cdr x))))))

;;; (GINTEGERS n) => generated list
;;;   returns a generated list for all integers,
;;;   starting at n.

(defun gintegers (&optional (n 0))
  (glist:gcons n (gintegers (1+ n))))
