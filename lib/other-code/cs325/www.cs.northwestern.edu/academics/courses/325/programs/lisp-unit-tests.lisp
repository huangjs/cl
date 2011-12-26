;;;-*- Mode: Lisp; Package: LISP-UNIT -*-

#|
Copyright (c) 2005 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; Code to test lisp-unit.lisp, without using lisp-unit.
;;;
;;; Do NOT COMPILE this file. The assert-expands with the
;;; macrolet won't work if you do.

;;; Author: Chris Riesbeck
;;; 
;;; Update history:
;;;
;;; 12/30/05 renamed ASSERT-PREDICATE to ASSERT-EQUALITY [CKR]
;;; 12/29/05 added tests for ASSERT-EQ, ASSERT-EQL, ASSERT-EQUALP [CKR]
;;; 11/07/05 added tests for ASSERT-PREDICATE [DFB]
;;; 09/18/05 replaced Academic Free License with MIT Licence [CKR]
;;; 08/30/05 added license notice [CKR]
;;; 06/28/05 final code released

(defpackage #:lisp-unit-tests
  (:use #:common-lisp #:lisp-unit)
  (:export #:test-lisp-unit)
  )

(in-package #:lisp-unit-tests)

(defvar *failure-count* 0)

;;; To avoid success by circularity, don't use
;;; lisp-unit to test itself.

(defun show-result (passed fail-msg args)
  (cond (passed
         (format t " -- OK"))
        (t
         (format t fail-msg args)
         (incf *failure-count*))))

(defun pass-listener (passed &rest args)
  (show-result passed  "-- did not pass as expected; data:~{ ~S~}" args))

(defun fail-listener (passed &rest args)
  (show-result (not passed)  "-- did not fail as expected; data:~{ ~S~}" args))

(defmacro test-pass (assert-form)
  `(with-test-listener pass-listener     
     (format t "~&~S" ',assert-form)
     ,assert-form
     ))

(defmacro test-fail (assert-form)
  `(with-test-listener fail-listener     
     (format t "~&~S" ',assert-form)
     ,assert-form
     ))

(defun test-lisp-unit ()
  (let ((*failure-count* 0))
    (flet ((foo-print () (princ "foo")))
      (macrolet ((foo-macro () '(foo)))
      
        (test-pass (assert-true (= 5 (+ 2 3))))
        (test-pass (assert-false (= 4 (+ 2 3))))
        (test-pass (assert-expands '(foo) (foo-macro)))
        (test-pass (assert-error 'error (foo)))
        (test-pass (assert-prints "foo" (foo-print)))
        (test-pass (assert-eq 'a 'a))
        (test-pass (assert-eql 1.2 1.2))
        (test-pass (assert-equal 5 (+ 2 3)))
        (test-pass (assert-equal (values 3 2) (floor 11 3)))
        (test-pass (assert-equal 3 (floor 11 3)))
        (test-pass (assert-equalp #2a((1 2) (3 4)) #2a((1 2) (3 4))))
        (test-pass (assert-equality #'set-equal '(1 2 3) (list 1 3 2)))
        
        (test-fail (assert-true (= 4 (+ 2 3))))
        (test-fail (assert-false (= 5 (+ 2 3))))
        (test-fail (assert-eq 1.2 1.2))
        (test-fail (assert-eql (list 1 2) (list 1 2)))
        (test-fail (assert-equal (values 1 2) (values 1 3)))
        (test-fail (assert-equal #2a((1 2) (3 4)) #2a((1 2) (3 4))))
        (test-fail (assert-expands '(baz) (foo)))
        (test-fail (assert-error 'error (+ 1 2)))
        (test-fail (assert-prints "foo" (+ 2 3)))
        (test-fail (assert-equality #'set-equal '(1 2 3) (list 1 3)))
        ))
    
    (let* ((*package* (make-package (gentemp))))
      (eval `(define-test foo nil))
      
      (test-pass (assert-equal '(foo) (get-tests *package*))))
    
    (format t "~%Number of failing tests: ~D~%" *failure-count*)
    
    ))

