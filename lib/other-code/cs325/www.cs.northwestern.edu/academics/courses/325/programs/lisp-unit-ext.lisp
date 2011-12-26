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

;;; Update history:
;;;
;;; 09/18/05 added MIT Licence [CKR]

(in-package :lisp-unit)

(defmacro with-failure-tests (tests &rest body)
  `(with-test-listener
       (lambda (passed type name form expected actual extras test-count pass-count)
         (with-test-listener default-listener
           (unless passed
             (show-failure type (get-failure-message type)
                           name form expected actual extras)
             (run-tests ,@tests))))
     ,@body))

(in-package :cs325-user)

(define-test get-mode  ;; mode = most common value in list
  (with-failure-tests (get-counts max-count)
    (assert-equal nil (get-mode nil))
    (assert-equal 2 (get-mode '(2)))
    (assert-equal 2 (get-mode '(1 2 1 3 2)))
    (assert-equal -1 (get-mode '(1 -1 3 -1 2)))
    ))

(define-test get-counts
  (assert-equal nil (get-counts nil))
  (assert-equal '((a . 1)) (get-counts '(a)))
  (assert-equal '((a . 3)) (get-counts '(a a a)))
  (assert-equal '((c . 2) (b . 1) (a . 2)) (get-counts '(a b a c c)))
  )

(define-test max-count
  (assert-equal 'nil (max-count nil))
  (assert-equal '(a . 1) (max-count '((a . 1))))
  (assert-equal '(c . 3) (max-count '((a . 2) (b . 1) (c . 3))))
  )

(defun get-mode (l)
  (car (max-count (get-counts l))))

(defun get-counts (l)
  (reduce #'(lambda (counts x)
              (if (assoc x counts) counts
                (cons (cons x (count x l)) counts)))
          l :initial-value nil))

(defun max-count (counts)
  (reduce #'(lambda (result count)
              (if (> (cdr count) (cdr result)) count result))
          counts :initial-value '(dummy . 0)))