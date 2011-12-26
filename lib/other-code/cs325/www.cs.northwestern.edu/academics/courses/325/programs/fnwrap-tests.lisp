(defpackage #:fnwrap-tests
  (:use #:common-lisp #:lisp-unit #:fnwrap)
  )

(in-package #:fnwrap-tests)


;;; Tests to make sure wrapping produces the
;;; correct behavior

(define-test fnwrap-effects
  (let ((fn-name (gensym)))
    (setf (symbol-function fn-name)
      (lambda (x) (* x x)))
    (assert-equal 9 (funcall fn-name 3))
    
    ;; wrapper replaces old code
    (set-wrapper fn-name (lambda (fn x) (+ x x)))
    (assert-equal 6 (funcall fn-name 3))
    
    ;; wrapper modifies return value of old code
    (set-wrapper fn-name 
                 (lambda (fn x) 
                   (+ 8 (funcall fn x))))
    (assert-equal 17 (funcall fn-name 3))    
    
    ;; wrapper tests arguments
    (set-wrapper fn-name 
                 (lambda (fn x)
                   (if (oddp x) 0 (funcall fn x))))
    (assert-equal 0 (funcall fn-name 3))
    (assert-equal 16 (funcall fn-name 4))
    
    ;; unwrap restores original code
    (remove-wrapper fn-name)
    (assert-equal 9 (funcall fn-name 3))
    ))

;;; Tests to make sure wrapping, unwrapping,
;;; and redefining interact correctly

(define-test fnwrap-wrap-unwrap
  (let ((fn-name (gensym))
          (fn-2 (lambda (x) (+ x x)))
          (fn (lambda (x) (* x x)))
          (wrapper (lambda (fn x) (list 'wrapper x)))
          (wrapper-2 (lambda (fn x) (list 'wrapper-2 x)))
          )
      
      ;; After wrapping, the function should be changed
      (setf (symbol-function fn-name) fn)
      (assert-equal fn (symbol-function fn-name))
      (set-wrapper fn-name wrapper)
      (assert-false (equal fn (symbol-function fn-name)))
      
      ;; After unwrapping, it should be itself again
      (remove-wrapper fn-name)
      (assert-equal fn (symbol-function fn-name))
      
      ;; Wrapping a wrapped function should have the new
      ;; wrapper, and only one wrapper needs to be removed.
      (set-wrapper fn-name wrapper)
      (let ((stored (symbol-function fn-name)))
        (assert-false (equal fn stored))
        (set-wrapper fn-name wrapper-2)
        (assert-false (equal stored (symbol-function fn-name)))
        (remove-wrapper fn-name)
        (assert-equal fn (symbol-function fn-name)))
       
      ;; Unwrapping should not affect a function redefined
      ;; since wrapping
      (set-wrapper fn-name wrapper)
      (assert-false (equal fn (symbol-function fn-name)))
      (setf (symbol-function fn-name) fn-2)
      (remove-wrapper fn-name nil)
      (assert-equal fn-2 (symbol-function fn-name))
      ))