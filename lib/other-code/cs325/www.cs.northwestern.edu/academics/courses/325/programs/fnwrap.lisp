;;;-*- Mode: Lisp; Package: FNWRAP -*-

#|
Copyright (c) 2006 Christopher K. Riesbeck

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


;;; A function wrapper package, modelled after FWRAP
;;; Author: Chris Riesbeck
;;; Documentation: http://www.cs.northwestern.edu/academics/courses/325/readings/fnwrap.html
;;; 
;;; Update history:
;;;
;;; 05/10/06 added TRACE-CALL, BREAK-CALL
;;; 03/29/06 first release

#|
(DEFWRAPPER fn-name arg-list . body)           [macro]
  Equivalent to (ADD-WRAPPER fn-name (LAMBDA arg-list . body)).

(UNWRAP-FUNCTIONS fn-name1 fn-name2 ...)        [macro]
  Equivalent to calling REMOVE-WRAPPER on every fn-name.
  If no names are given, unwraps every wrapped function.

(SET-WRAPPER fn-name function)                  [function]
  Wraps function around the global definition of fn-name.

(REMOVE-WRAPPER fn-name [warn=p])                        [function]
  Removes the function wrapped around fn-name, if any.
  If warn-p is true (the default), warnings are given
  if nothing is removed because the function is not 
  wrapped or has been changed since being wrapped.

(BREAK-CALL call-form result-form [test-form])
(TRACE-CALL call-form result-form [test-form])
  => result-form values                         [macro]

  Conditionally prints readable TRACE-like output. 
    - call-form: a function name, or a list (fn-name exp1 exp2 ...)
    - result-form: an expression, usually calling next-function
    - test-form: an expression; default is T
  BREAK-CALL also enters a break loop. TRACE-CALL does not.

  If test-form evaluates to true, PRINT-TRACE prints the function name
  and the values of exp1, exp2, etc., then it evaluates result-form
  and prints the values returned. Printing is done on the trace
  output stream.
|#


(defpackage #:fnwrap
  (:use #:common-lisp)
  (:export #:set-wrapper #:remove-wrapper 
           #:defwrapper #:unwrap-functions
           #:break-call #:trace-call))

(in-package :fnwrap)

(defvar *wrapped-functions* (make-hash-table))
(defvar *wrap-indent* 0)


(defmacro defwrapper (fn-name args &body body)
  `(set-wrapper ',fn-name (lambda ,args ,@body)))

(defmacro unwrap-functions (&rest l)
  `(remove-wrappers ',l))

(defmacro recall (fn-name &rest args)
  `(call-wrapped-function ',fn-name ,@args))

(defun call-wrapped-function (fn-name &rest args)
  (apply (get-wrapped-function fn-name) args))

(defun get-wrapped-function (fn-name)
  (first (gethash fn-name *wrapped-functions*)))

(defun set-wrapper (fn-name new-fn)
  (remove-wrapper fn-name nil)
  (let* ((old-fn (symbol-function fn-name))
         (wrap-fn (lambda (&rest l) (apply new-fn old-fn l))))
    (setf (symbol-function fn-name) wrap-fn)
    (setf (gethash fn-name *wrapped-functions*)
      (list old-fn wrap-fn))))

(defun get-wrapped-functions ()
  (let ((l nil))
    (maphash #'(lambda (key value)
                 (declare (ignore value))
                 (push key l))
             *wrapped-functions*)
    l))

(defun remove-wrappers (l)
  (mapc #'remove-wrapper (or l (get-wrapped-functions))))

(defun remove-wrapper (fn-name &optional (warn-p t))
  (let ((old-new-pair (gethash fn-name *wrapped-functions*)))
    (if (and (null old-new-pair) warn-p)
        (warn "No wrapper found for ~S" fn-name)
      (let ((current-fn (symbol-function fn-name)))
        (if (eql current-fn (second old-new-pair))
            (setf (symbol-function fn-name) (first old-new-pair))
          (if warn-p 
              (warn "~S has newer definition" fn-name)
            nil))
        (remhash fn-name *wrapped-functions*)))))

(defmacro trace-call (call-form result-form &optional (test-form t))
  (expand-trace-call nil call-form result-form test-form))

(defmacro break-call (call-form result-form &optional (test-form t))
  (expand-trace-call t call-form result-form test-form))

(defun expand-trace-call (breakp call-form result-form &optional (test-form t))
  (let ((fn-name (if (atom call-form) call-form (car call-form)))
        (arg-forms (if (atom call-form) nil (cdr call-form))))
    `(internal-trace-call ',fn-name ,breakp (list ,@arg-forms) (lambda () ,result-form) (lambda () ,test-form))))

(defun internal-trace-call (fn-name breakp args result-thunk test-thunk)
  (cond ((funcall test-thunk)
         (format *trace-output* "~&~VT~S <<~{ ~S~}~%" *wrap-indent* fn-name args)
         (when breakp (break))
         (let ((results 
                (let ((*wrap-indent* (+ *wrap-indent* 2)))
                  (multiple-value-list (funcall result-thunk)))))
           (format *trace-output* "~&~VT~S >>~{ ~S~}~%" *wrap-indent* fn-name results)
           (values-list results)))
        (t
         (funcall result-thunk))))


