; This book should certify in spite of the fact that identity-macro is not
; around on the include-book pass of certify-book, yet we are supplying
; :check-expansion arguments.  (We omit some :check-expansion arguments too,
; just for fun.)  Inspection of the .cert file shows that it should only
; contain an entry for the last (8th) form in this file; see
; local-requires-skip-check-include.lisp.

(in-package "ACL2")

(local
 (defmacro identity-macro (x)
   x))

(local
 (make-event
  '(defun test1 (x) (identity-macro x))
  :check-expansion
  t)
 )

(local
 (make-event
  '(defun test2 (x) (identity-macro x))
  :check-expansion
  (defun test2 (x) (identity-macro x)))
 )

(local
 (make-event
  '(defun test3 (x) (identity-macro x)))
 )

(encapsulate
 ()

 (local
  (make-event
   '(defun test4 (x) (identity-macro x))
   :check-expansion
   t)
  )

 (defun test5 (x) x))

(encapsulate
 ()

 (local
  (make-event
   '(defun test6 (x) (identity-macro x))
   :check-expansion
   (defun test6 (x) (identity-macro x)))
  )

 (defun test7 (x) x))

(encapsulate
 ()

 (local
  (make-event
   '(defun test8 (x) (identity-macro x)))
  )

 (defun test9 (x) x))

(include-book "eval")

(must-fail
 (local
  (make-event
   '(defun test10 (x) (identity-macro x))
   :check-expansion
   (defun test10 (x) (cons x x)))
  ))

