;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  Basic common lisp control flow constructs

;;; Common Lisp functions implemented here:
;;;   progn if and or cond when unless prog1


;;;  Progn - compile for no value except last one

(define-ps progn
  (let ((res 0))
   (for (:on fn (cdr ps-form))
        (:do (if (cdr fn)
	 	 (ps-compile-novalue (car fn))
		 (setf res (ps-compile (car fn))))))
   res))

;;; if - split into 2 cases depending in whether an else clause exists

(define-ps if
  (if (null (cdddr ps-form))
      (if-1 (cadr ps-form) (caddr ps-form))
      (if-2 (cadr ps-form) (caddr ps-form) (cadddr ps-form))))

;;; if with no else - cant leave anything on stack.

(defun if-1 (tst code)
    (compile-1 tst)
    (ps-compile-block-novalue code)
    (emit 'if)
    0)

;;; with else part, both then and else parts must return same number of values

(defun if-2 (tst code1 code2)
    (compile-1 tst)
    (let* ((r1 (ps-compile-block code1))
	   (r2 (ps-compile-block code2)))
	 (when (/= r1 r2)
	       (ps-error "Incompatable forms" code1 code2))
	 (emit 'ifelse)
	 r1))

(define-mac and (&rest clauses)
   (cond ((null clauses) t)
         ((null (cdr clauses)) (car clauses))
         (t `(if ,(car clauses) (and ,@(cdr clauses)) nil))))

(define-mac or (&rest clauses)
   (cond ((null clauses) t)
         ((null (cdr clauses)) (car clauses))
         (t `(if ,(car clauses) T (or ,@(cdr clauses))))))

       
(define-mac cond (&rest clauses)
  (expand-cond clauses))

(defun expand-cond (clauses)
   (cond ((eq (caar clauses) 'T)
	  (cons 'progn (cdar clauses)))
	 ((null (cdr clauses))
	  `(if ,(caar clauses) ,(cons 'progn (cdar clauses))))
	 (t `(if ,(caar clauses) ,(cons 'progn (cdar clauses))
	       ,(expand-cond (cdr clauses))))))

(define-mac when (test &rest clauses)
   `(if ,test ,(cons 'progn clauses)))

(define-mac unless (test &rest clauses)
   `(if (not ,test) ,(cons 'progn clauses)))

(define-ps prog1
   (let ((res (ps-compile (cadr ps-form))))
      (for (:in x (cddr ps-form))
	   (:do (ps-compile-novalue x)))
      res))
