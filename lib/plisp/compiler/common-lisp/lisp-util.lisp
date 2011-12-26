;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  Random useful lisp functions

;;; CL functions implemented here:
;;;   = > < >= <= /= 1+ 1- incf decf vector quote symbol-value elt
;;;   postscript (non CL) arrayp vectorp dictp (non CL) fontp (non CL)
;;;   integerp symbolp floatp numberp stringp token (non CL) 

;;; make = same as eq

(define-ps =
  (expand-transitive (cdr ps-form) 'eq))

(define-ps >
 (expand-transitive (cdr ps-form) 'gt))

(define-ps <
  (expand-transitive (cdr ps-form) 'lt))

(define-ps >=
  (expand-transitive (cdr ps-form) 'ge))

(define-ps <=
  (expand-transitive (cdr ps-form) 'le))

(defun expand-transitive (x op)
  (if (or (null x) (null (cdr x)))
      (ps-error "Not enough args" x op)
      (let ((braces 0))
	(compile-1 (car x))
	(setf x (cdr x))
	(while (cdr x)
	  (compile-1 (car x))
	  (setf x (cdr x))
	  (emit 'dup)
	  (emit 3)
	  (emit 1)
	  (emit 'roll)
	  (emit op)
	  (emit '\{)
	  (incf braces))
	(compile-1 (car x))
	(emit op)
	(for (:from i 1 braces)
	     (:do
	      (emit '\})
	      (emit '\{)
	      (ps-delete 1)
	      (emit nil)
	      (emit '\})
	      (emit 'ifelse)))))
  1)
	
(define-mac /= (&rest things)
  (if (= (length things) 2)
      `(ne ,(car things) ,(cadr things))
      (progn (ps-error "/= must have exactly 2 arguments" things)
	     nil)))

(define-mac 1+ (x)
  `(+ ,x 1))

(define-mac 1- (x)
  `(- ,x 1))

(define-mac incf (x &optional (delta 1))
  `(setf ,x (+ ,x ,delta)))

(define-mac decf (x &optional (delta 1))
  `(setf ,x (- ,x ,delta)))

;;; Use make-array to construct arrays

(define-ps vector
    (emit '\[)
    (for (:in x (cdr ps-form))
	 (:do (compile-1 x)))
    (emit '\])
    1)

;;; Quote isnt too smart right now - just quoted symbols.

(define-ps quote
  (emit ps-form)
  1)

;;; Symbol-value is "load" in the ps world

(define-ps symbol-value
    (compile-1 (cadr ps-form))
    (emit 'load)
    1)

(define-mac elt (x i)
  `(get ,x ,i))

(define-ps postscript
  (let ((args (cadr ps-form))
	(nres (caddr ps-form))
	(body (cdddr ps-form)))
    (for (:in a args)
	 (:do (ps-compile a)))
    (for (:in x body)
	 (:do (emit x)))
    nres))

(define-mac arrayp (x) `(eq (type ,x) 'arraytype))
(define-mac vectorp (x) `(eq (type ,x) 'arraytype))
(define-mac dictp (x) `(eq (type ,x) 'dicttype))
(define-mac fontp (x) `(eq (type ,x) 'fonttype))
(define-mac integerp (x) `(eq (type ,x) 'integertype))
(define-mac symbolp (x) `(eq (type ,x) 'nametype))
(define-mac floatp (x) `(eq (type ,x) 'floattype))
(define-mac numberp (x) `(or (integerp ,x) (floatp ,x)))
(define-mac stringp (x) `(eq (type ,x) 'stringtype))
