;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;; Control structures with looping

;;; Common lisp functions implemented here:
;;;  do do* while (not CL!) dolist dotimes dodict (not CL) map mapcar

(define-ps while
  (emit '\{)
  (compile-1 (cadr ps-form))
  (emit 'not)
  (emit '\{)
  (emit 'exit) 
  (emit '\})
  (emit 'if)
  (ps-compile-novalue (cons 'progn (cddr ps-form)))
  (emit '\})
  (emit 'loop)
  0)


;;;  The common lisp do function

(define-mac do (vars end &rest body)
   (do-da vars end body 'let))

(define-mac do* (vars end &rest body)
   (do-da vars end body 'let*))

(defun do-da (vars end body let-kind)
  (let (inits steps)
    (for (:in x vars)
	 (:do (push (list (car x) (or (cadr x) 0)) inits)
	      (when (caddr x)
		    (push (car x) steps)
		    (push (caddr x) steps))))
    `(,let-kind
        ,(reverse inits)
	(while
	     (not ,(car end))
	     (progn ,@body)
	     ,@(if steps
		   (if (eq let-kind 'let)
		       `((psetq ,@(reverse steps)))
		       `((setq ,@(reverse steps))))
		   nil))
	,@(cdr end))))


(define-mac dolist (vlr &rest body)
   (let ((v (car vlr))
	 (lst (cadr vlr))
	 (res (cddr vlr)))
      `(progn (forall ,lst #'(lambda (,v) ,@body))
	      ,@res)))

(define-mac dotimes (vcr &rest body)
    (let ((v (car vcr))
	  (cnt (cadr vcr))
	  (res (cddr vcr)))
	`(progn (for 0 1 (1- ,cnt) #'(lambda (,v) ,@body))
	        ,@res)))

;;; dodict has 2 vars (dodict (key val dict) statements)

(define-mac dodict (vvlr &rest body)
   (let ((v1 (car vvlr))
	 (v2 (cadr vvlr))
	 (lst (caddr vvlr))
	 (res (cdddr vvlr)))
      `(progn (forall ,lst #'(lambda (,v1 ,v2) ,@body))
	      ,@res)))

(define-mac map (ty fn seq)
    (cond ((eq ty 'nil)
	   `(forall ,seq ,fn))
	  ((member ty '('array 'list 'vector) :test #'equal)
	   `(mapcar ,fn ,seq))
	  (T (ps-error "Can't handle this sort of map"
		 `(map ,ty ,fn ,seq)))))

(define-ps mapcar
    (let ((fn (cadr ps-form))
	  (seq (caddr ps-form)))
	(emit '\[)
	(compile-1 seq)
	(compile-1 fn)
	(emit 'forall)
	(emit '\])
	1))
