;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  Binding operators

;;; CL functions implemented:
;;;   let let* setq psetq setf

;;; let looks like procedure invocation

(define-ps let
    (let ((var-list (cadr ps-form))
	  (body (cddr ps-form))
	  res
	  newvars)
	 (for (:in v var-list)
	      (:do
		  (cond ((symbolp v)   ; Initialize to 0 if no initial value
		         (push v newvars)
			 (emit 0))
			((symbolp (car v))
			 (push (car v) newvars)
			   (compile-1 (cadr v)))
			((consp (car v))
			 (let ((n (ps-compile (cadr v))))
			   (when (/= n (length (car v)))
				 (ps-error "Wrong number of values"
					   ps-form v))
			   (for (:in x (car v))
				(:do (push x newvars)))))
			(T (ps-error "Syntax error in let"
				     v ps-form)))))
	 (with-new-frame
	  (for (:in v newvars)  ; note newvars is in reversed order
	      (:do (bind-it v)))
	  (setf res (ps-compile (cons 'progn body))))
	 res))

;;  Let* expands to let

(define-mac let* (&rest ps-form)
    (if (null (car ps-form))
        `(progn ,@(cdr ps-form))
	`(let (,(car (car ps-form)))
	      (let* ,(cdr (car ps-form))
		    ,@(cdr ps-form)))))


(define-ps setq
  (setf ps-form (cdr ps-form))
  (while ps-form
    (when (null (cdr ps-form))
	  (ps-error "Odd # of arguments to setq" ps-form))
    (let* ((var (car ps-form))
	   (val (cadr ps-form)))
       (if (symbolp var)
	   (progn
	     (compile-var var)
	     (compile-1 val)
	     (emit 'store))
	   (let ((n (ps-compile val)))
	     (when (/= n (length var))
		   (ps-error "Wrong number of values" ps-form))
	     (for (:in v (reverse var))
		  (:do (compile-var v)
		       (emit 'exch)
		       (emit 'store))))))
    (setf ps-form (cddr ps-form)))
  0)

(define-ps psetq
  (setf ps-form (cdr ps-form))
  (let (vars)
    (while ps-form
      (when (null (cdr ps-form))
	    (ps-error "Odd # of arguments to psetq" ps-form))
      (compile-1 (cadr ps-form))
      (push (car ps-form) vars)
      (setf ps-form (cddr ps-form)))
    (while vars
      (compile-var (car vars))
      (emit 'exch)
      (emit 'store)
      (pop vars)))
  0)
      
(define-mac setf (&rest args)
  `(progn ,@(all-sets args args)))

(defun all-sets (args all)
  (cond ((null args) nil)
	((null (cdr args))
	 (ps-error "Odd number of arguments to setf" all))
	(T (cons (setf-1 (car args) (cadr args))
		 (all-sets (cddr args) all)))))

(defun setf-1 (var val)
  (if (symbolp var)
      `(setq ,var ,val)
      (let ((fn (car var)))
	(cond ((member fn '(elt get))
	       `(put ,(cadr var) ,(caddr var) ,val))
	      ((ps-macro fn)
	       (setf-1 (ps-macro-expand var) val))
	      ((eq fn 'values)
	       `(setq ,(cdr var) ,val))
	      ((or (get var 'ps-function) (get var 'ps-lisp-primitive))
		 (ps-error "Setf can't handle this" var))
	      (t `(setq ,var ,val))))))  ; implicit values

