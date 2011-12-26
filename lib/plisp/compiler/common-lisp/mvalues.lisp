;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;; Support enough multiple value stuff to be useable

;;; CL functions implemented here:
;;;  values multiple-value-call multiple-value-bind multiple-value-list
;;;  Note: setq subsumes multiple-value-setq

;;;  Values is trivial.

(define-ps values
    (for (:in fm (cdr ps-form))
	 (:do (compile-1 fm)))
    (length (cdr ps-form)))

;;;  Multiple value call is also trivial - just let things stack up.

(define-ps multiple-value-call
   (let* ((fn (cadr (cadr ps-form)))
	  (args (cddr ps-form))
	  (num-vals 0)
	  (ps-fun (get fn 'ps-function))
	  values-in values-out)
       (when (not (eq (car (cadr ps-form)) 'function))
	     (ps-error "First argument must be #'function" ps-form))
       (if ps-fun
	   (progn
	     (setf values-in (car ps-fun))
	     (setf values-out (cadr ps-fun)))
	   (let ((def (lookup-name fn)))
	     (if (eq (car def) 'user-function)
		 (progn (setf values-in (length (att main-table fn 'vars)))
	                (setf values-out (att main-table fn 'results)))
	         (ps-error "Unknown function" fn ps-form))))
       (for (:in arg args)
	    (:do (setf num-vals (+ num-vals (ps-compile arg)))))
       (if (/= num-vals values-in)
	   (ps-error "Wrong number of arguments" ps-form))
       (emit fn)
       values-out))

;;; Basically same as a let

(define-ps multiple-value-bind
   (let* ((vars (cadr ps-form))
	  (func (ps-compile (caddr ps-form)))
	  (body (cons 'progn (cdddr ps-form)))
	  (res 0))
	(if (/= (length vars) func)
	    (ps-error "Wrong number of values" ps-form)
	    (with-new-frame
	       (for (:in v (reverse vars))
		    (:do (bind-it v)))
	       (setf res (ps-compile body))))
	res))

(define-ps multiple-value-list
   (emit '\[)
   (ps-compile (cadr ps-form))
   (emit '\])
   1)




