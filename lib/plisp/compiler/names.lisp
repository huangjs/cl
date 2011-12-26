;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  This is the name resolver for PLisp.

(defun lookup-name (name)   ;;  Lookup the name in an environment
  (let (s-value temp
	(env current-env))
    (cond ((setf temp (get name 'ps-function))
	   (list 'postscript temp))
	  ((setf temp (get name 'ps-lisp-primitive))
	   (list 'primitive temp))
	  ((setf temp (get name 'ps-macro))
	   (list 'ps-macro temp))
	  ((setf temp (assoc name lexical-vars))
	   (cons 'lexical (cdr temp)))
	  (T
	   (while env
	    (let ((ty (att (car env) name 'type)))
		(if ty
		    (progn
		      (setf s-value (list ty (car env)))
		      (setf env nil))
		    (setf env (cdr env)))))
	   (when (and s-value (eq (car s-value) 'in-main))
		 (setf s-value (list (att main-table name 'type)
				       main-table)))
	   (or s-value (list 'undefined (car current-env)))))))


(defun define-name (table name type)
  (put-att table name 'type type))

(defun import-name (table name f-v)
 (when (not (eq main-table table))
  (let ((def (att main-table name 'type)))
    (cond ((null def)
	   (setf (gethash name main-table) (gethash name table))
	   (setf (gethash name table) nil)
	   (define-name table name 'in-main)
	   (if (eq f-v 'f)
	       (push-end name to-compile)
	       (push-end name to-init)))
	  (T (ps-error "Symbol conflict between main program and library"
		       name def))))))



