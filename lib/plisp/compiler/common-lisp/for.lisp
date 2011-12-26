;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.


;;;;  For macros.  Right now, just simple from, in, and do.

;;; Note this is not a CL function.  Derived from HP looping macro.

;;; Compile a very small subset of the HP for macro

(define-ps for
    (let ((stepper (cadr ps-form))
	  (control (caddr ps-form)))
	(when (not (eql (car control) :do))
	    (ps-error ":do required" ps-form))
	(cond ((eql (car stepper) :from)
	       (for-from (cadr stepper) (caddr stepper) (cadddr stepper)
		         (car (cddddr stepper)) (cdr control)))
	      ((eql (car stepper) :in)
	       (for-in (cadr stepper) (caddr stepper) (cdr control)))
	      ((eql (car stepper) :in-dict)
	       (for-in-dict (cadr stepper) (caddr stepper) (cadddr stepper)
			    (cdr control)))
	      (T (ps-error "Unsupported for loop clause." ps-form)))))

;;; compile (for (:from)) straight into postscript "for"

(defun for-from (var start stop step body)
    (compile-1 start)
    (compile-1 (or step 1))
    (compile-1 stop)
    (emit '\{)
    (with-new-frame
     (bind-it var)
     (ps-compile-novalue (cons 'progn body)))
    (emit '\})
    (emit 'for)
  0)

;;;  (for (:in ..) iturns into "forall"

(defun for-in (var lst body)
  (compile-1 lst)
  (emit '\{)
  (with-new-frame
    (bind-it var)
    (ps-compile-novalue (cons 'progn body)))
  (emit '\})
  (emit 'forall)
  0)

(defun for-in-dict (key val lst body)
  (compile-1 lst)
  (emit '\{)
  (with-new-frame
    (bind-it val)
    (bind-it key)
    (ps-compile-novalue (cons 'progn body)))
  (emit '\})
  (emit 'forall)
  0)
