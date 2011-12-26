;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  Basic numeric operators are defined here.

;;; CL functions implemented here:
;;;   + * - / 

;;; + has variable # of args, does constant folding

(define-ps +
  (let ((const-part 0)
	(pending-arg nil))
     (for (:in arg (cdr ps-form))
	  (:do
              (compile-1 arg)
	      (if (numeric-constant)
		  (setf const-part (+ (remove-number) const-part))
  		  (progn (when pending-arg (emit 'add))
		         (setf pending-arg t)))))
     (if pending-arg
	 (when (/= const-part 0)
	       (ps-compile const-part)
	       (emit 'add))
	 (ps-compile const-part)))
  1)

;;; like +

(define-ps *
  (let ((const-part 1)
	(pending-arg nil))
     (for (:in arg (cdr ps-form))
	  (:do
              (compile-1 arg)
	      (if (numeric-constant)
		  (setf const-part (* (remove-number) const-part))
  		  (progn (when pending-arg (emit 'mul))
		         (setf pending-arg t)))))
     (if pending-arg
	 (when (/= const-part 1)
	       (ps-compile const-part)
	       (emit 'mul))
	 (ps-compile const-part)))
  1)

;;; Like +

(define-ps -
 (if (null (cddr ps-form))
  (ps-negate ps-form)
  (let ((const-part 0)
	(pending-arg nil))
     (for (:in arg (cdr ps-form))
	  (:do
              (compile-1 arg)
	      (if (and pending-arg (numeric-constant))
		  (setf const-part (+ (remove-number) const-part))
  		  (progn (when pending-arg (emit 'sub))
		         (setf pending-arg t)))))
     (when (/= const-part 0)
	   (if (numeric-constant)
	       (ps-compile (- (remove-number) const-part))
	       (progn (ps-compile const-part)
	              (emit 'sub))))))
  1)

;;; Unary -

(defun ps-negate (ps-form)
   (compile-1 (cadr ps-form))
   (if (numeric-constant)
       (ps-compile (- (remove-number)))
       (emit 'neg))
   1)

;;; / is a little hairier for constant folding.

(define-ps /
 (if (null (cddr ps-form))
  (ps-recip ps-form)
  (let ((const-part 1.0)
	(pending-arg nil))
     (for (:in arg (cdr ps-form))
	  (:do
              (compile-1 arg)
	      (if (and pending-arg (numeric-constant))
		  (setf const-part (* (remove-number) const-part))
  		  (progn (when pending-arg (emit 'div))
		         (setf pending-arg t)))))
     (when (/= const-part 1.0)
	   (if (numeric-constant)
	       (ps-compile (/ (remove-number) const-part))
	       (progn (ps-compile const-part)
		      (emit 'div))))))
  1)

;;;  Unary /

(defun ps-recip (ps-form)
   (compile-1 (cadr ps-form))
   (if (numeric-constant)
       (ps-compile (/ 1.0 (remove-number)))
       (progn (emit 1)
	      (emit 'exch)
	      (emit 'div)))
   1)

