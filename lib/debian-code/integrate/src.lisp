;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: integrate -*-

(in-package :integrate)

;;; trap for the unwary: *step-size* gets changed through the course
;;; of an integration. It might be wise to wrap calls to integrate
;;; inside (let ((*step-size* 0.001)) (integrate ...))

(defvar *step-size* 1.0d-3)

(defvar *tolerance* 1.0d-3)

;;; BUG you moron. *epsilon* appears only to be used as a smoothing
;;; parameter in adaptive integrators. This is probably not clever.
(defvar *epsilon* single-float-epsilon)

;;; *integrator* api:

;;; takes a state, and integrates it from t (the first thingy in the
;;; state, usually 0) to whenever (funcall *stop-function* state)
;;; returns t, logging to the stream defined by *log*. Note that with
;;; higher-order adaptive integrators, it is perfectly possible that
;;; polynomial interpolation be necessary between the points produced.

(defvar *integrator* 'euler
  "An integrator takes a state (defined as a list, currently) and
integrates it from time = (first state) to whenever (funcall
*stop-function* state) does not return nil, with logging to the stream
defined by *log*.")

(defvar *target-t* 1.0)

(defvar *stop-function* (lambda (state) (<= *target-t* (car state)))
  "The *stop-function* must return nil for the integration to
continue.")

(defvar *log* nil
  "Stream (as interpreted by format) to log to. nil means no logging")

;;; we might eventually want integrate to return a function of the
;;; variable that can be evaluated as normal.

(define-condition precision-loss (arithmetic-error) ())

(defun integrate (initial-time &rest system)
  "This is just a wrapper, to convert the system into something that
will be understood by integrate-1. This is actually not ideal, as the
logging then becomes not immediately transparent, but hey. We could
always return a function that'll take the output log and return n
functions representing the solutions :-)"
  ;; do this in two passes -- it's just easier.
  (handler-case
      (let* ((state (cons initial-time
			  (loop for x in (cdr system) by #'cddr
				appending x))))
;	     (nargs (length state)))
	(let ((functions
	       (cons (lambda (&rest x) (declare (ignore x)) 1)
		     (loop for x on system by #'cddr
			   for offset = 0 then (+ offset len)
			   for len = (length (cadr x))
			   collect (car x)
			   append (loop for y from 1 to (1- len)
;;					with gensyms = (loop for i from 1 to nargs collect (gensym))
					collect
					(let ((n (+ y offset)))
					  (lambda (&rest stuff)
					    (nth n stuff))))))))
;;                                        (coerce `(lambda ,gensyms
;;                                                  (declare (ignorable ,@gensyms))
;;                                                   ,(nth (+ y offset) gensyms))
;;                                                'function))))))
	  (apply #'integrate-1 state functions)))
      (precision-loss () (warn "Before finishing, we've encountered loss of precision.") nil)
      (arithmetic-error (condition) (warn "~@(~a~) error signalled -- we probably hit a divergence." (type-of condition)) nil)
      ))


;;; document this. It is *so* cute you'll forget what on earth is
;;; going on.
(defun integrate-1 (state &rest functions) ; this is probably too "cute"
  "State is a list of numbers, corresponding to (time y^{(n-1)}
y^{(n-2)} ...), where the equation to solve is y^{(n)} = f(time,
y^{(n-1)}, ..., y). This function performs a reduction of this nth
order equation to n first order equations, adds an explicit equation
for time (dt/dt = 1) as a check, and then applies the *integrator*
function to the result"
  (if (not (= (length state) (length functions)))
      (let ((gensyms (loop for i from 1 to (length state) collect (gensym))))
	(apply #'integrate-1
	       state
	       (apply #'list
		      (lambda (&rest x) (declare (ignore x)) 1)
		      (car functions)
		      (loop for x from 3 to (length state)
			    collect (coerce `(lambda ,gensyms
					       (declare (ignorable ,@gensyms))
					       ,(nth (- x 2) gensyms))
					    'function)))))
    (apply *integrator* state functions)))

;;; there's a macro that is just asking to be written somewhere in
;;; here...
(defun linear-interpolate (x x0 y0 x1 y1)
  (flet ((frob (x a b by)
	   (* (/ (* (- x a)) (* (- b a))) by)))
    (+ (frob x x0 x1 y1)
       (frob x x1 x0 y0))))

(defun quadratic-interpolate (x x0 y0 x1 y1 x2 y2)
  (flet ((frob (x a b c cy)
	   (* (/ (* (- x a) (- x b)) (* (- c a) (- c b))) cy)))
    (+ (frob x x0 x1 x2 y2)
       (frob x x1 x2 x0 y0)
       (frob x x2 x0 x1 y1))))

(defun cubic-interpolate (x x0 y0 x1 y1 x2 y2 x3 y3)
  (flet ((frob (x a b c d dy)
	   (* (/ (* (- x a) (- x b) (- x c)) (* (- d a) (- d b) (- d c))) dy)))
    (+ (frob x x0 x1 x2 x3 y3)
       (frob x x1 x2 x3 x0 y0)
       (frob x x2 x3 x0 x1 y1)
       (frob x x3 x0 x1 x2 y2))))

;;; A function that returns a function representing the solution to
;;; the single-variable differential equation.
(defun integrated-1-function (state function)
  (let ((alist nil))
    (lambda (arg)
      (unless (assoc arg alist :test #'<)
	(let ((*log* (make-string-output-stream)) ; should be a broadcast-stream?
	      ;; this is wrong -- it should be something like (- arg last-value)
	      (*step-size* (min *step-size* (/ arg pi)))
	      (*target-t* arg))
	  (if alist
	      ;; this depends on ordering
	      (integrate-1 (car (last alist)) function)
	    (integrate-1 state function))
	  (with-each-stream-line (line (make-string-input-stream (get-output-stream-string *log*))
		;; preserve ordering -- this is inefficient.
		do (setf alist (sort (pushnew (read-from-string (format nil "(~a)" line)) alist :test #'equal) #'< :key #'car))))))
      (when (< (length alist) 2)
	(error "Need more points. Rethink."))
      (loop for ooos = state then oos
	    for oos = state then os
	    for os = state then s
	    for s in alist
	    if (< (car os) arg (car s))
	    do (cond
		((< (car ooos) (car oos))
		 (return (apply #'cubic-interpolate
				(append (list arg) ooos oos os s))))
		((< (car oos) (car os))
		 (return (apply #'quadratic-interpolate
				(append (list arg) oos os s))))
		(t (return (apply #'linear-interpolate
				  (append (list arg) os s)))))))))

(defun euler (state &rest functions)
  (let ((current-state (copy-list state)))
    (loop until (funcall *stop-function* current-state)
	  if *log*
	    ;; the benefit of this notation is that re-reading the
	    ;; state is as simple as
	    ;; (read-from-string (concatenate 'string "(" line ")"))
	    ;; oh, and it's gnuplot friendly format, too.
	    do (format *log* "~{~d~^ ~}~%" current-state) 
	  do (let ((derivs (loop for fn in functions collect (apply fn current-state))))
	       (setf current-state
		     (loop for y in current-state
			   for d in derivs
			   collect (+ y (* d *step-size*)))))
	  finally
	    (when *log*
	      (format *log* "~{~d~^ ~}" current-state))
	    (return current-state))))

(defun make-adaptive-integrator (fn order)
  "takes two arguments -- an integrator to make adaptive and a number
describing the order of the error (2 for euler, 5 for fourth-order
runge-kutta)"
  (lambda (state &rest functions)
    (let ((current-state (copy-list state)))
      (loop until (funcall *stop-function* current-state)
	    if *log*
	      do (format *log* "~{~d~^ ~}~%" current-state) 
	    do (let ((*log* nil))
		 (loop until
		       (let ((*target-t* (+ (first current-state) (* 1.9 *step-size*))))
			 (let ((*stop-function* (lambda (state) (<= *target-t* (first state)))))
			   (let ((new-state-1
				  (let ((*step-size* (* 2 *step-size*)))
				    (apply fn current-state functions)))
				 (new-state-2 (apply fn current-state functions)))
			     ;; FIXME
			     (let ((deltas (mapcar (lambda (x y) (abs (- x y)))
						   new-state-1 new-state-2)))
			       (if (every (lambda (x) (< x *tolerance*))
					  deltas)
				   (progn
				     (setf *step-size*
					   (* 0.99 *step-size* (expt (/ *tolerance* (+ *epsilon* (apply #'max deltas))) (/ 1 order))))
				     (setf current-state new-state-2))
				 (progn
				   ;; we need to retry with a smaller
				   ;; step size, so return nil after
				   ;; adjusting the step.
				   (setf *step-size*
					 (* 0.99 *step-size* (expt (/ *tolerance* (apply #'max deltas)) (/ 1 (1- order)))))
				   nil))))))))
	    finally
  	      (when *log*
		(format *log* "~{~d~^ ~}" current-state))
	      (return current-state)))))

(defun runge-kutta (state &rest functions)
  (let ((current-state (copy-list state)))
    (loop until (funcall *stop-function* current-state)
	  if *log*
	    do (format *log* "~{~d~^ ~}~%" current-state) 
	  do (setf current-state
		   ;; those mapcars could be turned into map-intos,
		   ;; not that this seems too slow in the first place.
		   (let* ((derivs1 (loop for fn in functions collect (apply fn current-state)))
			  (k1 (mapcar (lambda (x) (* *step-size* x)) derivs1))
			  (tmp-state (mapcar #'+ current-state (mapcar #'(lambda (x) (/ x 2)) k1)))
			  (derivs2 (loop for fn in functions collect (apply fn tmp-state)))
			  (k2 (mapcar (lambda (x) (* *step-size* x)) derivs2))
			  (tmp-state (mapcar #'+ current-state (mapcar #'(lambda (x) (/ x 2)) k2)))
			  (derivs3 (loop for fn in functions collect (apply fn tmp-state)))
			  (k3 (mapcar (lambda (x) (* *step-size* x)) derivs3))
			  (tmp-state (mapcar #'+ current-state k3))
			  (derivs4 (loop for fn in functions collect (apply fn tmp-state)))
			  (k4 (mapcar (lambda (x) (* *step-size* x)) derivs4)))
		     (let ((steps (mapcar #'+
					  (mapcar (lambda (x) (/ x 6)) k1)
					  (mapcar (lambda (x) (/ x 3)) k2)
					  (mapcar (lambda (x) (/ x 3)) k3)
					  (mapcar (lambda (x) (/ x 6)) k4))))
;		       (format t "~a" (mapcar #'(lambda (x y) (if (= y 0) 1.0 (/ x y))) steps current-state))
		       (if (some (lambda (x) (< (abs x) double-float-epsilon))
				 (mapcar #'(lambda (x y) (if (= y 0) 1.0 (/ x y))) steps current-state))
			   (error 'precision-loss)
			 (mapcar #'+ current-state steps))))
	  )
	  finally
	    (when *log*
	      (format *log* "~{~d~^ ~}" current-state))
  	    (return current-state))))
  
