;;; to generalize, we need a DEFINE-SYSTEM macro that defines a bunch
;;; of things including
;;;
;;; (a) INTEGRATE-SYSTEM &rest initial-conditions
;;; (b) WITH-STOP-CONDITION
;;; (c) ...
(import '(geodesics:dx/dp
	  geodesics:d2t/dp2
	  geodesics:d2y/dp2))

(defmacro with-stop-condition ((fbody &key maximum-time) &body body)
  (let ((arg (gensym))
	(secs (gensym)))
    (if maximum-time
	`(let (,secs)
	   (let ((*stop-function*
		  (lambda (,arg)
		    (symbol-macrolet ((p (nth 0 ,arg))
				      (tdot (nth 1 ,arg))
				      (time (nth 2 ,arg))
				      (ydot (nth 3 ,arg))
				      (y (nth 4 ,arg))
				      (x (nth 5 ,arg)))
		      (unless ,secs
			(setf ,secs (get-universal-time)))
		      (or (> (get-universal-time) (+ ,secs ,maximum-time))
			  ,fbody)))))
	     ,@body))
      `(let ((*stop-function*
	      (lambda (,arg)
		(symbol-macrolet ((p (nth 0 ,arg))
				  (tdot (nth 1 ,arg))
				  (time (nth 2 ,arg))
				  (ydot (nth 3 ,arg))
				  (y (nth 4 ,arg))
				  (x (nth 5 ,arg)))
		  ,fbody))))
	 ,@body))))

(defmacro with-logging-to-file (f &body body)
  (let ((s (gensym))
	(file (gensym)))
    ;; only evaluate f once, and don't capture variables either.
    `(let ((,file ,f)) 
       (with-open-file (,s ,file :direction :output :if-exists :supersede)
	 (let ((*log* ,s))
	   ,@body)))))

(setf *integrator* (make-adaptive-integrator #'runge-kutta 5))

(defun invert (fn value &key min max)
  ;; uses linear interpolation. With extreme loss of generality:
  (let ((min (or min 0.001d0))
	(max (or max 1000d0)))
    ;; allow the user to fix the assumption.
    (assert (not (= (signum (- (funcall fn min) value)) (signum (- (funcall fn max) value)))))
    (labels ((frob (x)
	     (if (or (< (abs (- (funcall fn x) value)) *epsilon*)
		     (= x max)
		     (= x min))
		 (return-from invert x)
	       (let ((f (funcall fn x)))
		 (if (= (signum (- f value)) (signum (- (funcall fn min) value)))
		     (setf min x)
		   (setf max x))
		 (frob (+ min (/ (* (- max min) (- value (funcall fn min))) (- (funcall fn max) (funcall fn min)))))))))
      (frob (+ min (/ (* (- max min) (- value (funcall fn min))) (- (funcall fn max) (funcall fn min))))))))