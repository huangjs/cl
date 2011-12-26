(in-package #:geodesics)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

#| These equations stay the same every time, fortunately |#

(defun ndot (tdot time ydot y)
  (declare (double-float tdot time ydot y))
  (+ (* tdot (dn/dt time y)) (* ydot (ndash time y))))

(defun d2t/dp2 (p tdot time ydot y x)
  (declare (ignore p x))
  (declare (double-float tdot time ydot y))
  (* (/ (n time y))
     (- (* (dn/dt time y) tdot tdot)
	(* 2 (ndot tdot time ydot y) tdot)
	(* (da/dt time y) (/ (* kappa kappa) (expt (a time y) 3) (n time y))))))

(defun dx/dp (p tdot time ydot y x)
  (declare (ignore p tdot ydot x))
  (declare (double-float time y))
  (/ kappa (expt (a time y) 2)))

(defun d2y/dp2 (p tdot time ydot y x)
  (declare (ignore p x tdot))
  (declare (double-float time ydot y))
  (- (- (* (/ (* kappa kappa) (expt (a time y) 2))
	   (- (/ (ndash time y) (n time y))
	      (/ (adash time y) (a time y)))))
     (* (/ (ndash time y) (n time y)) ydot ydot)))
