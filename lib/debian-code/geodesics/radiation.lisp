(in-package #:geodesics)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

#| This set of functions is for a more interesting case (translated from
   Geodesics2.nb). Should reduce to the above no matter equations for c=0,
   but don't. We think we understand why -- see above. |#

(defun-with-z-and-chi a (time y)
  (declare (double-float time y))
  (/ (* A (sqrt (+ 4 (* (- z 1) chi (+ -4 (* 2 c (- z 1)) (* (- z 1) chi))))))
     (* (sqrt z) (expt chi 1/4))))

(defun-with-z-and-chi n (time y)
  (declare (double-float time y))
  (- (/ (+ -4 (* (- z 1) chi (+ -4 (* 2 c (- z 1)) (* 3 (- z 1) chi))))
        (* 2 (sqrt z) (sqrt (+ 4 (* (- z 1) chi (+ -4 (* 2 c (- z 1)) (* (- z 1) chi)))))))))

(defun-with-z-and-chi adash (time y)
  (declare (double-float time y))
  (* dz/dy (/ (* A (+ -4 (* chi (+ -4 (* 2 c (- (* z z) 1)) (* (- (* z z) 1) chi)))))
              (* 2 (expt z 3/2) (expt chi 1/4)
                 (sqrt (+ 4 (* (- z 1) chi (+ -4 (* 2 c (- z 1)) (* (- z 1) chi)))))))))

(defun-with-z-and-chi ndash (time y)
  (declare (double-float time y))
  (/ (* dz/dy
        (- (+ 16
              (* 48 (+ -1 (* c (- z 1))) z chi)
              (* 4 (- z 1) (+ 6 (* 12 z) (* c (- z 1) (+ (* -2 (+ 2 z)) (* c (- (* z z) 1))) chi chi)))
              (* 4 (expt (- z 1) 2) (+ -4 (* -5 z) (* 2 c (- (* z z) 1))) (expt chi 3))
              (* 3 (expt (- z 1) 3) (+ z 1) (expt chi 4)))))
     (* 4 (expt z 3/2) (expt (+ 4 (* (- z 1) chi (+ -4 (* 2 c (- z 1)) (* (- z 1) chi)))) 3/2))))

(defun-with-z-and-chi da/dt (time y)
  (declare (double-float time y))
  ;; wow!
  (* dchi/dt (n time y) (/ (- A) 2 (expt chi 5/4))))

(defun-with-z-and-chi dn/dt (time y)
  (declare (double-float time y))
  (* dchi/dt
     (/ (- (* (- z 1) (+ (* 2 c c (expt (- z 1) 3) chi)
                         (* 3 (expt (+ -2 (* (- z 1) chi)) 3))
                         (* c (- z 1) (+ 12 (* (- z 1) chi (+ -8 (* 9 (- z 1) chi))))))))
        (* (sqrt z)
           (sqrt (+ 4 (* (- z 1) chi (+ -4 (* 2 c (- z 1)) (* (- z 1) chi)))))
           (+ 8 (* 2 (- z 1) chi (+ -4 (* 2 c (- z 1)) (* (- z 1) chi))))))))
