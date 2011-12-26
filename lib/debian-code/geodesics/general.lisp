(in-package #:geodesics)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

#| This set of equations is the same as above, but having lost the Z_2
   symmetry (parametrized by f). Generated with Mathematica, LispForm and
   convert.lisp. |#

(let ((h (make-hash-table :test 'equal)))
  ;; contains functions indexed by f. Not strictly correct, but OK if
  ;; f is the only thing changing -- i.e. not rho, k, c, gamma.
  (defun a0 (time)
    (declare (double-float time))
    (assert (> f 0.0d0))
    (if (< time (/ 3 80 k k rho)) ;; this is 1/10 of the equality time.
	(the double-float #i"((gamma/f)*((1+c)*(cosh(2*rho*f*time*k*k/3)-1)/(rho*f)+sinh(2*rho*f*time*k*k/3)/rho))^^(1/4)")
      (progn
	(unless (gethash (list k rho c f gamma) h)
	  (setf (gethash (list k rho c f gamma) h)
		(cl-user::integrated-1-function `(,(/ 3 80 k k rho) ,#i"((gamma/f)*((1+c)*(cosh(f/40)-1)/(rho*f)+sinh(f/40)/rho))^^(1/4)")
				       (lambda (time y)
					 (declare (ignore time))
					 (declare (double-float y))
					 #i"sqrt((1+c)*rho*k^^4*gamma/(18*y^^4)+k^^4*(gamma^^2/y^^8+f^^2*rho^^2*gamma^^2/(y^^8*(gamma/y^^4+rho)^^2))/36)"))))
	(the double-float (funcall (gethash (list k rho c f gamma) h) time)))))
  (defun clear-a0-function-cache ()
    (setf h (make-hash-table :test 'equal))))

(declaim (ftype (function (double-float) double-float) a0))

(defun-with-z-and-chi a (time y)
  (declare (double-float time y))
  (* (expt 2 1/2)
     A
     (expt z -1/2)
     (expt chi -1/4)
     (expt
      (+ (* (+ -1 (expt z 2)) (+ -1 (* -1 chi) (* f chi (expt (+ 1 chi) -1))))
         (* 2
            z
            (+ (* -1 chi)
               (* -1 c chi)
               (* (expt chi 2) -1/2)
               (* (expt f 2) (expt chi 2) (expt (+ 1 chi) -2) -1/2)))
         (* (+ 1 (expt z 2))
            (+ 1
               chi
               (* c chi)
               (* (expt chi 2) 1/2)
               (* (expt f 2) (expt chi 2) (expt (+ 1 chi) -2) 1/2))))
      1/2)))

(defun-with-z-and-chi da/dt (time y)
  (declare (double-float time y))
  (* dchi/dt
     (* A
        (expt z -1/2)
        (expt chi -5/4)
        (expt (+ 1 chi) -3)
        (+ -4
           (* chi
              (+ (* -4 (+ 2 z))
                 (* 3 (+ 1 (* (+ -6 z) z)) chi)
                 (* -1 (expt f 2) (expt (+ -1 z) 2) (+ -3 chi) chi)
                 (* (+ 17 (* 3 z (+ -10 (* 3 z)))) (expt chi 2))
                 (* (+ -1 z) (+ -13 (* 9 z)) (expt chi 3))
                 (* 3 (expt (+ -1 z) 2) (expt chi 4))
                 (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 3))
                 (* -2 f (+ -1 (expt z 2)) (+ -1 (expt chi 2))))))
        (expt
         (* (expt (+ 1 chi) -2)
            (+ 4
               (* chi
                  (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                     (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                     (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                     (* (+ -3 z (* (+ -1 z) chi))
                        (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
         -1/2)
        1/4)))

(defun-with-z-and-chi adash (time y)
  (declare (double-float time y))
  (* dz/dy
     (* A
        (expt z -3/2)
        (expt chi -1/4)
        (expt (+ 1 chi) -2)
        (expt
         (* (expt (+ 1 chi) -2)
            (+ 4
               (* chi
                  (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                     (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                     (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                     (* (+ -3 z (* (+ -1 z) chi))
                        (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
         -1/2)
        (+ -4
           (* chi
              (+ -12
                 (* (expt f 2) (+ -1 (expt z 2)) chi)
                 (* 2 f (+ 1 (expt z 2)) (+ 1 chi))
                 (* 2 c (+ -1 z) (+ 1 z) (expt (+ 1 chi) 2))
                 (* chi
                    (+ -13
                       (* -6 chi)
                       (* (+ z (* (+ -1 z) chi)) (+ z chi (* z chi))))))))
        1/2)))

(defun-with-z-and-chi n (time y)
  (declare (double-float time y))
  (* (expt z -1/2)
     (expt (+ 1 chi) -3)
     (+ 4
        (* -1
           chi
           (+ (* -4 (+ 2 z))
              (* 3 (+ 1 (* (+ -6 z) z)) chi)
              (* -1 (expt f 2) (expt (+ -1 z) 2) (+ -3 chi) chi)
              (* (+ 17 (* 3 z (+ -10 (* 3 z)))) (expt chi 2))
              (* (+ -1 z) (+ -13 (* 9 z)) (expt chi 3))
              (* 3 (expt (+ -1 z) 2) (expt chi 4))
              (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 3))
              (* -2 f (+ -1 (expt z 2)) (+ -1 (expt chi 2))))))
     (expt
      (* (expt (+ 1 chi) -2)
         (+ 4
            (* chi
               (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                  (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                  (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                  (* (+ -3 z (* (+ -1 z) chi))
                     (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
      -1/2)
     1/2))

(defun-with-z-and-chi dn/dt (time y)
  (declare (double-float time y))
  (* dchi/dt
     (* (+ -1 z)
        (expt z -1/2)
        (expt (+ 1 chi) -6)
        (expt
         (* (expt (+ 1 chi) -2)
            (+ 4
               (* chi
                  (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                     (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                     (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                     (* (+ -3 z (* (+ -1 z) chi))
                        (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
         -3/2)
        (+ (* (expt f 4) (expt (+ -1 z) 3) (+ 3 (* -5 chi)) (expt chi 3))
           (* 2 (expt c 2) (expt (+ -1 z) 3) chi (expt (+ 1 chi) 6))
           (* -3
              (expt f 3)
              (expt (+ -1 z) 2)
              (+ 1 z)
              (expt chi 2)
              (+ 1 chi)
              (+ -3 (* 5 chi)))
           (* 3 (expt (+ 1 chi) 6) (expt (+ -2 (* (+ -1 z) chi)) 3))
           (* (expt f 2)
              (+ -1 z)
              chi
              (expt (+ 1 chi) 2)
              (+ (* 2 (+ 15 (* z (+ 2 z))))
                 (* -2 (+ 6 (* z (+ 19 (* 5 z)))) chi)
                 (* 6 (+ -1 z) (+ 3 z) (expt chi 2))
                 (* (+ -7 z) (+ -1 z) (expt chi 3))
                 (* 7 (expt (+ -1 z) 2) (expt chi 4))))
           (* f
              (+ 1 z)
              (expt (+ 1 chi) 3)
              (+ 12
                 (* -4 (+ 3 (* 2 z)) chi)
                 (* 3 (+ -1 z) (+ 1 (* 3 z)) (expt chi 2))
                 (* 3 (+ -1 z) (+ -9 (* 5 z)) (expt chi 3))
                 (* 14 (expt (+ -1 z) 2) (expt chi 4))))
           (* c
              (+ -1 z)
              (expt (+ 1 chi) 2)
              (+ 12
                 (* chi
                    (+ 56
                       (* -8 z)
                       (* 113 chi)
                       (* z (+ -50 (* 9 z)) chi)
                       (* 3 (expt f 2) (expt (+ -1 z) 2) (+ -3 chi) (+ -1 chi) chi)
                       (* 12 (+ 11 (* z (+ -10 (* 3 z)))) (expt chi 2))
                       (* 2 (+ 49 (* z (+ -70 (* 27 z)))) (expt chi 3))
                       (* 4 (+ -1 z) (+ -11 (* 9 z)) (expt chi 4))
                       (* 9 (expt (+ -1 z) 2) (expt chi 5))
                       (* 2
                          f
                          (+ -1 z)
                          (+ 1 z)
                          (+ 1 chi)
                          (+ 2 (* 3 (+ -1 chi) chi))))))))
        -1/2)))

(defun-with-z-and-chi ndash (time y)
  (declare (double-float time y))
  (* dz/dy
     (+
      (* -1
         (expt z -1/2)
         chi
         (expt (+ 1 chi) -3)
         (+ (* -1 (expt f 2) (+ -1 z) (+ -3 chi) chi)
            (* 2 c (+ -1 z) (expt (+ 1 chi) 3))
            (* (expt (+ 1 chi) 3) (+ -2 (* -3 chi) (* 3 z chi)))
            (* -2 f z (+ -1 (expt chi 2))))
         (expt
          (* (expt (+ 1 chi) -2)
             (+ 4
                (* chi
                   (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                      (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                      (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                      (* (+ -3 z (* (+ -1 z) chi))
                         (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
          -1/2))
      (* (expt z -1/2)
         chi
         (expt (+ 1 chi) -5)
         (+ (* (expt f 2) (+ -1 z) chi)
            (* 2 f z (+ 1 chi))
            (* 2 c (+ -1 z) (expt (+ 1 chi) 2))
            (* (expt (+ 1 chi) 2) (+ -2 (* -1 chi) (* z chi))))
         (+ 4
            (* -1
               chi
               (+ (* -4 (+ 2 z))
                  (* 3 (+ 1 (* (+ -6 z) z)) chi)
                  (* -1 (expt f 2) (expt (+ -1 z) 2) (+ -3 chi) chi)
                  (* (+ 17 (* 3 z (+ -10 (* 3 z)))) (expt chi 2))
                  (* (+ -1 z) (+ -13 (* 9 z)) (expt chi 3))
                  (* 3 (expt (+ -1 z) 2) (expt chi 4))
                  (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 3))
                  (* -2 f (+ -1 (expt z 2)) (+ -1 (expt chi 2))))))
         (expt
          (* (expt (+ 1 chi) -2)
             (+ 4
                (* chi
                   (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                      (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                      (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                      (* (+ -3 z (* (+ -1 z) chi))
                         (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
          -3/2)
         -1/2)
      (* (expt z -3/2)
         (expt (+ 1 chi) -3)
         (+ 4
            (* -1
               chi
               (+ (* -4 (+ 2 z))
                  (* 3 (+ 1 (* (+ -6 z) z)) chi)
                  (* -1 (expt f 2) (expt (+ -1 z) 2) (+ -3 chi) chi)
                  (* (+ 17 (* 3 z (+ -10 (* 3 z)))) (expt chi 2))
                  (* (+ -1 z) (+ -13 (* 9 z)) (expt chi 3))
                  (* 3 (expt (+ -1 z) 2) (expt chi 4))
                  (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 3))
                  (* -2 f (+ -1 (expt z 2)) (+ -1 (expt chi 2))))))
         (expt
          (* (expt (+ 1 chi) -2)
             (+ 4
                (* chi
                   (+ (* (expt f 2) (expt (+ -1 z) 2) chi)
                      (* 2 f (+ -1 z) (+ 1 z) (+ 1 chi))
                      (* 2 c (expt (+ -1 z) 2) (expt (+ 1 chi) 2))
                      (* (+ -3 z (* (+ -1 z) chi))
                         (+ -4 (* chi (+ -3 z (* (+ -1 z) chi)))))))))
          -1/2)
         -1/4))))

#| End of stupidly complex case. |#

