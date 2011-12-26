(in-package #:geodesics)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

#| This set of equations has no dark radiation term and no breaking of Z_2 symmetry |#

#| Note that they are probably wrong, in that too many simplifications have been made
   (including the erroneous Sqrt(x^2) => x) |#

(defun-with-z-and-chi a (time y)
  (* A (expt z -1/2) (expt chi -1/4) (+ 2 (* chi (- 1 z)))))

(defun-with-z-and-chi n (time y)
  (* 1/2 (expt z -1/2) (+ 2 (* -3 chi (- 1 z)))))

(defun-with-z-and-chi adash (time y)
  (* (- A) mu (+ (* (expt z 1/2) (expt chi 3/4))
                 (* 1/2 (expt z -1/2) (expt chi -1/4) (+ 2 (* chi (- 1 z)))))))

(defun-with-z-and-chi ndash (time y)
  (* -1/2 mu (+ (* -3 (expt z 1/2) chi)
                (* 1/2 (expt z -1/2) (+ 2 (* -3 chi (- 1 z)))))))

(defun-with-z-and-chi da/dt (time y)
  (* A (expt z -1/2) dchi/dt
     (- (* (expt chi -1/4) (- 1 z))
        (* 1/4 (expt chi -5/4) (+ 2 (* chi (- 1 z)))))))

(defun-with-z-and-chi dn/dt (time y)
  (* 1/2 -3 (expt z -1/2) (- 1 z) dchi/dt))

#| End of simplest case equations. |#
