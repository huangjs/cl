(in-package #:geodesics)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

#| Static (as above case) but with n =/= a |#

(defun-with-z-and-chi a (time y)
  (declare (double-float y)
           (ignore time))
  (expt (+ (cosh (* 2 y mu))
           (* (+ d (* -1 r)) (sinh (* 2 y mu))))
        1/2))
  
(defun-with-z-and-chi da/dt (time y)
  (declare (ignore time y))
  0.0d0)

(defun-with-z-and-chi adash (time y)
  (declare (double-float y)
           (ignore time))
  (* mu
     (+ (* (+ d (* -1 r)) (cosh (* 2 y mu)))
        (sinh (* 2 y mu)))
     (expt (+ (cosh (* 2 y mu))
              (* (+ d (* -1 r)) (sinh (* 2 y mu))))
           -1/2)))

(defun-with-z-and-chi n (time y)
  (declare (double-float y)
           (ignore time))
  (* (expt (+ d (* -1 r)) -1)
     (+ (* (+ d (* -1 r)) (cosh (* 2 y mu))) (sinh (* 2 y mu)))
     (expt (+ (cosh (* 2 y mu))
              (* (+ d (* -1 r)) (sinh (* 2 y mu)))) -1/2)))

(defun-with-z-and-chi dn/dt (time y)
  (declare (ignore time y))
  0.0d0)

(defun-with-z-and-chi ndash (time y)
  (declare (double-float y)
           (ignore time))
  (* (expt (+ d (* -1 r)) -1)
     mu
     1/2
     (expt (+ (cosh (* 2 y mu)) (* (+ d (* -1 r)) (sinh (* 2 y mu)))) -3/2)
     (+ (* -3 (+ -1 (expt (+ d (* -1 r)) 2)))
        (* (+ 1 (expt (+ d (* -1 r)) 2)) (cosh (* 4 y mu)))
        (* 2 (+ d (* -1 r)) (sinh (* 4 y mu))))))
