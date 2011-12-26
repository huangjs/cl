(in-package #:geodesics)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (safety 0))))

#| Static with broken Z_2 and tuning. d=0 , r=1 is RS2 |#

(defun-with-z-and-chi a (time y)
  (declare (double-float y)
           (ignore time))
  (+ (cosh (* mu y))
     (* (- d r) (sinh (* mu y)))))

(defun-with-z-and-chi da/dt (time y)
  (declare (ignore time y))
  0.0d0)

(defun-with-z-and-chi adash (time y)
  (declare (double-float y)
           (ignore time))
  (+ (* mu (sinh (* mu y)))
     (* mu (- d r) (cosh (* mu y)))))

(defun-with-z-and-chi n (time y)
  (declare (double-float y)
           (ignore time))
  (+ (cosh (* mu y)) (* (- d r) (sinh (* mu y)))))

(defun-with-z-and-chi dn/dt (time y)
  (declare (ignore time y))
  0.0d0)

(defun-with-z-and-chi ndash (time y)
  (declare (double-float y)
           (ignore time))
  (+ (* mu (sinh (* mu y)))
     (* mu (- d r) (cosh (* mu y)))))

