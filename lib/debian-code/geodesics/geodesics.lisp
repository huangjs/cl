(defpackage #:geodesics
  (:nicknames #:gd)
  (:use #:common-lisp)
  (:export :dx/dp :d2y/dp2 :d2t/dp2 :k :kappa :mu :rho :A :c :f :d :r :gamma))

(in-package "GEODESICS")

;;; WARNING! WARNING! We have here global, special variables without
;;; `*'s round their names. This will cause breakage if we're not
;;; careful.

(defvar k 0.2d0 "Planck Mass scale(ish)")
(defvar kappa 0.0d0 "Effective initial velocity")

;;; should be set to k^2rho/3 -- symbol-macro?
;;;(define-symbol-macro mu (/ (* k k rho) 3))
(defvar mu 1.0d0 "Warp factor")

(defvar rho 1.0d0 "Density of the universe (parameter in chi)")

;;; the value here is a little deceptive -- it gets set in the general
;;; case to match up with the a0 solution.
(defvar A 1.0d0 "Scaling of a()") ; note case obfuscation -- for reader's clarity only
(defvar c 0.0d0 "Dark radiation term")
(defvar f 0.0d0 "Asymmetry term")
(defvar d 0.0d0 "Broken Z_2 term (same as f?)")
(defvar r 1.0d0 "Tuning of brane tension and bulk \Lambda")
(defvar gamma 1.0d0 "Factor needed for integration of a0 -- NOT INDEPENDENT OF A")

(declaim (double-float k kappa mu rho A c f d r gamma))

(defmacro with-z-and-chi (&body body)
  `(symbol-macrolet ((z (exp (* mu y)))
		     (dz/dy (* z mu)))
     (let* ((chi (if (= f 0.0d0)
		     (/ (* 2/3 k k rho (+ (* 1/3 k k (+ 1 c) rho time time) time)))
		   (/ gamma rho (expt (a0 time) 4))))
	    (dchi/dt (if (= f 0.0d0)
			 (* chi chi -2/3 k k rho (+ (* 2/3 k k (+ 1 c) rho time) 1))
		       (/ (* -4 gamma #i"sqrt((1+c)*rho*k^^4*gamma/(18*a0(time)^^4)+k^^4*(gamma^^2/a0(time)^^8+f^^2*rho^^2*gamma^^2/(a0(time)^^8*(gamma/a0(time)^^4+rho)^^2))/36)")
			  rho (expt (a0 time) 5)))))
		      
       ,@body)))

(defmacro defun-with-z-and-chi (function-name lambda-list &body body)
  `(defun ,function-name ,lambda-list
     (with-z-and-chi
      ,@body)))

