;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Written by Raymond Toy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: quadpack.lisp,v 1.4 2001/02/26 22:56:07 rtoy Exp $
;;;
;;; $Log: quadpack.lisp,v $
;;; Revision 1.4  2001/02/26 22:56:07  rtoy
;;; Add a few more test cases/examples.
;;;
;;; Revision 1.3  2001/02/26 17:43:06  rtoy
;;; o Fix typo:  integrate-gnq -> integrate-qng
;;; o Add some examples.
;;;
;;; Revision 1.2  2001/02/23 18:46:09  rtoy
;;; Make sure the integral limits and other parameters that should be
;;; double-float's are double-float's before calling the routines.
;;;
;;; Revision 1.1  2001/02/23 18:07:29  rtoy
;;; Initial revision
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is a simple interface to the QUADPACK routines for numerical
;;; integration.

(in-package "MATLISP")

(defun integrate-qng (f a b &key (epsabs 0d0) (epsrel 1d-5))
  "Compute the integral of f(x) from a to b.

	  b
	 /
	 [
	 I    f(x) dx
	 ]
	 /
	  a

 f      - double precision
          function subprogram defining the integrand function
          f(x)

 a      - lower limit of integration

 b      - upper limit of integration

 epsabs - absolute accuracy requested
 epsrel - relative accuracy requested
          if  epsabs.le.0
          and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
          the routine will end with ier = 6.

 RETURN
 result - approximation to the integral i
          result is obtained by applying the 21-point
          gauss-kronrod rule (res21) obtained by optimal
          addition of abscissae to the 10-point gauss rule
          (res10), or by applying the 43-point rule (res43)
          obtained by optimal addition of abscissae to the
          21-point gauss-kronrod rule, or by applying the
          87-point rule (res87) obtained by optimal addition
          of abscissae to the 43-point rule.

 ier    - ier = 0 normal and reliable termination of the
                  routine. it is assumed that the requested
                  accuracy has been achieved.
          ier.gt.0 abnormal termination of the routine. it is
                  assumed that the requested accuracy has
                  not been achieved.
 abserr - estimate of the modulus of the absolute error,
          which should equal or exceed abs(i-result)

 neval  - number of integrand evaluations

 ERROR INDICATIONS
          ier = 1 the maximum number of steps has been
                  executed. the integral is probably too
                  difficult to be calculated by dqng.
              = 6 the input is invalid, because
                  epsabs.le.0 and
                  epsrel.lt.max(50*rel.mach.acc.,0.5d-28).
                  result, abserr and neval are set to zero.
"
  (multiple-value-bind (junk-f junk-a junk-b junk-epsabs junk-epsrel result abserr neval ier)
      (quadpack:dqng f (float a 1d0) (float b 1d0) (float epsabs 1d0) (float epsrel 1d0)
		     0d0 0d0 0 0)
    (declare (ignore junk-f junk-a junk-b junk-epsabs junk-epsrel))
    (values result ier abserr neval)))

(defun integrate-qag (f a b &key (key 0) (limit 100) (epsabs 0d0) (epsrel 1d-5))
  "Compute the integral of f(x) from a to b.

	  b
	 /
	 [
	 I    f(x) dx
	 ]
	 /
	  a

  f      - double precision
           function subprogam defining the integrand
           function f(x). the actual name for f needs to be

  a      - double precision
           lower limit of integration

  b      - double precision
           upper limit of integration

  epsabs - double precision
           absolute accoracy requested
  epsrel - double precision
           relative accuracy requested
           if  epsabs.le.0
           and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
           the routine will end with ier = 6.

  key    - integer
           key for choice of local integration rule
           a gauss-kronrod pair is used with
             7 - 15 points if key.lt.2,
            10 - 21 points if key = 2,
            15 - 31 points if key = 3,
            20 - 41 points if key = 4,
            25 - 51 points if key = 5,
            30 - 61 points if key.gt.5.

  limit - limit determines the maximum number of subintervals
          in the partition of the given integration interval
          (a,b), limit.ge.1.
          if limit.lt.1, the routine will end with ier = 6.

 RETURN
  result - approximation to the integral

  ier    - ier = 0 normal and reliable termination of the
                   routine. it is assumed that the requested
                   accuracy has been achieved.
           ier.gt.0 abnormal termination of the routine
                   the estimates for result and error are
                   less reliable. it is assumed that the
                   requested accuracy has not been achieved.
  abserr - estimate of the modulus of the absolute error,
           which should equal or exceed abs(i-result)

  neval  - number of integrand evaluations

  last   - on return, last equals the number of
           subintervals produced in the subdivision process,
           which determines the significant number of
           elements actually in the work arrays.

 ERROR MESSAGES
           ier = 1 maximum number of subdivisions allowed
                   has been achieved. one can allow more
                   subdivisions by increasing the value of
                   limit (and taking the according dimension
                   adjustments into account). however, if
                   this yield no improvement it is advised
                   to analyze the integrand in order to
                   determine the integration difficulaties.
                   if the position of a local difficulty can
                   be determined (i.e.singularity,
                   discontinuity within the interval) one
                   will probably gain from splitting up the
                   interval at this point and calling the
                   integrator on the subranges. if possible,
                   an appropriate special-purpose integrator
                   should be used which is designed for
                   handling the type of difficulty involved.
               = 2 the occurrence of roundoff error is
                   detected, which prevents the requested
                   tolerance from being achieved.
               = 3 extremely bad integrand behaviour occurs
                   at some points of the integration
                   interval.
               = 6 the input is invalid, because
                   (epsabs.le.0 and
                    epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
                   or limit.lt.1 or lenw.lt.limit*4.
                   result, abserr, neval, last are set
                   to zero.
                   except when lenw is invalid, iwork(1),
                   work(limit*2+1) and work(limit*3+1) are
                   set to zero, work(1) is set to a and
                   work(limit+1) to b.

"
  (let* ((lenw (* 4 limit))
	 (iwork (make-array limit :element-type '(signed-byte 32)))
	 (work (make-array lenw :element-type 'double-float)))
    (multiple-value-bind (z-f z-a z-b z-epsabs z-epsrel z-key
			      result abserr neval ier
			      z-limit z-lenw
			      last
			      z-iwork z-work)
	(quadpack:dqag f (float a 1d0) (float b 1d0) (float epsabs 1d0) (float epsrel 1d0)
		       key 0d0 0d0 0 0 limit lenw 0 iwork work)
      (declare (ignore z-f z-a z-b z-epsabs z-epsrel z-key
		       z-limit z-lenw z-iwork z-work))
      (values result ier abserr neval last))))

(defun integrate-qags (f a b &key (epsabs 0d0) (epsrel 1d-5) (limit 200))
  "Compute the integral of f(x) from a to b.

	  b
	 /
	 [
	 I    f(x) dx
	 ]
	 /
	  a

  f      - function subprogram defining the integrand
           function f(x).

  a      - lower limit of integration

  b      - upper limit of integration

  epsabs - absolute accuracy requested
  epsrel - relative accuracy requested
           if  epsabs.le.0
           and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
           the routine will end with ier = 6.

 RETURN
  result - approximation to the integral

  ier    - integer
           ier = 0 normal and reliable termination of the
                   routine. it is assumed that the requested
                   accuracy has been achieved.
           ier.gt.0 abnormal termination of the routine
                   the estimates for integral and error are
                   less reliable. it is assumed that the
                   requested accuracy has not been achieved.
  abserr - estimate of the modulus of the absolute error,
           which should equal or exceed abs(i-result)

  neval  - number of integrand evaluations

  last   - on return, last equals the number of
           subintervals produced in the subdivision process,
           which determines the significant number of
           elements actually in the work arrays.

 ERROR MESSAGES
   ier = 1 maximum number of subdivisions allowed
           has been achieved. one can allow more sub-
           divisions by increasing the value of limit
           (and taking the according dimension
           adjustments into account. however, if
           this yields no improvement it is advised
           to analyze the integrand in order to
           determine the integration difficulties. if
           the position of a local difficulty can be
           determined (e.g. singularity,
           discontinuity within the interval) one
           will probably gain from splitting up the
           interval at this point and calling the
           integrator on the subranges. if possible,
           an appropriate special-purpose integrator
           should be used, which is designed for
           handling the type of difficulty involved.
       = 2 the occurrence of roundoff error is detec-
           ted, which prevents the requested
           tolerance from being achieved.
           the error may be under-estimated.
       = 3 extremely bad integrand behaviour
           occurs at some points of the integration
           interval.
       = 4 the algorithm does not converge.
           roundoff error is detected in the
           extrapolation table. it is presumed that
           the requested tolerance cannot be
           achieved, and that the returned result is
           the best which can be obtained.
       = 5 the integral is probably divergent, or
           slowly convergent. it must be noted that
           divergence can occur with any other value
           of ier.
       = 6 the input is invalid, because
           (epsabs.le.0 and
            epsrel.lt.max(50*rel.mach.acc.,0.5d-28)
           or limit.lt.1 or lenw.lt.limit*4.
           result, abserr, neval, last are set to
           zero.except when limit or lenw is invalid,
           iwork(1), work(limit*2+1) and
           work(limit*3+1) are set to zero, work(1)
           is set to a and work(limit+1) to b.

 DIMENSIONING PARAMETERS
  limit - limit determines the maximum number of subintervals
          in the partition of the given integration interval
          (a,b), limit.ge.1.
          if limit.lt.1, the routine will end with ier = 6.
"
  (let* ((lenw (* 4 limit))
	 (iwork (make-array limit :element-type '(signed-byte 32)))
	 (work (make-array lenw :element-type 'double-float)))
    (multiple-value-bind (z-f z-a z-b z-epsabs z-epsrel
			      result abserr neval ier
			      z-limit z-lenw
			      last
			      z-iwork z-work)
	(quadpack:dqags f (float a 1d0) (float b 1d0) (float epsabs 1d0) (float epsrel 1d0)
			0d0 0d0 0 0 limit lenw 0 iwork work)
      (declare (ignore z-f z-a z-b z-epsabs z-epsrel
		       z-limit z-lenw z-iwork z-work))
      (values result ier abserr neval last))))


(defun integrate-qagi (f infinity &key (bound 0d0) (epsabs 0d0) (epsrel 1d-5) (limit 200))
  "Compute the infinite integral of f(x):

	  b
	 /
	 [
	 I    f(x) dx
	 ]
	 /
	  a

where a and/or b may be infinite.

  f      - function subprogram defining the integrand
           function f(x).

  infinity - indicating the kind of integration range involved
              :pos-infinity corresponds to  (bound,+infinity),
              :neg-infinity             to  (-infinity,bound),
              :infinite   2             to (-infinity,+infinity).

  bound  - finite bound of integration range
           (has no meaning if interval is doubly-infinite)

  epsabs - double precision
           absolute accuracy requested
  epsrel - double precision
           relative accuracy requested
           if  epsabs.le.0
           and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
           the routine will end with ier = 6.

  limit - dimensioning parameter for iwork
          limit determines the maximum number of subintervals
          in the partition of the given integration interval
          (a,b), limit.ge.1.
          if limit.lt.1, the routine will end with ier = 6.

 RETURN
  result - double precision
           approximation to the integral

  ier    - integer
           ier = 0 normal and reliable termination of the
                   routine. it is assumed that the requested
                   accuracy has been achieved.
         - ier.gt.0 abnormal termination of the routine. the
                   estimates for result and error are less
                   reliable. it is assumed that the requested
                   accuracy has not been achieved.
  abserr - double precision
           estimate of the modulus of the absolute error,
           which should equal or exceed abs(i-result)

  neval  - integer
           number of integrand evaluations

  last   - on return, last equals the number of
           subintervals produced in the subdivision process,
           which determines the significant number of
           elements actually in the work arrays.
 ERROR MESSAGES
           ier = 1 maximum number of subdivisions allowed
                   has been achieved. one can allow more
                   subdivisions by increasing the value of
                   limit (and taking the according dimension
                   adjustments into account). however, if
                   this yields no improvement it is advised
                   to analyze the integrand in order to
                   determine the integration difficulties. if
                   the position of a local difficulty can be
                   determined (e.g. singularity,
                   discontinuity within the interval) one
                   will probably gain from splitting up the
                   interval at this point and calling the
                   integrator on the subranges. if possible,
                   an appropriate special-purpose integrator
                   should be used, which is designed for
                   handling the type of difficulty involved.
               = 2 the occurrence of roundoff error is
                   detected, which prevents the requested
                   tolerance from being achieved.
                   the error may be under-estimated.
               = 3 extremely bad integrand behaviour occurs
                   at some points of the integration
                   interval.
               = 4 the algorithm does not converge.
                   roundoff error is detected in the
                   extrapolation table.
                   it is assumed that the requested tolerance
                   cannot be achieved, and that the returned
                   result is the best which can be obtained.
               = 5 the integral is probably divergent, or
                   slowly convergent. it must be noted that
                   divergence can occur with any other value
                   of ier.
               = 6 the input is invalid, because
                   (epsabs.le.0 and
                    epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
                    or limit.lt.1 or leniw.lt.limit*4.
                   result, abserr, neval, last are set to
                   zero. exept when limit or leniw is
                   invalid, iwork(1), work(limit*2+1) and
                   work(limit*3+1) are set to zero, work(1)
                   is set to a and work(limit+1) to b.

"
  (let* ((inf (ecase infinity
		(:neg-infinity -1)
		(:pos-infinity 1)
		(:infinite 2)))
	 (lenw (* 4 limit))
	 (iwork (make-array limit :element-type '(signed-byte 32)))
	 (work (make-array lenw :element-type 'double-float)))
    (multiple-value-bind (z-f z-bound z-inf z-epsabs z-epsrel
			      result abserr neval ier
			      z-limit z-lenw
			      last
			      z-iwork z-work)
	(quadpack:dqagi f (float bound 1d0) inf (float epsabs 1d0) (float epsrel 1d0)
			0d0 0d0 0 0 limit lenw 0 iwork work)
      (declare (ignore z-f z-bound z-inf z-epsabs z-epsrel
		       z-limit z-lenw z-iwork z-work))
      (values result ier abserr neval last))))

(defun integrate-qaws (f a b alfa beta integr &key (epsabs 0d0) (epsrel 1d-5) (limit 200))
  "Compute the integral
	 b
	/
	[
        I  f(x) w(x) dx
	]
	/
	 a

where w(x) is the weight function below.

  f      - function subprogram defining the integrand
           function f(x). the actual name for f needs to be
           declared e x t e r n a l in the driver program.

  a      - lower limit of integration

  b      - upper limit of integration, b.gt.a
           if b.le.a, the routine will end with ier = 6.

  alfa   - parameter in the integrand function, alfa.gt.(-1)
           if alfa.le.(-1), the routine will end with
           ier = 6.

  beta   - parameter in the integrand function, beta.gt.(-1)
           if beta.le.(-1), the routine will end with
           ier = 6.

  integr - indicates which weight function is to be used
           = 1  (x-a)**alfa*(b-x)**beta
           = 2  (x-a)**alfa*(b-x)**beta*log(x-a)
           = 3  (x-a)**alfa*(b-x)**beta*log(b-x)
           = 4  (x-a)**alfa*(b-x)**beta*log(x-a)*log(b-x)
           if integr.lt.1 or integr.gt.4, the routine
           will end with ier = 6.

  epsabs - absolute accuracy requested
  epsrel - relative accuracy requested
           if  epsabs.le.0
           and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
           the routine will end with ier = 6.

  limit  - dimensioning parameter for iwork
           limit determines the maximum number of
           subintervals in the partition of the given
           integration interval (a,b), limit.ge.2.
           if limit.lt.2, the routine will end with ier = 6.
 RETURN
  result - double precision
           approximation to the integral

  ier    - integer
           ier = 0 normal and reliable termination of the
                   routine. it is assumed that the requested
                   accuracy has been achieved.
           ier.gt.0 abnormal termination of the routine
                   the estimates for the integral and error
                   are less reliable. it is assumed that the
                   requested accuracy has not been achieved.
  abserr - double precision
           estimate of the modulus of the absolute error,
           which should equal or exceed abs(i-result)

  neval  - integer
           number of integrand evaluations

  last   - on return, last equals the number of
           subintervals produced in the subdivision process,
           which determines the significant number of
           elements actually in the work arrays.

 ERROR MESSAGES
           ier = 1 maximum number of subdivisions allowed
                   has been achieved. one can allow more
                   subdivisions by increasing the value of
                   limit (and taking the according dimension
                   adjustments into account). however, if
                   this yields no improvement it is advised
                   to analyze the integrand, in order to
                   determine the integration difficulties
                   which prevent the requested tolerance from
                   being achieved. in case of a jump
                   discontinuity or a local singularity
                   of algebraico-logarithmic type at one or
                   more interior points of the integration
                   range, one should proceed by splitting up
                   the interval at these points and calling
                   the integrator on the subranges.
               = 2 the occurrence of roundoff error is
                   detected, which prevents the requested
                   tolerance from being achieved.
               = 3 extremely bad integrand behaviour occurs
                   at some points of the integration
                   interval.
               = 6 the input is invalid, because
                   b.le.a or alfa.le.(-1) or beta.le.(-1) or
                   or integr.lt.1 or integr.gt.4 or
                   (epsabs.le.0 and
                    epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
                   or limit.lt.2 or lenw.lt.limit*4.
                   result, abserr, neval, last are set to
                   zero. except when lenw or limit is invalid
                   iwork(1), work(limit*2+1) and
                   work(limit*3+1) are set to zero, work(1)
                   is set to a and work(limit+1) to b.


"
  (let* ((lenw (* 4 limit))
	 (iwork (make-array limit :element-type '(signed-byte 32)))
	 (work (make-array lenw :element-type 'double-float)))
    (multiple-value-bind (z-f z-a z-b z-alfa z-beta z-integr z-epsabs z-epsrel
			      result abserr neval ier
			      z-limit z-lenw
			      last
			      z-iwork z-work)
	(quadpack:dqaws f (float a 1d0) (float b 1d0) (float alfa 1d0) (float beta 1d0)
			integr (float epsabs 1d0) (float epsrel 1d0)
			0d0 0d0 0 0 limit lenw 0 iwork work)
      (declare (ignore z-f z-a z-b z-alfa z-beta z-integr z-epsabs z-epsrel
		       z-limit z-lenw z-iwork z-work))
      (values result ier abserr neval last))))


(defun integrate-qawc (f a b c &key (epsabs 0d0) (epsrel 1d-5) (limit 200))
  "Compute the Cauchy principal value of the integral:

	   b
	  /
	  [  f(x)
	  I  ----- dx
	  ]  x - c
	  /
	   a

  f      - function subprogram defining the integrand
           function f(x).

  a      - under limit of integration

  b      - upper limit of integration

  c      - parameter in the weight function, c.ne.a, c.ne.b.
           if c = a or c = b, the routine will end with
           ier = 6 .

  epsabs - absolute accuracy requested
  epsrel - relative accuracy requested
           if  epsabs.le.0
           and epsrel.lt.max(50*rel.mach.acc.,0.5d-28),
           the routine will end with ier = 6.

  limit - limit determines the maximum number of subintervals
          in the partition of the given integration interval
          (a,b), limit.ge.1.
          if limit.lt.1, the routine will end with ier = 6.

 RETURN
  result - double precision
           approximation to the integral

  ier    - integer
           ier = 0 normal and reliable termination of the
                   routine. it is assumed that the requested
                   accuracy has been achieved.
           ier.gt.0 abnormal termination of the routine
                   the estimates for integral and error are
                   less reliable. it is assumed that the
                   requested accuracy has not been achieved.
  abserr - double precision
           estimate or the modulus of the absolute error,
           which should equal or exceed abs(i-result)

  neval  - integer
           number of integrand evaluations

 ERROR MESSAGES
           ier = 1 maximum number of subdivisions allowed
                   has been achieved. one can allow more sub-
                   divisions by increasing the value of limit
                   (and taking the according dimension
                   adjustments into account). however, if
                   this yields no improvement it is advised
                   to analyze the integrand in order to
                   determine the integration difficulties.
                   if the position of a local difficulty
                   can be determined (e.g. singularity,
                   discontinuity within the interval) one
                   will probably gain from splitting up the
                   interval at this point and calling
                   appropriate integrators on the subranges.
               = 2 the occurrence of roundoff error is detec-
                   ted, which prevents the requested
                   tolerance from being achieved.
               = 3 extremely bad integrand behaviour occurs
                   at some points of the integration
                   interval.
               = 6 the input is invalid, because
                   c = a or c = b or
                   (epsabs.le.0 and
                    epsrel.lt.max(50*rel.mach.acc.,0.5d-28))
                   or limit.lt.1 or lenw.lt.limit*4.
                   result, abserr, neval, last are set to
                   zero. exept when lenw or limit is invalid,
                   iwork(1), work(limit*2+1) and
                   work(limit*3+1) are set to zero, work(1)
                   is set to a and work(limit+1) to b.
"
  (let* ((lenw (* 4 limit))
	 (iwork (make-array limit :element-type '(signed-byte 32)))
	 (work (make-array lenw :element-type 'double-float)))
    (multiple-value-bind (z-f z-a z-b z-c z-epsabs z-epsrel
			      result abserr neval ier
			      z-limit z-lenw
			      last
			      z-iwork z-work)
	(quadpack:dqawc f (float a 1d0) (float b 1d0) (float c 1d0) (float epsabs 1d0) (float epsrel 1d0)
			0d0 0d0 0 0 limit lenw 0 iwork work)
      (declare (ignore z-f z-a z-b z-c z-epsabs z-epsrel
		       z-limit z-lenw z-iwork z-work))
      (values result ier abserr neval last))))

#||
;; Here are some examples of usage.  The sample integrals were taken
;; from the QUADPACK book and some test code from F2CL.
;;

;; Test 1
;; Compute integral e^(alpha*x)*log(1/x) from 0 to 1.  The analytical
;; solution is (1+alpha)^(-2).
;;
;; alpha = -0.9(0.1)0(0.2)2.6
;; QAG with key 1, 3, 6
;;
;; For key = 1, 3, 6: fails for alpha = -0.9 (ier = 3)
(defun tst1 (&key (limit 200) (relerr 1d-8))
  (labels ((abs-error (est true)
	     (abs (- est true)))
	   (soln (alpha)
	     (declare (double-float alpha))
	     (expt (+ 1d0 alpha) -2))
	   (quad (alpha key)
	     (declare (double-float alpha))
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qag #'(lambda (x)
				    (declare (type (double-float 0d0) x))
				    (* (expt x alpha)
				       (log (/ x))))
				0d0 1d0 :key key :limit limit :epsrel relerr)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))
	       )))
    (format t "Test integral 1:~%")
    (format t "
           1
          /                        
          [    alpha x                        -2
        - I  %E        LOG(x) dx = (alpha + 1) 
          ]
          /
           0

for alpha = -0.9(0.1)0(0.2)2.6

Characteristics: end-point singularity of integrand or derivative

Best integrator: QAG with key = 1 more efficient for pronounced
singularity.

Expect non-zero error codes for

alpha = -0.9 (ier = 3)
~3%")

    (dolist (key '(1 3 6))
      (format t "QAG, Key = ~d~%" key)
      (format t
	      "alpha   est result              est abserr             neval    ier   true answer            true abs err~%")
      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad alpha key))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad alpha key)))))

;; Test 4 (same integral as 1)
;; Compute integral e^(alpha*x)*log(1/x) from 0 to 1.  The analytical
;; solution is e^((1+alpha)^(-2)).
;;
;; DQNG, DQAGS, DQAG (key = 1)
;;
;; Failures:
;; DQNG: alpha <= 1.0 (ier = 1)
;; DQAG: alpha = -0.9 (ier = 3)
(defun tst4 (&key (limit 200) (relerr 1d-8))
  (labels ((abs-error (est true)
	     (abs (- est true)))
	   (soln (alpha)
	     (declare (double-float alpha))
	     (expt (+ 1d0 alpha) -2))
	   (quad-qng (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qng #'(lambda (x)
				    (declare (type (double-float 0d0) x))
				    (* (expt x alpha)
				       (log (/ x))))
				0d0 1d0 :epsrel relerr)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))
	       ))
	   (quad-qags (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qags #'(lambda (x)
				     (declare (type (double-float 0d0) x))
				     (* (expt x alpha)
					(log (/ x))))
				 0d0 1d0 :epsrel relerr :limit limit)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))
	       ))
	   (quad-qag (alpha key)
	     (declare (double-float alpha))
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qag #'(lambda (x)
				    (declare (type (double-float 0d0) x))
				    (* (expt x alpha)
				       (log (/ x))))
				0d0 1d0 :epsrel relerr :key key :limit limit)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))
	       )))
    (format t "Test integral 4:~%")
    (format t "
           1
          /                        
          [    alpha x                        -2
        - I  %E        LOG(x) dx = (alpha + 1) 
          ]
          /
           0

for alpha = -0.9(0.1)0(0.2)2.6

Best integrator:  QAGS without failures, more efficient and accurate than QAG

Expect non-zero error codes for

QNG: alpha <= 1.0 (ier = 1)
QAG: alpha = -0.9 (ier = 3)
~3%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))

      (format t "~2&QNG~%")
      (format t "~A~%" header)

      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad-qng alpha))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad-qng alpha))

      (format t "~2&DQAGS~%")
      (format t "~A~%" header)
      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad-qags alpha))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad-qags alpha))

      (format t "~2&DQAG~%")
      (format t "~A~%" header)
      (do ((alpha -0.9d0 (+ alpha 0.1d0)))
	  ((> alpha 0d0))
	(quad-qag alpha 1))

      (do ((alpha 0.2d0 (+ alpha 0.2d0)))
	  ((> alpha 2.6d0))
	(quad-qag alpha 1)))))

;; Test 9
;; integral (1 - x*x)^(-1/2)/(x + 1 + 2^(-alpha))
;; Solution: pi*((1+2^(-alpha))^2-1)^(-1/2)
;;
;; alpha = 1(1)20
;;
;; DQAGS, DQAWS
;;
;; Failures:
;; DQAGS: alpha >= 17 (ier = 4 or 5)
(defun tst9 (&key (limit 200) (relerr 1d-8))
  (labels ((abs-error (est true)
	     (abs (- est true)))
	   (soln (alpha)
	     (declare (double-float alpha))
	     (let ((2alpha (expt 2 (- alpha))))
	       (float (/ pi
			 (sqrt (* 2alpha (+ 2 2alpha))))
		      1d0)))
	   (quad (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qags #'(lambda (x)
				     (declare (type (double-float -1d0 1d0) x))
				     (/ (/ (sqrt (- 1d0 (* x x))))
					(+ x 1d0 (expt 2d0 (- alpha)))))
				 -1d0 1d0 :epsrel relerr
				 :limit limit)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha)))))
	   (quad-qaws (alpha)
	     (declare (double-float alpha))
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qaws #'(lambda (x)
				     (declare (type (double-float -1d0 1d0) x))
				     (/ (float (+ x 1d0 (expt 2d0 (- alpha))) 1d0)))
				 -1d0 1d0
				 -0.5d0 -0.5d0 1
				 :epsrel relerr
				 :limit limit)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha))))))
    (format t "Test integral 9:

      1
     /                                                   alpha      
     [                  1		            %PI 2	      
     I    ----------------------------- dx =    ------------------  
     ]           1                   2	                alpha	      
     /    (x + ------ + 1) SQRT(1 - x )	        SQRT(2 2      + 1)  
      - 1       alpha
               2



for alpha = 1(1)20

Characteristics: singular integrand factor (and end-points) times
factor which increases the effect of the singularity as alpha
increases.

Best integrator: QAWS wider applicable, more efficient and accurate.

Expect non-zero error code for:

 QAGS: alpha >= 17 (ier = 4 or 5)
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAGS~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad alpha))
      (format t "~2&QAWS~%")
      (format t "~A~%" header)
      (do ((alpha 1d0 (+ alpha 1d0)))
	  ((> alpha 20d0))
	(quad-qaws alpha)))))

;; Test 17
;; Cauchy integral 0 to 5 2^(-alpha)*(((x-1)^2 + 4^(-alpha))*(x-2))^(-1)
;; Solution: (2^(-alpha)*ln(3/2) - 2^(-alpha-1)*ln((16 + 4^(-alpha))/(1+4^(-alpha))) - atan(2^(alpha +  2)) - atan(2^alpha))/(1 + 4^(-alpha))
;;
;; alpha = 0(1)10
;;
;; DQAWC
;;
;; Failures: none
(defun tst17 (&key (limit 200) (relerr 1d-8))
  (labels ((abs-error (est true)
	     (abs (- est true)))
	   (soln (alpha)
	     (declare (type (double-float 0d0) alpha))
	     (/ (- (* (expt 2 (- alpha))
		      (log 1.5d0))
		   (* (expt 2 (- (- alpha) 1))
		      (log (/ (+ 16 (expt 4 (- alpha)))
			      (+ 1 (expt 4 (- alpha))))))
		   (atan (expt 2 (+ alpha 2)))
		   (atan (expt 2 alpha)))
		(+ 1 (expt 4 (- alpha)))))
	   (quad (alpha)
	     (multiple-value-bind (result ier abserr neval)
		 (integrate-qawc #'(lambda (x)
				     (/ (expt 2 (- alpha))
					(+ (expt (- x 1) 2)
					   (expt 4 (- alpha)))))
				 0d0 5d0 2d0
				 :epsrel relerr
				 :limit limit)
	       (format t "~5f  ~22,15,2e  ~22,15,2e  ~5d  ~5d  ~22,15,2e ~10,3,2e~%"
		       alpha result abserr neval ier
		       (soln alpha) (abs-error result (soln alpha))))))
    (format t "Test integral 17:

Cauchy principal value of

    5
   /
   [               1
   I  --------------------------- dx
   ]          2     1
   /  ((x - 1)  + ------) (x - 2)
    0              alpha
                  4
   ---------------------------------
                 alpha
                2


							       1
							     ------ + 16       3
							      alpha         ln(-)
		 alpha + 2          alpha     - alpha - 1    4                 2
	 - ATAN(2         ) - ATAN(2     ) - 2            ln(-----------) + ------
							       1             alpha
							     ------ + 1     2
							      alpha
							     4
    =    -------------------------------------------------------------------------
					  1
					------ + 1
					 alpha
					4

for alpha = 0(1)10

Expect no non-zero error codes
~2%")
    (let ((header
	   "alpha   est result              est abserr             neval    ier   true answer            true abs err"))
      (format t "~2&QAWC~%")
      (format t "~A~%" header)
      (do ((alpha 0.0d0 (+ alpha 1d0)))
	  ((> alpha 10d0))
	(quad alpha)))))

||#
