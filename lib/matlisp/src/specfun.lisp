;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;; Fortran interface to the special function package SPECFUN, TOMS 715.
;;; Written by Raymond Toy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: specfun.lisp,v 1.5 2001/12/28 19:19:23 rtoy Exp $
;;;
;;; $Log: specfun.lisp,v $
;;; Revision 1.5  2001/12/28 19:19:23  rtoy
;;; Add some more documentation for the special functions.
;;;
;;; Revision 1.4  2001/12/28 16:54:35  rtoy
;;; Add documentation strings for the special functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(def-fortran-routine anorm :double-float
  "
 This function evaluates the normal distribution function:

                              / x
                     1       |       -t*t/2
          P(x) = ----------- |      e       dt
                 sqrt(2 pi)  |
                             /-oo

"
  (x :double-float :input))

(def-fortran-routine besei0 :double-float
  "
Computes the modified Bessel function of the first kind of order zero,
multiplied by exp(-|x|).
"
  (x :double-float :input))

(def-fortran-routine besei1 :double-float
  "
Computes the modified Bessel function of the first kind of order one,
multiplied by exp(-|x|).
"
  (x :double-float :input))

(def-fortran-routine besek0 :double-float
  "
Computes the modified Bessel function of the second kind of order zero,
multiplied by exp(-|x|)  x > 0.
"
  (x :double-float :input))

(def-fortran-routine besek1 :double-float
  "
Computes the modified Bessel function of the second kind of order one,
multiplied by exp(-|x|)  0 < x.
"
  (x :double-float :input))

(def-fortran-routine besi0 :double-float
  "
Computes the modified Bessel function of the second kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besi1 :double-float
  "
Computes the modified Bessel function of the second kind of order one,
"
  (x :double-float :input))

(def-fortran-routine besj0 :double-float
  "
Computes the Bessel function of the first kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besj1 :double-float
  "
Computes the Bessel function of the first kind of order one,
"
  (x :double-float :input))

(def-fortran-routine besk0 :double-float
  "
Computes the modified Bessel function of the second kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besk1 :double-float
  "
Computes the modified Bessel function of the second kind of order one,
"
  (x :double-float :input))

(def-fortran-routine besy0 :double-float
  "
Computes the Bessel function of the second kind of order zero,
"
  (x :double-float :input))

(def-fortran-routine besy1 :double-float
  "
Computes the Bessel function of the second kind of order one,
"
  (x :double-float :input))

(def-fortran-routine daw :double-float
  "
 This function program evaluates Dawson's integral,

                       2  / x   2
                     -x   |    t
             F(x) = e     |   e    dt
                          |
                          / 0

   for a real argument x.
"
  (x :double-float :input))

(def-fortran-routine derf :double-float
  "
Compute erf(x)
"
  (x :double-float :input))

(def-fortran-routine derfc :double-float
  "
Compute erfc(x) = 1 - erf(x)
"
  (x :double-float :input))

(def-fortran-routine derfcx :double-float
  "
Compute erfcx(x) = exp(x*x)*erfc(x)
"
  (x :double-float :input))

(def-fortran-routine dgamma :double-float
  "
Compute Gamma(x) for real x
"
  (x :double-float :input))

(def-fortran-routine dlgama :double-float
  "
Compute log(gamma(x)) for positive real x.
"
  (x :double-float :input))

(def-fortran-routine ei :double-float
  "
Compute exponential integral Ei(x), x real.
"
  (x :double-float :input))

(def-fortran-routine eone :double-float
  "
Compute exponential integral E1(x), x real.
"
  (x :double-float :input))

(def-fortran-routine expei :double-float
  "
Compute the function exp(-x)*Ei(x), x real.
"
  (x :double-float :input))

(def-fortran-routine psi :double-float
  "
 This function program evaluates the logarithmic derivative of the
   gamma function,

      psi(x) = d/dx (gamma(x)) / gamma(x) = d/dx (ln gamma(x))

   for real x
"
  (x :double-float :input))

(def-fortran-routine ribesl :void
  "
  This routine calculates Bessel functions I SUB(N+ALPHA) (X)
  for non-negative argument X, and non-negative order N+ALPHA,
  with or without exponential scaling.


 Explanation of variables in the calling sequence

 X     - Working precision non-negative real argument for which
         I's or exponentially scaled I's (I*EXP(-X))
         are to be calculated.  If I's are to be calculated,
         X must be less than EXPARG (see below).
 ALPHA - Working precision fractional part of order for which
         I's or exponentially scaled I's (I*EXP(-X)) are
         to be calculated.  0 .LE. ALPHA .LT. 1.0.
 NB    - Integer number of functions to be calculated, NB .GT. 0.
         The first function calculated is of order ALPHA, and the
         last is of order (NB - 1 + ALPHA).
 IZE   - Integer type.  IZE = 1 if unscaled I's are to calculated,
         and 2 if exponentially scaled I's are to be calculated.
 B     - Working precision output vector of length NB.  If the routine
         terminates normally (NCALC=NB), the vector B contains the
         functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
         corresponding exponentially scaled functions.
 NCALC - Integer output variable indicating possible errors.
         Before using the vector B, the user should check that
         NCALC=NB, i.e., all orders have been calculated to
         the desired accuracy.  See error returns below.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (ize :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))

(def-fortran-routine rjbesl :void
  "
 This routine calculates Bessel functions J sub(N+ALPHA) (X)
   for non-negative argument X, and non-negative order N+ALPHA.


  Explanation of variables in the calling sequence.

   X     - working precision non-negative real argument for which
           J's are to be calculated.
   ALPHA - working precision fractional part of order for which
           J's or exponentially scaled J'r (J*exp(X)) are
           to be calculated.  0 <= ALPHA < 1.0.
   NB  - integer number of functions to be calculated, NB > 0.
           The first function calculated is of order ALPHA, and the
           last is of order (NB - 1 + ALPHA).
   B  - working precision output vector of length NB.  If RJBESL
           terminates normally (NCALC=NB), the vector B contains the
           functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
           corresponding exponentially scaled functions.
   NCALC - integer output variable indicating possible errors.
           Before using the vector B, the user should check that
           NCALC=NB, i.e., all orders have been calculated to
           the desired accuracy.  See Error Returns below.
  Error returns

    In case of an error,  NCALC .NE. NB, and not all J's are
    calculated to the desired accuracy.

    NCALC .LT. 0:  An argument is out of range. For example,
       NBES .LE. 0, ALPHA .LT. 0 or .GT. 1, or X is too large.
       In this case, B(1) is set to zero, the remainder of the
       B-vector is not calculated, and NCALC is set to
       MIN(NB,0)-1 so that NCALC .NE. NB.

    NB .GT. NCALC .GT. 0: Not all requested function values could
       be calculated accurately.  This usually occurs because NB is
       much larger than ABS(X).  In this case, B(N) is calculated
       to the desired accuracy for N .LE. NCALC, but precision
       is lost for NCALC .LT. N .LE. NB.  If B(N) does not vanish
       for N .GT. NCALC (because it is too small to be represented),
       and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
       significant figures of B(N) can be trusted.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))

(def-fortran-routine rkbesl :void
  "
  This FORTRAN 77 routine calculates modified Bessel functions
  of the second kind, K SUB(N+ALPHA) (X), for non-negative
  argument X, and non-negative order N+ALPHA, with or without
  exponential scaling.

  Explanation of variables in the calling sequence

  Description of output values ..

 X     - Working precision non-negative real argument for which
         K's or exponentially scaled K's (K*EXP(X))
         are to be calculated.  If K's are to be calculated,
         X must not be greater than XMAX (see below).
 ALPHA - Working precision fractional part of order for which
         K's or exponentially scaled K's (K*EXP(X)) are
         to be calculated.  0 .LE. ALPHA .LT. 1.0.
 NB    - Integer number of functions to be calculated, NB .GT. 0.
         The first function calculated is of order ALPHA, and the
         last is of order (NB - 1 + ALPHA).
 IZE   - Integer type.  IZE = 1 if unscaled K's are to be calculated,
         and 2 if exponentially scaled K's are to be calculated.
 BK    - Working precision output vector of length NB.  If the
         routine terminates normally (NCALC=NB), the vector BK
         contains the functions K(ALPHA,X), ... , K(NB-1+ALPHA,X),
         or the corresponding exponentially scaled functions.
         If (0 .LT. NCALC .LT. NB), BK(I) contains correct function
         values for I .LE. NCALC, and contains the ratios
         K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
 NCALC - Integer output variable indicating possible errors.
         Before using the vector BK, the user should check that
         NCALC=NB, i.e., all orders have been calculated to
         the desired accuracy.  See error returns below.


*******************************************************************

 Error returns

  In case of an error, NCALC .NE. NB, and not all K's are
  calculated to the desired accuracy.

  NCALC .LT. -1:  An argument is out of range. For example,
       NB .LE. 0, IZE is not 1 or 2, or IZE=1 and ABS(X) .GE.
       XMAX.  In this case, the B-vector is not calculated,
       and NCALC is set to MIN0(NB,0)-2  so that NCALC .NE. NB.
  NCALC = -1:  Either  K(ALPHA,X) .GE. XINF  or
       K(ALPHA+NB-1,X)/K(ALPHA+NB-2,X) .GE. XINF.  In this case,
       the B-vector is not calculated.  Note that again
       NCALC .NE. NB.

  0 .LT. NCALC .LT. NB: Not all requested function values could
       be calculated accurately.  BK(I) contains correct function
       values for I .LE. NCALC, and contains the ratios
       K(ALPHA+I-1,X)/K(ALPHA+I-2,X) for the rest of the array.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (ize :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))

(def-fortran-routine rybesl :void
  "
  This routine calculates Bessel functions Y SUB(N+ALPHA) (X)
  for non-negative argument X, and non-negative order N+ALPHA.


 Explanation of variables in the calling sequence

 X     - Working precision positive real argument for which
         Y's are to be calculated.
 ALPHA - Working precision fractional part of order for which
         Y's are to be calculated.  0 .LE. ALPHA .LT. 1.0.
 NB    - Integer number of functions to be calculated, NB .GT. 0.
         The first function calculated is of order ALPHA, and the
         last is of order (NB - 1 + ALPHA).
 BY    - Working precision output vector of length NB.  If the
         routine terminates normally (NCALC=NB), the vector BY
         contains the functions Y(ALPHA,X), ... , Y(NB-1+ALPHA,X),
         If (0 .LT. NCALC .LT. NB), BY(I) contains correct function
         values for I .LE. NCALC, and contains the ratios
         Y(ALPHA+I-1,X)/Y(ALPHA+I-2,X) for the rest of the array.
 NCALC - Integer output variable indicating possible errors.
         Before using the vector BY, the user should check that
         NCALC=NB, i.e., all orders have been calculated to
         the desired accuracy.  See error returns below.


*******************************************************************
*******************************************************************

 Explanation of machine-dependent constants.  Let

   beta   = Radix for the floating-point system
   p      = Number of significant base-beta digits in the
            significand of a floating-point number
   minexp = Smallest representable power of beta
   maxexp = Smallest power of beta that overflows

 Then the following machine-dependent constants must be declared
   in DATA statements.  IEEE values are provided as a default.

   EPS    = beta ** (-p)
   DEL    = Machine number below which sin(x)/x = 1; approximately
            SQRT(EPS).
   XMIN   = Smallest acceptable argument for RBESY; approximately
            max(2*beta**minexp,2/XINF), rounded up
   XINF   = Largest positive machine number; approximately
            beta**maxexp
   THRESH = Lower bound for use of the asymptotic form; approximately
            AINT(-LOG10(EPS/2.0))+1.0
   XLARGE = Upper bound on X; approximately 1/DEL, because the sine
            and cosine functions have lost about half of their
            precision at that point.


     Approximate values for some important machines are:

                        beta    p     minexp      maxexp      EPS

  CRAY-1        (S.P.)    2    48     -8193        8191    3.55E-15
  Cyber 180/185
    under NOS   (S.P.)    2    48      -975        1070    3.55E-15
  IEEE (IBM/XT,
    SUN, etc.)  (S.P.)    2    24      -126         128    5.96E-8
  IEEE (IBM/XT,
    SUN, etc.)  (D.P.)    2    53     -1022        1024    1.11D-16
  IBM 3033      (D.P.)   16    14       -65          63    1.39D-17
  VAX           (S.P.)    2    24      -128         127    5.96E-8
  VAX D-Format  (D.P.)    2    56      -128         127    1.39D-17
  VAX G-Format  (D.P.)    2    53     -1024        1023    1.11D-16


                         DEL      XMIN      XINF     THRESH  XLARGE

 CRAY-1        (S.P.)  5.0E-8  3.67E-2466 5.45E+2465  15.0E0  2.0E7
 Cyber 180/855
   under NOS   (S.P.)  5.0E-8  6.28E-294  1.26E+322   15.0E0  2.0E7
 IEEE (IBM/XT,
   SUN, etc.)  (S.P.)  1.0E-4  2.36E-38   3.40E+38     8.0E0  1.0E4
 IEEE (IBM/XT,
   SUN, etc.)  (D.P.)  1.0D-8  4.46D-308  1.79D+308   16.0D0  1.0D8
 IBM 3033      (D.P.)  1.0D-8  2.77D-76   7.23D+75    17.0D0  1.0D8
 VAX           (S.P.)  1.0E-4  1.18E-38   1.70E+38     8.0E0  1.0E4
 VAX D-Format  (D.P.)  1.0D-9  1.18D-38   1.70D+38    17.0D0  1.0D9
 VAX G-Format  (D.P.)  1.0D-8  2.23D-308  8.98D+307   16.0D0  1.0D8

*******************************************************************
*******************************************************************

 Error returns

  In case of an error, NCALC .NE. NB, and not all Y's are
  calculated to the desired accuracy.

  NCALC .LE. -1:  An argument is out of range. For example,
       NB .LE. 0, or ABS(X) .GE. XLARGE.  In this case,
       BY(1) = 0.0, the remainder of the BY-vector is not
       calculated, and NCALC is set to MIN0(NB,0)-1  so that
       NCALC .NE. NB.
  1 .LT. NCALC .LT. NB: Not all requested function values could
       be calculated accurately.  BY(I) contains correct function
       values for I .LE. NCALC, and and the remaining NB-NCALC
       array elements contain 0.0.
"
  (x :double-float :input)
  (alpha :double-float :input)
  (nb :integer :input)
  (b (* :double-float) :output)
  (ncalc :integer :output))


(defgeneric m-normal-cdf (a)
  (:documentation
   "
  Syntax
  ======
  (m-normal-cdf a)

  Purpose
  =======
  Computes the cumulative normal probability integral defined by

                              / x
                     1       |       -t*t/2
          P(x) = ----------- |      e       dt
                 sqrt(2 pi)  |
                             /-oo


  for each x in the real matrix A.
  "))
(make-real-mapper -normal-cdf anorm)

(defgeneric m-bessel-scaled-i0 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-scaled-i0 a)

  Purpose
  =======
  Computes the scaled Bessel function I0 for each element of the
  real matrix A.

  The scaled Bessel function I0 is defined by

     exp(-|x|) * I0(x)

  where I0(x) is the modified Bessel function of the first kind of order
  zero:

  "))

(make-real-mapper -bessel-scaled-i0 besei0)

(defgeneric m-bessel-scaled-i1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-scaled-i1 a)

  Purpose
  =======
  Computes the scaled Bessel function I1 for each element of the
  real matrix A.

  The scaled Bessel function I1 is defined by

     exp(-|x|) * I1(x)

  where I1(x) is the modified Bessel function of the first kind of order
  one:

  "))

(make-real-mapper -bessel-scaled-i1 besei1)

(defgeneric m-bessel-scaled-k0 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-scaled-k0 a)

  Purpose
  =======
  Computes the scaled Bessel function K0 for each element of the
  real matrix A.

  The scaled Bessel function K0 is defined by

     exp(-|x|) * K0(x)

  where K0(x) is the modified Bessel function of the second kind of order
  zero:

  "))
(make-real-mapper -bessel-scaled-k0 besek0)

(defgeneric m-bessel-scaled-k1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-scaled-k1 a)

  Purpose
  =======
  Computes the scaled Bessel function K1 for each element of the
  real matrix A.

  The scaled Bessel function K1 is defined by

     exp(-|x|) * K1(x)

  where K1(x) is the modified Bessel function of the second kind of order
  one:

  "))
(make-real-mapper -bessel-scaled-k1 besek1)

(defgeneric m-bessel-i0 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-i0 a)

  Purpose
  =======
  Computes the Bessel function I0 for each element of the
  real matrix A where I0(x) is the modified Bessel function of the
  first kind of order zero:

  I0(x) = J0(x*exp(pi*i/2))
  "))
(make-real-mapper -bessel-i0 besi0)

(defgeneric m-bessel-i1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-i1 a)

  Purpose
  =======
  Computes the Bessel function I1 for each element of the
  real matrix A, where I1(x) is the modified Bessel function of the
  first kind of order one:

  I1(x) = exp(-pi*i/2)*J1(x*exp(pi*i/2))
  "))
(make-real-mapper -bessel-i1 besi1)

(defgeneric m-bessel-j0 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-j0 a)

  Purpose
  =======
  Computes the Bessel function J0 for each element of the
  real matrix A, where J0(x) is the Bessel function of the
  first kind of order zero:

               INF
               ====       k  2 k
               \     (- 1)  z
   J0(x) =      >    -----------
               /       2 k   2
               ====   2    k!
               k = 0


  "))
(make-real-mapper -bessel-j0 besj0)

(defgeneric m-bessel-j1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-j1 a)

  Purpose
  =======
  Computes the Bessel function J1 for each element of the
  real matrix A, where J1(x) is the modified Bessel function of the
  first kind of order one:

                      INF
                      ====         k  2 k
                  z   \       (- 1)  z
      J1(z) =    ---   >    ----------------
                  2   /      2 k
                      ====  2    k! (k + 1)!
                      k = 0

  "))
(make-real-mapper -bessel-j1 besj1)

(defgeneric m-bessel-k0 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-k0 a)

  Purpose
  =======
  Computes the Bessel function K0 for each element of the
  real matrix A, where K0(x) is the modified Bessel function of the
  second kind of order zero:

              pi          I[-n](z) - I[n](z)
   K0(z) =   ----  limit  -------------------
              2    n -> 0     sin(pi * n)

  "))
(make-real-mapper -bessel-k0 besk0)

(defgeneric m-bessel-k1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-k1 a)

  Purpose
  =======
  Computes the Bessel function K1 for each element of the
  real matrix A, where K1(x) is the modified Bessel function of the
  second kind of order one:

              pi          I[-n](z) - I[n](z)
   K1(z) =   ----  limit  -------------------
              2    n -> 1     sin(pi * n)
  "))
(make-real-mapper -bessel-k1 besk1)

(defgeneric m-bessel-y0 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-y0 a)

  Purpose
  =======
  Computes the Bessel function Y0 for each element of the
  real matrix A, where Y0(x) is the Bessel function of the
  second kind of order zero:

                      J[n](x) * cos(n * pi) - J[-n](x)
    Y0(x) =    limit  --------------------------------
               n -> 0           sin(n * pi)

  "))
(make-real-mapper -bessel-y0 besy0)

(defgeneric m-bessel-j1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-j1 a)

  Purpose
  =======
  Computes the Bessel function J1 for each element of the
  real matrix A, where J1(x) is the Bessel function of the
  second kind of order one:

                      J[n](x) * cos(n * pi) - J[-n](x)
    Y1(x) =    limit  --------------------------------
               n -> 1           sin(n * pi)
  "))
(make-real-mapper -bessel-y1 besy1)

(defgeneric m-dawson-integral (a)
  (:documentation
   "
  Syntax
  ======
  (m-dawson-integral a)

  Purpose
  =======
  Computes Dawson's integral for each element, x, of the
  real matrix A, where Dawson's integral is

                       2  / x   2
                     -x   |    t
             F(x) = e     |   e    dt
                          |
                          / 0


  "))
(make-real-mapper -dawson-integral daw)


(defgeneric m-erf (a)
  (:documentation
   "
  Syntax
  ======
  (m-erf a)

  Purpose
  =======
  Computes erf(x) for each element x of the real matrix A, where

			      x		 
			     /       2	  
		     2       [    - t	  
      erf(x) =   ---------   I   E     dt 
		 SQRT(%PI)   ]		 
			     /		 
			      0           


  "))
(make-real-mapper -erf derf)

(defgeneric m-erfc (a)
  (:documentation
   "
  Syntax
  ======
  (m-erfc a)

  Purpose
  =======
  Computes erfc(x) for each element x of the real matrix A, where

      erfc(x) = 1 - erf(x)

  "))
(make-real-mapper -erfc derfc)

(defgeneric m-erfcx (a)
  (:documentation
   "
  Syntax
  ======
  (m-erfcx a)

  Purpose
  =======
  Computes erfcx(x) for each element x of the real matrix A, where

		    2
		   x
      erfcx(x) = %E   erfc(x)

  "))
(make-real-mapper -erfcx derfcx)

(defgeneric m-gamma (a)
  (:documentation
   "
  Syntax
  ======
  (m-gamma a)

  Purpose
  =======
  Compute the gamma function for each real element, x, of the matrix A.

  The gamma function is

                  INF
                 /
                 [     n - 1   - t
  gamma(n) =     I    t      %E    dt
                 ]
                 /
                  0

  "))
(make-real-mapper -gamma dgamma)

(defgeneric m-log-gamma (a)
  (:documentation
   "
  Syntax
  ======
  (m-log-gamma a)

  Purpose
  =======
  Compute the log of gamma function for each real element, x,
  of the matrix A.  Each element must be non-negative.
  "))
(make-real-mapper -log-gamma dlgama)
;; We need to make sure the arguments to dlgama are positive!
(defmethod m-log-gamma :before ((matrix real-matrix))
  (assert (every #'(lambda (x)
		     (>= x 0))
		 (store matrix))))


(defgeneric m-exponential-integral (a)
  (:documentation
   "
  Syntax
  ======
  (m-exponential-integral a)

  Purpose
  =======
  Compute the exponential integral, Ei(x), for each real element, x,
  of the matrix A.

                       z
                      /       x
                      [     %E
       Ei(z) =        I     --- dx
                      ]      x
                      /
                       MINF

  "))
(make-real-mapper -exponential-integral ei)

(defgeneric m-exponential-integral-1 (a)
  (:documentation
   "
  Syntax
  ======
  (m-exponential-integral-1 a)

  Purpose
  =======
  Compute the exponential integral, E1(x), for each real element, x,
  of the matrix A.
  "))
(make-real-mapper -exponential-integral-1 eone)

(defgeneric m-psi (a)
  (:documentation
   "
  Syntax
  ======
  (m-psi a)

  Purpose
  =======
  Compute the derivative of the log gamma function, psi(x), for
  each real element, x, of the matrix A.
  "))
(make-real-mapper -psi psi)

(defgeneric m-bessel-series-i (x alpha n &key scale-p)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-series-i x alpha n &key scale-p)

  Purpose
  =======
  Computes Bessel functions I[m+alpha](x) for non-negative
  argument x and non-negative order m+alpha for m = 0, 1, 2,...,n-1,
  and 0 <= alpha < 1.  If scale-p is non-NIL, exponential scaling
  is applied: exp(-x)*I[n+alpha](x).  

  The resulting set of n values is returned as a real column vector.
  "))

(defgeneric m-bessel-series-j (x alpha n &key scale-p)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-series-j x alpha n &key scale-p)

  Purpose
  =======
  Computes Bessel functions J[m+alpha](x) for non-negative
  argument x and non-negative order m+alpha for m = 0, 1, 2,...,n-1,
  and 0 <= alpha < 1.  If scale-p is non-NIL, exponential scaling
  is applied: exp(x)*J[n+alpha](x).  

  The resulting set of n values is returned as a real column vector.
  "))

(defgeneric m-bessel-series-k (x alpha n &key scale-p)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-series-k x alpha n &key scale-p)

  Purpose
  =======
  Computes Bessel functions K[m+alpha](x) for non-negative
  argument x and non-negative order m+alpha for m = 0, 1, 2,...,n-1,
  and 0 <= alpha < 1.  If scale-p is non-NIL, exponential scaling
  is applied: exp(-x)*K[n+alpha](x).  

  The resulting set of n values is returned as a real column vector.
  "))

(defgeneric m-bessel-series-y (x alpha n &key scale-p)
  (:documentation
   "
  Syntax
  ======
  (m-bessel-series-i x alpha n &key scale-p)

  Purpose
  =======
  Computes Bessel functions Y[m+alpha](x) for non-negative
  argument x and non-negative order m+alpha for m = 0, 1, 2,...,n-1,
  and 0 <= alpha < 1.  If scale-p is non-NIL, exponential scaling
  is applied: exp(x)*Y[n+alpha](x).  

  The resulting set of n values is returned as a real column vector.
  "))

(macrolet
    ((frob (name)
       `(progn
	 (defmethod ,name ((x double-float) (alpha double-float) (n fixnum)
			   &key (scale-p t))
	   (declare (ignore scale-p))
	   (assert (>= x 0))
	   (assert (<= 0 alpha 1))
	   (assert (plusp n)))

	 (defmethod ,name ((x double-float) (alpha double-float) (n fixnum)
			   &key (scale-p t))
	   (let ((result (make-real-matrix n 1)))
	     (multiple-value-bind (t-result ncalc)
		 (ribesl x alpha n (if scale-p 2 1) (store result) 0)
		 (declare (ignore t-result))
	       (assert (= ncalc n) nil
		       "Could not compute all values to desired precision"))
	     result)))))
  (frob m-bessel-series-i)
  (frob m-bessel-series-j)
  (frob m-bessel-series-k)
  (frob m-bessel-series-y))
