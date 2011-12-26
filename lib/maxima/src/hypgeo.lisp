;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;;    ** (c) Copyright 1976, 1983 Massachusetts Institute of Technology **
;;;
;;;
;;; These are the main routines for finding the Laplace Transform
;;; of special functions   --- written by Yannis Avgoustis
;;;                        --- modified by Edward Lafferty
;;;                       Latest mod by jpg 8/21/81
;;;
;;;   This program uses the programs on ELL;HYP FASL.
;;;
;;; References:
;;;
;;; Definite integration using the generalized hypergeometric functions
;;; Avgoustis, Ioannis Dimitrios
;;; Thesis. 1977. M.S.--Massachusetts Institute of Technology. Dept.
;;; of Electrical Engineering and Computer Science
;;; http://dspace.mit.edu/handle/1721.1/16269
;;;
;;; Avgoustis, I. D., Symbolic Laplace Transforms of Special Functions,
;;; Proceedings of the 1977 MACSYMA Users' Conference, pp 21-41

(in-package :maxima)

(macsyma-module hypgeo)

(declare-top (special var *par* checkcoefsignlist $exponentialize $radexpand))

(load-macsyma-macros rzmac)

(defvar *hyp-return-noun-form-p* t
  "Return noun form instead of internal Lisp symbols for integrals
  that we don't support")

(defvar *hyp-return-noun-flag* nil
  "Flag to signal that we have no result and to return a noun.")

(defvar *debug-hypgeo* nil
  "Print debug information if enabled.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This function is no longer used. DK 09/2009.
;(defun bess (v z flg)
;  `((,(if (eq flg 'j)
;	  '%bessel_j
;	  '%bessel_i))
;    ,v ,z))

; Not used in Maxima core and share.
;(defun notnump (x)
;  (not (nump x)))

#+nil
(defun negnump (x)
  (cond ((not (maxima-integerp x))
	 (minusp (cadr (simplifya x nil))))
	(t (minusp x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Helper functions for this file

;; Test if EXP is 1 or %e.
(defun expor1p (exp)
  (or (equal exp 1)
      (eq exp '$%e)))

;; If FLG is non-NIL, return exp(%pi*%i/2), that is the polarfom of %i.
;; Otherwise, return exp(-%pi*%i*v/2), the polarform of %i^(-v).
;; This function is no longer used.
;(defun 1fact (flg v)
;  (power '$%e
;         (mul* '$%pi
;               '$%i
;               (1//2)
;               (cond (flg 1)
;                     (t (mul -1 v))))))

(defun substl (p1 p2 p3)
  (cond ((eq p1 p2) p3)
        (t (maxima-substitute p1 p2 p3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test functions for pattern match

(defun parp (a)
  (eq a *par*))

(defun hasvarnovarp (a) 
  (and (hasvar a) (not (varp a))))

(defun freevar0 (m)
  (cond ((equal m 0) nil)
        (t (freevar m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This pattern has been replaced by m2-asin
;(defun u*asinx (exp)
;  (m2 exp
;      '((mplus)
;	((coeffpt) (u nonzerp)((%asin)(x hasvar)))
;	((coeffpp)(a zerp)))
;      nil))

; This pattern has been replaced by m2-atan
;(defun u*atanx (exp)
;  (m2 exp
;      '((mplus)
;	((coeffpt)(u nonzerp)((%atan)(x hasvar)))
;	((coeffpp)(a zerp)))
;      nil))

;; I (rtoy) think this is the tail of the incomplete gamma function.
;; No longer used in Maxima core and share.
;(defun gminc (a b)
;  (list '(%gamma_incomplete) a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some shortcuts for special functions.

;; Lommel's little s[u,v](z) function.
(defun littleslommel (m n z)
  (list '(mqapply)(list '($%s array) m n) z))

;; Whittaker's M function
(defun mwhit (a i1 i2)
  (list '(mqapply) (list '($%m array) i1 i2) a))

;; Whittaker's W function
(defun wwhit (a i1 i2)
  (list '(mqapply) (list '($%w array) i1 i2) a))

;; Jacobi P
(defun pjac (x n a b)
  (list '(mqapply) (list '($%p array) n a b) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Two general pattern for the routine lt-sf-log.

;; Recognize c*u^v + a and a=0.
(defun arbpow1 (exp)
  (m2 exp
      '((mplus)
        ((coeffpt)
         (c freevar) ; more special to ensure that c is constant
         ((mexpt)(u hasvar)(v freevar)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize c*t^v*(a+b*t)^w+d and d=0. This is a generalization of arbpow1. 
(defun m2-arbpow2 (exp)
  (m2 exp
      '((mplus)
        ((mtimes)
         ((coefftt) (c freevar))
         ((mexpt) (t equal var) (v freevar))
         ((mexpt)
          ((mplus) (a freevar) ((coefft) (b freevar) (t equal var)))
          (w freevar0)))
        ((coeffpp)(d zerp)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The pattern to match special functions in the routine lt-sf-log.

;; Recognize asin(w)
(defun m2-asin (expr)
  (m2 expr
      '((mplus)
        ((coeffpt) (u nonzerp)
         ((%asin) (w hasvar)))
        ((coeffpp) (a equal 0)))
      nil))

;; Recognize atan(w)
(defun m2-atan (expr)
  (m2 expr
      '((mplus)
        ((coeffpt) (u nonzerp)
         ((%atan) (w hasvar)))
        ((coeffpp) (a equal 0)))
      nil))

;; Recognize bessel_j(v,w)
(defun onej (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_j) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize bessel_j(v1,w1)*bessel_j(v2,w2)
(defun twoj (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_j) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*bessel_y(v2,w2)
(defun twoy (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*bessel_k(v2,w2)
(defun twok (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_k) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*bessel_y(v2,w2)
(defun onekoney (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_j(v,w)^2
(defun onej^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_j) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v,w)^2
(defun oney^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_y) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v,w)^2
(defun onek^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_k) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v,w)
(defun onei (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_i(v2,w2)
(defun twoi (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_i) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_1(v1,w1)*hankel_1(v2,w2), product of 2 Hankel 1 functions.
(defun m2-two-hankel_1 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_1) (v1 true) (w1 true))
         ((%hankel_1) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_2(v1,w1)*hankel_2(v2,w2), product of 2 Hankel 2 functions.
(defun m2-two-hankel_2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_2) (v1 true) (w1 true))
         ((%hankel_2) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_1(v1,w1)*hankel_2(v2,w2), product of 2 Hankel functions.
(defun m2-hankel_1*hankel_2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_1) (v1 true) (w1 true))
         ((%hankel_2) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*bessel_j(v2,w2)
(defun oneyonej (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*bessel_j(v2,w2)
(defun onekonej (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*hankel_1(v2,w2)
(defun m2-bessel_y*hankel_1 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%bessel_y) (v1 true) (w1 true))
         ((%hankel_1) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v1,w1)*hankel_2(v2,w2)
(defun m2-bessel_y*hankel_2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%bessel_y) (v1 true) (w1 true))
         ((%hankel_2) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*hankel_1(v2,w2)
(defun m2-bessel_k*hankel_1 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%bessel_k) (v1 true) (w1 true))
         ((%hankel_1) (v1 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_k(v1,w1)*hankel_2(v2,w2)
(defun m2-bessel_k*hankel_2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%bessel_k) (v1 true) (w1 true))
         ((%hankel_2) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_j(v2,w2)
(defun oneionej (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_j) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*hankel_1(v2,w2)
(defun m2-bessel_i*hankel_1 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%bessel_i) (v1 true) (w1 true))
         ((%hankel_1) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*hankel_2(v2,w2)
(defun m2-bessel_i*hankel_2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%bessel_i) (v1 true) (w1 true))
         ((%hankel_2) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_1(v1,w1)*bessel_j(v2,w2)
(defun m2-hankel_1*bessel_j (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_1) (v1 true) (w2 true))
         ((%bessel_j) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_2(v1,w1)*bessel_j(v2,w2)
(defun m2-hankel_2*bessel_j (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_2) (v1 true) (w2 true))
         ((%bessel_j) (v2 true) (w2 true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_y(v2,w2)
(defun oneioney (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_y) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v1,w1)*bessel_k(v2,w2)
(defun oneionek (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_i) (v1 true) (w1 true))
	 ((%bessel_k) (v2 true) (w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_i(v,w)^2
(defun onei^2 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  ((%bessel_i) (v true) (w true))
	  2.))
	((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_1(v,w)^2
(defun m2-hankel_1^2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((mexpt)
          ((%hankel_1) (v true) (w true))
          2))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize hankel_2(v,w)^2
(defun m2-hankel_2^2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((mexpt)
          ((%hankel_2) (v true) (w true))
          2))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize bessel_y(v,w)
(defun oney (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_y) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize bessel_k(v,w)
(defun onek (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%bessel_k) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize hankel_1(v,w)
(defun m2-hankel_1 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_1) (v true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize hankel_2(v,w)
(defun m2-hankel_2 (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%hankel_2) (v true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize log(w)
(defun onelog (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%log)(w hasvar)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize erf(w)
(defun onerf (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%erf)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize erfc(w)
(defun onerfc (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%erfc)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize fresnel_s(w)
(defun onefresnel_s (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%fresnel_s)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize fresnel_c(w)
(defun onefresnel_c (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%fresnel_c)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize expintegral_e(v,w)
(defun oneexpintegral_e (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%expintegral_e) (v true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize expintegral_ei(w)
(defun oneexpintegral_ei (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%expintegral_ei)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize expintegral_e1(w)
(defun oneexpintegral_e1 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%expintegral_e1)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize expintegral_si(w)
(defun oneexpintegral_si (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%expintegral_si)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize expintegral_shi(w)
(defun oneexpintegral_shi (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%expintegral_shi)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize expintegral_ci(w)
(defun oneexpintegral_ci (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%expintegral_ci)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize expintegral_chi(w)
(defun oneexpintegral_chi (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)((%expintegral_chi)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize kelliptic(w), (new would be elliptic_kc)
(defun onekelliptic (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)(($kelliptic)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize elliptic_kc
(defun m2-elliptic_kc (expr)
  (m2 expr
      '((mplus)
        ((coeffpt) 
         (u nonzerp)
         ((%elliptic_kc) (w true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize %e(w), (new would be elliptic_ec)
(defun onee (exp)
  (m2 exp
      '((mplus)
	((coeffpt)(u nonzerp)(($%e)(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize elliptic_ec
(defun m2-elliptic_ec (expr)
  (m2 expr
      '((mplus)
        ((coeffpt) 
         (u nonzerp)
         ((%elliptic_ec) (w true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize gamma_incomplete(w1, w2), Incomplete Gamma function
(defun onegammaincomplete (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%gamma_incomplete)(w1 freevarpar)(w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize gammagreek(w1,w2), gamma(a)-gamma_incomplete(w1,w2)
(defun onegammagreek (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 (($gammagreek)(w1 freevarpar)(w2 true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize Struve H function.
(defun m2-struve_h (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%struve_h) (v true) (w true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize Struve L function.
(defun m2-struve_l (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%struve_l) (v true) (w true)))
        ((coeffpp)(a zerp)))
      nil))

;; Recognize Lommel s[v1,v2](w) function.
(defun ones (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%s array)(v1 true)(v2 true))(w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize S[v1,v2](w), Lommel function
(defun oneslommel (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($slommel array)(v1 true)(v2 true))
	  (w true)))
	((coeffpp)(a zerp)))
      nil))

;; Recognize parabolic_cylinder_d function
(defun m2-parabolic_cylinder_d (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($parabolic_cylinder_d) (v true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize kbatmann(v,w), Batemann function
(defun onekbateman (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($kbateman array) (v true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %l[v1,v2](w), Generalized Laguerre function
(defun onel (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%l array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize gen_laguerre(v1,v2,w), Generalized Laguerre function
(defun one-gen-laguerre (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%gen_laguerre) (v1 true) (v2 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize laguerre(v1,w), Laguerre function
(defun one-laguerre (exr)
  (m2 exr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((%laguerre) (v1 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize %c[v1,v2](w), Gegenbauer function
(defun onec (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%c array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %t[v1](w), Chebyshev function of the first kind
(defun onet (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%t array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %u[v1](w), Chebyshev function of the second kind
(defun oneu (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%u array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %p[v1,v2,v3](w), Jacobi function
(defun onepjac (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%p array) (v1 true)(v2 true)(v3 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize jacobi_p function
(defun m2-jacobi_p (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($jacobi_p) (v1 true) (v2true) (v3 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize %p[v1,v2](w), Associated Legendre P function
(defun hyp-onep (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%p array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize assoc_legendre_p function
(defun m2-assoc_legendre_p (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($assoc_legendre_p) (v1 true) (v2 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize %p[v1](w), Legendre P function
(defun onep0 (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)(($%p array) (v1 true)) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %p[v1](w), Legendre P function
(defun m2-legendre_p (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($legendre_p) (v true)) (w true))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize hermite(v1,w), Hermite function
(defun one-hermite (expr)
  (m2 expr
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((%hermite) (v1 true) (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize %q[v1,v2](w), Associated Legendre function of the second kind
(defun oneq (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%q array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize assoc_legendre_q function
(defun m2-assoc_legendre_q (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($assoc_legendre_q) (v1 true) (v2 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize %w[v1,v2](w), Whittaker W function.
(defun onew (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%w array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize whittaker_w function.
(defun m2-whittaker_w (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($whittaker_w) (v1 true) (v2 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize %m[v1,v2](w), Whittaker M function
(defun onem (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%m array) (v1 true)(v2 true))
	  (w true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize whittaker_m function.
(defun m2-whittaker_m (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         (($whittaker_m) (v1 true) (v2 true) (w true)))
        ((coeffpp) (a zerp)))
      nil))

;; Recognize %f[v1,v2](w1,w2,w3), Hypergeometric function
(defun onef (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mqapply)
	  (($%f array) (v1 true) (v2 true))
          (w1 freevarpar)
          (w2 freevarpar)
	  (w3 true)))
	((coeffpp) (a zerp)))
      nil))

;; Recognize hypergeometric function
(defun m2-hypergeometric (expr)
  (m2 expr
      '((mplus)
        ((coeffpt)
         (u nonzerp)
          (($hypergeometric) (w1 freevarpar) (w2 freevarpar) (w3 true)))
        ((coeffpp) (a zerp)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pattern for the routine hypgeo-exec.
;;; RECOGNIZES L.T.E. "U*%E^(A*X+E*F(X)-P*X+C)+D".

(defun ltep (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 ((mexpt)
	  $%e
	  ((mplus)
	   ((coeffpt) (a freevarpar) (x varp))
	   ((coeffpt) (e freevarpar) (f hasvar))
	   ((mtimes) -1. (p parp) (x varp))
	   ((coeffpp) (c freevarpar)))))
	((coeffpp) (d zerp)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pattern for the routine defexec.
;;; This is trying to match EXP to u*%e^(a*x+e*f+c)+d
;;; where a, c, and e are free of x, f is free of p, and d is 0.

(defun defltep (exp)
  (m2 exp
      '((mplus)
        ((coeffpt)
         (u nonzerp)
         ((mexpt)
          $%e
          ((mplus)
           ((coeffpt) (a freevar) (x varp))
           ((coeffpt) (e freevar) (f hasvarnovarp))
           ((coeffpp) (c freevar)))))
        ((coeffpp) (d zerp)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; $specint is the Maxima User function

(defmfun $specint (exp var)
  (prog ($radexpand checkcoefsignlist)
     (setq $radexpand '$all)
     (return (defintegrate exp var))))

; This routine is no longer called.
;(defun grasp-some-trigs (exp var)
;  (let ((*asinx* nil)
;	(*atanx* nil))
;    (declare (special *asinx* *atanx*))
;    (prog (u x l)
;       (cond ((setq l (u*asinx exp))
;	      (setq u (cdras 'u l)
;		    x (cdras 'x l)
;		    *asinx* 't)
;	      (return (defintegrate u var))))
;       (cond ((setq l (u*atanx exp))
;	      (setq u (cdras 'u l)
;		    x (cdras 'x l)
;		    *atanx* 't)
;	      (return (defintegrate u var))))
;       (return (defintegrate exp var)))))

(defun defintegrate (exp var)
  ;; This used to have $exponentialize enabled for everything, but I
  ;; don't think we should do that.  If various routines want
  ;; $exponentialize, let them set it themselves.  So, for here, we
  ;; want to expand the form with $exponentialize to convert trig and
  ;; hyperbolic functions to exponential functions that we can handle.
  (let ((form (let (($exponentialize t))
		($factor (resimplify exp))))) ; At first factor the integrand.

    ;; Because we call defintegrate recursively, we add code to end the
    ;; recursion safely.

    (when (atom form)
      (cond ((and (numberp form) (zerop form)) (return-from defintegrate 0))
	    (t (return-from defintegrate (list '(%specint) form var)))))

    ;; We try to find a constant denominator. This is necessary to get results
    ;; for integrands like u(t)/(a+b+c+...).

    (let ((den ($denom form)))
      (when (and (not (equal 1 den)) ($freeof var den))
	(when *debug-hypgeo*
	  (format t "~&DEFINTEGRATE: We have found a denominator.~%")
	  (format t "~&   : denom = ~A~%" den))
	(return-from defintegrate
	  (div (defintegrate (mul den form) var) den))))

    ;; We search for a sum of Exponential functions which we can integrate.
    ;; This code finds result for Trigonometric or Hyperbolic functions with
    ;; a factor t^-1 or t^-2 e.g. t^-1*sin(a*t).

    (let* ((l (defltep form))
	   (s (mul -1 (cdras 'a l)))
	   (u ($expand (cdras 'u l)))
	   (l1))
      (when *debug-hypgeo*
	(format t "~&DEFINTEGRATE:~%")
	(format t "~&   : l    = ~A~%" l))
      (cond
	((setq l1 (m2-sum-with-exp-case1 u))
	 ;;  c * t^-1 * (%e^(-a*t) - %e^(-b*t)) + d
	 (let ((c (cdras 'c l1))
	       (a (mul -1 (cdras 'a l1)))
	       (b (mul -1 (cdras 'b l1)))
	       (d (cdras 'd l1)))
	   (when *debug-hypgeo* (format t "~&   : l1 = ~A~%" l1))
	   (add
	     (mul c (simplify (list '(%log) (div (add s b) (add s a)))))
	     (defintegrate (mul d (power '$%e (mul -1 s var))) var))))

	((setq l1 (m2-sum-with-exp-case2 u))
	 ;;  c * t^(-3/2) * (%e^(-a*t) - %e^(-b*t)) + d
	 (let ((c (cdras 'c l1))
	       (a (mul -1 (cdras 'a l1)))
	       (b (mul -1 (cdras 'b l1)))
	       (d (cdras 'd l1)))
	   (when *debug-hypgeo* (format t "~&   : l1 = ~A~%" l1))
	   (add
	     (mul
	       2 c
	       (power '$%pi (div 1 2))
	       (sub
		 (power (add s b) (inv 2))
		 (power (add s a) (inv 2))))
	     (defintegrate (mul d (power '$%e (mul -1 s var))) var))))

	((setq l1 (m2-sum-with-exp-case3 u))
	 ;; c * t^-2 * (1 - 2 * %e^(-a*t) + %e^(2*a*t)) + d
	 (let ((c (cdras 'c l1))
	       (a (div (cdras 'a l1) -2))
	       (d (cdras 'd l1)))
	   (when *debug-hypgeo* (format t "~&   : l1 = ~A~%" l1))
	   (add
	     (mul
	       c
	       (add
		 (mul (add s a a) (simplify (list '(%log) (add s a a))))
		 (mul s (simplify (list '(%log) s)))
		 (mul -2 (add s a) (simplify (list '(%log) (add s a))))))
	     (defintegrate (mul d (power '$%e (mul -1 s var))) var))))

        ((setq l1 (m2-sum-with-exp-case4 u))
         ;; c * t^-1 * (1 - 2 * %e^(-a*t) + %e^(2*a*t)) + d
         (let ((c (cdras 'c l1))
               (a (div (cdras 'a l1) (mul 4 '$%i)))
               (d (cdras 'd l1)))
           (when *debug-hypgeo* (format t "~&   : l1 = ~A~%" l1))
           (add
             (mul
               -1 c
               (simplify 
                 (list '(%log)
                   (add 1
                     (div
                       (mul 4 a a)
                       (mul (sub s (mul 2 '$%i a)) (sub s (mul 2 '$%i a))))))))
             (defintegrate (mul d (power '$%e (mul -1 s var))) var))))

       ((setq l1 (m2-sum-with-exp-case5 u))
	 ;; c * t^-1 * (1 - %e^(2*a*t)) + d
	 (let ((c (cdras 'c l1))
	       (a (cdras 'a l1))
	       (d (cdras 'd l1)))
	   (when *debug-hypgeo* (format t "~&   : l1 = ~A~%" l1))
	   (add
	     (mul c (simplify (list '(%log) (div (sub s a) s))))
	     (defintegrate (mul d (power '$%e (mul -1 s var))) var))))

	(t
	  ;; At this point we expand the integrand.
	 (distrdefexecinit ($expand form)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Five pattern to find sums of Exponential functions which we can integrate.

;; Case 1: c * t^-1 * (%e^(-a*t) - %e^(-b*t))
(defun m2-sum-with-exp-case1 (exp)
  (m2 exp
      '((mplus)
	((coefft)
	 (c freevar)
	 ((mexpt) (t varp) -1)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (a  nonzerp) (t varp))
	      ((coeffpp) (z1 zerp)))))
	((coefft)
	 (c2 equal-times-minus-one c)
	 ((mexpt) (t varp) -1)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (b  nonzerp) (t varp))
	      ((coeffpp) (z2 zerp)))))
	((coeffpp) (d true)))
      nil))

;; Case 2: c * t^(-3/2) * (%e^(-a*t) - %e^(-b*t))
(defun m2-sum-with-exp-case2 (exp)
  (m2 exp
      '((mplus)
	((coefft)
	 (c freevar)
	 ((mexpt) (t varp) ((rat) -3 2))
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (a  nonzerp) (t varp))
	      ((coeffpp) (z1 zerp)))))
	((coefft)
	 (c2 equal-times-minus-one c)
	 ((mexpt) (t varp) ((rat) -3 2))
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (b  nonzerp) (t varp))
	      ((coeffpp) (z2 zerp)))))
	((coeffpp) (d true)))
      nil))

;; Case 3: c * t^-2 * (1 - 2 * %e^(-a*t) + %e^(2*a*t))
(defun m2-sum-with-exp-case3 (exp)
  (m2 exp
      '((mplus)
	((coefft)
	 (c freevar)
	 ((mexpt) (t varp) -2))
	((coefft)
	 (c2 equal c)
	 ((mexpt) (t varp) -2)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (a  nonzerp) (t varp))
	      ((coeffpp) (z1 zerp)))))
	((coefft)
	 (c3 equal-times-minus-two c)
	 ((mexpt) (t varp) -2)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (b  equal-div-two a) (t varp))
	      ((coeffpp) (z2 zerp)))))
	((coeffpp) (d true)))
      nil))

;; Case 4: c * t^-1 * (1 - 2 * %e^(-a*t) + %e^(2*a*t))
(defun m2-sum-with-exp-case4 (exp)
  (m2 exp
      '((mplus)
	((coefft)
	 (c freevar)
	 ((mexpt) (t varp) -1))
	((coefft)
	 (c2 equal c)
	 ((mexpt) (t varp) -1)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (a  nonzerp) (t varp))
	      ((coeffpp) (z1 zerp)))))
	((coefft)
	 (c3 equal-times-minus-two c)
	 ((mexpt) (t varp) -1)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (b  equal-div-two a) (t varp))
	      ((coeffpp) (z2 zerp)))))
	((coeffpp) (d true)))
      nil))

;; Case 5: c* t^-1 * (1 - %e^(2*a*t))
(defun m2-sum-with-exp-case5 (exp)
  (m2 exp
      '((mplus)
	((coefft)
	 (c freevar)
	 ((mexpt) (t varp) -1))
	((coefft)
	 (c2 equal-times-minus-one c)
	 ((mexpt) (t varp) -1)
	    ((mexpt) $%e
	     ((mplus)
	      ((coeffpt) (a  nonzerp) (t varp))
	      ((coeffpp) (z1 zerp)))))
	((coeffpp) (d true)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Test functions for the pattern m2-sum-with-exp-case<n>

(defun equal-times-minus-one (a b) 
  (equal a (mul -1 b)))

(defun equal-times-minus-two (a b) 
  (equal a (mul -2 b)))

(defun equal-div-two (a b) 
  (equal a (div b 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called by defintegrate.
;;; Call for every term of a sum defexec and add up the results.

;; Evaluate the transform of a sum as sum of transforms.
(defun distrdefexecinit (fun)
  (cond ((equal (caar fun) 'mplus)
         (distrdefexec (cdr fun)))
        (t (defexec fun var))))

;; FUN is a list of addends. Compute the transform of each addend and 
;; add them up.
(defun distrdefexec (fun)
  (cond ((null fun) 0)
        (t (add (defexec (car fun) var)
                (distrdefexec (cdr fun))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Called after transformation of a integrand to a new representation.
;;; Evalutate the tansform of a sum as sum of transforms.

(defun sendexec (r a)
  (distrexecinit ($expand (mul (init r) a))))

;; Compute r*exp(-p*t), where t is the variable of integration and 
;; p is the parameter of the Laplace transform.
(defun init (r)
  (mul* r (power '$%e (mul* -1 var *par*))))

(defun distrexecinit (fun)
  (cond ((and (consp fun)
              (consp (car fun))
              (equal (caar fun) 'mplus))
         (distrexec (cdr fun)))
        (t (hypgeo-exec fun var *par*))))

(defun distrexec (fun)
  (cond ((null fun) 0)
        (t (add (hypgeo-exec (car fun) var *par*)
                (distrexec (cdr fun))))))

;; It dispatches according to the kind of transform it matches.
(defun hypgeo-exec (exp var *par*)
  (prog (l u a c e f)
     (setq exp (simplifya exp nil))
     (cond ((setq l (ltep exp))
            (setq u (cdras 'u l)
                  a (cdras 'a l)
                  c (cdras 'c l)
                  e (cdras 'e l)
                  f (cdras 'f l))
            (return (ltscale u var *par* c a e f))))
     (return (setq *hyp-return-noun-flag* 'other-trans-to-follow))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Compute transform of EXP wrt the variable of integration VAR.

(defun defexec (exp var)
  (let* ((*hyp-return-noun-flag* nil) ; We reset the flag.
	 (form (simplifya exp nil))
	 (l (defltep exp))
	 (s (cdras 'a l))) ; Get the parameter of the Laplace transform.

    ;; If we have not found a parameter, we try to factor the integrand.

    (when (and (numberp s) (equal s 0))
       (setq l (defltep ($factor form)))
       (setq s (cdras 'a l))

       (when *debug-hypgeo*
	 (format t "~&DEFEXC: We try to factor.~%")
	 (format t "~&   : l = ~A~%" l)))

    (cond (l
	   ;; EXP is an expression of the form u*%e^(s*t+e*f+c).  So s
	   ;; is basically the parameter of the Laplace transform.
	   (let ((result (negtest l s)))
	     ;; At this point we construct the noun form if one of the
	     ;; called routines set the global flag. If the global flag
	     ;; is not set, the noun form has been already constructed.
	     (if (and *hyp-return-noun-form-p* *hyp-return-noun-flag*)
	       (list '(%specint) exp var)
	       result)))
	  (t
	   ;; If necessary we construct the noun form.
	   (if *hyp-return-noun-form-p*
	     (list '(%specint) exp var)
  	     'other-defint-to-follow-defexec)))))

;; L is the integrand of the transform, after pattern matching.  S is
;; the parameter (p) of the transform.
(defun negtest (l s)
  (prog (u e f c)
     (cond ((eq (checksigntm ($realpart s)) '$negative)
	    ;; The parameter of transform must have a negative
	    ;; realpart.  Break out the integrand into its various
	    ;; components.
	    (setq u (cdras 'u l)
		  e (cdras 'e l)
		  f (cdras 'f l)
		  c (cdras 'c l))
	    (cond ((zerp e) (setq f 1)))
	    ;; To compute the transform, we replace A with PSEY for
	    ;; simplicity.  After the transform is computed, replace
	    ;; PSEY with A.
	    ;;
	    ;; FIXME: Sometimes maxima will ask for the sign of PSEY.
	    ;; But that doesn't occur in the original expression, so
	    ;; it's very confusing.  What should we do?

	    ;; We know psey must be positive. psey is a substitution
	    ;; for the paratemter a and we have checked the sign.
	    ;; So it is the best to add a rule for the sign of psey.

	    (mfuncall '$assume `((mgreaterp) psey 0))

	    (return
	      (prog1
		(maxima-substitute
		  (mul -1 s)
		  'psey
		  (ltscale u var 'psey c 0 e f))

		;; We forget the rule after finishing the calculation.
		(mfuncall '$forget `((mgreaterp) psey 0))))))

     (return
       (setq *hyp-return-noun-flag* 'other-defint-to-follow-negtest))))

;; Compute the transform of
;;
;;  EXP * %E^(-VAR * (*PAR* - PAR0) + E*F + C)
(defun ltscale (exp var *par* c par0 e f)
  (mul* (power '$%e c)
	(substl (sub *par* par0) *par* (lt-exec exp e f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Compute the transform of u*%e^(-p*t+e*f)

(defun lt-exec (u e f)
;  (declare (special *asinx* *atanx*)) ; No longer used.
  (let (l a)
    (cond ((setq l (m2-sum u))
	   ;; We have found a summation.
	   (when *debug-hypgeo*
	     (format t "~&LT-EXEC: SUM gefunden~%")
	     (format t "~&   : l = ~A~%" l))
	   (mul
	     (cdras 'c l)
	     (simplify 
               (list
	        '(%sum)
	         (sendexec (cdras 'u l) 1)
	         (cdras 'i l) (cdras 'l l) (cdras 'h l)))))

	  ((setq l (m2-unit_step u))
	   ;; We have found the Unit Step function.
	   (setq u (cdras 'u l)
		 a (cdras 'a l))
	   (when *debug-hypgeo*
	     (format t "~&We have found unit_step:~%")
	     (format t "~&   : l = ~A~%" l))
	   (mul
	     (power '$%e (mul a *par*))
	     (sendexec
	       (cond (($freeof var u) u)
		     (t (maxima-substitute (sub var a) var u)))
	       1)))

; No special handling of asin and atan. 
; The functions are handled by the routine lt-sf-log.
;	  ((or *asinx* *atanx*)
;	   ;; We've already determined that we have an asin or atan;
;	   ;; expression, so use lt-asinatan to find the transform.
;	   (lt-asinatan u e))
          
	  ((zerp e)
	   ;; The simple case of u*%e^(-p*t)
	   (lt-sf-log u))
	  ((and (not (zerp e))
		(setq l (c*t^v u)))
	   ;; We have u*%e^(-p*t+e*f).  Try to see if U is of the form
	   ;; c*t^v.  If so, we can handle it here.
	   (lt-exp l e f))
	  (t
	   ;; The complicated case.  Remove the e*f term and move it
	   ;; to u.
	   (lt-sf-log (mul* u (power '$%e (mul e f))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pattern for the routine lt-exec

;; Recognize c*sum(u,index,low,high)
(defun m2-sum (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (c freevar)
	 ((%sum) (u true) (i true) (l true) (h true)))
	((coeffpp) (d zerp)))
      nil))

;; Recognize f(t)*unit_step(t-a)
(defun m2-unit_step (exp)
  (m2 exp
      '((mplus)
	((coeffpt)
	 (u nonzerp)
	 (($unit_step) ((mplus) (t equal var) ((coeffpp) (a true)))))
	((coeffpp) (d zerp)))
      nil))

;; Recognize c*t^v.
(defun c*t^v (exp)
  (m2 exp
      '((mtimes)
	((coefftt)(c freevar))
	((mexpt)(t varp)(v freevar)))
      nil))

;; Laplace transform of u*%e^(-p*t + e*f).  But we can't handle the
;; case where e isn't zero.
;; This routine is no longer called. In the routine lt-sf-log
;; lt-ltp is directly called.
;(defun lt-asinatan (u e)
;  (declare (special *asinx* *atanx*))
;  (cond ((zerp e)
;	 (cond (*asinx* (lt-ltp 'asin u var nil))
;	       (*atanx* (lt-ltp 'atan u var nil))
;	       (t (setq *hyp-return-noun-flag* 'lt-asinatan-failed-1))))
;	(t (setq *hyp-return-noun-flag* 'lt-asinatan-failed-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Algorithm 1: Laplace transform of c*t^v*exp(-s*t+e*f)
;;;
;;; L contains the pattern for c*t^v.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-exp (l e f)
  (let ((c (cdras 'c l))
	(v (cdras 'v l)))
    (cond ((t^2 f)
	   (setq e (inv (mul -8 e)) v (add v 1))
	   (f24p146test c v e))
	  ((sqroott f)
	   ;; We don't do the transformation at this place. Because we take the
	   ;; square of e we lost the sign and get wrong results.
	   ;(setq e (mul* e e (inv 4)) v (add v 1))
	   (f35p147test c v e))
	  ((t^-1 f)
	   (setq e (mul -4 e) v (add v 1))
	   (f29p146test c v e))       ; We have to call with the constant c.
	  ((and (equal v 0) (e^-t f)) ; We have to test for v=0 and to call
	   (f36p147 c e))             ; with the constant c.
	  ((and (equal v 0) (e^t f))
	   (f37p147 c e))
	  (t 
           (setq *hyp-return-noun-flag* 'other-lt-exponential-to-follow)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pattern for the routine lt-exp

;; Recognize t^2
(defun t^2 (exp)
  (m2 exp '((mexpt)(t varp) 2) nil))

;; Recognize sqrt(t)
(defun sqroott (exp)
  (m2 exp '((mexpt)(t varp)((rat) 1 2)) nil))

;; Recognize t^-1
(defun t^-1 (exp)
  (m2 exp '((mexpt)(t varp) -1) nil))

;; Recognize %e^-t
(defun e^-t (exp)
  (m2 exp
      '((mexpt) $%e ((mtimes) -1 (t varp)))
      nil))

;; Recognize %e^t
(defun e^t (exp)
  (m2 exp
      '((mexpt) $%e (t varp))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 1.1: Laplace transform of c*t^v*exp(-a*t^2)
;;;
;;; Table of Integral Transforms
;;;
;;; p. 146, formula 24:
;;;
;;; t^(v-1)*exp(-t^2/8/a)
;;;   -> gamma(v)*2^v*a^(v/2)*exp(a*p^2)*D[-v](2*p*sqrt(a))
;;;
;;; Re(a) > 0, Re(v) > 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun f24p146test (c v a)
  (cond ((and (eq (asksign a) '$positive)
	      (eq (asksign v) '$positive))
	 ;; Both a and v must be positive
	 (f24p146 c v a))
	(t
         (setq *hyp-return-noun-flag* 'fail-on-f24p146test))))

(defun f24p146 (c v a)
  (mul c
       (simplify (list '(%gamma) v))
       (power 2 v)
       (power a (div v 2))
       (power '$%e (mul a *par* *par*))
       (dtford (mul 2 *par* (power a (1//2)))
               (mul -1 v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 1.2: Laplace transform of c*t^v*exp(-a*sqrt(t))
;;;
;;; Table of Integral Transforms
;;;
;;; p. 147, formula 35:
;;;
;;; (2*t)^(v-1)*exp(-2*sqrt(a)*sqrt(t))
;;;    -> gamma(2*v)*p^(-v)*exp(a/p/2)*D[-2*v](sqrt(2*a/p))
;;;
;;; Re(v) > 0, Re(p) > 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if conditions for f35p147 hold
(defun f35p147test (c v a)
  (cond ((eq (asksign (add v 1)) '$positive)
	 ;; v must be positive
	 (f35p147 c v a))
	(t
	 ;; Set a global flag. When *hyp-return-noun-form-p* is T the noun
	 ;; form will be constructed in the routine DEFEXEC.
	 (setq *hyp-return-noun-flag* 'fail-on-f35p147test))))

(defun f35p147 (c v a)
  ;; We have not done the calculation v->v+1 and a-> a^2/4
  ;; and subsitute here accordingly.
  (let ((v (add v 1)))
    (mul
      c
      (simplify (list '(%gamma) (add v v)))
      (power 2 (sub 1 v))               ; Is this supposed to be here?
      (power *par* (mul -1 v))
      (power '$%e (mul a a (inv 8) (inv *par*)))
      ;; We need an additional factor -1 to get the expected results.
      ;; What is the mathematically reason?
      (dtford (mul -1 a (inv (power (mul 2 *par*) (inv 2)))) (mul -2 v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Express a parabolic cylinder function as either a parabolic
;; cylinder function or as hypergeometric function.
;;
;; Tables of Integral Transforms, p. 386 gives this definition:
;;
;; D[v](z) = 2^(v/2+1/4)*z^(-1/2)*W[v/2+1/4,1/4](z^2/2)

(defmvar $prefer_d nil)

(defun dtford (z v)
  (let ((inv4 (inv 4)))
    (cond ((or $prefer_d 
               (whittindtest (add (div v 2) inv4) inv4))
           ;; At this time the Parabolic Cylinder D function is not implemented
           ;; as a simplifying function. We call nevertheless the simplifer
           ;; to simplify the arguments. When we implement the function
           ;; The symbol has to be changed to the noun form.
           (simplify (list '($parabolic_cylinder_d) v z)))
          (t (simpdtf z v)))))

(defun whittindtest (i1 i2)
  (or (maxima-integerp (add i2 i2))
      (neginp (sub (sub (1//2) i2) i1))
      (neginp (sub (add (1//2) i2) i1))))

;; Return T if a is a non-positive integer.
;; (Do we really want maxima-integerp or hyp-integerp here?)
(defun neginp (a)
  (cond ((maxima-integerp a)
         (or (zerop1 a) 
             (eq ($sign a) '$neg)))))

;; Express parabolic cylinder function as a hypergeometric function.
;;
;; A&S 19.3.1 says
;;
;; U(a,x) = D[-a-1/2](x)
;;
;; and A&S 19.12.3 gives
;;
;; U(a,+/-x) = sqrt(%pi)*2^(-1/4-a/2)*exp(-x^2/4)/gamma(3/4+a/2)
;;                      *M(a/2+1/4,1/2,x^2/2)
;;              -/+ sqrt(%pi)*2^(1/4-a/2)*x*exp(-x^2/4)/gamma(1/4+a/2)
;;                           *M(a/2+3/4,3/2,x^2/2)
;;
;; So
;;
;; D[v](z) = U(-v-1/2,z)
;;         = sqrt(%pi)*2^(v/2+1/2)*x*exp(-x^2/4)
;;                    *M(1/2-v/2,3/2,x^2/2)/gamma(-v/2)
;;             + sqrt(%pi)*2^(v/2)*exp(-x^2/4)/gamma(1/2-v/2)
;;                        *M(-v/2,1/2,x^2/2)

(defun simpdtf (z v)
  (let ((inv2 (1//2))
        (pow (power '$%e (mul* z z (inv -4)))))
    (add (mul* (power 2 (div (sub v 1) 2)) ; sub or add ?
               z
               (simplify (list '(%gamma) (inv -2)))
               (inv (simplify (list '(%gamma) (mul v -1 inv2))))
               pow
               (hgfsimp-exec (list (sub inv2
                                        (div v
                                             2)))
                             (list (div 3 2))
                             (mul* z z inv2)))
         (mul* (power 2 (div v 2))
               (simplify (list '(%gamma) inv2))
               pow
               (inv (simplify (list '(%gamma) (sub inv2 (mul v inv2)))))
               (hgfsimp-exec (list (mul* v
                                         -1
                                         inv2))
                             (list inv2)
                             (mul* z z inv2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 1.3: Laplace transform of t^v*exp(1/t)
;;;
;;; Table of Integral Transforms
;;;
;;; p. 146, formula 29:
;;;
;;; t^(v-1)*exp(-a/t/4)
;;;    -> 2*(a/p/4)^(v/2)*bessel_k(v, sqrt(a)*sqrt(p))
;;;
;;; Re(a) > 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check if conditions for f29p146test hold
(defun f29p146test (c v a)
  (cond ((eq (asksign a) '$positive)
	 (f29p146 c v a))
	(t
         (setq *hyp-return-noun-flag* 'fail-on-f29p146test))))

(defun f29p146 (c v a)
  (mul* 2 c
        (power (mul* a (inv 4) (inv *par*))
               (div v 2))
        (ktfork a v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bessel_k(v, sqrt(a)*sqrt(p)) in terms of bessel_k or in terms of
;; hypergeometric functions.
;;
;; Choose bessel_k if the order v is an integer.  (Why?)

(defun ktfork (a v)
  (let ((z (power (mul* a *par*) (1//2))))
    (cond ((maxima-integerp v)
           (simplify (list '(%bessel_k) v z)))
          (t
           (simpktf z v)))))

;; Express the Bessel K function in terms of hypergeometric functions.
;;
;; K[v](z) = %pi/2*(bessel_i(-v,z)-bessel(i,z))/sin(v*%pi)
;;
;; and
;;
;; bessel_i(v,z) = (z/2)^v/gamma(v+1)*0F1(;v+1;z^2/4)

(defun simpktf (z v)
  (let ((dz2 (div z 2)))
    (mul* '$%pi
          (1//2)
          (inv (simplify (list '(%sin) (mul v '$%pi))))
          (sub (mul* (power  dz2 (mul -1 v))
                     (inv (simplify (list '(%gamma) (sub 1 v))))
                     (hgfsimp-exec nil
                                   (list (sub 1
                                              v))
                                   (mul* z
                                         z
                                         (inv 4))))
               (mul* (power dz2 v)
                     (inv (simplify (list '(%gamma) (add v 1))))
                     (hgfsimp-exec nil
                                   (list (add v
                                              1))
                                   (mul* z
                                         z
                                         (inv 4))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 1.4: Laplace transform of exp(exp(-t))
;;;
;;; Table of Integral Transforms
;;;
;;; p. 147, formula 36:
;;;
;;; exp(-a*exp(-t))
;;;   -> a^(-p)*gamma(p,a)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun f36p147 (c a)
  (let ((-a (mul -1 a)))
    (mul c
         (power -a (mul -1 *par*))
         `(($gammagreek) ,*par* ,-a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 1.5: Laplace transform of exp(exp(t))
;;;
;;; Table of Integral Transforms
;;;
;;; p. 147, formula 36:
;;;
;;; exp(-a*exp(t))
;;;   -> a^(-p)*gamma_incomplete(-p,a)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun f37p147 (c a)
  (let ((-a (mul -1 a)))
    (mul c
         (power -a *par*)
         ($gamma_incomplete (mul -1 *par*) -a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 2: Laplace transform of u(t)*%e^(-p*t).
;;;
;;; u contains one or more special functions. Dispatches according to the 
;;; special functions involved in the Laplace transformable expression.
;;;
;;; We have three general types of integrands:
;;;
;;;   1. Call a function to return immediately the Laplace transform,
;;;      e.g. call lt-arbpow, lt-arbpow2, lt-log, whittest to return the 
;;;      Laplace transform.
;;;   2. Call lt-ltp directly or via an "expert function on Laplace transform",
;;;      transform the special function to a representation in terms of one 
;;;      hypergeometric function and do the integration
;;;      e.g. for a direct call of lt-ltp asin, atan or via lt2j for
;;;      an integrand with two bessel function.
;;;   3. Call fractest, fractest1, ... which transform the involved special 
;;;      function to a new representation. Send the transformed expression with 
;;;      the routine sendexec to the integrator and try to integrate the new
;;;      representation, e.g. gamma_incomplete is first transformed to a new
;;;      representation.
;;;      
;;;   The ordering of the calls to match a pattern is important.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-sf-log (u)
  (prog (l index1 index11 index2 index21 arg1 arg2 rest)
     
     ;; Laplace transform of asin(w)
     (cond ((setq l (m2-asin u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt-ltp 'asin rest arg1 nil))))
     
     ;; Laplace transform of atan(w)
     (cond ((setq l (m2-atan u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt-ltp 'atan rest arg1 nil))))
     
     ;; Laplace transform of two Bessel J functions
     (cond ((setq l (twoj u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     
     ;; Laplace transform of two hankel_1 functions
     (cond ((setq l (m2-two-hankel_1 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            ;; Call the code for the symbol %h.
            (return (fractest rest arg1 arg2 index1 1 index2 1 '2htjory))))
     
     ;; Laplace transform of two hankel_2 functions
     (cond ((setq l (m2-two-hankel_2 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            ;; Call the code for the symbol %h.
            (return (fractest rest arg1 arg2 index1 2 index2 2 '2htjory))))
     
     ;; Laplace transform of hankel_1 * hankel_2
     (cond ((setq l (m2-hankel_1*hankel_2 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            ;; Call the code for the symbol %h.
            (return (fractest rest arg1 arg2 index1 1 index2 2 '2htjory))))
     
     ;; Laplace transform of two Bessel Y functions
     (cond ((setq l (twoy u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest arg1 arg2 index1 nil index2 nil '2ytj))))
     
     ;; Laplace transform of two Bessel K functions
     (cond ((setq l (twok u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest arg1 arg2 index1 nil index2 nil '2kti))))
     
     ;; Laplace transform of Bessel K and Bessel Y functions
     (cond ((setq l (onekoney u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest rest arg1 arg2 index1 nil index2 nil 'ktiytj))))
     
     ;; Laplace transform of Bessel I and Bessel J functions
     (cond ((setq l (oneionej u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index21 (cdras 'v21 l)
		  arg1 (mul '$%i (cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul (power '$%i (neg index1)) (cdras 'u l)))
	    (return (lt2j rest arg1 arg2 index1 index2))))
     
     ;; Laplace transform of Bessel I and Hankel 1 functions
     (cond ((setq l (m2-bessel_i*hankel_1 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (mul '$%i (cdras 'w1 l))
                  arg2 (cdras 'w2 l)
                  rest (mul (power '$%i (neg index1)) (cdras 'u l)))
            (return (fractest1 rest arg1 arg2 index1 index2 1 'besshtjory))))
        
     ;; Laplace transform of Bessel I and Hankel 2 functions
     (cond ((setq l (m2-bessel_i*hankel_2 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (mul '$%i (cdras 'w1 l))
                  arg2 (cdras 'w2 l)
                  rest (mul (power '$%i (neg index1)) (cdras 'u l)))
            (return (fractest1 rest arg1 arg2 index1 index2 2 'besshtjory))))
     
     ;; Laplace transform of Bessel Y and Bessel J functions
     (cond ((setq l (oneyonej u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest arg2 arg1 index2 index1 nil 'bessytj))))
     
     ;; Laplace transform of Bessel K and Bessel J functions
     (cond ((setq l (onekonej u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest1 rest arg2 arg1 index2 index1 nil 'besskti))))
     
     ;; Laplace transform of Hankel 1 and Bessel J functions
     (cond ((setq l (m2-hankel_1*bessel_j u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            (return (fractest1 rest arg2 arg1 index2 index1 1 'besshtjory))))
     
     ;; Laplace transform of Hankel 2 and Bessel J functions
     (cond ((setq l (m2-hankel_2*bessel_j u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            (return (fractest1 rest arg2 arg1 index2 index1 2 'besshtjory))))
     
     ;; Laplace transform of Bessel Y and Hankel 1 functions
     (cond ((setq l (m2-bessel_y*hankel_1 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            (return (fractest1 rest arg2 arg1 index2 index1 1 'htjoryytj))))
     
     ;; Laplace transform of Bessel Y and Hankel 2 functions
     (cond ((setq l (m2-bessel_y*hankel_2 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            (return (fractest1 rest arg2 arg1 index2 index1 2 'htjoryytj))))
     
     ;; Laplace transform of Bessel K and Hankel 1 functions
     (cond ((setq l (m2-bessel_k*hankel_1 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            (return (fractest1 rest arg2 arg1 index2 index1 1 'htjorykti))))
     
     ;; Laplace transform of Bessel K and Hankel 2 functions
     (cond ((setq l (m2-bessel_k*hankel_2 u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w1 l)
                  arg2 (cdras 'w2 l)
                  rest (cdras 'u l))
            (return (fractest1 rest arg2 arg1 index2 index1 2 'htjorykti))))
     
     ;; Laplace transform of Bessel I and Bessel Y functions
     (cond ((setq l (oneioney u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (mul '$%i (cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul (power '$%i (neg index1)) (cdras 'u l)))
	    (return (fractest1 rest arg1 arg2 index1 index2 nil 'bessytj))))
     
     ;; Laplace transform of Bessel I and Bessel K functions
     (cond ((setq l (oneionek u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (mul '$%i (cdras 'w1 l))
		  arg2 (cdras 'w2 l)
		  rest (mul (power '$%i (neg index1)) (cdras 'u l)))
	    (return (fractest1 rest arg1 arg2 index1 index2 nil 'besskti))))
     
     ;; Laplace transform of Struve H function
     (cond ((setq l (m2-struve_h u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1hstruve rest arg1 index1))))
     
     ;; Laplace transform of Struve L function
     (cond ((setq l (m2-struve_l u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1lstruve rest arg1 index1))))
     
     ;; Laplace transform of little Lommel s function
     (cond ((setq l (ones u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1s rest arg1 index1 index2))))
     
     ;; Laplace transform of Lommel S function
     (cond ((setq l (oneslommel u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 index1 index2 'slommel))))
     
     ;; Laplace transform of Bessel Y function
     (cond ((setq l (oney u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1yref rest arg1 index1))))
     
     ;; Laplace transform of Bessel K function
     (cond ((setq l (onek u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
            (cond ((zerop1 index1)
                   ;; Special case for a zero index
                   (return (lt-bessel_k0 rest arg1)))
                  (t
                   (return (fractest2 rest arg1 index1 nil 'kti))))))
     
     ;; Laplace transform of Parabolic Cylinder function
     (cond ((setq l (m2-parabolic_cylinder_d u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (fractest2 rest arg1 index1 nil 'd))))
     
     ;; Laplace transform of Incomplete Gamma function
     (cond ((setq l (onegammaincomplete u))
	    (setq arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 arg2 nil 'gamma_incomplete))))
     
     ;; Laplace transform of Batemann function
     (cond ((setq l (onekbateman u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 index1 nil 'kbateman))))
     
     ;; Laplace transform of Bessel J function
     (cond ((setq l (onej u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1j rest arg1 index1))))
     
     ;; Laplace transform of Gamma greek function
     (cond ((setq l (onegammagreek u))
	    (setq arg1 (cdras 'w1 l)
		  arg2 (cdras 'w2 l)
		  rest (cdras 'u l))
	    (return (lt1gammagreek rest arg1 arg2))))
        
     ;; Laplace transform of Hankel 1 function
     (cond ((setq l (m2-hankel_1 u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (fractest2 rest arg1 index1 1 'htjory))))
     
     ;; Laplace transform of Hankel 2 function
     (cond ((setq l (m2-hankel_2 u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (fractest2 rest arg1 index1 2 'htjory))))
     
     ;; Laplace transform of Whittaker M function
     (cond ((setq l (onem u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1m rest arg1 index1 index11))))
     
     ;; Laplace transform of Whittaker M function
     (cond ((setq l (m2-whittaker_m u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1m rest arg1 index1 index2))))

     ;; Laplace transform of the Generalized Laguerre function, %l[v1,v2](w)
     (cond ((setq l (onel u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest arg1 index1 index11 'l))))

     ;; Laplace transform for the Generalized Laguerre function
     ;; We call the routine for %l[v1,v2](w).
     (cond ((setq l (one-gen-laguerre u))
            (setq index1  (cdras 'v1 l)
                  index2  (cdras 'v2 l)
                  arg1    (cdras 'w l)
                  rest    (cdras 'u l))
            (return (integertest rest arg1 index1 index2 'l))))
        
     ;; Laplace transform for the Laguerre function
     ;; We call the routine for %l[v1,0](w).
     (cond ((setq l (one-laguerre u))
            (setq index1  (cdras 'v1 l)
                  arg1    (cdras 'w l)
                  rest    (cdras 'u l))
            (return (integertest rest arg1 index1 0 'l))))
     
     ;; Laplace transform of Gegenbauer function
     (cond ((setq l (onec u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest arg1 index1 index11 'c))))
     
     ;; Laplace transform of Chebyshev function of the first kind
     (cond ((setq l (onet u))
	    (setq index1 (cdras 'v1 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest arg1 index1 nil 't))))
     
     ;; Laplace transform of Chebyshev function of the second kind
     (cond ((setq l (oneu u))
	    (setq index1 (cdras 'v1 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (integertest rest arg1 index1 nil 'u))))
     
     ;; Laplace transform for the Hermite function, hermite(index1,arg1)
     (cond ((setq l (one-hermite u))
            (setq index1 (cdras 'v1 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return 
              (cond ((and (maxima-integerp index1)
                         (or (mevenp index1)
                             (moddp index1)))
                     ;; When index1 is an even or odd integer, we transform
                     ;; directly to a hypergeometric function. For this case we
                     ;; get a Laplace transform when the arg is the
                     ;; square root of the variable.
                     (sendexec rest (hermite-to-hypergeometric index1 arg1)))
                    (t
                     (integertest rest arg1 index1 nil 'he))))))
     
     ;; Laplace transform of %p[v1,v2](w), Associated Legendre P function
     (cond ((setq l (hyp-onep u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1p rest arg1 index1 index11))))
     
     ;; Laplace transform of Associated Legendre P function
     (cond ((setq l (m2-assoc_legendre_p u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1p rest arg1 index1 index2))))
     
     ;; Laplace transform of %p[v1,v2,v3](w), Jacobi function
     (cond ((setq l (onepjac u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  index21 (cdras 'v3 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (pjactest rest arg1 index1 index2 index21))))
     
     ;; Laplace transform of Jacobi P function
     (cond ((setq l (m2-jacobi_p u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  index21 (cdras 'v3 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (pjactest rest arg1 index1 index2 index21))))
     
     ;; Laplace transform of Associated Legendre function of the second kind
     (cond ((setq l (oneq u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1q rest arg1 index1 index11))))
     
     ;; Laplace transform of Associated Legendre function of the second kind
     (cond ((setq l (m2-assoc_legendre_q u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1q rest arg1 index1 index2))))
     
     ;; Laplace transform of %p[v1](w), Legendre P function
     (cond ((setq l (onep0 u))
	    (setq index1 (cdras 'v1 l)
		  index11 0
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1p rest arg1 index1 index11))))
     
     ;; Laplace transform of Legendre P function
     (cond ((setq l (m2-legendre_p u))
            (setq index1 (cdras 'v1 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1p rest arg1 index1 0))))
     
     ;; Laplace transform of Whittaker W function
     (cond ((setq l (onew u))
	    (setq index1 (cdras 'v1 l)
		  index11 (cdras 'v2 l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (whittest rest arg1 index1 index11))))
     
     ;; Laplace transform of Whittaker W function
     (cond ((setq l (m2-whittaker_w u))
            (setq index1 (cdras 'v1 l)
                  index2 (cdras 'v2 l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (whittest rest arg1 index1 index2))))
     
     ;; Laplace transform of square of Bessel J function
     (cond ((setq l (onej^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1j^2 rest arg1 index1))))
     
     ;; Laplace transform of square of Hankel 1 function
     (cond ((setq l (m2-hankel_1^2 u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (fractest rest arg1 arg1 index1 1 index1 1 '2htjory))))
     
     ;; Laplace transform of square of Hankel 2 function
     (cond ((setq l (m2-hankel_2^2 u))
            (setq index1 (cdras 'v l)
                  arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (fractest rest arg1 arg1 index1 2 index1 2 '2htjory))))
     
     ;; Laplace transform of square of Bessel Y function
     (cond ((setq l (oney^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest rest arg1 arg1 index1 nil index1 nil '2ytj))))
     
     ;; Laplace transform of square of Bessel K function
     (cond ((setq l (onek^2 u))
	    (setq index1 (cdras 'v l)
		  arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest rest arg1 arg1 index1 nil index1 nil '2kti))))
     
     ;; Laplace transform of two Bessel I functions
     (cond ((setq l (twoi u))
	    (setq index1 (cdras 'v1 l)
		  index2 (cdras 'v2 l)
		  arg1 (mul '$%i (cdras 'w1 l))
		  arg2 (mul '$%i (cdras 'w2 l))
		  rest (mul (power '$%i (neg index1))
		            (power '$%i (neg index1))
		            (cdras 'u l)))
	    (return (lt2j rest arg1 arg2 index1 index2))))

     ;; Laplace transform of Bessel I. We use I[v](w)=%i^n*J[n](%i*w).
     (cond ((setq l (onei u))
	    (setq index1 (cdras 'v l)
		  arg1   (mul '$%i (cdras 'w l))
		  rest   (mul (power '$%i (neg index1)) (cdras 'u l)))
	    (return (lt1j rest arg1 index1))))
     
     ;; Laplace transform of square of Bessel I function
     (cond ((setq l (onei^2 u))
            (setq index1 (cdras 'v l)
                  arg1 (mul '$%i (cdras 'w l))
                  rest (mul (power '$%i (neg index1))
                            (power '$%i (neg index1))
                            (cdras 'u l)))
            (return (lt1j^2 rest arg1 index1))))
     
     ;; Laplace transform of Erf function
     (cond ((setq l (onerf u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1erf rest arg1))))

     ;; Laplace transform of the logarithmic function.
     ;; We add an algorithm for the Laplace transform and call the routine
     ;; lt-log. The old code is still present, but isn't called.
     (cond ((setq l (onelog u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt-log rest arg1))))
     
     ;; Laplace transform of Erfc function
     (cond ((setq l (onerfc u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (fractest2 rest arg1 nil nil 'erfc))))
       
     ;; Laplace transform of expintegral_ei.
     ;; Maxima uses the build in transformation to the gamma_incomplete 
     ;; function and simplifies the log functions of the transformation. We do 
     ;; not use the dispatch mechanism of fractest2, but call sendexec directly 
     ;; with the transformed function.
     (cond ((setq l (oneexpintegral_ei u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (let (($expintrep '%gamma_incomplete)
                  ($logexpand '$all))
              (return (sratsimp (sendexec rest ($expintegral_ei arg1)))))))
     
     ;; Laplace transform of expintegral_e1
     (cond ((setq l (oneexpintegral_e1 u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (let (($expintrep '%gamma_incomplete)
                  ($logexpand '$all))
              (return (sratsimp (sendexec rest ($expintegral_e1 arg1)))))))
     
     ;; Laplace transform of expintegral_e
     (cond ((setq l (oneexpintegral_e u))
            (setq arg1 (cdras 'v l)
                  arg2 (cdras 'w l)
                  rest (cdras 'u l))
            (let (($expintrep '%gamma_incomplete)
                  ($logexpand '$all))
              (return (sratsimp (sendexec rest ($expintegral_e arg1 arg2)))))))
     
     ;; Laplace transform of expintegral_si
     (cond ((setq l (oneexpintegral_si u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            ;; We transform to the hypergeometric representation.
            (return 
              (sendexec rest (expintegral_si-to-hypergeometric arg1)))))
     
     ;; Laplace transform of expintegral_shi
     (cond ((setq l (oneexpintegral_shi u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            ;; We transform to the hypergeometric representation.
            (return 
              (sendexec rest (expintegral_shi-to-hypergeometric arg1)))))
     
     ;; Laplace transform of expintegral_ci
     (cond ((setq l (oneexpintegral_ci u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            ;; We transform to the hypergeometric representation.
            ;; Because we have Logarithmic terms in the transformation, 
            ;; we switch on the flag $logexpand and do a ratsimp.
            (let (($logexpand '$super))
            (return
              (sratsimp
                (sendexec rest (expintegral_ci-to-hypergeometric arg1)))))))
     
     ;; Laplace transform of expintegral_chi
     (cond ((setq l (oneexpintegral_chi u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            ;; We transform to the hypergeometric representation.
            ;; Because we have Logarithmic terms in the transformation, 
            ;; we switch on the flag $logexpand and do a ratsimp.
            (let (($logexpand '$super))
            (return
              (sratsimp
                (sendexec rest (expintegral_chi-to-hypergeometric arg1)))))))
     
     ;; Laplace transform of Complete elliptic integral of the first kind
     (cond ((setq l (onekelliptic u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1kelliptic rest arg1))))
     
     ;; Laplace transform of Complete elliptic integral of the first kind
     (cond ((setq l (m2-elliptic_kc u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1kelliptic rest arg1))))
     
     ;; Laplace transform of Complete elliptic integral of the second kind
     (cond ((setq l (onee u))
	    (setq arg1 (cdras 'w l)
		  rest (cdras 'u l))
	    (return (lt1e rest arg1))))
     
     ;; Laplace transform of Complete elliptic integral of the second kind
     (cond ((setq l (m2-elliptic_ec u))
            (setq arg1 (cdras 'w l)
                  rest (cdras 'u l))
            (return (lt1e rest arg1))))
     
     ;; Laplace transform of %f[v1,v2](w1,w2,w3), Hypergeometric function
     ;; We support the Laplace transform of the build in symbol %f. We do
     ;; not use the mechanism of defining an "Expert on Laplace transform",
     ;; the expert function does a call to lt-ltp. We do this call directly.
     (cond ((setq l (onef u))
            (setq rest   (cdras 'u l)
                  arg1   (cdras 'w3 l)
                  index1 (list (cdras 'w1 l) (cdras 'w2 l)))
            (return (lt-ltp 'f rest arg1 index1))))
     
     ;; Laplace transform of Hypergeometric function
     (cond ((setq l (m2-hypergeometric u))
            (setq rest   (cdras 'u l)
                  arg1   (cdras 'w3 l)
                  index1 (list (cdras 'w1 l) (cdras 'w2 l)))
            (return (lt-ltp 'f rest arg1 index1))))
     
     ;; Laplace transform of c * t^v * (a+t)^w
     ;; It is possible to combine arbpow2 and arbpow.
     (cond ((setq l (m2-arbpow2 u))
            (setq rest   (cdras 'c l)
                  arg1   (cdras 'a l)
                  arg2   (cdras 'b l)
                  index1 (cdras 'v l)
                  index2 (cdras 'w l))
            (return (lt-arbpow2 rest arg1 arg2 index1 index2))))
     
     ;; Laplace transform of c * t^v
     (cond ((setq l (arbpow1 u))
	    (setq arg1 (cdras 'u l)
		  arg2 (cdras 'c l)
		  index1 (cdras 'v l))
	    (return (mul arg2 (lt-arbpow arg1 index1)))))
     
     ;; We have specialized the pattern for arbpow1. Now a lot of integrals
     ;; will fail correctly and we have to return a noun form.
     (return (setq *hyp-return-noun-flag* 'other-j-cases-next))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 2.1: Laplace transform of c*t^v*%e(-p*t)
;;;
;;; Table of Integral Transforms
;;;
;;; p. 137, formula 1:
;;;
;;; t^u*exp(-p*t)
;;;   -> gamma(u+1)*p^(-u-1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-arbpow (exp pow)
  (cond ((or (eq exp var) (zerp pow))
	 (f1p137test pow))
	(t
	 (setq *hyp-return-noun-flag* 'lt-arbow-failed))))

;; Check if conditions for f1p137 hold
(defun f1p137test (pow)
  (cond ((eq (asksign (add pow 1)) '$positive)
         (f1p137 pow))
        (t
         (setq *hyp-return-noun-flag* 'fail-in-arbpow))))

(defun f1p137 (pow)
  (mul (simplify (list '(%gamma) (add pow 1)))
       (power *par* (sub (mul -1 pow) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 2.2: Laplace transform of c*t^v*(1+t)^w
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-arbpow2 (c a b pow1 pow2)
  (when *debug-hypgeo*
    (format t "~&LT-ARBPOW2 (c a pow1 pow2):~%")
    (format t "~&   : c    = ~A~%" c)
    (format t "~&   : a    = ~A~%" a)
    (format t "~&   : b    = ~A~%" b)
    (format t "~&   : pow1 = ~A~%" pow1)
    (format t "~&   : pow2 = ~A~%" pow2))

  (if (eq (asksign (add pow1 1)) '$positive)
    (cond
      ((equal pow1 0)
       ;; The Laplace transform is an Incomplete Gamma function.
       (mul
	 c
	 (power a (add pow2 1))
	 (inv b)
	 (power (mul *par* a (inv b)) (mul -1 (add pow2 1)))
	 (power '$%e (mul *par* a (inv b)))
	 ($gamma_incomplete (add pow2 1) (mul *par* a (inv b)))))
      ((not (maxima-integerp (add pow1 pow2 2)))
       ;; The general result is a Hypergeometric U function U(a,b,z) which can
       ;; be represented by two Hypergeometic 1F1 functions for the special
       ;; case that the index b is not an integer value.
       (add
	 (mul
	   c
	   (power a (add pow1 pow2 1))
	   (inv (power b (add pow1 1)))
	   (simplify (list '(%gamma) (add pow1 pow2 1)))
	   (power (mul *par* a (inv b)) (mul -1 (add pow1 pow2 1)))
	   (hgfsimp-exec
	     (list (mul -1 pow2))
	     (list (mul -1 (add pow1 pow2)))
	     (mul *par* a (inv b))))
	 (mul
	   c
	   (power a (add pow1 pow2 1))
	   (inv (power b (add pow1 1)))
	   (simplify (list '(%gamma) (add pow1 1)))
	   (simplify (list '(%gamma) (mul -1 (add pow1 pow2 1))))
	   (inv (simplify (list '(%gamma) (mul -1 pow2))))
	   (hgfsimp-exec
	     (list (add pow1 1))
	     (list (add pow1 pow2 2))
	     (mul *par* a (inv b))))))
      (t
	;; The most general case is a result with the Hypergeometric U function.
	(mul
	  c
	  (power a (add pow1 pow2 1))
	  (inv (power b (add pow1 1)))
	  (simplify (list '(%gamma) (add pow1 1)))
	  (list
	   '(%hypergeometric_u)
	    (add pow1 1)
	    (add pow1 pow2 2)
	    (mul *par* a (inv b))))))
    (setq *hyp-return-noun-flag* 'lt-arbpow2-failed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 2.3: Laplace transform of the Logarithmic function
;;;
;;;    c*t^(v-1)*log(a*t)
;;;       -> c*gamma(v)*s^(-v)*(psi[0](v)-log(s/a))
;;;
;;; This is the formula for an expression with log(t) scaled like 1/a*F(s/a).
;;;
;;; For the following cases we have to add further algorithm:
;;;    log(1+a*x), log(x+a), log(x)^2.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-log (rest arg)

  (let* ((l (c*t^v rest))
	 (c (cdras 'c l))
	 (v (add (cdras 'v l) 1))) ; because v -> v-1

    (when *debug-hypgeo*
      (format t "~&LT-LOG (rest arg):~%")
      (format t "~&   : rest = ~A~%" rest)
      (format t "~&   : arg  = ~A~%" arg)
      (format t "~&   : l    = ~A~%" l))

    (cond
      ((and l (eq (asksign v) '$positive))
       (let* ((l1 (m2-a*t arg))
	      (a  (cdras 'a l1)))
	 (when *debug-hypgeo* (format t "~&   : l1 = ~A~%" l1))
	 (cond
	   (l1
	     (mul
	       c
	       (simplify (list '(%gamma) v))
	       (inv (power *par* v))
	       (sub
		 (simplify (list '(mqapply) (list '($psi array) 0) v))
		 (simplify (list '(%log) (div *par* a))))))
	   (t
	    (setq *hyp-return-noun-flag* 'lt-log-failed)))))
      (t
       (setq *hyp-return-noun-flag* 'lt-log-failed)))))

;;; Pattern for lt-log.
;;; Extract the argument of a function: a*t+c for c=0.

(defun m2-a*t (exp)
  (m2 exp
   '((mplus)
     ((mtimes) (t varp) (a freevar))
     ((coeffpp) (c zerp)))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algorithm 2.4: Laplace transfom of the Whittaker function
;;;
;;; Test for Whittaker W function.  Simplify this if possible, or
;;; convert to Whittaker M function.
;;;
;;; We have r * %w[i1,i2](a)
;;;
;;; Formula 16, p. 217
;;;
;;; t^(v-1)*%w[k,u](a*t)
;;;   -> gamma(u+v+1/2)*gamma(v-u+1/2)*a^(u+1/2)/
;;;          (gamma(v-k+1)*(p+a/2)^(u+v+1/2)
;;;        *2f1(u+v+1/2,u-k+1/2;v-k+1;(p-a/2)/(p+a/2))
;;;
;;; For Re(v +/- mu) > -1/2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whittest (r a i1 i2)
  (let (n m)
  (cond ((f16p217test r a i1 i2))
        ((and 
           (not 
             (and (maxima-integerp (setq n (sub (sub '((rat simp) 1 2) i2) i1)))
                  (member ($sign n) '($zero $neg $nz))))
           (not 
             (and (maxima-integerp (setq m (sub (add '((rat simp) 1 2) i2) i1)))
                  (member ($sign m) '($zero $neg $nz)))))
           ;; 1/2-u-k and 1/2+u-k are not zero or a negative integer
           ;; Transform to Whittaker M and try again.
           (distrexecinit ($expand (mul (init r) (wtm a i1 i2)))))
        (t
         ;; Both conditions fails, return a noun form.
         (setq *hyp-return-noun-flag* 'whittest-failed)))))

(defun f16p217test (r a i1 i2)
  ;; We have r*%w[i1,i2](a)
  (let ((l (c*t^v r)))
    ;; Make sure r is of the form c*t^v
    (when l
      (let* ((v (add (cdras 'v l) 1))
             (c (cdras 'c l)))
        ;; Check that v + i2 + 1/2 > 0 and v - i2 + 1/2 > 0.
        (when (and (eq (asksign (add (add v i2) 1//2)) '$positive)
                   (eq (asksign (add (sub v i2) 1//2)) '$positive))
          ;; Ok, we satisfy the conditions.  Now extract the arg.
          ;; The transformation is only valid for an argument a*t. We have
          ;; to special the pattern to make sure that we satisfy the condition.
          (let ((l (m2-a*t a) ; more special pattern
;                   (m2 a
;                       '((mplus)
;                         ((coeffpt) (f hasvar) (a freevar))
;                         ((coeffpp) (c zerp)))
;                       nil)
                   ))
            (when l
              (let ((a (cdras 'a l)))
                ;; We're ready now to compute the transform.
                (mul* c
                      (power a (add i2 1//2))
                      (simplify (list '(%gamma) (add (add v i2) 1//2)))
                      (simplify (list '(%gamma) (add (sub v i2) 1//2)))
                      (inv (mul* (simplify (list '(%gamma) (add (sub v i1) 1)))
                                 (power (add *par* (div a 2))
                                        (add (add i2 v) 1//2))))
                      (hgfsimp-exec (list (add (add i2 v 1//2))
                                          (add (sub i2 i1) 1//2))
                                    (list (add (sub v i1) 1))
                                    (div (sub *par* (div a 2))
                                         (add *par* (div a 2)))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algorithm 2.5: Laplace transfom of bessel_k(0,a*t)
;;;
;;; The general algorithm handles the Bessel K function for an order |v|<1.
;;; but does not include the special case v=0. Return the Laplace transform:
;;;
;;;   bessel_k(0,a*t) --> acosh(s/a)/sqrt(s^2-a^2)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-bessel_k0 (rest arg)
  (let* ((l (c*t^v rest))
         (c (cdras 'c l))
         (v (cdras 'v l))
         (l (m2-a*t arg))
         (a (cdras 'a l)))
    (cond ((and l (zerop1 v))
           (mul c
                (simplify (list '(%acosh) (div *par* a)))
                (inv (power (sub (mul *par* *par*) (mul a a))
                            '((rat simp) 1 2)))))
          (t
            (setq *hyp-return-noun-flag* 'lt-bessel_k-failed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DISPATCH FUNCTIONS TO CHANGE THE REPRESENTATION OF SPECIAL FUNCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Laplace transform of a product of Bessel functions.  A1, A2 are
;; the args of the two functions. I1, I2 are the indices of each
;; function.  I11, I21 are secondary indices of each function, if any.
;; FLG is a symbol indicating how we should handle the special
;; functions (and also indicates what the special functions are.)
;;
;; I11 and I21 are for the Hankel functions.

(defun fractest (r a1 a2 i1 i11 i2 i21 flg)
  (cond ((or (and (listp i1) (equal (caar i1) 'rat)
		  (listp i2) (equal (caar i2) 'rat))
	     (eq flg '2htjory))
         ;; We have to Bessel or Hankel functions. Both indizes have to be 
         ;; rational numbers or we have two Hankel functions.
	 (sendexec r
		   (cond ((eq flg '2ytj)
			  (mul (ytj i1 a1)
			       (ytj i2 a2)))
			 ((eq flg '2htjory)
			  (mul (htjory i1 i11 a1)
			       (htjory i2 i21 a2)))
			 ((eq flg 'ktiytj)
			  (mul (kti i1 a1)
			       (ytj i2 a2)))
			 ((eq flg '2kti)
			  (mul (kti i1 a1)
			       (kti i2 a2))))))
	(t 
         (setq *hyp-return-noun-flag* 'product-of-y-with-nofract-indices))))

;; Laplace transform of a product of Bessel functions.  A1, A2 are
;; the args of the two functions. I1, I2 are the indices of each
;; function.  I is a secondary index to one function, if any.
;; FLG is a symbol indicating how we should handle the special
;; functions (and also indicates what the special functions are.)
;;
;; I is for the kind of Hankel function.

(defun fractest1 (r a1 a2 i1 i2 i flg)
  (cond ((or (and (listp i2)
		  (equal (caar i2) 'rat))
	     (eq flg 'besshtjory))
         ;; We have two Bessel or Hankel functions. The second index has to
         ;; be a rational number or one of the functions is a Hankel function
         ;; and the second function is Bessel J or Bessel I
	 (sendexec r
		   (cond ((eq flg 'bessytj)
			  (mul (simplify (list '(%bessel_j) i1 a1))
			       (ytj i2 a2)))
			 ((eq flg 'besshtjory)
			  (mul (simplify (list '(%bessel_j) i1 a1))
			       (htjory i2 i a2)))
			 ((eq flg 'htjoryytj)
			  (mul (htjory i1 i a1)
			       (ytj i2 a2)))
			 ((eq flg 'besskti)
			  (mul (simplify (list '(%bessel_j) i1 a1))
			       (kti i2 a2)))
			 ((eq flg 'htjorykti)
			  (mul (htjory i1 i a1)
			       (kti i2 a2))))))
	(t 
         (setq *hyp-return-noun-flag* 'product-of-i-y-of-nofract-index))))

;; Laplace transform of a single special function.  A is the arg of
;; the special function. I1, I11 are the indices of the function.  FLG
;; is a symbol indicating how we should handle the special functions
;; (and also indicates what the special functions are.)
;;
;; I11 is the kind of Hankel function

(defun fractest2 (r a1 i1 i11 flg)
  (cond ((or (and (listp i1)
		  (equal (caar i1) 'rat))
	     (eq flg 'd)
	     (eq flg 'kbateman)
	     (eq flg 'gamma_incomplete)
	     (eq flg 'htjory)
	     (eq flg 'erfc)
	     (eq flg 'slommel)
	     (eq flg 'ytj))
         ;; The index is a rational number or flg has the value of one of the
         ;; above special functions.
	 (sendexec r
		   (cond ((eq flg 'ytj)
			  (ytj i1 a1))
			 ((eq flg 'htjory)
			  (htjory i1 i11 a1))
			 ((eq flg 'd)
			  (dtw i1 a1))
			 ((eq flg 'kbateman)
			  (kbatemantw i1 a1))
			 ((eq flg 'gamma_incomplete)
			  (gamma_incomplete-to-gammagreek a1 i1))
			 ((eq flg 'kti)
			  (kti i1 a1))
			 ((eq flg 'erfc)
			  (erfctd a1))
			 ((eq flg 'slommel)
			  (slommeltjandy i1 i11 a1)))))
	(t
	  (setq *hyp-return-noun-flag* 'y-of-nofract-index))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integertest (r arg i1 i2 flg)
  (cond ((maxima-integerp i1)
         (dispatchpoltrans r arg i1 i2 flg))
        (t 
         (setq *hyp-return-noun-flag* 'index-should-be-an-integer-in-polys))))

(defun dispatchpoltrans (r x i1 i2 flg)
  (sendexec r
            (cond ((eq flg 'l)(ltw x i1 i2))
                  ((eq flg 'he)(hetd x i1))
                  ((eq flg 'c)(ctpjac x i1 i2))
                  ((eq flg 't)(ttpjac x i1))
                  ((eq flg 'u)(utpjac x i1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (-1)^n*n!*laguerre(n,a,x) = U(-n,a+1,x)
;;
;; W[k,u](z) = exp(-z/2)*z^(u+1/2)*U(1/2+u-k,1+2*u,z)
;;
;; So
;;
;; laguerre(n,a,x) = (-1)^n*U(-n,a+1,x)/n!
;;
;; U(-n,a+1,x) = exp(z/2)*z^(-a/2-1/2)*W[1/2+a/2+n,a/2](z)
;;
;; Finally,
;;
;; laguerre(n,a,x) = (-1)^n/n!*exp(z/2)*z^(-a/2-1/2)*W[1/2+a/2+n,a/2](z)
;;
;; This formula is not correct.
;;
;; Here the correct documentation has to be added.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defun ltw (x n a)
  (let ((diva2 (div a 2)))
    (mul* (power -1 n)
          (inv (factorial n))
          (power x (sub (inv -2) diva2))
          (power '$%e (div x 2))
          (wwhit x (add (1//2) diva2 n) diva2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ltw (x n a)
  (let ((diva2 (div a 2)))
    (mul
      ;; Use the Gamma function and not the Factorial function.
      (simplify (list '(%gamma) (add n a 1)))
      (inv (simplify (list '(%gamma) (add a 1))))
      (inv (simplify (list '(%gamma) (add n 1))))
      (power x (sub (inv -2) diva2))
      (power '$%e (div x 2))
      ;; Correct is Whittaker M not W
      (list '(mqapply) (list '($%m array) (add (inv 2) diva2 n) diva2) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hermite He function as a parabolic cylinder function
;;
;; Tables of Integral Transforms
;;
;; p. 386
;;
;; D[n](z) = (-1)^n*exp(z^2/4)*diff(exp(-z^2/2),z,n);
;;
;; p. 369
;;
;; He[n](x) = (-1)^n*exp(x^2/2)*diff(exp(-x^2/2),x,n)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hetd (x n)
  (mul* (power '$%e (mul* x x (inv 4)))
        ;; At this time the Parabolic Cylinder D function is not implemented
        ;; as a simplifying function. We call nevertheless the simplifer
        ;; to simplify the arguments. When we implement the function
        ;; The symbol has to be changed to the noun form.
        (simplify (list '($parabolic_cylinder_d) n x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Transform Gegenbauer function to Jacobi P function
(defun ctpjac (x n v)
  (let ((inv2 (1//2)))
    (mul* (simplify (list '(%gamma) (add v v n)))
          (inv (simplify (list '(%gamma) (add v v))))
          (simplify (list '(%gamma) (add inv2 v)))
          (inv (simplify (list '(%gamma) (add v inv2 n))))
          (pjac x n (sub v inv2) (sub v inv2)))))

;; Transform Chebyshev T function to Jacobi P function
(defun ttpjac (x n)
  (let ((inv2 (1//2)))
    (mul* (factorial n)
          (simplify (list '(%gamma) inv2))
          (inv (simplify (list '(%gamma) (add inv2 n))))
          (pjac x n (mul -1 inv2) (mul -1 inv2)))))

;; Transform Chebyshev U function to Jacobi P function
(defun utpjac (x n)
  (let ((inv2 (1//2)))
    (mul* (factorial (add n 1))
          inv2
          (simplify (list '(%gamma) inv2))
          (inv (simplify (list '(%gamma) (add inv2 n 1))))
          (pjac x n inv2 inv2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pjactest (rest arg index1 index2 index3)
  (cond ((maxima-integerp index1)
         (lt-ltp 'onepjac
                 rest
                 arg
                 (list index1 index2 index3)))
        (t 
         (setq *hyp-return-noun-flag* 'ind-should-be-an-integer-in-polys))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Laplace transform of a single Bessel Y function.
;;;
;;; REST is the multiplier, ARG1 is the arg, and INDEX1 is the order of
;;; the Bessel Y function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt1yref (rest arg1 index1)
  ;; If the index is an integer, use LT1Y.  Otherwise, convert Bessel
  ;; Y to Bessel J and compute the transform of that.  We do this
  ;; because converting Y to J for an integer index doesn't work so
  ;; well without taking limits.
  (cond ((maxima-integerp index1)
         ;; Do not call lt1y but lty directly.
         ;; lt1y calls lt-ltp with the flag 'oney. lt-ltp checks this flag
         ;; and calls lty. So we can do it at this place and the algorithm is
         ;; more simple.
	 (lty rest arg1 index1))
	(t (fractest2 rest arg1 index1 nil 'ytj))))

; Not used in Maxima core or share
;(defun eqrat (a)
;  (cond ((numberp a) nil)
;        (t (equal (caar a) 'rat))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORMATIONS TO CHANGE THE REPRESENTATION OF SPECIAL FUNCTIONS
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; erfc in terms of D, parabolic cylinder function
;;
;; Tables of Integral Transforms
;;
;; p 387:
;; erfc(x) = (%pi*x)^(-1/2)*exp(-x^2/2)*W[-1/4,1/4](x^2)
;;
;; p 386:
;; D[v](z) = 2^(v/2+1/2)*z^(-1/2)*W[v/2+1/4,1/4](z^2/2)
;;
;; So
;;
;; erfc(x) = %pi^(-1/2)*2^(1/4)*exp(-x^2/2)*D[-1](x*sqrt(2))

(defun erfctd (x)
  (let ((inv2 (1//2)))
    (mul* (power 2 inv2)		; Should this be 2^(1/4)?
	  (power '$%pi (mul* -1 inv2))
	  (power '$%e (mul* -1 inv2 x x))
          ;; At this time the Parabolic Cylinder D function is not implemented
          ;; as a simplifying function. We call nevertheless the simplifer
          ;; to simplify the arguments. When we implement the function
          ;; The symbol has to be changed to the noun form.
          (simplify 
            (list '($parabolic_cylinder_d) -1 (mul (power 2 inv2) x))))))

;; Lommel S function in terms of Bessel J and Bessel Y.
;; Luke gives
;;
;; S[u,v](z) = s[u,v](z) + {2^(u-1)*gamma((u-v+1)/2)*gamma((u+v+1)/2)}
;;                 * {sin[(u-v)*%pi/2]*bessel_j(v,z)
;;                     - cos[(u-v)*%pi/2]*bessel_y(v,z)

(defun slommeltjandy (m n z)
  (let ((arg (mul (1//2) '$%pi (sub m n))))
    (add (littleslommel m n z)
         (mul (power 2 (sub m 1))
         (simplify (list '(%gamma) (div (sub (add m 1) n) 2)))
         (simplify (list '(%gamma) (div (add m n 1) 2)))
         (sub (mul (simplify (list '(%sin) arg))
              (simplify (list '(%bessel_j) n z)))
         (mul (simplify (list '(%cos) arg))
              ;; (bess n z 'Y) is replaced with bessel_y.
              ;; The old code causes the use of bessel_i. 
              ;; The Laplace transform for the S Lommel function
              ;; needs verification. DK 09/2009.
              (simplify (list '(%bessel_y) n z))))))))

;; Whittaker W function in terms of Whittaker M function
;;
;; A&S 13.1.34
;;
;; W[k,u](z) = gamma(-2*u)/gamma(1/2-u-k)*M[k,u](z)
;;              + gamma(2*u)/gamma(1/2+u-k)*M[k,-u](z)

(defun wtm (a i1 i2)
  (add (mul* (simplify (list '(%gamma) (mul -2 i2)))
	     (mwhit a i1 i2)
	     (inv (simplify (list '(%gamma) (sub (sub (1//2) i2) i1)))))
       (mul* (simplify (list '(%gamma) (add i2 i2)))
	     (mwhit a i1 (mul -1 i2))
	     (inv (simplify (list '(%gamma) (sub (add (1//2) i2) i1)))))))

;; Incomplete gamma function in terms of Whittaker W function
;;
;; Tables of Integral Transforms, p. 387
;;
;; gamma_incomplete(a,x) = x^((a-1)/2)*exp(-x/2)*W[(a-1)/2,a/2](x)

(defun gammaincompletetw (a x)
  (mul* (power x (div (sub a 1) 2))
	(power '$%e (div x -2))
	(wwhit x (div (sub a 1) 2)(div a 2))))

;;; Incomplete Gamma function in terms of Gammagreek function
;;;
;;; Only for a=0 we use the general representation as a Whittaker W function:
;;;
;;;   gamma_incomplete(a,x) = x^((a-1)/2)*exp(-x/2)*W[(a-1)/2,a/2](x)
;;;
;;; In all other cases we transform to a Gammagreek function:
;;;
;;;   gamma_incomplete(a,x) = gamma(a)- gammagreek(a,x)
;;;
;;; The Gammagreek function will be further transformed to a Hypergeometric 1F1
;;; representation. With this change we get more simple and correct results for
;;; the Laplace transform of the Incomplete Gamma function.

(defun gamma_incomplete-to-gammagreek (a x)
  (if (or (eq ($sign a) '$zero)
          (and (integerp a) (< a 0)))
    ;; The representation as a Whittaker W function for a=0 or a negative 
    ;; integer (The gamma function is not defined for this case.)
    (mul (power x (div (sub a 1) 2))
         (power '$%e (div x -2))
         (wwhit x (div (sub a 1) 2) (div a 2)))
    ;; In all other cases the representation as a Gammagreek function
    (sub (simplify (list '(%gamma) a))
         (list '($gammagreek) a x))))

;; Bessel Y in terms of Bessel J
;;
;; A&S 9.1.2:
;;
;; bessel_y(v,z) = bessel_j(v,z)*cot(v*%pi)-bessel_j(-v,z)/sin(v*%pi)

(defun ytj (i a)
  (sub (mul (simplify (list '(%bessel_j) i a)) 
            (simplify (list '(%cot) (mul i '$%pi))))
       (mul (simplify (list '(%bessel_j) (mul -1 i) a))
            (inv (simplify (list '(%sin) (mul i '$%pi)))))))

;; Parabolic cylinder function in terms of Whittaker W function.
;;
;; See Table of Integral Transforms, p.386:
;;
;; D[v](z) = 2^(v/2+1/4)*z^(-1/2)*W[v/2+1/4,1/4](z^2/2)

(defun dtw (i a)
  (mul* (power 2 (add (div i 2) (inv 4)))
	(power a (inv -2))
	(wwhit (mul* a a (1//2))
	       (add (div i 2) (inv 4))
	       (inv 4))))

;; Bateman's function in terms of Whittaker W function
;;
;; See Table of Integral Transforms, p.386:
;;
;; k[2*v](z) = 1/gamma(v+1)*W[v,1/2](2*z)

(defun kbatemantw (v a)
  (div (wwhit (add a a) (div v 2) (1//2))
       (simplify (list '(%gamma) (add (div v 2) 1)))))

;; Bessel K in terms of Bessel I
;;
;; A&S 9.6.2
;;
;; bessel_k(v,z) = %pi/2*(bessel_i(-v,z)-bessel_i(v,z))/sin(v*%pi)

(defun kti (i a)
  (mul '$%pi
       (1//2)
       (inv (simplify (list '(%sin) (mul i '$%pi))))
       (sub (simplify (list '(%bessel_i) (mul -1 i) a))
            (simplify (list '(%bessel_i) i a)))))

;; Bessel Y
;; No longer used in Maxima core and share.
;(defun bessy (v z)
;  `((%bessel_y) ,v ,z))

;; Bessel K
;; No longer used in Maxima core and share.
;(defun kmodbes(z v)
;  `((%bessel_k) ,v ,z))

;; No longer used in Maxima core and share.
;(defun tan% (arg)
;  (list '(%tan) arg))

;; Express Hankel function in terms of Bessel J or Y function.
;;
;; A&S 9.1.3
;;
;; H[v,1](z) = %i*csc(v*%pi)*(exp(-v*%pi*%i)*bessel_j(v,z) - bessel_j(-v,z))
;;
;; A&S 9.1.4:
;; H[v,2](z) = %i*csc(v*%pi)*(bessel_j(-v,z) - exp(-v*%pi*%i)*bessel_j(v,z))
;;
;; Both formula are not valid for v an integer. 
;; For this case use the definitions of the Hankel functions:
;;    H[v,1](z) = bessel_j(v,z) + %i* bessel_y(v,z)
;;    H[v,2](z) = bessel_j(v,z) - %i* bessel_y(v,z)
;;
;; All this can be implemented more simple.
;; We do not need the transformation to bessel_j for rational numbers,
;; because the correct transformation for bessel_y is already implemented.
;; It is enough to use the definitions for the Hankel functions.

(defun htjory (v sort z)
  ;; V is the order, SORT is the kind of Hankel function (1 or 2), Z
  ;; is the arg.
  (cond ((and (consp v)
	      (consp (car v))
	      (equal (caar v) 'rat))
	 ;; If the order is a rational number of some sort,
	 ;;
	 ;; (bessel_j(-v,z) - bessel_j(v,z)*exp(-v*%pi*%i))/(%i*sin(v*%pi*%i))
	 (div (numjory v sort z 'j)
	      (mul '$%i (simplify (list '(%sin) (mul v '$%pi))))))
        ((equal sort 1)
         ;; Transform hankel_1(v,z) to bessel_j(v,z)+%i*bessel_y(v,z)
         (add (simplify (list '(%bessel_j) v z))
              (mul '$%i (simplify (list '(%bessel_y) v z)))))
        ((equal sort 2)
         ;; Transform hankel_2(v,z) to bessel_j(v,z)-%i*bessel_y(v,z)
         (sub (simplify (list '(%bessel_j) v z))
              (mul '$%i (simplify (list '(%bessel_y) v z)))))
        (t
         ;; We should never reach this point of code.
         ;; Problem: The user input for the symbol %h[v,sort](t) is not checked.
         ;; Therefore the user can generate this error as long as we do not cut
         ;; out the support for the Laplace transform of the symbol %h.
         (merror "htjory: Called with wrong sort of Hankel functions."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Three helper functions only used by htjory

;; Bessel J or Y, depending on if FLG is 'J or not.
(defun desjy (v z flg)
  (cond ((eq flg 'j)
         (simplify (list '(%bessel_j) v z)))
        (t
         (simplify (list '(%bessel_y) v z)))))

(defun numjory (v sort z flg)
  (cond ((equal sort 1)
         ;; bessel(-v, z) - exp(-v*%pi*%i)*bessel(v, z)
         ;;
         ;; Where bessel is bessel_j if FLG is 'j.  Otherwise, bessel
         ;; is bessel_y.
         ;;
         ;; bessel_y(-v, z) - exp(-v*%pi*%i)*bessel_y(v, z)
         (sub (desjy (mul -1 v) z flg)
              (mul* (power '$%e (mul* -1 v '$%pi '$%i))
                    (desjy v z flg))))
        (t
         ;; exp(-v*%pi*%i)*bessel(v,z) - bessel(-v,z), where bessel is
         ;; bessel_j or bessel_y, depending on if FLG is 'j or not.
         (sub (mul* (power '$%e (mul* v '$%pi '$%i))
                    (desmjy v z flg))
              (desmjy (mul -1 v) z flg)))))

(defun desmjy (v z flg)
  (cond ((eq flg 'j)
         ;; bessel_j(v,z)
         (simplify (list '(%bessel_j) v z)))
        (t
         ;; -bessel_y(v,z)
         (mul -1 (simplify (list '(%bessel_y) v z))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORM TO HYPERGEOMETRIC FUNCTION WITHOUT USING THE ROUTINE REF
;;;
;;; This functions are called in the routine lt-sf-log to get the
;;; representation in terms of a hypergeometric function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The algorithm of the implemented Hermite function %he does not work for
;; the known Laplace transforms. For an even or odd integer order, we 
;; can represent the Hermite function by the Hypergeometric function 1F1.
;; With this representations we get the expected Laplace transforms.
(defun hermite-to-hypergeometric (order arg)
  (cond
    ((and (maxima-integerp order)
          (mevenp order))
     ;; Transform to 1F1 for order an even integer
     (mul
        (power 2 order)
        (power '$%pi (div 1 2))
        (inv (simplify (list '(%gamma) (div (sub 1 order) 2))))
        (list '(mqapply) (list '($%f array) 1 1)
                         (list '(mlist) (div order -2)) 
                         (list '(mlist) (div 1 2))
                         (mul arg arg))))

     ((and (maxima-integerp order) 
           (moddp order))
      ;; Transform to 1F1 for order an odd integer
      (mul -2 arg
        (power 2 order)
        (power '$%pi (div 1 2))
        (inv (simplify (list '(%gamma) (div order -2))))
        (list '(mqapply) (list '($%f array) 1 1)
                         (list '(mlist) (div (sub 1 order) 2))
                         (list '(mlist) (div 3 2))
                         (mul arg arg))))
     (t
      ;; The general case, transform to 2F0
      ;; For this case we have no Laplace transform.
      (mul
        (power (mul 2 arg) order)
        (list '(mqapply) (list '($%f array) 2 0)
                         (list '(mlist) (div order 2) 
                                        (div (sub 1 order) 2))
                         (list '(mlist))
                         (div -1 (mul arg arg)))))))

;;; Hypergeometric representation of the Exponential Integral Si
;;; Si(z) = z*1F2([1/2],[3/2,3/2],-z^2/4)
(defun expintegral_si-to-hypergeometric (arg)
  (mul arg
       (list '(mqapply) (list '($%f array) 1 2)
             (list '(mlist) (div 1 2))
             (list '(mlist) (div 3 2) (div 3 2))
             (div (mul -1 arg arg) 4))))

;;; Hypergeometric representation of the Exponential Integral Shi
;;; Shi(z) = z*1F2([1/2],[3/2,3/2],z^2/4)
(defun expintegral_shi-to-hypergeometric (arg)
  (mul arg
       (list '(mqapply) (list '($%f array) 1 2)
             (list '(mlist) (div 1 2))
             (list '(mlist) (div 3 2) (div 3 2))
             (div (mul arg arg) 4))))

;;; Hypergeometric representation of the Exponential Integral Ci
;;; Ci(z) = -z^2/4*2F3([1,1],[2,2,3/2],-z^2/4)+log(z)+%gamma
(defun expintegral_ci-to-hypergeometric (arg)
  (add (mul (div (mul -1 arg arg) 4)
            (list '(mqapply) (list '($%f array) 2 3)
                  (list '(mlist) 1 1)
                  (list '(mlist) 2 2 (div 3 2))
                  (div (mul -1 arg arg) 4)))
            (simplify (list '(%log) arg))
            '$%gamma))

;;; Hypergeometric representation of the Exponential Integral Chi
;;; Chi(z) = z^2/4*2F3([1,1],[2,2,3/2],z^2/4)+log(z)+%gamma
(defun expintegral_chi-to-hypergeometric (arg)
  (add (mul (div (mul arg arg) 4)
            (list '(mqapply) (list '($%f array) 2 3)
                  (list '(mlist) 1 1)
                  (list '(mlist) 2 2 (div 3 2))
                  (div (mul arg arg) 4)))
            (simplify (list '(%log) arg))
            '$%gamma))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EXPERTS ON LAPLACE TRANSFORMS
;;; 
;;; LT<foo> functions are various experts on Laplace transforms of the
;;; function <foo>.  The expression being transformed is
;;; r*<foo>(args).  The first arg of each expert is r, The remaining
;;; args are the arg(s) and/or parameters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Laplace transform of one Bessel J
(defun lt1j (rest arg index)
  (lt-ltp 'onej rest arg index))

; This function is no longer needed. We call lty immediately and not via lt-ltp.
;;; Laplace transform of one Bessel Y
;(defun lt1y (rest arg index)
;  (lt-ltp 'oney rest arg index))

;; Laplace transform of two Bessel J functions.  
;; The argument of each must be the same, but the orders may be different.
(defun lt2j (rest arg1 arg2 index1 index2)
  (cond ((not (equal arg1 arg2))
         (setq *hyp-return-noun-flag* 'product-of-bessel-with-different-args))
        (t 
          ;; Call lt-ltp two transform and integrate two Bessel J functions.
          (lt-ltp 'twoj rest arg1 (list 'list index1 index2)))))

;; Laplace transform of a square of a Bessel J function
(defun lt1j^2 (rest arg index)
  (cond ((alike1 index '((rat) -1 2))
         ;; Special case: Laplace transform of bessel_j(v,arg)^2, v = -1/2.
         ;; For this case the algorithm for the product of two Bessel functions
         ;; does not work. Call the integrator with the hypergeometric 
         ;; representation: 2/%pi/arg*1/2*(1+0F1([], [1/2], -arg*arg)).
         (sendexec (mul (div 2 '$%pi)
                        (inv arg)
                        rest)
                   (add '((rat simp) 1 2)
                        (mul '((rat simp) 1 2)
                             (list '(mqapply) (list '($%f array) 1 0)
                                   (list '(mlist) )
                                   (list '(mlist) '((rat simp) 1 2))
                                   (mul -1 (mul arg arg)))))))
        (t
         (lt-ltp 'twoj rest arg (list 'list index index)))))

;; Laplace transform of Incomplete Gamma function
(defun lt1gammagreek (rest arg1 arg2)
  (lt-ltp 'gammagreek rest arg2 arg1))

;; Laplace transform of Whittaker M function
(defun lt1m (r a i1 i2)
  (lt-ltp 'onem r a (list i1 i2)))

;; Laplace transform of Jacobi function
(defun lt1p (r a i1 i2)
  (lt-ltp 'hyp-onep r a (list i1 i2)))

;; Laplace transform of Associated Legendre function of the second kind
(defun lt1q (r a i1 i2)
  (lt-ltp 'oneq r a (list i1 i2)))

;; Laplace transform of Erf function
(defun lt1erf (rest arg)
  (lt-ltp 'onerf rest arg nil))

;; Laplace transform of Log function
(defun lt1log (rest arg)
  (lt-ltp 'onelog rest arg nil))

;; Laplace transform of Complete elliptic integral of the first kind
(defun lt1kelliptic (rest arg)
  (lt-ltp 'onekelliptic rest arg nil))

;; Laplace transform of Complete elliptic integral of the second kind
(defun lt1e (rest arg)
  (lt-ltp 'onee rest arg nil))

;; Laplace transform of Struve H function
(defun lt1hstruve (rest arg1 index1)
  (lt-ltp 'hs rest arg1 index1))

;; Laplace transform of Struve L function
(defun lt1lstruve (rest arg1 index1)
  (lt-ltp 'hl rest arg1 index1))

;; Laplace transform of Lommel s function
(defun lt1s (rest arg1 index1 index2)
  (lt-ltp 's rest arg1 (list index1 index2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORM TO HYPERGEOMETRIC FUNCTION AND DO THE INTEGRATION
;;;
;;; FLG = special function we're transforming
;;; REST = other stuff
;;; ARG = arg of special function
;;; INDEX = index of special function.
;;;
;;; So we're transforming REST*FLG(INDEX, ARG).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lt-ltp (flg rest arg index)

  (when *debug-hypgeo*
    (format t "~&LT-LTP (flg rest arg index):~%")
    (format t "~&   : flg   = ~A~%" flg)
    (format t "~&   : rest  = ~A~%" rest)
    (format t "~&   : arg   = ~A~%" arg)
    (format t "~&   : index = ~A~%" index))

  (prog (index1 index2 argl const l l1)
     (when (or (zerp index)
	       (eq flg 'onerf)
	       (eq flg 'onekelliptic)
	       (eq flg 'onee)
	       (eq flg 'onepjac)
	       (eq flg 'd)
	       (eq flg 's)
	       (eq flg 'hs)
	       (eq flg 'ls)
	       (eq flg 'onem)
	       (eq flg 'oneq)
	       (eq flg 'gammagreek)
	       (eq flg 'asin)
	       (eq flg 'atan)
               (eq flg 'f))         ; hypergeometric function
	    (go labl))
     (cond ((or (eq flg 'hyp-onep)
		(eq flg 'onelog))
	    ;; Go to labl1 if we've got %p or log.
	    (go labl1)))
     ;; Skip this if we have exactly one index or if INDEX doesn't
     ;; look like '(LIST i1 i2)
     (cond ((not (consp index))
	    (go lab)))
     (cond ((not (eq (car index) 'list))
	    (go lab)))
     ;; If the first index is exactly 0, set INDEX1 to it and go to
     ;; LA.
     (cond ((zerp (setq index1 (cadr index)))(go la)))
     ;; If we're here, the index is of the form '(LIST i1 i2), which
     ;; means this is the product of two Bessel functions.
     ;;
     ;; Ok.  I think this is wrong, in general.  I think we get here
     ;; if we're doing the produce to two Bessel functions.  This code
     ;; seems to be applying the property that bessel_j(-n,x) =
     ;; (-1)^n*bessel_j(n,x).  But this only works if n is an integer.
     #+nil
     (cond ((eq (checksigntm (simplifya (inv (setq index1
						   (cadr
						    index)))
					nil))
		'$negative)
	    ;; FIXME: What is this supposed to do?  We take the
	    ;; reciprocal of the first index and see if it's negative.
	    ;; If so, we change the sign of index1 and divide REST by
	    ;; the index.  What is this for?
	    (setq index1
		  (mul -1 index1)
		  rest
		  (mul* (power -1 index1) rest))))
     la
     ;; If the second index is zero, skip over this.
     (cond ((zerp (setq index2 (caddr index)))
	    (go la2)))
     ;; Wrong too.  See comment above about bessel_j(-n,x).
     #+nil
     (cond ((eq (checksigntm (simplifya (inv (setq index2
						   (caddr
						    index)))
					nil))
		'$negative)
	    ;; FIXME: This does the same for index2 as for index1
	    ;; above.  Why?
	    (setq index2
		  (mul -1 index2)
		  rest
		  (mul* (power -1 index2) rest))))
     la2
     ;; Put the 2 indices in a list and go on.
     (setq index (list index1 index2))
     (go labl)
     lab
     ;; We're here if we have one index, and it's not one of the
     ;; special cases.
     ;;
     ;; FIXME:  Find out what functions trigger this.

     ;; I think this is the one bessel function case with a negative
     ;; index.  At least here we check that the index is an integer.
     ;; We can either leave it here or take it out.  It seems
     ;; bessel_j(-n,x) is converted by the Bessel simplifiers before
     ;; we get here.
     (cond ((and (eq (checksigntm (simplifya (inv index)
					     nil))
		     '$negative)
		 (maxima-integerp index))
	    ;; FIXME: Same as the 2 index thing above, but we need an
	    ;; (numeric) integer too.  Why?
	    (setq index (mul -1 index))
	    (setq rest (mul (power -1 index) rest))))
     labl
     ;; Handle index = 0 or one of the special functions erf,
     ;; kelliptic, E, Jacobi, %d, %s, hstruve, lstruve, %m, Q,
     ;; incomplete gamma, asin, atan.
     (setq argl (f+c arg))
     (setq const (cdras 'c argl)
	   arg (cdras 'f argl))
     ;; See if the arg is f + c, and replace arg with f.
     (cond ((null const)(go labl1)))
     ;; This handles the case of when the const term is actually there.
     (cond ((not (eq (checksigntm (simplifya (power const
						    2)
					     nil))
		     '$zero))
	    ;; I guess prop4 handles the case when square of the
	    ;; constant term is not zero.  Too bad I (rtoy) don't know
	    ;; what prop4 is.
	    ;;
	    ;; FIXME:  Implement prop4.
	    (when *debug-hypgeo*
  	      (format t "~&   : const = ~A~%" const)
	      (format t "~&   : f     = ~A~%" arg))

	    ;; We have to add the return of a noun form.
	    (return
	      (setq *hyp-return-noun-flag* 'prop4-to-be-applied))))
     labl1
     ;; No const term, if we're here.

; We no longer call lty via lt-ltp. This code is no longer needed.
;     (cond ((eq flg 'oney)
;	    ;; Handle bessel_y here.  We're done.
;	    (return (lty rest arg index))))
     
     ;; Try to express the function in terms of hypergeometric
     ;; functions that we can handle.
     (cond ((setq l
		  (d*x^m*%e^a*x ($factor (mul* rest
					       (car (setq
						     l1
						     (ref
						      flg
						      index
						      arg)))))))
	    (when *debug-hypgeo*
	      (format t "~&LT-LTP - labl1:~%")
	      (format t "~&   : l1 = ~A~%" l1)
	      (format t "~&   : l  = ~A~%" l))

	    ;; Convert the special function to a hypgergeometric
	    ;; function.  L1 is the special function converted to the
	    ;; hypergeometric function.  d*x^m*%e^a*x looks for that
	    ;; factor in the expanded form.
	    (return (%$etest l l1))))
     ;; We currently don't know how to handle this yet.
     ;; We add the return of a noun form.
     (return
       (setq *hyp-return-noun-flag* 'other-ca-later))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch function to convert the given function to a hypergeometric
;; function.
;;
;; The first arg is a symbol naming the function; the last is the
;; argument to the function.  The second arg is the index (or list of
;; indices) to the function.  Not used if the function doesn't have
;; any indices
;;
;; The result is a list of 2 elements: The first element is a
;; multiplier; the second, the hypergeometric function itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ref (flg index arg)
  (case flg
    (onej (j1tf index arg))
    (twoj (j2tf (car index) (cadr index) arg))
    (hs (hstf index arg))
    (hl (lstf index arg))
    (s (stf (car index) (cadr index) arg))
    (onerf (erftf arg))
    (onelog (logtf arg))
    (onekelliptic (kelliptictf arg)) ; elliptic_kc
    (onee (etf arg))                 ; elliptic_ec
    (onem (mtf (car index) (cadr index) arg))
    (hyp-onep (ptf (car index) (cadr index) arg))
    (oneq (qtf (car index) (cadr index) arg))
    (gammagreek (gammagreektf index arg))
    (onepjac (pjactf (car index) (cadr index) (caddr index) arg))
    (asin (asintf arg))
    (atan (atantf arg))
    (f
     ;; Transform %f to internal representation FPQ
     (list 1 (ref-fpq (rest (car index)) (rest (cadr index)) arg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRANSFORM FUNCTION IN TERMS OF HYPERGEOMETRIC FUNCTION
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a hypergeometric form that we recognize.  The function is
;; %f[n,m](p; q; arg).  We represent this as a list of the form
;; (fpq (<length p> <length q>) <p> <q> <arg>)

(defun ref-fpq (p q arg)
  (list 'fpq (list (length p) (length q))
	p q arg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Whittaker M function in terms of hypergeometric function
;;
;; A&S 13.1.32:
;;
;; M[k,u](z) = exp(-z/2)*z^(1/2+u)*M(1/2+u-k,1+2*u,z)

(defun mtf (i1 i2 arg)
  (list (mul (power arg (add i2 (1//2)))
	     (power '$%e (div arg -2)))
	(ref-fpq (list (add* (1//2) i2 (mul -1 i1)))
		 (list (add* i2 i2 1))
		 arg)))

;; Jacobi P in terms of hypergeometric function
;;
;; A&S 15.4.6:
;;
;; F(-n,a+1+b+n; a+1; x) = n!/poch(a+1,n)*jacobi_p(n,a,b,1-2*x)
;;
;; jacobi_p(n,a,b,x) = poch(a+1,n)/n!*F(-n,a+1+b+n; a+1; (1-x)/2)
;;                   = gamma(a+n+1)/gamma(a+1)/n!*F(-n,a+1+b+n; a+1; (1-x)/2)
;;
;; We have a problem:
;; We transform the argument x -> (1-x)/2. But an argument with a constant
;; part is not integrable with the implemented algorithm for the hypergeometric
;; function. It might be possible to get a result for an argument y=1-2*x 
;; with x=t^-2. But for this case the routine lt-ltp fails. The routine
;; recognize a constant term in the argument, but does not take into account
;; that the constant term might vanish, when we transform to a hypergeometric 
;; function.
;; Because of this the Laplace transform for the following functions does not
;; work too: Legendre P, Chebyshev T, Chebyshev U, and Gegenbauer.

(defun pjactf (n a b x)
  (list (mul* (simplify (list '(%gamma) (add n a 1)))
	      (inv (simplify (list '(%gamma) (add a 1))))
	      (inv (factorial n)))
	(ref-fpq (list (mul -1 n) (add* n a b 1))
		 (list (add a 1))
		 (sub (1//2) (div x 2)))))

;; ArcSin in terms of hypergeometric function
;;
;; A&S 15.1.6:
;;
;; F(1/2,1/2; 3/2; z^2) = asin(z)/z
;;
;; asin(z) = z*F(1/2,1/2; 3/2; z^2)

(defun asintf (arg)
  (let ((inv2 (1//2)))
    (list arg
	  (ref-fpq (list inv2 inv2)
		   (list (div 3 2))
		   (mul arg arg)))))

;; ArcTan in terms of hypergeometric function
;;
;; A&S 15.1.5
;;
;; F(1/2,1; 3/2; -z^2) = atan(z)/z
;;
;; atan(z) = z*F(1/2,1; 3/2; -z^2)

(defun atantf (arg)
  (list arg
	(ref-fpq (list (inv 2) 1)
		 (list (div 3 2))
		 (mul* -1 arg arg))))

;; Associated Legendre function P in terms of hypergeometric function
;;
;; A&S 8.1.2
;;
;; assoc_legendre_p(v,u,z) = ((z+1)/(z-2))^(u/2)/gamma(1-u)*F(-v,v+1;1-u,(1-z)/2)
;;
;; FIXME: What about the branch cut?  8.1.2 is for z not on the real
;; line with -1 < z < 1.

(defun ptf (n m z)
  (list (mul (inv (simplify (list '(%gamma) (sub 1 m))))
	     (power (div (add z 1)
			 (sub z 1))
		    (div m 2)))
	(ref-fpq (list (mul -1 n) (add n 1))
		 (list (sub 1 m))
		 (sub (1//2) (div z 2)))))

;; Associated Legendre function Q in terms of hypergeometric function
;;
;; A&S 8.1.3:
;;
;; assoc_legendre_q(v,u,z)
;;    = exp(%i*u*%pi)*2^(-v-1)*sqrt(%pi) *
;;       gamma(v+u+1)/gamma(v+3/2)*z^(-v-u-1)*(z^2-1)^(u/2) *
;;        F(1+v/2+u/2, 1/2+v/2+u/2; v+3/2; 1/z^2)
;;
;; FIXME:  What about the branch cut?

(defun qtf (n m z)
  (list (mul* (power '$%e (mul* m '$%pi '$%i))
	      (power '$%pi (1//2))
	      (simplify (list '(%gamma) (add* m n 1)))
	      (power 2 (sub -1 n))
	      (inv (simplify (list '(%gamma) (add n (div 3 2)))))
	      (power z (mul -1 (add* m n 1)))
	      (power (sub (mul z z) 1)
		     (div m 2)))
	(ref-fpq (list (div (add* m n 1) 2)
		       (div (add* m n 2) 2))
		 (list (add n (div 3 2)))
		 (power z -2))))

;; Gammagreek in terms of hypergeometric function
;;
;; A&S 13.6.10:
;;
;; M(a,a+1,-x) = a*x^(-a)*gammagreek(a,x)
;;
;; gammagreek(a,x) = x^a/a*M(a,a+1,-x)

(defun gammagreektf (a x)
  (list (mul (inv a) (power x a))
	(ref-fpq (list a)
		 (list (add a 1))
		 (mul -1 x))))

;; Complete elliptic K in terms of hypergeometric function
;;
;; A&S 17.3.9
;;
;; K(k) = %pi/2*2F1(1/2,1/2; 1; k^2)

(defun kelliptictf (k)
  (let ((inv2 (1//2)))
    (list (mul inv2 '$%pi)
	  (ref-fpq (list inv2 inv2)
		   (list 1)
		   (mul k k)))))

;; Complete elliptic E in terms of hypergeometric function
;;
;; A&S 17.3.10
;;
;; E(k) = %pi/2*2F1(-1/2,1/2;1;k^2)

(defun etf (k)
  (let ((inv2 (1//2)))
    (list (mul inv2 '$%pi)
	  (list 'fpq
		(list  2 1)
		(list (mul -1 inv2) inv2)
		(list 1)
		(mul k k)))))

;; erf in terms of hypgeometric function.
;;
;; A&S 7.1.21 gives
;;
;; erf(z) = 2*z/sqrt(%pi)*M(1/2,3/2,-z^2) 
;;        = 2*z/sqrt(%pi)*exp(-z^2)*M(1,3/2,z^2)

(defun erftf (arg)
  (list (mul* 2 arg (power '$%pi (inv -2)))
	(ref-fpq (list (1//2))
		 (list (div 3 2))
		 (mul* -1 arg arg))))

;; log in terms of hypergeometric function
;;
;; We know from A&S 15.1.3 that
;;
;; F(1,1;2;z) = -log(1-z)/z.
;;
;; So log(z) = (z-1)*F(1,1;2;1-z)

(defun logtf (arg)
  ;; This seems wrong.   Why is the multipler 1 instead of (z-1)?
  (list #+nil 1
	(sub arg 1)
	(ref-fpq (list 1 1)
		 (list 2)
		 (sub 1 arg))))

;; Bessel J function expressed as a hypergeometric function.
;;
;; A&S 9.1.10:
;;                         inf
;; bessel_j(v,z) = (z/2)^v*sum (-z^2/4)^k/k!/gamma(v+k+1)
;;                         k=0
;;
;;               = (z/2)^v/gamma(v+1)*sum 1/poch(v+1,k)*(-z^2/4)^k/k!
;;
;;               = (z/2)^v/gamma(v+1) * 0F1(; v+1; -z^2/4)

(defun j1tf (v z)
  (list (mul* (inv (power 2 v))
              (power z v)
              (inv (simplify (list '(%gamma) (add v 1)))))
        (ref-fpq nil
                 (list (add v 1))
                 (mul (inv -4)(power z 2)))))

;; Product of 2 Bessel J functions in terms of hypergeometric function
;;
;; See Y. L. Luke, formula 39, page 216:
;;
;; bessel_j(u,z)*bessel_j(v,z)
;;    = (z/2)^(u+v)/gamma(u+1)/gamma(v+1) *
;;        2F3((u+v+1)/2, (u+v+2)/2; u+1, v+1, u+v+1; -z^2)

(defun j2tf (n m arg)
  (list (mul* (inv (simplify (list '(%gamma) (add n 1))))
	      (inv (simplify (list '(%gamma) (add m 1))))
	      (inv (power 2 (add n m)))
	      (power arg (add n m)))
	(ref-fpq (list (add* (1//2) (div n 2) (div m 2))
		       (add* 1 (div n 2) (div m 2)))
		 (list (add 1 n) (add 1 m) (add* 1 n m))
		 (mul -1 (power arg 2)))))

;; Struve H function in terms of hypergeometric function.
;;
;; A&S 12.1.2 gives the following series for the Struve H function:
;;
;;                       inf
;; H[v](z) = (z/2)^(v+1)*sum (-1)^k*(z/2)^(2*k)/gamma(k+3/2)/gamma(k+v+3/2)
;;                       k=0
;;
;; We can write this in the form
;;
;; H[v](z) = 2/sqrt(%pi)*(z/2)^(v+1)/gamma(v+3/2)
;;
;;             inf
;;           * sum n!/poch(3/2,n)/poch(v+3/2,n)*(-z^2/4)^n/n!
;;             n=0
;;
;;         = 2/sqrt(%pi)*(z/2)^(v+1)/gamma(v+3/2) * 1F2(1;3/2,v+3/2;(-z^2/4))
;;
;; See also A&S 12.1.21.

(defun hstf (v z)
  (let ((d32 (div 3 2)))
    (list (mul* (power (div z 2)(add v 1))
                (inv (simplify (list '(%gamma) d32)))
                (inv (simplify (list '(%gamma) (add v d32)))))
          (ref-fpq (list 1)
                   (list d32 (add v d32))
                   (mul* (inv -4) z z)))))

;; Struve L function in terms of hypergeometric function
;;
;; A&S 12.2.1:
;;
;; L[v](z) = -%i*exp(-v*%i*%pi/2)*H[v](%i*z)
;;
;; This function computes exactly this way.  (But why is %i written as
;; exp(%i*%pi/2) instead of just %i)
;;
;; A&S 12.2.1 gives the series expansion as
;;
;;                       inf
;; L[v](z) = (z/2)^(v+1)*sum (z/2)^(2*k)/gamma(k+3/2)/gamma(k+v+3/2)
;;                       k=0
;;
;; It's quite easy to derive
;;
;; L[v](z) = 2/sqrt(%pi)*(z/2)^(v+1)/gamma(v+3/2) * 1F2(1;3/2,v+3/2;(z^2/4))

#+nil
(defun lstf (v z)
  (prog (hst)
     (return (list (mul* (power '$%e
                                (mul* (div (add v 1)
                                           -2)
                                      '$%pi
                                      '$%i))
                         (car (setq hst
                                    (hstf v
                                          (mul* z
                                                (power '$%e
                                                       (mul*
                                                        (1//2)
                                                        '$%i
                                                        '$%pi)))))))
                   (cadr hst)))))

(defun lstf (v z)
  (let ((d32 (div 3 2)))
    (list (mul* (power (div z 2) (add v 1))
                (inv (simplify (list '(%gamma) d32)))
                (inv (simplify (list '(%gamma) (add v d32)))))
          (ref-fpq (list 1)
                   (list d32 (add v d32))
                   (mul* (inv 4) z z)))))

;; Lommel s function in terms of hypergeometric function
;;
;; See Y. L. Luke, p 217, formula 1
;;
;; s(u,v,z) = z^(u+1)/(u-v+1)/(u+v+1)*1F2(1; (u-v+3)/2, (u+v+3)/2; -z^2/4)

(defun stf (m n z)
  (list (mul* (power z (add m 1))
              (inv (sub (add m 1) n))
              (inv (add m n 1)))
        (ref-fpq (list 1)
                 (list (div (sub (add m 3) n) 2)
                       (div (add* m n 3) 2))
                 (mul* (inv -4) z z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 3: Laplace transform of a hypergeometric function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %$etest (l l1)
  (prog(a q)
     (setq q (cdras 'q l))
     (cond ((equal q 1)(setq a 0)(go loop)))
     (setq a (cdras 'a l))
     loop
     (return (substl (sub *par* a)
                     *par*
                     (execf19 l (cadr l1))))))

(defun execf19 (l1 l2)
  (prog(ans)
     (setq ans (execargmatch (car (cddddr l2))))
     (cond ((eq (car ans) 'dionimo)
            (return (dionarghyp l1 l2 (cadr ans)))))
     ;; We specialized the pattern for the argument. Now the test fails
     ;; correctly and we have to add the return of a noun form.
     (return
       (setq *hyp-return-noun-flag* 'next-for-other-args))))

;; Executive for recognizing the sort of argument to the
;; hypergeometric function.  We look to see if the arg is of the form
;; a*x^m + c.  Return a list of 'dionimo (what does that mean?) and
;; the match.

(defun execargmatch (arg)
  (prog(l1)
     (cond ((setq l1 (a*x^m+c ($factor arg)))
            (return (list 'dionimo l1))))
     (cond ((setq l1 (a*x^m+c ($expand arg)))
            (return (list 'dionimo l1))))
     ;; The return value has to be a list.
     (return (list 'other-case-args-to-follow))))

;; We have hypergeometric function whose arg looks like a*x^m+c.  L1
;; matches the d*x^m... part, L2 is the hypergeometric function and
;; arg is the match for a*x^m+c.

(defun dionarghyp (l1 l2 arg)
  (prog(a m c)
     (setq a
           (cdras 'a arg)
           m
           (cdras 'm arg)
           c
           (cdras 'c arg))
     (cond ((and (maxima-integerp m)(zerp c))
            (return (f19cond a m l1 l2))))
     (return (setq *hyp-return-noun-flag* 'prop4-and-other-cases-to-follow))))

(defun f19cond (a m l1 l2)
  (prog(p q s d)
     (setq p (caadr l2)
           q (cadadr l2)
           s (cdras 'm l1)
           d (cdras 'd l1)
           l1 (caddr l2)
           l2 (cadddr l2))
     ;; At this point, we have the function d*x^s*%f[p,q](l1, l2, (a*t)^m).
     ;; Check to see if Formula 19, p 220 applies.
     (cond ((and (not (eq (checksigntm (sub (add* p
                                                  m
                                                  -1)
                                            q))
                          '$positive))
                 (eq (checksigntm (add s 1))
                     '$positive))
            (return (mul d
                         (f19p220-simp (add s 1)
                                       l1
                                       l2
                                       a
                                       m)))))
     (return (setq *hyp-return-noun-flag*
                   'failed-on-f19cond-multiply-the-other-cases-with-d))))

;; Table of Laplace transforms, p 220, formula 19:
;;
;; If m + k <= n + 1, and Re(s) > 0, the Laplace transform of
;;
;;    t^(s-1)*%f[m,n]([a1,...,am],[p1,...,pn],(c*t)^k)
;; is
;;
;;    gamma(s)/p^s*%f[m+k,n]([a1,...,am,s/k,(s+1)/k,...,(s+k-1)/k],[p1,...,pm],(k*c/p)^k)
;;
;; with Re(p) > 0 if m + k <= n, Re(p+k*c*exp(2*%pi*%i*r/k)) > 0 for r
;; = 0, 1,...,k-1, if m + k = n + 1.
;;
;; The args below are s, [a's], [p's], c^k, k.

(defun f19p220-simp (s l1 l2 cf k)
  (mul* (simplify (list '(%gamma) s))
        (inv (power *par* s))
        (hgfsimp-exec (append l1 (addarglist s k))
                      l2
                      (mul* cf
                            (power k k)
                            (power (inv *par*) k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Return a list of s/k, (s+1)/k, ..., (s+|k|-1)/k
(defun addarglist (s k)
  (let ((abs-k (abs k))
        (res '()))
    (dotimes (n abs-k)
      (push (div (add s n) k) res))
    (nreverse res)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Pattern for the Laplace transform of the hypergeometric function

;; Match d*x^m*%e^(a*x).  If we match, Q is the e^(a*x) part, A is a,
;; M is M, and D is d.
(defun d*x^m*%e^a*x (exp)
  (m2 exp
      '((mtimes)
	((coefftt)(d freevarpar))
	((mexpt) (x varp) (m freevarpar))
	((mexpt)
	 (q expor1p)
	 ((mtimes)((coefftt)(a freevarpar)) (x varp))))
      nil))

;; Match f(x)+c
(defun f+c (exp)
  (m2 exp
      '((mplus)((coeffpt)(f hasvar))((coeffpp)(c freevar)))
      nil))

;; Match a*x^m+c.
;; The pattern was too general. We match also a*t^2+b*t. But that's not correct.
(defun a*x^m+c (exp)
  (m2 exp
      '((mplus)
	((coefft) ; more special (not coeffpt)
	 (a freevar)
	 ((mexpt) (x varp) (m freevar0)))
	((coeffpp) (c freevar)))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 4: SPECIAL HANDLING OF Bessel Y for an integer order
;;;
;;; This is called for one Bessel Y function, when the order is an integer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lty (rest arg index)
  (prog(l)
     (cond ((setq l (d*x^m*%e^a*x rest))
            (return (execfy l arg index))))
     (return (setq *hyp-return-noun-flag* 'fail-in-lty))))

(defun execfy (l arg index)
  (prog(ans)
     (setq ans (execargmatch arg))
     (cond ((eq (car ans) 'dionimo)
            (return (dionarghyp-y l index (cadr ans)))))
     (return (setq *hyp-return-noun-flag* 'fail-in-execfy))))

(defun dionarghyp-y (l index arg)
  (prog (a m c)
     (setq a (cdras 'a arg)
           m (cdras 'm arg)
           c (cdras 'c arg))
     (cond ((and (zerp c) (equal m 1.))
            (let ((ans (f2p105v2cond a l index)))
              (unless (symbolp ans)
                (return ans)))))
     (cond ((and (zerp c) (equal m (inv 2.)))
            (let ((ans (f50cond a l index)))
              (unless (symbolp ans)
                (return ans)))))
     (return (setq *hyp-return-noun-flag* 'fail-in-dionarghyp-y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 4.1: Laplace transform of t^n*bessel_y(v,a*t)
;;;                v is an integer and n>=v
;;;
;;; Table of Integral Transforms
;;;
;;; Volume 2, p 105, formula 2 is a formula for the Y-transform of
;;;
;;;    f(x) = x^(u-3/2)*exp(-a*x)
;;;
;;; where the Y-transform is defined by
;;;
;;;    integrate(f(x)*bessel_y(v,x*y)*sqrt(x*y), x, 0, inf)
;;;
;;; which is
;;;
;;;    -2/%pi*gamma(u+v)*sqrt(y)*(y^2+a^2)^(-u/2)
;;;          *assoc_legendre_q(u-1,-v,a/sqrt(y^2+a^2))
;;;
;;; with a > 0, Re u > |Re v|.
;;;
;;; In particular, with a slight change of notation, we have
;;;
;;;    integrate(x^(u-1)*exp(-p*x)*bessel_y(v,a*x)*sqrt(a), x, 0, inf)
;:;
;;; which is the Laplace transform of x^(u-1/2)*bessel_y(v,x).
;;;
;;; Thus, the Laplace transform is
;;;
;;;    -2/%pi*gamma(u+v)*sqrt(a)*(a^2+p^2)^(-u/2)
;;;          *assoc_legendre_q(u-1,-v,p/sqrt(a^2+p^2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun f2p105v2cond (a l index)
  (prog (d m)
     (setq d (cdras 'd l) ; contains constant part of integrand
           m (cdras 'm l))
     (setq m (add m 1.))
     (cond ((eq (checksigntm ($realpart (sub m index)))
                '$positive)
            (return (mul d (f2p105v2cond-simp m index a)))))
     (return (setq *hyp-return-noun-flag* 'fail-in-f2p105v2cond))))

(defun f2p105v2cond-simp (m v a)
  (mul -2
       (power '$%pi -1)
       (simplify (list '(%gamma) (add m v)))
       (power (add (mul a a) (mul *par* *par*))
              (mul -1 (inv 2) m))
       ;; Call Associated Legendre Q function, which simplifies accordingly.
       ;; We have to do a Maxima function call, because $assoc_legendre_q is
       ;; not in Maxima core and has to be autoloaded.
       (mfuncall '$assoc_legendre_q
                 (sub m 1)
                 (mul -1 v)
                 (mul *par*
                      (power (add (mul a a) (mul *par* *par*))
                             (inv -2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algorithm 4.2: Laplace transform of t^n*bessel_y(v, a*sqrt(t))
;;;
;;; Table of Integral Transforms
;;;
;;; p. 188, formula 50:
;;;
;;; t^(u-1/2)*bessel_y(2*v,2*sqrt(a)*sqrt(t))
;;;    -> a^(-1/2)*p^(-u)*exp(-a/2/p)
;;;       * [tan((u-v)*%pi)*gamma(u+v+1/2)/gamma(2*v+1)*M[u,v](a/p)
;;;          -sec((u-v)*%pi)*W[u,v](a/p)]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun f50cond (a l v)
  (prog (d m)
     (setq d (cdras 'd l)
           m (cdras 'm l)
           m (add m (inv 2.))
           v (div v 2.))
     (cond
       ((and (eq (checksigntm ($realpart (add m v (inv 2.))))
                 '$positive)
             (eq (checksigntm ($realpart (sub (add m (inv 2.))
                                              v)))
                 '$positive)
             (not (maxima-integerp (mul (sub (add m m) (add v v 1.))
                                        (inv 2.)))))
        (setq a (mul a a (inv 4.)))
        (return (f50p188-simp d m v a))))
     (return (setq *hyp-return-noun-flag* 'fail-in-f50cond))))

(defun f50p188-simp (d u v a)
  (mul d
       (power a (inv -2))
       (power *par* (mul -1 u))
       (power '$%e (div a (mul -2 *par*)))
       (sub (mul (simplify (list '(%tan) (mul '$%pi (sub u v))))
                 (simplify (list '(%gamma) (add u v (inv 2))))
                 (inv (simplify (list '(%gamma) (add v v 1))))
                 (mwhit (div a *par*) u v))
            (mul (simplify (list '(%sec) (mul '$%pi (sub u v))))
                 (wwhit (div a *par*) u v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This doesn't seem to be used anywhere.
;;
;; A&S 8.1.2:
;;
;; assoc_legendre_p(v,m,z)
;;    = 1/gamma(1-m)*((z+1)/(z-1))^(m/2)*F(-v,v+1;1-m;(1-z)/2)
;;
;; for |1-z|<2
;;
;; Note: The args here are reversed from our definition of
;; assoc_legendre_p!

#+nil
(defun leg1fsimp (m v z)
  (mul (inv (simplify (list '(%gamma) (sub 1. m))))
       (power (div (add z 1.) (sub z 1.)) (div m 2.))
       (hgfsimp-exec (list (mul -1. v) (add v 1.))
                     (list (sub 1. m))
                     (sub (inv 2.) (div z 2.)))))

;; A&S 8.1.3:
;;
;; assoc_legendre_q(v,m,z)
;;    = exp(%i*%pi*m)*2^(-v-1)*sqrt(%pi)*gamma(v+m+1)/gamma(v+3/2)*z^(-v-u-1)
;;        *(z^2-1)^(m/2)*F(1+v/2+u/2,1/2+v/2+u/2;v+3/2;1/z^2)
;;
;; for |z| > 1.
;;
;; But note that we are called with z = p/sqrt(p^2+a^2) so |z| < 1 for
;; all real p and a.  So I (rtoy) don't think this is the right thing
;; to use.
;;
;; So, for now, just return the Legendre Q function and hope that
;; someone else can simplify it.

;; This code is no longer called. We call the Maxima function
;; $assoc_legendre_q in the routine f2p105v2cond-simp.

#+nil
(defun leg2fsimp (m v z)
  (cond (t
         (legen m v z '$q))
        (nil
         (mul (power '$%e (mul m '$%pi '$%i))
              (power '$%pi (inv 2.))
              (simplify (list '(%gamma) (add m v 1.)))
              (inv (power 2. (add v 1.)))
              (inv (simplify (list '(%gamma) (add v (div 3. 2.)))))
              (power z (sub -1. (add m v)))
              (power (sub (mul z z) 1.) (mul (inv 2.) m))
              (hgfsimp-exec (list (div (add m v 1.) 2.)
                                  (div (add m v 2.) 2.))
                            (list (add v (mul 3. (inv 2.))))
                            (inv (mul z z)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
