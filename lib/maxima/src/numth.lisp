;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;;   *****************************************************************
;;;   ***** NUMTH ******* VARIOUS NUMBER THEORY FUNCTIONS *************
;;;   *****************************************************************

(macsyma-module numth)

(declare-top (special $intfaclim))

(load-macsyma-macros rzmac)

;;; Sum of divisors and Totient functions

(defmfun $divsum (n &optional (k 1))
  (let (($intfaclim nil))
    (if (and (integerp k) (integerp n))
	(let ((n (abs n)))
	  (cond ((= n 1) 1)
		((zerop n) 1)
		((zerop k)
		 (do ((l (cfactorw n) (cddr l))
		      (a 1 (* a (1+ (cadr l)))))
		     ((null l) a)))
		((minusp k)
		 `((rat) ,(divsum (cfactorw n) (- k)) ,(expt n (- k))))
		(t
		 (divsum (cfactorw n) k))))
	(list '($divsum) n k))))

(defun divsum (l k)
  (do ((l l (cddr l))
       (ans 1))
      ((null l) ans)
    (unless (eql (car l) 1)
      (let ((temp (expt (car l) k)))
	(setq ans (* ans
		     (truncate (1- (expt temp (1+ (cadr l))))
			       (1- temp))))))))

(defmfun $totient (n)
  (cond ((integerp n)
	 (setq n (abs n))
	 (cond ((< n 1) 0)
	       ((equal n 1) 1)
	       (t (do ((factors (let ($intfaclim) (cfactorw n))
				(cddr factors))
		       (total 1 (* total (1- (car factors))
				   (expt (car factors) (1- (cadr factors))))))
		      ((null factors) total)))))
	(t (list '($totient) n))))


;;; JACOBI symbol and Gaussian factoring

(declare-top (special modulus $intfaclim))

(defvar *incl* (let ((l (list 2 4))) (nconc l l)))

(defun imodp (p)
  (cond ((not (= (rem p 4) 1)) nil)
	((= (rem p 8) 5) (imodp1 2 p))
	((= (rem p 24) 17) (imodp1 3 p)) ;p=2(mod 3)
	(t (do ((i 5 (+ i (car j)))	;p=1(mod 24)
		(j *incl* (cdr j)))
	       ((= (jacobi i p) -1) (imodp1 i p))))))

(defun imodp1 (i modulus)
  (abs (cexpt i (ash (1- modulus) -2) )))

(defun psumsq (p)
  (let ((x (imodp p)))
    (cond ((equal p 2) (list 1 1))
	  ((null x) nil)
	  (t (psumsq1 p x)))))

(defun psumsq1 (p x)
  (do ((sp ($isqrt p))
       (r1 p r2)
       (r2 x (rem r1 r2)))
      ((not (> r1 sp)) (list r1 r2))))

(defun gctimes (a b c d)
  (list (- (* a c) (* b d))
	(+ (* a d) (* b c))))

(defmfun $gcfactor (n)
  (let ((n (cdr ($totaldisrep ($bothcoef ($rat n '$%i) '$%i)))))
    (if (not (and (integerp (car n)) (integerp (cadr n))))
	(gcdisp (nreverse n))
	(do ((factors (gcfactor (cadr n) (car n)) (cddr factors))
	     (res nil))
	    ((null factors)
	     (cond ((null res) 1)
		   ((null (cdr res)) (car res))
		   (t (cons '(mtimes simp) (nreverse res)))))
	  (let ((term (car factors))
		(exp (cadr factors)))
	    (push (if (= exp 1)
		      (gcdisp term)
		      (pow (gcdisp term) exp))
		  res))))))

(defun gcdisp (term)
  (cond ((atom term) term)
	((let ((rp (car term))
	       (ip (cadr term)))
	   (setq ip (if (equal ip 1) '$%i (list '(mtimes) ip '$%i)))
	   (if (equal rp 0)
	       ip
	       (list '(mplus) rp ip))))))

(defun gcfactor (a b &aux tem)
  (prog (gl cd dc econt p e1 e2 ans plis nl $intfaclim )
     (setq e1 0
	   e2 0
	   econt 0
	   gl (gcd a b)
	   a (quotient a gl)
	   b (quotient b gl)
	   nl (cfactorw (+ (* a a) (* b b)))
	   gl (cfactorw gl))
     (and (equal 1 (car gl)) (setq gl nil))
     (and (equal 1 (car nl)) (setq nl nil))
     loop
     (cond ((null gl)
	    (cond ((null nl) (go ret))
		  ((setq p (car nl)))))
	   ((null nl) (setq p (car gl)))
	   (t (setq p (max (car gl) (car nl)))))
     (setq cd (psumsq p))
     (cond ((null cd)
	    (setq plis (cons p (cons (cadr gl) plis)))
	    (setq gl (cddr gl)) (go loop))
	   ((equal p (car nl))
	    (cond ((zerop (rem (setq tem (+ (* a (car cd)) ;gcremainder
					    (* b (cadr cd))))
			       p))		;remainder(real((a+bi)cd~),p)
					;z~ is complex conjugate
		   (setq e1 (cadr nl)) (setq dc cd))
		  (t (setq e2 (cadr nl))
		     (setq dc (reverse cd))))
	    (setq dc (gcexpt dc (cadr nl)) ;
		  dc (gctimes a b (car dc) (- (cadr dc)))
		  a (quotient (car dc) p)
		  b (quotient (cadr dc) p)
		  nl (cddr nl))))
     (cond ((equal p (car gl))
	    (setq econt (+ econt (cadr gl)))
	    (cond ((equal p 2)
		   (setq e1 (+ e1 (* 2 (cadr gl)))))
		  (t (setq e1 (+ e1 (cadr gl))
			   e2 (+ e2 (cadr gl)))))
	    (setq gl (cddr gl))))
     (and (not (zerop e1))
	  (setq ans (cons cd (cons e1 ans)))
	  (setq e1 0))
     (and (not (zerop e2))
	  (setq ans (cons (reverse cd) (cons e2 ans)))
	  (setq e2 0))
     (go loop)
     ret    (setq cd (gcexpt (list 0 -1)
			     (rem econt 4)))
     (setq a (gctimes a b (car cd) (cadr cd)))
     ;;a hasn't been divided by p yet..
     (setq a (mapcar 'signum a))
     #+cl (assert (or (zerop (car a))(zerop (second a))))
     (cond ((or (equal (car a) -1) (equal (cadr a) -1))
	    (setq plis (cons -1 (cons 1 plis)))))
     (cond ((equal (car a) 0)
	    (setq ans (cons '(0 1) (cons 1 ans)))))
     (setq ans (nconc plis ans))
     (return ans)))

(defun multiply-gcfactors (lis)
  (loop for (term exp) on (cddr lis) by #'cddr
	 with answ = (cond ((numberp (car lis))(list (pexpt (car lis) (second lis)) 0))
			   (t(gcexpt (car lis) (second lis))))
	 when (numberp term)
	 do (setq answ (list (* (first answ) term) (* (second answ) term)))
	 (show answ)
	 else
	 do (setq answ (apply 'gctimes (append answ (gcexpt term exp))))
	 finally (return answ)))

(defun gcexpt (a n)
  (cond ((zerop n) '(1 0))
	((equal n 1) a)
	(t (gctime1 a (gcexpt a (1- n))))))

(defun gctime1 (a b)
  (gctimes (car a) (cadr a) (car b) (cadr b)))
