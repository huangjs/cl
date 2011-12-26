;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module simp)

(declare-top (special rulesw *inv* substp
		      limitp plusflag
		      prods negprods sums negsums
		      $scalarmatrixp nounl
		      $keepfloat $ratprint
		      $demoivre
		      bigfloatzero bigfloatone $assumescalar
		      opers-list *opers-list $dontfactor *n
		      *out *in varlist genvar $factorflag radcanp))

;; General purpose simplification and conversion switches.

(defmvar $float nil
  "Causes non-integral rational numbers to be converted to
	 floating point."
  evflag
  see-also $numer)

(defmvar $negdistrib t
  "Causes negations to be distributed over sums, e.g. -(A+B) is
	 simplified to -A-B.")

(defmvar $numer nil
  "Causes SOME mathematical functions (including exponentiation)
	 with numerical arguments to be evaluated in floating point.
	 It causes variables in an expression which have been given
	 NUMERVALs to be replaced by their values.  It also turns
	 on the FLOAT switch."
  see-also ($numerval $float))

(defmvar $simp t "Enables simplification.")

(defmvar $sumexpand nil
  "If TRUE, products of sums and exponentiated sums go into nested
	 sums.")

(defmvar $numer_pbranch nil)

;; Switches dealing with matrices and non-commutative multiplication.

(defmvar $doscmxplus nil
  "Causes SCALAR + MATRIX to return a matrix answer.  This switch
	 is not subsumed under DOALLMXOPS.")

(defmvar $domxexpt t
  "Causes SCALAR^MATRIX([1,2],[3,4]) to return
	 MATRIX([SCALAR,SCALAR^2],[SCALAR^3,SCALAR^4]).  In general, this
	 transformation affects exponentiations where the *print-base* is a scalar
	 and the power is a matrix or list.")

(defmvar $domxplus nil)

(defmvar $domxtimes nil)

(defmvar $mx0simp t)

;; Switches dealing with expansion.

(defmvar $expop 0
  "The largest positive exponent which will be automatically
	 expanded.  (X+1)^3 will be automatically expanded if
	 EXPOP is greater than or equal to 3."
  fixnum
  see-also ($expon $maxposex $expand))

(defmvar $expon 0
  "The largest negative exponent which will be automatically
	 expanded.  (X+1)^(-3) will be automatically expanded if
	 EXPON is greater than or equal to 3."
  fixnum
  see-also ($expop $maxnegex $expand))

(defmvar $maxposex 1000.
  "The largest positive exponent which will be expanded by
	 the EXPAND command."
  fixnum
  see-also ($maxnegex $expop $expand))

(defmvar $maxnegex 1000.
  "The largest negative exponent which will be expanded by
	 the EXPAND command."
  fixnum
  see-also ($maxposex $expon $expand))

;; Lisp level variables

(defmvar dosimp nil
  "Causes SIMP flags to be ignored.  $EXPAND works by binding
	 $EXPOP to $MAXPOSEX, $EXPON to $MAXNEGEX, and DOSIMP to T.")

(defmvar errorsw nil
  "Causes a throw to the tag ERRORSW when certain errors occur
	 rather than the printing of a message.  Kludgy MAXIMA-SUBSTITUTE for
	 MAXIMA-ERROR signalling.")

(defmvar derivsimp t "Hack in `simpderiv' for `rwg'")

(defmvar $rootsepsilon 1e-7)
(defmvar $grindswitch nil)
(defmvar $algepsilon 100000000)
(defmvar $true t)
(defmvar $false nil)
(defmvar $on t)
(defmvar $off nil)
(defmvar $logabs nil)
(defmvar $limitdomain '$complex)
(defmvar $listarith t)
(defmvar $lognumer nil)
(defmvar $domain '$real)
(defmvar $m1pbranch nil)
(defmvar $%e_to_numlog nil)
(defmvar $%emode t)
(defmvar $lognegint nil)
(defmvar $ratsimpexpons nil)
(defmvar $logexpand t)
(defmvar $radexpand t)
(defmvar $subnumsimp nil)
(defmvar $logsimp t)
(defmvar $distribute_over t) ; If T, functions are distributed over bags.

(defvar rischp nil)
(defvar rp-polylogp nil)
(defvar wflag nil)
(defvar expandp nil)
(defvar timesinp nil)
(defvar %e-val (mget '$%e '$numer))
(defvar %pi-val (mget '$%pi '$numer))
(defvar derivflag nil)
(defvar exptrlsw nil)
(defvar expandflag nil)
(defvar *zexptsimp? nil)
(defvar *const* 0)

(defprop mnctimes t associative)
(defprop lambda t lisp-no-simp)

;; Local functions should not be simplified.   Various
;; lisps use various names for the list structure defining
;; these:
(eval-when
    #+gcl (load)
    #-gcl (:load-toplevel)
  (eval '(let* ((x 1)
		(z #'(lambda () 3)))
	  (dolist (y (list x z))
	    (and (consp y)
		 (symbolp (car y))
		 (setf (get (car y) 'lisp-no-simp) t))))))


(dolist (x '(mplus mtimes mnctimes mexpt mncexpt %sum))
  (setf (get x 'msimpind) (cons x '(simp))))

;; operators properties

(mapc #'(lambda (x) (setf (get (first x) 'operators) (second x)))
      '((mplus simplus) (mtimes simptimes) (mncexpt simpncexpt)
	(mminus simpmin) (%gamma simpgamma) (mfactorial simpfact)
	(mnctimes simpnct) (mquotient simpquot) (mexpt simpexpt)
	(%log simpln) 
;        (%sqrt simpsqrt) 
        (%derivative simpderiv)
	(mabs simpabs) (%signum simpsignum)
	(%integrate simpinteg) (%limit simp-limit) 
;        ($exp simpexp)
	(bigfloat simpbigfloat) (lambda simplambda) (mdefine simpmdef)
	(mqapply simpmqapply) (%gamma simpgamma) (%erf simperf)
	($beta simpbeta) (%sum simpsum) (%binomial simpbinocoef)
	(%plog simpplog) (%product simpprod) (%genfact simpgfact)
	($atan2 simpatan2) ($matrix simpmatrix) (%matrix simpmatrix)
	($bern simpbern) ($euler simpeuler)))

(defprop $li lisimp specsimp)
(defprop $psi psisimp specsimp)

(defprop $equal t binary)
(defprop $notequal t binary)

(defun $bfloatp (x)
  (and (consp x)
       (consp (car x))
       (eq (caar x) 'bigfloat)))

(defun zerop1 (x)
  (or
   (and (integerp x) (= 0 x))
   (and (floatp x) (= 0.0 x))
   (and ($bfloatp x) (= 0 (second x)))))

(defun onep1 (x)
  (or
   (and (integerp x) (= 1 x))
   (and (floatp x) (= 1.0 x))
   (and ($bfloatp x) (zerop1 (sub x 1)))))

(defmfun mnump (x)
  (or (numberp x)
      (and (not (atom x)) (not (atom (car x)))
	   (member (caar x) '(rat bigfloat)))))

;; EVEN works for any arbitrary lisp object since it does an integer
;; check first.  In other cases, you may want the Lisp EVENP function
;; which only works for integers.

(defmfun even (a) (and (integerp a) (not (oddp a))))

(defmfun ratnump (x) (and (not (atom x)) (eq (caar x) 'rat)))

(defmfun mplusp (x) (and (not (atom x)) (eq (caar x) 'mplus)))

(defmfun mtimesp (x) (and (not (atom x)) (eq (caar x) 'mtimes)))

(defmfun mexptp (x) (and (not (atom x)) (eq (caar x) 'mexpt)))

(defmfun mnctimesp (x) (and (not (atom x)) (eq (caar x) 'mnctimes)))

(defmfun mncexptp (x) (and (not (atom x)) (eq (caar x) 'mncexpt)))

(defmfun mlogp (x) (and (not (atom x)) (eq (caar x) '%log)))

(defmfun mmminusp (x) (and (not (atom x)) (eq (caar x) 'mminus)))

(defmfun mnegp (x) (cond ((numberp x) (minusp x))
			 ((or (ratnump x) ($bfloatp x)) (minusp (cadr x)))))

(defmfun mqapplyp (e) (and (not (atom e)) (eq (caar e) 'mqapply)))

(defmfun ratdisrep (e) (simplifya ($ratdisrep e) nil))

(defmfun sratsimp (e) (simplifya ($ratsimp e) nil))

(defmfun simpcheck (e flag)
  (cond ((specrepp e) (specdisrep e))
        (flag e)
        (t (let (($%enumer $numer))
             ;; Switch $%enumer on, when $numer is TRUE to allow
             ;; simplification of $%e to its numerical value.
             (simplifya e nil)))))

(defmfun mratcheck (e) (if ($ratp e) (ratdisrep e) e))

(defmfun $numberp (e) (or ($ratnump e) ($floatnump e) ($bfloatp e)))

(defmfun $integerp (x)
  (or (integerp x)
      (and ($ratp x)
	   (not (member 'trunc (car x)))
	   (integerp (cadr x))
	   (equal (cddr x) 1))))

;; The call to $INTEGERP in the following two functions checks for a CRE
;; rational number with an integral numerator and a unity denominator.

(defmfun $oddp (x)
  (cond ((integerp x) (oddp x))
	(($integerp x) (oddp (cadr x)))))

(defmfun $evenp (x)
  (cond ((integerp x) (evenp x))
	(($integerp x) (not (oddp (cadr x))))))

(defmfun $floatnump (x)
  (or (floatp x)
      (and ($ratp x) (floatp (cadr x)) (onep1 (cddr x)))))

(defmfun $ratnump (x)
  (or (integerp x)
      (ratnump x)
      (and ($ratp x)
	   (not (member 'trunc (car x)))
	   (integerp (cadr x))
	   (integerp (cddr x)))))

(defmfun $ratp (x)
  (and (not (atom x))
       (consp (car x))
       (eq (caar x) 'mrat)))

(defmfun $taylorp (x)
  (and (not (atom x)) (eq (caar x) 'mrat) (member 'trunc (cdar x) :test #'eq) t))

(defmfun specrepcheck (e) (if (specrepp e) (specdisrep e) e))

;; Note that the following two functions are carefully coupled.

(defmfun specrepp (e) (and (not (atom e)) (member (caar e) '(mrat mpois) :test #'eq)))

(defmfun specdisrep (e)
  (cond ((eq (caar e) 'mrat) (ratdisrep e))
	;;      ((EQ (CAAR E) 'MPOIS) ($OUTOFPOIS E))
	(t ($outofpois e))))

(defmfun $polysign (x) (setq x (cadr (ratf x)))
	 (cond ((equal x 0) 0) ((pminusp x) -1) (t 1)))

;; These check for the correct number of operands within Macsyma expressions,
;; not arguments in a procedure call as the name may imply.

(defmfun oneargcheck (l)
  (if (or (null (cdr l)) (cddr l)) (wna-err (caar l))))

(defmfun twoargcheck (l)
  (if (or (null (cddr l)) (cdddr l)) (wna-err (caar l))))

(defmfun wna-err (op) (merror "Wrong number of arguments to ~:@M" op))

(defmfun improper-arg-err (exp fn)
  (merror "Improper argument to ~:M:~%~M" fn exp))

(defmfun subargcheck (form subsharp argsharp fun)
  (if (or (not (= (length (subfunsubs form)) subsharp))
	  (not (= (length (subfunargs form)) argsharp)))
      (merror "Wrong number of arguments or subscripts to ~:@M" fun)))

;; Constructor and extractor primitives for subscripted functions, e.g.
;; F[1,2](X,Y).  SUBL is (1 2) and ARGL is (X Y).

;; These will be flushed when NOPERS is finished.  They will be macros in
;; NOPERS instead of functions, so we have to be careful that they aren't
;; mapped or applied anyplace.  What we really want is open-codable routines.

(defmfun subfunmakes (fun subl argl)
  `((mqapply simp) ((,fun simp array) . ,subl) . ,argl))

(defmfun subfunmake (fun subl argl)
  `((mqapply) ((,fun simp array) . ,subl) . ,argl))

(defmfun subfunname (exp) (caaadr exp))

(defmfun subfunsubs (exp) (cdadr exp))

(defmfun subfunargs (exp) (cddr exp))

(defmfun $numfactor (x)
  (setq x (specrepcheck x))
  (cond ((mnump x) x)
	((atom x) 1)
	((not (eq (caar x) 'mtimes)) 1)
	((mnump (cadr x)) (cadr x))
	(t 1)))

(defun scalar-or-constant-p (x flag)
  (if flag (not ($nonscalarp x)) ($scalarp x)))

(defmfun $constantp (x)
  (cond ((atom x) (or ($numberp x) (kindp x '$constant)))
	((member (caar x) '(rat bigfloat) :test #'eq) t)
	((specrepp x) ($constantp (specdisrep x)))
	((or (mopp (caar x)) (kindp (caar x) '$constant))
	 (do ((x (cdr x) (cdr x))) ((null x) t)
	   (if (not ($constantp (car x))) (return nil))))))

(defmfun constant (x)
  (cond ((symbolp x) (kindp x '$constant))
	(($subvarp x)
	 (and (kindp (caar x) '$constant)
	      (do ((x (cdr x) (cdr x))) ((null x) t)
		(if (not ($constantp (car x))) (return nil)))))))

(defun maxima-constantp (x) (or (numberp x) (and (symbolp x) (kindp x '$constant))))

(defun consttermp (x) (and ($constantp x) (not ($nonscalarp x))))

(defmfun $scalarp (x) (or (consttermp x) (eq (scalarclass x) '$scalar)))

(defmfun $nonscalarp (x) (eq (scalarclass x) '$nonscalar))

(defun scalarclass (exp) ;  Returns $SCALAR, $NONSCALAR, or NIL (unknown).
  (cond ((atom exp)
	 (cond ((or (mget exp '$nonscalar) (arrayp exp) ($member exp $arrays)) '$nonscalar)
	       ((mget exp '$scalar) '$scalar)))
	((specrepp exp) (scalarclass (specdisrep exp)))
	;;  If the function is declared scalar or nonscalar, then return.  If it isn't
	;;  explicitly declared, then try to be intelligent by looking at the arguments
	;;  to the function.
	((scalarclass (caar exp)))
	;;  <number> + <scalar> is SCALARP because that seems to be useful.  This should
	;;  probably only be true if <number> is a member of the field of scalars.
	;;  <number> * <scalar> is SCALARP since <scalar> + <scalar> is SCALARP.
	;;  Also, this has to be done to make <scalar> - <scalar> SCALARP.
	((member (caar exp) '(mplus mtimes) :test #'eq)
	 (do ((l (cdr exp) (cdr l))) ((null l) '$scalar)
	   (if (not (consttermp (car l)))
	       (return (scalarclass-list l)))))
	((and (eq (caar exp) 'mqapply) (scalarclass (cadr exp))))
	((mxorlistp exp) '$nonscalar)
	;;  If we can't find out anything about the operator, then look at the arguments
	;;  to the operator.  I think NIL should be returned at this point.  -cwh
	(t (do ((exp (cdr exp) (cdr exp)) (l))
	       ((null exp) (scalarclass-list l))
	     (if (not (consttermp (car exp)))
		 (setq l (cons (car exp) l)))))))

;;  Could also do <scalar> +|-|*|/ |^ <declared constant>, but this is not
;;  always correct and could screw somebody.

;;  SCALARCLASS-LIST takes a list of expressions as its argument.  If their
;;  scalarclasses all agree, then that scalarclass is returned.

(defun scalarclass-list (llist)
  (cond ((null llist) nil)
	((null (cdr llist)) (scalarclass (car llist)))
	(t (let ((sc-car (scalarclass (car llist)))
		 (sc-cdr (scalarclass-list (cdr llist))))
	     (cond ((or (eq sc-car '$nonscalar)
			(eq sc-cdr '$nonscalar))
		    '$nonscalar)
		   ((and (eq sc-car '$scalar) (eq sc-cdr '$scalar))
		    '$scalar))))))

(defmfun mbagp (x)
  (and (not (atom x))
       (member (caar x) '(mequal mlist $matrix) :test #'eq)))

(defmfun mequalp (x) (and (not (atom x)) (eq (caar x) 'mequal)))

(defmfun mxorlistp (x)
  (and (not (atom x))
       (member (caar x) '(mlist $matrix) :test #'eq)))

(defun mxorlistp1 (x)
  (and (not (atom x))
       (or (eq (caar x) '$matrix)
	   (and (eq (caar x) 'mlist) $listarith))))

(defmfun constfun (ign)
  (declare (ignore ign)) ; Arg ignored.  Function used for mapping down lists.
  *const*)

(defun constmx (*const* x)
  (simplifya (fmapl1 'constfun x) t))

;(defmfun isinop (exp var)		; VAR is assumed to be an atom
;  (cond ((atom exp) nil)
;	((and (eq (caar exp) var) (not (member 'array (cdar exp) :test #'eq))))
;	(t (do ((exp (cdr exp) (cdr exp))) ((null exp))
;	     (cond ((isinop (car exp) var) (return t)))))))

;;; A modification of the old isinop. This version returns the complete 
;;; subexpression with the operator OP, when the operator OP is found in EXPR.

(defun isinop (expr op)    ; OP is assumed to be an atom
  (cond ((atom expr) nil)
        ((and (eq (caar expr) op)
              (not (member 'array (cdar expr) :test #'eq)))
         expr)
        (t 
         (do ((expr (cdr expr) (cdr expr))
              (res nil))
             ((null expr))
           (when (setq res (isinop (car expr) op)) 
             (return res))))))

(defmfun free (exp var)
  (cond ((alike1 exp var) nil)
	((atom exp) t)
	(t
	 (and (listp (car exp))
	      (free (caar exp) var)
	      (freel (cdr exp) var)))))

(defmfun freel (l var)
  (do ((l l (cdr l))) ((null l) t)
    (cond ((not (free (car l) var)) (return nil)))))

(defmfun freeargs (exp var)
  (cond ((alike1 exp var) nil)
	((atom exp) t)
	(t (do ((l (margs exp) (cdr l))) ((null l) t)
	     (cond ((not (freeargs (car l) var)) (return nil)))))))

(defmfun simplifya (x y)
  (cond ((and $%enumer $numer (atom x) (eq x '$%e))
         ;; Replace $%e with its numerical value, when %enumer and $numer TRUE
         (setq x %e-val))
        ((or (atom x) (not $simp)) x)
	((atom (car x))
	 (cond ((and (cdr x) (atom (cdr x)))
		(merror "~%~S is a cons with an atomic cdr - `simplifya'" x))
	       ((get (car x) 'lisp-no-simp)
		;; this feature is to be used with care. it is meant to be
		;; used to implement data objects with minimum of consing.
		;; forms must not bash the DISPLA package. Only new forms
		;; with carefully chosen names should use this feature.
		x)
	       (t (cons (car x)
			(mapcar #'(lambda (x) (simplifya x y)) (cdr x))))))
	((eq (caar x) 'rat) (*red1 x))
	((and (not dosimp) (member 'simp (cdar x) :test #'eq)) x)
	((eq (caar x) 'mrat) x)
	((and (member (caar x) '(mplus mtimes mexpt) :test #'eq)
	      (member (get (caar x) 'operators) '(simplus simpexpt simptimes) :test #'eq)
	      (not (member 'array (cdar x) :test #'eq)))
	 (cond ((eq (caar x) 'mplus) (simplus x 1 y))
	       ((eq (caar x) 'mtimes) (simptimes x 1 y))
	       (t (simpexpt x 1 y))))
	((not (atom (caar x)))
	 (cond ((or (eq (caaar x) 'lambda)
		    (and (not (atom (caaar x))) (eq (caaaar x) 'lambda)))
		(mapply1 (caar x) (cdr x) (caar x) x))
	       (t (merror "Illegal form - `simplifya':~%~S" x))))
        ((and $distribute_over
              (get (caar x) 'distribute_over)
              ;; A function with the property 'distribute_over.
              ;; Look, if we have a bag as argument to the function.
              (distribute-over x)))
	((get (caar x) 'opers)
	 (let ((opers-list *opers-list)) (oper-apply x y)))
	((and (eq (caar x) 'mqapply)
	      (or (atom (cadr x))
		  (and (eq substp 'mqapply)
		       (or (eq (car (cadr x)) 'lambda)
			   (eq (caar (cadr x)) 'lambda)))))
	 (cond ((or (symbolp (cadr x)) (not (atom (cadr x))))
		(simplifya (cons (cons (cadr x) (cdar x)) (cddr x)) y))
	       ((or (not (member 'array (cdar x) :test #'eq)) (not $subnumsimp))
		(merror "Improper value in functional position:~%~M" x))
	       (t (cadr x))))
	;;sometimes want function or closure!
	;;        ((and (not (symbolp (caar x)))
	;;	      (functionp (caar x))) (show (caar x))
	;;	 (apply (caar x) (cdr x)))
	(t (let ((w (get (caar x) 'operators)))
	     (cond ((and w (or (not (member 'array (cdar x) :test #'eq)) (rulechk (caar x))))
		    (funcall w x 1 y))
		   (t (simpargs x y)))))))

(defmfun eqtest (x check)
  (let ((y nil))
    (cond ((or (atom x)
	       (eq (caar x) 'rat)
	       (eq (caar x) 'mrat)
	       (member 'simp (cdar x) :test #'eq))
	   x)
	  ((and (eq (caar x) (caar check))
		(equal (cdr x) (cdr check)))
	   (cond ((and (null (cdar check))
		       (setq y (get (caar check) 'msimpind)))
		  (cons y (cdr check)))
		 ((member 'simp (cdar check) :test #'eq)
		  check)
		 (t
		  (cons (cons (caar check)
			      (if (cdar check)
				  (cons 'simp (cdar check))
				  '(simp)))
			(cdr check)))))
	  ((setq y (get (caar x) 'msimpind))
	   (rplaca x y))
	  ((or (member 'array (cdar x) :test #'eq)
	       (and (eq (caar x) (caar check))
		    (member 'array (cdar check) :test #'eq)))
	   (rplaca x (cons (caar x) '(simp array))))
	  (t
	   (rplaca x (cons (caar x) '(simp)))))))

;; A function, which distributes of bags like a list, matrix, or equation.
;; Check, if we have to distribute of one of the bags or any other operator.
(defun distribute-over (expr)
  (cond ((= 1 (length (cdr expr)))
         ;; Distribute over for a function with one argument.
         (cond ((and (not (atom (cadr expr)))
                     (member (caaadr expr) (get (caar expr) 'distribute_over))
                     ;; Distribute over lists only if $listarith is T
                     (or $listarith (not (eq (caaadr expr) 'mlist))))
                (simplify
                  (cons (caadr expr)
                        (mapcar #'(lambda (u) (simplify (list (car expr) u)))
                                (cdadr expr)))))
                (t nil)))
        (t
         ;; A function with more than one argument.
         (do ((args (cdr expr) (cdr args))
              (new-expr nil)
              (first-args nil))
             ((null args) nil)
           (when (and (not (atom (car args)))
                      (member (caar (car args))
                              (get (caar expr) 'distribute_over))
                      ;; Disribute over lists only if $listarith is T
                      (or $listarith (not (eq (caar (car args)) 'mlist))))
             ;; Distribute the function over the arguments and simplify again.
             (return 
               (simplify 
                 (cons (ncons (caar (car args)))
                       (mapcar #'(lambda (u) 
                                   (simplify 
                                     (append 
                                       (append 
                                         (cons (ncons (caar expr))
                                               (reverse first-args))
                                         (ncons u))
                                       (rest args))))
                               (cdr (car args)))))))
           (setq first-args (cons (car args) first-args))))))

(defun rulechk (x) (or (mget x 'oldrules) (get x 'rules)))

(defmfun resimplify (x) (let ((dosimp t)) (simplifya x nil)))

(defmfun ssimplifya (x) (let ((dosimp t)) (simplifya x nil))) ; temporary

(defun simpargs (x y)
  (if (or (eq (get (caar x) 'dimension) 'dimension-infix)
	  (get (caar x) 'binary))
      (twoargcheck x))
  (if (and (member 'array (cdar x) :test #'eq) (null (margs x)))
      (merror "Subscripted variable found with no subscripts."))
  (eqtest (if y x (let ((flag (member (caar x) '(mlist mequal) :test #'eq)))
		    (cons (ncons (caar x))
			  (mapcar #'(lambda (u)
				      (if flag (simplifya u nil)
					  (simpcheck u nil)))
				  (cdr x)))))
	  x))

(defmfun addk (xx yy)	; Xx and Yy are assumed to be alreadyy reduced
  (cond ((equal xx 0) yy)
	((equal yy 0) xx)
	((and (numberp xx) (numberp yy)) (+ xx yy))
	((or ($bfloatp xx) ($bfloatp yy)) ($bfloat (list '(mplus) xx yy)))
	(t (prog (g a b (x xx)(y yy))
	      (cond ((numberp x)
		     (cond ((floatp x) (return (+ x (fpcofrat y))))
			   (t (setq x (list '(rat) x 1)))))
		    ((numberp y)
		     (cond ((floatp y) (return (+ y (fpcofrat x))))
			   (t (setq y (list '(rat) y 1))))))
	      (setq g (gcd (caddr x) (caddr y)))
	      (setq a (*quo (caddr x) g) b (*quo (caddr y) g))
	      (setq g (timeskl (list '(rat) 1 g)
			       (list '(rat)
				     (+ (* (cadr x) b)
					   (* (cadr y) a))
				     (* a b))))
	      (return (cond ((numberp g) g)
			    ((equal (caddr g) 1) (cadr g))
			    ($float (fpcofrat g))
			    (t g)))))))

(defun *red1 (x)
  (cond ((member 'simp (cdar x) :test #'eq)
	 (if $float (fpcofrat x) x))
	(t (*red (cadr x) (caddr x)))))

(defun *red (n d)
  (cond ((zerop n) 0)
	((equal d 1) n)
	(t (let ((u (gcd n d)))
	     (setq n (*quo n u) d (*quo d u))
	     (if (minusp d) (setq n (- n) d (- d)))
	     (cond ((equal d 1) n)
		   ($float (fpcofrat1 n d))
		   (t (list '(rat simp) n d)))))))



(defun num1 (a) (if (numberp a) a (cadr a)))

(defun denom1 (a) (if (numberp a) 1 (caddr a)))

(defmfun timesk (x y)	   ; X and Y are assumed to be already reduced
  (cond ((equal x 1) y)
	((equal y 1) x)
	((and (numberp x) (numberp y)) (* x y))
	((or ($bfloatp x) ($bfloatp y)) ($bfloat (list '(mtimes) x y)))
	((floatp x) (* x (fpcofrat y)))
	((floatp y) (* y (fpcofrat x)))
	(t (timeskl x y))))

(defun timeskl (x y)
  (prog (u v g)
     (setq u (*red (num1 x) (denom1 y)))
     (setq v (*red (num1 y) (denom1 x)))
     (setq g (cond ((or (equal u 0) (equal v 0)) 0)
		   ((equal v 1) u)
		   ((and (numberp u) (numberp v)) (* u v))
		   (t (list '(rat simp)
			    (* (num1 u) (num1 v))
			    (* (denom1 u) (denom1 v))))))
     (return (cond ((numberp g) g)
		   ((equal (caddr g) 1) (cadr g))
		   ($float (fpcofrat g))
		   (t g)))))

(defmfun fpcofrat (ratno) (fpcofrat1 (cadr ratno) (caddr ratno)))

;;--- fpcofrat1  :: Floating Point Conversion OF RATional number routine
;;  find floating point approximation to rational number
;;  fpcofrat1 computes the quotient of nu/d

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)
    (defconstant machine-mantissa-precision (float-digits 1.0)))

(defun fpcofrat1 (nu d)
  (float (/ nu d)))

(defun expta (x y)
  (cond ((equal y 1)
	 x)
	((numberp x)
	 (exptb x (num1 y)))
	(($bfloatp x)
	 ($bfloat (list '(mexpt) x y)))
	((minusp (num1 y))
	 (*red (exptb (caddr x) (- (num1 y)))
	       (exptb (cadr x) (- (num1 y)))))
	(t
	 (*red (exptb (cadr x) (num1 y))
	       (exptb (caddr x) (num1 y))))))

;; I (rtoy) think EXPTB is meant to compute a^b, where b is an
;; integer.
(defun exptb (a b)
  (cond ((equal a %e-val)
	 ;; Make B a float so we'll get double-precision result.
	 (exp (float b)))
	((or (floatp a) (not (minusp b)))
	 (expt a b))
	(t
	 (setq b (expt a (- b)))
	 (*red 1 b))))

(defmfun simplus (x w z)		; W must be 1
  (prog (res check eqnflag matrixflag sumflag)
     (if (null (cdr x)) (return 0))
     (setq check x)
     start(setq x (cdr x))
     (if (null x) (go end))
     (setq w (if z (car x) (simplifya (car x) nil)))
     st1  (cond
	    ((atom w) nil)
	    ((eq (caar w) 'mrat)
	     (cond ((or eqnflag matrixflag
			(and sumflag (not (member 'trunc (cdar w) :test #'eq)))
			(spsimpcases (cdr x) w))
		    (setq w (ratdisrep w)) (go st1))
		   (t (return (ratf (cons '(mplus)
					  (nconc (mapcar #'simplify (cons w (cdr x)))
						 (cdr res))))))))
	    ((eq (caar w) 'mequal)
	     (setq eqnflag
		   (if (not eqnflag)
		       w
		       (list (car eqnflag)
			     (add2 (cadr eqnflag) (cadr w))
			     (add2 (caddr eqnflag) (caddr w)))))
	     (go start))
	    ((member (caar w) '(mlist $matrix) :test #'eq)
	     (setq matrixflag
		   (cond ((not matrixflag) w)
			 ((and (or $doallmxops $domxmxops $domxplus
				   (and (eq (caar w) 'mlist) ($listp matrixflag)))
			       (or (not (eq (caar w) 'mlist)) $listarith))
			  (addmx matrixflag w))
			 (t (setq res (pls w res)) matrixflag)))
	     (go start))
	    ((eq (caar w) '%sum)
	     (setq sumflag t res (sumpls w res))
	     (setq w (car res) res (cdr res))))
     (setq res (pls w res))
     (go start)
     end  (setq res (testp res))
     (if matrixflag
	 (setq res (cond ((zerop1 res) matrixflag)
			 ((and (or ($listp matrixflag)
				   $doallmxops $doscmxplus $doscmxops)
			       (or (not ($listp matrixflag)) $listarith))
			  (mxplusc res matrixflag))
			 (t (testp (pls matrixflag (pls res nil)))))))
     (setq res (eqtest res check))
     (return (if eqnflag
		 (list (car eqnflag)
		       (add2 (cadr eqnflag) res)
		       (add2 (caddr eqnflag) res))
		 res))))

(defun mxplusc (sc mx)
  (cond ((mplusp sc)
	 (setq sc (partition-ns (cdr sc)))
	 (cond ((null (car sc)) (cons '(mplus) (cons mx (cadr sc))))
	       ((not (null (cadr sc)))
		(cons '(mplus)
		      (cons (simplify
			     (outermap1 'mplus (cons '(mplus) (car sc)) mx))
			    (cadr sc))))
	       (t (simplify (outermap1 'mplus (cons '(mplus) (car sc)) mx)))))
	((not (scalar-or-constant-p sc $assumescalar))
	 (testp (pls mx (pls sc nil))))
	(t (simplify (outermap1 'mplus sc mx)))))

(defun partition-ns (x)
  (let (sp nsp)		      ; SP = scalar part, NSP = nonscalar part
    (mapc #'(lambda (z) (if (scalar-or-constant-p z $assumescalar)
			    (setq sp (cons z sp))
			    (setq nsp (cons z nsp))))
	  x)
    (list (nreverse sp) (nreverse nsp))))

(defun addmx (x1 x2)
  (let (($doscmxops t) ($domxmxops t) ($listarith t))
    (simplify (fmapl1 'mplus x1 x2))))

;; adds x into running list of additive terms fm
;; returns new list of terms
(defun plusin (x fm)
  (prog (x1 flag check w xnew)
     (setq w 1)
     (cond ((mtimesp x)
            (setq check x)
            (if (mnump (cadr x)) (setq w (cadr x) x (cddr x))
                (setq x (cdr x))))
           (t (setq x (ncons x))))
     (setq x1 (if (null (cdr x)) (car x) (cons '(mtimes) x))
           xnew (list* '(mtimes) w x))
  start 
     (cond ((null (cdr fm)))
           ((mtimesp (cadr fm))
            (cond ((alike1 x1 (cadr fm))
                   (go equt))
                  ((and (mnump (cadadr fm)) (alike x (cddadr fm)))
                   (setq flag t) ; found common factor
                   (go equt))
                  ((great xnew (cadr fm)) (go gr))))
           ((and (alike1 x1 (cadr fm)) (null (cdr x))) 
            (go equ))
           ((great x1 (cadr fm)) (go gr)))
     (setq xnew (eqtest (testt xnew) (or check '((foo)))))
     (return (cdr (rplacd fm (cons xnew (cdr fm)))))
  gr   
     (setq fm (cdr fm))
     (go start)
  equ
     ;; Call muln to get a simplified product.
     (setq x1 (muln (cons (addk 1 w) x) t))
     (if (or (zerop1 x1)
             (onep1 x1))
         (setq x1 (list* '(mtimes simp) x1 x))
         (setq x1 (if (not (atom x1)) (testtneg x1) x1)))
     (rplaca (cdr fm) x1)
  del  
     (cond ((not (mtimesp (cadr fm)))
            (go check))
           ((onep (cadadr fm))
            ;; Do this simplification for an integer 1, not for 1.0 and 1.0b0
            (rplacd (cadr fm) (cddadr fm))
            (return (cdr fm)))
           ((not (zerop1 (cadadr fm))) 
            (return (cdr fm)))
           ((and (or (not $listarith) (not $doallmxops))
                 (zerop1 (cadadr fm))
                 (mxorlistp (caddr (cadr fm))))
            (return (rplacd fm 
                            (cons (constmx 0 (caddr (cadr fm))) (cddr fm))))))
     ;; (cadadr fm) is zero. If the first term of fm is a number, 
     ;;  add it to preserve the type.
     (when (mnump (car fm))
       (rplaca fm (add (car fm) (cadadr fm))))
     (return (rplacd fm (cddr fm)))
  equt
     ;; Call muln to get a simplified product.
     (setq x1 (muln (cons (addk (cond (flag (cadadr fm))
                                      (t 1))
                           w)
                   x) t))
     (if (or (zerop1 x1)
             (onep1 x1))
         (setq x1 (list* '(mtimes) x1 x))
         (setq x1 (if (not (atom x1)) (testtneg x1) x1)))
     (rplaca (cdr fm) x1)
     (if (not (mtimesp x1)) (go check))
     (when (and (onep (cadadr fm)) flag (null (cdddr (cadr fm))))
       ;; Do this simplification for an integer 1, not for 1.0 and 1.0b0
       (rplaca (cdr fm) (caddr (cadr fm))) (go check))
     (go del)
  check
     (if (mplusp (cadr fm)) (setq plusflag t))
     (return (cdr fm))))

(defmfun simpln (x y z)
  (oneargcheck x)
  (cond ((onep1 (setq y (simpcheck (cadr x) z))) (addk -1 y))
	((zerop1 y)
	 (cond (radcanp (list '(%log simp) 0))
	       ((not errorsw) (merror "log(0) has been generated."))
	       (t (throw 'errorsw t))))
	((eq y '$%e) 1)
	((and (mexptp y) (eq (cadr y) '$%e))	;; log(%e^x) -> x
	 (simplifya (caddr y) t))
	((ratnump y)
	 (cond ((equal (cadr y) 1) (simpln1 (list nil (caddr y) -1)))
	       ((eq $logexpand '$super)
		(simplifya (list '(mplus) (simplifya (list '(%log) (cadr y)) t)
				 (simpln1 (list nil (caddr y) -1))) t))
	       (t (eqtest (list '(%log) y) x))))
	((and $logexpand (mexptp y)) (simpln1 y))
	((and (member $logexpand '($all $super) :test #'eq) (mtimesp y))
	 (prog (b)
	    (setq y (cdr y))
	    loop (setq b (cons (cond ((not (mexptp (car y)))
				      (simplifya (list '(%log) (car y)) t))
				     (t (simpln1 (car y)))) b))
	    (cond ((null (setq y (cdr y)))
		   (return (simplifya (cons '(mplus) b) t))))
	    (go loop)))
    ((and (member $logexpand '($all $super)) (consp y) (member (caar y) '(%product $product)))
     (let ((new-op (if (eq (getcharn (caar y) 1) #\%) '%sum '$sum)))
       (simplifya `((,new-op) ((%log) ,(cadr y)) ,@(cddr y)) t)))
	((flonum-eval (mop x) y))
	((and (not (member 'simp (car x) :test #'eq))
	      (big-float-eval (mop x) y)))
	;; (($bfloatp y) ($bfloat (list '(%log) y)))
	;; ((or (floatp y) (and $numer (integerp y)))
	;;  (cond ((plusp y) (log y))
	;;        ($lognumer (cond ((equal y -1) 0) (t (log (- y)))))
	;;        (t (add2 (log (- y)) (mul2 '$%i %pi-val)))))
	((and $lognegint (maxima-integerp y) (eq ($sign y) '$neg))
	 (add (mul '$%i '$%pi) (take '(%log) (neg y))))
        ((taylorize (mop x) (second x)))
	(t (eqtest (list '(%log) y) x))))

(defun simpln1 (w)
  (simplifya (list '(mtimes) (caddr w)
		   (simplifya (list '(%log) (cadr w)) t)) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementation of the Square root function

(defprop $sqrt %sqrt verb)
(defprop $sqrt %sqrt alias)

(defprop %sqrt $sqrt noun)
(defprop %sqrt $sqrt reversealias)

(defprop %sqrt simp-sqrt operators)

(defun $sqrt (z)
  (simplify (list '(%sqrt) z)))

(defmfun simp-sqrt (x y z)
  (declare (ignore y))
  (oneargcheck x)
  (simplifya (list '(mexpt) (cadr x) '((rat simp) 1 2)) z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun simpquot (x y z)
  (twoargcheck x)
  (cond ((and (integerp (cadr x)) (integerp (caddr x)) (not (zerop (caddr x))))
	 (*red (cadr x) (caddr x)))
	((and (numberp (cadr x)) (numberp (caddr x)) (not (zerop (caddr x))))
	 (*quo (cadr x) (caddr x)))
	(t (setq y (simplifya (cadr x) z))
	   (setq x (simplifya (list '(mexpt) (caddr x) -1) z))
	   (if (equal y 1) x (simplifya (list '(mtimes) y x) t)))))

;; Obsolete.  Use DIV*.  All references to this should now be flushed.
;; This definition will go away soon.

;;(DEFUN QSNT (X Y) (SIMPLIFY (LIST '(MTIMES) X (LIST '(MEXPT) Y -1))))

(setf (get '%mabs 'operators) 'simpabs)

(defmfun simpabs (x y z)
  (oneargcheck x)
  (setq y (simpcheck (cadr x) z))
  (cond ((numberp y) (abs y))
	((or (arrayp y) ($member y $arrays)) `((mabs simp) ,y))
	((or (ratnump y) ($bfloatp y)) (list (car y) (abs (cadr y)) (caddr y)))
	((taylorize 'mabs (second x)))
	((member y '($inf $infinity $minf) :test #'eq) '$inf)
	((member y '($ind $und) :test #'eq) y)

        ;; Simplify $conjugate before handling complex expressions.
	((op-equalp y '$conjugate) 
         (simplifya `((mabs) ,(first (margs y))) nil))

        ;; Check for a complex expression with $csign, but not when in limit.
        ((and (not limitp) 
              (member (setq z ($csign y)) '($complex $imaginary)))
         (cond ((symbolp y)
                ;; Do not call cabs for complex symbols.
                (cond ((eq y '$%i) 1)
                      (t (eqtest (list '(mabs) y) x))))
               (t (cabs y))))

        ;; Check for a complex expression with csign, when in limit.
	((eq (setq z (csign y)) t) (cabs y))
        ;; Check for the sign of the expression.
	((member z '($pos $pz) :test #'eq) y)
	((member z '($neg $nz) :test #'eq) (neg y))
	((eq z '$zero) 0)
	;; If csign(y) = pn, we have abs(signum(y)) = 1.
	((and (eq z '$pn) (op-equalp y '%signum)) 1)

	((and (mexptp y) ($featurep (caddr y) '$integer))
	 (list (car y) (simplifya (list '(mabs) (cadr y)) nil) (caddr y)))
	((mtimesp y)
	 (muln 
           (mapcar #'(lambda (u) (simplifya (list '(mabs) u) nil)) (cdr y)) t))
	((mminusp y) (list '(mabs simp) (neg y)))
	((mbagp y)
	 (cons (car y)
	       (mapcar #'(lambda (u)
			   (simplifya (list '(mabs) u) nil)) (cdr y))))
	(t (eqtest (list '(mabs) y) x))))


(defun pls (x out)
  (prog (fm plusflag)
     (if (mtimesp x) (setq x (testtneg x)))
     (when (and $numer (atom x) (eq x '$%e))
       ;; Replace $%e with its numerical value, when $numer ist TRUE
       (setq x %e-val))
     (cond ((null out)
	    (return
	      (cons '(mplus)
		    (cond ((mnump x) (ncons x))
			  ((not (mplusp x))
			   (list 0 (cond ((atom x) x) (t (copy-list x)))))
			  ((mnump (cadr x)) (copy-list (cdr x) ))
			  (t (cons 0 (copy-list (cdr x) )))))))
	   ((mnump x)
	    (return (cons '(mplus)
			  (if (mnump (cadr out))
			      (cons (addk (cadr out) x) (cddr out))
			      (cons x (cdr out))))))
	   ((not (mplusp x)) (plusin x (cdr out)) (go end)))
     (rplaca (cdr out)
	     (addk (if (mnump (cadr out)) (cadr out) 0)
		   (cond ((mnump (cadr x)) (setq x (cdr x)) (car x)) (t 0))))
     (setq fm (cdr out))
     start(if (null (setq x (cdr x))) (go end))
     (setq fm (plusin (car x) fm))
     (go start)
     end  (if (not plusflag) (return out))
     (setq plusflag nil)		; PLUSFLAG T handles e.g.
     a    (setq fm (cdr out))		;	a+b+3*(a+b)-2*(a+b)
     loop (when (mplusp (cadr fm))
	    (setq x (cadr fm)) (rplacd fm (cddr fm))
	    (pls x out) (go a))
     (setq fm (cdr fm))
     (if (null (cdr fm)) (return out))
     (go loop)))

;; I (rtoy) think this does some simple optimizations of x * y.
(defun testt (x)
  (cond ((mnump x)
	 x)
	((null (cddr x))
	 ;; We have something like ((mtimes) foo).  This is the same as foo.
	 (cadr x))
	((eql 1 (cadr x))
	 ;; We have 1*foo.  Which is the same as foo.  This should not
	 ;; be applied to 1.0 or 1b0!
	 (cond ((null (cdddr x))
		(caddr x))
	       (t (rplacd x (cddr x)))))
	(t
	 (testtneg x))))

;; This basically converts -(a+b) to -a-b.
(defun testtneg (x)
  (cond ((and (equal (cadr x) -1)
	      (null (cdddr x))
	      (mplusp (caddr x))
	      $negdistrib)
	 ;; If x is exactly of the form -1*(sum), and $negdistrib is
	 ;; true, we distribute the -1 across the sum.
	 (addn (mapcar #'(lambda (z)
			   (mul2 -1 z))
		       (cdaddr x))
	       t))
	(t x)))

(defun testp (x) (cond ((atom x) 0)
		       ((null (cddr x)) (cadr x))
		       ((zerop1 (cadr x))
			(cond ((null (cdddr x)) (caddr x)) (t (rplacd x (cddr x)))))
		       (t x)))

(defun simpmin (x vestigial z)
  (declare (ignore vestigial))
  (oneargcheck x)
  (cond ((numberp (cadr x)) (- (cadr x)))
	((atom (cadr x)) (list '(mtimes simp) -1 (cadr x)))
	(t (simplifya (list '(mtimes) -1 (simplifya (cadr x) z)) t))))

(defmfun simptimes (x w z)		; W must be 1
  (prog (res check eqnflag matrixflag sumflag)
     (if (null (cdr x)) (return 1))
     (setq check x)
     start(setq x (cdr x))
     (cond ((zerop1 res)
	    (cond ($mx0simp
		   (cond ((and matrixflag (mxorlistp1 matrixflag))
			  (return (constmx res matrixflag)))
			 (eqnflag (return (list '(mequal simp)
						(mul2 res (cadr eqnflag))
						(mul2 res (caddr eqnflag)))))
			 (t (dolist (u x)
			      (cond (  (mxorlistp u)
				     (return
				       (setq res (constmx res u))))
				    ((and (mexptp u)
					  (mxorlistp1 (cadr u))
					  ($numberp (caddr u)))
				     (return
				       (setq res (constmx res (cadr u)))))
				    ((mequalp u)
				     (return
				       (setq res (list '(mequal simp)
						       (mul2 res (cadr u))
						       (mul2 res (caddr u))))))))))))
	    (return res))
	   ((null x) (go end)))
     (setq w (if z (car x) (simplifya (car x) nil)))
     st1  (cond
	    ((atom w) nil)
	    ((eq (caar w) 'mrat)
	     (cond ((or eqnflag matrixflag
			(and sumflag (not (member 'trunc (cdar w) :test #'eq)))
			(spsimpcases (cdr x) w))
		    (setq w (ratdisrep w)) (go st1))
		   (t (return (ratf (cons '(mtimes)
					  (nconc (mapcar #'simplify (cons w (cdr x)))
						 (cdr res))))))))
	    ((eq (caar w) 'mequal)
	     (setq eqnflag
		   (if (not eqnflag)
		       w
		       (list (car eqnflag)
			     (mul2 (cadr eqnflag) (cadr w))
			     (mul2 (caddr eqnflag) (caddr w)))))
	     (go start))
	    ((member (caar w) '(mlist $matrix) :test #'eq)
	     (setq matrixflag
		   (cond ((not matrixflag) w)
			 ((and (or $doallmxops $domxmxops $domxtimes)
			       (or (not (eq (caar w) 'mlist)) $listarith)
			       (not (eq *inv* '$detout)))
			  (stimex matrixflag w))
			 (t (setq res (tms w 1 res)) matrixflag)))
	     (go start))
	    ((and (eq (caar w) '%sum) $sumexpand)
	     (setq sumflag (sumtimes sumflag w)) (go start)))
     (setq res (tms w 1 res))
     (go start)
     end  (cond ((mtimesp res) (setq res (testt res))))
     (cond (sumflag (setq res (cond ((or (null res) (equal res 1)) sumflag)
				    ((not (mtimesp res))
				     (list '(mtimes) res sumflag))
				    (t (nconc res (list sumflag)))))))
     (cond ((or (atom res)
		(not (member (caar res) '(mexpt mtimes) :test #'eq))
		(and (zerop $expop) (zerop $expon))
		expandflag))
	   ((eq (caar res) 'mtimes) (setq res (expandtimes res)))
	   ((and (mplusp (cadr res))
		 (fixnump (caddr res))
		 (not (or (> (caddr res) $expop)
			  (> (- (caddr res)) $expon))))
	    (setq res (expandexpt (cadr res) (caddr res)))))
     (cond (matrixflag
	    (setq res (cond ((null res) matrixflag)
			    ((and (or ($listp matrixflag) $doallmxops
				      (and $doscmxops (not (member res '(-1 -1.0) :test #'equal)))
			;;; RES should only be -1 here (not = 1)
				      (and $domxmxops (member res '(-1 -1.0) :test #'equal)))
				  (or (not ($listp matrixflag)) $listarith))
			     (mxtimesc res matrixflag))
			    (t (testt (tms matrixflag 1 (tms res 1 nil))))))))
     (if res (setq res (eqtest res check)))
     (return (cond (eqnflag
		    (if (null res) (setq res 1))
		    (list (car eqnflag)
			  (mul2 (cadr eqnflag) res)
			  (mul2 (caddr eqnflag) res)))
		   (t res)))))

(defun spsimpcases (l e)
  (dolist (u l)
    (if (or (mbagp u) (and (not (atom u))
			   (eq (caar u) '%sum)
			   (not (member 'trunc (cdar e) :test #'eq))))
	(return t))))

(defun mxtimesc (sc mx)
  (let (sign out)
    (and (mtimesp sc) (member (cadr sc) '(-1 -1.0) :test #'equal)
	 $doscmxops (not (or $doallmxops $domxmxops $domxtimes))
	 (setq sign (cadr sc)) (rplaca (cdr sc) nil))
    (setq out (let ((scp* (cond ((mtimesp sc) (partition-ns (cdr sc)))
				((not (scalar-or-constant-p sc $assumescalar)) nil)
				(t sc))))
		(cond  ((null scp*) (list '(mtimes simp) sc mx))
		       ((and (not (atom scp*)) (null (car scp*)))
			(append '((mtimes)) (cadr scp*) (list mx)))
		       ((or (atom scp*) (and (null (cdr scp*))
					     (not (null (cdr sc)))
					     (setq scp* (cons '(mtimes) (car scp*))))
			    (not (mtimesp sc)))
			(simplifya (outermap1 'mtimes scp* mx) nil))
		       (t (append '((mtimes))
				  (list (simplifya
					 (outermap1 'mtimes
						    (cons '(mtimes) (car scp*)) mx)
					 t))
				  (cadr scp*))))))
    (cond (sign (if (mtimesp out)
		    (rplacd out (cons sign (cdr out)))
		    (list '(mtimes) sign out)))
	  ((mtimesp out) (testt out))
	  (t out))))

(defun stimex (x y)
  (let (($doscmxops t) ($domxmxops t) ($listarith t))
    (simplify (fmapl1 'mtimes x y))))

;;  TMS takes a simplified expression FACTOR and a cumulative
;;  PRODUCT as arguments and modifies the cumulative product so
;;  that the expression is now one of its factors.  The
;;  exception to this occurs when a tellsimp rule is triggered.
;;  The second argument is the POWER to which the expression is
;;  to be raised within the product.

(defun tms (factor power product &aux tem)
  (let ((rulesw nil)
	(z nil))
    (when (mplusp product) (setq product (list '(mtimes simp) product)))
    (cond ((zerop1 factor)
	   (cond ((mnegp power)
		  (if errorsw
		      (throw 'errorsw t)
		      (merror "Division by 0")))
		 (t factor)))
	  ((and (null product)
		(or (and (mtimesp factor) (equal power 1))
		    (and (setq product (list '(mtimes) 1)) nil)))
	   (setq tem (append '((mtimes)) (if (mnump (cadr factor)) nil '(1))
			     (cdr factor) nil))
	   (if (= (length tem) 1)
	       (setq tem (copy-list tem))
	       tem))

; We do not handle numbers in TMS but in TIMESIN
;      	  ((mnump factor)
;	   ;; We need to do something better here.  Look through
;	   ;; product to see if there are any terms of the form
;	   ;; factor^k, and adjust the exponent.
;
;	   ;;(format t "tms mnump factor = ~A~%" factor)
;	   ;;(format t "tms product = ~A~%" product)
;	   (let ((expo nil))
;	     (do ((p (cdr product) (cdr p)))
;		 ((or (null p) expo))
;	       ;;(format t "p = ~A~%" p)
;	       (when (and (mexptp (car p))
;			  (integerp (second (car p)))
;			  ;;(integerp factor)
;			  (setf expo (exponent-of factor (second (car p)))))
;		 (let* ((q (div factor (power (second (car p)) expo)))
;			(temp (mul q (list '(mexpt)
;					   (second (car p))
;					   (add expo (third (car p)))))))
;		   ;;(format t "temp = ~A~%" temp)
;		   ;;(format t "p = ~A~%" p)
;		   ;;(format t "cdr p = ~A~%" (cdr p))
;		   (setf temp (append (list temp) (cdr p)))
;		   ;;(format t "new temp = ~A~%" temp)
;		   ;;(rplaca p temp)
;		   (rplacd p (cdr temp))
;		   (rplaca p (car temp))
;		   ;;(format t "mod p = ~A~%" p)
;		   )))
;	     (unless expo
;	       (rplaca (cdr product) (timesk (cadr product) (expta factor power)))))
;	   (if (and (mtimesp product)
;		    (mtimesp (cadr product)))
;	       (rplacd product (append (cdadr product) (cddr product))))
;	   product)
          
	  ((mtimesp factor)
; We do not handle numbers in TMS, but in TIMESIN	   
;	   (when (mnump (cadr factor))
;	     (setq factor (cdr factor))
;	     (rplaca (cdr product)
;		     (timesk (cadr product) (expta (car factor) power))))
	   (do ((factor-list (cdr factor) (cdr factor-list)))
	       ((or (null factor-list) (zerop1 product))  product)
	     (setq z (timesin (car factor-list) (cdr product) power))
	     (when rulesw
	       (setq rulesw nil)
	       (setq product (tms-format-product z)))))
	  (t
	   (setq z (timesin factor (cdr product) power))
	   (if rulesw
	       (tms-format-product z)
	       product)))))

(defun tms-format-product (x)
  (cond ((zerop1 x) x)
	((mnump x) (list '(mtimes) x))
	((not (mtimesp x)) (list '(mtimes) 1 x))
	((not (mnump (cadr x))) (cons '(mtimes) (cons 1 (cdr x))))
	(t x)))

(defun plsk (x y)
  (cond ($ratsimpexpons (sratsimp (list '(mplus) x y)))
	((and (mnump x) (mnump y)) (addk x y))
	(t (add2 x y))))

(defun mult (x y)
  (if (and (mnump x) (mnump y))
      (timesk x y)
      (mul2 x y)))

(defmfun simp-limit (x vestigial z)
  (declare (ignore vestigial))
  (let ((l1 (length x))
	y)
    (unless (or (= l1 2) (= l1 4) (= l1 5))
      (merror "Wrong number of arguments to '`limit'"))
    (setq y (simpmap (cdr x) z))
    (cond ((and (= l1 5) (not (member (cadddr y) '($plus $minus) :test #'eq)))
	   (merror "4th arg to `limit' must be either `plus' or `minus':~%~M" (cadddr y)))
	  ((mnump (cadr y))
	   (merror "Wrong second arg to `limit':~%~M" (cadr y)))
	  ((equal (car y) 1)
	   1)
	  (t
	   (eqtest (cons '(%limit) y) x)))))

(defmfun simpinteg (x vestigial z)
  (declare (ignore vestigial))
  (let ((l1 (length x))
	y)
    (unless (or (= l1 3) (= l1 5))
      (merror "Wrong number of arguments to '`integrate'"))
    (setq y (simpmap (cdr x) z))
    (cond ((mnump (cadr y))
	   (merror "Attempt to integrate with respect to a number:~%~M" (cadr y)))
	  ((and (= l1 5) (alike1 (caddr y) (cadddr y)))
	   0)
	  ((and (= l1 5) (free (setq z (sub (cadddr y) (caddr y))) '$%i) (eq ($sign z) '$neg))
	   (neg (simplifya (list '(%integrate) (car y) (cadr y) (cadddr y) (caddr y)) t)))
	  ((equal (car y) 1)
	   (if (= l1 3)
	       (cadr y)
	       (if (or (among '$inf z) (among '$minf z))
		   (infsimp z)
		   z)))
	  (t
	   (eqtest (cons '(%integrate) y) x)))))

(defmfun simpbigfloat (x vestigial simp-flag)
  (declare (ignore vestigial simp-flag))
  (bigfloatm* x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Implementation of the Exp function.

; This is the old definition
;(defmfun simpexp (x vestigial z)
;  (declare (ignore vestigial))
;  (oneargcheck x)
;  (if ($taylorp (cadr x)) 
;      ($taylor x) 
;      (simplifya (list '(mexpt) '$%e (cadr x)) z)))

(defprop $exp %exp verb)
(defprop $exp %exp alias)

(defprop %exp $exp noun)
(defprop %exp $exp reversealias)

(defprop %exp simp-exp operators)

(defun $exp (z)
  (simplify (list '(%exp) z)))

;; Support a function for code,
;; which depends on an unsimplified noun form. 
(defun $exp-form (z)
  (list '(mexpt) '$%e z))

(defun simp-exp (x y z)
  (declare (ignore y))
  (oneargcheck x)
  (simplifya (list '(mexpt) '$%e (cadr x)) z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun simplambda (x vestigial simp-flag)
  (declare (ignore vestigial simp-flag))
  (cons '(lambda simp) (cdr x)))

(defmfun simpmdef (x vestigial simp-flag)
  (declare (ignore vestigial simp-flag))
  (twoargcheck x)
  (cons '(mdefine simp) (cdr x)))

(defun simpmap (e z)
  (mapcar #'(lambda (u) (simpcheck u z)) e))

(defmfun infsimp (e)
  (let ((x ($expand e 1 1)))
    (cond ((or (not (free x '$ind)) (not (free x '$und))
	       (not (free x '$zeroa)) (not (free x '$zerob))
	       (not (free x '$infinity))
	       (mbagp x))
	   (infsimp2 x e))
	  ((and (free x '$inf) (free x '$minf)) x)
	  (t (infsimp1 x e)))))

(defun infsimp1 (x e)
  (let ((minf-coef (coeff x '$minf 1))
	(inf-coef (coeff x '$inf 1)))
    (cond ((or (and (equal minf-coef 0)
		    (equal inf-coef 0))
	       (and (not (free minf-coef '$inf))
		    (not (free inf-coef '$minf)))
	       (let ((new-exp (sub (add2 (mul2 minf-coef '$minf)
					 (mul2 inf-coef '$inf))
				   x)))
		 (and (not (free new-exp '$inf))
		      (not (free new-exp '$minf)))))
	   (infsimp2 x e))
	  (t (let ((sign-minf-coef ($asksign minf-coef))
		   (sign-inf-coef ($asksign inf-coef)))
	       (cond ((or (and (eq sign-inf-coef '$zero)
			       (eq sign-minf-coef '$neg))
			  (and (eq sign-inf-coef '$pos)
			       (eq sign-minf-coef '$zero))
			  (and (eq sign-inf-coef '$pos)
			       (eq sign-minf-coef '$neg)))  '$inf)
		     ((or (and (eq sign-inf-coef '$zero)
			       (eq sign-minf-coef '$pos))
			  (and (eq sign-inf-coef '$neg)
			       (eq sign-minf-coef '$zero))
			  (and (eq sign-inf-coef '$neg)
			       (eq sign-minf-coef '$pos)))  '$minf)
		     ((or (and (eq sign-inf-coef '$pos)
			       (eq sign-minf-coef '$pos))
			  (and (eq sign-inf-coef '$neg)
			       (eq sign-minf-coef '$neg)))  '$und)))))))

(defun infsimp2 (x e)
  (setq x ($limit x))
  (if (isinop x '%limit) e x))

(defmfun simpderiv (x y z)
  (prog (flag w u)
     (cond ((not (even (length x)))
	    (cond ((and (cdr x) (null (cdddr x))) (nconc x '(1)))
		  (t (wna-err '%derivative)))))
     (setq w (cons '(%derivative) (simpmap (cdr x) z)))
     (setq y (cadr w))
     (do ((u (cddr w) (cddr u))) ((null u))
       (cond ((mnump (car u))
	      (merror "Attempt to differentiate with respect to a number:~%~M"
		      (car u)))))
     (cond ((or (zerop1 y)
		(and (or (mnump y) (and (atom y) (constant y)))
		     (or (null (cddr w))
			 (and (not (alike1 y (caddr w)))
			      (do ((u (cddr w) (cddr u))) ((null u))
				(cond ((and (numberp (cadr u)) (not (zerop (cadr u))))
				       (return t))))))))
	    (return 0))
	   ((and (not (atom y)) (eq (caar y) '%derivative) derivsimp)
	    (rplacd w (append (cdr y) (cddr w)))))
     (if (null (cddr w))
	 (return (if (null derivflag) (list '(%del simp) y) (deriv (cdr w)))))
     (setq u (cdr w))
     ztest(cond ((null u) (go next))
		((zerop1 (caddr u)) (rplacd u (cdddr u)))
		(t (setq u (cddr u))))
     (go ztest)
     next (cond ((null (cddr w)) (return y))
		((and (null (cddddr w)) (onep (cadddr w))
		      (alike1 (cadr w) (caddr w)))
		 (return 1)))
     again(setq z (cddr w))
     sort	(cond ((null (cddr z)) (go loop))
		      ((alike1 (car z) (caddr z))
		       (rplaca (cdddr z) (add2 (cadr z) (cadddr z)))
		       (rplacd z (cdddr z)))
		      ((great (car z) (caddr z))
		       (let ((u1 (car z)) (u2 (cadr z)) (v1 (caddr z)) (v2 (cadddr z)))
			 (setq flag t) (rplaca z v1)
			 (rplacd z (cons v2 (cons u1 (cons u2 (cddddr z))))))))
     (cond ((setq z (cddr z)) (go sort)))
     loop	(cond ((null flag) (return (cond ((null derivflag) (eqtest w x))
						 (t (deriv (cdr w)))))))
     (setq flag nil)
     (go again)))

(defmfun signum1 (x)
  (cond ((mnump x)
	 (setq x (num1 x)) (cond ((plusp x) 1) ((minusp x) -1) (t 0)))
	((atom x) 1)
	((mplusp x) (if expandp 1 (signum1 (car (last x)))))
	((mtimesp x) (if (mplusp (cadr x)) 1 (signum1 (cadr x))))
	(t 1)))

(defmfun simpsignum (x y z) 
  (oneargcheck x)
  (setq y (simpcheck (cadr x) z))
  (setq z ($csign y))
  ;; When $csign thinks y is complex, let it be.
  (cond ((memq z '($complex $imaginary)) (eqtest (list '(%signum) y) x))
	(t 
	 ;; positive * x --> x and negative * x --> -1 * x.
	 (if (mtimesp y)
	     (setq y (muln (mapcar #'(lambda (s) (let ((sgn (csign s)))
						   (cond ((eq sgn '$neg) -1)
							 ((eq sgn '$pos) 1)
							 (t s)))) (margs y)) t)))

	 (cond ((and (not ($mapatom y)) (eq (mop y) '%signum)) y) ;; signum(signum(x)) --> signum(x)
	       ((eq z '$pos) 1) 
	       ((eq z '$neg) -1) 
	       ((eq z '$zero) 0) 
	       ((great (neg y) y) (neg (take '(%signum) (neg y)))) ;; signum(x) --> -signum(-x).
	       (t (eqtest (list '(%signum) y) x))))))

(defmfun exptrl (r1 r2)
  (cond ((equal r2 1) r1)
        ((equal r2 1.0) 
         (cond ((mnump r1) (addk 0.0 r1))
               ;; Do not simplify the type of the number away.
               (t (list '(mexpt simp) r1 1.0))))
        ((equal r2 bigfloatone)
         (cond ((mnump r1) ($bfloat r1))
               ;; Do not simplify the type of the number away.
               (t (list '(mexpt simp) r1 bigfloatone))))
	((zerop1 r1)
	 (cond ((or (zerop1 r2) (mnegp r2))
		(if (not errorsw)
		    (merror "~M has been generated" (list '(mexpt) r1 r2))
		    (throw 'errorsw t)))
	       (t (zerores r1 r2))))
	((or (zerop1 r2) (onep1 r1))
	 (cond ((or ($bfloatp r1) ($bfloatp r2)) bigfloatone)
	       ((or (floatp r1) (floatp r2)) 1.0)
	       (t 1)))
	((or ($bfloatp r1) ($bfloatp r2)) ($bfloat (list '(mexpt) r1 r2)))
	((and (numberp r1) (integerp r2)) (exptb r1 r2))
	((and (numberp r1) (floatp r2) (equal r2 (float (floor r2))))
	 (exptb (float r1) (floor r2)))
	((or $numer (and (floatp r2) (or (plusp (num1 r1)) $numer_pbranch)))
	 (let (y  #+kcl(r1 r1) #+kcl(r2 r2))
	   (cond ((minusp (setq r1 (addk 0.0 r1)))
		  (cond ((or $numer_pbranch (eq $domain '$complex))
			 ;; for R1<0: R1^R2 = (-R1)^R2*cos(pi*R2) + i*(-R1)^R2*sin(pi*R2)
			 (setq r2 (addk 0.0 r2))
			 (setq y (exptrl (- r1) r2) r2 (* %pi-val r2))
			 (add2 (* y (cos r2))
			       (list '(mtimes simp) (* y (sin r2)) '$%i)))
			(t (setq y (let ($numer $float $keepfloat $ratprint)
				     (power -1 r2)))
			   (mul2 y (exptrl (- r1) r2)))))
		 ((equal (setq r2 (addk 0.0 r2)) (float (floor r2))) (exptb r1 (floor r2)))
		 ((and (equal (setq y (* 2.0 r2)) (float (floor y))) (not (equal r1 %e-val)))
		  (exptb (sqrt r1) (floor y)))
		 (t (exp (* r2 (log r1)))))))
	((floatp r2) (list '(mexpt simp) r1 r2))
	((integerp r2)
	 (cond ((minusp r2)
		(exptrl (cond ((equal (abs (cadr r1)) 1) (* (cadr r1) (caddr r1)))
			      ((minusp (cadr r1))
			       (list '(rat) (- (caddr r1)) (- (cadr r1))))
			      (t (list '(rat) (caddr r1) (cadr r1))))
			(- r2)))
	       (t (list '(rat simp) (exptb (cadr r1) r2) (exptb (caddr r1) r2)))))
	((and (floatp r1) (alike1 r2 '((rat) 1 2)))
	 (if (minusp r1)
	     (list '(mtimes simp) (sqrt (- r1)) '$%i)
	     (sqrt r1)))
	((and (floatp r1) (alike1 r2 '((rat) -1 2)))
	 (if (minusp r1)
	     (list '(mtimes simp) (/ -1.0 (sqrt (- r1))) '$%i)
	     (/ (sqrt r1))))
	((floatp r1)
	 (if (plusp r1)
	     (exptrl r1 (fpcofrat r2))
	     (mul2 (exptrl -1 r2) ;; (-4.5)^(1/4) -> (4.5)^(1/4) * (-1)^(1/4)
		   (exptrl (- r1) r2))))
	(exptrlsw (list '(mexpt simp) r1 r2))
	(t
	 (let ((exptrlsw t))
	   (simptimes (list '(mtimes)
			    (exptrl r1 (*quo (cadr r2) (caddr r2)))
			    (let ((y (let ($keepfloat $ratprint)
				       (simpnrt r1 (caddr r2))))
				  (z (rem (cadr r2) (caddr r2))))
			      (if (mexptp y)
				  (list (car y) (cadr y) (mul2 (caddr y) z))
				  (power y z))))
		      1 t)))))

(defmfun simpexpt (x y z)
  (prog (gr pot check res rulesw w mlpgr mlppot)
     (setq check x)
     (cond (z (setq gr (cadr x) pot (caddr x)) (go cont)))
     (twoargcheck x)
     (setq gr (simplifya (cadr x) nil))
     (setq pot
           (let (($%enumer $numer))
             ;; Switch $%enumer on, when $numer is TRUE to allow 
             ;; simplification of $%e to its numerical value.
             (simplifya (if $ratsimpexpons ($ratsimp (caddr x)) (caddr x))
                        nil)))
  cont  
     (cond (($ratp pot)
            (setq pot (ratdisrep pot))
            (go cont))
           (($ratp gr)
            (cond ((member 'trunc (car gr) :test #'eq)
                   (return (srf (list '(mexpt) gr pot))))
                  ((integerp pot)
                   (let ((varlist (caddar gr)) (genvar (cadddr (car gr))))
                     (return (ratrep* (list '(mexpt) gr pot)))))
                  (t
                   (setq gr (ratdisrep gr))
                   (go cont))))
           ((or (setq mlpgr (mxorlistp gr))
                (setq mlppot (mxorlistp pot)))
            (go matrix))
           ((onep1 pot) (go atgr))
           ((or (zerop1 pot) (onep1 gr)) (go retno))
           ((zerop1 gr)
            (cond ((or (mnegp pot) (and *zexptsimp? (eq ($asksign pot) '$neg)))
                   (cond ((not errorsw) (merror "Division by 0"))
                         (t (throw 'errorsw t))))
                  ((not (free pot '$%i))
                   (cond ((not errorsw)
                          (merror "0 to a complex quantity has been generated."))
                         (t (throw 'errorsw t))))
                  ((and *zexptsimp? (eq ($asksign pot) '$zero))
                   (cond ((not errorsw) (merror "0^0 has been generated"))
                         (t (throw 'errorsw t))))
                  (t (return (zerores gr pot)))))
           ((and (mnump gr)
                 (mnump pot)
                 (or (not (ratnump gr)) (not (ratnump pot))))
            (return (eqtest (exptrl gr pot) check)))
           ;; Check for numerical evaluation of the sqrt.
           ((and (alike1 pot '((rat) 1 2))
                 (or (setq res (flonum-eval '%sqrt gr))
                     (and (not (member 'simp (car x) :test #'eq))
                          (setq res (big-float-eval '%sqrt gr)))))
            (return res))
           ((eq gr '$%i)
            (return (%itopot pot)))
           ((and (numberp gr) (minusp gr) (mevenp pot))
            (setq gr (- gr))
            (go cont))
           ((and (numberp gr) (minusp gr) (moddp pot))
            (return (mul2 -1 (power (- gr) pot))))
           ((and (equal gr -1) (maxima-integerp pot) (mminusp pot))
            (setq pot (neg pot))
            (go cont))
           ((and (equal gr -1)
                 (maxima-integerp pot)
                 (mtimesp pot)
                 (= (length pot) 3)
                 (fixnump (cadr pot))
                 (oddp (cadr pot))
                 (maxima-integerp (caddr pot)))
            (setq pot (caddr pot))
            (go cont))
           ((atom gr) (go atgr))
           ((and (eq (caar gr) 'mabs)
                 (evnump pot)
                 (or (and (eq $domain '$real) (not (decl-complexp (cadr gr))))
                     (and (eq $domain '$complex) (decl-realp (cadr gr)))))
            (return (power (cadr gr) pot)))
           ((eq (caar gr) 'mequal)
            (return (eqtest (list (ncons (caar gr))
                                  (power (cadr gr) pot)
                                  (power (caddr gr) pot))
                            gr)))
           ((symbolp pot) (go opp))
           ((eq (caar gr) 'mexpt) (go e1))
           ((and (eq (caar gr) '%sum)
                 $sumexpand
                 (integerp pot)
                 (signp g pot)
                 (< pot $maxposex))
            (return (do ((i (1- pot) (1- i))
                         (an gr (simptimes (list '(mtimes) an gr) 1 t)))
                        ((signp e i) an))))
           ((equal pot -1) 
            (return (eqtest (testt (tms gr pot nil)) check)))
           ((fixnump pot)
            (return (eqtest (cond ((and (mplusp gr)
                                        (not (or (> pot $expop)
                                                 (> (- pot) $expon))))
                                   (expandexpt gr pot))
                                  (t (simplifya (tms gr pot nil) t)))
                            check))))
     
  opp
     (cond ((eq (caar gr) 'mexpt) (go e1))
           ((eq (caar gr) 'rat)
            (return (mul2 (power (cadr gr) pot)
                          (power (caddr gr) (mul2 -1 pot)))))
           ((not (eq (caar gr) 'mtimes)) (go up))
           ((or (eq $radexpand '$all) (and $radexpand (simplexpon pot)))
            (setq res (list 1))
            (go start))
           ((and (or (not (numberp (cadr gr))) (equal (cadr gr) -1))
                 (setq w (member ($num gr) '(1 -1) :test #'equal)))
            (setq pot (mult -1 pot) gr (mul2 (car w) ($denom gr)))
            (go cont))
          ((not $radexpand) (go up)))

     (return (do ((l (cdr gr) (cdr l)) (res (ncons 1)) (rad))
                 ((null l)
                  (cond ((equal res '(1))
                         (eqtest (list '(mexpt) gr pot) check))
                        ((null rad) 
                         (testt (cons '(mtimes simp) res)))
                        (t
                         (setq rad (power* ; RADEXPAND=()?
                                     (cons '(mtimes) (nreverse rad)) pot))
                         (cond ((not (onep1 rad))
                                (setq rad
                                      (testt (tms rad 1 (cons '(mtimes) res))))
                                (cond (rulesw
                                       (setq rulesw nil res (cdr rad))))))
                         (eqtest (testt (cons '(mtimes) res)) check))))
               (setq z (csign (car l)))
               (if (eq z t) (setq z '$pnz)) ; if appears complex, unknown sign
               (setq w (cond ((member z '($neg $nz) :test #'eq)
                              (setq rad (cons -1 rad))
                              (mult -1 (car l)))
                             (t (car l))))
               (cond ((onep1 w))
                     ((alike1 w gr) (return (list '(mexpt simp) gr pot)))
                     ;; not needed?        ((MEXPTP W)
                     ;;             (SETQ Z (LIST '(MEXPT) (CAR L) POT))
                     ;;             (COND ((ALIKE1 Z (SETQ Z (SIMPLIFYA Z NIL)))
                     ;;                    (SETQ RAD (CONS W RAD)))
                     ;;                   (T (SETQ W (TIMESIN Z RES 1)))))
                     ((member z '($pn $pnz) :test #'eq)
                      (setq rad (cons w rad)))
                     (t
                      (setq w (testt (tms (simplifya (list '(mexpt) w pot) t)
                                          1 (cons '(mtimes) res))))))
               (cond (rulesw (setq rulesw nil res (cdr w))))))
     
  start
     (cond ((and (cdr res) (onep1 (car res)) (ratnump (cadr res)))
            (setq res (cdr res))))
     (cond ((null (setq gr (cdr gr)))
            (return (eqtest (testt (cons '(mtimes) res)) check)))
           ((mexptp (car gr))
            (setq y (list (caar gr) (cadar gr) (mult (caddar gr) pot))))
           ((eq (car gr) '$%i)
            (setq y (%itopot pot)))
           ((mnump (car gr))
            (setq y (list '(mexpt) (car gr) pot)))
           (t (setq y (list '(mexpt simp) (car gr) pot))))
     (setq w (testt (tms (simplifya y t) 1 (cons '(mtimes) res))))
     (cond (rulesw (setq rulesw nil res (cdr w))))
     (go start)

  retno 
     (return (exptrl gr pot))
     
  atgr  
     (cond ((zerop1 pot) (go retno))
           ((onep1 pot)
            (let ((y (mget gr '$numer)))
              (if (and y (floatp y) (or $numer (not (equal pot 1))))
                  ;; A numeric constant like %e, %pi, ... and 
                  ;; exponent is a float or bigfloat value.
                  (return (if (and (member gr *builtin-numeric-constants*)
                                   (equal pot bigfloatone))
                              ;; Return a bigfloat value.
                              ($bfloat gr)
                              ;; Return a float value.
                              y))
                  ;; In all other cases exptrl simplifies accordingly.
                  (return (exptrl gr pot)))))
           ((eq gr '$%e)
            ;; Numerically evaluate if the power is a flonum.
            (when $%emode
              (let ((val (flonum-eval '%exp pot)))
                (when val
                  (return val)))
              ;; Numerically evaluate if the power is a (complex)
              ;; big-float.  (This is basically the guts of
              ;; big-float-eval, but we can't use big-float-eval.)
              (when (and (not (member 'simp (car x) :test #'eq))
                         (complex-number-p pot 'bigfloat-or-number-p))
                (let ((x ($realpart pot))
                      (y ($imagpart pot)))
                  (cond ((and ($bfloatp x) (like 0 y))
                         (return ($bfloat `((mexpt simp) $%e ,pot))))
                        ((or ($bfloatp x) ($bfloatp y))
                         (let ((z (add ($bfloat x) (mul '$%i ($bfloat y)))))
                           (setq z ($rectform `((mexpt simp) $%e ,z)))
                           (return ($bfloat z))))))))
            (cond ;; (($bfloatp pot) 
                  ;;  (return ($bfloat (list '(mexpt) '$%e pot))))
                  ;; ((or (floatp pot) (and $numer (integerp pot)))
                  ;;  (return (exp pot)))
                  ((and $logsimp (among '%log pot)) (return (%etolog pot)))
                  ((and $demoivre (setq z (demoivre pot))) (return z))
                  ((and $%emode
                        ;; Check for an expression exp(%i*%pi*p/q).
                        (mtimesp pot)
                        (or (not (mnump (cadr pot))) ; Special case p/q = 1
                            (integerp (cadr pot))    ; an integer
                            (ratnump (cadr pot)))    ; a rational
                        (setq z (%especial pot)))    ; Try to simplify.
                   (return z))
                  (($taylorp (third x))
                   ;; taylorize %e^taylor(...)
                   (return ($taylor x)))))
           (t
            (let ((y (mget gr '$numer)))
              ;; Check for a numeric constant.
              (and y
                   (floatp y)
                   (or (floatp pot)
                       ;; The exponent is a bigfloat. Convert base to bigfloat.
                       (and ($bfloatp pot)
                            (member gr *builtin-numeric-constants*)
                            (setq y ($bfloat gr)))
                       (and $numer (integerp pot)))
                   (return (exptrl y pot))))))

  up 
     (return (eqtest (list '(mexpt) gr pot) check))

  matrix
     (cond ((zerop1 pot)
            (cond ((mxorlistp1 gr) (return (constmx (addk 1 pot) gr)))
                  (t (go retno))))
           ((onep1 pot) (return gr))
           ((or $doallmxops $doscmxops $domxexpt)
            (cond ((or (and mlpgr
                            (or (not ($listp gr)) $listarith)
                            (scalar-or-constant-p pot $assumescalar))
                       (and $domxexpt
                            mlppot
                            (or (not ($listp pot)) $listarith)
                            (scalar-or-constant-p gr $assumescalar)))
                   (return (simplifya (outermap1 'mexpt gr pot) t)))
                  (t (go up))))
           ((and $domxmxops (member pot '(-1 -1.0) :test #'equal))
            (return (simplifya (outermap1 'mexpt gr pot) t)))
           (t (go up)))

  e1  
     (cond ((or (eq $radexpand '$all)
                (simplexpon pot)
                (member ($csign (cadr gr)) '($pos $pz $zero))
                (equal (caddr gr) -1)
                (and (eq $domain '$real)
                     (odnump (caddr gr))))
            (setq pot (mult pot (caddr gr)) gr (cadr gr)))
           ((and (eq $domain '$real)
                 (free gr '$%i)
                 $radexpand
                 (not (decl-complexp (cadr gr)))
                 (evnump (caddr gr)))
            (setq pot (mult pot (caddr gr)) gr (radmabs (cadr gr))))
           ((mminusp (caddr gr))
            (setq pot (neg pot)
                  gr (list (car gr) (cadr gr) (neg (caddr gr)))))
           (t (go up)))
     (go cont)))

(defun exponent-of (m base)
  ;; Basically computes log of m base b.  Except if m is not a power
  ;; of b, we return nil.

  ;; Give up on the cases we can't handle.
  (unless (and (mnump m)
	       (ratgreaterp m 0)
	       (integerp base)
	       (not (eql (abs base) 1)))
    (return-from exponent-of nil))
  (cond ((ratgreaterp 1 m)
	 ;; m < 1, so change the problem to finding the exponent for
	 ;; 1/m.
	 (let ((expo (exponent-of (inv m) base)))
	   (when expo
	     (- expo))))
	((ratnump m)
	 ;; exponent-of doesn't know how to handle maxima rationals,
	 ;; so make it a Lisp rational.
	 (exponent-of (/ (second m) (third m)) base))
	(t
	 ;; Main case.  Just compute base^k until base^k >= m.  Then
	 ;; check if they're equal.  If so, we have the exponent.
	 ;; Otherwise, give up.
	 (let ((expo 0))
	   (when (integerp m)
	     (loop
		(multiple-value-bind (q r)
		    (floor m base)
		  (cond ((zerop r)
			 (setf m q)
			 (incf expo))
			(t
			 (return nil))))))
	   (if (zerop expo)
	       nil
	       expo)))))

(defun timesin (x y w)                  ; Multiply X^W into Y
  (prog (fm temp z check u expo)
     (if (mexptp x) (setq check x))
  top
     ;; Prepare the factor x^w and initialize the work of timesin
     (cond ((equal w 1)
            (setq temp x))
           (t
            (setq temp (cons '(mexpt) (if check 
                                          (list (cadr x) (mult (caddr x) w))
                                          (list x w))))
            (if (and (not timesinp) (not (eq x '$%i)))
                (let ((timesinp t))
                  (setq temp (simplifya temp t))))))
     (setq x (if (mexptp temp)
                 (cdr temp)
                 (list temp 1)))
     (setq w (cadr x)
           fm y)
  start
     ;; Go through the list of terms in fm and look what is to do.
     (cond ((null (cdr fm))
            ;; The list of terms is empty. The loop is finshed.
            (go less))
           ((or (and (mnump temp)
                     (not (or (integerp temp)
                              (ratnump temp))))
                (and (integerp temp)
                     (equal temp -1)))
            ;; Stop the loop for a float or bigfloat number, or number -1.
            (go less))
           ((mexptp (cadr fm))
            (cond ((alike1 (car x) (cadadr fm))
                   (cond ((zerop1 (setq w (plsk (caddr (cadr fm)) w)))
                          (go del))
                         ((and (mnump w)
                               (or (mnump (car x))
                                   (eq (car x) '$%i)))
                          (rplacd fm (cddr fm))
                          (cond ((mnump (setq x (if (mnump (car x))
                                                    (exptrl (car x) w)
                                                    (power (car x) w))))
                                 (return (rplaca y (timesk (car y) x))))
                                ((mtimesp x)
                                 (go times))
                                (t
                                 (setq temp x
                                       x (if (mexptp x) (cdr x) (list x 1)))
                                 (setq w (cadr x)
                                       fm y)
                                 (go start))))
                         ((maxima-constantp (car x))
                          (go const))
                         ((onep1 w)
                          (cond ((mtimesp (car x))
                                 ;; A base which is a mtimes expression.
                                 ;; Remove the factor from the lists of products.
                                 (rplacd fm (cddr fm))
                                 ;; Multiply the factors of the base with 
                                 ;; the list of all remaining products.
                                 (setq rulesw t)
                                 (return (muln (nconc y (cdar x)) t)))
                                (t (return (rplaca (cdr fm) (car x))))))
                         (t
                          (go spcheck))))
                  ;; At this place we have to add code for a rational number
                  ;; as a factor to the list of products.
                  ((and (onep1 w)
                        (or (ratnump (car x))
                            (and (integerp (car x))
                                 (not (onep (car x))))))
                   ;; Multiplying a^k * rational.
                   (let* ((numerator (if (integerp (car x)) 
                                         (car x)
                                         (second (car x))))
                          (denom (if (integerp (car x)) 
                                     1
                                     (third (car x))))
                          (sgn (signum numerator)))
                     (setf expo (exponent-of (abs numerator) (second (cadr fm))))
                     (when expo
                       ;; We have a^m*a^k.
                       (setq temp (power (second (cadr fm)) 
                                         (add (third (cadr fm)) expo)))
                       ;; Set fm to have 1/denom term.
                       (setq x (mul sgn
                                    (car y)
                                    (div (div (mul sgn numerator)
                                              (power (second (cadr fm))
                                                     expo))
                                         denom)))
                       (setf y (rplaca y 1))
                       ;; Add in the a^(m+k) term.
                       (rplacd fm (cddr fm))
                       (rplacd fm (cons temp (cdr fm)))
                       (setq temp x
                             x (list x 1)
                             w 1
                             fm y)
                       (go start))
                     (setf expo (exponent-of (inv denom) (second (cadr fm))))
                     (when expo
                       ;; We have a^(-m)*a^k.
                       (setq temp (power (second (cadr fm)) 
                                         (add (third (cadr fm)) expo)))
                       ;; Set fm to have the numerator term.
                       (setq x (mul (car y)
                                    (div numerator
                                         (div denom
                                              (power (second (cadr fm)) 
                                                     (- expo))))))
                       (setf y (rplaca y 1))
                       ;; Add in the a^(k-m) term.
                       (rplacd fm (cddr fm))
                       (rplacd fm (cons temp (cdr fm)))
                       (setq temp x
                             x (list x 1)
                             w 1
                             fm y)
                       (go start))
                     ;; Next term in list of products.
                     (setq fm (cdr fm))
                     (go start)))
                  ((or (maxima-constantp (car x))
                       (maxima-constantp (cadadr fm)))
                   (if (great temp (cadr fm))
                       (go gr)))
                  ((great (car x) (cadadr fm))
                   (go gr)))
            (go less))
           ((alike1 (car x) (cadr fm))
            (go equ))
          ((mnump temp)
           ;; When a number goto start and look in the next term.
           (setq fm (cdr fm))
           (go start))
           ((maxima-constantp (car x))
            (when (great temp (cadr fm))
              (go gr)))
           ((great (car x) (cadr fm))
            (go gr)))
  less
     (cond ((mnump temp)
           ;; Multiply a number into the list of products.
           (return (rplaca y (timesk (car y) temp))))
           ((and (eq (car x) '$%i)
                 (fixnump w))
            (go %i))
           ((and (eq (car x) '$%e)
                 $numer
                 (integerp w))
            (return (rplaca y (timesk (car y) (exp w)))))
           ((and (onep1 w)
                 (not (constant (car x))))
            (go less1))                  
           ;; At this point we will insert a mexpt expression,
           ;; but first we look at the car of the list of products and
           ;; modify the expression if we found a rational number.
           ((and (mexptp temp)
                 (not (onep1 (car y)))
                 (or (integerp (car y))
                     (ratnump (car y))))
            ;; Multiplying x^w * rational or integer.
            (let* ((numerator (if (integerp (car y)) 
                                 (car y)
                                 (second (car y))))
                   (denom (if (integerp (car y)) 
                              1
                              (third (car y))))
                   (sgn (signum numerator)))
              (setf expo (exponent-of (abs numerator) (car x)))
              (when expo
                ;; We have a^m*a^k.
                (setq temp (power (car x)
                                  (add (cadr x) expo)))
                ;; Set fm to have 1/denom term.
                (setq x (div (div numerator
                                  (power (car x) expo))
                             denom))
                (setf y (rplaca y 1))
                ;; Add in the a^(m+k) term.
                (rplacd fm (cons temp (cdr fm)))
                (setq temp x
                      x (list x 1)
                      w 1
                      fm y)
                (go start))
              (setf expo (exponent-of (inv denom) (car x)))
              (when expo
                ;; We have a^(-m)*a^k.
                (setq temp (power (car x) 
                                  (add (cadr x) expo)))
                ;; Set fm to have the numerator term.
                (setq x (div numerator
                             (div denom
                                  (power (car x) 
                                         (- expo)))))
                (setf y (rplaca y 1))
                ;; Add in the a^(k-m) term.
                (rplacd fm (cons temp (cdr fm)))
                (setq temp x
                      x (list x 1)
                      w 1
                      fm y)
                (go start))
              ;; The rational doesn't contain any (simple) powers of
              ;; the exponential term.  We're done.
              (return (cdr (rplacd fm (cons temp (cdr fm)))))))
           ((and (maxima-constantp (car x))
                 (do ((l (cdr fm) (cdr l)))
                     ((null (cdr l)))
                   (when (and (mexptp (cadr l))
                              (alike1 (car x) (cadadr l)))
                     (setq fm l)
                     (return t))))
            (go start))
           ((or (and (mnump (car x))
                     (mnump w))
                (and (eq (car x) '$%e)
                     $%emode
                     (setq u (%especial w))))
            (setq x (cond (u)
                          ((alike (cdr check) x)
                           check)
                          (t
                           (exptrl (car x) w))))
            (cond ((mnump x)
                   (return (rplaca y (timesk (car y) x))))
                  ((mtimesp x)
                   (go times))
                  ((mexptp x)
                   (return (cdr (rplacd fm (cons x (cdr fm))))))
                  (t
                   (setq temp x
                         x (list x 1)
                         w 1
                         fm y)
                   (go start))))
           ((onep1 w)
            (go less1))
           (t
            (setq temp (list '(mexpt) (car x) w))
            (setq temp (eqtest temp (or check '((foo)))))
            (return (cdr (rplacd fm (cons temp (cdr fm)))))))
  less1
     (return (cdr (rplacd fm (cons (car x) (cdr fm)))))
  gr
     (setq fm (cdr fm))
     (go start)
  equ
     (cond ((and (eq (car x) '$%i) (equal w 1))
            (rplacd fm (cddr fm))
            (return (rplaca y (timesk -1 (car y)))))
           ((zerop1 (setq w (plsk 1 w)))
            (go del))
           ((and (mnump (car x)) (mnump w))
            (return (rplaca (cdr fm) (exptrl (car x) w))))
           ((maxima-constantp (car x))
            (go const)))
  spcheck
     (setq z (list '(mexpt) (car x) w))
     (cond ((alike1 (setq x (simplifya z t)) z)
            (return (rplaca (cdr fm) x)))
           (t
            (rplacd fm (cddr fm))
            (setq rulesw t)
            (return (muln (cons x y) t))))
  const
     (rplacd fm (cddr fm))
     (setq x (car x) check nil)
     (go top)
  times
     (setq z (tms x 1 (setq temp (cons '(mtimes) y))))
     (return (cond ((eq z temp)
                    (cdr z))
                   (t
                    (setq rulesw t) z)))
  del
     (return (rplacd fm (cddr fm)))
  %i
     (if (minusp (setq w (rem w 4)))
         (incf w 4))
     (return (cond ((zerop w)
                    fm)
                   ((= w 2)
                    (rplaca y (timesk -1 (car y))))
                   ((= w 3)
                    (rplaca y (timesk -1 (car y)))
                    (rplacd fm (cons '$%i (cdr fm))))
                   (t
                    (rplacd fm (cons '$%i (cdr fm))))))))

(defmfun simpmatrix (x vestigial z)
  (declare (ignore vestigial))
  (if (and (null (cddr x))
	   $scalarmatrixp
	   (or (eq $scalarmatrixp '$all) (member 'mult (cdar x) :test #'eq))
	   ($listp (cadr x)) (cdadr x) (null (cddadr x)))
      (simplifya (cadadr x) z)
      (let ((badp (dolist (row (cdr x)) (if (not ($listp row)) (return t))))
	    (args (simpmap (cdr x) z)))
	(if (and args (not badp)) (matcheck args))
	(cons (if badp '(%matrix simp) '($matrix simp)) args))))

(defun %itopot (pot)
  (if (fixnump pot)
      (let ((i (boole  boole-and pot 3)))
	(cond ((= i 0) 1)
	      ((= i 1) '$%i)
	      ((= i 2) -1)
	      (t (list '(mtimes simp) -1 '$%i))))
      (power -1 (mul2 pot '((rat simp) 1 2)))))

(defun mnlogp (pot)
  (cond ((eq (caar pot) '%log) (simplifya (cadr pot) nil))
	((and (eq (caar pot) 'mtimes)
	      (or (maxima-integerp (cadr pot)) (and $%e_to_numlog ($numberp (cadr pot))))
	      (not (atom (caddr pot))) (eq (caar (caddr pot)) '%log)
	      (null (cdddr pot)))
	 (power (cadr (caddr pot)) (cadr pot)))))

(defun mnlog (pot)
  (prog (a b c)
   loop (cond ((null pot)
	       (cond (a (setq a (cons '(mtimes) a))))
	       (cond (c (setq c (list '(mexpt simp) '$%e (addn c nil)))))
	       (return (cond ((null c) (simptimes a 1 nil))
			     ((null a) c)
			     (t (simptimes (append a (list c)) 1 nil)))))
	      ((and (among '%log (car pot)) (setq b (mnlogp (car pot))))
	       (setq a (cons b a)))
	      (t (setq c (cons (car pot) c))))
   (setq pot (cdr pot))
   (go loop)))

(defun %etolog (pot) (cond ((mnlogp pot))
			   ((eq (caar pot) 'mplus) (mnlog (cdr pot)))
			   (t (list '(mexpt simp) '$%e pot))))

(defun zerores (r1 r2)
  (cond ((or ($bfloatp r1) ($bfloatp r2)) bigfloatzero)
	((or (floatp r1) (floatp r2)) 0.0)
	(t 0)))

(defmfun $orderlessp (a b)
  (setq a (specrepcheck a) b (specrepcheck b))
  (and (not (alike1 a b)) (great b a) t))


(defmfun $ordergreatp (a b)
  (setq a (specrepcheck a) b (specrepcheck b))
  (and (not (alike1 a b)) (great a b) t))


(defun evnump (n) (or (even n) (and (ratnump n) (even (cadr n)))))
(defun odnump (n) (or (and (integerp n) (oddp n))
		      (and (ratnump n) (oddp (cadr n)))))

(defun simplexpon (e)
  (or (maxima-integerp e)
      (and (eq $domain '$real) (ratnump e) (oddp (caddr e)))))

;; This function is not called in Maxima core or share code
;; and can be cut out.
(defun noneg (p)
  (and (free p '$%i) (member ($sign p) '($pos $pz $zero) :test #'eq)))

(defun radmabs (e)
  (if (and limitp (free e '$%i)) (asksign-p-or-n e))
  (simplifya (list '(mabs) e) t))

(defmfun simpmqapply (exp y z)
  (let ((simpfun (and (not (atom (cadr exp))) (get (caaadr exp) 'specsimp))) u)
    (if simpfun
	(funcall simpfun exp y z)
	(progn (setq u (simpargs exp z))
	       (if (symbolp (cadr u))
		   (simplifya (cons (cons (cadr u) (cdar u)) (cddr u)) z)
		   u)))))

;; TRUE, if the symbol e is declared to be $complex or $imaginary.
(defun decl-complexp (e)
  (and (symbolp e)
       (kindp e '$complex)))

;; TRUE, if the symbol e is declared to be $integer, $rational, or $real
(defun decl-realp (e)
  (and (symbolp e)
       (or (kindp e '$real)
           (kindp e '$rational)
           (kindp e '$integer))))

;; WARNING:  Exercise extreme caution when modifying this function!
;;
;; Richard Fateman and Stavros Macrakis both say that changing the
;; actual ordering relations (as opposed to making them faster to
;; determine) could have very subtle and wide-ranging effects.  Also,
;; the simplifier spends the vast majority of its time here, so be
;; very careful about changes that may drastically slow down the
;; simplifier.
(defmfun great (x y)
  (cond ((atom x)
	 (cond ((atom y)
		(cond ((numberp x)
		       (cond ((numberp y)
			      (setq y (- x y))
			      (cond ((zerop y) (floatp x)) (t (plusp y))))))
		      ((constant x)
		       (cond ((constant y) (alphalessp y x)) (t (numberp y))))
		      ((mget x '$scalar)
		       (cond ((mget y '$scalar) (alphalessp y x)) (t (maxima-constantp y))))
		      ((mget x '$mainvar)
		       (cond ((mget y '$mainvar) (alphalessp y x)) (t t)))
		      (t (or (maxima-constantp y) (mget y '$scalar)
			     (and (not (mget y '$mainvar)) (alphalessp y x))))))
	       (t (not (ordfna y x)))))
	((atom y) (ordfna x y))
	((eq (caar x) 'rat)
	 (cond ((eq (caar y) 'rat)
		(> (* (caddr y) (cadr x)) (* (caddr x) (cadr y))))))
	((eq (caar y) 'rat))
	((member (caar x) '(mbox mlabox) :test #'eq) (great (cadr x) y))
	((member (caar y) '(mbox mlabox) :test #'eq) (great x (cadr y)))
	((or (member (caar x) '(mtimes mplus mexpt %del) :test #'eq)
	     (member (caar y) '(mtimes mplus mexpt %del) :test #'eq))
	 (ordfn x y))
	((and (eq (caar x) 'bigfloat) (eq (caar y) 'bigfloat)) (mgrp x y))
	(t (do ((x1 (margs x) (cdr x1)) (y1 (margs y) (cdr y1))) (())
	     (cond ((null x1)
		    (return (cond (y1 nil)
				  ((not (alike1 (mop x) (mop y)))
				   (great (mop x) (mop y)))
				  ((member 'array (cdar x) :test #'eq) t))))
		   ((null y1) (return t))
		   ((not (alike1 (car x1) (car y1)))
		    (return (great (car x1) (car y1)))))))))

;; Trivial function used only in ALIKE1.  Should be defined as an open-codable subr.

(defmacro memqarr (l)
  `(if (member 'array ,l :test #'eq) t))

;; Compares two Macsyma expressions ignoring SIMP flags and all other
;; items in the header except for the ARRAY flag.

(defmfun alike1 (x y)
  (cond ((eq x y))
	((atom x)
     (cond
       ((arrayp x)
	(and (arrayp y) (lisp-array-alike1 x y)))

    ;; NOT SURE IF WE WANT TO ENABLE COMPARISON OF MAXIMA ARRAYS
    ;; ASIDE FROM THAT, ADD2LNC CALLS ALIKE1 (VIA MEMALIKE) AND THAT CAUSES TROUBLE
    ;; ((maxima-declared-arrayp x)
    ;;  (and (maxima-declared-arrayp y) (maxima-declared-array-alike1 x y)))
    ;; ((maxima-undeclared-arrayp x)
    ;;  (and (maxima-undeclared-arrayp y) (maxima-undeclared-array-alike1 x y)))

       (t (equal x y))))
	((atom y) nil)
	(t (and (not (atom (car x)))
		(not (atom (car y)))
		(eq (caar x) (caar y))
		(eq (memqarr (cdar x)) (memqarr (cdar y)))
		(alike (cdr x) (cdr y))))))

(defun lisp-array-alike1 (x y)
  (and
    (equal (array-dimensions x) (array-dimensions y))
    (progn
      (dotimes (i (array-total-size x))
	(if (not (alike1 (row-major-aref x i) (row-major-aref y i)))
	  (return-from lisp-array-alike1 nil)))
      t)))

(defun maxima-declared-array-alike1 (x y)
  (lisp-array-alike1 (get (mget x 'array) 'array) (get (mget y 'array) 'array)))

(defun maxima-undeclared-array-alike1 (x y)
  (and
    (alike1 (mfuncall '$arrayinfo x) (mfuncall '$arrayinfo y))
    (alike1 ($listarray x) ($listarray y))))

;; Maps ALIKE1 down two lists.

(defmfun alike (x y)
  (do ((x x (cdr x)) (y y (cdr y))) ((atom x) (equal x y))
    (cond ((or (atom y) (not (alike1 (car x) (car y))))
	   (return nil)))))

(defun ordfna (e a)			; A is an atom
  (cond ((numberp a)
	 (or (not (eq (caar e) 'rat))
	     (> (cadr e) (* (caddr e) a))))
	((and (constant a) (not (member (caar e) '(mplus mtimes mexpt) :test #'eq)))
	 (not (member (caar e) '(rat bigfloat) :test #'eq)))
	((null (margs e)) nil)
	((eq (caar e) 'mexpt)
	 (cond ((and (maxima-constantp (cadr e))
		     (or (not (constant a)) (not (maxima-constantp (caddr e)))))
		(or (not (free (caddr e) a)) (great (caddr e) a)))
	       ((eq (cadr e) a) (great (caddr e) 1))
	       (t (great (cadr e) a))))
	((member (caar e) '(mplus mtimes) :test #'eq)
	 (let ((u (car (last e))))
	   (cond ((eq u a) (not (ordhack e))) (t (great u a)))))
	((eq (caar e) '%del))
	((prog2 (setq e (car (margs e)))
	     (and (not (atom e)) (member (caar e) '(mplus mtimes) :test #'eq)))
	 (let ((u (car (last e)))) (or (eq u a) (great u a))))
	((eq e a))
	(t (great e a))))

(defun ordlist (a b cx cy)
  (prog (l1 l2 c d)
     (setq l1 (length a) l2 (length b))
     loop (cond ((= l1 0)
		 (return (cond ((= l2 0) (eq cx 'mplus))
			       ((and (eq cx cy) (= l2 1))
				(great (cond ((eq cx 'mplus) 0) (t 1)) (car b))))))
		((= l2 0) (return (not (ordlist b a cy cx)))))
     (setq c (nthelem l1 a) d (nthelem l2 b))
     (cond ((not (alike1 c d)) (return (great c d))))
     (setq l1 (1- l1) l2 (1- l2))
     (go loop)))

(defun ordfn (x y)
  (let ((cx (caar x)) (cy (caar y)) u)
    (cond ((eq cx '%del) (if (eq cy '%del) (great (cadr x) (cadr y)) t))
	  ((eq cy '%del) nil)
	  ((member cx '(mplus mtimes) :test #'eq)
	   (cond ((member cy '(mplus mtimes) :test #'eq) (ordlist (cdr x) (cdr y) cx cy))
		 ((alike1 (setq u (car (last x))) y) (not (ordhack x)))
		 ((and (eq cx 'mplus) (eq cy 'mexpt) (mplusp (cadr y)))
		  (not (ordmexpt y x)))
		 (t (great u y))))
	  ((member cy '(mplus mtimes) :test #'eq) (not (ordfn y x)))
	  ((eq cx 'mexpt) (ordmexpt x y))
	  (t (not (ordmexpt y x))))))	; (EQ CY 'MEXPT)

(defun ordhack (x)
  (if (and (cddr x) (null (cdddr x)))
      (great (if (eq (caar x) 'mplus) 0 1) (cadr x))))

(defun ordmexpt (x y)
  (cond ((eq (caar y) 'mexpt)
	 (cond ((alike1 (cadr x) (cadr y)) (great (caddr x) (caddr y)))
	       ((maxima-constantp (cadr x))
		(if (maxima-constantp (cadr y))
		    (if (or (alike1 (caddr x) (caddr y))
			    (and (mnump (caddr x)) (mnump (caddr y))))
			(great (cadr x) (cadr y))
			(great (caddr x) (caddr y)))
		    (great x (cadr y))))
	       ((maxima-constantp (cadr y)) (great (cadr x) y))
	       ((mnump (caddr x))
		(great (cadr x) (if (mnump (caddr y)) (cadr y) y)))
	       ((mnump (caddr y)) (great x (cadr y)))
	       (t (let ((x1 (simpln1 x)) (y1 (simpln1 y)))
		    (if (alike1 x1 y1) (great (cadr x) (cadr y))
			(great x1 y1))))))
	((maxima-constantp (cadr x))
	 (if (alike1 (caddr x) y) t (great (caddr x) y)))
	((alike1 (cadr x) y) (great (caddr x) 1))
	((mnump (caddr x)) (great (cadr x) y))
	(t (great (simpln1 x) (simpln (list '(%log) y) 1 t)))))

(defmfun $multthru (e1 &optional e2)
  (let (arg1 arg2)
    (cond (e2				;called with two args
	   (setq arg1 (specrepcheck e1)
		 arg2 (specrepcheck e2))
	   (cond ((or (atom arg2) (not (member (caar arg2) '(mplus mequal) :test #'eq)))
		  (mul2 arg1 arg2))
		 ((eq (caar arg2) 'mequal)
		  (list (car arg2) ($multthru arg1 (cadr arg2))
			($multthru arg1 (caddr arg2))))
		 (t (expandterms arg1 (cdr arg2)))))
	  (t 				;called with only one arg
	   (prog (l1)
	      (setq arg1 (setq arg2 (specrepcheck e1)))
	      (cond ((atom arg1) (return arg1))
		    ((eq (caar arg1) 'mnctimes)
		     (setq arg1 (cdr arg1)) (go nct))
		    ((not (eq (caar arg1) 'mtimes)) (return arg1)))
	      (setq arg1 (reverse (cdr arg1)))
	      times (when (mplusp (car arg1))
		      (setq l1 (nconc l1 (cdr arg1)))
		      (return (expandterms (muln l1 t) (cdar arg1))))
	      (setq l1 (cons (car arg1) l1))
	      (setq arg1 (cdr arg1))
	      (if (null arg1) (return arg2))
	      (go times)
	      nct  (when (mplusp (car arg1))
		     (setq l1 (nreverse l1))
		     (return (addn (mapcar
				    #'(lambda (u)
					(simplifya
					 (cons '(mnctimes) (append l1 (ncons u) (cdr arg1)))
					 t))
				    (cdar arg1))
				   t)))
	      (setq l1 (cons (car arg1) l1))
	      (setq arg1 (cdr arg1))
	      (if (null arg1) (return arg2))
	      (go nct))))))

;;  EXPANDEXPT computes the expansion of (x1 + x2 + ... + xm)^n
;;  taking a sum and integer power as arguments.
;;  Its theory is to recurse down the binomial expansion of
;;  (x1 + (x2 + x3 + ... + xm))^n using the Binomial Expansion
;;  Thus it does a sigma:
;;
;;                n
;;             -------
;;              \         / n \    k                     (n - k)
;;               >        |   |  x1  (x2 + x3 + ... + xm)
;;              /         \ k /
;;             -------
;;               k=0
;;
;;   The function EXPONENTIATE-SUM does this and recurses through the second
;;   sum raised to a power.  It takes a list of terms and a positive integer
;;   power as arguments.


(defun expandexpt (sum power)
  (declare (fixnum power))
  (let ((expansion (exponentiate-sum (cdr sum) (abs power))))
    (cond ((plusp power) expansion)
	  (t `((mexpt simp) ,expansion -1)))))

(defun exponentiate-sum (terms rpower)
  (declare (fixnum rpower))
  (cond ((= rpower 0) 1)
	((null (cdr terms)) (power (car terms) rpower))
	((= rpower 1) (cons '(mplus simp) terms))
	(t (do ((i 0 (1+ i))
		(result 0 (add2 result
				(muln (list (combination rpower i)
					    (exponentiate-sum (cdr terms)
							      (- rpower i))
					    (power (car terms) i)) t))))
	       ((> i rpower) result)
	     (declare (fixnum i))))))

;;  Computes the combination of n elements taken m at a time by the formula
;;
;;     (n * (n-1) * ... * (n - m + 1)) / m! =
;;	(n / 1) * ((n - 1) / 2) * ... * ((n - m + 1) / m)
;;
;;  Checks for the case when m is greater than n/2 and translates
;;  to an equivalent expression.

(defun combination (n m)
  (declare (fixnum n m))
  (cond ((> m (truncate n 2))
	 (combination n (- n m)))
	(t
	 (do ((result 1 (truncate (* result n1) m1))
	      (n1 n (1- n1))
	      (m1 1 (1+ m1)))
	     ((> m1 m) result)
	   (declare (fixnum  n1 m1))))))

(defun expandsums (a b)
  (addn (prog (c)
	   (setq a (fixexpand a) b (cdr b))
	   loop
	   (when (null a) (return c))
	   (setq c (cons (expandterms (car a) b) c))
	   (setq a (cdr a))
	   (go loop))
	t))

(defun expandterms (a b)
  (addn (prog (c)
	 loop
	 (when (null b) (return c))
	 (setq c (cons (mul2 a (car b)) c))
	 (setq b (cdr b))
	 (go loop))
	t))

(defun genexpands (l)
  (prog ()
   loop
   (setq l (cdr l))
   (cond ((null l)
	  (setq prods (nreverse prods)
		negprods (nreverse negprods)
		sums (nreverse sums)
		negsums (nreverse negsums))
	  (return nil))
	 ((atom (car l))
	  (push (car l) prods))
	 ((eq (caaar l) 'rat)
	  (unless (equal (cadar l) 1)
	    (push (cadar l) prods))
	  (push (caddar l) negprods))
	 ((eq (caaar l) 'mplus)
	  (push (car l) sums))
	 ((and (eq (caaar l) 'mexpt)
	       (equal (caddar l) -1) (mplusp (cadar l)))
	  (push (cadar l) negsums))
	 ((and (eq (caaar l) 'mexpt)
	       (let ((expandp t))
		 (mminusp (caddar l))))
	  (push (if (equal (caddar l) -1)
		    (cadar l)
		    (list (caar l) (cadar l) (neg (caddar l))))
		negprods))
	 (t
	  (push (car l) prods)))
   (go loop)))

(defun expandtimes (a)
  (prog (prods negprods sums negsums expsums expnegsums)
     (genexpands a)
     (setq prods (cond ((null prods) 1)
		       ((null (cdr prods)) (car prods))
		       (t (cons '(mtimes simp) prods))))
     (setq negprods (cond ((null negprods) 1)
			  ((null (cdr negprods)) (car negprods))
			  (t (cons '(mtimes simp) negprods))))
     (cond ((null sums) (go down))
	   (t (setq expsums (car sums))
	      (mapc #'(lambda (c)
			(setq expsums (expandsums expsums c)))
		    (cdr sums))))
     (setq prods (cond ((equal prods 1) expsums)
		       (t (expandterms prods (fixexpand expsums)))))
     down (cond ((null negsums)
		 (cond ((equal 1 negprods) (return prods))
		       ((mplusp prods) (return (expandterms (power negprods -1) (cdr prods))))
		       (t (return (let ((expandflag t))
				    (mul2 prods (power negprods -1)))))))
		(t
		 (setq expnegsums (car negsums))
		 (mapc #'(lambda (c)
			   (setq expnegsums (expandsums expnegsums c)))
		       (cdr negsums))))
     (setq expnegsums (expandterms negprods (fixexpand expnegsums)))
     (return (if (mplusp prods)
		 (expandterms (list '(mexpt simp) expnegsums -1) (cdr prods))
		 (let ((expandflag t))
		   (mul2 prods (list '(mexpt simp) expnegsums -1)))))))

(defmfun expand1 (exp $expop $expon)
  (unless (and (integerp $expop) (> $expop -1))
    (merror "Maxposex must be a non-negative-integer: ~%~M" $expop))
  (unless (and (integerp $expon) (> $expon -1))
    (merror "Maxnegex must be a non-negative-integer: ~%~M" $expon))
  (ssimplifya (specrepcheck exp)))

(defmfun $expand (exp &optional (expop $maxposex) (expon $maxnegex))
  (expand1 exp expop expon))

(defun fixexpand (a)
  (if (not (mplusp a))
      (ncons a)
      (cdr a)))

(defmfun simpnrt (x *n)			; computes X^(1/*N)
  (prog (*in *out varlist genvar $factorflag $dontfactor)
     (setq $factorflag t)
     (newvar x)
     (setq x (ratrep* x))
     (when (equal (cadr x) 0) (return 0))
     (setq x (ratfact (cdr x) 'psqfr))
     (simpnrt1 (mapcar #'pdis x))
     (setq *out (if *out (muln *out nil) 1))
     (setq *in (cond (*in
		      (setq *in (muln *in nil))
		      (nrthk *in *n))
		     (t 1)))
     (return (let (($%emode t))
	       (simplifya (list '(mtimes) *in *out)
			  (not (or (atom *in)
				   (atom (cadr *in))
				   (member (caaadr *in) '(mplus mtimes rat) :test #'eq))))))))

(defun simpnrt1 (x)
  (do ((x x (cddr x)) (y))
      ((null x))
    (cond ((not (equal 1 (setq y (gcd (cadr x) *n))))
	   (push (simpnrt (list '(mexpt) (car x) (quotient (cadr x) y))
			  (quotient *n y))
		 *out))
	  ((and (equal (cadr x) 1) (integerp (car x)) (plusp (car x))
		(setq y (pnthrootp (car x) *n)))
	   (push y *out))
	  (t
	   (unless (> *n (abs (cadr x)))
	     (push (list '(mexpt) (car x) (quotient (cadr x) *n)) *out))
	   (push (list '(mexpt) (car x) (rem (cadr x) *n)) *in)))))

(defun nrthk (in *n)
  (cond ((equal in 1)
	 1)
	((equal in -1)
	 (cond ((equal *n 2)
		'$%i)
	       ((eq $domain '$real)
		(if (even *n)
		    (nrthk2 -1 *n)
		    -1))
	       ($m1pbranch
		(let (($%emode t))
		  (power* '$%e (list '(mtimes) (list '(rat) 1 *n) '$%pi '$%i))))
	       (t
		(nrthk2 -1 *n))))
	((or (and wflag (eq ($asksign in) '$neg))
	     (and (mnump in) (equal ($sign in) '$neg)))
	 (nrthk1 (mul2* -1 in) *n))
	(t
	 (nrthk2 in *n))))

(defun nrthk1 (in *n)			; computes (-IN)^(1/*N)
  (if $radexpand
      (mul2 (nrthk2 in *n) (nrthk -1 *n))
      (nrthk2 (mul2* -1 in) *n)))

(defun nrthk2 (in *n)
  (power* in (list '(rat) 1 *n)))	; computes IN^(1/*N)

;; The following was formerly in SININT.  This code was placed here because
;; SININT is now an out-of-core file on MC, and this code is needed in-core
;; because of the various calls to it. - BMT & JPG

(declare-top (special var $ratfac ratform context))

(defmfun $integrate (expr x &optional lo hi)
  (let ($ratfac)
    (if (not hi)
	(with-new-context (context)
	  (if (member '%risch nounl :test #'eq)
	      (rischint expr x)
	      (sinint expr x)))
	($defint expr x lo hi))))

(defmfun ratp (a var)
  (cond ((atom a) t)
	((member (caar a) '(mplus mtimes) :test #'eq)
	 (do ((l (cdr a) (cdr l))) ((null l) t)
	   (or (ratp (car l) var) (return nil))))
	((eq (caar a) 'mexpt)
	 (if (free (cadr a) var)
	     (free (caddr a) var)
	     (and (integerp (caddr a)) (ratp (cadr a) var))))
	(t (free a var))))

(defmfun ratnumerator (r)
  (cond ((atom r) r)
	((atom (cdr r)) (car r))
	((numberp (cadr r)) r)
	(t (car r))))

(defmfun ratdenominator (r)
  (cond ((atom r) 1)
	((atom (cdr r)) (cdr r))
	((numberp (cadr r)) 1)
	(t (cdr r))))

(declare-top (special var))

(defun bprog (r s)
  (prog (p1b p2b coef1r coef2r coef1s coef2s f1 f2 a egcd)
     (setq r (ratfix r))
     (setq s (ratfix s))
     (setq coef2r (setq coef1s 0))
     (setq coef2s (setq coef1r 1))
     (setq a 1 egcd 1)
     (setq p1b (car r))
     (unless (zerop (pdegree p1b var)) (setq egcd (pgcdexpon p1b)))
     (setq p2b (car s))
     (unless (or (zerop (pdegree p2b var)) (= egcd 1))
       (setq egcd (gcd egcd (pgcdexpon p2b)))
       (setq p1b (pexpon*// p1b egcd nil)
	     p2b (pexpon*// p2b egcd nil)))
     b1   (cond ((< (pdegree p1b var) (pdegree p2b var))
		 (exch p1b p2b)
		 (exch coef1r coef2r)
		 (exch coef1s coef2s)))
     (when (zerop (pdegree p2b var))
       (unless (zerop (pdegree coef2r var))
	 (setq coef2r (pexpon*// coef2r egcd t)))
       (unless (zerop (pdegree coef2s var))
	 (setq coef2s (pexpon*// coef2s egcd t)))
       (return (cons (ratreduce (ptimes (cdr r) coef2r) p2b)
		     (ratreduce (ptimes (cdr s) coef2s) p2b))))
     (setq f1 (psquorem1 (cdr p1b) (cdr p2b) t))
     (setq f2 (psimp var (cadr f1)))
     (setq p1b (pquotientchk (psimp var (caddr f1)) a))
     (setq f1 (car f1))
     (setq coef1r (pquotientchk (pdifference (ptimes f1 coef1r)
					     (ptimes f2 coef2r))
				a))
     (setq coef1s (pquotientchk (pdifference (ptimes f1 coef1s)
					     (ptimes f2 coef2s))
				a))
     (setq a f1)
     (go b1)))

(defmfun ratdifference (a b) (ratplus a (ratminus b)))

(defmfun ratpl (a b) (ratplus (ratfix a) (ratfix b)))

(defmfun ratti (a b c) (rattimes (ratfix a) (ratfix b) c))

(defmfun ratqu (a b) (ratquotient (ratfix a) (ratfix b)))

(defmfun ratfix (a) (cond ((equal a (ratnumerator a)) (cons a 1)) (t a)))

(defmfun ratdivide (f g)
  (destructuring-let* (((fnum . fden) (ratfix f))
		       ((gnum . gden) (ratfix g))
		       ((q r) (pdivide fnum gnum)))
    (cons (ratqu (ratti q gden t) fden)
	  (ratqu r fden))))

(defmfun polcoef (l n) (cond ((or (atom l) (pointergp var (car l)))
			      (cond ((equal n 0) l) (t 0)))
			     (t (pterm (cdr l) n))))

(defun disrep (l) (cond ((equal (ratnumerator l) l)
			 ($ratdisrep (cons ratform (cons l 1))))
			(t ($ratdisrep (cons ratform l)))))

(declare-top (unspecial var))


;; The following was formerly in MATRUN.  This code was placed here because
;; MATRUN is now an out-of-core file on MC, and this code is needed in-core
;; so that MACSYMA SAVE files will work. - JPG

(defvar *afterflag nil)

(defmfun matcherr ()
  (throw 'match nil))

(defmfun kar (x) (if (atom x) (matcherr) (car x)))

(defmfun kaar (x) (kar (kar x)))

(defmfun kdr (x) (if (atom x) (matcherr) (cdr x)))

(defmfun simpargs1 (a vestigial c)
  (declare (ignore vestigial))
  (simpargs a c))

(defmfun *kar (x)
  (if (not (atom x)) (car x)))

(defquote retlist (&rest l)
  (cons '(mlist simp)
	(mapcar #'(lambda (z) (list '(mequal simp) z (meval z))) l)))

(defmfun nthkdr (x c)
  (if (zerop c) x (nthkdr (kdr x) (1- c))))
