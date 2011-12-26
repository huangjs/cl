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

(macsyma-module trgred)

(declare-top (special var *n *a *sp1logf splist *var usexp $verbose ans *trigred
		      *noexpand sc^ndisp *lin *trig laws triglaws hyperlaws
		      $trigexpand trigbuckets hyperbuckets half%pi
		      trans-list-plus $ratprint $keepfloat))

(load-macsyma-macros rzmac)

;;The Trigreduce file contains a group of routines which can be used to
;;make trigonometric simplifications of expressions.  The bulk of the
;;routines here involve the reductions of products of sin's and cos's.
;;
;;	*TRIGRED	indicates that the special simplifications for
;;			$TRIGREDUCE are to be used.
;;	*NOEXPAND	indicates that trig functions of sums of
;;			angles are not to be used.

(defmfun $trigreduce (exp &optional (var '*novar))
  (let ((*trigred t) (*noexpand t) $trigexpand $verbose $ratprint)
    (gcdred (sp1 exp))))

(defun sp1 (e)
  (cond ((atom e) e)
	((eq (caar e) 'mplus)
	 (do ((l trans-list-plus (cdr l)) (a))
	     ((null l) (m+l (mapcar 'sp1 (cdr e))))
	   (and (setq a (m2 e (caar l) nil))
		(return (sp1 (sch-replace a (cadar l)))))))
	((eq (caar e) 'mtimes)
	 (sp1times e))
	((eq (caar e) 'mexpt)
	 (sp1expt (sp1 (cadr e)) (sp1 (caddr e))))
	((eq (caar e) '%log)
	 (sp1log (sp1 (cadr e))))
	((member (caar e) '(%cos %sin %tan %cot %sec %csc
			  %cosh %sinh %tanh %coth %sech %csch) :test #'eq)
	 (sp1trig (list (car e) (let* ((*noexpand t)) (sp1 (cadr e))))))
	((member (caar e) '(%asin %acos %atan %acot %asec %acsc
			  %asinh %acosh %atanh %acoth %asech %acsch) :test #'eq)
	 (sp1atrig (caar e) (let* ((*noexpand t)) (sp1 (cadr e)))))
	((eq (caar e) 'mrat) (sp1 (ratdisrep e)))
	((mbagp e)
	 (cons (list (caar e)) (mapcar #'(lambda (u)
					   (gcdred (sp1 u)))
				       (cdr e))))
	((eq (caar e) '%integrate)
	 (list* '(%integrate) (sp1 (cadr e)) (cddr e)))
	(t e)))

(setq trans-list-plus
      '( (((mplus) ((coeffpt) (c true) ((mexpt) ((%tan) (x true)) 2))
	   (var* (uvar) c))
	  ((mtimes) c ((mexpt) ((%sec) x) 2)))
	(((mplus) ((coeffpt) (c true) ((mexpt) ((%cot) (x true)) 2))
	  (var* (uvar) c))
	 ((mtimes) c ((mexpt) ((%csc) x) 2)))
	(((mplus) ((coeffpt) (c true) ((mexpt) ((%tanh) (x true)) 2))
	  ((mtimes) -1 (var* (uvar) c)))
	 ((mtimes) -1 c ((mexpt) ((%sech) x) 2)))
	(((mplus) ((coeffpt) (c true) ((mexpt) ((%coth) (x true)) 2))
	  ((mtimes) -1 (var* (uvar) c)))
	 ((mtimes) c ((mexpt) ((%csch) x) 2))) ))

(defun trigfp (e) (or (and (not (atom e)) (trigp (caar e))) (equal e 1)))

(defun gcdred (e)
  (cond ((atom e) e)
	((eq (caar e) 'mplus) (m+l (mapcar 'gcdred (cdr e))))
	((eq (caar e) 'mtimes)
	 (let* ((nn
		 '(1))( nd '(1))( gcd nil))
	   (do ((e (cdr e) (cdr e)))
	       ((null e)
		(setq nn (m*l nn) nd (m*l nd)))
	     (cond ((and (mexptp (car e))
			 (or (signp l (caddar e))
			     (and (mtimesp (caddar e))
				  (signp l (cadr (caddar e))))))
		    (setq nd (cons (m^ (cadar e) (m- (caddar e))) nd)))
		   ((ratnump (car e))
		    (setq nn (cons (cadar e) nn)
			  nd (cons (caddar e) nd)))
		   ((setq nn (cons (car e) nn)))))
	   (cond ((equal nd 1) nn)
		 ((equal (setq gcd ($gcd nn nd)) 1) e)
		 ((div* (cadr ($divide nn gcd))
			(cadr ($divide nd gcd)))))))
	(t e)))

(defun sp1times (e)
  (let* ((fr
	  nil)( g '(1))( trigbuckets nil)( hyperbuckets nil)( tr nil)( hyp nil)( *lin '(0)))
    (do ((e (cdr e) (cdr e)))
	((null e) (setq g (mapcar 'sp1 g)))
      (cond ((or (mnump (car e))
		 (and (not (eq var '*novar)) (free (car e) var)))
	     (setq fr (cons (car e) fr)))
	    ((atom (car e)) (setq g (cons (car e) g)))
	    ((or (trigfp (car e))
		 (and (eq (caaar e) 'mexpt) (trigfp (cadar e))))
	     (sp1add (car e)))
	    ((setq g (cons (car e) g)))))
    (mapcar #'(lambda (q)  (sp1sincos q t)) trigbuckets)
    (mapcar #'(lambda (q) (sp1sincos q nil)) hyperbuckets)
    (setq fr (cons (m^ (1//2) (m+l *lin)) fr)
	  *lin nil)
    (setq tr (cons '* (mapcan 'sp1untrep trigbuckets)))
    (setq g (nconc (sp1tlin tr t) (sp1tplus *lin t) g)
	  *lin nil)
    (setq hyp (cons '* (mapcan 'sp1untrep hyperbuckets)))
    (setq g (nconc (sp1tlin hyp nil) (sp1tplus *lin nil) g))
    (setq g ($expand (let* (($keepfloat t)) ($ratsimp (cons '(mtimes) g)))))
    (cond ((mtimesp g) (setq g (mapcar 'sp1 (cdr g))))
	  ((setq g (list (sp1 g)))))
    (m*l (cons 1 (nconc g fr (cdr tr) (cdr hyp))))))

(setq triglaws
      '(* %sin (* %cot %cos %sec %tan) %cos (* %tan %sin %csc %cot)
	%tan (* %cos %sin %csc %sec) %cot (* %sin %cos %sec %csc)
	%sec (* %sin %tan %cot %csc) %csc (* %cos %cot %tan %sec)))

(setq hyperlaws
      '(* %sinh (* %coth %cosh %sech %tanh) %cosh (* %tanh %sinh %csch %coth)
	%tanh (* %cosh %sinh %csch %sech) %coth (* %sinh %cosh %sech %csch)
	%sech (* %sinh %tanh %coth %csch) %csch (* %cosh %coth %tanh %sech)))

(defun sp1tlin (l *trig) (sp1tlin1 l))

(defun sp1tlin1 (l)
  (cond ((null (cdr l)) nil)
	((and (eq (caaadr l) 'mexpt)
	      (integerp (caddr (cadr l)))
	      (member (caaadr (cadr l))
		    (if *trig '(%sin %cos) '(%sinh %cosh)) :test #'eq))
	 (cons (funcall (cdr (assoc (caaadr (cadr l)) sc^ndisp :test #'eq))
			(caddr (cadr l)) (cadadr (cadr l)))
	       (sp1tlin1 (rplacd l (cddr l)))))
	((member (caaadr l) (if *trig '(%sin %cos) '(%sinh %cosh)) :test #'eq)
	 (setq *lin (cons (cadr l) *lin))
	 (sp1tlin1 (rplacd l (cddr l))))
	((sp1tlin1 (cdr l)))))

(defun sp1tplus (l *trig)
  (cond ((or (null l) (null (cdr l))) l)
	((do ((c (list '(rat) 1 (expt 2 (1- (length l)))))
	      (ans (list (car l)))
	      (l (cdr l) (cdr l)))
	     ((null l) (list c (m+l ans)))
	   (setq ans
		 (m+l
		  (mapcar #'(lambda (q)
			      (cond ((mtimesp q)
				     (m* (cadr q) (sp1sintcos (caddr q) (car l))))
				    ((sp1sintcos q (car l)))))
			  ans)))
	   (setq ans (cond ((mplusp ans) (cdr ans)) (t (ncons ans))))))))

(defun sp1sintcos (a b)
  (let* ((x
	  nil)( y nil))
    (cond ((or (atom a) (atom b)
	       (not (member (caar a) '(%sin %cos %sinh %cosh) :test #'eq))
	       (not (member (caar b) '(%sin %cos %sinh %cosh) :test #'eq)))
	   (mul3 2 a b))
	  ((prog2 (setq x (m+ (cadr a) (cadr b)) y (m- (cadr a) (cadr b)))
	       (null (eq (caar a) (caar b))))
	   (setq b (if *trig '(%sin) '(%sinh)))
	   (or (eq (caar a) '%sin) (eq (caar a) '%sinh)
	       (setq y (m- y)))
	   (m+ (list b x) (list b y)))
	  ((member (caar a) '(%cos %cosh) :test #'eq)
	   (m+ (list (list (caar a)) x)
	       (list (list (caar a)) y)))
	  (*trig
	   (m- (list '(%cos) y) (list '(%cos) x)))
	  ((m- (list '(%cosh) x) (list '(%cosh) y))))))

;; For COS(X)^2, TRIGBUCKET is (X (1 (COS . 2))) or, more generally,
;; (arg (numfactor-of-arg (operator . exponent)))

(defun sp1add (e)
  (let* ((n
	  (cond ((eq (caar e) 'mexpt)
		 (cond ((= (signum1 (caddr e)) -1)
			(prog1 (m- (caddr e))
			  (setq e (cons (list (oldget (caaadr e) 'recip)) (cdadr e)))))
		       ((prog1 (caddr e) (setq e (cadr e))))))
		( 1 )))( arg
			(sp1kget (cadr e)))( buc nil)( laws hyperlaws))
    (cond ((member (caar e) '(%sin %cos %tan %cot %sec %csc) :test #'eq)
	   (cond ((setq buc (assoc (cdr arg) trigbuckets :test #'equal))
		  (setq laws triglaws)
		  (sp1addbuc (caar e) (car arg) n buc))
		 ((setq trigbuckets
			(cons (list (cdr arg) (list (car arg) (cons (caar e) n)))
			      trigbuckets)))))
	  ((setq buc (assoc (cdr arg) hyperbuckets :test #'equal))
	   (sp1addbuc (caar e) (car arg) n buc))
	  ((setq hyperbuckets
		 (cons (list (cdr arg) (list (car arg) (cons (caar e) n)))
		       hyperbuckets))))))

(defun sp1addbuc (f arg n b) ;FUNCTION, ARGUMENT, EXPONENT, BUCKET LIST
  (cond ((and (cdr b) (alike1 arg (caadr b))) ;GOES IN THIS BUCKET
	 (sp1putbuc f n (cadr b)))
	((or (null (cdr b)) (great (caadr b) arg))
	 (rplacd b (cons (list arg (cons f n)) (cdr b))))
	((sp1addbuc f arg n (cdr b)))))

(defun sp1putbuc (f n *buc)		;PUT IT IN THERE
  (do ((buc *buc (cdr buc)))
      ((null (cdr buc))
       (rplacd buc (list (cons f n))))
    (cond ((eq f (caadr buc))		;SAME FUNCTION
	   (return
	     (rplacd (cadr buc) (m+ n (cdadr buc))))) ;SO BOOST EXPONENT
	  ((eq (caadr buc) (oldget f 'recip)) ;RECIPROCAL FUNCTIONS
	   (setq n (m- (cdadr buc) n))
	   (return
	     (cond ((signp e n) (rplacd buc (cddr buc)))
		   ((= (signum1 n) -1)
		    (rplaca (cadr buc) f)
		    (rplacd (cadr buc) (neg n)))
		   (t (rplacd (cadr buc) n)))))
	  (t (let* ((nf    (oldget (oldget laws (caadr buc)) f))( m nil))
	       (cond ((null nf))	;NO SIMPLIFICATIONS HERE
		     ((equal n (cdadr buc)) ;EXPONENTS MATCH
		      (rplacd buc (cddr buc))
		      (return
			(sp1putbuc1 nf n *buc))) ;TO MAKE SURE IT DOESN'T OCCUR TWICE
		     ((eq (setq m (sp1great n (cdadr buc))) 'nomatch))
		     (m (setq m (cdadr buc))
			(rplacd buc (cddr buc))
			(sp1putbuc1 nf m *buc)
			(sp1putbuc1 f (m- n m) *buc)
			(return t))
		     (t (rplacd (cadr buc) (m- (cdadr buc) n))
			(return (sp1putbuc1 nf n *buc)))))))))

(defun sp1putbuc1 (f n buc)
  (cond ((null (cdr buc))
	 (rplacd buc (list (cons f n))))
	((eq f (caadr buc))
	 (rplacd (cadr buc) (m+ n (cdadr buc))))
	((sp1putbuc1 f n (cdr buc)))))

(defun sp1great (x y)
  (let* ((a    nil)( b nil))
    (cond ((mnump x)
	   (cond ((mnump y) (great x y)) (t 'nomatch)))
	  ((or (atom x) (atom y)) 'nomatch)
	  ((and (eq (caar x) (caar y))
		(alike (cond ((mnump (cadr x))
			      (setq a (cadr x)) (cddr x))
			     (t (setq a 1) (cdr x)))
		       (cond ((mnump (cadr y))
			      (setq b (cadr y)) (cddr y))
			     (t (setq b 1) (cdr y)))))
	   (great a b))
	  (t 'nomatch))))

(defun sp1untrep (b)
  (mapcan
   #'(lambda (buc)
       (mapcar #'(lambda (term)
		   (let* ((bas	     (simplifya (list (list (car term))
						      (m* (car b) (car buc)))
						t)))
		     (cond ((equal (cdr term) 1) bas)
			   ((m^ bas (cdr term))))))
	       (cdr buc)))
   (cdr b)))

(defun sp1kget (e)			;FINDS NUMERIC COEFFICIENTS
  (or (and (mtimesp e) (numberp (cadr e))
	   (cons (cadr e) (m*l (cddr e))))
      (cons 1 e)))

(defun sp1sincos (l *trig)
  (mapcar #'(lambda (q) (sp1sincos2 (m* (car l) (car q)) q)) (cdr l)))

(defun sp1sincos2 (arg l)
  (let* ((a
	  nil))
    (cond ((null (cdr l)))
	  ((and
	    (setq a (member (caadr l)
			  (cond ((null *trig)
				 '(%sinh %cosh %sinh %csch %sech %csch))
				('(%sin %cos %sin %csc %sec %csc))) :test #'eq))
	    (cddr l))		 ;THERE MUST BE SOMETHING TO MATCH TO.
	   (sp1sincos1 (cadr a) l arg))
	  ((sp1sincos2 arg (cdr l))))))

(defun sp1sincos1 (s l arg)
  (let* ((g
	  nil)( e 1))
    (do ((ll (cdr l) (cdr ll)))
	((null (cdr ll)) t)
      (cond ((eq s (caadr ll))
	     (setq arg (m* 2 arg))
	     (cond (*trig
		    (cond ((member s '(%sin %cos) :test #'eq)
			   (setq s '%sin))
			  ((setq s '%csc e -1))))
		   (t
		    (cond ((member s '(%sinh %cosh) :test #'eq)
			   (setq s '%sinh))
			  ((setq s '%csch e -1)))))
	     (cond ((alike1 (cdadr ll) (cdadr l))
		    (sp1addto s arg (cdadr l))
		    (setq *lin (cons (m* e (cdadr l)) *lin))
		    (rplacd ll (cddr ll))   ;;;MUST BE IN THIS ORDER!!
		    (rplacd l (cddr l))
		    (return t))
		   ((eq (setq g (sp1great (cdadr l) (cdadr ll))) 'nomatch))
		   ((null g)
		    (rplacd (cadr ll) (m- (cdadr ll) (cdadr l)))
		    (sp1addto s arg (cdadr l))
		    (setq *lin (cons (m* e (cdadr l)) *lin))
		    (rplacd l (cddr l))
		    (return t))
		   (t
		    (rplacd (cadr l) (m- (cdadr l) (cdadr ll)))
		    (sp1addto s arg (cdadr ll))
		    (setq *lin (cons (m* e (cdadr ll)) *lin))
		    (rplacd ll (cddr ll))
		    (return t))))))))

(defun sp1addto (fn arg exp)
  (setq arg (list (list fn) arg))
  (sp1add (cond ((equal exp 1) arg) (t (m^ arg exp)))))

(setq sc^ndisp '((%sin . sin^n) (%cos . cos^n) (%sinh . sinh^n) (%cosh . cosh^n)))

(defun sp1expt (b e)
  (cond ((mexptp b)
	 (sp1expt (cadr b) (m* e (caddr b))))
	((and (null (trigfp b)) (free e var))
	 (m^ b e))
	((equal b '$%e)
	 (sp1expt2 e))
	((and (null (eq var '*novar)) (free b var))
	 (sp1expt2 (m* (list '(%log) b) e)))
	((member (caar b) '(%sin %cos %tan %cot %sec %csc
			  %sinh %cosh %tanh %coth %sech %csch) :test #'eq)
	 (cond ((= (signum1 e) -1)
		(sp1expt (list (list (oldget (caar b) 'recip)) (cadr b))
			 (neg e)))
	       ((and (signp g e)
		     (member (caar b) '(%sin %cos %sinh %cosh) :test #'eq))
		(funcall (cdr (assoc (caar b) sc^ndisp :test #'eq)) e (cadr b)))
	       ((m^ b e))))
	((m^ b e))))

(defun sp1expt2 (e)
  (let* ((ans
	  nil)( fr nil)( exp nil))
    (setq ans (m2 e '((mplus) ((coeffpp) (fr freevar))
		      ((coeffpp) (exp true)))
		  nil)
	  fr (cdr (assoc 'fr ans :test #'eq))
	  exp (cdr (assoc 'exp ans :test #'eq)))
    (cond ((equal fr 0)
	   (m^ '$%e exp))
	  ((m* (m^ '$%e fr) (m^ '$%e exp))))))

(setq *sp1logf nil)

(defun sp1log (e)
  (cond ((or *trigred (atom e) (free e var))
	 (list '(%log) e))
	((eq (caar e) 'mplus)
	 (let* ((exp
		 (m1- e))( *a nil)( *n nil))
	   (cond ((smono exp var)
		  (list '(%log) e))
		 (*sp1logf (sp1log2 e))
		 ((let* ((*sp1logf
			  t)) (sp1log ($factor e)))))))
	((eq (caar e) 'mtimes)
	 (sp1 (m+l (mapcar 'sp1log (cdr e)))))
	((eq (caar e) 'mexpt)
	 (sp1 (m* (caddr e) (list '(%log) (cadr e)))))
	((sp1log2 e))))

(defun sp1log2 (e)
  (and $verbose
       (prog2 (mtell "trigreduce: can't expand ")
	   (show-exp (list '(%log) e))
	 (mtell "trigreduce: try again after applying the rule:~2%~M~%~%"
		(list '(mlable) nil
		      (out-of
		       (list '(mequal)
			     (list '(%log) e)
			     (list '(%integrate)
				   (list '(mquotient)
					 (list '(%derivative) e var 1)
					 e)
				   var)))))))
  (list '(%integrate)
	(sp1 ($ratsimp (list '(mtimes) (sdiff e var) (list '(mexpt) e -1))))
	var))

(defun sp1trig (e)
  (cond ((atom (cadr e)) (simplify e))
	((eq (caaadr e) (oldget (caar e) '$inverse)) (sp1 (cadadr e)))
	((eq (caaadr e) (oldget (oldget (caar e) 'recip) '$inverse))
	 (sp1 (m// (cadadr e))))
	((and (null *trigred) (null *noexpand) (eq (caaadr e) 'mplus))
	 (sp1trigex e))
	( e )))

(defun sp1trigex (e)
  (let* ((ans
	  nil)( fr nil)( exp nil))
    (setq ans (m2 (cadr e) '((mplus) ((coeffpp) (fr freevar))
			     ((coeffpp) (exp true)))
		  nil)
	  fr (cdr (assoc 'fr ans :test #'eq))
	  exp (cdr (assoc 'exp ans :test #'eq)))
    (cond ((signp e fr)
	   (setq fr (cadr exp)
		 exp (cond ((cdddr exp)
			    (cons (car exp) (cddr exp)))
			   ((caddr exp))))))
    (cond ((or (equal fr 0)
	       (null (member (caar e) '(%sin %cos %sinh %cosh) :test #'eq)))
	   e)
	  ((eq (caar e) '%sin)
	   (m+ (m* (sp1trig (list '(%sin) exp))
		   (sp1trig (list '(%cos) fr)))
	       (m* (sp1trig (list '(%cos) exp))
		   (sp1trig (list '(%sin) fr)))))
	  ((eq (caar e) '%cos)
	   (m- (m* (sp1trig (list '(%cos) exp))
		   (sp1trig (list '(%cos) fr)))
	       (m* (sp1trig (list '(%sin) exp))
		   (sp1trig (list '(%sin) fr)))))
	  ((eq (caar e) '%sinh)
	   (m+ (m* (sp1trig (list '(%sinh) exp))
		   (sp1trig (list '(%cosh) fr)))
	       (m* (sp1trig (list '(%cosh) exp))
		   (sp1trig (list '(%sinh) fr)))))
	  ((eq (caar e) '%cosh)
	   (m+ (m* (sp1trig (list '(%cosh) exp))
		   (sp1trig (list '(%cosh) fr)))
	       (m* (sp1trig (list '(%sinh) exp))
		   (sp1trig (list '(%sinh) fr))))))))

(defun sp1atrig (fn exp)
  (cond ((atom exp)
	 (sp1atrig2 fn exp))
	((eq fn (oldget (cadr exp) '$inverse))
	 (sp1 (cadr exp)))
	(t (sp1atrig2 fn exp))))

(defun sp1atrig2 (fn exp)
  (cond ((member fn '(%cot %sec %csc %coth %sech %csch) :test #'eq)
	 (setq exp (sp1 (m// exp))
	       fn (cdr (assoc fn '((%acot . %atan) (%asec . %acos) (%acsc . %asin)
				  (%acoth . %atanh) (%asech . %acosh) (%acsch . %asinh)) :test #'eq)))))
  (cond ((and (null *trigred)
	      (member fn '(%acos %acosh) :test #'eq))
	 (m+ half%pi (list
		      (list (cdr (assoc fn '((%acos . %asin) (%acosh . %asinh)) :test #'eq)))
		      exp)))
	((list (list fn) exp))))

(defun sin^n (%n v)
  (sc^n %n v (cond ((oddp %n) '(%sin))('(%cos))) (not (oddp %n))
	(m^ -1 (m+ (ash %n -1) 'k))))

(defun sinh^n (%n v)
  (if (oddp %n)
      (sc^n %n v '(%sinh) nil (m^ -1 'k))
      (if (zerop (mod %n 4))
	  (sc^n %n v '(%cosh) t (m^ -1 'k))
	  (m- (sc^n %n v '(%cosh) t (m- (m^ -1 'k)))))))

(defun cos^n (%n v) (sc^n %n v '(%cos) (not (oddp %n)) 1))

(defun cosh^n (%n v) (sc^n %n v '(%cosh) (not (oddp %n)) 1))

(defun sc^n (%n v fn fl coef)
  (cond ((minusp %n) (merror "Bug in `trigreduce'.  Please report.")))
  (m* (list '(rat) 1 (expt 2 %n))
      (m+ (cond (fl (list '(%binomial) %n (ash %n -1))) (t 0))
	  (maxima-substitute v 'trig-var
			     (dosum (m+ (m* 2
					    (list '(%binomial) %n 'k)
					    coef
					    (list fn (m* 'trig-var
							 (m+ %n (m* -2 'k))))))
				    'k 0 (ash (1- %n) -1) t)))))
