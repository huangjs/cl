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

(macsyma-module sin)

;;; Reference:  J. Moses, Symbolic Integration, MIT-LCS-TR-047, 12-1-1967.
;;; http://www.lcs.mit.edu/publications/pubs/pdf/MIT-LCS-TR-047.pdf.
;;;;
;;;; Unfortunately, some important pages in the scan are all black.
;;;;
;;;; A version with the missing pages is available (2008-12-14) from
;;;; http://www.softwarepreservation.org/projects/LISP/MIT

(declare-top (special ratform exptsum $radexpand $%e_to_numlog
		      exptind quotind splist l ans splist arcpart coef
		      aa dict exptflag base* powerlist *a* *b* k stack
		      ratroot rootlist square e w y expres arg var
		      *powerl* *c* *d* exp varlist genvar repswitch $liflag
		      noparts top maxparts numparts blank $opsubst))

(defvar *debug-integrate* nil
  "Enable debugging for the integrator routines.")

(defmacro op (frob)
  `(get ,frob 'operators))

(defun integerp1 (x)
  "Returns 2*x if 2*x is an integer, else nil"
  (integerp2 (mul2* 2 x)))

(defun superexpt (exp var base*)
  (prog (exptflag y w)
	(setq y (elemxpt exp))
	(when exptflag (return nil))
	(return
	 (substint
	  (list '(mexpt) base* var)
	  var
	  (integrator (div y (mul2 var (simplog (list base*)))) var)))))

(defun elemxpt (exp)
  (cond ((freevar exp) exp)
	((atom exp) (setq exptflag t))
	((not (eq (caar exp) 'mexpt))
	 (cons (car exp)
	       (mapcar #'(lambda (c) (elemxpt c)) (cdr exp))))
	((not (freevar (cadr exp)))
	 (list '(mexpt)
	       (elemxpt (cadr exp))
	       (elemxpt (caddr exp))))
	((not (eq (cadr exp) base*))
	 (elemxpt (list '(mexpt)
			base*
			(simplify (list '(mtimes)
					(list '(mexpt) (list '(%log) base*) -1)
					(list '(%log) (cadr exp))
					(caddr exp))))))
	((not (setq w (m2 (caddr exp)
			  '((mplus)
			    ((coeffpt) (a freevar) (var varp))
			    ((coeffpt) (b freevar)))
			  nil)))
	 (list (car exp) base* (elemxpt (caddr exp))))
	(t (maxima-substitute base*
			      'base*
			      (subliss w '((mtimes)
					   ((mexpt) base* b)
					   ((mexpt) var a)))))))

(defun subst10 (ex)
  (cond ((atom ex) ex)
	((and (eq (caar ex) 'mexpt) (eq (cadr ex) var))
	 (list '(mexpt) var (integerp2 (quotient (caddr ex) *d*))))
	(t (cons (ncons (caar ex))
		 (mapcar #'(lambda (c) (subst10 c)) (cdr ex))))))

;; Returns a list equal to x2 with first occurrence of x1 removed.
;; Stack overflow if x1 does not occur in x2.
(defun choicesin (x1 x2)
  (if (eq x1 (car x2))
      (cdr x2)
      (cons (car x2)
	    (choicesin x1 (cdr x2)))))

(defun rationalizer (x)
  (let ((ex (simplify ($factor x))))
    (if (not (alike1 ex x)) ex)))

(defun intform (expres)
  (declare (special *chebyform* *ratrootform*))
  (cond ((freevar expres) nil)
	((atom expres) nil)
	((member (caar expres) '(mplus mtimes) :test #'eq)
	 (let ((l (cdr expres)))
	   (prog (y)
	    loop (cond ((setq y (intform (car l))) (return y))
		       ((not (setq l (cdr l))) (return nil))
		       (t (go loop))))))
	((or (eq (caar expres) '%log) (arcp (caar expres)))
	 (cond
	   ((setq arg (m2 exp
			  `((mtimes) ((,(caar expres)) (b rat8))
			    ((coefftt) (c rat8prime)))
			  nil))
	    (ratlog exp var (cons (cons 'a expres) arg)))
	   (t
	    (prog (y z)
	       (cond
		 ((setq y (intform (cadr expres))) (return y))
		 ((and (eq (caar expres) '%log)
		       (setq z (m2 (cadr expres) *c* nil))
		       (setq y (m2 exp
				   '((mtimes)
				     ((coefftt) (c rat8))
				     ((coefftt) (d elem)))
				   nil)))
		  (return
		    (let ((*a* (cdr (sassq 'a z 'nill)))
			  (*b* (cdr (sassq 'b z 'nill)))
			  (*c* (cdr (sassq 'c y 'nill)))
			  (*d* (cdr (sassq 'd y 'nill))))
		      (substint
		       expres
		       var
		       (integrator
			(muln
			 (list (maxima-substitute
				`((mquotient) ((mplus) ((mexpt) $%e ,var)
					       ((mtimes) -1 ,*a*))
				  ,*b*)
				var
				*c*)
			       `((mquotient) ((mexpt) $%e ,var) ,*b*)
			       (maxima-substitute var expres *d*))
			 nil)
			var)))))
		 (t (return nil)))))))

      ;; We have a special function with an integral on the property list.
      ;; After the integral property was defined for the trig functions,
      ;; in rev 1.52, need to exclude trig functions here.
      ((and (not (atom (car expres)))
            (not (optrig (caar expres)))
	    (not (eq (caar expres) 'mexpt))
	    (get (caar expres) 'integral))
       (when *debug-integrate*
	 (format t "~&INTFORM with Integral on property list~%"))
       (cond
	 ((setq arg
	    (m2 exp
	     `((mtimes) ((,(caar expres)) (b rat8)) ((coefftt) (c rat8prime)))
	      nil))
	  ;; A rational function times the special function.
	  ;; Integrate with the method integration-by-parts.
	  (partial-integration (cons (cons 'a expres) arg) var))
	 ;; The method of integration-by-parts can not be applied.
	 ;; Maxima tries to get a clue for the argument of the function which
	 ;; allows a substitution for the argument.
	 ((intform (cadr expres)))
	 (t nil)))

	((optrig (caar expres))
	 (cond ((not (setq w (m2 (cadr expres) *c* nil)))
		(intform (cadr expres)))
	       (t
		(prog2
		    (setq *powerl* t)
		    (monstertrig exp var (cadr expres))))))
	((and (eq (caar expres) '%derivative)
	      (eq (caar exp) (caar expres))
	      (or (atom (cadr exp))
		  (not (eq (caaadr exp) 'mqapply))
		  (merror "Invalid arg to `integrate':~%~M" exp))
	      (checkderiv exp)))
	((not (eq (caar expres) 'mexpt)) nil)
	((integerp (caddr expres)) (intform (cadr expres)))
	((freevar (cadr expres))
	 (cond ((m2 (caddr expres) *c* nil)
		(superexpt exp var (cadr expres)))
	       ((intform (caddr expres)))
	       (t (let* (($%e_to_numlog t)
			 (nexp (resimplify exp)))
		    (cond ((alike1 exp nexp) nil)
			  (t (intform (setq exp nexp))))))))
	((not (rat8 (cadr expres)))
	 (intform (cadr expres)))
	((and (setq w (m2 (cadr expres) *ratrootform* nil)) ;e*(a*x+b) / (c*x+d)
	      (denomfind (caddr expres))) ;expon is ratnum
	 (cond ((setq w (prog2
			    (setq *powerl* t)
			    (ratroot exp var (cadr expres) w))) w)
	       (t (inte exp var))))
	((not (integerp1 (caddr expres))) ;2*exponent not integer
	 (cond ((m2 exp *chebyform* nil)
		(chebyf exp var))
	       (t (intform (cadr expres)))))
	((setq w (m2 (cadr expres) *d* nil)) ;sqrt(c*x^2+b*x+a)
	 #+nil
	 (format t "expres = sqrt(c*x^2+b*x+a)~%")
	 ;; I think this is method 5, arctrigonometric substitutions.
	 ;; (Moses, pg 80.)  The integrand is of the form
	 ;; R(x,sqrt(c*x^2+b*x+a)).  This method first eliminates the b
	 ;; term of the quadratic, and then uses an arctrig substitution.
	 (inte exp var))
	((m2 exp *chebyform* nil)
	 (chebyf exp var))
	((not (m2 (setq w ($expand (cadr expres)))
		  (cadr expres) nil))
	 (prog2 (setq exp (maxima-substitute w (cadr expres) exp))
	     (intform (simplify (list '(mexpt) w (caddr expres))))))
	((setq w (rationalizer (cadr expres)))
	 ;; The forms below used to have $radexpand set to $all.  But I
	 ;; don't think we really want to do that here because that makes
	 ;; sqrt(x^2) become x, which might be totally wrong.  This is one
	 ;; reason why we returned -4/3 for the
	 ;; integrate(sqrt(x+1/x-2),x,0,1).  We were replacing
	 ;; sqrt((x-1)^2) with x - 1, which is totally wrong since 0 <= x
	 ;; <= 1.
	 (setq exp (let (($radexpand $radexpand))
		     (maxima-substitute w (cadr expres) exp)))
	 (intform (let (($radexpand '$all))
		    (simplify (list '(mexpt) w (caddr expres))))))))

(defun separc (ex)
  (cond ((arcfuncp ex) (setq arcpart ex coef 1))
	((eq (caar ex) 'mtimes)
	 (arclist (cdr ex))
	 (setq coef (cond ((null (cdr coef)) (car coef))
			  (t (setq coef (cons (car ex) coef))))))))
(defun arclist (list)
  (cond ((null list) nil)
	((and (arcfuncp (car list)) (null arcpart))
	 (setq arcpart (car list)) (arclist (cdr list)))
	(t (setq coef (cons (car list) coef))
	   (arclist (cdr list)))))

(defun arcfuncp (ex)
  (and (not (atom ex))
       (or (arcp (caar ex))
	   (eq (caar ex) '%log)	     ; Experimentally treat logs also.
	   (and (eq (caar ex) 'mexpt)
		(integerp2 (caddr ex))
		(> (integerp2 (caddr ex)) 0)
		(arcfuncp (cadr ex))))))

+;; This is the main integration routine.  It is called from sinint.
+;; exp is guaranteed to be a product
(defun integrator (exp var)
  (prog (y arg *powerl* const *b* w *c* *d* e *ratrootform*
	 *chebyform* arcpart coef integrand result)
     (declare (special *ratrootform* *chebyform* *integrator-level*))
     ;; Increment recursion counter
     (incf *integrator-level*)

     ;; Trivial case. exp is not a function of var.
     (if (freevar exp) (return (mul2* exp var)))

     ;; Remove constant factors
     (setq w (partition exp var 1))
     (setq const (car w))
     (setq exp (cdr w))
     #+nil
     (progn
       (format t "w = ~A~%" w)
       (format t "const = ~A~%" const)
       (format t "exp = ~A~%" exp))

     (cond ((mplusp exp)
	    (return (mul2* const (integrate1 (cdr exp)))))

	   ;; Convert atan2(a,b) to atan(a/b) and try again.
	   ((and (not (atom exp))
		 (eq (caar exp) '$atan2))
	    (return (mul2* const (integrator
				  (simplifya (list '(%atan) (div (cadr exp) (caddr exp))) t)
				  var))))

	   ;; Integrate sums.
	   ((and (not (atom exp))
		 (eq (caar exp) '%sum))
	    (return (mul2* const (intsum exp var)))))

     ;; Try derivative-divides method.
     ;; This is the workhorse that solves many integrals.
     (cond ((setq y (diffdiv exp var))
	    (return (mul2* const y))))

     ;; At this point, we have EXP as a product of terms.  Make Y a
     ;; list of the terms of the product.
     (setq y (cond ((eq (caar exp) 'mtimes)
		    (cdr exp))
		   (t
		    (list exp))))

     #+nil
     (format t "y = ~S~%" y)
     ;; Pattern to match b*x + a
     (setq *c* '((mplus)
		 ((coeffpt) (b freevar) (x varp))
		 ((coeffpt) (a freevar))))
     ;; I think this is matching the pattern e*(a*x+b)/(c*x+d), where
     ;; a, b, c, d, and e are free of x, and x is the variable of
     ;; integration.
     (setq *ratrootform* '((mtimes)
			   ((coefftt) (e freevar))
			   ((mplus)
			    ((coeffpt) (a freevar) (var varp))
			    ((coeffpt) (b freevar)))
			   ((mexpt)
			    ((mplus)
			     ((coeffpt) (c freevar) (var varp))
			     ((coeffpt) (d freevar)))
			    -1)))
     ;; This is for matching the pattern a*x^r1*(c1+c2*x^q)^r2.
     (setq *chebyform* '((mtimes)
			 ((mexpt) (var varp) (r1 numberp))
			 ((mexpt)
			  ((mplus)
			   ((mtimes)
			    ((coefftt) (c2 freevar))
			    ((mexpt) (var varp) (q free1)))
			   ((coeffpp) (c1 freevar)))
			  (r2 numberp))
			 ((coefftt) (a freevar))))
     ;; This is the pattern c*x^2 + b * x + a.
     (setq *d* '((mplus)
		 ((coeffpt) (c freevar) ((mexpt) (x varp) 2))
		 ((coeffpt) (b freevar) (x varp))
		 ((coeffpt) (a freevar))))
     ;; This is the pattern (a*x+b)*(c*x+d)
     (setq e '((mtimes)
	       ((mplus)
		((coeffpt) (a freevar) (var varp))
		((coeffpt) (b freevar)))
	       ((mplus)
		((coeffpt) (c freevar) (var varp))
		((coeffpt) (d freevar)))))
     ;; Not sure what this loop is meant to do, but we're looking at
     ;; each term of the product and doing something with it if we
     ;; can.
     loop
     #+nil
     (progn
       (format t "car y =~%")
       (maxima-display (car y)))
     (cond ((rat8 (car y))
	    #+nil
	    (format t "In loop, go skip~%")
	    (go skip))
	   ((and (setq w (intform (car y)))
		 ;; Do not return a noun form as result at this point, because
		 ;; we would like to check for further special integrals.
		 ;; We store the result for later use.
		 (setq result w)
		 (not (isinop w '%integrate)))
	    #+nil
	    (format t "In loop, case intform~%")
	    (return (mul2* const w)))
	   (t
	    #+nil
	    (format t "In loop, go special~%")
	    ;; Store a possible partial result
	    (setq result w)
	    (go special)))
     skip
     (setq y (cdr y))
     (cond ((null y)
	    (return (mul2* const (cond ((setq y (powerlist exp var)) y)
				       (t (ratint exp var)))))))
     (go loop)
     special

     ;; SEPARC SETQS ARCPART AND COEF SUCH THAT
     ;; COEF*ARCEXP=EXP WHERE ARCEXP IS OF THE FORM
     ;; ARCFUNC^N AND COEF IS ITS ALGEBRAIC COEFFICIENT
     (separc exp)

     #+nil
     (progn
       (format t "arcpart = ~A~%" arcpart)
       (format t "coef =~%")
       (maxima-display coef))
     (cond ((and (not (null arcpart))
		 (do  ((stacklist stack (cdr stacklist)))
		      ((null stacklist) t)
		   (cond ((alike1 (car stacklist) coef)
			  (return nil))))
		 (not (isinop (setq w (let ((stack (cons coef stack)))
					(integrator coef var)))
			      '%integrate))
		 (setq integrand (mul2 w (sdiff arcpart var)))
		 (do ((stacklist stack (cdr stacklist)))
		     ((null stacklist) t)
		   (cond ((alike1 (car stacklist) integrand)
			  (return nil))))
		 (not (isinop
		       (setq y (let ((stack (cons integrand stack))
				     (integ integrand))
				 (integrator integ var)))
		       '%integrate)))
	    (return (add* (list '(mtimes) const w arcpart)
			  (list '(mtimes) -1 const y))))
	   (t
	    ;;(format t "t part~%")
	    (return
		(mul2 const
		      (cond ((setq y (scep exp var))
			     (cond ((cddr y)
				    #+nil
				    (progn
				      (format t "cddr y =~%")
				      (maxima-display (cddr y)))
				    (integrator ($trigreduce exp) var))
				   (t (sce-int (car y) (cadr y) var))))
			    ;; I don't understand why we do this. This
			    ;; causes the stack overflow in Bug
			    ;; 1487703, because we keep expanding exp
			    ;; into a form that matches the original
			    ;; and therefore we loop forever.  To
			    ;; break this we keep track how how many
			    ;; times we've tried this and give up
			    ;; after 4 (arbitrarily selected) times.
			    ((and (< *integrator-level* 4)
				  (not (alike1 exp (setq y ($expand exp)))))
			     #+nil
			     (progn
			       (format t "exp = ~A~%" exp)
			       (maxima-display exp)
			       (format t "y   = ~A~%" y)
			       (maxima-display y)
			       (break))
			     (integrator y var))
			    ((and (not *powerl*)
				  (setq y (powerlist exp var)))
			     y)
			    ((and (setq y (rischint exp var))
				  ;; rischint has not found an integral but
				  ;; returns a noun form. Do not return that
				  ;; noun form as result at this point, but
				  ;; store it for later use.
				  (setq result y)
				  (not (isinop y '%integrate)))
			     y)
			    ((setq y (integrate-exp-special exp var))
			     ;; Maxima found an integral for a power function
			     y)
			    (t
			     ;; Integrate-exp-special has not found an integral
			     ;; We look for a previous result obtained by
			     ;; intform or rischint.
			     (if result
				 result
				 (list '(%integrate) exp var))))))))))


;; This predicate is used with m2 pattern matcher.
;; A rational expression in var.
(defun rat8 (ex)
  (cond ((or (alike1 ex var) (freevar ex))
	 t)
	((member (caar ex) '(mplus mtimes) :test #'eq)
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat8 (car u)))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 nil)
	((integerp (caddr ex))
	 (rat8 (cadr ex)))))

(defun optrig (x)
  (member x '(%sin %cos %sec %tan %csc %cot) :test #'eq))

;;after finding a non-integrable summand usually better to pass rest to risch
(defun integrate1 (exp)
  (do ((terms exp (cdr terms)) (ans))
      ((null terms) (addn ans nil))
    (let ($liflag)					;don't gen li's for
      (push (integrator (car terms) var) ans))		;parts of integrand
    (when (and (not (free (car ans) '%integrate)) (cdr terms))
	  (return (addn (cons (rischint (cons '(mplus) terms) var) (cdr ans))
			nil)))))

(defun scep (expr var &aux trigl exp)	; Product of SIN, COS, EXP
  (and (mtimesp expr)			;	of linear args.
       (loop for fac in (cdr expr) do
	     (cond ((atom fac) (return nil))
		   ((trig1 (car fac))
		    (if (linearp (cadr fac) var) (push fac trigl)
			(return nil)))
		   ((and (mexptp fac)
			 (eq (cadr fac) '$%e)
			 (linearp (caddr fac) var))
		    ;; should be only one exponential factor
		    (setq exp fac))
		   (t (return nil)))
	     finally (return (cons exp trigl)))))

;; Integrates exponential * sin or cos, all with linear args.

(defun sce-int (exp s-c var)		; EXP is non-trivial
  (let* ((e-coef (car (islinear (caddr exp) var)))
         (sc-coef (car (islinear (cadr s-c) var)))
         (sc-arg (cadr s-c))
         (abs-val (add (power e-coef 2) (power sc-coef 2))))
    (if (zerop1 abs-val)
        ;; The numerator is zero. Exponentialize the integrand and try again.
        (integrator ($exponentialize (mul exp s-c)) var)
        (mul (div exp abs-val)
             (add (mul e-coef s-c)
                  (if (eq (caar s-c) '%sin)
                      (mul* (neg sc-coef) `((%cos) ,sc-arg))
                      (mul* sc-coef `((%sin) ,sc-arg))))))))

(defun checkderiv (expr)
  (checkderiv1 (cadr expr) (cddr expr) () ))

;; CHECKDERIV1 gets called on the expression being differentiated,
;; an alternating list of variables being differentiated with
;; respect to and powers thereof, and a reversed list of the latter
;; that have already been examined.  It returns either the antiderivative
;; or (), saying this derivative isn't wrt the variable of integration.

(defun checkderiv1 (expr wrt old-wrt)
  (cond ((alike1 (car wrt) var)
	 (if (equal (cadr wrt) 1)	;Power = 1?
	     (if (null (cddr wrt))	;single or partial
		 (if (null old-wrt)
		     expr		;single
		     `((%derivative), expr ;partial in old-wrt
		       ,.(nreverse old-wrt)))
		 `((%derivative) ,expr	;Partial, return rest
		   ,.(nreverse old-wrt)
		   ,@(cddr wrt)))
	     `((%derivative) ,expr	;Higher order, reduce order
	       ,.(nreverse old-wrt)
	       ,(car wrt) ,(add* (cadr wrt) -1)
	       ,@ (cddr wrt))))
	((null (cddr wrt)) () )		;Say it doesn't apply here
	(t (checkderiv1 expr (cddr wrt)	;Else we check later terms
			(list* (cadr wrt) (car wrt) old-wrt)))))

(defun elem (a)
  (cond ((freevar a) t)
	((atom a) nil)
	((m2 a expres nil) t)
	(t (eval (cons 'and (mapcar #'elem (cdr a)))))))

(defun freevar (a)
  (cond ((atom a) (not (eq a var)))
	((alike1 a var) nil)
	((and (not (atom (car a)))
	      (member 'array (cdar a) :test #'eq))
	 (cond ((freevar (cdr a)) t)
	       (t (merror "Variable of integration appeared in subscript"))))
	(t (and (freevar (car a)) (freevar (cdr a))))))

(defun varp (x)
  (alike1 x var))

(defun integrallookups (exp)
  (let (form dummy-args real-args)
  (cond
	((eq (caar exp) 'mqapply)
	 ;; Transform to functional form and try again.
	 ;; For example:
	 ;; ((MQAPPLY SIMP) (($PSI SIMP ARRAY) 1) $X)
	 ;; => (($PSI) 1 $X)
	 (integrallookups `((,(caaadr exp)) ,@(cdadr exp) ,@(cddr exp))))

	;; Lookup algorithm for integral of a special function. 
	;; The integral form is put on the property list, and can be a 
	;; lisp function of the args.  If the form is nil, or evaluates 
        ;; to nil, then return noun form unevaluated.
	((and (not (atom (car exp)))
	    (setq form (get (caar exp) 'integral))
	    (setq dummy-args (car form))
	    (setq real-args (cdr exp))
	    ;; search through the args of exp and find the arg containing var
	    ;; look up the integral wrt this arg from form
	    (setq form
	      (do ((x real-args (cdr x))
		   (y (cdr form) (cdr y)))
		  ((or (null x) (null y)) nil)
		  (if (not (freevar (car x))) (return (car y)))))
	    ;; If form is a function then evaluate it with actual args
	    (or (not (functionp form))
		(setq form (apply form real-args))))
	 (when *debug-integrate*
	   (format t "~&INTEGRALLOOKUPS: Found integral ~A.~%" (caar exp)))
	 (substitutel real-args dummy-args form))

	((eq (caar exp) 'mplus)
	 (muln (list '((rat simp) 1 2) exp exp) nil))

	(t nil))))

;; Integrals of elementary special functions
;; This may not be the best place for this definition, but it is close 
;; to the original code.
(defprop %log  ((x) ((mplus) ((mtimes) x ((%log) x)) ((mtimes) -1 x))) integral)
(defprop %sin  ((x) ((mtimes) -1 ((%cos) x))) integral)
(defprop %cos  ((x) ((%sin) x)) integral)
(defprop %tan  ((x) ((%log) ((%sec) x))) integral)
(defprop %csc  ((x) ((mtimes) -1 ((%log) ((mplus) ((%csc) x) ((%cot) x))))) integral)
(defprop %sec  ((x) ((%log) ((mplus) ((%sec) x) ((%tan) x)))) integral)
(defprop %cot  ((x) ((%log) ((%sin) x))) integral)
(defprop %sinh ((x) ((%cosh) x))  integral)
(defprop %cosh ((x) ((%sinh) x)) integral)
(defprop %tanh ((x) ((%log) ((%cosh) x))) integral)
(defprop %coth ((x) ((%log) ((%sinh) x))) integral)
(defprop %sech ((x) ((%atan) ((%sinh)x))) integral)
(defprop %csch ((x) ((%log) ((%tanh) ((mtimes) ((rat simp) 1 2) x)))) integral)

;; Integral of a^b == ((mexpt) a b)
(putprop 'mexpt
  `((a b)
  ;;integrate(a^b,a);
  ,(lambda (a b)
    (cond
      ((or (equal b -1)
	   (and (not (mnump b))
		(freeof '$%i b)
		(eq (asksign (power (add b 1) 2)) '$zero)))
         (logmabs a))
      (t
       '((mtimes) ((mexpt) a ((mplus) b 1)) ((mexpt) ((mplus) b 1) -1)))))
  ;; integrate(a^b,b);
  ((mtimes) ((mexpt) a b) ((mexpt) ((%log) a) -1)))
  'integral)

(defun rat10 (ex)
  (cond ((freevar ex) t)
	((alike1 ex var) nil)
	((eq (caar ex) 'mexpt)
	 (if (alike1 (cadr ex) var)
	     (if (integerp2 (caddr ex))
		 (setq powerlist (cons (caddr ex) powerlist)))
	     (and (rat10 (cadr ex)) (rat10 (caddr ex)))))
	((member (caar ex) '(mplus mtimes) :test #'eq)
	 (do ((u (cdr ex) (cdr u))) ((null u) t)
	     (if (not (rat10 (car u))) (return nil))))
	(t
	 (let ((examine (margs ex)))
	   (if (atom (first examine))
	       (do* ((element examine (rest element))
		     (result (rat10 (first examine))
			     (and result (rat10 (first element)))))
		   ((or (null result) (null element)) result))
	     (rat10 (first examine)))))))

(defun listgcd (powerlist)
  (prog (p)
     (setq p (car powerlist))
   loop
     (setq powerlist (cdr powerlist))
     (if (equal p 1) (return nil))
     (if (null powerlist) (return p))
     (setq p (gcd p (car powerlist)))
     (go loop)))

(defun integrate5 (ex var)
  (if (rat8 ex)
      (ratint ex var)
      (integrator ex var)))

(defun integerp2 (x)
  "Returns x if x is an integer, else false"
  (let (u)
    (cond ((not (numberp x)) nil)
	  ((not (floatp x)) x)
	  ((prog2 (setq u (maxima-rationalize x))
	       (equal (cdr u) 1)) (car u)))))

(defun rat3 (ex ind)
  (cond ((freevar ex) t)
	((atom ex) ind)
	((member (caar ex) '(mtimes mplus) :test #'eq)
	 (do ((u (cdr ex) (cdr u)))
	     ((null u) t)
	   (if (not (rat3 (car u) ind))
	       (return nil))))
	((not (eq (caar ex) 'mexpt))
	 (rat3 (car (margs ex)) t))
	((freevar (cadr ex))
	 (rat3 (caddr ex) t))
	((integerp (caddr ex))
	 (rat3 (cadr ex) ind))
	((and (m2 (cadr ex) ratroot nil)
	      (denomfind (caddr ex)))
	 (setq rootlist (cons (denomfind (caddr ex)) rootlist)))
	(t (rat3 (cadr ex) nil))))

(defun subst4 (ex)
  (cond ((freevar ex) ex)
	((atom ex) *a*)
	((not (eq (caar ex) 'mexpt))
	 (mapcar #'(lambda (u) (subst4 u)) ex))
	((m2 (cadr ex) ratroot nil)
	 (list (car ex) *b* (integerp2 (timesk k (caddr ex)))))
	(t (list (car ex) (subst4 (cadr ex)) (subst4 (caddr ex))))))

(defun findingk (list)
  (do ((kk 1) (l list (cdr l)))
      ((null l) kk)
    (setq kk (lcm kk (car l)))))

(defun denomfind (x)
  (cond ((ratnump x) (caddr x))
	((not (numberp x)) nil)
	((not (floatp x)) 1)
	(t (cdr (maxima-rationalize x)))))

;; EXP = f(t,u) where f is some function with, say, VAR = t,
;; u^k = RATROOT = e*(a*t+b)/(c*t+d), where the smallest possible k
;; is calculated below.
;; As always, W is an alist which associates to the coefficients
;; a, b... (and to VAR) their values.
(defun ratroot (exp var ratroot w)
  (prog (rootlist k y w1)
     (cond ((setq y (chebyf exp var)) (return y)))
     (cond ((not (rat3 exp t)) (return nil)))
     (setq k (findingk rootlist))
     (setq w1 (cons (cons 'k k) w))
     (setq y
	   (subst41 exp
		    (simplify
		     (subliss w1
			      '((mquotient)
				((mplus) ((mtimes) b e)
				 ((mtimes) -1 d ((mexpt) var k)))
				((mplus) ((mtimes) c ((mexpt) var k))
				 ((mtimes) -1 e a)))))
		    var))
     (setq y
	   (integrator
	    (simplify
	     (list '(mtimes)
		   y
		   (subliss
		    w1 '((mquotient)
			 ((mtimes)
			  e ((mplus)
			     ((mtimes) a d k
			      ((mexpt) var ((mplus) -1 k)))
			     ((mtimes)
			      -1
			      ((mtimes) b c k
			       ((mexpt) var ((mplus) -1 k))))))
			 ((mexpt) ((mplus)
				   ((mtimes) c ((mexpt) var k))
				   ((mtimes) -1 a e))
			  2)))))
	    var))
     (return (substint (simplify (list '(mexpt)
				       ratroot
				       (list '(mexpt) k -1)))
		       var
		       y))))

(defun subst41 (exp *a* *b*)
  (subst4 exp))

;; exp = a*t^r1*(c1+c2*t^q)^r2, where var = t.
;;
;; G&S 2.202 has says this integral can be expressed by elementary
;; functions ii:
;;
;; 1. q is an integer
;; 2. (r1+1)/q is an integer
;; 3. (r1+1)/q+r2 is an integer.
;;
;; I (rtoy) think that for this code to work, r1, r2, and q must be
;; numbers.
(defun chebyf (exp var)
  (prog (r1 r2 d1 d2 n1 n2 w q)
     ;; Return NIL if the expression doesn't match.
     (cond ((not (setq w
		       (m2 exp
			   ;; Why aren't we using *chebyform* here?
			   ;; This pattern is the same as *chebyform*.
			   '((mtimes)
			     ((mexpt) (var varp) (r1 numberp))
			     ((mexpt)
			      ((mplus)
			       ((mtimes)
				((coefftt) (c2 freevar))
				((mexpt) (var varp) (q free1)))
			       ((coeffpp) (c1 freevar)))
			      (r2 numberp))
			     ((coefftt) (a freevar)))
			   nil)))
	    (return nil)))
     #+nil
     (format t "w = ~A~%" w)
     (when (zerop1 (cdr (sassq 'c1 w #'nill)))
       ;; rtoy: Is it really possible to be in this routine with c1 =
       ;; 0?
       (return
	 (mul*
	  ;; This factor is locally constant as long as t and
	  ;; c2*t^q avoid log's branch cut.
	  (subliss w '((mtimes) a ((mexpt) var ((mtimes) -1 q r2))
		       ((mexpt) ((mtimes) c2 ((mexpt) var q)) r2)))
	  (integrator
	   (subliss w '((mexpt) var ((mplus) r1 ((mtimes) q r2)))) var))))
     (setq q (cdr (sassq 'q w 'nill)))
     ;; Reset parameters.  a = a/q, r1 = (1 - q + r1)/q
     (setq w
	   (list* (cons 'a (div* (cdr (sassq 'a w 'nill)) q))
		  (cons
		   'r1
		   (div* (addn (list 1 (neg (simplify q)) (cdr (sassq 'r1 w 'nill))) nil) q))
		  w))
     #+nil
     (format t "new w = ~A~%" w)
     (setq r1 (cdr (sassq 'r1 w 'nill))
	   r2 (cdr (sassq 'r2 w 'nill)))
     #+nil
     (progn
       (format t "new r1 = ~A~%" r1)
       (format t "r2     = ~A~%" r2))
     ;; Write r1 = d1/n1, r2 = d2/n2, if possible.  Update w with
     ;; these values, if so.  If we can't, give up.  I (rtoy) think
     ;; this only happens if r1 or r2 can't be expressed as rational
     ;; numbers.  Hence, r1 and r2 have to be numbers, not variables.
     (cond
       ((not (and (setq d1 (denomfind r1))
		  (setq d2 (denomfind r2))
		  (setq n1 (integerp2 (timesk r1 d1)))
		  (setq n2 (integerp2 (timesk r2 d2)))
		  (setq w (list* (cons 'd1 d1) (cons 'd2 d2)
				 (cons 'n1 n1) (cons 'n2 n2)
				 w))))
	#+nil
	(progn
	  (format t "cheby can't find one of d1,d2,n1,n2:~%")
	  (format t "  d1 = ~A~%" d1)
	  (format t "  d2 = ~A~%" d2)
	  (format t "  n1 = ~A~%" n1)
	  (format t "  n2 = ~A~%" n2))
	(return nil))
       ((and (integerp2 r1) (> r1 0))
	#+nil (format t "integer r1 > 0~%")
	;; (r1+q-1)/q is positive integer.
	;;
	;; I (rtoy) think we are using the substitution z=(c1+c2*t^q).
	;; Maxima thinks the resulting integral should then be
	;;
	;; a/q*c2^(-r1/q-1/q)*integrate(z^r2*(z-c1)^(r1/q+1/q-1),z)
	;;
	(return
	  (substint
	   (subliss w '((mplus) c1 ((mtimes) c2 ((mexpt) var q))))
	   var
	   (integrator
	    (expands (list (subliss w
				    ;; a*t^r2*c2^(-r1-1)
				    '((mtimes)
				      a
				      ((mexpt) var r2)
				      ((mexpt)
				       c2
				       ((mtimes)
					-1
					((mplus) r1 1))))))
		     (cdr
		      ;; (t-c1)^r1
		      (expandexpt (subliss w
					   '((mplus)
					     var
					     ((mtimes) -1 c1)))
				  r1)))
	    var))))
       ((integerp2 r2)
	#+nil (format t "integer r2~%")
	;; I (rtoy) think this is using the substitution z = t^(q/d1).
	;;
	;; The integral (as maxima will tell us) becomes
	;;
	;; a*d1/q*integrate(z^(n1/q+d1/q-1)*(c1+c2*z^d1)^r2,z)
	;;
	;; But be careful because the variable A in the code is
	;; actually a/q.
	(return
	  (substint (subliss w '((mexpt) var ((mquotient) q d1)))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 d1 a
						 ((mexpt)
						  var
						  ((mplus)
						   n1 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mtimes)
						    c2
						    ((mexpt)
						     var d1))
						   c1)
						  r2))))
			    var))))
       ((and (integerp2 r1) (< r1 0))
	#+nil (format t "integer r1 < 0~%")
	;; I (rtoy) think this is using the substitution
	;;
	;; z = (c1+c2*t^q)^(1/d2)
	;;
	;; With this substitution, maxima says the resulting integral
	;; is
	;;
	;;  a/q*c2^(-r1/q-1/q)*d2*
	;;    integrate(z^(n2+d2-1)*(z^d2-c1)^(r1/q+1/q-1),z)
	(return
	  (substint (subliss w
			     ;; (c1+c2*t^q)^(1/d2)
			     '((mexpt)
			       ((mplus)
				c1
				((mtimes) c2 ((mexpt) var q)))
			       ((mquotient) 1 d2)))
		    var
		    (ratint (simplify (subliss w
					       ;; This is essentially
					       ;; the integrand above,
					       ;; except A and R1 here
					       ;; are not the same as
					       ;; derived above.
					       '((mtimes)
						 a d2
						 ((mexpt)
						  c2
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 1)))
						 ((mexpt)
						  var
						  ((mplus)
						   n2 d2 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    var d2)
						   ((mtimes) -1 c1))
						  r1))))
			    var))))
       ((integerp2 (add* r1 r2))
	#+nil (format t "integer r1+r2~%")
	;; If we're here,  (r1-q+1)/q+r2 is an integer.
	;;
	;; I (rtoy) think this is using the substitution
	;;
	;; z = ((c1+c2*t^q)/t^q)^(1/d1)
	;;
	;; With this substitution, maxima says the resulting integral
	;; is
	;;
	;; a*d2/q*c1^(r2+r1/q+1/q)*
	;;   integrate(z^(d2*r2+d2-1)*(z^d2-c2)^(-r2-r1/q-1/q-1),z)
	(return
	  (substint (let (($radexpand '$all))
		      ;; Setting $radexpand to $all here gets rid of
		      ;; ABS in the subtitution.  I think that's ok in
		      ;; this case.  See Bug 1654183.
		      (subliss w
			       '((mexpt)
				 ((mquotient)
				  ((mplus)
				   c1
				   ((mtimes) c2 ((mexpt) var q)))
				  ((mexpt) var q))
				 ((mquotient) 1 d1))))
		    var
		    (ratint (simplify (subliss w
					       '((mtimes)
						 -1 a d1
						 ((mexpt)
						  c1
						  ((mplus)
						   r1 r2 1))
						 ((mexpt)
						  var
						  ((mplus)
						   n2 d1 -1))
						 ((mexpt)
						  ((mplus)
						   ((mexpt)
						    var d1)
						   ((mtimes)
						    -1 c2))
						  ((mtimes)
						   -1
						   ((mplus)
						    r1 r2
						    2))))))
			    var))))
       (t (return (list '(%integrate) exp var))))))

(defun greaterratp (x1 x2)
  (cond ((and (numberp x1) (numberp x2))
	 (> x1 x2))
	((ratnump x1)
	 (greaterratp (quotient (float (cadr x1))
				(caddr x1))
		      x2))
	((ratnump x2)
	 (greaterratp x1
		      (quotient (float (cadr x2))
				(caddr x2))))))

(defun trig1 (x)
  (member (car x) '(%sin %cos) :test #'eq))

(defun supertrig (exp)
  (declare (special *notsame* *trigarg*))
  (cond ((freevar exp) t)
	((atom exp) nil)
	((member (caar exp) '(mplus mtimes) :test #'eq)
	 (and (supertrig (cadr exp))
	      (or (null (cddr exp))
		  (supertrig (cons (car exp)
				   (cddr exp))))))
	((eq (caar exp) 'mexpt)
	 (and (supertrig (cadr exp))
	      (supertrig (caddr exp))))
	((eq (caar exp) '%log)
	 (supertrig (cadr exp)))
	((member (caar exp)
	       '(%sin %cos %tan %sec %cot %csc) :test #'eq)
	 (cond ((m2 (cadr exp) *trigarg* nil) t)
	       ((m2 (cadr exp)
		    '((mplus)
		      ((coeffpt) (b freevar) (x varp))
		      ((coeffpt) (a freevar)))
		    nil)
		(and (setq *notsame* t) nil))
	       (t (supertrig (cadr exp)))))
	(t (supertrig (cadr exp)))))

(defun subst2s (ex pat)
  (cond ((null ex) nil)
	((m2 ex pat nil) var)
	((atom ex) ex)
	(t (cons (subst2s (car ex) pat)
		 (subst2s (cdr ex) pat)))))

;; Match (c*x+b), where c and b are free of x
(defun simple-trig-arg (exp)
  (m2 exp '((mplus) ((mtimes)
		     ((coefftt) (c freevar))
		     ((coefftt) (v varp)))
	    ((coeffpp) (b freevar)))
      nil))


(defun monstertrig (exp var *trigarg*)
  (declare (special *trigarg*))
  (when (not (atom *trigarg*))
    (let ((arg (simple-trig-arg *trigarg*)))
      (cond (arg
	     ;; We have trig(c*x+b).  Use the substitution y=c*x+b to
	     ;; try to compute the integral.  Why?  Because x*sin(n*x)
	     ;; takes longer and longer as n gets larger and larger.
	     ;; This is caused by the Risch integrator.  This is a
	     ;; work-around for this issue.
	     (let* ((c (cdras 'c arg))
		    (b (cdras 'b arg))
		    (new-var (gensym "NEW-VAR-"))
		    (new-exp (maxima-substitute (div (sub new-var b) c)
						var exp))
		    (new-int
		     (if (every-trigarg-alike new-exp new-var)
			 ;; avoid endless recursion when more than one
			 ;; trigarg exists or c is a float
			 (div ($integrate new-exp new-var) c)
		       (rischint exp var))))
		 (return-from monstertrig (maxima-substitute *trigarg* new-var new-int))))
	    (t
	     (return-from monstertrig (rischint exp var))))))
  (prog (*notsame* w *a* *b* y *d*)
     (declare (special *notsame*))
	(cond
	 ((supertrig exp) (go a))
	 ((null *notsame*) (return nil))
	 ((not (setq y (m2 exp
			   '((mtimes)
			     ((coefftt) (a freevar))
			     (((b trig1))
			      ((mtimes)
			       (x varp)
			       ((coefftt) (m freevar))))
			     (((d trig1))
			      ((mtimes)
			       (x varp)
			       ((coefftt) (n freevar)))))
			   nil)))
	  (go b))
	 ((not (and (member (car (setq *b* (cdr (sassq 'b y 'nill))))
			  '(%sin %cos) :test #'eq)
		    (member (car (setq *d* (cdr (sassq 'd y 'nill))))
			  '(%sin %cos) :test #'eq)))
	  (return nil))
	 ((and (eq (car *b*) '%sin) (eq (car *d*) '%sin))
	  (return (subvar (subliss y
				   '((mtimes)
				     a
				     ((mplus)
				      ((mquotient)
				       ((%sin)
					((mtimes)
					 ((mplus) m ((mtimes) -1 n))
					 x))
				       ((mtimes)
					2
					((mplus) m ((mtimes) -1 n))))
				      ((mtimes)
				       -1
				       ((mquotient)
					((%sin)
					 ((mtimes) ((mplus) m n) x))
					((mtimes)
					 2
					 ((mplus) m n))))))))))
	 ((and (eq (car *b*) '%cos) (eq (car *d*) '%cos))
	  (return (subvar (subliss y
				   '((mtimes)
				     a
				     ((mplus)
				      ((mquotient)
				       ((%sin)
					((mtimes)
					 ((mplus) m ((mtimes) -1 n))
					 x))
				       ((mtimes)
					2
					((mplus) m ((mtimes) -1 n))))
				      ((mquotient)
				       ((%sin)
					((mtimes) ((mplus) m n) x))
				       ((mtimes)
					2
					((mplus) m n)))))))))
	 ((or (and (eq (car *b*) '%cos)
		   (setq w (cdr (sassq 'm y 'nill)))
		   (rplacd (sassq 'm y 'nill)
			   (cdr (sassq 'n y 'nill)))
		   (rplacd (sassq 'n y 'nill) w))
	      t)
	  (return (subvar (subliss y
				   '((mtimes)
				     -1
				     a
				     ((mplus)
				      ((mquotient)
				       ((%cos)
					((mtimes)
					 ((mplus) m ((mtimes) -1 n))
					 x))
				       ((mtimes)
					2
					((mplus) m ((mtimes) -1 n))))
				      ((mquotient)
				       ((%cos)
					((mtimes) ((mplus) m n) x))
				       ((mtimes)
					2
					((mplus) m n))))))))))
   b    (cond ((not (setq y (prog2 (setq *trigarg* var)
				   (m2 exp
				       '((mtimes)
					 ((coefftt) (a freevar))
					 (((b trig1))
					  ((mtimes)
					   (x varp)
					   ((coefftt) (n integerp2))))
					 ((coefftt) (c supertrig)))
				       nil))))
	       (return nil)))
	(return
	 (integrator
	  ($expand
	   (list '(mtimes)
		 (sch-replace y 'a)
		 (sch-replace y 'c)
		 (cond ((eq (car (setq *b* (sch-replace y 'b))) '%cos)
			(maxima-substitute var 'x (supercosnx (sch-replace y 'n))))
		       (t (maxima-substitute var 'x (supersinx (sch-replace y 'n)))))))
	  var))
   a    (setq w (subst2s exp *trigarg*))
	(setq *b* (cdr (sassq 'b
			    (m2 *trigarg*
				'((mplus)
				  ((coeffpt) (b freevar) (x varp))
				  ((coeffpt) (a freevar)))
				nil)
			    'nill)))
	(setq *a* (substint *trigarg*
			    var
			    (trigint (div* w *b*) var)))
   (cond ((m2 *a* '((mtimes) ((coefftt) (d freevar)) ((%integrate) (b true) (c true))) nil)
	 (return (list '(%integrate) exp var))))
   (return *a*)))

(defun trig2 (x)
  (member (car x) '(%sin %cos %tan %cot %sec %csc) :test #'eq))

(defun supersinx (n)
  (let ((i (if (< n 0) -1 1)))
    ($expand (list '(mtimes) i (sinnx (timesk i n))))))

(defun supercosnx (n)
  ($expand (cosnx (timesk (if (< n 0) -1 1) n))))

(defun sinnx (n)
  (if (equal n 1)
      '((%sin) x)
      (list '(mplus)
	    (list '(mtimes) '((%sin) x) (cosnx (1- n)))
	    (list '(mtimes) '((%cos) x) (sinnx (1- n))))))

(defun cosnx (n)
  (if (equal n 1)
      '((%cos) x)
      (list '(mplus)
	    (list '(mtimes) '((%cos) x) (cosnx (1- n)))
	    (list '(mtimes) -1 '((%sin) x) (sinnx (1- n))))))

(defun poseven (x)
  (and (even x) (> x -1)))

(defun trigfree (x)
  (if (atom x)
      (not (member x '(sin* cos* sec* tan*) :test #'eq))
      (and (trigfree (car x)) (trigfree (cdr x)))))

(defun rat1 (exp)
  (prog (*b1* *notsame*)
     (declare (special *yy* *b1* *notsame*))
     (when (and (numberp exp) (zerop exp))
       (return nil))
     (setq *b1* (subst *b* 'b '((mexpt) b (n even))))
     (return (prog2
		 (setq *yy* (rats exp))
		 (cond ((not *notsame*) *yy*))))))

(defun rats (exp)
  (prog (y)
     (declare (special *notsame* *b1*))
     (return
       (cond ((eq exp *a*) 'x)
	     ((atom exp)
	      (cond ((member exp '(sin* cos* sec* tan*) :test #'eq)
		     (setq *notsame* t))
		    (t exp)))
	     ((setq y (m2 exp *b1* nil))
	      (f3 y))
	     (t (cons (car exp) (mapcar #'(lambda (g) (rats g)) (cdr exp))))))))


(defun f3 (y)
  (maxima-substitute *c*
		     'c
		     (maxima-substitute (quotient (cdr (sassq 'n y nil)) 2)
					'n
					'((mexpt)
					  ((mplus)
					   1
					   ((mtimes)
					    c
					    ((mexpt) x 2)))
					  n))))

(defun odd1 (n)
  (declare (special *yz*))
  (cond ((not (numberp n)) nil)
	((not (equal (rem n 2) 0))
	 (setq *yz*
	       (maxima-substitute *c*
				  'c
				  (list '(mexpt)
					'((mplus) 1 ((mtimes) c ((mexpt) x 2)))
					(quotient (1- n) 2)))))
	(t nil)))

(defun subvar (x)
  (maxima-substitute var 'x x))

(defun subvardlg (x)
  (mapcar #'(lambda (m)
	      (cons (maxima-substitute var 'x (car m)) (cdr m)))
	  x))

;; This appears to be the implementation of Method 6, pp.82 in Moses'
;; thesis.

(defun trigint (exp var)
  (prog (y repl y1 y2 *yy* z m n *c* *yz* *a* *b* )
     (declare (special *yy* *yz*))
     ;; Transform trig(x) into trig* (for simplicity?)  Convert cot to
     ;; tan and csc to sin.
     (setq y2
	   (subliss (subvardlg '((((%sin) x) . sin*)
				 (((%cos) x) . cos*)
				 (((%tan) x) . tan*)
				 (((%cot) x) . ((mexpt) tan* -1))
				 (((%sec) x) . sec*)
				 (((%csc) x) . ((mexpt) sin* -1))))
		    (simplifya exp nil)))
     #+nil
     (progn
       (format t "y2 = ~%")
       (maxima-display y2))
     ;; Now transform tan to sin/cos and sec to 1/cos.
     (setq y1 (setq y (simplify (subliss '((tan* . ((mtimes) sin*
						    ((mexpt) cos* -1)))
					   (sec* . ((mexpt) cos* -1)))
					 y2))))
     #+nil
     (progn
       (format t "y  =~%")
       (maxima-display y))
     (cond ((null (setq z (m2 y
			      '((mtimes)
				((coefftt) (b trigfree))
				((mexpt) sin* (m poseven))
				((mexpt) cos* (n poseven)))
			      nil)))
	    ;; Go if y is not of the form sin^m*cos^n for positive
	    ;; even m and n.
	    #+nil
	    (format t "Not of form sin^m*cos^n, for m, n non-negative and even.~%")
	    (go l1)))

     ;; Case III:
     ;;
     ;; Handle the case of sin^m*cos^n, m, n both non-negative and
     ;; even.

     #+nil
     (format t "Case III~%")
     (setq m (cdr (sassq 'm z 'nill)))
     (setq n (cdr (sassq 'n z 'nill)))
     (setq *a* (integerp2 (* 0.5 (if (< m n) 1 -1) (+ n (* -1 m)))))
     (setq z (cons (cons 'a *a*) z))
     #+nil
     (progn
       (format t "m, n = ~A ~A~%" m n)
       (format t "a = ~A~%" a)
       (format t "z = ~A~%" z))
     ;; integrate(sin(y)^m*cos(y)^n,y) is transformed to the following form:
     ;;
     ;; m < n:
     ;;   integrate((sin(2*y)/2)^n*(1/2+1/2*cos(2*y)^((n-m)/2),y)
     ;;
     ;; m >= n:
     ;;
     ;;   integrate((sin(2*y)/2)^n*(1/2-1/2*cos(2*y)^((m-n)/2),y)
     (return
       (simplify
	(list
	 '(mtimes)
	 (cdr (sassq 'b z 'nill))
	 '((rat simp) 1 2)
	 (substint
	  (list '(mtimes) 2 var)
	  'x
	  (integrator (simplify (cond ((< m n)
				       (subliss z
						'((mtimes)
						  ((mexpt)
						   ((mtimes)
						    ((rat simp) 1 2)
						    ((%sin) x))
						   m)
						  ((mexpt)
						   ((mplus)
						    ((rat simp) 1 2)
						    ((mtimes)
						     ((rat simp) 1 2)
						     ((%cos) x)))
						   a))))
				      (t (subliss z
						  '((mtimes)
						    ((mexpt)
						     ((mtimes)
						      ((rat simp) 1 2)
						      ((%sin) x))
						     n)
						    ((mexpt)
						     ((mplus)
						      ((rat simp) 1 2)
						      ((mtimes)
						       ((rat simp)
							-1
							2)
						       ((%cos) x)))
						     a))))))
		      'x)))))
     l1
     ;; I think this is case IV, working on the expression in terms of
     ;; sin and cos.

     ;; Elem(x) means constants, x, trig functions of x, log and
     ;; inverse trig functions of x, and which are closed under
     ;; addition, multiplication, exponentiation, and substitution.
     ;;
     ;; Elem(f(x)) is the same as Elem(x), but f(x) replaces x in the
     ;; definition.

     #+nil
     (format t "Case IV~%")
     (setq *c* -1)
     (setq *a* 'sin*)
     (setq *b* 'cos*)
     (cond ((and (m2 y '((coeffpt) (c rat1) ((mexpt) cos* (n odd1))) nil)
		 (setq repl (list '(%sin) var)))
	    ;; The case cos^(2*n+1)*Elem(cos^2,sin).  Use the
	    ;; substitution z = sin.
	    #+nil
	    (format t "Case cos^(2*n+1)*Elem(cos^2,sin)~%")
	    (go getout)))
     (setq *a* *b*)
     (setq *b* 'sin*)
     (cond ((and (m2 y '((coeffpt) (c rat1) ((mexpt) sin* (n odd1))) nil)
		 (setq repl (list '(%cos) var)))
	    ;; The case sin^(2*n+1)*Elem(sin^2,cos).  Use the
	    ;; substitution z = cos.
	    #+nil
	    (format t "Case sin^(2*n+1)*Elem(sin^2,cos)~%")
	    (go get3)))
     ;; Case V
     ;;
     ;; Transform sin and cos to tan and sec to see if the integral is
     ;; of the form Elem(tan, sec^2).  If so, use the substitution z =
     ;; tan.
     #+nil
     (format t "Case V~%")
     (setq y (simplify (subliss '((sin* (mtimes) tan* ((mexpt) sec* -1))
				  (cos* (mexpt) sec* -1))
				y2)))
     (setq *c* 1)
     (setq *a* 'tan*)
     (setq *b* 'sec*)
     (cond ((and (rat1 y) (setq repl (list '(%tan) var)))
	    (go get1)))
     (setq *a* *b*)
     (setq *b* 'tan*)
     (cond ((and (m2 y '((coeffpt) (c rat1) ((mexpt) tan* (n odd1))) nil)
		 (setq repl (list '(%sec) var)))
	    (go getout)))
     (cond ((not (alike1 (setq repl ($expand exp)) exp))
	   (return (integrator repl var))))
     (setq y (simplify (subliss '((sin* (mtimes)
				   2
				   x
				   ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))
				  (cos* (mtimes)
				   ((mplus)
				    1
				    ((mtimes) -1 ((mexpt) x 2)))
				   ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1)))
				y1)))
     (setq y (list '(mtimes) y '((mtimes) 2 ((mexpt) ((mplus) 1 ((mexpt) x 2)) -1))))
     (setq repl (subvar '((mquotient)
			  ((%sin) x)
			  ((mplus) 1 ((%cos) x)))))
     (go get2)
     get3 (setq y (list '(mtimes) -1 *yy* *yz*))
     (go get2)
     get1 (setq y (list '(mtimes) '((mexpt) ((mplus) 1 ((mexpt) x 2)) -1) *yy*))
     (go get2)
     getout
     (setq y (list '(mtimes) *yy* *yz*))
     get2 (setq y (simplify y))
     (return (substint repl 'x (integrator y 'x)))))

(defmvar $integration_constant_counter 0)
(defmvar $integration_constant '$%c)

;; This is the top level of the integrator
(defmfun sinint (exp var)
  ;; *integrator-level* is a recursion counter for INTEGRATOR.  See
  ;; INTEGRATOR for more details.  Initialize it here.
  (let ((*integrator-level* 0))
    (declare (special *integrator-level*))
    (cond ((mnump var) (merror "Attempt to integrate wrt a number: ~:M" var))
	  (($ratp var) (sinint exp (ratdisrep var)))
	  (($ratp exp) (sinint (ratdisrep exp) var))
	  ((mxorlistp exp)    ;; if exp is an mlist or matrix
	   (cons (car exp)
		 (mapcar #'(lambda (y) (sinint y var)) (cdr exp))))
	  ;; if exp is an equality, integrate both sides
	  ;; and add an integration constant
	  ((mequalp exp)
	   (list (car exp) (sinint (cadr exp) var)
		 (add (sinint (caddr exp) var)
	      ($concat $integration_constant (incf $integration_constant_counter)))))
	  ((and (atom var)
		(isinop exp var))
	   (list '(%integrate) exp var))
	  ((let ((ans (simplify
		       (let ($opsubst varlist genvar stack)
			 (integrator exp var)))))
	     (if (sum-of-intsp ans)
		 (list '(%integrate) exp var)
		 ans))))))

(defun sum-of-intsp (ans)
  (cond ((atom ans) (not (eq ans var)))
	((mplusp ans) (every #'sum-of-intsp (cdr ans)))
	((eq (caar ans) '%integrate) t)
	((mtimesp ans)
	 (do ((facs (cdr ans) (cdr facs))
	      (ints))
	     ((null facs)
	      (< (length ints) 2))
	   (unless (freeof var (car facs))
	     (if (sum-of-intsp (car facs))
		 (push (car facs) ints)
		 (return nil)))))
	((freeof var ans) t)
	(t nil)))

(defun intsum (form var)
  (prog (exp idx ll ul pair val)
     (setq exp (cadr form)
	   idx (caddr form)
	   ll (cadddr form)
	   ul (car (cddddr form)))
     (if (or (not (atom var))
	     (not (free idx var))
	     (not (free ll var))
	     (not (free ul var)))
	 (return (list '(%integrate) form var)))
     (setq pair (partition exp var 1))
     (when (and (mexptp (cdr pair))
		(eq (caddr pair) var))
       (setq val (maxima-substitute ll idx (cadddr pair)))
       (cond ((equal val -1)
	      (return (add (integrator (maxima-substitute ll idx exp) var)
			    (intsum1 exp idx (add 1 ll) ul var))))
	     ((mlsp val -1)
	      (return (list '(%integrate) form var)))))
     (return (intsum1 exp idx ll ul var))))

(defun intsum1 (exp idx ll ul var)
  (assume (list '(mgeqp) idx ll))
  (if (not (eq ul '$inf))
      (assume (list '(mgeqp) ul idx)))
  (simplifya (list '(%sum) (integrator exp var) idx ll ul) t))

(defun rat8prime (c)
  (and (rat8 c)
       (or (not (mnump c))
	   (not (zerop1 c)))))

(defun finds (x)
  (if (atom x)
      (member x '(%log %integrate %atan) :test #'eq)
      (or (finds (car x)) (finds (cdr x)))))

;;; ratlog is called for an expression containing the log or atan function
;;; The integrand is like log(x)*f'(x). To obtain the result the technique of
;;; partial integration is applied: log(x)*f(x)-integrate(1/x*f(x),x)

(defun ratlog (exp var form)
  (prog (*a* *b* *c* *d* y z w)
     (setq y form)
     (setq *b* (cdr (sassq 'b y 'nill)))
     (setq *c* (cdr (sassq 'c y 'nill)))
     (setq y (integrator *c* var))
     (cond ((finds y) (return nil)))
     (setq *d* (sdiff (cdr (sassq 'a form 'nill))
		    var))

     (setq z (integrator (mul2* y *d*) var))
     (setq *d* (cdr (sassq 'a form 'nill)))
     (return (simplify (list '(mplus)
			     (list '(mtimes) y *d*)
			     (list '(mtimes) -1 z))))))

;;; partial-integration is an extension of the algorithm of ratlog to support
;;; the technique of partial integration for more cases. The integrand
;;; is like g(x)*f'(x) and the result is g(x)*f(x)-integrate(g'(x)*f(x),x).
;;;
;;; Adding integrals properties for elementary functions led to infinite recursion 
;;; with integrate(z*expintegral_shi(z),z). This was resolved by limiting the 
;;; recursion depth. *integrator-level* needs to be at least 3 to solve 
;;;  o  integrate(expintegral_ei(1/sqrt(x)),x)
;;;  o  integrate(sqrt(z)*expintegral_li(z),z)
;;; while a value of 4 causes testsuite regressions with 
;;;  o  integrate(z*expintegral_shi(z),z)
(defun partial-integration (form var)
  (let ((g  (cdr (assoc 'a form)))   ; part g(x)
	(df (cdr (assoc 'c form)))   ; part f'(x)
	(f  nil))
    (setq f (integrator df var))     ; integrate f'(x) wrt var
    (cond
      ((or (isinop f '%integrate)    ; no result or
	   (isinop f (caar g))       ; g in result
	   (> *integrator-level* 3))
       nil)                          ; we return nil
      (t
       ;; Build the result: g(x)*f(x)-integrate(g'(x)*f(x))
       (add (mul f g)
	    (mul -1 (integrator (mul f (sdiff g var)) var)))))))

;; returns t if argument of every trig operation in y matches arg
(defun every-trigarg-alike (y arg)
  (cond ((atom y) t)
	((optrig (caar y)) (alike1 arg (cadr y)))
	(t (every (lambda (exp)
		    (every-trigarg-alike exp arg))
		  (cdr y)))))

;; return argument of first trig operation encountered in y
(defun find-first-trigarg (y)
  (cond ((atom y) nil)
	((optrig (caar y)) (cadr y))
	(t (some (lambda (exp)
		   (find-first-trigarg exp))
		 (cdr y)))))


(defun matchsum (alist blist)
  (prog (r s *c* *d*)
     (setq s (m2 (car alist)
		 '((mtimes)
		   ((coefftt) (a freevar))
		   ((coefftt) (c true)))
		 nil))
     (setq *c* (cdr (sassq 'c s 'nill)))
     (cond ((not (setq r
		       (m2 (cons '(mplus) blist)
			   (list '(mplus)
				 (cons '(mtimes)
				       (cons '((coefftt) (b free1))
					     (cond ((mtimesp *c*)
						    (cdr *c*))
						   (t (list *c*)))))
				 '(d true))
			   nil)))
	    (return nil)))
     (setq *d* (simplify (list '(mtimes)
			     (subliss s 'a)
			     (list '(mexpt)
				   (subliss r 'b)
				   -1))))
     (cond ((m2 (cons '(mplus) alist)
		(timesloop *d* blist)
		nil)
	    (return *d*))
	   (t (return nil)))))

(defun timesloop (a b)
  (cons '(mplus) (mapcar #'(lambda (c) (mul2* a c)) b)))

(defun simplog (a)
  (simplifya (cons '(%log) a) nil))

(defun expands (aa b)
  (addn (mapcar #'(lambda (c) (timesloop c aa)) b) nil))

(defun powerlist (exp var)
  (prog (y *c* *d* powerlist *b*)
     (setq y (m2 exp
		 '((mtimes)
		   ((mexpt) (var varp) (c integerp2))
		   ((coefftt) (a freevar))
		   ((coefftt) (b true)))
		 nil))
     (setq *b* (cdr (sassq 'b y 'nill)))
     (setq *c* (cdr (sassq 'c y 'nill)))
     (unless  (rat10 *b*) (return nil))
     (setq *d* (listgcd (cons (1+ *c*) powerlist)))
     (cond ((or (null *d*) (zerop *d*)) (return nil)))
     (return
       (substint
	(list '(mexpt) var *d*)
	var
	(integrate5 (simplify (list '(mtimes)
				    (power* *d* -1)
				    (cdr (sassq 'a y 'nill))
				    (list '(mexpt) var (1- (quotient (1+ *c*) *d*)))
				    (subst10 *b*)))
		    var)))))

;; This is the derivative-divides algorithm of Moses.
;;
;;                /
;;                [
;; Look for form  I  c * op(u(x)) * u'(x) dx
;;                ]
;;                /
;;
;;  where:  c     is a constant
;;          u(x)  is an elementary expression in x
;;          u'(x) is its derivative
;;          op    is an elementary operator:
;;                - the indentity, or
;;                - any function that can be integrated by INTEGRALLOOKUPS
;;
;; The method of solution, once the problem has been determined to
;; posses the form above, is to look up OP in a table and substitute
;; u(x) for each occurrence of x in the expression given in the table.
;; In other words, the method performs an implicit substitution y = u(x),
;; and obtains the integral of op(y)dy by a table look up.
;;
(defun diffdiv (exp var)
  (prog (y *a* x v *d* z w r)
     (cond ((and (mexptp exp)
		 (mplusp (cadr exp))
		 (integerp (caddr exp))
		 (< (caddr exp) 6)
		 (> (caddr exp) 0))
	    (return (integrator (expandexpt (cadr exp) (caddr exp)) var))))

     ;; If not a product, transform to a product with one term
     (setq exp (cond ((mtimesp exp) exp) (t (list '(mtimes) exp))))

     ;; Loop over the terms in exp
     (setq z (cdr exp))
     a    (setq y (car z))

     ;; This m2 pattern matches const*(exp/y)
     (setq r (list '(mplus)
		   (cons '(coeffpt)
			 (cons '(c free1)
			       (choicesin y (cdr exp))))))
     (cond
      ;; Case u(var) is the identity function. y is a term in exp.
      ;; Match if diff(y,var) == c*(exp/y).
      ;; This even works when y is a function with multiple args.
       ((setq w (m2 (sdiff y var) r nil))
	(return (muln (list y y (power* (mul2* 2 (cdr (sassq 'c w 'nill))) -1)) nil))))

     ;; w is the arg in y.
     (let ((arg-freevar))
       (setq w
	 (cond
	  ((or (atom y) (member (caar y) '(mplus mtimes) :test #'eq)) y)
	  ;; Take the argument of a function with one value.
	  ((= (length (cdr y)) 1) (cadr y))
	  ;; A function has multiple args, and exactly one arg depends on var
	  ((= (count-if #'null (setq arg-freevar (mapcar #'freevar (cdr y)))) 1)
	   (do ((args (cdr y) (cdr args))
		(argf arg-freevar (cdr argf)))
	       ((if (not (car argf)) (return (car args))))))
	  (t 0))))

     (cond
       ((setq w (cond ((and (setq x (sdiff w var))
			    (mplusp x)
			    (setq *d* (choicesin y (cdr exp)))
			    (setq v (car *d*))
			    (mplusp v)
			    (not (cdr *d*)))
		       (cond ((setq *d* (matchsum (cdr x) (cdr v)))
			      (list (cons 'c *d*)))
			     (t nil)))
		      (t (m2 x r nil))))
	(return (cond ((null (setq x (integrallookups y))) nil)
		      ((eq w t) x)
		      (t (mul2* x (power* (cdr (sassq 'c w 'nill)) -1)))))))
     (setq z (cdr z))
     (cond ((null z) (return nil)))
     (go a)))

(defun subliss (a b)
  "A is alist consisting of a variable (symbol) and its value.  B is
  an expression.  For each entry in A, substitute the corresponding
  value into B."
  (prog (x y z)
     (setq x b)
     (setq z a)
   loop
     (when (null z) (return x))
     (setq y (car z))
     (setq x (maxima-substitute (cdr y) (car y) x))
     (setq z (cdr z))
     (go loop)))

(defun substint (x y expres)
  (cond ((and (not (atom expres))
	      (eq (caar expres) '%integrate))
	 (list (car expres) exp var))
	(t (substint1 (maxima-substitute x y expres)))))

(defun substint1 (exp)
  (cond ((atom exp) exp)
	((and (eq (caar exp) '%integrate)
	      (null (cdddr exp))
	      (not (symbolp (caddr exp)))
	      (not (free (caddr exp) var)))
	 (simplify (list '(%integrate)
			 (mul2 (cadr exp) (sdiff (caddr exp) var))
			 var)))
	(t (recur-apply #'substint1 exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;:; Extension of the integrator for more integrals with power functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Recognize a^(b*(z^r)^p+d)

(defun m2-exp-type-1 (expr)
  (m2 expr
    '((mexpt)
	(a freevar0)
	((mplus)
	   ((coefft)
	      (b freevar0)
	      ((mexpt)
		 ((mexpt) (z varp) (r freevar0))
		 (p freevar0)))
	   ((coeffpp) (d freevar))))
    nil))

;;; Recognize z^v*a^(b*z^r+d)

(defun m2-exp-type-2 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (v nonzerp))
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coefft) (b freevar0) ((mexpt) (z varp) (r freevar0)))
	      ((coeffpp) (d freevar)))))
    nil))

;;; Recognize (a*z+b)^p*%e^(c*z+d)

(defun m2-exp-type-3 (expr)
  (m2 expr
    '((mtimes)
	((mexpt)
	   ((mplus)
	      ((coefft) (a freevar0) (z varp))
	      ((coeffpp) (b freevar)))
	   (p freevar0))
      ((mexpt)
	 $%e
	 ((mplus)
	    ((coefft) (c freevar0) (z varp))
	    ((coeffpp) (d freevar)))))
     nil))

;;; Recognize d^(a*z^2+b/z^2+c)

(defun m2-exp-type-4 (expr)
  (m2 expr
    '((mexpt)
	(d freevar0)
	((mplus)
	   ((coefft) (a freevar0) ((mexpt) (z varp) 2))
	   ((coefft) (b freevar0) ((mexpt) (z varp) -2))
	   ((coeffpp) (c freevar))))
    nil))

;;; Recognize z^(2*n)*d^(a*z^2+b/z^2+c)

(defun m2-exp-type-4-1 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar0))
	((mexpt)
	   (d freevar0)
	   ((mplus)
	      ((coefft)  (a freevar0) ((mexpt) (z varp) 2))
	      ((coefft)  (b freevar0) ((mexpt) (z varp) -2))
	      ((coeffpp) (c freevar)))))
    nil))

;;; Recognize z^n*d^(a*z^2+b*z+c)

(defun m2-exp-type-5 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar0))
	((mexpt)
	   (d freevar0)
	   ((mplus)
	      ((coefft) (a freevar0) ((mexpt) (z varp) 2))
	      ((coefft) (b freevar0) (z varp))
	      ((coeffpp) (c freevar)))))
    nil))

;;; Recognize z^n*d^(a*sqrt(z)+b*z+c)

(defun m2-exp-type-6 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar0))
	((mexpt)
	   (d freevar0)
	   ((mplus)
	      ((coefft) (a freevar0) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coefft) (b freevar0) (z varp))
	      ((coeffpp) (c freevar)))))
     nil))

;;; Recognize z^n*a^(b*z^r+e)*h^(c*z^r+g)

(defun m2-exp-type-7 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar))
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coefft)
		 (b freevar0)
		 ((mexpt) (z varp) (r freevar0)))
	      ((coeffpp) (e freevar))))
	((mexpt)
	   (h freevar0)
	   ((mplus)
	      ((coefft)
		 (c freevar0)
		 ((mexpt) (z varp) (r1 freevar0)))
	      ((coeffpp) (g freevar)))))
    nil))

;;; Recognize a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)

(defun m2-exp-type-8 (expr)
  (m2 expr
    '((mtimes)
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coeffpt) (b freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt) (d freevar) (z varp))
	      ((coeffpp) (e freevar))))
	((mexpt)
	   (h freevar0)
	   ((mplus)
	      ((coeffpt) (c freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt) (f freevar) (z varp))
	      ((coeffpp) (g freevar)))))
    nil))

;;; Recognize z^n*a^(b*z^2+d*z+e)*h^(c*z^2+f*z+g)

(defun m2-exp-type-9 (expr)
  (m2 expr
    '((mtimes)
      ((mexpt) (z varp) (n freevar))
      ((mexpt)
	 (a freevar0)
	 ((mplus)
	    ((coeffpt)  (b freevar) ((mexpt) (z varp) 2))
	    ((coeffpt)  (d freevar) (z varp))
	    ((coeffpp) (e freevar))))
      ((mexpt)
	 (h freevar0)
	 ((mplus)
	    ((coeffpt)  (c freevar) ((mexpt) (z varp) 2))
	    ((coeffpt)  (f freevar) (z varp))
	    ((coeffpp) (g freevar)))))
     nil))

;;; Recognize z^n*a^(b*sqrt(z+)d*z+e)*h^(c*sqrt(z+)f*z+g)

(defun m2-exp-type-10 (expr)
  (m2 expr
    '((mtimes)
	((mexpt) (z varp) (n freevar))
	((mexpt)
	   (a freevar0)
	   ((mplus)
	      ((coeffpt)  (b freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt)  (d freevar) (z varp))
	      ((coeffpp) (e freevar))))
	((mexpt)
	   (h freevar0)
	   ((mplus)
	      ((coeffpt)  (c freevar) ((mexpt) (z varp) ((rat) 1 2)))
	      ((coeffpt)  (f freevar) (z varp))
	      ((coeffpp) (g freevar)))))
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integrate-exp-special (expr var)

  (when *debug-integrate*
    (format t "~&INTEGRATE-EXP-SPECIAL with ~A~%" expr)
    (format t "~&Factored is ~A~%" (facsum-exponent expr)))

  ;; First we factor the expression.
  (setq expr ($factor expr))

  (cond
    ((setq w (m2-exp-type-1 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (d (cdras 'd w))
	   (r (cdras 'r w))
	   (p (cdras 'p w)))

       (when *debug-integrate*
	 (format t "~&Type 1: a^(b*(z^r)^p+d) : w = ~A~%" w))

       (mul
	 (power a d)
	 (div -1 (mul p r))
	 var
	 ($gamma_incomplete
	   (div 1 (mul p r))
	   (mul -1 b (power (power var r) p) ($log a)))
	 (power
	   (mul -1 b (power (power var r) p) ($log a))
	   (div -1 (mul r p))))))

    ((setq w (m2-exp-type-2 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (d (cdras 'd w))
	   (v (cdras 'v w))
	   (r (cdras 'r w)))

       (when *debug-integrate*
	 (format t "~&Type 2: z^v*a^(b*z^r+d) : w = ~A~%" w))

       (mul
	 (div -1 r)
	 (power a d)
	 (power var (add v 1))
	 ($gamma_incomplete
	   (div (add v 1) r)
	   (mul -1 b (power var r) ($log a)))
	 (power
	   (mul -1 b (power var r) ($log a))
	   (mul -1 (div (add v 1) r))))))

    ((setq w (m2-exp-type-3 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (p (cdras 'p w)))

       (when *debug-integrate*
	 (format t "~&Type 3: (a*z+b)^p*%e^(c*z+d) : w = ~A~%" w))

       (mul
	 (div -1 a)
	 (power '$%e (sub d (div (mul b c) a)))
	 (power (add b (mul a var)) (add p 1))
	 ($expintegral_e (mul -1 p) (mul (div -1 a) c (add b (mul a var)))))))

    ((setq w (m2-exp-type-4 expr))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   ($trigsign nil)) ; Do not simplify erfc(-x) !

       (when *debug-integrate*
	 (format t "~&Type 4: d^(a*z^2+b/z^2+c) : w = ~A~%" w))

       (mul
	 (div 1 (mul 4 (power (mul -1 a ($log d)) (div 1 2))))
	 (mul
	   (power d c)
	   (power '$%pi (div 1 2))
	   (power '$%e
	     (mul -2
	       (power (mul -1 a ($log d)) (div 1 2))
	       (power (mul -1 b ($log d)) (div 1 2))))
	   (add
	     ($erfc
	       (add
		 (div (power (mul -1 b ($log d)) (div 1 2)) var)
		 (mul -1 var (power (mul -1 a ($log d)) (div 1 2)))))
	     (mul -1
	       (power '$%e
		 (mul 4
		   (power (mul -1 a ($log d)) (div 1 2))
		   (power (mul -1 b ($log d)) (div 1 2))))
	       ($erfc
		 (add
		   (mul var (power (mul -1 a ($log d)) (div 1 2)))
		   (div (power (mul -1 b ($log d)) (div 1 2)) var)))))))))

    ((and (setq w (m2-exp-type-4-1 expr))
	  ($evenp (cdras 'n w))   ; only for n an even integer
	  (symbolp (cdras 'a w))) ; a has to be a symbol
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (n (cdras 'n w))
	   ($trigsign nil)) ; Do not simplify erfc(-x) !

       (when *debug-integrate*
	 (format t "~&Type 4-1: z^(2*n)*d^(a*z^2+b/z^2+c) : w = ~A~%" w))

       (setq n (div n 2))

       (mul (div 1 4)
	    (power d c)
	    (power '$%pi (div 1 2))
	    (simplify (list '(%derivative)
	     (div
	       (sub
		 (mul
		   (power ($log d) (mul -1 n))
		   (add
		     (mul
		       (power
			 '$%e
			 (mul -2
			   (power (mul -1 a ($log d)) (div 1 2))
			   (power (mul -1 b ($log d)) (div 1 2))))
		     ($erfc
		       (sub
			 (div
			   (power (mul -1 b ($log d)) (div 1 2))
			   var)
			 (mul var (power (mul -1 ($log d)) (div 1 2))))))))
		 (mul
		   (power
		     '$%e
		     (mul 2
		       (power (mul -1 a ($log d)) (div 1 2))
		       (power (mul -1 b ($log d)) (div 1 2))))
		   ($erfc
		     (add
		       (power (mul -1 a ($log d)) (div 1 2))
		       (div (power (mul -1 b ($log d)) (div 1 2)) var)))))
	       (power (mul -1 a ($log d)) (div 1 2)))
	     a n)))))

    ((and (setq w (m2-exp-type-5 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Exponential type z^n*d^(a*z^2+b*z+c) : w = ~A~%" w))

       (mul
	 (div -1 (mul 2 (power (mul a ($log d)) (div 1 2))))
	 (mul
	   (power d (sub c (div (mul b b) (mul 4 a))))
	   (let ((index (gensumindex)))
	     (dosum
	       (mul
		 (power 2 (sub index n))
		 ($binomial n index)
		 ($gamma_incomplete
		   (div (add index 1) 2)
		   (mul
		     (div -1 (mul 4 a))
		     (power (add b (mul 2 a var)) 2)
		     ($log d)))
		 (power (mul a ($log d)) (mul -1 (add n (div 1 2))))
		 (power (mul -1 b ($log d)) (sub n index))
		 (power (mul (add b (mul 2 a var)) ($log d)) (add index 1))
		 (power
		   (mul (div -1 a) (power (add b (mul 2 a var)) 2) ($log d))
		   (mul (div -1 2) (add index 1))))
	       index 0 n t))))))

    ((and (setq w (m2-exp-type-6 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Exponential type z^n*d^(a*sqrt(z)+b*z+c) : w = ~A~%" w))

       (mul
	 (power 2 (mul -1 (add n 1)))
	 (power d (sub c (div (mul a a) (mul 4 b))))
	 (power (mul b ($log d)) (mul -2 (add n 1)))
	 (let ((index1 (gensumindex))
	       (index2 (gensumindex)))
	   (dosum
	     (dosum
	       (mul
		 (power -1 (sub index1 index2))
		 (power 4 index1)
		 ($binomial index1 index2)
		 ($binomial n index1)
		 ($log d)
		 (power (mul a ($log d)) (sub (mul 2 n) (add index1 index2)))
		 (power
		   (mul (add a (mul 2 b (power var (div 1 2)))) ($log d))
		   (add index1 index2))
		 (power
		   (mul
		     (div -1 b)
		     (power (add a (mul 2 b (power var (div 1 2)))) 2)
		     ($log d))
		   (mul (div -1 2) (add index1 index2 1)))
		 (add
		   (mul 2 b
		     (power
		       (mul
			 (div -1 b)
			 (power (add a (mul 2 b (power var (div 1 2)))) 2)
			 ($log d))
		       (div 1 2))
		     ($gamma_incomplete
		       (div (add index1 index2 2) 2)
		       (mul
			 (div -1 (mul 4 b))
			 (power (add a (mul 2 b (power var (div 1 2)))) 2)
			 ($log d))))
		   (mul a
		     (add a (mul 2 b (power var (div 1 2))))
		     ($log d)
		     ($gamma_incomplete
		       (div (add index1 index2 1) 2)
		       (mul
			 (div -1 (mul 4 b))
			 (power (add a (mul 2 b (power var (div 1 2)))) 2)
			 ($log d))))))
	       index2 0 index1 t)
	     index1 0 n t)))))

    ((and (setq w (m2-exp-type-7 (facsum-exponent expr)))
	  (eq ($sign (sub (cdras 'r w) (cdras 'r1 w))) '$zero))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (e (cdras 'e w))
	   (g (cdras 'g w))
	   (h (cdras 'h w))
	   (r (cdras 'r w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 7: z^n*a^(b*z^r+e)*h^(c*z^r+g) : w = ~A~%" w))

       (setq n (add n 1))

       (mul
	 (power var n)
	 (div -1 r)
	 (power a e)
	 (power h g)
	 (power
	   (mul -1
	     (power var r)
	     (add (mul b ($log a)) (mul c ($log h))))
	   (div (mul -1 n) r))
	 ($gamma_incomplete
	   (div n r)
	   (mul -1 (power var r) (add (mul b ($log a)) (mul c ($log h))))))))

    ((setq w (m2-exp-type-8 (facsum-exponent expr)))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (e (cdras 'e w))
	   (f (cdras 'f w))
	   (g (cdras 'g w))
	   (h (cdras 'h w)))

       (when *debug-integrate*
	 (format t "~&Type 8: a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)")
	 (format t "~&   : w = ~A~%" w))

       (mul
	 (div 1 2)
	 (power a e)
	 (power h g)
	 (add
	   (mul 2
	     (power a (add (mul b (power var (div 1 2))) (mul d var)))
	     (power h (add (mul c (power var (div 1 2))) (mul f var)))
	     (div 1 (add (mul d ($log a)) (mul f ($log h)))))
	   (mul -1
	     (power '$%pi (div 1 2))
	     (power '$%e
	       (mul -1
		 (div
		   (power (add (mul b ($log a)) (mul c ($log h))) 2)
		   (mul 4 (add (mul d ($log a)) (mul f ($log h)))))))
	     ($erfi
	       (div
		 (add
		   (mul b ($log a))
		   (mul c ($log h))
		   (mul 2
		     (power var (div 1 2))
		     (add (mul d ($log a)) (mul f ($log h)))))
		 (mul 2
		   (power (add (mul d ($log a)) (mul f ($log h))) (div 1 2)))))
	     (add (mul b ($log a)) (mul c ($log h)))
	     (power (add (mul d ($log a)) (mul f ($log h))) (div -3 2)))))))

    ((and (setq w (m2-exp-type-9 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos)
	  (or (not (eq ($sign (cdras 'b w)) '$zero))
	      (not (eq ($sign (cdras 'c w)) '$zero))))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (e (cdras 'e w))
	   (f (cdras 'f w))
	   (g (cdras 'g w))
	   (h (cdras 'h w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 9: z^n*a^(b*z^2+d*z+e)*h^(c*z^2+f*z+g)")
	 (format t "~&   : w = ~A~%" w))

       (mul
	 (div -1 2)
	 (power a e)
	 (power h g)
	 (power '$%e
	   (div
	     (power (add (mul d ($log a)) (mul f ($log h))) 2)
	     (mul -4 (add (mul b ($log a)) (mul c ($log h))))))
	 (power (add (mul b ($log a)) (mul c ($log h))) (mul -1 (add n 1)))
	 (let ((index (gensumindex)))
	   (dosum
	     (mul
	       (power 2 (sub index n))
	       ($binomial n index)
	       (power
		 (add (mul -1 d ($log a)) (mul -1 f ($log h)))
		 (sub n index))
	       (power
		 (add
		   (mul (add d (mul 2 b var)) ($log a))
		   (mul (add f (mul 2 c var)) ($log h)))
		 (add index 1))
	       (power
		 (mul -1
		   (div
		     (power
		       (add
			 (mul (add d (mul 2 b var)) ($log a))
			 (mul (add f (mul 2 c var)) ($log h)))
		       2)
		     (add (mul b ($log a)) (mul c ($log h)))))
		 (div (add index 1) -2))
	       ($gamma_incomplete
		 (div (add index 1) 2)
		 (mul -1
		   (div
		     (power
		       (add
			 (mul (add d (mul 2 b var)) ($log a))
			 (mul (add f (mul 2 c var)) ($log h)))
		       2)
		     (mul 4 (add (mul b ($log a)) (mul c ($log h))))))))
	     index 0 n t)))))

    ((and (setq w (m2-exp-type-10 (facsum-exponent expr)))
	  (maxima-integerp (cdras 'n w))
	  (eq ($sign (cdras 'n w)) '$pos)
	  (or (not (eq ($sign (cdras 'b w)) '$zero))
	      (not (eq ($sign (cdras 'c w)) '$zero))))
     (let ((a (cdras 'a w))
	   (b (cdras 'b w))
	   (c (cdras 'c w))
	   (d (cdras 'd w))
	   (e (cdras 'e w))
	   (f (cdras 'f w))
	   (g (cdras 'g w))
	   (h (cdras 'h w))
	   (n (cdras 'n w)))

       (when *debug-integrate*
	 (format t "~&Type 10: z^n*a^(b*sqrt(z)+d*z+e)*h^(c*sqrt(z)+f*z+g)")
	 (format t "~&   : w = ~A~%" w))

       (mul
	 (power 2 (add (mul -2 n) -1))
	 (power a e)
	 (power h g)
	 (power '$%e
	   (div
	     (power (add (mul b ($log a)) (mul c ($log h))) 2)
	     (mul -4 (add (mul d ($log a)) (mul f ($log h))))))
	 (power (add (mul d ($log a)) (mul f ($log h))) (mul -2 (add n 1)))
	 (let ((index1 (gensumindex))
	       (index2 (gensumindex)))
	   (dosum
	     (dosum
	       (mul
		 (power -1 (sub index1 index2))
		 (power 4 index1)
		 ($binomial index1 index2)
		 ($binomial n index1)
		 (power
		   (add (mul b ($log a)) (mul c ($log h)))
		   (sub (mul 2 n) (add index1 index2)))
		 (power
		   (add
		     (mul b ($log a))
		     (mul c ($log h))
		     (mul 2
		       (power var (div 1 2))
		       (add (mul d ($log a)) (mul f ($log h)))))
		   (add index1 index2))
		 (power
		   (mul -1
		     (div
		       (power
			 (add
			   (mul b ($log a))
			   (mul c ($log h))
			   (mul 2
			     (power var (div 1 2))
			     (add (mul d ($log a)) (mul f ($log h)))))
			 2)
		       (add (mul d ($log a)) (mul f ($log h)))))
		   (mul (div -1 2) (add index1 index2 1)))
		 (add
		   (mul
		     ($gamma_incomplete
		       (mul (div 1 2) (add index1 index2 1))
		       (mul
			 (div -1 4)
			 (div
			   (power
			     (add
			       (mul b ($log a))
			       (mul c ($log h))
			       (mul 2
				 (power var (div 1 2))
				 (add (mul d ($log a)) (mul f ($log h)))))
			     2)
			   (add (mul d ($log a)) (mul f ($log h))))))
		     (add (mul b ($log a)) (mul c ($log h)))
		     (add
		       (mul b ($log a))
		       (mul c ($log h))
		       (mul 2
			 (power var (div 1 2))
			 (add (mul d ($log a)) (mul f ($log h))))))
		   (mul 2
		     ($gamma_incomplete
		       (mul (div 1 2) (add index1 index2 2))
		       (mul
			 (div -1 4)
			 (div
			   (power
			     (add
			       (mul b ($log a))
			       (mul c ($log h))
			       (mul 2
				 (power var (div 1 2))
				 (add (mul d ($log a)) (mul f ($log h)))))
			     2)
			   (add (mul d ($log a)) (mul f ($log h))))))
		     (add (mul d ($log a)) (mul f ($log h)))
		     (power
		       (mul -1
			 (div
			   (power
			     (add
			       (mul b ($log a))
			       (mul c ($log h))
			       (mul 2
				 (power var (div 1 2))
				 (add (mul d ($log a)) (mul f ($log h)))))
			     2)
			   (add (mul d ($log a)) (mul f ($log h)))))
		       (div 1 2)))))
	       index2 0 index1 t)
	     index1 0 n t)))))
    (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Do a facsum for the exponent of power functions.
;;; This is necessary to integrate all general forms. The pattern matcher is
;;; not powerful enough to do the job.

(defun facsum-exponent (expr)
  ;; Make sure that expr has the form ((mtimes) factor1 factor2 ...)
  (when (not (mtimesp expr)) (setq expr (list '(mtimes) expr)))
  (do ((result nil)
       (l (cdr expr) (cdr l)))
      ((null l) (cons (list 'mtimes) result))
    (cond
      ((mexptp (car l))
       ;; Found an power function. Factor the exponent with facsum.
       (setq result
	     (cons
	       (cons
		 (list 'mexpt)
		 (cons
		   (cadr (car l))
		   (list (mfuncall '$facsum (caddr (car l)) var))))
	       result)))
      (t
       ;; Nothing to do.
       (setq result (cons (car l) result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

