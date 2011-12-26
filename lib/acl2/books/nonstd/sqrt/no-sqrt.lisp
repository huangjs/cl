#|					;

HISTORICAL NOTE

In ACL2 1.9 and earlier, ACL2 did not recognize the reals.  Moreover, it
asserted that the result of any arithmetic operation was numeric, and that
non-numeric arguments to arithmetic operators behaved as zero.  It was thus
able to prove that any numeric operation resulted in either a rational or
complex-rational number, and moreover that all other objects were (numerically)
equivalent to zero.  Accordingly, it was possible to prove that the square root
of two did not exist.  Basically, the square root of two cannot be a rational
number, and since all numbers are rational, it cannot exist.

This let us to extend the type system of ACL2 to include the reals.  Hence, the
original result is no longer provable.  In its stead, we now prove that

    (defthm irrational-sqrt-2
      (implies (equal (* x x) 2)
	       (and (realp x)
		    (not (rationalp x)))))

That is, the square root of two must be irrational.

What follows is the original commentary on this file (before the addition of
the reals into ACL2):

---------------------------

In this Acl2 book, we prove that the square root of two does not exist.  In
particular, we prove the following theorem:

    (defthm there-is-no-sqrt-2
      (not (equal (* x x) 2)))

Why is this important?  Well, we wondered what would happen if somebody tried
to define sqrt using something like

    (encapsulate
      ((sqrt (x) t))

      (defthm sqrt-sqrt
        (equal (* (sqrt x) (sqrt x)) x)))

Of course, Acl2 will complain, because our careless programmer failed to
provide a _witness_ function for sqrt.  That is, he failed to provide an
example of a function that would satisfy the theorem sqrt-sqrt.  While it is
often the case that providing such functions is annoying, in this case it is
crucial, since we will see that no such function exists in the Acl2 world.

Specifically, let's consider (sqrt 2).  It is well-known that this is an
irrational number.  Since the Acl2 numbers are limited to the rationals and
coplex-rationals, then it becomes obvious that (sqrt 2) can not exist in the
Acl2 universe.  The heart of the proof, then, is to show that (sqrt 2) is not
rational.

To prove that (sqrt 2) is irrational, we follow the standard proof by
contradiction.  Suppose that there were such a rational p/q so that p/q * p/q
is equal to 2.  Well, then, p^2 = 2 q^2, so we have immediately that p^2 is
even, and hence so is p.  But in that case, p^2 is a multiple of 4, so q^2 is a
multiple of 2, and hence q is also even.  But then, if we let p' = p/2 and q' =
q/2, we have that p'/q' * p'/q' = 2.  Since this argument is unchanged
regardless of p and q, it suffices only to consider p & q in lowest terms
(i.e., p, q relatively prime) to complete the proof.

The interested reader should also take a look at "iter-sqrt.lisp", in which we
prove that you _can_ get arbitrarily close to the square root.

To load this book, it is sufficient to do something like this:

    (certify-book "no-sqrt" 0 nil)

|#

(in-package "ACL2")		; We're too lazy to build our own package

(include-book "../arithmetic/top"
	      :load-compiled-file nil)

;;
;; We define the following macro purely for readability.  (divisible n p) is
;; true iff p divides n.
;;
(defmacro divisible (n d)
  `(integerp (/ ,n ,d)))

(encapsulate
 ()

 ;;
 ;; This is a handy induction scheme to reason about even numbers.
 ;;
 (local
  (defun even-induction (x)
    "Induct by going two steps at a time"
    (if (or (zp x) (equal x 1))
	x
      (1+ (even-induction (1- (1- x)))))))

 ;;
 ;; This is really the main theorem of this encapsulate.  What he have is that
 ;; for non-negative numbers p, if p*p is even, then p must be even as well.
 ;; We were somewhat surprised that proving this fact isn't really trivial.
 ;; The reason is that the "obvious" proof rewrites "p" into "2n+1".  In turn,
 ;; this means invoking the remainder theorem and so on.  It was simply easier
 ;; to prove it by induction!
 ;;
 (local
  (defthm even-square-implies-even-1
    (implies (and (integerp p)
		  (<= 0 p)
		  (divisible (* p p) 2))
	     (divisible p 2))
    :hints (("Goal"
	     :induct (even-induction p)))
    :rule-classes nil))

 ;;
 ;; We don't really need to consider negative numbers here, but it makes life
 ;; much simpler later if we can get rid of the non-negative hypothesis.  So,
 ;; we prove the equivalent result here for negative numbers here.  We can
 ;; simply invoke the previous theorem, since -p * -p is equal to p*p and -p
 ;; is even iff p is even.
 ;;
 (local
  (defthm even-square-implies-even-2
    (implies (and (integerp p)
		  (<= p 0)
		  (divisible (* p p) 2))
	     (divisible p 2))
    :hints (("Goal"
	     :use (:instance even-square-implies-even-1 (p (- p)))))
    :rule-classes nil))

 ;;
 ;; Now, we can "export" the useful theorem that all integers are even if their
 ;; squares are even.
 ;;
 (defthm even-square-implies-even
   (implies (and (integerp p)
		 (divisible (* p p) 2))
	    (divisible p 2)) 
   :hints (("Goal"
	    :use ((:instance even-square-implies-even-1)
		  (:instance even-square-implies-even-2))))))

;;
;; Surprisingly, we found we needed the following rewrite rule.  Surely there's
;; a better way.
;;
(defthm integers-closed-under-square
  (implies (integerp p)
	   (integerp (* p p))))

;;
;; After showing that "p" is even from "p*p = 2*q*q", we need to turn around
;; and show that "q" is even.  To do that, we show that "p*p" is a multiple of
;; 4, and divide both sides by 2.  Here's the first step.
;;
(defthm even-implies-square-multiple-of-4
  (implies (and (integerp p)
		(divisible p 2))
	   (divisible (* p p) 4))
  :hints (("Goal'"
	   :use (:instance integers-closed-under-square (p (* p 1/2)))
	   :in-theory (disable integers-closed-under-square))))


(encapsulate
 ()

 ;;
 ;; Now, we have enough machinery to prove that both p and q are even from the
 ;; equation "p*p = 2*q*q".  We start by showing that in fact, p*p is even
 ;; follows naturally from the above equation.  It's a bit surprising that this
 ;; isn't obvious to Acl2.
 ;;
 (local
  (defthm aux-1
    (implies (and (integerp q)
		  (equal p (* 2 (* q q))))
	     (and (integerp p)
		  (divisible p 2)))))
 ;;
 ;; Acl2 gets lost in the next proof because it doesn't know to divide by 2
 ;; at the crucial step.  So, we prove this nifty rewrite rule....
 ;;
 (local
  (defthm aux-2
    (implies (equal (* p p) (* 2 q q))
	     (equal (* 1/2 q q) (* 1/4 p p)))))

 ;;
 ;; And now, here's a crucial component.  We show that q*q must be even.
 ;;
 (local
  (defthm aux-3
    (implies (and (integerp p)
		  (integerp q)
		  (equal (* p p) (* 2 (* q q))))
	     (divisible (* q q) 2))
    :hints (("goal"
	     :use ((:instance even-square-implies-even)
		   (:instance aux-1 (p (* p p ))))
	     :in-theory (disable even-square-implies-even aux-1))
	    ("goal'5'"
	     :use (:instance even-implies-square-multiple-of-4)
	     :in-theory (disable even-implies-square-multiple-of-4)))
    :rule-classes nil))

 ;;
 ;; From the above, it is a simple corollary that q is even, and so we have our
 ;; first major lemma.  Hmmm, maybe we should learn how to use the :corollary
 ;; "hint".
 ;;
 (defthm sqrt-lemma-1.1
   (implies (and (integerp p)
		 (integerp q)
		 (equal (* p p) (* 2 (* q q))))
	    (divisible q 2))
   :hints (("goal"
	    :use ((:instance aux-3)
		  (:instance even-square-implies-even (p q))))))

 ;;
 ;; The second key lemma is that p is also even.  This is much simpler!
 ;;
 (defthm sqrt-lemma-1.2
   (implies (and (integerp p)
		 (integerp q)
		 (equal (* p p) (* 2 (* q q))))
	    (divisible p 2))
   :hints (("goal"
	    :use ((:instance even-square-implies-even)
		  (:instance aux-1 (p (* p p ))))
	    :in-theory (disable even-square-implies-even aux-1)))))

(encapsulate
 ()

 ;;
 ;; Now, we're ready to instantiate our results with the numerator and
 ;; denominator of the alleged square root of x.  In Acl2, we're already
 ;; guaranteed that the numerator and denominator are relatively prime, so if
 ;; we can apply the previous lemmas to them, we'll be done.
 ;;
 ;; First, however, we have to convert the equation x*x = 2 into the friendlier
 ;; (numerator x) * (numerator x) = 2 * (denominator x) * (denominator x).  We
 ;; start out with this simple cancellation lemma.  We do this for no better
 ;; reason than to give it a name, so we can refer to it later in a :use hint.
 ;;
 (local
  (defthm aux-1
    (implies (equal x y)
	     (equal (* x z) (* y z)))
    :rule-classes nil))

 ;;
 ;; Now, for the crucial step.  We show Acl2 how to move the q*q from one side
 ;; of the equation to the other.
 ;;
 (local
  (defthm aux-2
    (implies (and (integerp p)
		  (integerp q)
		  (> q 0)
		  (equal (* (/ p q) (/ p q)) 2))
	     (equal (* p p) (* 2 q q)))
    :hints (("goal''"
	     :use (:instance aux-1
			     (x (* p p (/ q) (/ q)))
			     (y 2)
			     (z (* q q)))))))

 ;;
 ;; Finally, we need only instantiate the previous theorem with (numerator x)
 ;; and (denominator x) to get our result.
 ;;
 (defthm sqrt-lemma-1.3
   (implies (and (rationalp x)
		 (equal (* x x) 2))
	    (equal (* (numerator x) (numerator x))
		   (* 2 (* (denominator x) 
			   (denominator x)))))
   :hints (("Goal"
	    :use (:instance aux-2
			    (p (numerator x))
			    (q (denominator x)))))))

;;
;; And here is the mathematical high point.  We're really doing hothing more
;; than putting together the previous results into a single theorem.
;;
(defthm sqrt-lemma-1.4
  (implies (and (rationalp x)
		(equal (* x x) 2))
	   (and (divisible (numerator x) 2)
		(divisible (denominator x) 2)))
  :hints (("Goal"
	   :use ((:instance sqrt-lemma-1.1
			    (p (numerator x))
			    (q (denominator x)))
		 (:instance sqrt-lemma-1.2
			    (p (numerator x))
			    (q (denominator x)))))))

;;
;; And now, we need only invoke the property that the numerator and denominator
;; are relatively prime to show that no such x can exist.  Sadly, this was more
;; difficult than we thought.  From the documentation, all I could gather was
;; that (numerator x) and (denominator x) were in "lowest terms", but if so
;; Acl2 didn't seem to believe it.  Worse, the one key theorem according to the
;; docs was that the numerator for non-rationals was zero.  Searching the
;; released Acl2 "books", I found no relevant theorem.  Luckily, however, Acl2
;; sources _are_ released as well.  Digging in "axioms.lisp", I found the
;; following beauty:
;;
;;      (defaxiom Lowest-Terms
;;        (implies (and (integerp n)
;;                      (rationalp x)
;;                      (integerp r)
;;                      (integerp q)
;;                      (< 0 n)
;;                      (equal (numerator x) (* n r))
;;                      (equal (denominator x) (* n q)))
;;                 (equal n 1))
;;        :rule-classes nil)
;;
;; This was just what I needed -- and the :rule-classes nil explained why Acl2
;; seemed oblivious to it....
;;
(defthm sqrt-lemma-1.5
  (implies (and (divisible (numerator x) 2)
		(divisible (denominator x) 2))
	   (not (rationalp x)))
  :hints (("Goal"
	   :use (:instance Lowest-Terms
			   (n 2)
			   (r (/ (numerator x) 2))
			   (q (/ (denominator x) 2)))))
  :rule-classes nil)

;;
;; So finally, we can prove that the square root of two is irrational by simple
;; propositional rewriting.
;;
(defthm sqrt-2-is-not-rationalp
  (implies (rationalp x)
	   (not (equal (* x x) 2)))
  :hints (("Goal"
	   :use ((:instance sqrt-lemma-1.4)
		 (:instance sqrt-lemma-1.5)))))

;;
;; Next, we turn our attention to the complex numbers.  These are fairly easy
;; to dismiss as candidates for the square root of two, for the following
;; reasons.  First of all, the only time the square of a complex is real is
;; when its real or imaginary parts are zero.  Since Acl2 complex numbers
;; have non-zero imaginary parts, this means that the only candidates for
;; square roots of real numbers are pure imaginary numbers.  But all these have
;; negative squares, and so none of them can be the square root of 2.
;;
;; Unfortunately, Acl2 doesn't seem happy reasoning about complex arithmetic.
;; Part of the reason is that the following axiom is disabled:
;;      
;;      (defaxiom complex-definition
;;        (implies (and (realp x)
;;                      (realp y))
;;                 (equal (complex x y)
;;                        (+ x (* #c(0 1) y))))
;;        :rule-classes nil)
;;
;; So, we start out by defining what complex squares look like....
;;
(encapsulate
 ()
 
 ;;
 ;; First, we show Acl2 how to rewrite complex squares into its complex form.
 ;;
 (local
  (defthm complex-square-definition-1
    (equal (* (+ x (* #c(0 1) y))
	      (+ x (* #c(0 1) y)))
	   (+ (- (* x x) (* y y))
	      (* #c(0 1) (+ (* x y) (* x y)))))
    :rule-classes nil))
	  
 ;;
 ;; Now, we can use the theorem above to show how to rewrite complex squares.
 ;;
 (local
  (defthm complex-square-definition-2
    (implies (and (realp x)
		  (realp y))
	     (equal (* (+ x (* #c(0 1) y))
		       (+ x (* #c(0 1) y)))
		    (complex (- (* x x) (* y y))
			     (+ (* x y) (* x y)))))
    :hints (("Goal"
	     :use ((:instance complex-square-definition-1)
		   (:instance complex-definition
			      (x (- (* x x) (* y y)))
			      (y (+ (* x y) (* x y)))))))
    :rule-classes nil))
	  
 ;;
 ;; Finally, we can characterize complex squares.  Perhaps we should enable a
 ;; rule like this to reduce complex multiplication?
 ;;
 (defthm complex-square-definition
   (implies (and (realp x)
		 (realp y))
	    (equal (* (complex x y) (complex x y))
		   (complex (- (* x x) (* y y))
			    (+ (* x y) (* x y)))))
   :hints (("Goal"
	    :use ((:instance complex-definition)
		  (:instance complex-square-definition-2))))
   :rule-classes nil))

;;
;; Now that we know how to square complex numbers, we can show that if a
;; complex square is rational, then the complex number was a pure imaginary. 
;;
(encapsulate
 ()
 
 ;;
 ;; First, we use the complex square definition to find the imaginary part of
 ;; the complex square -- this part must be zero for the square to be rational
 ;;
 (local
  (defthm complex-squares-real-iff-imaginary-aux
    (implies (and (complexp x)
		  (realp (* x x)))
	     (equal (+ (* (realpart x) (imagpart x))
		       (* (realpart x) (imagpart x)))
		    0))
    :hints (("Goal"
	     :use (:instance complex-square-definition
			     (x (realpart x))
			     (y (imagpart x)))))
    :rule-classes nil))

  ;;
  ;; Surely there's a better way!  I need to give Acl2 a hint here, so I have
  ;; to name this, well, silly lemma.
  ;;
  (local
   (defthm silly
     (implies (and (realp x)
		   (equal (+ x x) 0))
	      (= x 0))
     ;; Added by Matt K. for v2-7.
     :rule-classes nil))

   ;;
   ;; And now, a key result.  Only the imaginary numbers are good candidates
   ;; for the square root of 2.
   ;;
   (defthm complex-squares-real-iff-imaginary
     (implies (and (complexp x)
		   (realp (* x x)))
	      (equal (realpart x) 0))
     :hints (("Goal"
	      :use ((:instance complex-squares-real-iff-imaginary-aux)
		    (:instance silly (x (* (realpart x) (imagpart x)))))))))

;;
;; We're almost done.  The only candidates left are the imaginary numbers, but
;; we can rule these out, since all their squares are negative.
;;
(defthm imaginary-squares-are-negative
  (implies (and (complexp x)
		(equal (realpart x) 0))
	   (< (* x x) 0))
  :hints (("Goal"
	   :use (:instance complex-square-definition
			   (x 0)
			   (y (imagpart x))))))

;;
;; Simple propositional suffices now to show that the complex numbers weren't
;; good candidates for the square root of two.
;;
(defthm sqrt-2-is-not-complexp
  (implies (complexp x)
	   (not (equal (* x x) 2)))
  :hints (("Goal"
	   :use ((:instance complex-squares-real-iff-imaginary)
		 (:instance imaginary-squares-are-negative)))))


#|
This result is no longer provable, since we added the reals to ACL2's type
system.

;;
;; And finally, the main result.  There is no square root of two.  We prove
;; this by cases.  If "x" is a number, then it's either rational or
;; complex-rational.  But, we already know the square root of two is neither
;; rational nor complex-rational.  To complete the proof, we consider the
;; non-numeric objects of the Acl2 universe, but of course, none of these can
;; be the square root of two, since their square is zero.
;;
(defthm there-is-no-sqrt-2
  (not (equal (* x x) 2))
  :hints (("Goal"
	   :cases ((rationalp x) (complex-rationalp x)))))

|#

(defthm irrational-sqrt-2
  (implies (equal (* x x) 2)
	   (and (realp x)
		(not (rationalp x))))
  :hints (("Goal"
	   :cases ((rationalp x) (complexp x)))))
