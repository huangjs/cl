#|					;
In this Acl2 book, we prove that the square root function can be approximated
in Acl2.  In particular, we prove the following theorem:

 (defthm convergence-of-iter-sqrt
   (implies (and (rationalp x)	
		 (rationalp epsilon)
		 (< 0 epsilon)
		 (<= 0 x))
	    (and (<= (* (iter-sqrt x epsilon) (iter-sqrt x epsilon)) x)
		 (< (- x (* (iter-sqrt x epsilon) (iter-sqrt x epsilon)))
		    epsilon))))

That is, for any non-negative number, we can approximate its square root within
an arbitrary positive epsilon.

The proof follows by introducing the bisection algorithm to approximate square
root.  Our bisection function looks like

    (iterate-sqrt-range low high x num-iters)

where low/high define the range being bisected, x is the number in whose square
root we're interested, and num-iters is the total number of iterations we wish
to perform.  This function returns a pair of numbers low' and high' which
define the resulting range.

The proof consists of two parts.  First of all, we show that for any high/low
range resulting from iterate-sqrt-range, if |high-low| is sufficiently small
(i.e., less than delta, which depends on epsilon) then |x-a^2| is less than
epsilon.  Secondly, we show that for any desired delta, we can find an adequate
num-iters so that calling iterate-sqrt-range with num-iters will result in a
high' and low' returned with |high'-low'| < delta.  The two proofs combined
give us our convergence result.

Note that we did not do the more natural convergence result, namely that after
iterating a number of times |low'-(sqrt x)| < epsilon.  The reason is that the
number (sqrt x) may not exist.  This result is shown in the companion book
"no-sqrt.lisp".  However, for those cases where (sqrt x) does exist, our proof
will naturally imply that (since we'll have a < (sqrt x) < b as a consequence
of a^2 < x < b^2 and also that |b-a| < delta < epsilon).  The interested reader
is encouraged to try that result.

To load this book, it is sufficient to do something like this:

    (certify-book "iter-sqrt" 0 nil)

|#

(in-package "ACL2")		; We're too lazy to build our own package

(include-book "../arithmetic/top"
	      :load-compiled-file nil)

;;
;; We start out by defining the bisection approximation to square root.  At one
;; time, we experimented with making this function return two values, but that
;; led to all sorts of confusion.  So, we now return the cons of the new low
;; and high endpoints of the range.  Perhaps we'll go back and change this.
;;
(defun iterate-sqrt-range (low high x num-iters)
  (declare (xargs :measure (nfix num-iters)))
  (if (<= (nfix num-iters) 0)
      (cons (rfix low) (rfix high))
    (let ((mid (/ (+ low high) 2)))
      (if (<= (* mid mid) x)
	  (iterate-sqrt-range mid high x (1- num-iters))
	(iterate-sqrt-range low mid x 
			    (1- num-iters))))))

;;
;; Acl2 doesn't seem to infer the type of the function above, so we give it
;; some help.  Hmmm, maybe this is a candidate for builtin-clause?
;;
(defthm iterate-sqrt-range-type-prescription
  (and (rationalp (car (iterate-sqrt-range low high x num-iters)))
       (rationalp (cdr (iterate-sqrt-range low high x num-iters)))))

;;
;; We will now show some basic properties of iterate-sqrt-range.  One important
;; property (since it'll be used in the induction hypotheses to come) is that
;; if the initial range is proper (i.e., non-empty and not a singleton), then
;; so are all resulting ranges from iterate-sqrt-root.
;;
(defthm iterate-sqrt-range-reduces-range
  (implies (and (rationalp low)
		(rationalp high)
		(< low high))
	   (< (car (iterate-sqrt-range low high x 
				       num-iters))
	      (cdr (iterate-sqrt-range low high x
				       num-iters))))
  :rule-classes (:linear :rewrite))

;;
;; We had foolishly combined the two properties below into one, but then
;; discovered that in some cases we only had one of the needed hypothesis, so
;; we split the halves of the lemma.  The first half says that the high
;; endpoint of the range under consideration can only decrease.
;;
(defthm iterate-sqrt-range-non-increasing-upper-range
  (implies (and (rationalp low)
		(rationalp high)
		(< low high))
	   (<= (cdr (iterate-sqrt-range low high x
					num-iters))
	       high))
  :rule-classes (:linear :rewrite))

;;
;; Similarly, the lower endpoint can only increase.
;;
(defthm iterate-sqrt-range-non-decreasing-lower-range
  (implies (and (rationalp low)
		(rationalp high)
		(< low high))
	   (<= low
	       (car (iterate-sqrt-range low high x num-iters))))
  :rule-classes (:linear :rewrite))

;;
;; Another property we need is that if the high endpoint starts out above the
;; square root of x, it will never be moved below its square root....
;;
(defthm iterate-sqrt-range-upper-sqrt-x
  (implies (and (rationalp low)
		(rationalp high)
		(rationalp x)
		(<= x (* high high)))
	   (<= x
	       (* (cdr (iterate-sqrt-range low high x
					   num-iters))
		  (cdr (iterate-sqrt-range low high x
					   num-iters))
		  )))
  :rule-classes (:linear :rewrite))

;;
;; ....And likewise for the lower endpoint.
;;
(defthm iterate-sqrt-range-lower-sqrt-x
  (implies (and (rationalp low)
		(rationalp high)
		(rationalp x)
		(<= (* low low) x))
	   (<= (* (car (iterate-sqrt-range low high x num-iters))
		  (car (iterate-sqrt-range low high x num-iters)))
	       x))
  :rule-classes (:linear :rewrite))

;;
;; We are now trying to prove that if the final range is small enough, then the
;; squares of the endpoints are very close to x.  To do that, we have to prove
;; some inequalities of real numbers (er, I mean rationals).  We're working too
;; hard here, and that makes us suspicious.  We're doing _something_ wrong, and
;; we're very open to suggestions.
;;
(encapsulate
 ()

 ;;
 ;; First, we show Acl2 how to 'discover' the needed delta.
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux
    (implies (and (rationalp a)
		  (rationalp b)
		  (rationalp epsilon)
		  (<= 0 a)
		  (< a b))
	     (equal (< (- (* b b) (* a a)) epsilon)
		    (< (- b a) (/ epsilon (+ b a)))))))

 ;;
 ;; This is really nothing more than transitivity of less-than.  We want to
 ;; move away from reasoning about the specific delta found above, and move on
 ;; towards an arbitrarily small delta.
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux-2
    (implies (and (rationalp a)
		  (rationalp b)
		  (rationalp epsilon)
		  (<= 0 a)
		  (< a b)
		  (< (- b a) delta)
		  (<= delta (/ epsilon (+ b a))))
	     (< (- b a) (/ epsilon (+ b a))))
    :rule-classes (:linear :rewrite)))

 ;;
 ;; Now, we have the mathematical high-point of this small derivation.  If a
 ;; and b are within our chosen delta, then a^2 and b^2 are within epsilon.
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux-3
    (implies (and (rationalp a)
		  (rationalp b)
		  (rationalp epsilon)
		  (<= 0 a)
		  (< a b)
		  (< (- b a) delta)
		  (<= delta (/ epsilon (+ b a))))
	     (< (- (* b b) (* a a)) epsilon))
    :hints (("Goal"
	     :use ((:instance sqrt-epsilon-delta-aux)
		   (:instance sqrt-epsilon-delta-aux-2))
	     :in-theory (disable sqrt-epsilon-delta-aux
				 sqrt-epsilon-delta-aux-2)))
    :rule-classes (:linear :rewrite)))

 ;;
 ;; In anticipation of theorems to come, we modify our theorem so that a^2 is
 ;; within epsilon of x instead of within epsilon of b^2.
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux-4
    (implies (and (rationalp a)
		  (rationalp b)
		  (rationalp x)
		  (rationalp epsilon)
		  (<= 0 a)
		  (< a b)
		  (<= x (* b b))
		  (< (- b a) delta)
		  (<= delta (/ epsilon (+ b a))))
	     (< (- x (* a a)) epsilon))
    :hints (("Goal"
	     :use ((:instance sqrt-epsilon-delta-aux-3))
	     :in-theory (disable sqrt-epsilon-delta-aux-3)))
    :rule-classes (:linear :rewrite)))

 ;;
 ;; More anticipation.  The hypothesis uses (+ a b).  In our iteration, we're
 ;; guaranteed that b becomes smaller, but a becomes larger.  So, it is very
 ;; advantageous to replace (+ a b) with the larger (+ b b), since we _know_
 ;; this term becomes smaller (and hence is easier to reason about).  So, the
 ;; first step is to teach Acl2 the properties of (+ a b) and (+ b b) under our
 ;; assumptions.  Incidentally, we need that (+ a b) is positive, because the
 ;; inequality rewriting below only works for positive terms (e.g., multiplying
 ;; both sides of an inequality by a term).
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux-5
    (implies (and (rationalp a)
		  (rationalp b)
		  (<= 0 a)
		  (< a b))
	     (and (< (+ a b) (+ b b))
		  (< 0 (+ a b))))))

 ;;
 ;; We discovered that it's easier to reason about A and B than about (+ a b)
 ;; and (+ b b) respectively.  This is an instance of Acl2's problems with
 ;; non-recursive functions.  I.e., it doesn't know that it shouldn't reason
 ;; about the shape of (+ a b).  Can't blame it, really.  It's a program.
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux-6
    (implies (and (rationalp a)
		  (rationalp b)
		  (rationalp c)
		  (< 0 a)
		  (< a b)
		  (< 0 c))
	     (<= (/ c b) (/ c a)))))

 ;;
 ;; Now, we translate the theorem above into the desired terms.  What we have
 ;; is that (/ epsilon (+ b b)) is less than (/ epsilon (+ b a)) when a is less
 ;; than b and everything is non-negative.  Not exactly reason to phone home
 ;; about, but it _does_ lead to the next theorem....
 ;;
 (local
  (defthm sqrt-epsilon-delta-aux-7
    (implies (and (rationalp a)
		  (rationalp b)
		  (rationalp epsilon)
		  (< 0 epsilon)
		  (<= 0 a)
		  (< a b))
	     (<= (/ epsilon (+ b b)) (/ epsilon (+ b a))))
    :hints (("Goal"
	     :use (:instance sqrt-epsilon-delta-aux-6
			     (a (+ b a))
			     (b (+ b b))
			     (c epsilon))
	     :in-theory (disable sqrt-epsilon-delta-aux-6)))))


 ;;
 ;; ....which is that as long as we choose a delta smaller than
 ;; (/ epsilon (+ b b)), a^2 will be close to x.  This is a high point, because
 ;; when we translate this theorem back into ranges, we'll be able to pick the
 ;; delta before starting the iterations as follows.  We know the initial
 ;; range [A, B].  The final range [a, b] will satisfy the theorem below if
 ;; delta < (/ epsilon (+ b b)) but since (+ b b) <= (+ B B), all we need to do
 ;; is make delta smaller than (/ epsilon (+ B B)).
 ;;
 (defthm sqrt-epsilon-delta
   (implies (and (rationalp a)
		 (rationalp b)
		 (rationalp x)
		 (rationalp epsilon)
		 (< 0 epsilon)
		 (<= 0 a)
		 (< a b)
		 (<= x (* b b))
		 (< (- b a) delta)
		 (<= delta (/ epsilon (+ b b))))
	    (< (- x (* a a)) epsilon))
   :hints (("Goal"
	    :use ((:instance sqrt-epsilon-delta-aux-4)
		  (:instance sqrt-epsilon-delta-aux-7))
	    :in-theory (disable <-*-/-left <-*-/-right
				<-*-left-cancel
				sqrt-epsilon-delta-aux-4
				sqrt-epsilon-delta-aux-6
				sqrt-epsilon-delta-aux-7)))
   :rule-classes (:linear :rewrite)))

;;
;; Now, we translate the results above into iter-sqrt-delta.  Again, this seems
;; harder than it needs to be.  It looks like my proof goes directly against
;; Acl2's inequality heuristics.  We _must_ be doing something wrong.
;;
(encapsulate
 ()

 ;;
 ;; We start out with the simple lemma that dividing epsilon by something
 ;; larger results in something smaller.  We do this just so that we can
 ;; instantiate this theorem below.
 ;;
 (local
  (defthm iter-sqrt-epsilon-delta-aux-1
    (implies (and (rationalp epsilon)
		  (rationalp high)
		  (rationalp high2)
		  (< 0 epsilon)
		  (< 0 high2)
		  (<= high2 high))
	     (<= (/ epsilon high) (/ epsilon high2)))))

 ;;
 ;; Now, we extend the theorem above, but we add the extra delta inequality to
 ;; weaken the hypothesis.  We end up having to disable many of Acl2's rewrite
 ;; rules, because they rewrite the intermediate terms before we can bring our
 ;; hypothesis to bear.  This is our biggest indication that we just don't know
 ;; how to reason about inequalities in Acl2.  Need more practice!
 ;;
 (local
  (defthm iter-sqrt-epsilon-delta-aux-2
    (implies (and (rationalp epsilon)
		  (rationalp delta)
		  (rationalp high)
		  (rationalp high2)
		  (< 0 epsilon)
		  (<= delta (/ epsilon high))
		  (< 0 high2)
		  (<= high2 high))
	     (<= delta (/ epsilon high2)))
    :hints (("Goal"
	     :use (:instance iter-sqrt-epsilon-delta-aux-1)
	     :in-theory (disable <-*-/-left
				 <-*-/-right
				 <-*-y-x-y
				 <-*-left-cancel
				 iter-sqrt-epsilon-delta-aux-1)))))

 ;;
 ;; And finally, we have the first half of the convergence proof.  If we start
 ;; with a suitable choice of high and low, then if we iterate long enough that
 ;; the final range is less than delta, then our approximation is very close to
 ;; the square root of x (its squares are within epsilon).
 ;;
 (defthm iter-sqrt-epsilon-delta
   (implies (and (rationalp low)
		 (rationalp high)
		 (rationalp epsilon)
		 (rationalp delta)
		 (rationalp x)
		 (< 0 epsilon)
		 (<= 0 low)
		 (< low high)
		 (<= x (* high high))
		 (<= delta (/ epsilon (+ high high))))
	    (let ((range (iterate-sqrt-range low high x 
					     num-iters)))
	      (implies (< (- (cdr range) (car range))
			  delta)
		       (< (- x 
			     (* (car range) (car range)))
			  epsilon))))
   :hints (("Goal"
	    :do-not-induct t
	    :use (:instance sqrt-epsilon-delta
			    (a (car (iterate-sqrt-range low high x num-iters)))
			    (b (cdr (iterate-sqrt-range low high x num-iters))))
	    :in-theory (disable sqrt-epsilon-delta))
	   ("subgoal 1"
	    :use (:instance iter-sqrt-epsilon-delta-aux-2
			    (high2 (+ (cdr (iterate-sqrt-range low high x
							       num-iters))
				      (cdr (iterate-sqrt-range low high x
							       num-iters))))
			    (high (+ high high)))
	    :in-theory (disable iter-sqrt-epsilon-delta-aux-2)))))

;;
;; For the second half of the proof, we have to reason about how quickly the
;; ranges become small.  Since we halve the range at each step, it makes to
;; start out by introducing the computer scientist's favorite function:
;;
(defun 2-to-the-n (n)
  (if (<= (nfix n) 0)
      1
    (* 2 (2-to-the-n (1- n)))))

;;
;; Unfortunately, we have to use the ceiling function.  Acl2 doesn't seem to
;; want to reason much about it, so we prove its fundamental theorem ourselves.
;;
(encapsulate
 ()

 ;;
 ;; Ceiling is defined in terms of nonnegative-integer-quotient (a recursive
 ;; function), so we prefer to reason about that first.
 ;;
 (local
  (defthm ceiling-greater-than-quotient-aux
    (implies (and (integerp i)
		  (integerp j)
		  (< 0 j))
	     (< (/ i j) (1+ (nonnegative-integer-quotient i j))))
    :rule-classes (:linear :rewrite)))

 ;;
 ;; Now, we can raise the result up to ceiling.
 ;;
 (defthm ceiling-greater-than-quotient
   (implies (and (rationalp x)
		 (rationalp y)
		 (> x 0)
		 (> y 0))
	    (>= (ceiling x y) (/ x y)))
   :hints (("Subgoal 1"
	    :use (:instance ceiling-greater-than-quotient-aux
			    (i (numerator (* x (/ y))))
			    (j (denominator (* x (/ y)))))
	    :in-theory (disable ceiling-greater-than-quotient-aux)))
   :rule-classes (:linear :rewrite)))


;;
;; This function is used to figure out how many iterations of iter-sqrt-range
;; are needed to make the final range sufficiently small.  Since one can't
;; recurse on the rationals, we take the ceiling first, so that we can reason
;; strictly about integers.
;;
(defun guess-num-iters-aux (range num-iters)
  (declare (xargs :measure (nfix (- range (2-to-the-n num-iters)))))
  (if (and (integerp range)
	   (integerp num-iters)
	   (> num-iters 0)
	   (> range (2-to-the-n num-iters)))
      (guess-num-iters-aux range (1+ num-iters))
    (1+ (nfix num-iters))))
(defmacro guess-num-iters (range delta)
  `(guess-num-iters-aux (ceiling ,range ,delta) 1))

;;
;; Now, we're ready to define our square-root approximation function.  All it
;; does is initialize the high/low range, find a suitable delta, convert this
;; to a needed num-iters, and then return the low endpoint of the resulting
;; call to iter-sqrt-range.
;;
(defun iter-sqrt (x epsilon)
  (if (and (rationalp x)
	   (<= 0 x))
      (let ((low 0)
	    (high (if (> x 1) x 1)))
	(let ((range (iterate-sqrt-range
		      low high x
		      (guess-num-iters (- high low)
				       (/ epsilon
					  (+ high 
					     high))))))
	  (car range)))
    nil))

;;
;; To show how quickly the function converges, we teach Acl2 a simple way to
;; characterize the final range from a given initial range.  As expected, the
;; powers of two feature prominently :-)
;;
(defthm iterate-sqrt-reduces-range-size
  (implies (and (<= (* low low) x)
		(<= x (* high high))
		(rationalp low)
		(rationalp high)
		(integerp num-iters))
	   (let ((range (iterate-sqrt-range low high x
					    num-iters)))
	     (equal (- (cdr range) (car range))
		    (/ (- high low)
		       (2-to-the-n num-iters))))))

;;
;; So now, we show that our function to compute needed num-iters does produce a
;; large enough num-iters.  First, we study the recursive function....
;;
(defthm guess-num-iters-aux-is-a-good-guess
  (implies (and (integerp x)
		(integerp num-iters)
		(> x 0)
		(> num-iters 0))
	   (< x (2-to-the-n (guess-num-iters-aux x num-iters))))
  :rule-classes (:linear :rewrite))

;;
;; ...now, we convert that to the non-recursive front-end. This is where we
;; need to reason heavily about ceiling.
;;
(defthm guess-num-iters-is-a-good-guess
  (implies (and (rationalp range)
		(rationalp delta)
		(> range 0)
		(> delta 0))
	   (< (/ range delta) (2-to-the-n (guess-num-iters range delta))))
  :hints (("Goal"
	   :use ((:instance ceiling-greater-than-quotient (x range) (y delta))
		 (:instance guess-num-iters-aux-is-a-good-guess
			    (x (ceiling range delta))
			    (num-iters 1)))
	   :in-theory (disable ceiling
			       ceiling-greater-than-quotient
			       guess-num-iters-aux-is-a-good-guess))
	  ("goal'''"
	   :use (:instance <-*-left-cancel
			   (z delta)
			   (x (ceiling range delta))
			   (y (2-to-the-n (guess-num-iters-aux (ceiling
								range
								delta)
							       1))))
	   :in-theory (disable ceiling <-*-left-cancel))))

;;
;; Now, we're ready to show that iter-sqrt-range will produce a small enough
;; final range.
;;
(encapsulate
 ()

 ;;
 ;; We start out with a simple cancellation rule....
 ;;
 (local
  (defthm aux-1
    (implies (and (rationalp high)
		  (rationalp low)
		  (rationalp max)
		  (rationalp right)
		  (rationalp left)
		  (< 0 max)
		  (equal (- high  low)
			 (- (* max right)
			    (* max left))))
	     (equal (- right left)
		    (/ (-  high low) max)))
    :hints (("Goal" :use (:instance left-cancellation-for-*
				    (z max)
				    (x (/ (-  high low) max))
				    (y (- right left)))
	     :in-theory (disable left-cancellation-for-*)))
    :rule-classes nil))
		
 ;;
 ;; Using that, we show Acl2 how to derive an inequality contradiction that
 ;; it'll see in the next proof.
 ;;
 (local
  (defthm aux-2
    (implies (and (rationalp high)
		  (rationalp low)
		  (rationalp delta)
		  (rationalp max)
		  (rationalp right)
		  (rationalp left)
		  (< 0 delta)
		  (< 0 max)
		  (< (- (/ high delta) (/ low delta))
		     max))
	     (< (- (/ high max) (/ low max))
		delta))
    :hints (("Goal" :use (:instance <-*-left-cancel
				    (z (/ delta max))
				    (x (- (/ high delta) (/ low delta)))
				    (y max))
	     :in-theory (disable <-*-left-cancel)))
    :rule-classes nil))

 ;;
 ;; So now, we can prove a general form of our theorem without appealing to the
 ;; iter-sqrt functions (with all the added complication that excites Acl2's
 ;; rewriting heuristics) 
 ;;
 (local
  (defthm aux
    (implies (and (rationalp high)
		  (rationalp low)
		  (rationalp delta)
		  (rationalp max)
		  (rationalp right)
		  (rationalp left)
		  (< 0 delta)
		  (< 0 max)
		  (equal (- high  low)
			 (- (* max right)
			    (* max left)))
		  (< (- (/ high delta) (/ low delta))
		     max))
	     (< (- right left) delta))
    :hints (("Goal" :use ((:instance aux-1)
			  (:instance aux-2))))))

 ;;
 ;; And finally, we translate the theorem above into iterate-sqrt-range itself.
 ;; This is the second major point, since it shows how we can force
 ;; iterate-sqrt-range to iterate long enough to produce a small enough range.
 ;; Together with the first major result, this will prove the convergence of
 ;; iter-sqrt-range. 
 ;;
 (defthm iterate-sqrt-range-reduces-range-size-to-delta
   (implies (and (rationalp high)
		 (rationalp low)
		 (rationalp delta)
		 (< 0 delta)
		 (< low high)
		 (<= (* low low) x)
		 (<= x (* high high)))
	    (let ((range (iterate-sqrt-range
			  low
			  high
			  x
			  (guess-num-iters (- high low)
					   delta))))
	      (< (- (cdr range) (car range)) delta)))
   :hints (("Goal"
	    :use ((:instance iterate-sqrt-reduces-range-size
			     (num-iters (guess-num-iters (- high low) delta)))
		  (:instance guess-num-iters-is-a-good-guess
			     (range (- high low))))
	    :in-theory (disable iterate-sqrt-reduces-range-size
				guess-num-iters-is-a-good-guess
				ceiling))
	   ("Goal'''"
	    :use (:instance aux
			    (max (2-to-the-n (guess-num-iters (- high low)
							      delta)))
			    (left (car (iterate-sqrt-range
					low high x
					(guess-num-iters (- high low)
							 delta))))
			    (right (cdr (iterate-sqrt-range
					 low high x
					 (guess-num-iters (- high low)
							  delta)))))
	    :in-theory (disable iterate-sqrt-reduces-range-size
				guess-num-iters-is-a-good-guess
				ceiling aux)))))


;;
;; Finally, we prove the convergence theorem for our approximation function.
;;
(encapsulate
 ()

 ;;
 ;; The first convergence result is that the answer we give is no larger than
 ;; the correct one.  Think of this as a (stronger) form a half of the absolute
 ;; value less than epsilon part of the proof.
 ;;
 (local
  (defthm convergence-of-iter-sqrt-1
    (implies (and (rationalp x)	
		  (rationalp epsilon)
		  (< 0 epsilon)
		  (<= 0 x))
	     (<= (* (iter-sqrt x epsilon) (iter-sqrt x epsilon)) x))))

 ;;
 ;; Now, we show that our answer (which is known to be smaller than needed)
 ;; isn't too much smaller.  I.e., it's within epsilon of the square root.
 ;;
 (local
  (defthm convergence-of-iter-sqrt-2
    (implies (and (rationalp x)	
		  (rationalp epsilon)
		  (< 0 epsilon)
		  (<= 0 x))
	     (< (- x (* (iter-sqrt x epsilon) (iter-sqrt x epsilon)))
		epsilon))
    :hints (("Goal"
	     :use ((:instance iterate-sqrt-range-reduces-range-size-to-delta
			      (low 0)
			      (high (if (< x 1) 1 x))
			      (delta (/ epsilon (+ (if (< x 1) 1 x)
						   (if (< x 1) 1 x)))))
		   (:instance iter-sqrt-epsilon-delta
			      (low 0)
			      (high (if (< x 1) 1 x))
			      (delta (/ epsilon (+ (if (< x 1) 1 x)
						   (if (< x 1) 1 x))))
			      (num-iters (guess-num-iters (- (if (< x 1) 1 x)
							     0)
							  (/ epsilon
							     (+ (if (< x 1)
								    1
								  x)
								(if (< x 1)
								    1
								  x)))))))
	     :in-theory (disable ceiling
				 iterate-sqrt-range-reduces-range-size-to-delta
				 iter-sqrt-epsilon-delta)))))

 ;;
 ;; For stylistic reasons, we combine the two results above into a single
 ;; theorem. 
 ;;
 (defthm convergence-of-iter-sqrt
   (implies (and (rationalp x)   
		 (rationalp epsilon)
		 (< 0 epsilon)
		 (<= 0 x))
	    (and (<= (* (iter-sqrt x epsilon)
			(iter-sqrt x epsilon))
		     x)
		 (< (- x (* (iter-sqrt x epsilon)
			    (iter-sqrt x epsilon)))
		    epsilon)))
   :hints (("Goal"
	    :use ((:instance convergence-of-iter-sqrt-1)
		  (:instance convergence-of-iter-sqrt-2))
	    :in-theory (disable iter-sqrt
				convergence-of-iter-sqrt-1
				convergence-of-iter-sqrt-2)))))
