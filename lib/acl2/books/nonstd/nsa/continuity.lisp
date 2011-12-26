(in-package "ACL2")

;; This book establishes some facts about real continuous functions.
;; First, it shows that a function that is continuous on a closed
;; interval is uniformly continuous.  Second, it proves the
;; intermediate value theorem.  Last, it proves the extreme-value
;; theorems; i.e., a continuous function achieves its maximum and
;; minimum over a closed interval.

(include-book "nsa")
(include-book "../arithmetic/realp")

; Added by Matt K. for v2-7.
(add-match-free-override :once t)
(set-match-free-default :once)

;; First, we introduce rcfn - a Real Continuous FunctioN of a single
;; argument.  It is assumed to return standard values for standard
;; arguments, and to satisfy the continuity criterion.  

(encapsulate
 ((rcfn (x) t))

 ;; Our witness continuous function is the identity function.

 (local (defun rcfn (x) x))

 ;; The function returns standard values for standard arguments.

 (defthm rcfn-standard
   (implies (standard-numberp x)
	    (standard-numberp (rcfn x)))
   :rule-classes (:rewrite :type-prescription))

 ;; For real arguments, the function returns real values.

 (defthm rcfn-real
   (implies (realp x)
	    (realp (rcfn x)))
   :rule-classes (:rewrite :type-prescription))

 ;; If x is a standard real and y is a real close to x, then rcfn(x)
 ;; is close to rcfn(y).

 (defthm rcfn-continuous
   (implies (and (standard-numberp x)
		 (realp x)
		 (i-close x y)
		 (realp y))
	    (i-close (rcfn x) (rcfn y))))
 )

;; First, we have a simple lemma.  If x is limited, then
;; standard_part(x) is close to x.

(defthm standard-part-close
  (implies (i-limited x)
	   (i-close (standard-part x) x))
  :hints (("Goal"
	   :use ((:instance i-small-non-standard-part))
	   :in-theory (enable-disable (i-close i-small)
				      (i-small-non-standard-part)))))

;; Now, we show that Rcfn is uniformly continuous.  Note, this only
;; holds for limited x.  I.e., x is in the interval [-M,M] where M is
;; some standard real M.  But then, Rcfn is continuous on [-M,M], and
;; so its uniformly continuous on [-M,M] -- in particular, its
;; uniformly continuous around x.

(defthm rcfn-uniformly-continuous
  (implies (and (i-limited x)
		(realp x)
		(i-close x y)
		(realp y))
	   (i-close (rcfn x) (rcfn y)))
  :hints (("Goal"
	   :use ((:instance rcfn-continuous
			    (x (standard-part x))
			    (y x))
		 (:instance rcfn-continuous
			    (x (standard-part x))
			    (y y))
		 (:instance i-close-transitive
			    (x (standard-part x))
			    (y x)
			    (z y))
		 (:instance i-close-transitive
			    (x (rcfn x))
			    (y (rcfn (standard-part x)))
			    (z (rcfn y)))
		 (:instance i-close-symmetric
			    (x (rcfn (standard-part x)))
			    (y (rcfn x))))
	   :in-theory (disable rcfn-continuous i-close-transitive
			       i-close-symmetric))))

;; This function finds the largest a+i*eps so that f(a+i*eps)<z.

(defun find-zero-n (a z i n eps)
  (declare (xargs :measure (nfix (1+ (- n i)))))
  (if (and (realp a)
	   (integerp i)
	   (integerp n)
	   (< i n)
	   (realp eps)
	   (< 0 eps)
	   (< (rcfn (+ a eps)) z))
      (find-zero-n (+ a eps) z (1+ i) n eps)
    (realfix a)))

;; We prove that f(a+i*eps)<z for the i chosen above.

(defthm rcfn-find-zero-n-<-z
  (implies (and (realp a) (< (rcfn a) z))
	   (< (rcfn (find-zero-n a z i n eps)) z)))

;; Moreover, we show that f(a+i*eps+eps) >= z, so that the i chosen by
;; find-zero-n is the largest possible.

(defthm rcfn-find-zero-n+eps->=-z
  (implies (and (realp a)
		(integerp i)
		(integerp n)
		(< i n)
		(realp eps)
		(< 0 eps)
		(< (rcfn a) z)
		(< z (rcfn (+ a (* (- n i) eps)))))
	   (<= z (rcfn (+ (find-zero-n a z i n eps) 
			  eps)))))


;; The root found by find-zero-n is at least equal to a.
 
(defthm find-zero-n-lower-bound
  (implies (and (realp a) (realp eps) (< 0 eps))
	   (<= a (find-zero-n a z i n eps))))

;; Moreover, the root found by find-zero-n can't be any larger than
;; b-eps.  That means it must be in the range [a,b)

(encapsulate
 ()

 (local
  (defthm lemma-1
    (implies (and (realp a)
		  (realp x))
	     (equal (<= a (+ a x))
		    (<= 0 x)))))

 (defthm find-zero-n-upper-bound
   (implies (and (realp a)
		 (integerp i)
		 (integerp n)
		 (<= 0 i)
		 (<= i n)
		 (realp eps)
		 (< 0 eps))
	    (<= (find-zero-n a z i n eps)
		(+ a (* (- n i) eps))))
   :hints (("Subgoal *1/6.1"
	    :use ((:instance lemma-1
			     (x (* (- n i) eps))))
	    :in-theory (disable lemma-1))))
 )

;; Now, if a and b are limited and a<=x<=b, then x is limited.  This
;; routine should probably go in nsa.lisp.

(local
 (defthm limited-squeeze
   (implies (and (realp a) (realp b) (realp x)
		 (<= a x) (<= x b)
		 (i-limited a) (i-limited b))
	    (i-limited x))
   :hints (("Goal"
	    :use ((:instance large-if->-large
			     (x x)
			     (y a))
		  (:instance large-if->-large
			     (x x)
			     (y b)))
	    :in-theory (enable-disable (abs) (large-if->-large))))))

(encapsulate
 ()

 (local
  (defthm lemma-0
    (implies (and (realp a)
		  (realp x)
		  (<= 0 x))
	     (not (< (+ a x) a)))))
 
 (local
  (defthm lemma-1
    (implies (and (realp a) (i-limited a)
		  (realp b) (i-limited b)
		  (integerp i) (integerp n)
		  (<= 0 i) (<= i n)
		  (<= (+ a (* (+ n (- i)) eps)) b)
		  (realp eps)
		  (< 0 eps))
	     (i-limited (+ a (* (+ n (- i)) eps))))
    :hints (("Goal" :do-not-induct t
	     :use ((:instance limited-squeeze 
			      (x (+ a (* (- n i) eps)))))
	     :in-theory (disable distributivity limited-squeeze))
	    ("Goal'''"
	     :use ((:instance lemma-0
			      (x (* EPS (+ (- I) N))))))
	    )))
	     
 (defthm limited-find-zero-n
   (implies (and (realp a) (i-limited a)
		 (realp b) (i-limited b)
		 (integerp i) (integerp n)
		 (<= 0 i) (<= i n)
		 (<= (+ a (* (+ n (- i)) eps)) b)
		 (realp eps)
		 (< 0 eps))
	    (i-limited (find-zero-n a z i n eps)))
   :hints (("Goal" :do-not-induct t
	    :use ((:instance find-zero-n-lower-bound)
		  (:instance find-zero-n-upper-bound)
		  (:instance lemma-1)
		  (:instance limited-squeeze 
			     (x (find-zero-n a z i n eps))
			     (b (+ a (* (- n i) eps)))))
	    :in-theory (disable lemma-1
				find-zero-n-lower-bound 
				find-zero-n-upper-bound 
				large-if->-large
				limited-squeeze))))
 )

;; Specifically, the invocation of find-zero-n in find-zero is
;; i-limited

(encapsulate
 ()

 ;; First, we need to show what happens to find-zero-n when  the range
 ;; [a,b] is void.

 (local
  (defthm lemma-1
    (implies (and (<= b a) (realp b))
	     (equal (FIND-ZERO-N A Z 0 (I-LARGE-INTEGER)
				 (+ (- (* (/ (I-LARGE-INTEGER)) A))
				    (* (/ (I-LARGE-INTEGER)) B)))
		    (realfix a)))
    :hints (("Goal"
	     :expand ((FIND-ZERO-N A Z 0 (I-LARGE-INTEGER)
				   (+ (- (* (/ (I-LARGE-INTEGER)) A))
				      (* (/ (I-LARGE-INTEGER)) B)))))
	    ("Goal'"
	     :use ((:instance <-*-left-cancel
			      (z (/ (i-large-integer)))
			      (x a)
			      (y b)))
	     :in-theory (disable <-*-left-cancel
				 <-*-/-LEFT-COMMUTED
				 /-cancellation-on-left)))))

 ;; Silly simplification!  N+0=N

 (local
  (defthm lemma-2
    (equal (+ (i-large-integer) 0) (i-large-integer))))
 
 ;; And, N*x/N = x.

 (local
  (defthm lemma-3
    (equal (* (i-large-integer) x (/ (i-large-integer))) (fix x))))

 ;; Now, it's possible to show that find-zero-n is limited!

 (defthm limited-find-zero-body
   (implies (and (i-limited a)
		 (i-limited b)
		 (realp b))
	    (i-limited (find-zero-n a
				    z 
				    0 
				    (i-large-integer)
				    (+ (- (* (/ (i-large-integer)) a))
				       (* (/ (i-large-integer)) b)))))
   :hints (("Goal"
	    :cases ((and (realp a) (< a b))))
	   ("Subgoal 1"
	    :use ((:instance limited-find-zero-n
			     (i 0)
			     (n (i-large-integer))
			     (eps (/ (- b a) (i-large-integer)))))
	    :in-theory (disable limited-find-zero-n))))
 )
	    
;; And now, here's a routine that finds a "zero" in a given [a,b]
;; range.

(defun-std find-zero (a b z)
  (if (and (realp a)
	   (realp b)
	   (realp z)
	   (< a b))
      (standard-part
       (find-zero-n a
		    z 
		    0 
		    (i-large-integer)
		    (/ (- b a) (i-large-integer))))
    0))

;; This doesn't belong here.  It should be moved over to nsa.lisp, and
;; probably written as (equal (equal (stdpt x) (stdpt y)) t) instead.
;; It could be a dangerous lemma if it tries to rewrite all
;; occurrences of standard-part!

(local
 (defthm close-x-y->same-standard-part
   (implies (and (i-close x y)
		 (i-limited x))
	    (equal (standard-part x) (standard-part y)))
   :hints (("Goal" 
	    :use ((:instance i-close-limited))
	    :in-theory (enable-disable (i-close i-small)
				       (i-close-limited))))))

;; But using that lemma, we can prove that (rcfn (std-pt x)) is equal
;; to (std-pt (rcfn x)) -- the reason is that x is close to its
;; std-pt, and since rcfn is continuous, that means (rcfn x) is to
;; close to the (rcfn (std-pt x)).  The last one is known to be
;; standard (by an encapsulate hypothesis), so it must be the
;; standard-part of (rcfn x).

(defthm rcfn-standard-part
  (implies (and (realp x)
		(i-limited x))
	   (equal (rcfn (standard-part x))
		  (standard-part (rcfn x))))
  :hints (("Goal"
	   :use ((:instance rcfn-continuous
			    (x (standard-part x))
			    (y x))
		 (:instance close-x-y->same-standard-part
			    (x (RCFN (STANDARD-PART X)))
			    (y (RCFN X))))
	   :in-theory (enable-disable (standards-are-limited)
				      (rcfn-continuous
				       rcfn-uniformly-continuous
				       close-x-y->same-standard-part)))))

;; Again, find-zero returns a root r so that f(r) <= z.

(defthm-std rcfn-find-zero-<=-z
  (implies (and (realp a)
		(realp b)
		(< a b)
		(realp z)
		(< (rcfn a) z))
	   (<= (rcfn (find-zero a b z)) z))
  :hints (("Goal"
	   :use ((:instance standard-part-<-2
			    (x z)
			    (y (rcfn (find-zero-n a z 0
						  (i-large-integer)
						  (+ (- (* (/ (i-large-integer)) a))
						     (* (/
							 (i-large-integer)) b))))))
		 (:instance rcfn-find-zero-n-<-z
			    (i 0)
			    (n (i-large-integer))
			    (eps (+ (- (* (/ (i-large-integer)) a))
				    (* (/ (i-large-integer)) b)))))
	   :in-theory (disable rcfn-find-zero-n-<-z))))

;; We need to know that if x is limited, so is (rcfn x)

(defthm rcfn-limited
  (implies (and (realp x)
		(i-limited x))
	   (i-limited (rcfn x)))
  :hints (("Goal"
	   :use ((:instance i-close-limited
			    (x (rcfn (standard-part x)))
			    (y (rcfn x)))
		 (:instance rcfn-continuous
			    (x (standard-part x))
			    (y x))
		 )
	   :in-theory (enable-disable (standards-are-limited)
				      (i-close-limited
				       rcfn-continuous
				       rcfn-standard-part
                                       ;; added for v2-6:
                                       rcfn-uniformly-continuous)))))

;; We'll show that f(r+eps) >= z, so that the r found above is the
;; largest possible (within an eps resolution).

(encapsulate
 ()

 ;; First, a quick lemma: N+0 = N.

 (local
  (defthm lemma-1
    (equal (+ (i-large-integer) 0) (i-large-integer))))
 
 ;; Also, N*x/N = x.

 (local
  (defthm lemma-2
    (equal (* (i-large-integer) x (/ (i-large-integer))) (fix x))))

 ;; This silly rule lets us know that x is close to x+eps! 

 (local
  (defthm lemma-3
    (implies (and (realp x)
		  (i-limited x)
		  (realp eps)
		  (i-small eps))
	     (i-close x (+ eps x)))
    :hints (("Goal" :in-theory (enable i-small i-close)))))

 ;; This horrible technical lemma simply gets rid of the +eps part of
 ;; (standard-part (rcfn (+ eps (find-zero-n ....)))) It follows,
 ;; simply, from the fact that eps is small and rcfn is uniformly
 ;; continuous, so (rcfn (+ eps (find-zero-n ...))) is close to (rcfn
 ;; (find-zero-n ...)).

 (local
  (defthm lemma-4
    (implies (and (realp a) (standard-numberp a)
		  (realp b) (standard-numberp b)
		  (< a b)
		  (standard-numberp z)
		  (< (rcfn a) z)
		  (< z (rcfn b)))
	     (equal (standard-part 
		     (rcfn (+ (- (* (/ (i-large-integer)) a))
			      (* (/ (i-large-integer)) b)
			      (find-zero-n a z 0 (i-large-integer)
					   (+ (- (* (/ (i-large-integer)) a))
					      (* (/ (i-large-integer)) b))))))
		    (standard-part
		     (rcfn (find-zero-n a z 0 (i-large-integer)
					(+ (- (* (/ (i-large-integer)) a))
					   (* (/ (i-large-integer))
					      b)))))))
    :hints (("Goal"
	     :use ((:instance close-x-y->same-standard-part
			      (x (rcfn (find-zero-n a z 0 (i-large-integer)
						    (+ (- (* (/ (i-large-integer)) a))
						       (* (/ (i-large-integer))
							  b)))))
			      (y (rcfn (+ (- (* (/ (i-large-integer)) a))
					  (* (/ (i-large-integer)) b)
					  (find-zero-n a z 0 (i-large-integer)
						       (+ (- (* (/ (i-large-integer)) a))
							  (* (/ (i-large-integer))
							     b)))))))
		   (:instance rcfn-uniformly-continuous
			      (x (find-zero-n a z 0 (i-large-integer)
						    (+ (- (* (/ (i-large-integer)) a))
						       (* (/ (i-large-integer))
							  b))))
			      (y (+ (- (* (/ (i-large-integer)) a))
					  (* (/ (i-large-integer)) b)
					  (find-zero-n a z 0 (i-large-integer)
						       (+ (- (* (/ (i-large-integer)) a))
							  (* (/ (i-large-integer))
							     b))))))
		   (:instance lemma-3
			      (x (find-zero-n a z 0 (i-large-integer)
                                (+ (- (* (/ (i-large-integer)) a))
                                   (* (/ (i-large-integer)) b))))
			      (eps (+ (- (* (/ (i-large-integer)) a))
				      (* (/ (i-large-integer)) b)))))
	     :in-theory (disable close-x-y->same-standard-part
				 rcfn-uniformly-continuous
				 lemma-3)))))

 ;; And now, f(r+eps) >= z.

 (defthm-std rcfn-find-zero->=-z
   (implies (and (realp a)
		 (realp b)
		 (< a b)
		 (realp z)
		 (< (rcfn a) z)
		 (< z (rcfn b)))
	    (<= z (rcfn (find-zero a b z))))
   :hints (("Goal"
	    :use ((:instance rcfn-find-zero-n+eps->=-z
			     (n (i-large-integer))
			     (i 0)
			     (eps (/ (- b a) (i-large-integer))))
		  (:instance standard-part-<=
			     (x z)
			     (y (RCFN (+ (- (* (/ (I-LARGE-INTEGER)) A))
					 (* (/ (I-LARGE-INTEGER)) B)
					 (FIND-ZERO-N A Z 0 (I-LARGE-INTEGER)
						      (+ (- (* (/ (I-LARGE-INTEGER)) A))
							 (* (/ (I-LARGE-INTEGER)) B)))))))
		  )
	    :in-theory (disable rcfn-find-zero-n+eps->=-z
				standard-part-<=))))
 )

;; Next, we prove that (find-zero a b z) is in the range (a,b)

(encapsulate
 ()
 
 ;; First, if a and b are standard, (b-a)/N is small, for N a large
 ;; integer.

 (local
  (defthm lemma-1
    (implies (and (standard-numberp a)
		  (standard-numberp b))
	     (i-small (/ (- b a) (i-large-integer))))))
   
 ;; Silly algebra!  a<=a+x if and only if 0<=x....

 (local
  (defthm lemma-2
    (implies (and (realp a)
		  (realp x))
	     (equal (<= a (+ a x))
		    (<= 0 x)))))

 ;; Now, we find an upper bound for the root returned by find-zero-n.

 (local
  (defthm lemma-3
    (implies (and (realp a)
		  (integerp i)
		  (integerp n)
		  (<= 0 i)
		  (<= i n)
		  (realp eps)
		  (< 0 eps))
	     (<= (find-zero-n a z i n eps)
		 (+ a (* (- n i) eps))))
    :hints (("Subgoal *1/6.1"
	     :use ((:instance lemma-2
			      (x (* (- n i) eps))))
	     :in-theory (disable lemma-2)))))

 ;; Silly simplification!  N+0=N

 (local
  (defthm lemma-4
    (equal (+ (i-large-integer) 0) (i-large-integer))))
 
 ;; And, N*x/N = x.

 (local
  (defthm lemma-5
    (equal (* (i-large-integer) x (/ (i-large-integer))) (fix x))))

 ;; A simple consequence is that the root found by find-zero(a,b,z) is
 ;; at most b.

 (local
  (defthm-std find-zero-upper-bound
    (implies (and (realp a) (realp b) (realp z)
		  (< a b))
	     (<= (find-zero a b z) b))
    :hints (("Goal"
	     :use ((:instance lemma-3
			      (i 0)
			      (n (i-large-integer))
			      (eps (/ (- b a) (i-large-integer))))
		   (:instance standard-part-<=
			      (x (find-zero-n a z 0 (i-large-integer)
					      (/ (- b a)
						 (i-large-integer))))
			      (y b)))
	     :in-theory (disable lemma-3
				 standard-part-<=)))))
		 
 ;; Similarly, find-zero-n finds a root at least equal to a.

 (local
  (defthm lemma-7
    (implies (and (realp a) (realp eps) (< 0 eps))
	     (<= a (find-zero-n a z i n eps)))))

 ;; And that means find-zero finds a root at least a.

 (local
  (defthm-std find-zero-lower-bound
    (implies (and (realp a) (realp b) (realp z) (< a b))
	     (<= a (find-zero a b z)))
    :hints (("Goal"
	     :use ((:instance standard-part-<=
			      (x a)
			      (y (find-zero-n a z 0 (i-large-integer)
					      (/ (- b a)
						 (i-large-integer))))))
	     :in-theory (disable standard-part-<=)))))

 ;; And here is the intermediate value theorem.

 (defthm intermediate-value-theorem
   (implies (and (realp a)
		 (realp b)
		 (realp z)
		 (< a b)
		 (< (rcfn a) z)
		 (< z (rcfn b)))
	    (and (realp (find-zero a b z))
		 (< a (find-zero a b z))
		 (< (find-zero a b z) b)
		 (equal (rcfn (find-zero a b z))
			z)))
   :hints (("Goal"
	    :use ((:instance rcfn-find-zero-<=-z)
		  (:instance rcfn-find-zero->=-z)
		  (:instance find-zero-lower-bound)
		  (:instance find-zero-upper-bound))
	    :in-theory (disable find-zero
				find-zero-lower-bound
				find-zero-upper-bound
				rcfn-find-zero-<=-z
				rcfn-find-zero->=-z))))
 )

;; Now, what happens when f(a)>z and f(b)<z.  First, we find the root.

(defun find-zero-n-2 (a z i n eps)
  (declare (xargs :measure (nfix (1+ (- n i)))))
  (if (and (realp a)
	   (integerp i)
	   (integerp n)
	   (< i n)
	   (realp eps)
	   (< 0 eps)
	   (< z (rcfn (+ a eps))))
      (find-zero-n-2 (+ a eps) z (1+ i) n eps)
    (realfix a)))

;; The key theorem -- if -x is close to -y, then x is close to y.

(defthm close-uminus
  (implies (and (acl2-numberp x)
		(acl2-numberp y))
	   (equal (i-close (- x) (- y))
		  (i-close x y)))
  :hints (("Goal"
	   :use ((:instance i-small-uminus (x (+ x (- y)))))
	   :in-theory (enable i-close i-small-uminus))))

;; We prove that this function returns a limited number for limited
;; arguments.

(defthm limited-find-zero-2-body
  (implies (and (i-limited a)
		(i-limited b)
		(realp b)
		(realp z)
		)
	   (i-limited (find-zero-n-2 a
				     z 
				     0 
				     (i-large-integer)
				     (+ (- (* (/ (i-large-integer)) a))
					(* (/ (i-large-integer)) b)))))
   :hints (("Goal"
	    :use ((:instance
		   (:functional-instance 
		    limited-find-zero-body
		    (rcfn (lambda (x) (- (rcfn x))))
		    (find-zero-n (lambda (a z i n
					    eps)
				   (find-zero-n-2
				    a (- z) i n eps))))
		   (z (- z))))
	    :in-theory (disable limited-find-zero-body))))

;; We define the root we want in the range [a,b)

(defun-std find-zero-2 (a b z)
  (if (and (realp a)
	   (realp b)
	   (realp z)
	   (< a b))
      (standard-part
       (find-zero-n-2 a
		      z 
		      0 
		      (i-large-integer)
		      (/ (- b a) (i-large-integer))))
    0))

;; And here is the second version of the intermediate value theorem.

(defthm intermediate-value-theorem-2
  (implies (and (realp a)
		(realp b)
		(realp z)
		(< a b)
		(< z (rcfn a))
		(< (rcfn b) z))
	   (and (realp (find-zero-2 a b z))
		(< a (find-zero-2 a b z))
		(< (find-zero-2 a b z) b)
		(equal (rcfn (find-zero-2 a b z))
		       z)))
  :hints (("Goal"
	   :use ((:instance
		  (:functional-instance 
		   intermediate-value-theorem
		   (rcfn (lambda (x) (- (rcfn x))))
		   (find-zero (lambda (a b z)
				(find-zero-2 a b
					     (- z))))
		   (find-zero-n (lambda (a z i n
					   eps)
				  (find-zero-n-2
				   a (- z) i n eps))))
		  (z (- z))
		  ))
	   :in-theory 
	   (disable intermediate-value-theorem))))

;; The next task is to prove the extreme theorems.  The approach is
;; similar to the intermediate-value theorem.  First, we define a
;; function that splits up the interval [a,b] into a grid of size eps
;; and then we find the maximum of the function at the points in the
;; grid.

(defun find-max-rcfn-x-n (a max-x i n eps)
  (declare (xargs :measure (nfix (1+ (- n i)))))
  (if (and (integerp i)
	   (integerp n)
	   (<= i n)
	   (realp a)
	   (realp eps)
	   (< 0 eps))
      (if (> (rcfn (+ a (* i eps))) (rcfn max-x))
	  (find-max-rcfn-x-n a (+ a (* i eps)) (1+ i) n eps)
	(find-max-rcfn-x-n a max-x (1+ i) n eps))
    max-x))

;; Since the function above takes in a "max-so-far" argument, it is
;; important to note that the initial value of max-so-far is a lower
;; bound for the maximum. 

(defthm find-max-rcfn-x-n-is-monotone
  (<= (rcfn max-x) (rcfn (find-max-rcfn-x-n a max-x i n eps))))

;; Now, we can say that the maximum returned really is the maximum of
;; all the f(x) values at the points x on the grid.

(defthm find-max-rcfn-x-n-is-maximum
  (implies (and (integerp i)
		(integerp k)
		(integerp n)
		(<= 0 i)
		(<= i k)
		(<= k n)
		(realp a)
		(realp eps)
		(< 0 eps))
	   (<= (rcfn (+ a (* k eps)))
	       (rcfn (find-max-rcfn-x-n a max-x i n eps))))
  :hints (("Subgoal *1/7"
	   :use ((:instance find-max-rcfn-x-n-is-monotone))
	   :in-theory (disable find-max-rcfn-x-n-is-monotone))))

;; Naturally, we want to prove that the x value returned for the
;; maximum is in the interval [a,b].  First, we show that it's at most
;; b.  Notice we need to assume the starting value of max-x is less
;; than b!

(defthm find-max-rcfn-x-n-upper-bound
  (implies (and (<= max-x (+ a (* n eps)))
		(realp a)
		(realp eps)
		(integerp i)
		(integerp n)
		(< 0 eps))
	   (<= (find-max-rcfn-x-n a max-x i n eps) (+ a (* n eps))))
  :hints (("Subgoal *1/1"
	   :use ((:theorem
		  (implies (and (< (+ a (* eps n)) (+ a (* i eps)))
				(realp a)
				(realp eps)
				(< 0 eps)
				(integerp i)
				(integerp n))
			   (< n i)))))
	  ("Subgoal *1/1.1"
	   :use ((:theorem
		  (implies (< (+ a (* eps n)) (+ a (* eps i)))
			   (< (* eps n) (* eps i)))))))
  :rule-classes nil)

;; To show that find-max-rcfn-x-n returns a value that is not less
;; than a, we need a simple lemma to do the induction at each of the
;; points in the grid.

(defthm find-max-rcfn-x-n-lower-bound-lemma
  (implies (<= max-x (+ a (* i eps)))
	   (<= max-x (find-max-rcfn-x-n a max-x i n eps))))

;; Now, we can fix the lower range of find-max-x-r-n

(defthm find-max-rcfn-x-n-lower-bound
  (<= a (find-max-rcfn-x-n a a 0 n eps))
  :hints (("Goal"
	   :use ((:instance find-max-rcfn-x-n-lower-bound-lemma
			    (max-x a)
			    (i 0)))
	   :in-theory (disable find-max-rcfn-x-n-lower-bound-lemma))))

;; Next, we would like to use defun-std to introduce find-max-x.
;; Before that, we have to show that find-max-x-n is i-limited.  This
;; is simple, since we know it's in the range [a,b] and b is limited.

(defthm find-max-rcfn-x-n-limited
  (implies (and (realp a) 
		(i-limited a)
		(realp b) 
		(i-limited b)
		(< a b))
	   (i-limited (find-max-rcfn-x-n a a
				    0 (i-large-integer)
				    (+ (- (* (/ (i-large-integer)) a))
				       (* (/ (i-large-integer)) b)))))
  :hints (("Goal"
	   :use ((:instance find-max-rcfn-x-n-upper-bound
			    (max-x a)
			    (n (i-large-integer))
			    (eps (/ (- b a) (i-large-integer)))
			    (i 0))))
	  ("Goal'"
	   :use ((:instance
		  (:theorem
		   (implies (and (realp a)
				 (realp b)
				 (realp x)
				 (i-limited a)
				 (i-limited b)
				 (<= a x)
				 (<= x b))
			    (i-limited x)))
		  (x (find-max-rcfn-x-n a a 0 (i-large-integer)
				   (+ (- (* (/ (i-large-integer)) a))
				      (* (/ (i-large-integer))
					 b)))))))
	  ("Subgoal 1'"
	   :use ((:instance large-if->-large 
			    (x x)
			    (y (if (< (abs a) (abs b))
				   (abs b)
				 (abs a)))))
	   :in-theory (disable large-if->-large))))
	     
;; And now we can introduce the function find-max-rcfn-x which (we
;; claim) finds the point x in [a,b] at which (rcfn x) achieves a
;; maximum.

(defun-std find-max-rcfn-x (a b)
  (if (and (realp a)
	   (realp b)
	   (< a b))
      (standard-part (find-max-rcfn-x-n a
				   a
				   0 
				   (i-large-integer)
				   (/ (- b a) (i-large-integer))))
    0))

;; So first, let's do the easy part of the claim, namely that the x
;; returned by find-max satisfies a <= x.

(defthm-std find-max-rcfn-x->=-a
  (implies (and (realp a)
		(realp b)
		(< a b))
	   (<= a (find-max-rcfn-x a b)))
  :hints (("Goal'"
	   :use ((:instance standard-part-<= 
			    (x a)
			    (y (find-max-rcfn-x-n a
				   a
				   0 
				   (i-large-integer)
				   (/ (- b a) (i-large-integer))))))
	   :in-theory (disable standard-part-<=))))

;; Similarly, that x satisfies x <= b, so x is in [a, b].

(defthm-std find-max-rcfn-x-<=-b
  (implies (and (realp a)
		(realp b)
		(< a b))
	   (<= (find-max-rcfn-x a b) b))
  :hints (("Goal''"
	   :use ((:instance standard-part-<= 
			    (x (find-max-rcfn-x-n a
				   a
				   0 
				   (i-large-integer)
				   (/ (- b a) (i-large-integer))))
			    (y b))
		 (:instance find-max-rcfn-x-n-upper-bound
			    (max-x a)
			    (i 0)
			    (n (i-large-integer))
			    (eps (/ (- b a) (i-large-integer))))
			    
		 )
	   :in-theory (disable standard-part-<=)))
)

;; OK now, (rcfn max) should be the maximum at all the grid points,
;; modulo standard-part.  Why?  Because max is (std-pt max-n).  By
;; construction, max-n is the maximum of all grid-points.  But, (rcfn
;; max) and (rcfn max-n) are close to each other, since rcfn is
;; continuous. Also, (rcfn max) is standard, since max is standard, so
;; (rcfn max) = (std-pt (rcfn max-n)) >= (std-pt (rcfn x_i)) where x_i
;; is any point in the grid.

(defthm find-max-rcfn-is-maximum-of-grid
  (implies (and (realp a) (standard-numberp a)
		(realp b) (standard-numberp b)
		(< a b)
		(integerp k)
		(<= 0 k)
		(<= k (i-large-integer)))
	   (<= (standard-part (rcfn (+ a (* k (/ (- b a)
						 (i-large-integer))))))
	       (rcfn (find-max-rcfn-x a b))))
  :hints (("Goal"
	   :use ((:instance standard-part-<=
			    (x (rcfn (+ a (* k (/ (- b a)
						  (i-large-integer))))))
			    (y (rcfn 
				      (find-max-rcfn-x-n a a 0
						    (i-large-integer)
						    (/ (- b a)
						       (i-large-integer))))))
		 (:instance find-max-rcfn-x-n-is-maximum
			    (i 0)
			    (n (i-large-integer))
			    (eps (/ (- b a) (i-large-integer)))
			    (max-x a)))
	   :in-theory (disable standard-part-<=
			       find-max-rcfn-x-n-is-maximum))))

;; Now, we know the maximum we found really is the maximum at all the
;; grid points.  But what about an arbitrary x in [a,b]?  What we'll
;; do is to find where x falls in the grid.  I.e., we want the i so
;; that x is in [x_{i-1},x_i].  What we'll know is that (rcfn x) is
;; the standard-part of (rcfn x_i), since x and x_i are close to each
;; other and x is standard.  But then, since we know that (rcfn max)
;; is >= (std-pt (rcfn x_i)) = (rcfn x) we have that max really is the
;; maximum for all x.

;; But wait!  That's not quite true.  The equality (std-pt (rcfn x_i)) =
;; (rcfn x) only holds when x is standard!  So what this argument does
;; is prove that (rcfn max) >= (rcfn x) for all standard x.  To finish
;; up the proof, we need to appeal to the transfer principle!

;; First, we define the function that finds the right index i.

(defun upper-bound-of-grid (a x i n eps)
  (declare (xargs :measure (nfix (1+ (- n i)))))
  (if (and (integerp i)
	   (integerp n)
	   (< i n)
	   (<= (+ a (* i eps)) x))
      (upper-bound-of-grid a x (1+ i) n eps)
    i))

;; This seems obvious -- why didn't ACL2 figure it out by itself? --
;; but the index returned is a real number.

(defthm realp-upper-bound-of-grid
  (implies (realp i)
	   (realp (upper-bound-of-grid a x i n eps))))

;; More precisely, it's an _integer_.

(defthm integerp-upper-bound-of-grid
  (implies (integerp i)
	   (integerp (upper-bound-of-grid a x i n eps))))

;; OK now, the index found is at least equal to the starting index....

(defthm upper-bound-of-grid-lower-bound
  (<= i (upper-bound-of-grid a x i n eps)))

;; ...and it's at most the final index.

(defthm upper-bound-of-grid-upper-bound
  (implies (<= i n)
	   (<= (upper-bound-of-grid a x i n eps) n)))

;; So now, we can show that x is in the range [x_{i-1},x_i]

(defthm x-in-upper-bound-of-grid
  (implies (and (integerp i)
		(integerp n)
		(realp eps)
		(< 0 eps)
		(realp x)
		(<= i n)
		(<= (+ a (* i eps)) x)
		(<= x (+ a (* n eps))))
	   (and (<= (- (+ a (* (upper-bound-of-grid a x i n eps)
			       eps))
		       eps)
		    x)
		(<= x (+ a (* (upper-bound-of-grid a x i n eps)
			      eps))))))

;; The above theorem implies that when eps is small, the difference
;; between x and x_i is small (since x_{i-1} <= x <= x_i and
;; x_i-x_{i-1} = eps is small).

(defthm x-in-upper-bound-of-grid-small-eps
  (implies (and (integerp i)
		(integerp n)
		(realp eps)
		(< 0 eps)
		(realp a)
		(realp x)
		(<= i n)
		(<= (+ a (* i eps)) x)
		(<= x (+ a (* n eps)))
		(i-small eps))
	   (i-small (- (+ a (* (upper-bound-of-grid a x i n eps)
			       eps))
		       x)))
  :hints (("Goal"
	   :do-not-induct t
	   :use ((:instance small-if-<-small
			    (x eps)
			    (y (- (+ a (* (upper-bound-of-grid a x i n eps)
					  eps))
				  x)))
		 (:instance x-in-upper-bound-of-grid))
	   :in-theory (disable small-if-<-small
			       x-in-upper-bound-of-grid))))
  
;; So, we have that when eps is small, x and x_i are close to each other.

(defthm x-in-upper-bound-of-grid-small-eps-better
  (implies (and (integerp i)
		(integerp n)
		(realp eps)
		(< 0 eps)
		(realp a)
		(realp x)
		(<= i n)
		(<= (+ a (* i eps)) x)
		(<= x (+ a (* n eps)))
		(i-small eps))
	   (i-close x
		   (+ a (* (upper-bound-of-grid a x i n eps)
			   eps))))
  :hints (("Goal"
	   :use ((:instance i-close-symmetric
			    (x (+ a (* (upper-bound-of-grid a x i n eps)
				       eps)))
			    (y x))
		 (:instance x-in-upper-bound-of-grid-small-eps))
	   :in-theory '(i-close))))

;; Since rcfn is continuous, it follows that (rcfn x) and (rcfn x_i)
;; are close to each other!

(defthm rcfn-x-close-to-rcfn-upper-bound-of-grid
  (implies (and (integerp i)
		(integerp n)
		(realp eps)
		(< 0 eps)
		(realp a)
		(realp x)
		(standard-numberp x)
		(<= i n)
		(<= (+ a (* i eps)) x)
		(<= x (+ a (* n eps)))
		(i-small eps))
	   (i-close (rcfn x)
		    (rcfn (+ a (* (upper-bound-of-grid a x i n eps)
				  eps)))))
  :hints (("Goal"
	   :use ((:instance rcfn-continuous
			    (y (+ a (* (upper-bound-of-grid a x i n eps)
				       eps))))
		 (:instance x-in-upper-bound-of-grid-small-eps-better))
	   :in-theory (disable rcfn-continuous
			       x-in-upper-bound-of-grid-small-eps-better
			       upper-bound-of-grid))))

;; In particular, (std-pt (rcfn x_i)) = (std-pt (rcfn x)) and when x
;; is standard that's equal to (rcfn x).

(defthm rcfn-x-close-to-rcfn-upper-bound-of-grid-better
  (implies (and (integerp i)
		(integerp n)
		(realp eps)
		(< 0 eps)
		(realp a)
		(realp x)
		(standard-numberp x)
		(<= i n)
		(<= (+ a (* i eps)) x)
		(<= x (+ a (* n eps)))
		(i-small eps))
	   (equal (standard-part (rcfn (+ a (* (upper-bound-of-grid a x i n eps)
					       eps))))
		  (rcfn x)))
  :hints (("Goal"
	   :use ((:instance rcfn-x-close-to-rcfn-upper-bound-of-grid)
		 (:instance close-x-y->same-standard-part
			    (x (rcfn x))
			    (y (rcfn (+ a (* (upper-bound-of-grid a x i n eps)
					     eps))))))
	   :in-theory (disable
		       rcfn-x-close-to-rcfn-upper-bound-of-grid
		       close-x-y->same-standard-part
		       upper-bound-of-grid))))

;; So that means that (rcfn max) >= (rcfn x), because we already know
;; that (rcfn max) >= (std-pt (rcfn x_i)) for all indices i!  That
;; only works for standard values of x.

(local
 (defthm small-range
   (implies (and (realp a) (standard-numberp a)
		 (realp b) (standard-numberp b)
		 (< a b))
	    (i-small (+ (- (* (/ (i-large-integer)) a))
			(* (/ (i-large-integer)) b))))))

(defthm find-max-rcfn-is-maximum-of-standard
  (implies (and (realp a) (standard-numberp a)
		(realp b) (standard-numberp b)
		(realp x) (standard-numberp x)
		(<= a x)
		(<= x b)
		(< a b))
	   (<= (rcfn x) (rcfn (find-max-rcfn-x a b))))
  :hints (("Goal"
	   :use ((:instance find-max-rcfn-is-maximum-of-grid
			    (k (upper-bound-of-grid a x 0
						    (i-large-integer)
						    (/ (- b a)
						       (i-large-integer)))))
		 (:instance
		  rcfn-x-close-to-rcfn-upper-bound-of-grid-better
		  (n (i-large-integer))
		  (eps (/ (- b a) (i-large-integer)))
		  (i 0)))
	   :in-theory
	   (disable
	    rcfn-x-close-to-rcfn-upper-bound-of-grid-better
	    find-max-rcfn-is-maximum-of-grid
	    small-<-non-small
	    limited-integers-are-standard))))

;; So now, we "transfer" that result to *all* values of x in [a,b].
;; What we have is that for all x in [a,b], (rcfn max) >= (rcfn x) and
;; that max is in [a,b].  This is the "maximum theorem".

(defthm-std find-max-rcfn-is-maximum
  (implies (and (realp a)
		(realp b)
		(realp x)
		(<= a x)
		(<= x b)
		(< a b))
	   (<= (rcfn x) (rcfn (find-max-rcfn-x a b))))
  :hints (("Goal"
	   :in-theory (disable find-max-rcfn-x))))

;; Of course, the function also achieves its minimum.  To do that, we
;; start with the follogin function, which is similar to the "max-x-n"
;; function above.  Shouldn't ACL2 be able to do this sort of thing by
;; itself?

(defun find-min-rcfn-x-n (a min-x i n eps)
  (declare (xargs :measure (nfix (1+ (- n i)))))
  (if (and (integerp i)
	   (integerp n)
	   (<= i n)
	   (realp a)
	   (realp eps)
	   (< 0 eps))
      (if (< (rcfn (+ a (* i eps))) (rcfn min-x))
	  (find-min-rcfn-x-n a (+ a (* i eps)) (1+ i) n eps)
	(find-min-rcfn-x-n a min-x (1+ i) n eps))
    min-x))

;; We have to prove that this function is limited.  Luckily, we can
;; just reuse the theorem about max-n being limited.

(defthm find-min-rcfn-x-n-limited
  (implies (and (realp a) 
		(i-limited a)
		(realp b) 
		(i-limited b)
		(< a b))
	   (i-limited (find-min-rcfn-x-n a a
				    0 (i-large-integer)
				    (+ (- (* (/ (i-large-integer)) a))
				       (* (/ (i-large-integer)) b)))))
  :hints (("Goal"
	   :use ((:functional-instance find-max-rcfn-x-n-limited
				       (rcfn (lambda (x) (- (rcfn
							     x))))
				       (find-max-rcfn-x-n find-min-rcfn-x-n)
				       ))
	   :in-theory (disable find-max-rcfn-x-n-limited))))
		   
;; That justifies the definition of min-x.

(defun-std find-min-rcfn-x (a b)
  (if (and (realp a)
	   (realp b)
	   (< a b))
      (standard-part (find-min-rcfn-x-n a
				   a
				   0 
				   (i-large-integer)
				   (/ (- b a) (i-large-integer))))
    0))

;; Now, to see that this function really returns a minimum, we just
;; have to instantiate the appropriate theorem about maximums.

(defthm find-min-rcfn-is-minimum
  (implies (and (realp a)
		(realp b)
		(realp x)
		(<= a x)
		(<= x b)
		(< a b))
	   (<= (rcfn (find-min-rcfn-x a b)) (rcfn x)))
  :hints (("Goal"
	   :use ((:functional-instance find-max-rcfn-is-maximum
				       (rcfn (lambda (x) (- (rcfn
							     x))))
				       (find-max-rcfn-x-n find-min-rcfn-x-n)
				       (find-max-rcfn-x find-min-rcfn-x)))
	   :in-theory (disable find-max-rcfn-is-maximum))))

;; Similarly, we want to show that a <= min-x -- just instantiate the
;; theorem about maximum!

(defthm find-min-rcfn-x->=-a
  (implies (and (realp a)
		(realp b)
		(< a b))
	   (<= a (find-min-rcfn-x a b)))
  :hints (("Goal"
	   :use ((:functional-instance find-max-rcfn-x->=-a
				       (rcfn (lambda (x) (- (rcfn
							     x))))
				       (find-max-rcfn-x-n find-min-rcfn-x-n)
				       (find-max-rcfn-x find-min-rcfn-x)))
	   :in-theory (disable find-max-rcfn-x->=-a))))

;; And finally,, we want to show that min-x <= b -- again, just
;; instantiate the theorem about maximum!

(defthm find-min-rcfn-x-<=-b
  (implies (and (realp a)
		(realp b)
		(< a b))
	   (<= (find-min-rcfn-x a b) b))
  :hints (("Goal"
	   :use ((:functional-instance find-max-rcfn-x-<=-b
				       (rcfn (lambda (x) (- (rcfn
							     x))))
				       (find-max-rcfn-x-n find-min-rcfn-x-n)
				       (find-max-rcfn-x find-min-rcfn-x)))
	   :in-theory (disable find-max-rcfn-x-<=-b))))
