; Contributed by Alex Spiridonov, with helpful consulting from Robert Krug.

(in-package "ACL2")

; Theorems from arithmetic/top-with-meta
(encapsulate
  ()

  (local (include-book "arithmetic/top-with-meta" :dir :system))

  ; Theorems about inequalities
  (defthm /-inverts-order-1
    (implies (and (< 0 x)
                  (< x y)
                  (rationalp x)
                  (rationalp y))
             (< (/ y) (/ x))))
  
  (defthm /-inverts-order-2
    (implies (and (< y 0)
                  (< x y)
                  (rationalp x)
                  (rationalp y))
             (< (/ y) (/ x))))

  (defthm /-inverts-weak-order
    (implies (and (< 0 x)
                  (<= x y)
                  (rationalp x)
                  (rationalp y))
             (not (< (/ x) (/ y)))))

  ; Theorems about equalities
  (defthm equal-*-x-y-x
    (equal (equal (* x y) x)
           (or (equal x 0)
               (and (equal y 1)
                    (acl2-numberp x)))))

  (defthm equal-*-x-y-y
    (equal (equal (* x y) y)
           (or (equal y 0)
               (and (equal x 1)
                    (acl2-numberp y)))))

  (defthm equal-/
   (implies (and (acl2-numberp x)
                 (not (equal 0 x)))
            (equal (equal (/ x) y)
                   (equal 1 (* x y)))))

; From rtl/rel5/arithmetic
; Originally written as (mod (binary-* k x) (binary-* y k)), but we write is
; this way because arithmetic-3 will rewrite (binary-* y k) to (binary-* k y).
  (defthm mod-cancel-special-1-ext
    (implies (if (acl2-numberp x)
                 (if (rationalp k)
                     (if (acl2-numberp y)
                         (if (not (equal y '0))
                             (not (equal k '0))
                             'nil)
                         'nil)
                     'nil)
                 'nil)
             (equal (mod (binary-* k x) (binary-* k y))
                    (binary-* k (mod x y)))))

)

; Theorems from ihs
(encapsulate
  ()

  (local (include-book "ihs/ihs-definitions" :dir :system))
  (local (include-book "ihs/ihs-lemmas" :dir :system))
  (local (minimal-ihs-theory))

  (defthm integerp-i/j-integerp-forward
    (implies
     (and (integerp (/ i j))
	  (real/rationalp i)
	  (integerp j)
	  (not (zerop j)))
     (integerp i))
    :rule-classes
    ((:forward-chaining
      :corollary
      (implies
       (and (integerp (/ i j))
	    (force (real/rationalp i))
	    (integerp j)
	    (force (not (equal 0 j))))
       (integerp i)))
     (:forward-chaining
      :corollary
      (implies
       (and (integerp (* (/ j) i))
	    (force (real/rationalp i))
	    (integerp j)
	    (force (not (equal 0 j))))
       (integerp i)))))

  (defthm justify-floor-recursion-ext
    (implies
     (and (force (real/rationalp x))
          (force (real/rationalp y))
          (force (not (equal 0 y))))
     (and
      (implies
       (and (< 0 x)
            (< 1 y))
       (< (floor x y) x))
      (implies
       (and (< x -1)
            (<= 2 y))
       (< x (floor x y))))))

; From arithmetic-2
; Alternative: mod-x-y-=-x+y from IHS
  (defthm mod-x-y-=-x-+-y-ext
    (implies (and (rationalp x)
                  (rationalp y)
                (not (equal y 0))
		(if (< 0 y)
		    (and (< x 0)
			 (<= (- x) y))
		  (and (< 0 x)
		       (<= y (- x)))))
             (equal (mod x y) (+ x y)))
    :rule-classes ((:rewrite :backchain-limit-lst 0)
                   (:rewrite
                    :corollary
                    (implies (and (rationalp x)
                                  (rationalp y)
                                  (not (equal y 0)))
                             (equal (equal (mod x y) (+ x y))
                                    (if (< 0 y)
                                        (and (< x 0)
                                             (<= (- x) y))
                                      (and (< 0 x)
                                           (<= y (- x)))))))))

  (defthm mod-x-i*j
    (implies
     (and (> i 0)
	  (> j 0)
	  (force (integerp i))
	  (force (integerp j))
	  (force (real/rationalp x)))
     (equal (mod x (* i j))
	    (+ (mod x i) (* i (mod (floor x i) j))))))

  (defthm floor-x+i*k-i*j
    (implies
     (and (force (real/rationalp x))
          (force (integerp i))
          (force (integerp j))
          (force (integerp k))
          (< 0 i)
          (< 0 j)
          (<= 0 x)
          (< x i))
     (equal (floor (+ x (* i k)) (* i j))
            (floor k j))))

  (defthm mod-x+i*k-i*j
    (implies
     (and (force (real/rationalp x))
          (force (integerp i))
          (force (integerp j))
          (force (integerp k))
          (< 0 i)
          (< 0 j)
          (<= 0 x)
          (< x i))
     (equal (mod (+ x (* i k)) (* i j))
            (+ x (* i (mod k j))))))

)

; An encapsulate for rtl/rel6/arithmetic.
(encapsulate
  ()

  (local (include-book "rtl/rel6/arithmetic/top" :dir :system))

  (defthm x*/y=1->x=y-ext
    (implies (and (rationalp x)
                  (rationalp y)
                  (not (equal x 0))
                  (not (equal y 0)))
             (equal (equal (* x (/ y)) 1)
                    (equal x y))))

  ; From arithmetc-2
  (defthm ratio-theory-of-1-f
    (implies (and (real/rationalp x)
                  (real/rationalp y)
                  (<= 0 x)
                  (< x (- y)))
             (and (<= (/ x y) 0)
                  (< -1 (/ x y))))
    :rule-classes :linear)

  ; From arithmetc-2
  (defthm x*y>1-positive-stronger
   (implies (and (or (and (< 1 x)
                          (<= 1 y))
                     (and (<= 1 x)
                          (< 1 y)))
                 (real/rationalp x)
                 (real/rationalp y))
    (< 1 (* x y)))
   :rule-classes (:linear :rewrite))

   ; From arithmetc-2
   (defthm nintegerp-/
     (implies (and (real/rationalp x)
                   (< 1 x))
              (not (integerp (/ x))))
   :rule-classes :type-prescription)

  (defthm mod-even
    (implies (rationalp x)
             (equal (integerp (* 1/2 (mod x 2)))
                    (integerp (* 1/2 x)))))
  
  (defthm mod-1-integerp
    (implies (case-split (acl2-numberp x))
             (equal (integerp (mod x 1))
                    (integerp x))))

  (defthm mod-prod
    (implies (and (rationalp m)
                  (rationalp n)
                  (rationalp k))
             (equal (mod (* k m) (* k n))
                    (* k (mod m n)))))

  (defthm mod-mult-n
    (equal (mod (* a n) n)
           (* n (mod a 1))))

  (defthm mod-when-y-is-complex-rationalp
    (implies (complex-rationalp y)
             (equal (mod x y)
                  (if (not (complex-rationalp x))
                      (fix x)
                    (if (not (rationalp (/ x y)))
                        x
                      (if (integerp (/ x y))
                          0 
                        (+ x (* -1 y (floor (* x (/ y)) 1)))
                        ))))))

  (defthm mod-when-y-is-an-inverse
    (implies (and (integerp (/ y))
                  (integerp x)
                  (case-split (< 0 y))
                  )
             (equal (mod x y)
                    0)))

  (defthm rationalp-mod-ext
    (implies (case-split (rationalp x))
             (rationalp (mod x y)))
    :rule-classes (:rewrite :type-prescription))

  (defthm mod-integerp-when-y-is-power-of-2
    (implies (integerp x)
             (integerp (mod x (expt 2 i))))
    :rule-classes (:rewrite :type-prescription))

  (defthm mod-of-mod
    (implies (and (case-split (natp k))
                  (case-split (natp n)))
             (equal (mod (mod x (* k n)) n)
                    (mod x n))))

  (defthm mod-sum
    (implies (and (rationalp a)
                  (rationalp b)
                  )
             (equal (mod (+ a (mod b n)) n)
                    (mod (+ a b) n))))

  (defthm mod-mod-sum
    (implies (and (rationalp a)
                  (rationalp b)
                  )
             (equal (mod (+ (mod a n) (mod b n)) n)
                    (mod (+ a b) n))))

  (defthm mod-diff
    (implies (and (case-split (rationalp a))
                  (case-split (rationalp b))
                  )
	     (equal (mod (- a (mod b n)) n)
		    (mod (- a b) n))))

  (defthm mod-does-nothing
    (implies (and (< m n)
                  (<= 0 m)
                  (case-split (rationalp m)))
             (equal (mod m n)
                    m)))

   (defthm mod-bnd-2
     (implies (and (<= 0 m)
                   (case-split (rationalp m))
                   )
              (<= (mod m n) m))  
     :rule-classes :linear)

  (defthm mod-sums-cancel-1
    (implies (and (case-split (<= 0 y))
                  (case-split (rationalp k))
                  (case-split (rationalp y))
                  (case-split (rationalp x1))
                  (case-split (rationalp x2)))
             (equal (equal (mod (+ k x1) y) (mod (+ k x2) y))
                    (equal (mod x1 y) (mod x2 y)))))

; We write (mod (+ k x) y) rather than (mod (+ x k) y); otherwise it ;
; gets re-written by |(+ x y)|
  (defthm mod-sums-cancel-5-ext
    (implies (and (case-split (<= 0 y))
                  (case-split (rationalp k))
                  (case-split (rationalp y))
                  (case-split (rationalp x)))
             (equal (equal (mod k y) (mod (+ k x) y))
                    (equal 0 (mod x y)))))

  (defthm mod-mod-2-thm
    (implies (and (<= y1 y2)
                  (case-split (< 0 y1))
                  (case-split (acl2-numberp x))
                  (case-split (rationalp y1))
                  (case-split (rationalp y2))
                  (case-split (not (equal y1 0))))
             (equal (mod (mod x y1) y2)
                    (mod x y1))))

  (defthm mod-mod-2-not-equal-ext
    (implies (acl2-numberp m)
             (not (equal (mod m 2) (mod (1+ m) 2))))
   :hints (("Goal" :use ((:instance mod-mod-2-not-equal)))))

  (defthm mod-quotient-integerp
    (implies (and (integerp (* y k))
                  (rationalp x)
                  (rationalp y)
                  (rationalp k))
             (equal (integerp (* k (mod x y)))
                    (integerp (* k x)))))

  (defthm mod-1-sum-integer
    (implies (and (rationalp x)
                  (rationalp y))
             (equal (integerp (+ x (mod y 1)))
                    (integerp (+ x y)))))

  (defthm mod-mod-e
    (implies (and (integerp (/ y1 y2))
                  (case-split (not (equal y2 0)))
                  (case-split (rationalp y1))
                  (case-split (rationalp y2)))
             (equal (mod (mod x y1) y2)
                    (mod x y2))))

  (defthm mod-sum-elim-second
    (implies (and (case-split (not (complex-rationalp x1)))
                  (case-split (not (complex-rationalp x2))) 
                  )
             (equal (mod (+ x1 (mod x2 y)) y)
                    (mod (+ x1 x2) y))))

  (defthm mod-sum-elim-second-gen
    (implies (and (integerp (/ y2 y))
                  (case-split (not (complex-rationalp x1)))
                  (case-split (not (complex-rationalp x2))) 
                  (case-split (not (equal y 0)))
                  (case-split (rationalp y))
                  )
             (equal (mod (+ x1 (mod x2 y2)) y)
                    (mod (+ x1 x2) y))))

  (defthm mod-sum-elim-both
    (implies (and (case-split (not (complex-rationalp a)))
                  (case-split (not (complex-rationalp b)))
                  )
             (equal (mod (+ (mod a y) (mod b y)) y)
                    (mod (+ a b) y))))

  (defthm mod-drop-irrelevant-first-term
    (implies (and (integerp (* k (/ y)))
                  (case-split (not (equal y 0)))
                  (case-split (rationalp y))
                  (case-split (not (complex-rationalp x)))
                  )
             (equal (mod (+ k x) y)
                    (mod x y))))

  (defthm mod-mult-ext
    (implies (and (integerp a)
                  (case-split (not (complex-rationalp x)))
                  (case-split (not (complex-rationalp y)))
                  )
             (equal (mod (+ x (* a y)) y)
                    (mod x y))))

  (defthm mod-complex-rationalp-rewrite
    (implies (case-split (rationalp y))
             (equal (complex-rationalp (mod x y))
                    (complex-rationalp x))))
  
  (defthm mod-upper-bound-less-tight-rewrite
    (implies (and (case-split (< 0 y))
                  (case-split (not (complex-rationalp x)))
                  (case-split (not (complex-rationalp y)))
                  )
             (<= (mod x y) y)))

  (defthm mod-upper-bound-3
    (implies (and (<= y z)
                  (case-split (< 0 y))
                  (case-split (not (complex-rationalp x)))
                  (case-split (not (complex-rationalp y)))
                  )
             (< (mod x y) z)))

  (defthm mod-non-negative-linear
    (implies (and (case-split (< 0 y))
                  (case-split (not (complex-rationalp x)))
                  (case-split (not (complex-rationalp y)))
                  )
             (<= 0 (mod x y)))
    :rule-classes ((:linear :trigger-terms ((mod x y)))))

  (defthm mod-integerp-2
    (implies (and (integerp y)
                  (case-split (acl2-numberp x)))
             (equal (integerp (mod x y))
                    (integerp x))))

  (defthm mod-rational-when-y-is-rational-rewrite
    (implies (and (rationalp y)
                  (case-split (acl2-numberp x)))
             (equal (rationalp (mod x y))
                    (rationalp x))))

  (defthm mod-force-equal-ext ; rule-classes nil
    (implies (and (< (abs (- a b)) n)
                  (rationalp a)
                  (rationalp b)
                  (integerp n))
             (iff (equal (mod a n) (mod b n))
                  (equal a b)))
    :hints (("Goal" :use ((:instance mod-force-equal)))))

  (defthm mod-equal-int-ext ; rule-classes nil
    (implies (and (equal (mod a n) (mod b n))
                  (rationalp a)
                  (rationalp b))
             ;(integerp (/ (- a b) n)))
             ; arithmetic-3 rewrites the above line to this one
             (integerp (+ (* a (/ n)) (- (* b (/ n))))))
    :hints (("Goal" :use ((:instance mod-equal-int)))))

  (defthm mod-equal-int-reverse-ext ; rule-classes nil
    (implies (and ; (integerp (/ (- a b) n))
                  ; arithmetic-3 rewrites the above line to this one
                  (integerp (+ (* a (/ n)) (- (* b (/ n)))))
                  (rationalp a)
                  (rationalp b)
                  (rationalp n)
                  (< 0 n))
             (equal (mod a n) (mod b n)))
    :hints (("Goal" :use ((:instance mod-equal-int-reverse)))))

  (defthm even-odd-5
    (implies (and (rationalp x)
                  (integerp (* 1/2 x)))
             (and (integerp (- x 1))
                  (not (integerp (* 1/2 (- x 1)))))))

  (defthm expt-2-is-not-odd
    (implies (and (evenp x)
                  (< 0 i)
                  (integerp i))
             (equal (equal (expt 2 i)
                           (+ 1 x))
                    nil)))

  (defthm expt2-integer
    (implies (case-split (integerp i))
             (equal (integerp (expt 2 i))
                    (<= 0 i))))

  (defthm expt-bigger-than-i
    (implies (integerp i)
             (< i (expt 2 i))))

  (defthm expt2-inverse-integer
    (implies (case-split (integerp i))
             (equal (integerp (/ (expt 2 i)))
                    (<= i 0))))

  (defthm expt2-1-to-1
    (implies (and (integerp i1)
                  (integerp i2))
             (equal (equal (expt 2 i1) (expt 2 i2))
                    (equal i1 i2))))

  (defthm expt-between-one-and-two
    (implies (and (<= 1 (expt 2 i))
                  (< (expt 2 i) 2))
             (equal (expt 2 i) 1)))

  (defthm expt-prod-integer-3-terms-2-ext
    (implies (and (<= (+ j l) i)
                  (integerp i)
                  (integerp j)
                  (integerp l)
                  )
             (integerp (* (expt 2 i) (/ (expt 2 j)) (/ (expt 2 l))))))

  (defthm complex-rationalp-prod
    (implies (and (rationalp k)
                  (case-split (not (equal k 0)))
                  )
             (and (equal (complex-rationalp (* k x))
                         (complex-rationalp x))
                  (equal (complex-rationalp (* x k))
                         (complex-rationalp x)))))

  (defthm product-greater-than-zero-ext
    (implies (or (case-split (not (complex-rationalp x)))
                 (case-split (not (complex-rationalp y))))
             (equal (< 0 (* x y))
                    (or (and (< 0 x) (< 0 y))
                        (and (< y 0) (< x 0))))))

  (defthm product-less-than-zero
    (implies (case-split (or (not (complex-rationalp x))
                             (not (complex-rationalp y))))
             (equal (< (* x y) 0)
                    (if (< x 0)
                        (< 0 y)
                      (if (equal 0 x)
                          nil
                        (if (not (acl2-numberp x))
                            nil
                          (< y 0)))))))

   (defthm quotient-not-integerp
     (implies (and (< i j)
                   (<= 0 i)
                   (<= 0 j)
                   (case-split (< 0 i))
                   (case-split (< 0 j))
                   (case-split (rationalp j)))
              (not (integerp (/ i j)))))

   (defthm rationalp-product-when-one-arg-is-rational
     (implies (and (rationalp x)
                   (case-split (not (equal x 0)))
                   (case-split (acl2-numberp y))
                   )
              (and (equal (rationalp (* x y))
                          (rationalp y))
                   (equal (rationalp (* y x))
                          (rationalp y)))))

   (defthm rationalp-sum-when-one-arg-is-rational
     (implies (and (rationalp x)
                   (case-split (acl2-numberp y)))
              (and (equal (rationalp (+ x y))
                          (rationalp y))
                   (equal (rationalp (+ y x))
                          (rationalp y)))))
   
   (defthm rationalp-unary-divide
     (implies (case-split (acl2-numberp x))
              (equal (rationalp (/ x))
                     (rationalp x))))

   (defthm complex-rationalp-+-when-second-term-is-not-complex
     (implies (not (complex-rationalp y))
              (equal (complex-rationalp (+ x y))
                     (complex-rationalp x))))

   (defthm complex-rationalp-+-when-first-term-is-not-complex
     (implies (not (complex-rationalp x))
              (equal (complex-rationalp (+ x y))
                     (complex-rationalp y))))

   (defthm fraction-less-than-1
     (implies (and (< (abs m) (abs n))
                   (rationalp m)
                   (rationalp n))
              (<= (* m (/ n)) 1)))

   ; Floor theorems
   (defthm floor-with-i-not-rational
     (implies (not (rationalp i))
              (equal (floor i j)
                     (if (and (complex-rationalp i) (complex-rationalp j) (rationalp (/ i j)))
                         (floor (/ i j) 1)
                       0))))

   (defthm floor-with-j-not-rational
     (implies (not (rationalp j))
              (equal (floor i j)
                     (if (and (complex-rationalp i) (complex-rationalp j) (rationalp (/ i j)))
                         (floor (/ i j) 1)
                       0))))
   
   (defthm floor-of-rational-and-complex
     (implies (and (rationalp i)
                   (not (rationalp j))
                   (case-split (acl2-numberp j)))
              (and (equal (floor i j)
                          0)
                   (equal (floor j i)
                          0))))
   
   (defthm floor-when-arg-quotient-isnt-rational
     (implies (not (rationalp (* i (/ j))))
              (equal (floor i j) 0)))

   (defthm floor-non-negative-integerp-type-prescription
     (implies (and (<= 0 i)
                   (<= 0 j)
                   (case-split (not (complex-rationalp j)))
                   )
              (and (<= 0 (floor i j))
                   (integerp (floor i j))))
     :rule-classes (:type-prescription))

   (defthm floor-non-negative
     (implies (and (<= 0 i)
                   (<= 0 j)
                   (case-split (not (complex-rationalp i)))
                   )
              (<= 0 (floor i j))))

   (defthm floor-equal-i-over-j-rewrite
     (implies (and (case-split (not (equal j 0)))
                   (case-split (rationalp i))
                   (case-split (rationalp j))
                   )
              (equal (equal (* j (floor i j)) i)
                     (integerp (* i (/ j))))))

   (defthm integerp-sum-of-odds-over-2
     (implies (and (rationalp x)
                   (rationalp y)
                   (integerp (* 2 x))
                   (not (integerp x))
                 )
              (equal (integerp (+ x y))
                     (and (integerp (* 2 y))
                          (not (integerp y))
                          ))))
   
  (defthm mod-bnd-3
    (implies (and (< m (+ (* a n) r))
		  (<= (* a n) m)
                  (integerp a)
                  (case-split (rationalp m))
		  (case-split (rationalp n))
		  )
	     (< (mod m n) r))
  :rule-classes :linear)

  ; rule-classes nil
  (defthm mod-force-ext
    (implies (and (<= (* a n) m)
                  (< m (* (1+ a) n))
                  (integerp a)
                  (rationalp m)
                  (rationalp n)
                  )
             (= (mod m n) (- m (* a n))))
    :hints (("Goal" :use ((:instance mod-force)))))

  (defthm mod-equal-0-ext
    (implies (and (case-split (rationalp y))
                  (case-split (not (equal y 0))))
             (equal (equal (mod x y) 0)
                    (integerp (* (/ y) x))))
    :hints (("Goal" :in-theory (enable mod-equal-0))))

  (defthm mod-integerp-2-2
    (implies (and (integerp y)
                  (integerp x))
             (integerp (mod x (/ y)))))

  (local (include-book "rtl/rel6/arithmetic/extra-rules" :dir :system)) ; for exp-invert
  ; rule-classes nil
  (defthm exp-invert-ext
    (implies (and (integerp n)
		  (<= n -1))
	     (<= (/ (- 1 (expt 2 n)))
		 (1+ (expt 2 (1+ n)))))
    :hints (("Goal" :use ((:instance exp-invert)))))

  (defthm mod-integerp-when-y-is-an-inverse
    (implies (and (integerp (/ y))
                  (integerp x))
             (integerp (mod x y))))

#| ; expensive: more than doubles execution time!
  (defthm x-2xx-ext
    (implies (and (rationalp x)
                  (integerp (* 2 x x)))
             (integerp x))
    :hints (("Goal" :use ((:instance x-2xx)))))
|#

)

(encapsulate
  ()

  (local (include-book "arithmetic-2/meta/expt" :dir :system))
  (local (include-book "arithmetic-2/meta/integerp" :dir :system))
  
  (defthm expt-1-linear-b
    (implies (and (<= 0 x)
                  (< x 1)
                  (< 0 i)
                  (real/rationalp x)
                  (integerp i))
             (< (expt x i) 1))
    :rule-classes :linear)

  (defthm expt-1-linear-d
    (implies (and (<= 0 x)
                  (<= x 1)
                  (<= 0 i)
                  (real/rationalp x)
                  (integerp i))
             (<= (expt x i) 1))
    :rule-classes :linear)

  (defthm expt-1-linear-h
    (implies (and (< 0 x)
                  (<= x 1)
                  (< i 0)
                  (real/rationalp x)
                  (integerp i))
             (<= 1 (expt x i)))
    :rule-classes :linear)

  (defthm nintegerp-expt
    (implies (and (real/rationalp x)
                  (< 1 x)
                  (integerp n)
                  (< n 0))
             (not (integerp (expt x n))))
    :hints (("Goal" :use nintegerp-expt-helper))
    :rule-classes :type-prescription)

)

#|
(encapsulate
  ()

  ; Expensive: adds ~9 seconds to test suite. Gain: 1.
  (local (include-book "arithmetic-3/bind-free/top" :dir :system))
  (local (include-book "arithmetic-3/floor-mod/floor-mod" :dir :system))
  
  (defthm mod-x-y-=-x---y
    (implies (and (and (rationalp x)
                       (rationalp y)
                       (not (equal y 0)))
                  (if (< 0 y)
                      (and (<= y x)
                           (< x (* 2 y)))
                    (and (<= x y)
                         (< (* 2 y) x))))
             (equal (mod x y) (- x y)))
    :hints ((nonlinearp-default-hint stable-under-simplificationp hist pspv))
    :rule-classes ((:rewrite :backchain-limit-lst 0)
                   (:rewrite
                    :corollary
                    (implies (and (rationalp x)
                                  (rationalp y)
                                  (not (equal y 0)))
                             (equal (equal (mod x y) (- x y))
                                    (if (< 0 y)
                                        (and (<= y x)
                                             (< x (* 2 y)))
                                      (and (<= x y)
                                           (< (* 2 y) x))))))))
)
|#
