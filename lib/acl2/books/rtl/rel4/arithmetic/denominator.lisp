(in-package "ACL2")

(local (include-book "ground-zero"))
(local (include-book "fp2"))

;denom of non-rat?

(defthm denominator-positive-integer-type-prescription
  (and (< 0 (denominator x))
       (integerp (denominator x)))
  :rule-classes (:type-prescription))

(defthm denominator-positive
  (< 0 (denominator x))
  :rule-classes (:rewrite :linear))

(defthm denominator-integerp
  (integerp (denominator x)))

(defthm denominator-one-means-integer
  (implies (case-split (rationalp x))
           (equal (equal (denominator x) 1)
                  (integerp x)))
  :hints (("goal" :in-theory (disable rational-implies2)
           :use (rational-implies2
                 (:instance lowest-terms 
                            (n (denominator x))
                            (r x) 
                            (q 1))))))

(defthm denominator-of-integer-is-one
  (implies (integerp x)
           (equal (denominator x)
                  1)))
;linear?
(encapsulate
 ()
 (local (include-book "../../../arithmetic/mod-gcd"))
 (defthm denominator-lower-bound
   (implies (and (< 0 q)
                 (integerp p)
                 (integerp q)
                 )
            (<= (denominator (* p (/ q))) q))
   :hints (("goal" :use (:instance least-numerator-denominator-<= (n p) (d q))))
   ))