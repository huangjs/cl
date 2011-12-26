(in-package "ACL2")

(defun fl (x)
  (declare (xargs :guard (real/rationalp x)))
  (floor x 1))

(include-book "ground-zero")
(include-book "negative-syntaxp")

(local (include-book "floor"))
(local (include-book "arith2"))
(local (include-book "power2p"))
(local (include-book "fp2"))
(local (include-book "arith"))
(local (include-book "integerp"))
(local (include-book "product"))
(local (include-book "complex-rationalp"))
(local (include-book "../../../meta/meta-plus-equal"))
(local (include-book "../../../meta/meta-plus-lessp"))
(local (include-book "predicate"))
(local (include-book "rationalp"))


#| 

Todo: We could probably prove REM analogs for most of the rules in this book (since REM and MOD agree on a
certain range of inputs), but we don't use REM much at all in the library (for good reason, thinks Eric), so
perhaps this isn't worth spending time on.

|#




(defthm mod-rational-when-y-is-rational-rewrite
  (implies (and (rationalp y)
                (case-split (acl2-numberp x)))
           (equal (rationalp (mod x y))
                  (rationalp x)))
  :hints (("Goal" :in-theory (enable mod))))

;mod when x is complex?

(defthm mod-with-x-a-non-acl2-number-is-zero
  (implies (not (acl2-numberp x))
           (equal (mod x y)
                  0))
  :hints (("Goal" :in-theory (enable mod))))




(defthmd mod-when-y-is-complex-rationalp
  (implies (complex-rationalp y)
           (equal (mod x y)
                  (if (not (complex-rationalp x))
                      (fix x)
                    (if (not (rationalp (/ x y)))
                        x
                      (if (integerp (/ x y))
                          0 
                        (+ X (* -1 Y (FLOOR (* X (/ Y)) 1))) ;this case is gross (basically the defn of mod)
                        )))))
  :otf-flg t
  :hints (("Goal" :in-theory (enable mod))))

;enable?
(defthmd mod-when-y-is-not-an-acl2-numberp
  (implies (not (acl2-numberp y))
           (equal (mod x y)
                  (fix x)))
  :otf-flg t
  :hints (("Goal" :in-theory (enable mod))))


;we probably get this when mod is defined.  perhaps we should disable mod's t-p rule since this duplicates it. 
(defthm mod-acl2-numberp-type-prescription
  (acl2-numberp (mod x y))
  :rule-classes (:type-prescription))

;Perhaps we don't need this as a rewrite rule, but here it is anyway:
(defthm mod-acl2-numberp
  (acl2-numberp (mod x y)))

;add a weaker vesion of this in case we can establish (rationalp x) but not (not (complex-rationalp x)) ?
(defthm mod-rationalp
  (implies (case-split (not (complex-rationalp x)))
           (rationalp (mod x y)))
  :rule-classes (:rewrite :type-prescription)
  :hints (("Goal" :cases ((rationalp x))
           :in-theory (enable mod))))

;I weakened the hyp on x as much as possible and then weakened the hyps on y as much as possible.  (We might
;get a different rule by doing the reverse of that.)
(defthm mod-non-negative
  (implies (and (case-split (< 0 y)) ;can't gen: (mod -1 0) = -1 and  (mod 3 -2) = -1
                (case-split (not (complex-rationalp x))) ;can't gen: (mod #C(-4 -3) 1) = #c(-4 -3)
                (case-split (not (complex-rationalp y))) ;can't gen: (mod -3 #c(1 1)) = -3
                )
           (<= 0 (mod x y)))
  :hints (("Goal" :cases ((acl2-numberp y))
           :in-theory (enable mod))))

(defthm mod-non-negative-rationalp-type-prescription
  (implies (and (case-split (< 0 y))                     
                (case-split (not (complex-rationalp x))) 
                (case-split (not (complex-rationalp y))) 
                )
           (and (<= 0 (mod x y))
                (rationalp (mod x y)) ;we might as well include this
                ))
  :rule-classes ((:type-prescription :typed-term (mod x y))))

(defthm mod-non-negative-linear
  (implies (and (case-split (< 0 y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (<= 0 (mod x y)))
  :rule-classes ((:linear :trigger-terms ((mod x y)))))

(defthm mod-upper-bound
  (implies (and (case-split (< 0 y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (< (mod x y) y))
  :hints (("Goal" :in-theory (enable mod) :cases ((rationalp x)))))

(defthm mod-upper-bound-linear
  (implies (and (case-split (< 0 y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (< (mod x y) y))
  :rule-classes ((:linear :trigger-terms ((mod x y)))))

;included to help a hyp which matches this rule's conclusion get written away quickly
(defthm mod-upper-bound-less-tight-rewrite
  (implies (and (case-split (< 0 y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (<= (mod x y) y)))

;do we need this?  is it expensive?
(defthm mod-upper-bound-3
  (implies (and (<= y z)
                (case-split (< 0 y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (< (mod x y) z)))

(defthm mod-upper-bound-2
  (implies (and (<= 0 x)
                (case-split (not (complex-rationalp x)))
                )
           (<= (mod x y) x))
  :rule-classes (:rewrite (:linear :trigger-terms ((mod x y))))
  :hints (("Goal" :in-theory (enable mod))))

(defthm mod-integerp
  (implies (and (integerp x) ;can't gen: (mod 2/3   5)=2/3
                (integerp y) ;can't gen: (mod 5   2/3)=1/3
                )
           (integerp (mod x y)))
  :hints (("Goal" :in-theory (enable mod)))
  :rule-classes (:rewrite :type-prescription))

;what about when x is a known int?
(defthm mod-integerp-2
  (implies (and (integerp y)
                (case-split (acl2-numberp x))
                )
           (equal (integerp (mod x y))
                  (integerp x)))
  :hints (("Goal" :in-theory (enable mod))))

(defthm mod-0
    (and (equal (mod 0 y)
                0)
         (equal (mod x 0)
                (fix x)))
  :hints (("Goal" :in-theory (enable mod))))

(defthm mod-complex-rationalp-rewrite
  (implies (case-split (rationalp y))
           (equal (complex-rationalp (mod x y))
                  (complex-rationalp x)))
  :hints (("Goal" :in-theory (enable mod))))

;Don't make this a rewrite rule (we don't want to backchain to (< y 0) to establish (rationalp (mod x y))
(defthm mod-non-positive-type-prescription
  (implies (and (< y 0) ;rarely will be the case
                (rationalp x)
                (rationalp y)
                )
           (and (rationalp (mod x y))
                (<= (mod x y) 0)))
  :hints (("Goal" :in-theory (enable mod)))
  :rule-classes (:type-prescription))

(defthm mod-non-positive
  (implies (and (< y 0) ;rarely will be the case
                (case-split (rationalp x))
                (case-split (rationalp y))
                )
           (<= (mod x y) 0)))

;rewrite a claim about mod being non-positive to a claim about y?

(local (include-book "fl")) ;drop?

(defthm mod-drop-irrelevant-first-term
  (implies (and (integerp (* k (/ y)))
                (case-split (not (equal y 0)))
                (case-split (rationalp y))
                (case-split (not (complex-rationalp x)))
                )
           (equal (mod (+ k x) y)
                  (mod x y)))
  :hints (("Goal"  :in-theory (enable mod))))

(defthm mod-drop-irrelevant-second-term
  (implies (and (integerp (* k (/ y)))
                (case-split (not (equal y 0)))
                (case-split (rationalp y))
                (case-split (not (complex-rationalp x)))
                )
           (equal (mod (+ x k) y)
                  (mod x y))))

(defthm mod-drop-irrelevant-second-term-with-more-terms
  (implies (and (integerp (* k (/ y)))
                (case-split (not (equal y 0)))
                (case-split (rationalp y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp x2)))
                )
           (equal (mod (+ x k x2) y)
                  (mod (+ x x2) y))))

(defthm mod-drop-irrelevant-third-term
  (implies (and (integerp (* k (/ y)))
                (case-split (not (equal y 0)))
                (case-split (rationalp y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp x2)))
                )
           (equal (mod (+ x x2 k) y)
                  (mod (+ x x2) y))))

;We could make analogs to MOD-DROP-IRRELEVANT-SECOND-TERM in which we drop the third, fourth, etc. term.

(defthm mod-mult-eric
  (implies (and (integerp a)
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y))) ;gen?
                )
           (equal (mod (+ x (* a y)) y)
                  (mod x y))))

;similar lemmas in mod.lisp
;change params on the rest?

;could we generalize (mod x2 y) to (* k (mod x2 y)) ??
;I don't think we can drop either hyp.
(defthm mod-sum-elim-second
  (implies (and (case-split (not (complex-rationalp x1)))
                (case-split (not (complex-rationalp x2))) 
                )
           (equal (mod (+ x1 (mod x2 y)) y)
                  (mod (+ x1 x2) y)))
  :hints (("Goal" :in-theory (enable mod)
           :cases ((and (rationalp x2) (equal y 0) (rationalp x1))
                   (and (rationalp x2) (equal y 0) (not (rationalp x1)))
                   (and (rationalp x2) (not (equal y 0)) (rationalp x1))
                   (and (rationalp x2) (not (equal y 0)) (not (rationalp x1)))
                   (and (not (acl2-numberp x2)) (equal y 0) (rationalp x1))
                   (and (not (acl2-numberp x2)) (equal y 0) (not (rationalp x1)))
                   (and (not (acl2-numberp x2)) (not (equal y 0)) (rationalp x1))
                   (and (not (acl2-numberp x2)) (not (equal y 0)) (not (rationalp x1)))))))

(defthm mod-sum-elim-second-gen
  (implies (and (integerp (/ y2 y))
                (case-split (not (complex-rationalp x1)))
                (case-split (not (complex-rationalp x2))) 
                (case-split (not (equal y 0)))
                (case-split (rationalp y))
                )
           (equal (mod (+ x1 (mod x2 y2)) y)
                  (mod (+ x1 x2) y)))
  :hints (("Goal" :in-theory (enable mod)
           :cases ((and (rationalp x2) (equal y 0) (rationalp x1))
                   (and (rationalp x2) (equal y 0) (not (rationalp x1)))
                   (and (rationalp x2) (not (equal y 0)) (rationalp x1))
                   (and (rationalp x2) (not (equal y 0)) (not (rationalp x1)))
                   (and (not (acl2-numberp x2)) (equal y 0) (rationalp x1))
                   (and (not (acl2-numberp x2)) (equal y 0) (not (rationalp x1)))
                   (and (not (acl2-numberp x2)) (not (equal y 0)) (rationalp x1))
                   (and (not (acl2-numberp x2)) (not (equal y 0)) (not (rationalp x1)))))))


;Follows from MOD-SUM-ELIM-SECOND
(defthm mod-sum-elim-first
  (implies (and (case-split (not (complex-rationalp a)))
                (case-split (not (complex-rationalp b)))
                )
           (equal (mod (+ (mod b y) a) y)
                  (mod (+ a b) y))))

;Follows from MOD-SUM-ELIM-SECOND-GEN
(defthm mod-sum-elim-first-gen
  (implies (and (integerp (/ y2 y))
                (case-split (not (complex-rationalp x1)))
                (case-split (not (complex-rationalp x2))) 
                (case-split (not (equal y 0)))
                (case-split (rationalp y))
                )
           (equal (mod (+ (mod x2 y2) x1) y)
                  (mod (+ x1 x2) y))))

;Follows from MOD-SUM-ELIM-SECOND and MOD-SUM-ELIM-FIRST
;Do we really need this if we have the other two?
(defthm mod-sum-elim-both
  (implies (and (case-split (not (complex-rationalp a)))
                (case-split (not (complex-rationalp b)))
                )
           (equal (mod (+ (mod a y) (mod b y)) y)
                  (mod (+ a b) y))))

;see mod-diff
(defthm mod-difference-elim-second
  (implies (and (case-split (rationalp x1))
                (case-split (rationalp x2))
                )
           (equal (mod (+ x1 (* -1 (mod x2 y))) y)
                  (mod (+ x1 (* -1 x2)) y)))
  :hints (("Goal" :in-theory (enable mod))))

;Follows from MOD-DIFFERENCE-ELIM-SECOND
;bad name?
(defthm mod-sum-elim-negative-first-arg
  (implies (and (case-split (rationalp x1))
                (case-split (rationalp x2))
                )
           (equal (mod (+ (* -1 (mod x2 y)) x1) y)
                  (mod (+ (* -1 x2) x1) y))))

(defthm mod-by-1
  (implies (integerp m)
           (equal (mod m 1)
                  0)))

;I'm going to try keeping this disabled, since relieving the first hyp may be expensive.
;rename: no more n!
;(integerp (* x (/ y))) basically says that x is a multiple of y, which is 
;basicially what (equal (mod x y) 0) says too.
(defthmd mod-mult-of-n
  (implies (and (integerp (* x (/ y)))
                (not (equal y 0))
                (rationalp x)
                (rationalp y)
                )
           (equal (mod x y)
                  0))
  :hints (("goal" :in-theory (enable mod))))

;prove a rule for negative x too?
;try disabling?
(defthmd mod-negative-y
  (implies (and (< 0 y)
                (integerp x)
                (integerp y)
                )
           (equal (mod x (- y))
                  (if (integerp (/ x y))
                      0
                    (+ (- y) (mod x y)))))
  :hints (("Goal" :in-theory (enable mod))))


;BOZO
;try disabling???
(defthm mod-does-nothing
  (implies (and (< m n)
                (<= 0 m)
                (case-split (rationalp m)))
           (equal (mod m n)
                  m))
  :hints (("Goal" :in-theory (enable mod))))


;better name
;can derive mod-of-mod, mod-idempotent from this?
;perhaps keep this disabled??
(defthm mod-mod-e
  (implies (and (integerp (/ y1 y2))
                (case-split (not (equal y2 0)))
                (case-split (rationalp y1))
                (case-split (rationalp y2))
                )
           (equal (mod (mod x y1) y2)
                  (mod x y2)))
  :hints (("Goal" :in-theory (enable mod))))

(defthm mod-of-mod
  (implies (and (case-split (natp k))
                (case-split (natp n)))
           (equal (mod (mod x (* k n)) n)
                  (mod x n))))

;Follows from mod-mod-e and mod-by-0
(defthm mod-idempotent
  (implies (and (case-split (rationalp x)) ;(integerp x)
                (case-split (rationalp y)) ;(integerp y)
                )
           (equal (mod (mod x y) y)
                  (mod x y))))


;cute; why does this help so much?
;like quot-mod
(defthm mod-fl-2
  (implies (case-split (acl2-numberp x))
           (equal (+ (* y (fl (/ x y))) (mod x y))
                  x))
  :rule-classes ()
  :hints (("Goal" :in-theory (enable floor-fl mod))))

(defthm mod-def
  (implies (case-split (acl2-numberp x))
           (equal (mod x y)
                  (- x (* y (fl (/ x y))))))
  :hints (("Goal"
           :in-theory (union-theories (current-theory :here)
                                      (theory 'ground-zero))))
  :rule-classes ())

;a is a free var
(defthmd mod-force-eric
  (implies (and (<= (* a y) x)
                (< x (* (1+ a) y))
                (integerp a)
                (rationalp x)
                (rationalp y)
                )
           (equal (mod x y)
                  (- x (* a y))))
  :hints
  (("goal" :in-theory (enable mod)
    :use ((:instance fl-unique (x (/ x y)) (n a))))))



;chose a in mod-force-eric to be -1
;expensive?
(defthmd mod-force-chosen-a-neg
    (implies (and (< x 0)
		  (<= (* -1 y) x)
                  (rationalp x)
                  (rationalp y)
                  )
	     (equal (mod x y)
                    (- x (* -1 y))))
    :hints (("Goal" :in-theory (disable mod-force-eric)
         :use (:instance mod-force-eric (a -1)))))

;gen?
;or could rewrite to (equal 0 (mod x 2))
(defthm mod-even
  (implies (rationalp x)
           (equal (integerp (* 1/2 (mod x 2)))
                  (integerp (* 1/2 x))))
  :hints (("Goal" :in-theory (enable mod))))

;gen 2 to m?
(defthm mod-even-gen
  (implies (and (rationalp x)
                (integerp n)
                (integerp (* 1/2 n)) ;address the other case?
                )
           (equal (integerp (* 1/2 (mod x n)))
                  (integerp (* 1/2 x))))
  :hints (("Goal" :in-theory (enable mod))))


;Enforces a new normal form for mod in which we force the second arg to be 1.
;Maybe this is just a weird idea.
;BOZO bad name?
(defthmd mod-cancel
  (implies (syntaxp (not (and (quotep y) (equal (cadr y) 1)))) ;prevents looping
           (equal (mod x y)
                  (if (acl2-numberp x)
                      (if (acl2-numberp y)
                          (if (equal 0 y)
                              x
                            (* y (mod (/ x y) 1)))
                        x)
                    0)))
  :hints (("Goal" :in-theory (enable mod inverse-of-*)))
  )


;can derive at least 1 thm above from this?
(defthmd mod-equal-0
  (implies (and ;(case-split (rationalp x))
                (case-split (rationalp y)) ;gen?
                (case-split (not (equal y 0)))
                )
           (equal (equal (mod x y) 0)
                  (integerp (* (/ y) x))))
  :hints (("Goal" 
           :in-theory (enable mod))))

(defthmd mod-minus-case-non-integerp
  (implies (and (not (integerp (/ x y)))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (equal (mod (* -1 x) y)
                  (- y (mod x y))))
  :hints (("Goal" :in-theory (enable mod))))

(defthmd mod-minus-case-integerp
  (implies (and (integerp (/ x y))
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y)))
                )
           (equal (mod (* -1 x) y)
                  (- (mod x y))))
  :hints (("Goal" :in-theory (enable mod))))

(defthmd mod-minus-case-integerp-better
  (implies (and (integerp (/ x y))
                (case-split (not (equal 0 y)))
                (case-split (rationalp y))
                )
           (equal (mod (* -1 x) y)
                  0))
  :hints (("Goal" :use (mod-minus-case-integerp mod-equal-0))))

;old version
;disable?
(defthm mod-minus
  (implies (and (case-split (rationalp x))
                (case-split (rationalp y))
                )
           (equal (mod (* -1 x) y)
                  (if (equal 0 y)
                      (- x)
                    (if (integerp (/ x y))
                        0
                      (- y (mod x y))))))
  :hints (("Goal" :in-theory (e/d ( mod-minus-case-integerp-better
                                    mod-minus-case-non-integerp)
                                  ())
           :cases ((integerp (/ x y))))))  

(defthm mod-minus-alt
  (implies (and (syntaxp (negative-syntaxp x))
                (case-split (rationalp x))
                (case-split (rationalp y))
                )
           (equal (mod x y)
                  (if (equal 0 y)
                      x
                    (if (integerp (/ (- x) y))
                        0
                      (- y (mod (- x) y))))))
           :hints (("Goal" :in-theory (disable mod-minus) :use (:instance mod-minus (x (- x))))))

(defthm mod-1-integerp
  (implies (case-split (acl2-numberp x))
           (equal (integerp (mod x 1))
                  (integerp x)))
  :hints (("Goal" :in-theory (enable mod)))
  )




; needs fl-of-odd/2
;keep this disabled
;gen?
(defthmd mod-by-2-rewrite-to-even
  (implies (integerp x)
           (equal (equal (mod x 2) 0)
                  (integerp (* 1/2 x))))
  :otf-flg t
  :hints (("Goal" :in-theory (enable mod))))

(defthm fl-plus-md
  (implies (rationalp x)
           (equal (+ (fl x) (mod x 1))
                  x))
  :hints (("Goal" :in-theory (enable mod))))

;sort of an odd rule...
(defthm mod-1-sum-integer
  (implies (and (rationalp x)
                (rationalp y))
           (equal (integerp (+ x (mod y 1)))
                  (integerp (+ x y))))
  :hints (("Goal" :in-theory (enable mod))))

;needed?
;expensive?
#|
??
ex:   (INTEGERP (* (/ (EXPT 2 J))
               (MOD X (* 2 (EXPT 2 I)))))
|#
;bad name?
(defthm mod-quotient-integerp
  (implies (and (integerp (* y k))
                (rationalp x)
                (rationalp y)
                (rationalp k)
                )
           (equal (integerp (* k (mod x y)))
                  (integerp (* k x))))
  :hints (("Goal" :in-theory (enable mod))))







;gen
;may someday subsume mod-idempotent
(defthm mod-mod-2-thm
  (implies (and (<= y1 y2)
;test                (case-split (<= 0 y2))
                (case-split (< 0 y1))                 ;drop?
                (case-split (acl2-numberp x)) ;gen?
                (case-split (rationalp y1))
                (case-split (rationalp y2))
                (case-split (not (equal y1 0)))
                )
           (equal (mod (mod x y1) y2)
                  (mod x y1)))
  :otf-flg t
  :hints (("Goal" :in-theory (enable mod))))




;keep disabled?
(defthmd mod-2-1-means-odd
  (implies (integerp x)
           (equal (equal (mod x 2) 1)
                  (not (integerp (* 1/2 x)))))
  :hints (("Goal" :in-theory (enable mod))))

;unlikely to fire automatically
;make a t-p rule too?
(defthm mod-integerp-2-2
  (implies (and (integerp y)
                (integerp x))
           (integerp (mod x (/ y))))
  :hints (("Goal" :cases ((equal 0 y))
           :in-theory (enable mod))))


;like mod-prod?
(defthm mod-cancel-special-1
  (implies (and (acl2-numberp x)
                (rationalp k)
                (acl2-numberp y)
                (not (equal y 0))
                (not (equal k 0)))
           (equal (mod (* k x)
                       (* y k))
                  (* k (mod x y))))
  :hints (("goal" :in-theory (enable mod-cancel))))

;move up
;expensive?
(defthm mod-integerp-when-y-is-an-inverse
  (implies (and (integerp (/ y))
                (integerp x))
           (integerp (mod x y)))
  :hints (("Goal" :in-theory (enable mod))))

;this is a bit odd..
(defthm mod-when-y-is-an-inverse
  (implies (and (integerp (/ y))
                (integerp x)
                (case-split (< 0 y))
                )
           (equal (mod x y)
                  0))
  :hints (("Goal" :in-theory (enable mod))))

(defthm fl-mod-x-1
  (equal (fl (mod x 1)) 
         0))

(defthmd mod-by-2
  (implies (integerp x)
           (equal (mod x 2)
                  (if (integerp (* 1/2 x))
                      0
                    1)))
  :hints (("Goal" :in-theory (enable mod-by-2-rewrite-to-even mod-2-1-means-odd))))





#|
(defthm mod-with-y-not-rational
  (implies (and (acl2-numberp x)
                (not (rationalp y)))
           (equal (mod x y)
                  x))
  :hints (("Goal" :in-theory (enable mod))))

(defthm mod-negative-rewrite
  (implies (and ;(< 0 y)
                (case-split (rationalp y))
                (case-split (rationalp x)))
           (implies (< (mod x y) 0)
                    (and (not (integerp (* x (/ y))))
                         (< y 0)
                         )))
  :rule-classes nil
  :hints (("Goal" :in-theory (enable mod)))
)


not done
(defthm mod-of-complex
  (implies (complex-rationalp x)
           (equal (mod x y)
                  xx))
  :otf-flg t
  :hints (("Goal" :in-theory (enable mod)
           :cases ((rationalp y) (not (acl2-numberp y))))))
|#




(encapsulate
 ()
 (local (defthm mod-sum-move-forward-implication
          (implies (and (case-split (<= 0 k1))
                        (case-split (< k1 y))
                        (rationalp y)
                        (rationalp x)
                        (rationalp k1)
                        )
                   (implies (equal k1 (mod (+ k2 x) y))
                            (equal (mod (+ k1 (- k2)) y) (mod x y))))
          :rule-classes nil
          ))

 (local (defthm mod-sum-move-backward-implication
          (implies (and (case-split (<= 0 k1))
                        (case-split (< k1 y))
                        (rationalp y)
                        (rationalp x)
                        (rationalp k1)
                        )
                   (implies (equal (mod (+ k1 (- k2)) y) (mod x y))
                            (equal k1 (mod (+ k2 x) y))))
          :rule-classes nil
          :hints (("Goal" :use ((:instance mod-sum-move-forward-implication
                                           (k1 (mod x y))
                                           (x k1)
                                           (k2 (- k2))
                                           ))))))

;I put this in :rule-classes nil, because it can loop if it is a rewrite rule..
 (defthm mod-sum-move
   (implies (and (case-split (<= 0 k1))
                 (case-split (< k1 y))
                 (case-split (rationalp y))
                 (case-split (rationalp x))
                 (case-split (rationalp k1))
;(rationalp k2)
                 )
            (equal (equal k1 (mod (+ k2 x) y))
                   (equal (mod (+ k1 (- k2)) y) (mod x y))))
   :rule-classes nil
   :hints (("Goal" :use (mod-sum-move-forward-implication
                         mod-sum-move-backward-implication)))))


;Unlike the above, this rule shouldn't loop; since k1 and k2 are constants and we compute (+ k1 (- k2)) in the
;conclusion...
(defthm mod-sum-move-constants
  (implies (and (syntaxp (and (quotep k1)
                              (quotep k2)
;                              (quotep y)  ;drop?
                              )
                         )
                (case-split (<= 0 k1))
                (case-split (< k1 y))
                (rationalp y)
                (rationalp x)
                (rationalp k1)
    ;(rationalp k2)
                )
           (equal (equal k1 (mod (+ k2 x) y))
                  (equal (mod (+ k1 (- k2)) y) (mod x y))))
  :hints (("Goal" :use mod-sum-move)))



;BOZO don't need some of these?
(defthm mod-sums-cancel-1
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x1))
                (case-split (rationalp x2))
                )
           (equal (equal (mod (+ k x1) y) (mod (+ k x2) y))
                  (equal (mod x1 y) (mod x2 y))))
  :hints (("Goal" :use (:instance mod-sum-move (k2 k) (x x2) (k1 (mod (+ k x1) y))))))

(defthm mod-sums-cancel-2
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x1))
                (case-split (rationalp x2))
                )
           (equal (equal (mod (+ x1 k) y) (mod (+ k x2) y))
                  (equal (mod x1 y) (mod x2 y)))))

(defthm mod-sums-cancel-3
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x1))
                (case-split (rationalp x2))
                )
           (equal (equal (mod (+ x1 k) y) (mod (+ x2 k) y))
                  (equal (mod x1 y) (mod x2 y)))))

;don't need this one..?
(defthm mod-sums-cancel-4
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x1))
                (case-split (rationalp x2))
                )
           (equal (equal (mod (+ k x1) y) (mod (+ x2 k) y))
                  (equal (mod x1 y) (mod x2 y)))))

(defthm mod-sums-cancel-5
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x))
                )
           (equal (equal (mod k y) (mod (+ x k) y))
                  (equal 0 (mod x y))))
  :hints (("Goal" :use (:instance mod-sums-cancel-4 (x1 0) (x2 x)))))

(defthm mod-sums-cancel-6
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x))
                )
           (equal (equal (mod k y) (mod (+ k x) y))
                  (equal 0 (mod x y))))
  :hints (("Goal" :use (:instance mod-sums-cancel-4 (x1 0) (x2 x)))))

;don't need this one..?
(defthm mod-sums-cancel-7
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x))
                )
           (equal (equal (mod (+ k x) y) (mod k y))
                  (equal 0 (mod x y))))
  :hints (("Goal" :use (:instance mod-sums-cancel-4 (x1 0) (x2 x)))))

;don't need this one..?
(defthm mod-sums-cancel-8
  (implies (and (case-split (<= 0 y))
                (case-split (rationalp k))
                (case-split (rationalp y))
                (case-split (rationalp x))
                )
           (equal (equal (mod (+ x k) y) (mod k y))
                  (equal 0 (mod x y))))
  :hints (("Goal" :use (:instance mod-sums-cancel-4 (x1 0) (x2 x)))))
  



#|
;weird case
;not quite right
(thm
 (implies (and ;(<= 0 x1)
               ;(<= 0 x2)
               (rationalp x1)
               (rationalp x2)
               (rationalp y)
               (<= 0 y)
               (< x1 0)
               (< 0 x2)
               (< (+ x1 x2) 0)
               (< (- y) (+ x1 x2))
               )
          (equal (+ (mod x1 y) (mod x2 y))
                 (+ y (mod (+ x1 x2) y))))
 :hints (("Goal" :in-theory (enable  mod-force-chosen-a-neg)))
 )
|#


(defthm fl-mod-equal
  (implies (and (equal (fl (/ x 2)) (fl (/ y 2)))
                (equal (mod x 2) (mod y 2))
                (acl2-numberp x)
                (acl2-numberp y)
                )
           (equal x y))
  :hints (("goal" :in-theory (enable mod)))
  :rule-classes nil)


#|

;there are many ways to say that x is even.  perhaps we should forward chain from any of them to each of the others.
(defthmd blah
  (equal (equal (mod x 2) 0)
         (evenp x))
  :hints (("Goal" :in-theory (enable evenp mod-equal-0)))
  )

(defthm mod-of-1-plus-even
  (equal (mod (+ 1 (* 2 x)) y)
	 (+ 1 (* 2 (mod x (/ y 2)))))
  :hints (("Goal" :in-theory (e/d (mod)
                                  (LESS-THAN-MULTIPLY-THROUGH-BY-INVERTED-FACTOR-FROM-RIGHT-HAND-SIDE)))))

|#



;====


(defun natp (x)
  (declare (xargs :guard t))
  (and (integerp x)
       (<= 0 x)))




;comes from mod-upper-bound
(defthmd mod-bnd-1
  (implies (and (case-split (< 0 n))
                (case-split (not (complex-rationalp m)))
                (case-split (not (complex-rationalp n)))
                )
           (< (mod m n) n))
  :rule-classes :linear)

;why not just enable mod whenever we'd use this rule?
;make an alternate definition of MOD in terms of FL?
(defthm quot-mod
  (implies (case-split (acl2-numberp m))
           (equal (+ (* n (fl (/ m n))) (mod m n))
                  m))
  :rule-classes ()
  :hints (("Goal" :in-theory (e/d (mod floor-fl)))))

;proved in mod2
;like old mod+-thm
;make alt
(defthm mod-mult-eric
  (implies (and (integerp a)
                (case-split (not (complex-rationalp x)))
                (case-split (not (complex-rationalp y))) ;gen?
                )
           (equal (mod (+ x (* a y)) y)
                  (mod x y))))



(defthm integerp-mod
    (implies (and (integerp m)
		  (integerp n))
	     (integerp (mod m n)))
  :rule-classes (:rewrite :type-prescription)
  :hints (("Goal" :in-theory (enable mod))))  

; Matt K., November 2006:
; We prefer to export (to lib) a version of the following rule that does not
; have a case-split in the hypothesis, because the case-split significantly
; slowed down a proofs in the regression suite in
; books/workshops/2004/schmaltz-borrione/support and
; books/workshops/2004/legato/support/, and we want to be compatible with
; books/arithmetic-3/.  (The rule rationalp-mod also appears in
; books/arithmetic-3/floor-mod/floor-mod.lisp, in support of those books.)
; However, we need the case-split version for mod-bits in
; ../support/support/bits-proofs.lisp.

(defthm rationalp-mod
  (implies (rationalp x)
           (rationalp (mod x y)))
  :rule-classes (:rewrite :type-prescription)
  :hints (("Goal" :in-theory (enable mod))))

(defthm rationalp-mod-case-split
  (implies (case-split (rationalp x))
           (rationalp (mod x y)))
  :rule-classes (:rewrite :type-prescription)
  :hints (("Goal" :in-theory (enable mod))))

(in-theory (disable rationalp-mod))


;better conclusion?
;this follows pretty trivially from quot-mod
;never used in support/
(defthm mod-0-fl
    (implies (acl2-numberp m)
	     (iff (= (mod m n) 0)
		  (= m (* (fl (/ m n)) n))))
  :rule-classes ()
  :hints (("goal" :use (quot-mod))))


;wow! this just goes through!
(defthm mod-0-0
  (implies (and (integerp p)
                (rationalp m)
                (rationalp n)
                )
           (iff (= (mod m (* n p)) 0)
                (and (= (mod m n) 0)
                     (= (mod (fl (/ m n)) p) 0))))
  :rule-classes ()
  :hints (("goal" :in-theory (enable mod))))

;BOZO more like this?
(defthmd mod-prod
  (implies (and (rationalp m)
                (rationalp n)
                (rationalp k)
                )
           (equal (mod (* k m) (* k n))
                  (* k (mod m n))))
  :hints (("goal" :in-theory (enable mod-cancel))))

(defthm mod012
  (implies (integerp m)
           (or (equal (mod m 2) 0)
               (equal (mod m 2) 1)))
  :rule-classes ()
  :hints (("Goal" :use ((:instance mod-bnd-1 (m m) (n 2))))))

;gen the 2?
;bad name?
(defthm mod-mod-2-not-equal
   (implies (acl2-numberp m) ;(integerp m)
            (not (= (mod m 2) (mod (1+ m) 2))))
   :rule-classes ()
   :hints (("Goal" :use ((:instance quot-mod (m m) (n 2))
                         (:instance quot-mod (m (1+ m)) (n 2))))))

;change the formals on these?
;these are from mod2
(encapsulate
 ()
 (defthmd mod-sum
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
;BOZO
 (defthmd mod-bnd-2
   (implies (and (<= 0 m)
                 (case-split (rationalp m))
                 )
            (<= (mod m n) m))  
   :rule-classes :linear)

 )

;BOZO make this into a better rewrite rule (and generalize)
;this is sort of a cancellation rule
;prove from my mod cancellation rules?
(defthm mod-plus-mod-2
  (implies (and (integerp a)
                (integerp b))
           (iff (= (mod (+ a b) 2) (mod a 2))
                (= (mod b 2) 0)))
  :rule-classes ()
  :hints (("goal" :in-theory (e/d (mod-mult-of-n)
                                  (MOD-SUM-ELIM-SECOND
                                   MOD-DROP-IRRELEVANT-SECOND-TERM
                                   MOD-2-1-MEANS-ODD
                                   ))
           :use ((:instance mod012 (m b))
                 (:instance mod-sum (a a) (b b) (n 2))
                 (:instance mod-mod-2-not-equal)))))

;bad name
;The only multiple of N between 0 and 2N is N itself.
(defthm mod-must-be-n
    (implies (and (= (mod m n) 0)
                  (< m (* 2 n))
		  (< 0 m)
                  (rationalp m);(integerp m)
		  (rationalp n);(integerp n)
		  )
	     (= m n))
  :rule-classes ()
  :hints (("goal" :in-theory (disable  MOD-does-nothing)
           :use ((:instance mod-does-nothing (m m) (n n))
                 (:instance mod-does-nothing (n n) (m (- m n)))
;			(:instance mod+-thm (m (- m n)) (a 1))
                 ))))

;We need this here so that ACL2 knows how to make natp-mod into a type-prescription rule.
(defthm natp-compound-recognizer
  (equal (natp x)
         (and (integerp x)
              (<= 0 x)))
  :rule-classes :compound-recognizer)


;drop?
(defthm natp-mod
  (implies (and (natp m)
                (natp n))
           (natp (mod m n)))
  :rule-classes ((:type-prescription :typed-term (mod m n)))
  :hints (("Goal" :use (:instance mod-non-negative (x m) (y n)))))
           
(defthm natp-mod-rewrite
  (implies (and (natp m)
                (natp n))
           (natp (mod m n))))

;BOZO kill ;gen?  make alt?
;see mod-mult-eric
(defthmd mod-mult
    (implies (and (integerp a)
                  (rationalp m)
		  (rationalp n))
	     (equal (mod (+ m (* a n)) n)
		    (mod m n)))
  :hints (("Goal" ;:use mod+-thm
           )))

;gen?
;essentially  mod-difference-elim-second
(defthmd mod-diff
    (implies (and (case-split (rationalp a))
                  (case-split (rationalp b))
                  )
	     (equal (mod (- a (mod b n)) n)
		    (mod (- a b) n))))


;this doesn't seem to be used anywhere
(defthmd mod-bnd-3
    (implies (and (< m (+ (* a n) r))
		  (<= (* a n) m)
                  (integerp a)
                  (case-split (rationalp m))
		  (case-split (rationalp n))
		  )
	     (< (mod m n) r))
  ;; Free variables make this rule very weak, but it seems harmless
  ;; enough to make it a :linear rule.
  :rule-classes :linear
  :hints (("goal" :in-theory (disable FL-<-INTEGER) ;why needed?
           :use ((:instance quot-mod)
			(:instance n<=fl-linear (x (/ m n)) (n a))))))

(defthm mod-force
  (implies (and (<= (* a n) m)
                (< m (* (1+ a) n))
                (integerp a)
                (rationalp m)
                (rationalp n)
                )
           (= (mod m n) (- m (* a n))))
  :rule-classes nil :hints
  (("goal" :use
    ((:instance quot-mod)
     (:instance fl-unique (x (/ m n)) (n a))))))


(local
 (defthm mod=mod-1
   (implies (and (= (mod a n) (mod b n))
                 (rationalp a) ; (>= a 0)
                 (rationalp b); (>= b 0)
;                 (integerp n) ;(> n 0)
                 )
            (= (- a (* n (fl (/ a n))))
               (- b (* n (fl (/ b n))))))
   :rule-classes ()
   :hints (("goal" :use ((:instance quot-mod (m a))
                         (:instance quot-mod (m b)))))))

(local
 (defthm mod=mod-2
   (implies (and (= (mod a n) (mod b n))
                 (rationalp a) ; (>= a 0)
                 (rationalp b) ; (>= b 0)
;                 (integerp n) ;(> n 0)
                 )
            (= (- a b) (* n (- (fl (/ a n)) (fl (/ b n))))))
   :rule-classes ()
   :hints (("goal" :use ((:instance mod=mod-1))))))

(local
 (defthm hack-m10
   (implies (and (rationalp a) (rationalp b) (rationalp c) (> b 0) (= a (* b c)))
            (= (/ a b) c))
   :rule-classes ()))

(local
 (defthm mod=mod-3
   (implies (and (= (mod a n) (mod b n))
                 (rationalp a) ; (>= a 0)
                 (rationalp b); (>= b 0)
;                 (integerp n) ;(> n 0)
                 )
            (= (/ (- a b) n) (- (fl (/ a n)) (fl (/ b n)))))
   :rule-classes ()
   :hints (("goal" :use ((:instance mod=mod-2)
                         (:instance hack-m10 (a (- a b)) (b n) (c (- (fl (/ a n)) (fl (/ b n))))))))))

; If A and B are congruent mod N, then their difference is a multiple of N; and conversely.
; First, the forward direction.

(defthm mod-equal-int
  (implies (and (= (mod a n) (mod b n))
                (rationalp a)
                (rationalp b))
           (integerp (/ (- a b) n)))
  :rule-classes ()
  :hints (("goal" :use ((:instance mod=mod-3)))))

(local
 (defthm mod-equal-int-reverse-1
   (implies (and (rationalp a)
                 (rationalp b))
            (equal (integerp (/ (- a b) n))
                   (integerp (/ (- (mod a n) (mod b n))
                                n))))
   :rule-classes ()
   :hints (("goal" :use ((:instance mod-def
                                    (x a) (y n))
                         (:instance mod-def
                                    (x b) (y n)))))))

(local
 (defthm mod-equal-int-reverse-2
   (implies (and (rationalp a)
                 (rationalp b))
            (equal (integerp (/ (- a b) n))
                   (integerp (/ (- (mod a n) (mod b n))
                                n))))
   :rule-classes ()
   :hints (("goal" :use mod-equal-int-reverse-1))))

(local
 (defthm mod-equal-int-reverse-3-1-1-1
   (implies (and (rationalp i)
                 (<= 0 i)
                 (rationalp n)
                 (< i n))
            (implies (integerp (/ i n))
                     (= i 0)))
   :rule-classes nil))

(local
 (defthm mod-equal-int-reverse-3-1-1
   (implies (and (rationalp i)
                 (rationalp n)
                 (< (abs i) n))
            (implies (integerp (/ i n))
                     (= i 0)))
   :hints (("Goal" :use (mod-equal-int-reverse-3-1-1-1
                         (:instance mod-equal-int-reverse-3-1-1-1
                                    (i (- i))))))
   :rule-classes nil))

(local
 (defthm mod-equal-int-reverse-3-1
   (implies (and (rationalp i)
                 (<= 0 i)
                 (rationalp j)
                 (<= 0 j)
                 (rationalp n)
                 (< i n)
                 (< j n))
            (implies (integerp (/ (- i j) n))
                     (= i j)))
   :hints (("Goal" :use ((:instance mod-equal-int-reverse-3-1-1
                                    (i (- i j))))))
   :rule-classes nil))

(local
 (defthm mod-equal-int-reverse-3
   (implies (and (rationalp a)
                 (rationalp b)
                 (rationalp n)
                 (< 0 n))
            (implies (integerp (/ (- a b) n))
                     (= (mod a n) (mod b n))))
   :rule-classes ()
   :hints (("goal" :use (mod-equal-int-reverse-2
                         (:instance mod-equal-int-reverse-3-1
                                    (i (mod a n))
                                    (j (mod b n))))))))

(local
 (defthm mod-equal-int-reverse-4
   (implies (and (rationalp a)
                 (rationalp b)
                 (rationalp n)
                 (< 0 n))
            (implies (= (mod a n) (mod b n))
                     (integerp (/ (- a b) n))))
   :rule-classes ()
  :hints (("goal" :use (mod=mod-3)))))

(defthm mod-equal-int-reverse
  (implies (and (integerp (/ (- a b) n))
                (rationalp a)
                (rationalp b)
                (rationalp n)
                (< 0 n))
           (= (mod a n) (mod b n)))
  :rule-classes ()
  :hints (("goal" :use (mod-equal-int-reverse-3 mod-equal-int-reverse-4))))

(defthmd mod-mult-2
  (implies (integerp a)
           (equal (mod (* a n) n)
                  0))
  :hints (("goal" :in-theory (enable mod-when-y-is-complex-rationalp)
           :use ((:instance mod-mult-eric (x 0) (y n))
                 ))))

(defthmd mod-mult-2-alt
  (implies (integerp a)
           (equal (mod (* n a) n)
                  0))
  :hints (("Goal" :in-theory (enable mod-mult-2))))

(defthmd mod-mult-n
  (equal (mod (* a n) n)
         (* n (mod a 1)))
  :hints (("Goal" :in-theory (enable mod))))

(defthmd mod-mult-n-commuted
  (equal (mod (* n a) n)
         (* n (mod a 1)))
  :hints (("Goal" :in-theory (enable mod))))

(defthm mod-0-int
  (implies (and (integerp m)
                (integerp n)
                (not (= n 0)))
           (iff (= (mod m n) 0)
                (integerp (/ m n))))
  :hints (("Goal" :use ((:instance mod-equal-int
                                   (a m) (b 0) (n n))
                        quot-mod)))
  :rule-classes ())

;rename params on these:

;just a special case of mod-mult-2-alt
;generalize?
;; Rule A3 in fp.lisp suggests using (* 2 i) instead of
;; (+ i i).
(defthm mod-2*i
  (implies (integerp i)
           (equal (mod (* 2 i) 2)
                  0))
  :hints (("Goal" :in-theory (enable mod-mult-2-alt))))

;gen the 2?
(defthm mod-2*m+1-rewrite
  (implies (integerp m)
           (equal (mod (1+ (* 2 m)) 2)
                  1)))

;eliminate this
;in fact, it equals 1!
(defthm mod-2*i+1
  (implies (integerp i)
           (not (equal (mod (1+ (* 2 i)) 2)
                       0))))

(defun INDUCT-NAT (x)
  (if (and (integerp x)
	   (> x 0))
      (induct-nat (1- x))
    ()))


(local (defthm nk>=k-1
    (implies (and (integerp n)
		  (>= n 0)
		  (integerp k)
		  (> k 0)
		  (not (= (* n k) 0))
                  )
	     (>= (* n k) k))
  :rule-classes ()
  :hints (("goal" :induct (induct-nat n)))
   ))

(local (defthm nk>=k-2
    (implies (and (integerp n)
		  (>= n 0)
		  (integerp k)
		  (> k 0)
		  (not (= (* n k) 0)))
	     (>= (abs (* n k)) k))
  :rule-classes ()
  :hints (("goal" :use (nk>=k-1)))))





(local (defthm nk>=k-3
    (implies (and (integerp n)
		  (<= n 0)
		  (integerp k)
		  (> k 0)
		  (not (= (* n k) 0)))
	     (>= (abs (* n k)) k))
  :rule-classes ()
  :hints (("goal" :use ((:instance nk>=k-2 (n (- n))))))))

;BOZO move or drop?
(defthm nk>=k
    (implies (and (integerp n)
		  (integerp k)
		  (> k 0)
		  (not (= (* n k) 0)))
	     (>= (abs (* n k)) k))
  :rule-classes ()
  :hints (("goal" :use (nk>=k-2 nk>=k-3))))

;BOZO gen?
;not used anywhere but exported by lib/basic
(defthm mod-force-equal
  (implies (and (< (abs (- a b)) n)
                (rationalp a) ;                  (natp a)
                (rationalp b) ;(natp b)
                (integerp n) ;(rationalp n) ;(natp n)
                )
           (iff (= (mod a n) (mod b n))
                (= a b)))
  :rule-classes ()
  :hints (("goal" :use ( mod-equal-int
                         (:instance nk>=k (k n) (n (/ (- a b) n)))
;			(:instance *cancell (x a) (y b) (z n))
                         ))))


;yuck? BOZO used anywhere?
(defthm nk>=k-linear
    (implies (and (integerp n)
		  (integerp k)
		  (not (= n 0)))
	     (>= (abs (* n k)) k))
  :rule-classes :linear
  :hints (("Goal" :use nk>=k)))


;BOZO more like this.  get rid of mod-mult-alt-2?
(defthm mod-mult-2-gen
  (equal (mod (* a n) n)
         (* n (mod a 1)))
  :hints (("Goal" :in-theory (enable mod-cancel))))
(defthm mod-mult-2-alt-gen
  (equal (mod (* n a) n)
         (* n (mod a 1)))
  :hints (("Goal" :in-theory (enable mod-cancel))))

#|
(defthm mod-sum-subst
  (implies (and (syntaxp (not (quotep z)))
                (equal (mod z y) k)
                (syntaxp (quotep k))
                (rationalp z)
                (rationalp x)
                )
           (equal (mod (+ x z) y)
                  (mod (+ x k) y)))) 
                        

(defthm mod-split-around-zero
  (implies (and (< x y)
                (< (* -1 y) x)
                (case-split (rationalp x))
                (case-split (rationalp y)))
           (equal (mod x y)
                  (if (< x 0) 
                      (+ x y)
                      x)))
  :hints (("Goal" :in-theory (enable mod-does-nothing  mod-force-chosen-a-neg)))
)

(defthm hack-ky
  (implies (and; (syntaxp (and (quotep k) (quotep y)))
                (< k y)
                (<= 0 k)
                ;(<= 0 y)
                (rationalp x)
                (rationalp y)
                (rationalp k)
                )
           (equal (mod (+ k x) y)
                  (if (< (+ k (mod x y)) y)
                      (+ k (mod x y))
                    (+ (- k y) (mod x y)))))
  :hints (("Goal" :use((:instance mod-does-nothing (m (+ k (mod x y))) (n y))
                       (:instance mod-does-nothing (m (+ (- k y) (mod x y))) (n y)))
           :in-theory (disable mod-does-nothing)))
  )

(defthm mod-normalize-leading-constant
  (implies (and (syntaxp (and (quotep k)
                              (or (< (cadr k) 0)
                                  (<= (cadr y) (cadr k)))))
                (syntaxp (quotep y))
                (rationalp k)
                (rationalp x)
                (rationalp y)
                )
           (equal (mod (+ k x) y)
                  (mod (+ (mod k y) x) y))))




(defthm hack
  (equal (INTEGERP (* 1/8 x))
         (equal 0 (mod x 8)))
  :hints (("Goal" :in-theory (enable mod)))
)



|#


(defthm fl-mod
  (implies (and (rationalp x)
                (natp y)
                )
           (equal (fl (mod x y))
                  (mod (fl x) y)))
  :hints (("Goal" :in-theory (enable mod)))
  )

; a powerful rule!
(defthmd mod-sum-cases
  (implies (and; (syntaxp (and (quotep k) (quotep y)))
;                (< k y)
;                (<= 0 k)
                (<= 0 y)
                (rationalp x)
                (rationalp y)
                (rationalp k)
                )
           (equal (mod (+ k x) y)
                  (if (< (+ (mod k y) (mod x y)) y)
                      (+ (mod k y) (mod x y))
                    (+ (mod k y) (mod x y) (* -1 y)))))
  :otf-flg t
  :hints (("Goal" :use((:instance mod-does-nothing (m (+ (mod k y) (mod x y))) (n y))
                       (:instance  mod-force-eric (a 1) (x (+ (MOD K Y) (MOD X Y))))
                       )
           :in-theory (disable mod-does-nothing)))
  )



;BOZO move
(defthmd mod-fl-eric
  (implies (and (<= 0 y)
                (integerp y)
                )
           (equal (mod (fl x) y)
                  (fl (mod x y))))
  :hints (("Goal" :use (:instance fl/int-rewrite (n y))
           :in-theory (enable mod ))))

(defthm mod-squeeze
  (implies (and (= (mod m n) 0)
                (< m (* (1+ a) n))
                (< (* (1- a) n) m)
                (integerp a)
                (integerp m)
                (integerp n))
           (= m (* a n)))
  :hints (("Goal" :use (mod-0-int
                        (:theorem
                         (implies (and (< m (* (1+ a) n))
                                       (< (* (1- a) n) m)
                                       (rationalp n))
                                  (and (< (1- a) (/ m n))
                                       (< (/ m n) (1+ a))))))))
  :rule-classes ())
