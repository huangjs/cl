(in-package "ACL2")

(set-enforce-redundancy t)

(include-book "rtl")
(include-book "arith")
(include-book "bits")
(local (include-book "../support/simplify-model-helpers"))

(defthm equal-log=-0
  (equal (equal (log= k x)
                0)
         (not (equal k x))))

(defthm equal-log=-1 ; possibly not needed
  (equal (equal (log= k x)
                1)
         (equal k x)))

(defthm equal-lnot-0
  (implies (bvecp x 1)
           (equal (equal (lnot x 1) 0)
                  (equal x 1))))

(defthm equal-lnot-1 ; possibly not needed
  (implies (bvecp x 1)
           (equal (equal (lnot x 1) 1)
                  (equal x 0))))

(defthm bits-if
  (equal (bits (if x y z) i j)
         (if x (bits y i j) (bits z i j))))

(defthm bitn-if
  (equal (bitn (if x y z) i)
         (if x (bitn y i) (bitn z i))))

(defthm bits-if1
  (equal (bits (if1 x y z) i j)
         (if1 x (bits y i j) (bits z i j))))

(defthm bitn-if1
  (equal (bitn (if1 x y z) i)
         (if1 x (bitn y i) (bitn z i))))

(defthm log=-0-rewrite
  (implies (bvecp k 1)
           (equal (log= 0 k)
                  (lnot k 1))))

(defthm log=-1-rewrite
  (implies (bvecp k 1)
           (equal (log= 1 k)
                  k)))

(defthm log<>-is-lnot-log=
  (equal (log<> x y) (lnot (log= x y) 1)))

(defthm cat-combine-constants
  (implies (and (syntaxp (and (quotep x)
                              (quotep m)
                              (quotep y)
                              (quotep n)))
                (equal (+ n p) r)
                (case-split (<= 0 m))
                (case-split (<= 0 n))
                (case-split (<= 0 p))
                (case-split (integerp m))
                (case-split (integerp n))
                (case-split (integerp p)))
           (equal (cat x m (cat y n z p) r)
                  (cat (cat x m y n) (+ m n) z p))))

(defthm bvecp-if
  (equal (bvecp (if test x y) k)
         (if test (bvecp x k) (bvecp y k))))

; bvecp-if1 is analogous to the above, and is already included in rtl.lisp

; Setbits can introduce a call of cat, which can introduce (bits (sig n) 2 0)
; say, even though sig is a single bit.  So we add the following.

(in-theory (enable bvecp-monotone))
