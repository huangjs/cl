(in-package "ACL2")

(local (include-book "../support/decode"))

;;Necessary defuns:

(defund bvecp (x k)
  (declare (xargs :guard (integerp k)))
  (and (integerp x)
       (<= 0 x)
       (< x (expt 2 k))))

(defun natp (x)
  (declare (xargs :guard t))
  (and (integerp x)
       (<= 0 x)))

;; New stuff:

(defund decode (x n)
  (declare (xargs :guard (rationalp n)))
  (if (and (natp x) (< x n)) 
      (ash 1 x) 
    0))

(defthm decode-nonnegative-integer-type
  (and (integerp (decode x n))
       (<= 0 (decode x n)))
  :rule-classes (:type-prescription))

;this rule is no better than decode-nonnegative-integer-type and might be worse:
(in-theory (disable (:type-prescription decode)))

(defthm decode-natp
  (natp (decode x n)))

(defthm decode-bvecp
  (implies (and (<= n k)
                (case-split (integerp k))
                )
           (bvecp (decode x n) k)))
