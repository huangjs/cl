#|
  Book:    list-defthms-help
  Copyright: (c) 2005 Galois Connections, Inc.
  Author:    Lee Pike, Galois Connections, Inc. <leepike@galois.com>
|#

(in-package "ACL2")

(encapsulate ()

(local (include-book "data-structures/list-defthms" :dir :system))

;; From lists-defthms library
   (defthm true-listp-first-n-ac
     (implies (true-listp ac)
	      (true-listp (first-n-ac i l ac)))
     :rule-classes (:rewrite :type-prescription)
     :hints (("Goal" :induct (first-n-ac i l ac))))

;; From lists-defthms library
(defthm true-listp-take
  (true-listp (take n l)))

; from list-defthms
   (defun xfirstn (n l)
     (declare (xargs :guard (and (integerp n)
				 (<= 0 n)
				 (true-listp l))))
     (cond ((zp n) nil)
	   (t (cons (car l) (xfirstn (1- n) (cdr l))))))

; from list-defthms
   (defthm nth-xfirstn
     (implies (and (integerp i)
		   (<= 0 i)
		   (integerp n)
		   (<= 0 n))
	      (equal (nth i (xfirstn n l))
		     (if (<= n (len l))
			 (if (< i n)
			     (nth i l)
			   nil)
		       (if (< i (len l))
			   (nth i l)
			 nil)))))

; from list-defthms
(defthm first-n-ac-non-recursive
  (implies (and (true-listp ac)
                (integerp n)
                (<= 0 n))
           (equal (first-n-ac n l ac)
                  (revappend ac (xfirstn n l)))))

; from list-defthms
(defthm nth-take
  (implies (and (integerp i) 
                (<= 0 i)
                (integerp n) 
                (<= 0 n))
           (equal (nth i (take n l))
                  (if (<= n (len l))
                      (if (< i n)
                          (nth i l)
                        nil)
                    (if (< i (len l))
                        (nth i l)
                      nil))))
  :hints (("Goal" :do-not-induct t)))

)
