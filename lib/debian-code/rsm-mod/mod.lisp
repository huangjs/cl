;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mod.lisp
;;;; Purpose:       Modular arithmetic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: mod.lisp,v 1.6 2003/10/21 20:59:44 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.mod)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(declaim (ftype (function (integer &rest integer) integer) +))

(defun + (mod &rest args)
  "Add <args> in Mod <mod> arithmetic.
Example: (rsm.mod:+ 3 3 5)
            2"
  (reduce #'(lambda (x y) (mod (cl:+ (mod x mod) (mod y mod)) mod)) 
          args :initial-value 0))

(declaim (ftype (function (integer &rest integer) integer) *))

(defun * (mod &rest args)
  "Multiply <args> in Mod <mod> arithmetic.
Example: (rsm.mod:* 3 2 5)
            1"
  (reduce #'(lambda (x y) (mod (cl:* (mod x mod) (mod y mod)) mod)) 
          args :initial-value 1))

(defun ppow (b n p)
  "Raise <b> to the <n>th power in the field Z mod <p>. Here <p> must be prime.
Example: (rsm.mod:ppow 12 100 7)
          2"
  (^ b n p :e-phi (1- p)))


(declaim (ftype (function (integer integer integer 
                                   &key (:e-phi integer)) integer) ^))

(defun ^ (b n mod &key (e-phi 0))
  "Raise <b> to the <n>th power mod <mod> by repeated squaring. If <e-phi> 
is non zero, use the generalization of Fermat's little theorem: 
b^phi(mod) = 1 mod mod, when the gcd of b and mod is 1. The theorem is 
used to replace b^n with b^r where r = mod(n, phi(mod)) and phi is 
the Euler Phi function.
Example: (rsm.mod:^ 213317 527131763 173)
          170
Example: (rsm.mod:^ 7 2134145213317 33 :e-phi 20)
          28"
  (let ((bmod (mod b mod)))
    (when (= bmod 0)
      (return-from ^ 0))
    (when (= bmod 1)
      (return-from ^ 1))
    (when (and 
           (/= e-phi 0)
           (= (gcd mod bmod) 1))
      (setf n (mod n e-phi)))
    (loop 
        :with prd = 1 
        :with pow = bmod
        :with nn = n
        :while (> nn 0) 
        :if (oddp nn) :do
          (setf prd (mod (* mod prd pow) mod))
          (when (= prd 0)
            (return 0))
          (setf nn (/ (1- nn) 2))
          (setf pow (* mod pow pow))
        :else :do
             (setf nn (/ nn 2))
             (setf pow (* mod pow pow))
        :finally (return prd))))

(defun euler-phi (n)
  "Computes the Euler Phi function of <n>.
Example: (rsm.mod:euler-phi 15)
          8"
  (let ((factors (factors n)))
    (reduce #'cl:* 
            (mapcar #'(lambda (p) (- 1 (/ p))) factors)
            :initial-value n)))

(defun %get-powers (k n)
  "Get the list of the factor <k> that appears in <n>."
  (loop 
      :with nn = n
      :with facts = nil
      :while (= (mod nn k) 0) :do
        (setf nn (/ nn k))
        (push k facts)
      :finally (return facts)))

(defun %get-powers-of-2-3 (n)
  "Get the list of the primes 2 and 3 that occur in <n>."
  (let ((2-facts (%get-powers 2 n))
        (3-facts (%get-powers 3 n)))
    (nconc 2-facts 3-facts)))

(defun factors (n &key (no-dups t))
  "Computes and returns a list of the primes factors of <n>. If <no-dups> is
true, then no multiple entries of a factor are returned.
Example: (rsm.mod:factors 100)
         (2 5)
Example: (rsm.mod:factors 100 :no-dups nil)
         (2 2 5 5)"
  (let ((2-3-facts (%get-powers-of-2-3 n)))
    (let ((other-facts
           (loop 
               :with nn = (/ n (apply #'cl:* 2-3-facts))
               :with m = (isqrt nn)
               :with k = 5
               :with factors = nil
               :with skip fixnum = 2
               :while (<= k m) :do
                 (if (= (mod nn k) 0)
                     (progn
                       (setf nn 
                         (do ((n1 nn (/ n1 k)))
                             ((> (mod n1 k) 0) n1)
                           (push k factors)))
                       (setf m (isqrt nn)))
                   (progn
                     (incf k skip)
                     (if (= skip 2)
                         (setf skip 4)
                       (setf skip 2))))
               :finally (return (nreverse 
                                (if (> nn 1)
                                    (cons nn factors)
                                  factors))))))
      (if no-dups
          (delete-duplicates
           (nconc 2-3-facts other-facts))
        (nconc 2-3-facts other-facts)))))


(defun %get-gcd-pair (ms flip)
  (let ((u 1)
        (v (- (pop ms))))
    (loop 
      :until (null ms) :do
      (psetq u v
             v (cl:- u (cl:* (pop ms) v))))
    (if flip
        (list v u)
      (list u v))))

(defun gcd-with-pair (n m)
  "Returns two values: The gcd of <n> and <m>, and the list (r s) such that 
r * n + s * m = gcd(n,m).
Example: (rsm.mod:gcd-with-pair 15 21)
         3
         (3 -2)"
  (let* ((max (max n m))
         (min (min n m))
         (flip (when (= min n)
                t)))
    (let (ms (qs (list min max)))
      (loop 
          :with p = max
          :with q = min
          :with r = 1 
          :do
            (multiple-value-bind (m1 r1)
                (truncate p q)
              (setf p q)
              (setf q r1)
              (setf r r1)
              (if (= r 0)
                  (return)
                (progn
                  (push r qs)
                  (push m1 ms)))))
      (if (null ms)
          (values min (if flip
                          (list 1 0)
                        (list 0 1)))
        (values (pop qs) 
                (%get-gcd-pair ms flip))))))
      

(defun has-inverse-p (a n)
  "Does <a> have an inverse in Z mod <n>?
Example: (rsm.mod:has-inverse-p 10 100)
         nil"
  (= (gcd a n) 1))

(defun inverse (a n &optional (error nil) (not-invert-return 0))
  "Finds the inverse of <a> in Z mod <n>. If <a> inverse does not exist, 
an error is thrown if <error> is non nil. If <error> is nil, then 
<not-invert-return> is returned.
Example: (rsm.mod:inverse 21 100)
          81"
  (let ((gcd (gcd a n)))
    (if (= gcd 1)
        (multiple-value-bind 
            (r pairs)
            (gcd-with-pair a n)
          (declare (ignore r))
          (mod (car pairs) n))
      (if error
          (error "rsm.mod:inverse: First arg, ~s, is not invertible 
in Z mod ~s." a n)
        not-invert-return))))


(defun solve-congruence-system (as ms)
  "Use the Chinese remainder theorem to solve for x, the system of 
congruences: x = as_i mod ms_i. The moduli, <ms>, must all be pairwise 
relatively prime. x will be unique in Z mod (product of <ms>'s).
Example: (rsm.mod:solve-congruence-system '(1 2 3) '(2 3 5))
          23"
  (unless (= (length as) (length ms))
    (error "rsm.mod:solve-congruence-system: Congruence values, ~s, are not
 the same length as the moduli, ~s~%" as ms))
  (loop for (mod . mod-rest) on ms do
        (loop for modi in mod-rest do
              (unless (= (gcd mod modi) 1)
                (error "rsm.mod:solve-congruence-system: Modulus ~s and ~s are 
not relatively prime.~%" mod modi))))
  (let ((M (reduce #'cl:* ms))
        (x 0))
    (loop for m in ms
        as a in as do
          (let* ((Mi (/ M m))
                 (Ni (inverse Mi m)))
            (setf x (+ M x (* M (mod a m) Mi Ni)))))
    x))


(defun rational-approx (number &optional (epsilon nil))
  "Find a simple rational approximation to <number> within <epsilon>.
Example: (rsm.mod:rational-approx pi 0.0000003)
         355/113"
  (let ((last-approx (rational number)))
    (flet ((rat-approx (rat)
             (let ((num (numerator rat))
                   (den (denominator rat)))
               (if (or (= num 1)
                       (= den 1))
                   rat
                 (multiple-value-bind (gcd pair)
                     (gcd-with-pair num den)
                   (declare (ignore gcd))
                   (/ (cadr pair) (- (car pair))))))))
      (let ((approx
             (if (rationalp number)
                 (rat-approx number)
               (rat-approx (rational number)))))
        (if epsilon
            (progn
              (loop :until (or (= approx last-approx)
                               (>= (abs (- approx number)) epsilon)) :do
                (setf last-approx approx)
                (setf approx (rat-approx approx)))
              last-approx)
          (progn
            (setf approx (rat-approx approx))
            (loop :until (= approx last-approx) :do
              (setf last-approx approx)
              (setf approx (rat-approx approx)))
            approx))))))

