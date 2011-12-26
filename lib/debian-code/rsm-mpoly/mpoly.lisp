;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mpoly.lisp
;;;; Purpose:       Arithmetic for Multivariate Polynomials.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: mpoly.lisp,v 1.3 2003/10/20 11:55:43 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.mpoly)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(in-package rsm.mpoly)

;;;;
;;;; Representation Layer.
;;;;

;;;; Internal functions that manipulate terms.
;; "TERM"             ; Construct a term; e.g., (2 #(2 0 4)) -> 2 x1^2 x3^4
;; "TERM-COEFF"       ; Get the coefficient of a term; e.g. 2 x1^2 x3^4 -> 2
;; "TERM-POWERS"      ; Get the powers of a term; e.g., 2 x1^2 x3^4 -> #(2 0 4)
;; "TERM-MUL"         ; Multiply two terms.
;; "TERM-SCALAR-MUL"  ; Multiply a term by a scalar.

(defvar *order-type* 'lex-order 
  "Lexical order type the default order is lexical.")
(defvar *order-types* '(lex-order deglex-order) 
  "List of orderings; lexical and degree-lexical.")
(defvar *check-consistency* t 
  "If true do a consistency check when doing polynomial operations.")
(defvar *modulus* nil 
  "The arithmetic modulus used in calculations, nil means none.")
(defvar *power-modulus* nil 
  "If true and modulus is non-null, replace variable powers x^n with x.")

(defconstant +inconsistent-type+ -1 
  "Constant that indicates inconsistent terms when doing a 
polynomial computation.")

(defconstant +not-a-poly+ -2 
  "Constant that indicates an arg to a function expecting a 
polynomial was in fact not.")

(defconstant +plus+ 'cl:+)

(defconstant +minus+ '-)

(defstruct (poly
            ;; A BOA-constructor.
            (:constructor make-a-poly 
                          (&key
                           (raw 'raw)
                           (terms (if (null raw)
                                      (error "make-a-poly: 
Null arg to constructor.")
                                    (normal-form (copy-list raw))))))
            (:print-function print-poly))
  "Structure that represents a polynomial.
The terms field contains a list of terms; e.g., 
2 X1^2 X3^5 which is represented as (2 #(2 0 5))."
  lt
  (order *order-type*)
  terms)

(defmacro make-poly (&key raw)
  "Make a polynomial from raw ingredients. <raw> is a list whose elements
have the form: (coeff . vector-of-powers).
Example: (rsm.mpoly::make-poly ((2 . #(1 3)) (10 . #(2 1)) (4 . #(1 1)))
         #< 10X1^2*X2 + 2X1*X2^3 + 4X1*X2 >
"
  `(let ((poly (make-a-poly :raw ,raw)))
     (setf (poly-lt poly) (car (poly-terms poly)))
     poly))


;;; mul :: Number -> Number -> Number
(defmacro mul (a b)
  "Multiply a and b moding by *modulus* if non-null."
  `(if *modulus*
       (mod (cl:* ,a ,b) *modulus*)
     (cl:* ,a ,b)))

;;; add :: Number -> Number -> Number
(defmacro add (a b)
  "Add a and b moding by *modulus* if non-null."
  `(if *modulus*
       (mod (cl:+ ,a ,b) *modulus*)
     (cl:+ ,a ,b)))

;;; power-add :: Number -> Number -> Number
(defmacro power-add (a b)
  "If both *modulus* and *power-modulus* (true when *modulus* is prime)
add and mod by (*modulus* - 1); otherwise, just add."
  (let ((val (gensym)))
    `(if (and *modulus* *power-modulus*)
         (if (> *modulus* 2)
             (let ((,val (mod (cl:+ ,a ,b) (1- *modulus*))))
               (if (= ,val 0)
                   (if (= 0 (cl:+ ,a ,b))
                       0
                     (1- *modulus*))
                 ,val))
           1)
       (cl:+ ,a ,b))))

;;; poly-pow-mod :: Vector -> Vector
(defun poly-pow-mod (powers)
  "Adjust the powers of a term using the *modulus* if the 
*power-modulus* is true."
  (let (result)
    (loop 
      :for pow :across powers :do
      (push (power-add pow 0) result))
    (coerce (nreverse result) 'vector)))

(defmacro poly-mod (a)
  "Return the input unchanged if *modulus* is null; 
otherwise, mod the input by *modulus*."
  `(if *modulus*
       (mod ,a *modulus*)
     ,a))

(defun powers-are-all-zero? (powers)
  "Return true of the powers of a term represented by 
a vector of non-negative numbers are all 0."
  (loop 
    :for power :across powers :do
    (if (not (= power 0))
        (return-from powers-are-all-zero? nil)))
  t)


(defun coeff-sign (num)
  "Get the sign of a number as a symbol. Either '- or 'cl:+."
  (if (>= num 0) 
      +plus+
    +minus+))


;;; print-poly :: Object -> Stream -> Integer -> ()
(defun print-poly (obj stream depth)
  "Print function for the structure poly."
  (declare (ignore depth))
  (print-unreadable-object (obj stream)
    (with-standard-io-syntax
      (let* ((terms (normal-form (copy-list (poly-terms obj)))))
        (format stream " ")
        (loop 
          :for term :in terms 
          :for j :from 1 :do
          (let ((coeff (poly-mod (term-coeff term)))
                (powers (poly-pow-mod (term-powers term))))
            (cond ((powers-are-all-zero? powers)
                   (if (complexp coeff)
                       (format stream "~s" coeff)
                     (let ((coeff-sign (coeff-sign coeff)))
                       (cond ((eql coeff-sign '-)
                              (format stream "- ~s" (abs coeff)))
                             ((not (= j 1))
                              (format stream "~s ~s" 
                                      coeff-sign (abs coeff)))
                             (t
                              (format stream "~s " coeff))))))
                  ((complexp coeff)
                   (if (= j 1)
                       (format stream "~s" coeff)
                     (format stream "+ ~s" coeff)))
                  ((not (or (= coeff 1) (= coeff 0) (= coeff -1)))
                   (let ((coeff-sign (coeff-sign coeff)))
                     (cond ((and (= j 1) (eql coeff-sign '-))
                            (format stream "- ~s" (abs coeff)))
                           ((not (= j 1))
                            (format stream "~s ~s" coeff-sign (abs coeff)))
                           (t
                            (format stream "~s" coeff)))))
                  ((and (not (= j 1)) (= coeff 1))
                   (format stream "+ "))
                  ((= coeff -1)
                   (format stream "- ")))
            (when (not (= coeff 0))
              (loop 
                :with last = nil
                             :for power :across powers 
                             :for i :from 1 :do
                             (cond ((= power 1)
                                    (when last
                                      (format stream "*"))
                                    (format stream "X~s" i)
                                    (setf last t))
                                   ((not (= power 0))
                                    (when last
                                      (format stream "*"))
                                    (format stream "X~s^~s" i power)
                                    (setf last t))))
              (format stream " "))))))))

;;; term :: Number -> [non-negative Integer] -> Term
(defun make-term (coeff lst)
  "Constructor for term - represented by a coefficient cons'd with a vector
Example: (2 . #(1 5)) represents 2 * X1 * X2^5."
  (cons coeff (coerce lst 'vector)))

;;; term :: Number -> (Non-negative Integer) -> Term
(defmacro term (c (&rest powers))
  "Macro to construct a term.
Example: (rsm.mpoly:term (2 (1 3)))
creates the term 2xy^3."
  `(make-term ',c ',powers))

;;; term-powers :: Term -> [non-negative Integer]
(defun term-powers (term)
  "Retrieve the vector that represents the powers of a term.
Example: (rsm.mpoly::term-powers (2 . #(1 5)))
   #(1 5)"
  (cdr term))

;;; term-coeff :: Term -> Number
(defun term-coeff (term)
  "Retrieves the coefficient of a term.
Example: (rsm.mpoly::term-coeff (2 . #(1 5)))
   2"
  (car term))

(defmacro term-length (term)
  "Get the length of a term."
  `(length (term-powers ,term)))

(defun div-term-by-term (term1 term2)
  "Divide term1 by term2."
  (let ((power1 (term-powers term1))
        (power2 (term-powers term2))
        div-power)
    (loop 
      :for pow1 across power1
                       :for pow2 across power2 :do
                                        (if (>= pow1 pow2)
                                            (push (- pow1 pow2) div-power)
                                          (return-from div-term-by-term nil)))
    (cons (/ (term-coeff term1) (term-coeff term2)) 
          (coerce (nreverse div-power) 'vector))))


;;; similar-terms :: Term -> Term -> Bool
(defun similar-terms (term1 term2)
  "Checks to see if two terms have the same powers.
Example: (rsm.mpoly::similar-terms (2 . #(3 2 1)) (7 #(3 2 1)))
          T"
  (loop 
    :for pow1 :across (term-powers term1)
    :for pow2 :across (term-powers term2) :do
    (when (< pow1 pow2)
      (return-from similar-terms nil))
    (when (< pow2 pow1)
      (return-from similar-terms nil)))
  t)

;;; lex-order :: Term -> Term -> Bool
(defun lex-order (term1 term2)
  "A \"lexical\" ordering function for terms. Determines size by 
finding the first power of a term that is larger than the 
corresponding power of another term. Orders from larger to smaller."
  (loop 
    :for pow1 :across (term-powers term1)
    :for pow2 :across (term-powers term2) :do
    (when (< pow2 pow1)
      (return-from lex-order t))
    (when (< pow1 pow2)
      (return-from lex-order nil)))
  t)


(defvar *order-func* #'lex-order "Lexical order function.")

;;; term-mag :: Term -> non-negative Number
(defmacro term-mag (term)
  "Finds the sum of the powers of a term."
  `(loop 
     :for elem :across (term-powers ,term )
     :sum elem))


;;; deglex-order :: Term -> Term -> Bool
(defun deglex-order (term1 term2)
  "An ordering function for terms. Orders by magnitude of 
the powers of a term; breaks ties by using lexical ordering.
Orders from larger to smaller."
  (let ((len1 (term-mag term1))
        (len2 (term-mag term2)))
    (cond ((< len2 len1)
           t)
          ((< len1 len2)
           nil)
          (t
           (loop 
             :for pow1 :across (term-powers term1)
             :for pow2 :across (term-powers term2) :do
             (when (< pow2 pow1)
               (return-from deglex-order t))
             (when (< pow1 pow2)
               (return-from deglex-order nil)))
           t))))

;;; term-mul :: Term -> Term -> Term
(defun term-mul (term1 term2)
  "Multiplies two terms, returns the new term."
  (let ((powers (make-array (length (term-powers term1))))
        (c1 (term-coeff term1))
        (c2 (term-coeff term2)))
    (loop 
      :for pow1 :across (term-powers term1)
      :for pow2 :across (term-powers term2)
      :for i :from 0 :do
      (setf (aref powers i) (power-add pow1 pow2)))
    (make-term (mul c1 c2) powers)))

;;; add-similar-terms :: Term -> Term -> Term
(defun add-similar-terms (term1 term2)
  "Adds two similar terms that is, terms that have the same power
signature. Note: assumes the two terms are similar."
  (let ((c1 (term-coeff term1))
        (c2 (term-coeff term2)))
    (make-term (add c1 c2) (term-powers term1))))

;;; term-not-zero? :: Term -> Bool
(defun term-not-zero? (term)
  "Checks if a term is a zero term."
  (when (and term (not (= 0 (term-coeff term))))
    t))

;;; is-zero-poly? Poly -> Bool
(defun is-zero-poly? (poly)
  "Check if a polynomial is the zero polynomial."
  (and (poly-p poly)
       (let ((terms (poly-terms poly)))
         (or
          (null terms)
          (and (= (length terms) 1)
               (= (term-mag (car terms)) 0)
               (= (term-coeff (car terms)) 0))))))

;;; term-scalar-mul :: Number -> Term -> Term
(defun term-scalar-mul (c term)
  "Multiply a term by a scalar."
  (make-term (mul c (term-coeff term)) (term-powers term)))

(defun nterm-scalar-mul (c term)
  "Multiply a term by a scalar - destructively."
  (setf (car term) (mul (car term) c))
  term)


;;; normal-form :: [Terms] -> Poly
(defun normal-form (terms)
  "Puts <terms> into a normal form, formed 
by ordering and then simplifying."
  (let ((new-poly
         (sort terms *order-func*)))
    (simplify new-poly)))

;;; term-type :: Term -> Vector of zeros
(defun term-type (term)
  "Get the term power signature (how many variables used).
Example: (rsm.mpoly::term-type '(2 #(1 2 0 2)))
        #(0 0 0 0)"
  (let ((len (length (cdr term))))
    (make-array len :initial-element 0)))

;;; poly-type :: Poly -> Vector of zeros
(defun poly-type (poly)
  "Get the signature (how many variables used) of the polynomial.
Do this by getting the signature of the first term."
  (term-type (car (poly-terms poly))))


;;; simplify :: (Terms) -> (Terms)
;;; Note: The output is always a non null list.
(defun simplify (terms)
  "Simplify a list of terms."
  (let ((result nil)
        (last (car terms)))
    (loop 
      :for term :in (cdr terms) :do
      (if (similar-terms last term)
          (setf last (add-similar-terms last term))
        (progn 
          (when (term-not-zero? last)
            (push last result))
          (setf last term))))
    (when (term-not-zero? last)
      (push last result))
    (if (null result)
        (let ((type (term-type (car terms))))
          (list (make-term 0 type)))
      (nreverse result))))

;;; poly-scalar-mul :: Number -> Poly -> Poly
(defun poly-scalar-mul (c poly)
  "Multiply a polynomial by a scalar."
  (unless (numberp c)
    (error "poly-scalar-mul: The first arg is not a number."))
  (when (null poly)
    (error "poly-scalar-mul: The second arg is null."))
  (let ((result-terms nil))
    (loop 
      :for term :in (poly-terms poly) :do
      (push (term-scalar-mul c term) result-terms))
    (make-poly :raw result-terms)))

(defun npoly-scalar-mul (c poly)
  "Multiply a polynomial by a scalar - destructively."
  (unless (numberp c)
    (error "poly-scalar-mul: The first arg is not a number."))
  (when (null poly)
    (error "poly-scalar-mul: The second arg is null."))
  (loop 
    :for term :in (poly-terms poly) :do
    (nterm-scalar-mul c term))
  poly)

;;; poly-scalar-add :: Number -> Poly -> Poly
(defun poly-scalar-add (c poly)
  "Add a scalar to a polynomial."
  (unless (numberp c)
    (error "poly-scalar-add: The first arg is not a number."))
  (when (null poly)
    (error "poly-scalar-add: The second arg is null."))
  (let (result-terms)
    (push (make-term c (poly-type poly)) result-terms)
    (loop 
      :for term :in (poly-terms poly) :do
      (push term result-terms))
    (make-poly :raw result-terms)))

;;; term-powers-ok? :: (Terms) -> Bool
(defun term-powers-ok? (terms)
  "Determines if every term have non-negative integer powers."
  (every 
   #'(lambda (term)
       (let ((powers (term-powers term)))
         (every 
          #'(lambda (power)
              (and 
               (integerp power)
               (>= power 0))) powers)))
   terms))

;;; consistent-terms? (Terms) -> [non-negative Integers] | +inconsistent-type+
(defun consistent-terms? (args)
  "Check that the list of terms are consistent in the sense that 
each term has the same signature (uses the same number of variables).
Return the signature of the terms or +inconsistent-type+."
  (cond ((consp args)
         (let* ((first (car args))
                (len (length (cdr first))))
           (loop 
             :for arg :in (cdr args) :do
             (when (not (= (length (cdr arg)) len))
               (return-from consistent-terms? +inconsistent-type+)))
           (make-array len :initial-element 0)))
        (t
         +inconsistent-type+)))

(defun prime? (n)
  (let ((root-n (isqrt n)))
    (loop 
      :for i :from 2 :upto root-n :do
      (when (= (mod n i) 0)
        (return-from prime? nil)))
    t))

;;;;
;;;; Abstraction Layer
;;;;


;;; consistency-check :: (Poly) -> [non-negative Integers] | +inconsistent-type+
(defun consistency-check (polys)
  "Check that the list of polynomials are consistent in the sense that 
each polynomial uses the same number of variables.
Return the signature of the polynomials or +inconsistent-type+."
  (if (every #'poly-p polys)
      (let ((terms (mapcan #'(lambda (poly)
                               (copy-list (poly-terms poly)))
                           polys)))
        (consistent-terms? terms))
    +not-a-poly+))

;;; mul-poly-term :: Poly -> Term -> Poly
(defun mul-poly-term (poly term)
  "Multiply polynomial <poly> by term, <term>."
  (let ((p-terms (poly-terms poly))
        new-terms)
    (dolist (p-term p-terms)
      (push (term-mul p-term term) new-terms))
    (make-poly :raw new-terms)))

(defun make-poly-from-term (term)
  (make-poly :raw (list term)))


;;; pure-poly-* :: Poly -> (Poly) -> Poly
(defun pure-poly-* (poly1 &rest polys)
  "Multiplies one or more polynomials together.
Returns the resulting polynomial."
  (when (not (poly-p poly1))
    (error "*: First arg is not a valid polynomial."))
  (let ((type (poly-type poly1)))
    (if (not polys)
        (make-poly :raw (poly-terms poly1))
      (progn
        (when *check-consistency*
          (setf type (consistency-check (cons poly1 polys)))
          (cond ((eql type +inconsistent-type+)
                 (error "*: Trying to 
multiply polynomials of different types."))
                ((eql type +not-a-poly+)
                 (error "*: Some multiplicands are not polynomials."))))
        (when (is-zero-poly? poly1)
          (return-from pure-poly-* (make-poly :raw (list (make-term 0 type)))))
        (loop 
          :for poly :in polys :do
          (when (is-zero-poly? poly)
            (return-from pure-poly-* 
              (make-poly :raw 
                         (list (make-term 0 type))))))
        (let ((result-terms (poly-terms poly1)))
          (loop 
            :with tmp-terms = nil
                              :for poly :in polys :do
                              (setf tmp-terms nil)
                              (dolist (term1 result-terms)
                                (dolist (term2 (poly-terms poly))
                                  (push (term-mul term1 term2) tmp-terms)))
                              (setf result-terms tmp-terms))
          (make-poly :raw result-terms))))))

;;; * :: [Poly | Number] -> ([Poly | Number]) -> [Poly | Number]
(defun * (p1 &rest ps)
  "Multiplies one or more polynomials (or numbers). The first argument must 
be a polynomial."
  (let* ((polys (rsm.filter:filter ps #'numberp))
         (nums (set-difference ps polys)))
    (if (numberp p1)
        (setf nums (cons p1 nums))
      (setf polys (cons p1 polys)))
    (cond ((and 
            (null nums)
            (null polys))
           (error "+: No valid polys or numbers given."))
          ((null nums)
           (apply #'pure-poly-* polys))
          ((null polys)
           (apply #'cl:* nums))
          (t            
           (poly-scalar-mul (apply #'cl:* nums)
                            (apply #'pure-poly-* polys))))))


;;; pure-poly-+ Poly -> (Poly) -> Poly
(defun pure-poly-+ (poly1 &rest polys)
  "Adds one or more polynomials together.
Returns the resulting polynomial."
  (when (not (poly-p poly1))
    (error "*: First arg is not a valid polynomial."))
  (when (null polys)
    (return-from pure-poly-+ (make-poly :raw (copy-list (poly-terms poly1)))))
  (when *check-consistency*
    (let ((type (consistency-check (cons poly1 polys))))
      (cond ((eql type +not-a-poly+)
             (error "+: One of the addens is not a polynomial."))
            ((eql type +inconsistent-type+)
             (error "+: Trying to add polynomials of different types.")))))
  (let ((result-terms (poly-terms poly1)))
    (loop 
      :for poly :in polys :do
      (dolist (term (poly-terms poly))
        (push term result-terms)))
    (make-poly :raw result-terms)))

;;; + :: [Poly | Number] -> ([Poly | Number]) -> [Poly | Number]
(defun + (p1 &rest ps)
  "Add one or more polynomials (or numbers). The first argument must be 
a polynomial."
  (let* ((polys (rsm.filter:filter ps #'numberp))
         (nums (set-difference ps polys)))
    (if (numberp p1)
        (setf nums (cons p1 nums))
      (setf polys (cons p1 polys)))
    (cond ((and 
            (null nums)
            (null polys))
           (error "+: No valid polys or numbers given."))
          ((null nums)
           (apply #'pure-poly-+ polys))
          ((null polys)
           (apply #'cl:+ nums))
          (t            
           (poly-scalar-add (apply #'cl:+ nums)
                            (apply #'pure-poly-+ polys))))))


;;; ^ :: Poly -> non-negative Integer -> Poly
(defun ^ (poly n)
  "Returns <poly> raised to the <n>th power."
  (when (not (poly-p poly))
    (error "^: First arg is not a valid polynomial."))
  (when (not (and 
              (integerp n)
              (>= n 0)))
    (error "^: Second arg is not a valid non-negative integer."))
  (let ((*check-consistency* nil))
    (loop 
      :with prd = (make-poly :raw (list (make-term 1 (poly-type poly))))
                  :with pow = poly
                              :with nn = n
                                         :while (> nn 0) 
                                         :if (oddp nn) :do
                                         (setf prd (* prd pow))
                                         (setf nn (/ (1- nn) 2))
                                         (setf pow (* pow pow))
                                         :else :do
                                         (setf nn (/ nn 2))
                                         (setf pow (* pow pow))
                                         :finally (return prd))))

(defun valid-poly-arg? (term)
  "Checks that a given term (as given to the macro poly) is valid."
  (and 
   (listp term)
   (numberp (car term))
   (listp (cadr term))
   (every #'(lambda (n)
              (and
               (integerp n)
               (>= n 0)))
          (cadr term))))


;;; poly :: (Number (non-negative Integers)) -> Poly
(defmacro poly (&whole args &rest r)
  "Constructor macro for polynomials.
Example: (rsm.mpoly:poly (2 (1 3)) (10 (2 1)) (4 (1 1)))
creates the polynomial 2xy^3 + 10x^2y + 4xy."
  (declare (ignore r))
  `(let ((terms 
          (mapcar #'(lambda (term-info)
                      (when (symbolp term-info)
                        (setf term-info (symbol-value term-info)))
                      (when (not (valid-poly-arg? term-info))
                        (error "poly: \"~s\" is an invalid term.~%" 
                               term-info))
                      (make-term (car term-info) 
                                 (coerce (cadr term-info) 'vector)))
                  ',(cdr args))))
     (when (eql (consistent-terms? terms) +inconsistent-type+)
       (error "poly: Unable to construct polynomial.
Reason: Terms represented by the given args are inconsistent.~%"))
     (when (not (term-powers-ok? terms))
       (error "poly: Unable to construct polynomial.
Reason: Some term powers are not non-negative integers.~%"))
     (make-poly :raw terms)))


(defun lt (poly)
  "Return the leading term of <poly> (using the current ordering)
in the form (coeff . #(power-vector))."
  (if (eql *order-type* (poly-order poly))
      (poly-lt poly)
    (progn
      (setf (poly-order poly) *order-type*)
      (setf (poly-terms poly) (normal-form (poly-terms poly)))
      (setf (poly-lt poly) (car (poly-terms poly)))
      (poly-lt poly))))

(defun lp (poly)
  "Return the leading power of <poly> (using the current ordering)
in the form #(power-vector)."
  (cdr (lt poly)))

(defun lc (poly)
  "Return the leading coefficient of <poly> (using the current ordering)."
  (car (lt poly)))


;;; set-order :: Symbol -> ()
(defmacro set-order (order)
  "Set the ordering used for computation and printing.
Returns the previous ordering type."
  `(let ((prev-order-type *order-type*))
     (cond ((and 
             (symbolp ',order)
             (member (find-symbol (symbol-name ',order) :poly) *order-types*))
            (setf *order-func* (symbol-function (find-symbol 
                                                 (symbol-name ',order) :poly)))
            (setf *order-type* ',order)
            prev-order-type)
           (t
            (error "Bad order type. Choices are: ~a" 
                   (mapcar #'symbol-name *order-types*))))))

;;; get-order :: () -> Symbol
(defun get-order ()
  "Get the ordering type currently used."
  *order-type*)

;;; get-modulus :: () -> Non-negative Integer
(defun get-modulus ()
  "Get the current modulus."
  *modulus*)

;;; set-modulus :: Integer >=2 -> ()
(defun set-modulus (mod)
  "Set the modulus to <mod>."
  (if (not 
       (or (null mod)
           (and
            (integerp mod)
            (> mod 1))))
      (error "set-modulus: Invalid modulus value.")
    (progn
      (if (and 
           (numberp mod) 
           (prime? mod))
          (setf *power-modulus* t)
        (setf *power-modulus* nil))
      (setf *modulus* mod))))

(defun use-power-modulus (val)
  "A non nil value sets the \"power-modulus\" to true; false otherwise.
Returns the previous value of the \"power modulus\"."
  (let ((prev-pm *power-modulus*))
    (if val
        (setf *power-modulus* t)
      (setf *power-modulus* nil))
    prev-pm))


;;; report-state :: () -> ()
(defun report-state ()
  "Write state information that determines how polynomial 
calculations are computed."
  (with-standard-io-syntax
    (format t "~&order-type = ~a" (symbol-name *order-type*))
    (format t "~&modulus = ~s" *modulus*)
    (format t "~&power-modulus = ~s" *power-modulus*)
    (values)))


;;; get-state :: () -> (list)
(defun get-state ()
  "Return state information that determines how polynomial 
calculations are computed."
  (list (symbol-name *order-type*) *modulus* *power-modulus*))


(defun make-zero-poly (f)
  "Make the zero polynomial of the same type as <f>."
  (make-poly :raw (list (make-term 0 (term-type (lt f))))))


