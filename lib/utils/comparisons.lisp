(defpackage hjs.util.comparisons
  (:documentation "This package contains a collection of generic
  functions that add generic comparisons comparable to the EQUAL and
  EQUALP functions defined in the spec. This works for NUMBERs,
  CHARACTERs, STRINGs, and LISTs.

  No functions are defined for mixed arguments. Lists can have mixed
  types as long as the corresponding positions are of the same type.")
  (:use #:cl)
  (:export #:less #:greater #:not-less #:not-greater
           #:lessp #:greaterp #:not-lessp #:not-greaterp))

(in-package :hjs.util.comparisons)

;;; case-sensitive
(defgeneric less (x y))
(defgeneric greater (x y))
(defgeneric not-less (x y))
(defgeneric not-greater (x y))

;;; case-insensitive
(defgeneric lessp (x y))
(defgeneric greaterp (x y))
(defgeneric not-lessp (x y))
(defgeneric not-greaterp (x y))

;;; Numbers

(defmethod less ((x number) (y number))
  (< x y))

(defmethod greater ((x number) (y number))
  (> x y))

(defmethod not-less ((x number) (y number))
  (>= x y))

(defmethod not-greater ((x number) (y number))
  (<= x y))

(defmethod lessp ((x number) (y number))
  (< x y))

(defmethod greaterp ((x number) (y number))
  (> x y))

(defmethod not-lessp ((x number) (y number))
  (>= x y))

(defmethod not-greaterp ((x number) (y number))
  (<= x y))

;;; Characters

(defmethod less ((x character) (y character))
  (char< x y))

(defmethod greater ((x character) (y character))
  (char> x y))

(defmethod not-less ((x character) (y character))
  (char>= x y))

(defmethod not-greater ((x character) (y character))
  (char<= x y))

(defmethod lessp ((x character) (y character))
  (char-lessp x y))

(defmethod greaterp ((x character) (y character))
  (char-greaterp x y))

(defmethod not-lessp ((x character) (y character))
  (char-not-lessp x y))

(defmethod not-greaterp ((x character) (y character))
  (char-not-greaterp x y))

;;; Strings

(defmethod less ((x string) (y string))
  (string< x y))

(defmethod greater ((x string) (y string))
  (string> x y))

(defmethod not-less ((x string) (y string))
  (string>= x y))

(defmethod not-greater ((x string) (y string))
  (string<= x y))

(defmethod lessp ((x string) (y string))
  (string-lessp x y))

(defmethod greaterp ((x string) (y string))
  (string-greaterp x y))

(defmethod not-lessp ((x string) (y string))
  (string-not-lessp x y))

(defmethod not-greaterp ((x string) (y string))
  (string-not-greaterp x y))

;;; Lists

(defmethod less ((x list) (y list))
  (cond ((endp x) (not (endp y)))
        ((endp y) nil)
        ((less (car x) (car y)) t)
        ((equal (car x) (car y)) (less (cdr x) (cdr y)))
        (t nil)))

(defmethod greater ((x list) (y list))
  (cond ((endp x) nil)
        ((endp y) t)
        ((greater (car x) (car y)) t)
        ((equal (car x) (car y)) (greater (cdr x) (cdr y)))
        (t nil)))

(defmethod not-less ((x list) (y list))
  (cond ((endp x) (endp y))
        ((endp y) t)
        ((not-less (car x) (car y)) (not-less (cdr x) (cdr y)))
        (t nil)))

(defmethod not-greater ((x list) (y list))
  (cond ((endp x) t)
        ((endp y) nil)
        ((not-greater (car x) (car y)) (not-greater (cdr x) (cdr y)))
        (t nil)))

(defmethod lessp ((x list) (y list))
  (cond ((endp x) (not (endp y)))
        ((endp y) nil)
        ((lessp (car x) (car y)) t)
        ((equalp (car x) (car y)) (lessp (cdr x) (cdr y)))
        (t nil)))

(defmethod greaterp ((x list) (y list))
  (cond ((endp x) nil)
        ((endp y) t)
        ((greaterp (car x) (car y)) t)
        ((equalp (car x) (car y)) (greaterp (cdr x) (cdr y)))
        (t nil)))

(defmethod not-lessp ((x list) (y list))
  (cond ((endp x) (endp y))
        ((endp y) t)
        ((not-lessp (car x) (car y)) (not-lessp (cdr x) (cdr y)))
        (t nil)))
  
(defmethod not-greaterp ((x list) (y list))
  (cond ((endp x) t)
        ((endp y) nil)
        ((not-greaterp (car x) (car y)) (not-greaterp (cdr x) (cdr y)))
        (t nil)))
