;; (defpackage :list-comprehension
;;   (:use "COMMON-LISP")
;;   (:export
;;    "OPEN-BRACKET"
;;    "CLOSING-BRACKET")
;;   (:documentation
;;    "Add two read macros supporting list comprehension"))

;; (in-package :list-comprehension)

(defun open-bracket (stream ch)
  (declare (ignorable stream ch))
  (defmacro comp ((e &rest qs) l2) 
	(if (null qs) `(cons ,e ,l2)		; rule A 
		(let ((q1 (car qs)) 
			  (q (cdr qs))) 
		  (if (not(eq (cadr q1) '<-))		   ; a generator? 
			  `(if ,q1 (comp (,e ,@q),l2) ,l2) ; rule B 
			  (let ((v (car q1))			   ; rule C 
					(l1 (third q1)) 
					(h (gentemp "H-")) 
					(us (gentemp "US-")) 
					(us1 (gentemp "US1-"))) 
				`(labels ((,h (,us)		; corresponds to a letrec 
							(if (null ,us) ,l2 
								(let ((,v (car ,us)) 
									  (,us1 (cdr ,us))) 
								  (comp (,e ,@q) (,h ,us1)))))) 
				   (,h ,l1))))))) 
  (do ((l nil) 
	   (c (read stream t nil t)(read stream t nil t))) 
	  ((eq c '|]|) `(comp ,(reverse l) ())) 
	(push c l))) 


(defun closing-bracket (stream ch)
  (declare (ignorable stream ch))
  `|]|)

;; (in-package :common-lisp)

(defparameter *list-comprehension-already-started-p* nil)
(defparameter *orig-left-bracket-macro* nil)
(defparameter *orig-right-bracket-macro* nil)

(defun start-list-comprehension ()
  (if (not *list-comprehension-already-started-p*)
	  (progn
		(setf *orig-left-bracket-macro* (get-macro-character #\[))
		(setf *orig-right-bracket-macro* (get-macro-character #\]))
		(set-macro-character #\[ #'open-bracket)
		(set-macro-character #\] #'closing-bracket)
		(setf *list-comprehension-already-started-p* t))
	  (warn "~&List comprehension is already started.")))

(defun stop-list-comprehension ()
  (if *list-comprehension-already-started-p*
	  (progn
		(set-macro-character #\[ *orig-left-bracket-macro*)
		(set-macro-character #\] *orig-right-bracket-macro*)
		(setf *list-comprehension-already-started-p* nil))
	  (format t "~&Already stopped. So don't tap.")))

(defun force-stop-list-comprehension ()
  (set-macro-character #\[ *orig-left-bracket-macro*)
  (set-macro-character #\] *orig-right-bracket-macro*)
  (setf *list-comprehension-already-started-p* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (start-list-comprehension))

;;; examples

;;; outer-product
;; (defun outer-product (xs ys)
;;   "also called as tensor product in mathematics.
;; e.g. (outer-product '(1 2) '(4 5 6))
;; 		=>
;; 		((4 5 6) (8 10 12))"
;;   (flet ((scalar-product (a x)
;; 		   (mapcar #'(lambda (e) (* a e))
;; 				   x)))
;; 	[ (scalar-product x ys) (x <- xs) ]))

