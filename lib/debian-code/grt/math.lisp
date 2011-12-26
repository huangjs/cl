;; This file is part of GRT, a Common Lisp raytracing system
;; Copyright (C) 2002-2003 Nikodemus Siivola
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
;; USA

(in-package grt)
(export
 '(grt-vector vector-add vector-sub vector-mul vector-div
   dot-product cross-product normalize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; General math for GRT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +grt-epsilon+            (* single-float-epsilon 1000))
(defconstant most-positive-grt-float  most-positive-single-float)
(defconstant least-positive-grt-float least-positive-single-float)
(defconstant least-negative-grt-float least-negative-single-float)
(defconstant most-negative-grt-float  most-negative-single-float)

(deftype grt-float (&optional lo hi)
  "Floating point type used in GRT."
  (cond ((and lo hi) `(single-float ,lo ,hi))
	(lo `(single-float ,lo))
	(t 'single-float)))

(defun grt-float (f)
  (coerce f 'grt-float))

(defun facos (f)
  (grt-float (acos f)))

(defmacro fcos (f)
  `(coerce (cos ,f) 'grt-float))

(defmacro fsin (f)
  `(coerce (sin ,f) 'grt-float))

(defmacro =~ (a b)
  "Almost equal."
  `(<= (- +grt-epsilon+) (- ,a ,b) +grt-epsilon+))

(declaim
 (ftype (function (grt-float grt-float grt-float) (grt-float 0.0))
	min-pos-root))
(defun min-pos-root (a b c)
  "Quadratic root solver that returns the smallest
   positive real root -- or 0.0 if there are no positive
   real roots."
  (let ((D (- (expt b 2) (* 4.0 a c))))
    (declare (type grt-float D))
    (if (> D 0.0)
	(let ((r1 (max 0.0 (/ (+ (- b) (sqrt D)) (* 2.0 a))))
	      (r2 (max 0.0 (/ (- (- b) (sqrt D)) (* 2.0 a)))))
	  (cond ((and (> r1 0.0) (> r2 0.0)) (min r1 r2))
		((> r1 0.0) r1)
		(t r2)))
      0.0)))

(declaim
 (ftype (function (grt-float grt-float grt-float) (vector (grt-float 0.0)))
	pos-roots))
(defun pos-roots (a b c)
  "Quadratic root solver that returns the positive roots as a sorted vector
of grt-floats."
  (let ((D (- (expt b 2) (* 4.0 a c))))
    (declare (type grt-float D))
    (if (> D 0.0)
	(let ((r1 (max 0.0 (/ (+ (- b) (sqrt D)) (* 2.0 a))))
	      (r2 (max 0.0 (/ (- (- b) (sqrt D)) (* 2.0 a)))))
	  (cond ((and (> r1 0.0) (> r2 0.0))
		 (if (< r1 r2)
		     (vector r1 r2)
		   (vector r2 r1)))
		((> r1 0.0) (vector r1))
		(t (vector r2))))
      #())))

(declaim
 (inline linear-interpolate)
 (ftype (function (grt-float grt-float grt-float) grt-float)
	linear-interpolate))
(defun linear-interpolate (r f1 f2)
  "Linear interpolation between f1 and f2."
  (+ (* r (- f2 f1)) f1))

(defun deg-to-rad (deg)
  (grt-float (* 2 PI (/ deg 360.0))))

(defun rad-to-deg (rad)
  (grt-float (* 360.0 (/ rad (* 2 PI)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vector math for GRT
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (inline grt-vector))
(defstruct (grt-vector (:constructor grt-vector (x y z))
		       (:conc-name nil))
  "3D vector type used in GRT."
  (x 0.0 :type grt-float)
  (y 0.0 :type grt-float)
  (z 0.0 :type grt-float))

(export '(+x+ +y+ +z+ +o+))

(defvar +x+ (grt-vector 1.0 0.0 0.0))
(defvar +y+ (grt-vector 0.0 1.0 0.0))
(defvar +z+ (grt-vector 0.0 0.0 1.0))
(defvar +o+ (grt-vector 0.0 0.0 0.0))

(declaim (ftype (function ((or number sequence grt-vector))
			  (values grt-float grt-float grt-float))
		vector-values))
(defun vector-values (vector)
  "Returns as VALUES components of VECTOR. VECTOR can be a number,
a sequence, or grt-vector.

This is a convenience function, intended for translating user input,
not for internal use."
  (declare (optimize (speed 0) (debug 3) (safety 3)))
  (labels ((ret3 (x y z) (values (grt-float x) (grt-float y) (grt-float z)))
	   (ret2 (x y) (ret3 x y y))
	   (ret1 (x) (ret3 x x x)))
    (ctypecase vector
	       (number (ret1 vector))
	       (sequence (case (length vector)
			   (0 (cerror "Return (values 0.0 0.0 0.0)."
				      "Sequence used as vector is empty.")
			      (ret1 0.0))
			   (1 (ret1 (elt vector 0)))
			   (2 (ret2 (elt vector 0) (elt vector 1)))
			   (t (ret3 (elt vector 0)
				    (elt vector 1) (elt vector 2)))))
	       (grt-vector (ret3 (x vector) (y vector) (z vector))))))

(defun as-grt-vector (vector)
  "As VECTOR-VALUES, but returns a grt-vector."
  (multiple-value-bind (x y z) (vector-values vector)
    (grt-vector x y z)))

(defmacro grt-vector-bind (bindings &body body)
  "A destructuring bind for grt-vectors: with bindings each of form
   ((X Y Z) V) binds the components of grt-vector V to X, Y and Z."
  `(let ,(mapcan #'(lambda (bind)
		     (destructuring-bind ((x y z) v) bind
		       `((,x (x ,v))
			 (,y (y ,v))
			 (,z (z ,v)))))
		 bindings)
     (declare (type grt-float
		    ,@(mapcan #'(lambda (bind)
				  (destructuring-bind ((x y z) v) bind
				    (declare (ignore v))
				    `(,x ,y ,z)))
			      bindings)))
     ,@body))

(declaim
 (inline vector-add2)
 (ftype (function (grt-vector grt-vector) grt-vector) vector-add2))
(defun vector-add2 (u v)
  "Sum of 2 grt-vectors. Returns a newly allocated grt-vector.
   What you probably want is the macro VECTOR-ADD."
  (grt-vector (+ (x u) (x v))
	      (+ (y u) (y v))
	      (+ (z u) (z v))))

(declaim
 (inline vector-sub)
 (ftype (function (grt-vector grt-vector) grt-vector) vector-sub))
(defun vector-sub (u v)
  "Substract one grt-vector from another. Returns a newly allocated
   grt-vector."
  (grt-vector (- (x u) (x v))
	      (- (y u) (y v))
	      (- (z u) (z v))))

(declaim
 (ftype (function (grt-vector grt-vector grt-float) grt-vector)
	vector-adjust))
(defun vector-adjust (v d l)
  "Adjust vector V along D by L. Returns a newly allocated grt-vector."
  (grt-vector (+ (x v) (* (x d) l))
	      (+ (y v) (* (y d) l))
	      (+ (z v) (* (z d) l))))

(defmacro vector-add (&rest vectors)
  "Sum of N grt-vectors. Returns a newly allocated grt-vector."
  (n-to-2 'vector-add2 vectors))

(declaim
 (inline dot-product)
 (ftype (function (grt-vector grt-vector) grt-float) dot-product))
(defun dot-product (u v)  
  "Dot-product of two grt-vectors."
  (+ (* (x u) (x v))
     (* (y u) (y v))
     (* (z u) (z v))))

(defun cross-product (a b)
  "Cross-product of two grt-vectors."
  (grt-vector-bind (((a1 a2 a3) a)
		    ((b1 b2 b3) b))
   (grt-vector (- (* a2 b3) (* a3 b2))
	       (- (* a3 b1) (* a1 b3))
	       (- (* a1 b2) (* a2 b1)))))

(declaim
 (inline vector-mul)
 (ftype (function (grt-vector grt-float) grt-vector) vector-mul))
(defun vector-mul (v f)
  "Grt-vector multiplied by a grt-float. Returns a newly allocated
   grt-vector."
  (grt-vector (* (x v) f) (* (y v) f) (* (z v) f)))

(declaim
 (inline vector-div)
 (ftype (function (grt-vector (grt-float 0.0)) grt-vector) vector-div))
(defun vector-div (v f)
  "Grt-vector divided by a positive grt-float. Returns a newly allocated
   grt-vector."
  (grt-vector (/ (x v) f) (/ (y v) f) (/ (z v) f)))

(declaim
 (inline vector-length)
 (ftype (function (grt-vector) (grt-float 0.0)) vector-lenght))
(defun vector-length (v)
  "Length of a grt-vector."    
  (sqrt (+ (expt (x v) 2)
	   (expt (y v) 2)
	   (expt (z v) 2))))

(defun vector-alignp (a b)
  "Returns NIL when grt-vector A and B are aligned."
  (= 0.0 (vector-length (cross-product a b))))

(declaim
 (inline normalize)
 (ftype (function (grt-vector) grt-vector) normalize))
(defun normalize (vector)
  "Normalized copy of a grt-vector."
  (vector-div vector (vector-length vector)))

(declaim
 (inline nnormalize)
 (ftype (function (grt-vector) grt-vector) nnormalize))
(defun nnormalize (v)
  "Normalized version of a grt-vector. Non-consing."
  (let ((l (vector-length v)))
    (setf (x v) (/ (x v) l)
	  (y v) (/ (y v) l)
	  (z v) (/ (z v) l))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Matrix math for GRT
;;
;; Our matrix types are on occasion a bit restrictive in the interest of
;; speeeed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype matrix-1 ()
  '(array grt-float (4)))

(deftype matrix-4 ()
  '(array grt-float (4 4)))

(deftype matrix ()
  "4x1 or 4x4 floating point matrix used in GRT."
  '(or matrix-1 matrix-4))

(defmacro mref (matrix &rest indices)  ; row, col
  `(aref (the matrix ,matrix) ,@indices))

(defmacro make-matrix (dimensions &key initial-contents)
  "Create a matrix of given DIMENSIONS (maximum two dimensions!) filled with 0.0's
   or the given :INITIAL-CONTENTS."
  (if initial-contents
      `(make-array ,dimensions :element-type 'grt-float :initial-contents ,initial-contents)
      `(make-array ,dimensions :element-type 'grt-float :initial-element 0.0)))

(defun identity-matrix ()
  "Create a 4x4 identity matrix."
  (let ((I (make-matrix '(4 4))))
    (dotimes (j 4 I)
      (setf (mref I j j) 1.0))))

(declaim
 (ftype (function (matrix) unsigned-byte) matrix-rows))
(defun matrix-rows (matrix)
  "Return the number of rows in matrix."
  (array-dimension matrix 0))

(declaim
 (ftype (function (matrix) unsigned-byte) matrix-columns))
(defun matrix-columns (matrix)
  "Return the number of columns in matrix."
  (array-dimension matrix 1))

(defun copy-matrix (matrix)
  "Creates a copy of a 4x4 matrix."
  (let ((copy (make-matrix '(4 4))))
    (dotimes (row 4 copy)
      (dotimes (col 4)
	(setf (mref copy row col) (mref matrix row col))))))

(defun matrix-multiply (m1 m2)
  "Multiply two 4x4 matrices. Returns a newly allocated matrix."
  (let ((result (make-matrix '(4 4))))
    (dotimes (row 4 result)
      (dotimes (col 4)
	(dotimes (i 4)
	  (setf (mref result row col)
		(+ (mref result row col)
		   (* (mref m2 row i)
		      (mref m1 i col)))))))))

(declaim
 (ftype (function (grt-vector matrix-4) grt-vector) transform-vector))
(defun transform-vector (v m)
  "Apply a 4x4 transformation matrix M to a grt-vector V.
   Returns a newly allocated grt-vector."
  (grt-vector-bind (((x y z) v))
     (grt-vector (+ (* x (mref m 0 0))
		    (* y (mref m 0 1))
		    (* z (mref m 0 2))
		    (mref m 0 3))
		 (+ (* x (mref m 1 0))
		    (* y (mref m 1 1))
		    (* z (mref m 1 2))
		    (mref m 1 3))
		 (+ (* x (mref m 2 0))
		    (* y (mref m 2 1))
		    (* z (mref m 2 2))
		    (mref m 2 3)))))

(declaim
 (ftype (function (grt-vector matrix-4) grt-vector) ntransform-vector))
(defun ntransform-vector (v m)
  "Apply a 4x4 transformation matrix M to a grt-vector V.
   Returns a grt-vector. Non-consing."
  (grt-vector-bind (((x y z) v))
     (setf (x v) (+ (* x (mref m 0 0))
		    (* y (mref m 0 1))
		    (* z (mref m 0 2))
		    (mref m 0 3))
	   (y v) (+ (* x (mref m 1 0))
		    (* y (mref m 1 1))
		    (* z (mref m 1 2))
		    (mref m 1 3))
	   (z v) (+ (* x (mref m 2 0))
		    (* y (mref m 2 1))
		    (* z (mref m 2 2))
		    (mref m 2 3)))
     v))

(declaim
 (ftype (function (grt-vector matrix-4) grt-vector)
	transform-direction-vector))
(defun transform-direction-vector (v m)
  "Apply a 4x4 transformation matrix M to a grt-vector V,
   ignoring translation component. Returns a newly
   allocated grt-vector."
  (grt-vector-bind (((x y z) v))
     (grt-vector (+ (* x (mref m 0 0))
		    (* y (mref m 0 1))
		    (* z (mref m 0 2)))
		 (+ (* x (mref m 1 0))
		    (* y (mref m 1 1))
		    (* z (mref m 1 2)))
		 (+ (* x (mref m 2 0))
		    (* y (mref m 2 1))
		    (* z (mref m 2 2))))))

(defun inverse-matrix (matrix)
  "Find the inverse of a orthogonal affine matrix."
  (let ((res (make-matrix '(4 4))))
    ;; The inverse of the upper 3x3 is the transpose (since the basis
    ;; vectors are orthogonal to each other.  We also need to invert out
    ;; any scales in these basis vectors.  To do this, divide by the
    ;; square of the magnitude of the vector (once to get a unit vector,
    ;; and once more to get a vector of inverse length.)  This just
    ;; amounts to dividing by the vector dotted with itself.
    (dotimes (i 3)
      (let ((l (/ 1.0 (+ (expt (mref matrix 0 i) 2)
			 (expt (mref matrix 1 i) 2)
			 (expt (mref matrix 2 i) 2)))))
	(dotimes (j 3)
	  (setf (mref res i j) (* l (mref matrix j i))))))
    ;; The inverse of the translation component is just the negation of the
    ;; translation after dotting with the new upper3x3 rows.
    (let ((tx (mref matrix 0 3))
	  (ty (mref matrix 1 3))
	  (tz (mref matrix 2 3)))
      (dotimes (i 3)
	(setf (mref res i 3) (- (+ (* tx (mref res i 0))
				   (* ty (mref res i 1))
				   (* tz (mref res i 2)))))))
    ;; We assume a bottom (affine) row of [0 0 0 1].
    (dotimes (i 3)
      (setf (mref res 3 i) 0.0))
    (setf (mref res 3 3) 1.0)
    res))

(declaim
 (ftype (function (matrix-4) matrix-4) transpose-matrix))
(defun transpose-matrix (matrix)
  "Transposes a matrix. Returns a newly allocated matrix."
  (let ((rows (matrix-rows matrix))
	(cols (matrix-columns matrix))
	(result (make-matrix '(4 4))))
    (dotimes (row rows result)
      (dotimes (col cols)
	(setf (mref result col row)
	      (mref matrix row col))))))
