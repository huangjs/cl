;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved. 
;;; 
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;; 
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Raymond Toy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: mdivide.lisp,v 1.5 2001/06/22 12:54:28 rtoy Exp $
;;;
;;; $Log: mdivide.lisp,v $
;;; Revision 1.5  2001/06/22 12:54:28  rtoy
;;; o Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;;   instead of using the error-prone make-array.
;;; o Fix a bug in (m./ <complex> <real>).  The implementation was totally
;;;   broken.
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.2  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

#+nil (export '(m/
		m./
		m/!
		m./!))

(defgeneric m/ (a &optional b)
  (:documentation
   "
  Syntax
  ======
  (M/ a [b])

  Purpose
  =======

  Equivalent to inv(A) * B, but far more efficient and accurate.
  If A is not given, the inverse of B is returned. 

  A is an NxN square matrix and B is NxM.

  B may be a scalar in which case M/ is equivalent to M./

  See M/!, M./, M./!, GESV
"))

(defgeneric m/! (a &optional b)
  (:documentation
   "
  Syntax
  ======
  (M/! a [b])

  Purpose
  =======
  Destructive version of M/

           B <- (M/ A B)

      or 
 
           A <- (M/ A)

  See M/, M./, M./!, GESV
"))

(defgeneric m./ (a b)
  (:documentation
   "
  Syntax
  ======
  (M./ a b)

  Purpose
  =======
  Create a new matrix which is the element by
  element division of A by B.

  A and/or B may be scalars.

  If A and B are matrices they need not be of
  the same dimension, however, they must have
  the same number of total elements.

  See M./!, M/, M/!, GESV
"))

(defgeneric m./! (a b)
  (:documentation
   "
  Syntax
  ======
  (M./! a b)

  Purpose
  =======
  Destructive version of M./

            B <- (M./ A B)

  If B is a scalar then the result overrides A, not B.
  
  See M./, M/, M/!, GESV
"))


(defmethod m/ :before ((a standard-matrix) &optional b)

  (if b
      (typecase b
	(number t)
	(standard-matrix (if (not 
			      (and 
			       (square-matrix-p a)
			       (= (nrows b) (nrows a))))
			     (error "dimensions of A,B given to M/ do not match")))
	(t (error "argument B given to M/ is not a matrix or a number")))

    (if (not (square-matrix-p a))
	(error "argument A given to M/ is not a square matrix"))))


(defmethod m/ ((a standard-matrix) &optional b)
  (if b
      (typecase b
	(number (m./ a b))
	(standard-matrix (multiple-value-bind (x ipiv f info)
			     (gesv a b)
			   (declare (ignore ipiv f))
			   (if (numberp info)		     
			       (error "argument A given to M/ is singular to working machine precision")
			     x))))
    (multiple-value-bind (x ipiv f info)
	(gesv a (eye (nrows a)))
      (declare (ignore ipiv f))
      (if (numberp info)
	(error "argument A given to M/ is singular to working machine precision")
	x))))

(defmethod m/! :before ((a standard-matrix) &optional b)

  (if b
      (typecase b
	(number t)
	(standard-matrix (if (not 
			      (and 
			       (square-matrix-p a)
			       (= (nrows b) (nrows a))))
			     (error "dimensions of A,B given to M/ do not match")))
	(t (error "argument B given to M/! is not a matrix or a number")))

    (if (not (square-matrix-p a))
	(error "argument A given to M/! is not a square matrix"))))


(defmethod m/! ((a standard-matrix) &optional b)
  (if b
      (typecase b
	(number (m./! a b))
	(standard-matrix (multiple-value-bind (x ipiv f info)
			     (gesv! (copy a) b)
			   (declare (ignore ipiv f))
			   (if (numberp info)		     
			       (error "argument A given to M/! is singular to working machine precision")
			     x))))
    (multiple-value-bind (x ipiv f info)
	(gesv! (copy a) (eye (nrows a)))
      (declare (ignore ipiv f))
      (if (numberp info)
	(error "argument A given to M/! is singular to working machine precision")
	x))))

(defmethod m./ :before ((a standard-matrix) (b standard-matrix))
  (let ((nxm-a (number-of-elements a))
	(nxm-b (number-of-elements b)))
    (declare (type fixnum nxm-a nxm-b))
    (unless (= nxm-a nxm-b)
      (error "arguments A,B given to M./ are not the same size"))))

(defmethod m./! :before ((a standard-matrix) (b standard-matrix))
  (let ((nxm-a (number-of-elements a))
	(nxm-b (number-of-elements b)))
    (declare (type fixnum nxm-a nxm-b))
    (unless (= nxm-a nxm-b)
      (error "arguments A,B given to M./! are not the same size"))))

  
(defmethod m./ ((a real-matrix) (b real-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-real-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type real-matrix-element-type a-val b-val))
	(setf (matrix-ref result k) (/ a-val b-val))))))

(defmethod m./ ((a complex-matrix) (b complex-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type) a-val b-val))
	(setf (matrix-ref result k) (/ a-val b-val))))))

(defmethod m./ ((a real-matrix) (b complex-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type)  b-val)
		 (type real-matrix-element-type a-val))
	(setf (matrix-ref result k) (/ a-val b-val))))))

(defmethod m./ ((a complex-matrix) (b real-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type)  a-val)
		 (type real-matrix-element-type b-val))
	(setf (matrix-ref result k) (/ a-val b-val))))))

  
(defmethod m./! ((a real-matrix) (b real-matrix))
  (let* ((nxm (number-of-elements b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type real-matrix-element-type a-val b-val))
	(setf (matrix-ref b k) (/ a-val b-val))))))

(defmethod m./! ((a complex-matrix) (b complex-matrix))
  (let* ((nxm (number-of-elements b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type) a-val b-val))
	(setf (matrix-ref b k) (/ a-val b-val))))))

(defmethod m./! ((a real-matrix) (b complex-matrix))
  (let* ((nxm (number-of-elements b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (let ((a-val (matrix-ref a k))
	    (b-val (matrix-ref b k)))
	(declare (type (complex complex-matrix-element-type)  b-val)
		 (type real-matrix-element-type a-val))
	(setf (matrix-ref b k) (/ a-val b-val))))))

(defmethod m./! ((a complex-matrix) (b real-matrix))
  (error "cannot M./! a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m./ ((a number) (b number))
  (/ a b))

(defmethod m./! ((a number) (b number))
  (/ a b))

(defmethod m./ ((a standard-matrix) (b number))
  (scal (/ b) a))

(defmethod m./! ((a standard-matrix) (b number))
  (scal! (/ b) a))

(defmethod m./ ((a number) (b standard-matrix))
  (scal! a (map-matrix! #'/ (copy b))))

(defmethod m./! ((a number) (b standard-matrix))
  (scal! a (map-matrix! #'/ b)))



