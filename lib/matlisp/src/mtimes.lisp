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
;;; $Id: mtimes.lisp,v 1.7 2002/07/29 01:08:45 rtoy Exp $
;;;
;;; $Log: mtimes.lisp,v $
;;; Revision 1.7  2002/07/29 01:08:45  rtoy
;;; Fix typos and initialization errors in m.* methods for (real-matrix
;;; complex-matrix) and (complex-matrix real-matrix).
;;;
;;; Revision 1.6  2002/06/13 16:54:08  rtoy
;;; Jefferson Provost reports that m.* runs very, very slowly.  Fix this.
;;; The slowness was mostly likely caused by calling matrix-ref for each
;;; array element.  Tunc supplied a version for the real x real function.
;;; I supplied versions for the other cases and for the m.*! cases.
;;;
;;; Revision 1.5  2001/06/22 12:52:41  rtoy
;;; Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;; instead of using the error-prone make-array.
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

#+nil (export '(m*
	  m*!
	  m.*
	  m.*!
	  set-m*!-swap-size))


(defgeneric m* (a b)
  (:documentation
   "
  Syntax
  ======
  (M* a b)

  Purpose
  =======
  Create a new matrix which is the product of A and B.
  A and/or B  may be scalars, in which
  case the multiplication is element-wise.
"))

(defgeneric m*! (a b)
  (:documentation
   "
  Syntax
  ======
  (M*! a b)

  Purpose
  =======
  Desctructive version of M*:

       B <- A * B
"))

(defgeneric m.* (a b)
  (:documentation
   "
  Syntax
  ======
  (M.* a b)

  Purpose
  =======
  Create a new matrix which is the element by
  element product of A and B.

  A and/or B may be scalars.

  If A,B are matrices they need not be of the
  same dimension, however, they must have the
  same number of elements.
"))

(defgeneric m.*! (a b)
  (:documentation
   "
  Syntax
  ======
  (M.*! a b)

  Purpose
  =======
  Desctructive version of M.*:

       B <- A .* B
"))

;;;;;

(defmethod m.* :before ((a standard-matrix) (b standard-matrix))
  (let ((nxm-a (number-of-elements a))
	(nxm-b (number-of-elements b)))
    (declare (type fixnum nxm-a nxm-b))
    (unless (= nxm-a nxm-b)
      (error "arguments A,B given to M.* are not the same size"))))

(defmethod m.*! :before ((a standard-matrix) (b standard-matrix))
  (let ((nxm-a (number-of-elements a))
	(nxm-b (number-of-elements b)))
    (declare (type fixnum nxm-a nxm-b))
    (unless (= nxm-a nxm-b)
      (error "arguments A,B given to M.* are not the same size"))))

  
(defmethod m.* ((a real-matrix) (b real-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-real-matrix-dim n m))
	 (a-store (store a))
	 (b-store (store b))
	 (r-store (store result)))
    (declare (type fixnum n m nxm))

    (dotimes (k nxm result)
      (declare (type fixnum k))
      (setf (aref r-store k)
	    (* (aref a-store k) (aref b-store k))))))

(defmethod m.* ((a complex-matrix) (b complex-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-complex-matrix-dim n m))
	 (a-store (store a))
	 (b-store (store b))
	 (r-store (store result)))
    (declare (type fixnum n m nxm))

    (do ((k 0 (+ k 1))
	 (k-r 0 (+ k-r 2))
	 (k-i 1 (+ k-i 2)))
	((>= k nxm))
      (declare (type fixnum k-r k-i))
      (let* ((a-val (complex (aref a-store k-r) (aref a-store k-i)))
	     (b-val (complex (aref b-store k-r) (aref b-store k-i)))
	     (r-val (* a-val b-val)))
	(setf (aref r-store k-r) (realpart r-val))
	(setf (aref r-store k-i) (imagpart r-val))))
    result))

(defmethod m.* ((a real-matrix) (b complex-matrix))
  (let* ((n (nrows b))
	 (m (ncols b))
	 (nxm (number-of-elements b))
	 (result (make-complex-matrix-dim n m))
	 (a-store (store a))
	 (b-store (store b))
	 (r-store (store result)))
    (declare (type fixnum n m nxm))

    (do ((k 0 (1+ k))
	 (b-r 0 (+ b-r 2))
	 (b-i 1 (+ b-i 2)))
	((>= k nxm))
      (declare (type fixnum k b-r b-i))
      (let* ((a-val (aref a-store k))
	     (r-val (* a-val (aref b-store b-r)))
	     (i-val (* a-val (aref b-store b-i))))
	(setf (aref r-store b-r) r-val)
	(setf (aref r-store b-i) i-val)))
    result))

(defmethod m.* ((a complex-matrix) (b real-matrix))
  (m.* b a))

;;;;;

  
(defmethod m.*! ((a real-matrix) (b real-matrix))
  (let* ((nxm (number-of-elements b))
	 (a-store (store a))
	 (b-store (store b)))
    (declare (type fixnum nxm))

    (dotimes (k nxm b)
      (declare (type fixnum k))
      (setf (aref b-store k) (* (aref a-store k) (aref b-store k))))))

(defmethod m.*! ((a complex-matrix) (b complex-matrix))
  (let ((nxm (number-of-elements b))
	(a-store (store a))
	(b-store (store b)))
    (declare (type fixnum nxm))

    (do ((k 0 (+ k 1))
	 (k-r 0 (+ k-r 2))
	 (k-i 1 (+ k-i 2)))
	((>= k nxm))
      (declare (type fixnum k k-r k-i))
      (let* ((a-val (complex (aref a-store k-r) (aref a-store k-i)))
	     (b-val (complex (aref b-store k-r) (aref b-store k-i)))
	     (r-val (* a-val b-val)))
	(setf (aref b-store k-r) (realpart r-val))
	(setf (aref b-store k-i) (imagpart r-val))))
    b))

(defmethod m.*! ((a real-matrix) (b complex-matrix))
  (let ((nxm (number-of-elements b))
	(a-store (store a))
	(b-store (store b)))
    (declare (type fixnum nxm))

    (do ((k 0 (1+ k))
	 (b-r 0 (+ b-r 2))
	 (b-i 1 (+ b-i 2)))
	((>= k nxm))
      (declare (type fixnum k b-r b-i))
      (let* ((a-val (aref a-store k))
	     (r-val (* a-val (aref b-store b-r)))
	     (i-val (* a-val (aref b-store b-i))))
	(setf (aref b-store b-r) r-val)
	(setf (aref b-store b-i) i-val)))
    b))

(defmethod m.*! ((a complex-matrix) (b real-matrix))
  (error "cannot M.*! a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m.* ((a number) (b number))
  (* a b))

(defmethod m.*! ((a number) (b number))
  (* a b))

(defmethod m.* ((a standard-matrix) (b number))
  (scal b a))

(defmethod m.* ((a number) (b standard-matrix))
  (scal a b))


(defmethod m.*! ((a standard-matrix) (b number))
  (scal! b a))

(defmethod m.*! ((a number) (b standard-matrix))
  (scal! a b))

;;;;;

(defmethod m* ((a number) (b number))
  (* a b))

(defmethod m* ((a standard-matrix) (b number))
  (scal b a))

(defmethod m* ((a number) (b standard-matrix))
  (scal a b))

;;;;;

(defmethod m*! ((a number) (b number))
  (* a b))

(defmethod m*! ((a standard-matrix) (b number))
  (scal! b a))

(defmethod m*! ((a number) (b standard-matrix))
  (scal! a b))

(defmethod m*! ((a complex-matrix) (b real-matrix))
  (error "cannot M*! a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce COMPLEX to REAL"))

(defmethod m* ((a real-matrix) (b real-matrix))
  (gemm! 1.0d0 a b 0.0d0 (make-real-matrix-dim (nrows a) (ncols b))))

(defmethod m* ((a standard-matrix) (b standard-matrix))
  (gemm! 1.0d0 a b 0.0d0 (make-complex-matrix-dim (nrows a) (ncols b))))

(defmethod m*! :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b)))
    (if (not (= n-a m-a n-b))
	(error "cannot M*! a ~dx~d matrix into a ~dx~d matrix"
	       n-a
	       m-a
	       n-b
	       (ncols b)))))

;; TODO: on installation, try GEMM and see if this swap space
;; is necessary.

;; Q: what exactly is a special variable?
(defparameter *auto-set-m*!-swap* t)
(defparameter *m*!-swap-size* (* 512 512))
(defparameter *m*!-swap* 
  (make-array (* 2 *m*!-swap-size*) :element-type 'complex-matrix-element-type))
(defparameter *m*!-complex-wrapper* 
  (make-instance 'complex-matrix :nrows *m*!-swap-size* :ncols 1 :store *m*!-swap*))
(defparameter *m*!-real-wrapper* 
  (make-instance 'real-matrix :nrows *m*!-swap-size* :ncols 1 :store *m*!-swap*))

(declaim (inline set-m*!-swap-size))
(defun set-m*!-swap-size (nxm)
  "
  Syntax
  ======
  (SET-M*!-SWAP-SIZE nxm)

  Purpose
  =======
  Resets the swap space allocated for M*!.
  By default, 

              NxM = (* 512 512)

  meaning that M*! can safely multiply a PxN matrix and
  an NxM matrix Y given by the expression:

              (M*! X Y)

  There is no restriction on P.

  The actual storage allocated is 2 * NxM * size of DOUBLE-FLOAT,
  or equivalently NxM * size of (COMPLEX DOUBLE-FLOAT).

  If *auto-set-m*!-swap* (default) then the swap will be adjusted 
  if argument Y given to M*! exceeds swap size.

  Returns NxM
"
  (declare (special *m*!-swap-size*
		    *m*!-swap*
		    *m*!-real-wrapper*
		    *m*!-complex-wrapper*))
  (if (<= nxm 0)
      (error "argument NxM given to SET-M*!-SWAP-SIZE should be bigger than 0"))

  (let ((swap (allocate-complex-store nxm)))

    (setf *m*!-swap-size* nxm)
    (setf *m*!-swap* swap)
    (setf (store *m*!-complex-wrapper*) swap) 
    (setf (nrows *m*!-complex-wrapper*) nxm) 
    (setf (ncols *m*!-complex-wrapper*) 1)     
    (setf (number-of-elements *m*!-complex-wrapper*) nxm) 
    (setf (store *m*!-real-wrapper*) swap) 
    (setf (nrows *m*!-real-wrapper*) nxm)
    (setf (ncols *m*!-real-wrapper*) 1)
    (setf (number-of-elements *m*!-real-wrapper*) nxm) 
    
    nxm))
  

(defmethod m*! ((a real-matrix) (b real-matrix))
  (let ((n (nrows b))
	(m (ncols b)))
    (declare (type fixnum n m)
	     (special *m*!-real-wrapper*))

    (if (and *auto-set-m*!-swap*
	     (> (* n m) *m*!-swap-size*))
	(set-m*!-swap-size (* n m)))

    (copy! b *m*!-real-wrapper*)
    (setf (nrows *m*!-real-wrapper*) n)
    (setf (ncols *m*!-real-wrapper*) m)
    (setf (number-of-elements *m*!-real-wrapper*) (* n m))
    (gemm! 1.0d0 a b 0.0d0 *m*!-real-wrapper*)
    (copy! *m*!-real-wrapper* b)
    (setf (nrows *m*!-real-wrapper*) *m*!-swap-size*)
    (setf (ncols *m*!-real-wrapper*) 1)
    (setf (number-of-elements *m*!-real-wrapper*) *m*!-swap-size*)
    b))

(defmethod m*! ((a standard-matrix) (b complex-matrix))
  (let ((n (nrows b))
	(m (ncols b)))
    (declare (type fixnum n m)
	     (special *m*!-complex-wrapper*))

    (if (and *auto-set-m*!-swap*
	     (> (* n m) *m*!-swap-size*))
	(set-m*!-swap-size (* n m)))
    
    (copy! b *m*!-complex-wrapper*)
    (setf (nrows *m*!-complex-wrapper*) n)
    (setf (ncols *m*!-complex-wrapper*) m)
    (setf (number-of-elements *m*!-complex-wrapper*) (* n m))
    (gemm! 1.0d0 a b 0.0d0 *m*!-complex-wrapper*)
    (copy! *m*!-complex-wrapper* b)
    (setf (nrows *m*!-complex-wrapper*) *m*!-swap-size*)
    (setf (ncols *m*!-complex-wrapper*) 1)
    (setf (number-of-elements *m*!-complex-wrapper*) *m*!-swap-size*)
    b))




