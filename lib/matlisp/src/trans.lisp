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
;;; $Id: trans.lisp,v 1.5 2001/06/22 12:52:41 rtoy Exp $
;;;
;;; $Log: trans.lisp,v $
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

;; notes
;; =====
;; transposition is usually a redundant operation.  For example, all BLAS/LAPACK
;; operators take an extra argument asking whether the matrix is transposed,
;; for small operations transposition doesn't make a difference, for repeated
;; small operations -- it may, so you need to use this feature of the 
;; interfaced BLAS/LAPACK functions.
;;
;; also, the intent that TRANSPOSE creates a new matrix should be made clear,
;; for example, taking the transpose of a row/column vector is easy, due to
;; representation, but this will not create a new matrix.

#+nil (export '(transpose
	  ctranspose))

(defgeneric transpose (matrix)
  (:documentation 
   "
  Syntax
  ======
  (TRANSPOSE matrix)
 
  Purpose
  =======
  Creates a new matrix which is the transpose of MATRIX.
"))

(defgeneric ctranspose (matrix)
  (:documentation
   "
  Syntax
  ======
  (CTRANSPOSE matrix)

  Purpose
  =======
  Returns a new matrix which is the conjugate transpose
  of MATRIX.
"))

(defmethod transpose ((mat t))
  (error "argument to TRANSPOSE must be a matrix"))

(defmethod ctranspose ((mat t))
  (error "argument to CTRANSPOSE must be a matrix"))

(defmethod transpose ((x number))
  x)

(defmethod transpose ((mat standard-matrix))
  (let* ((n (nrows mat))
	 (m (ncols mat))
	 (new-mat (copy mat)))

    (declare (type fixnum n m))

    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j m)
	(declare (type fixnum j))
	(setf (matrix-ref new-mat j i)
	      (matrix-ref mat i j))))
    new-mat))

(defmethod transpose ((mat real-matrix))
  (let* ((n (nrows mat))
	 (m (ncols mat))
	 (nxm (number-of-elements mat))
	 (store (store mat))
	 (new-store (allocate-real-store nxm)))

    (declare (type fixnum n m nxm)
	     (type (real-matrix-store-type (*)) store new-store))

    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j m)
	(declare (type fixnum j))
	(setf (aref new-store (fortran-matrix-indexing j i m))
	      (aref store (fortran-matrix-indexing i j n)))))
    
    (make-instance 'real-matrix :nrows m :ncols n :store new-store)))

(defmethod transpose ((mat complex-matrix))
  (let* ((n (nrows mat))
	 (m (ncols mat))
	 (nxm (number-of-elements mat))
	 (store (store mat))
	 (new-store (allocate-complex-store nxm)))

    (declare (type fixnum n m nxm)
	     (type (complex-matrix-store-type (*)) store new-store))

    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j m)
	(declare (type fixnum j))
	(let* ((store-index (fortran-complex-matrix-indexing i j n))
	       (new-store-index (fortran-complex-matrix-indexing j i m))
	       (realpart (aref store store-index))
	       (imagpart (aref store (1+ store-index))))
	  (declare (type fixnum store-index new-store-index)
		   (type complex-matrix-element-type realpart imagpart))
	  
	  (setf (aref new-store new-store-index) realpart)
	  (setf (aref new-store (1+ new-store-index)) imagpart))))

    (make-instance 'complex-matrix :nrows m :ncols n :store new-store)))


(defmethod ctranspose ((x complex))
  (declare (type complex x))
  (conjugate x))

(defmethod ctranspose ((x real))
  x)

(defmethod ctranspose ((mat standard-matrix))
  (error "don't know how to take the conjugate transpose of
a STANDARD-MATRIX, element types are not known"))

(defmethod ctranspose ((mat real-matrix))
  (transpose mat))

(defmethod ctranspose ((mat complex-matrix))
  (let* ((n (nrows mat))
	 (m (ncols mat))
	 (nxm (number-of-elements mat))
	 (store (store mat))
	 (new-store (allocate-complex-store nxm)))

    (declare (type fixnum n m nxm)
	     (type (complex-matrix-store-type (*)) store new-store))

    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j m)
	(declare (type fixnum j))
	(let* ((store-index (fortran-complex-matrix-indexing i j n))
	       (new-store-index (fortran-complex-matrix-indexing j i m))
	       (realpart (aref store store-index))
	       (imagpart (aref store (1+ store-index))))
	  (declare (type fixnum store-index new-store-index)
		   (type complex-matrix-element-type realpart imagpart))
	  
	  (setf (aref new-store new-store-index) realpart)
	  (setf (aref new-store (1+ new-store-index)) (- imagpart)))))

    (make-instance 'complex-matrix :nrows m :ncols n :store new-store)))

