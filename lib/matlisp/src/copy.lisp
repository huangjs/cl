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
;;; $Id: copy.lisp,v 1.8 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: copy.lisp,v $
;;; Revision 1.8  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.7  2003/02/14 05:42:12  rtoy
;;; Undo previous change.  We really need the 1x1-complex-array for
;;; Allegro because we don't (currently) pass in complex double-floats as
;;; an array.  (Not needed for CMUCL which handles this correctly.)
;;;
;;; Revision 1.5  2001/10/29 16:23:10  rtoy
;;; COPY! was broken on CMUCL because FORTRAN-DSCAL is no longer
;;; exported.  Use the Allegro version.  From M. Koerber.
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

#+nil (export '(copy!
		copy))

(defvar *1x1-real-array* (make-array 1 :element-type 'double-float))
(defvar *1x1-complex-array* (make-array 2 :element-type 'double-float))

(defgeneric copy (matrix)
  (:documentation 
   "
  Syntax
  ======
  (COPY x)
 
  Purpose
  =======
  Return a copy of the matrix X"))

(defgeneric copy! (matrix new-matrix)
  (:documentation
   "
  Syntax
  ======
  (COPY! x y)

  Purpose
  =======
  Copies the contents of the matrix X to
  matrix Y, returns Y.

  X,Y need not have the same dimensions,
  nor the same number of elements.

  Furthermore, x may be a scalar, in which
  case Y is filled with X.

  The contents of X must be coercable to
  the type of Y.  For example,
  a COMPLEX-MATRIX cannot be copied to a
  REAL-MATRIX but the converse is possible.
"))


(defmethod copy ((matrix standard-matrix))
  (make-instance 'standard-matrix :nrows (nrows matrix) :ncols (ncols matrix) :store (copy-seq (store matrix))))

(defmethod copy ((matrix real-matrix))
  (let* ((size (number-of-elements matrix))
	 (n (nrows matrix))
	 (m (ncols matrix))
	 (result (make-real-matrix-dim n m)))
    (declare (type fixnum size n m))
    (blas:dcopy size (store matrix) 1 (store result) 1)
    result))

(defmethod copy ((matrix complex-matrix))
  (let* ((size (number-of-elements matrix))
	 (n (nrows matrix))
	 (m (ncols matrix))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum size n m))
    (blas:zcopy size (store matrix) 1 (store result) 1)
    result))

(defmethod copy ((matrix number))
  matrix)

#|
(defmethod copy! :before ((x standard-matrix) (y standard-matrix))
  (let ((nxm-x (number-of-elements x))
	(nxm-y (number-of-elements y)))
    (declare (type fixnum nxm-x nxm-y))
    (if (not (= nxm-x nxm-y))
	(error "arguments X,Y to COPY! are of different size"))))
|#

(defmethod copy! ((x real-matrix) (y real-matrix))
  (let* ((nxm-x (number-of-elements x))
	 (nxm-y (number-of-elements y))
	 (nxm (min nxm-x nxm-y)))
    (declare (type fixnum nxm-x nxm-y nxm))
    (dcopy nxm (store x) 1 (store y) 1)
    y))

(defmethod copy! ((x real-matrix) (y complex-matrix))
  (let* ((nxm-x (number-of-elements x))
	 (nxm-y (number-of-elements y))
	 (nxm (min nxm-x nxm-y)))
    (declare (type fixnum nxm-x nxm-y nxm))

    ;; Set the imaginary parts of Y to zero.
    (zdscal nxm 0.0d0 (store y) 1)    

    ;; Copy the elements of X to the real parts of Y.
    (dcopy nxm (store x) 1 (store y) 2)
    y))
  
(defmethod copy! ((x complex-matrix) (y real-matrix))
  (error "cannot copy a COMPLEX-MATRIX into a REAL-MATRIX,
don't know how to coerce a COMPLEX to a REAL"))

(defmethod copy! ((x complex-matrix) (y complex-matrix))
  (let* ((nxm-x (number-of-elements x))
	 (nxm-y (number-of-elements y))
	 (nxm (min nxm-x nxm-y)))
    (declare (type fixnum nxm-x nxm-y nxm))
    (dcopy (* 2 nxm) (store x) 1 (store y) 1)
    y))

(defmethod copy! ((x standard-matrix) (y standard-matrix))
  (let* ((nxm-x (number-of-elements x))
	 (nxm-y (number-of-elements y))
	 (nxm (min nxm-x nxm-y)))
    (declare (type fixnum nxm-x nxm-y nxm))

    (dotimes (i nxm)
      (declare (type fixnum i))
      (setf (matrix-ref y i) (matrix-ref x i)))

    y))

(defmethod copy! ((x double-float) (y real-matrix))
  (let ((nxm (number-of-elements y)))
    (setf (aref *1x1-real-array* 0) x)
    (dcopy nxm *1x1-real-array* 0 (store y) 1)
    y))

(defmethod copy! ((x real) (y real-matrix))
  (let ((nxm (number-of-elements y)))
    (setf (aref *1x1-real-array* 0) (coerce x 'real-matrix-element-type))
    (dcopy nxm *1x1-real-array* 0 (store y) 1)
    y))

(defmethod copy! ((x complex) (y real-matrix))
  (error "cannot copy ~a to ~a, don't know how to coerce COMPLEX to REAL"
	 x
	 y))

(defmethod copy! ((x #+:cmu kernel::complex-double-float
                     #+:sbcl sb-kernel::complex-double-float
		     #+:allegro complex) (y complex-matrix))
  (let ((nxm (number-of-elements y)))

    #+:allegro (setq x (complex-coerce x))

    (setf (aref *1x1-complex-array* 0) (realpart x))
    (setf (aref *1x1-complex-array* 1) (imagpart x))
    (zcopy nxm *1x1-complex-array* 0 (store y) 1)
    y))

(defmethod copy! ((x number) (y complex-matrix))
  (let ((nxm (number-of-elements y)))
    (setq x (complex-coerce x))
    (setf (aref *1x1-complex-array* 0) (realpart x))
    (setf (aref *1x1-complex-array* 1) (imagpart x))
    (zcopy nxm *1x1-complex-array* 0 (store y) 1)
    y))

    
