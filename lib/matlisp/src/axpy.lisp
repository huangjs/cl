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
;;; Originally written by Raymond Toy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: axpy.lisp,v 1.7 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: axpy.lisp,v $
;;; Revision 1.7  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.6  2003/02/14 05:42:11  rtoy
;;; Undo previous change.  We really need the 1x1-complex-array for
;;; Allegro because we don't (currently) pass in complex double-floats as
;;; an array.  (Not needed for CMUCL which handles this correctly.)
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

#+nil (use-package "LAPACK")
#+nil (use-package "BLAS")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

#+nil (export '(axpy!
		axpy))

;; note: we should optimize the calls to axpy from other axpy's since
;; we know exactly which one we are calling.
;;
;; also, the most common type of bug is with ! operators, e.g. when
;; you say (axpy! s a a)

(defgeneric axpy (alpha x y)
  (:documentation
 "
 Syntax
 ======
 (AXPY alpha x y)

 Purpose
 =======
 Computes  
      
                 ALPHA * X + Y

 where ALPHA is a scalar and X,Y are
 matrices.

 The result is stored in a new matrix 
 that has the same dimensions as Y.

 X,Y need not have the same dimensions,
 but must have the same total number of 
 elements.  Practically, this is useful
 for adding a row and column vector of 
 the same size etc ...
"))

(defgeneric axpy! (alpha x y)
  (:documentation
 " 
 Syntax
 ======
 (AXPY! alpha x y)

 Purpose
 =======
  Same as AXPY except that the result
  is stored in Y and Y is returned.
"))


(defmethod axpy :before ((alpha number) (x standard-matrix) (y standard-matrix))
  (let ((nxm-x (number-of-elements x))
	(nxm-y (number-of-elements y)))
    (declare (type fixnum nxm-x nxm-y))

    (if (not (= nxm-x nxm-y))
	(error "arguments X and Y to AXPY not of the same size"))))


(defmethod axpy ((alpha double-float) (x real-matrix) (y real-matrix))
  (let* ((nxm (number-of-elements y))
	 (result (copy y)))
    (declare (type fixnum nxm))

    (daxpy nxm alpha (store x) 1 (store result) 1)
    result))

(defmethod axpy ((alpha real) (x real-matrix) (y real-matrix))
  (axpy (coerce alpha 'real-matrix-element-type) x y))

(defmethod axpy ((alpha double-float) (x complex-matrix) (y real-matrix))
  (let* ((nxm (number-of-elements y))
	 (n (nrows y))
	 (m (ncols y))
	 (result (make-complex-matrix-dim n m))
	 (store-x (store x))
	 (store-y (store y))
	 (store-result (store result)))
    (declare (type fixnum n m nxm)
	     (type (real-matrix-store-type (*)) store-y)
	     (type (complex-matrix-store-type (*)) store-x store-result))

    (zcopy nxm store-x 1 store-result 1) ;; same as (COPY! x result)
    (zdscal nxm alpha store-result 1)   ;; same as (SCAL! alpha result)
    (daxpy nxm 1.0d0 store-y 1 store-result 2) ;; same as (AXPY! 1d0 y result)
    result))

(defmethod axpy ((alpha real) (x complex-matrix) (y real-matrix))
  (axpy (coerce alpha 'real-matrix-element-type) x y))


(defmethod axpy ((alpha double-float) (x real-matrix) (y complex-matrix))
  (let* ((nxm (number-of-elements y))
	 (result (copy y)))
    (declare (type fixnum nxm))
    (daxpy nxm alpha (store x) 1 (store result) 2)
    result))

(defmethod axpy ((alpha real) (x real-matrix) (y complex-matrix))
  (axpy (coerce alpha 'complex-matrix-element-type) x y))

(defmethod axpy ((alpha double-float) (x complex-matrix) (y complex-matrix))
  (let ((nxm (number-of-elements y))
	(result (copy y)))
    (declare (type fixnum nxm))
    (daxpy (* 2 nxm) alpha (store x) 1 (store result) 1)
    result))

(defmethod axpy ((alpha real) (x complex-matrix) (y complex-matrix))
  (axpy (coerce alpha 'complex-matrix-element-type) x y))


(defmethod axpy ((alpha #+:cmu kernel::complex-double-float
                        #+:sbcl sb-kernel::complex-double-float
			#+:allegro complex) (x real-matrix) (y complex-matrix))
  (let* ((nxm (number-of-elements y))
	 (n (nrows y))
	 (m (ncols y))
	 (result (make-complex-matrix-dim n m))
	 (store-x (store x))
	 (store-y (store y))
	 (store-result (store result)))
    (declare (type fixnum n m nxm)
	     (type (real-matrix-store-type (*)) store-x)
	     (type (complex-matrix-store-type (*)) store-y store-result))

    #+:allegro (setq alpha (complex-coerce alpha))

    (dcopy nxm store-x 1 store-result 2)
    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* store-result 1)
    (daxpy (* 2 nxm) 1.0d0 store-y 1 store-result 1)

    result))

#+(or :cmu :sbcl)
(defmethod axpy ((alpha complex) (x real-matrix) (y complex-matrix))
  (axpy (complex-coerce alpha) x y))

(defmethod axpy ((alpha #+:cmu kernel::complex-double-float
                        #+:sbcl sb-kernel::complex-double-float
			#+:allegro complex) (x complex-matrix) (y real-matrix))
  (let* ((nxm (number-of-elements y))
	 (result (copy x))
	 (store-result (store result)))
    (declare (type fixnum nxm)
	     (type (complex-matrix-store-type (*)) store-result))
   
    #+:allegro (setq alpha (complex-coerce alpha))

    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* store-result 1)
    (daxpy nxm 1.0d0 (store y) 1 store-result 2)
    
    result))

#+(or :cmu :sbcl)
(defmethod axpy ((alpha complex) (x complex-matrix) (y real-matrix))
  (axpy (complex-coerce alpha) x y))

(defmethod axpy ((alpha #+:cmu kernel::complex-double-float
                        #+:sbcl sb-kernel::complex-double-float
			#+:allegro complex) (x complex-matrix) (y complex-matrix))
  (let ((nxm (number-of-elements y))
	(result (copy y)))
    (declare (type fixnum nxm))


    #+:allegro (setq alpha (complex-coerce alpha))

    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zaxpy nxm *1x1-complex-array* (store x) 1 (store result) 1)
    result))

#+(or :cmu :sbcl)
(defmethod axpy ((alpha complex) (x complex-matrix) (y complex-matrix))
  (axpy (complex-coerce alpha) x y))



(defmethod axpy! :before ((alpha number) (x standard-matrix) (y standard-matrix))
  (let ((nxm-x (number-of-elements x))
	(nxm-y (number-of-elements y)))
    (declare (type fixnum nxm-x nxm-y))

    (if (not (= nxm-x nxm-y))
	(error "arguments X and Y to AXPY! not of the same size"))))



(defmethod axpy! ((alpha double-float) (x real-matrix) (y real-matrix))
  (let* ((nxm (number-of-elements y)))
    (declare (type fixnum nxm))

    (daxpy nxm alpha (store x) 1 (store y) 1)
    y))

(defmethod axpy! ((alpha real) (x real-matrix) (y real-matrix))
  (axpy! (coerce alpha 'real-matrix-element-type) x y))

(defmethod axpy! ((alpha number) (x complex-matrix) (y real-matrix))
  (error "cannot AXPY! a complex X to a real Y,
don't know how to coerce COMPLEX to REAL"))

(defmethod axpy! ((alpha double-float) (x real-matrix) (y complex-matrix))
  (let* ((nxm (number-of-elements y)))
    (declare (type fixnum nxm))
    (daxpy nxm alpha (store x) 1 (store y) 2)
    y))

(defmethod axpy! ((alpha real) (x real-matrix) (y complex-matrix))
  (axpy! (coerce alpha 'complex-matrix-element-type) x y))

(defmethod axpy! ((alpha double-float) (x complex-matrix) (y complex-matrix))
  (let ((nxm (number-of-elements y)))
    (declare (type fixnum nxm))
    (daxpy (* 2 nxm) alpha (store x) 1 (store y) 1)
    y))

(defmethod axpy! ((alpha real) (x complex-matrix) (y complex-matrix))
  (axpy! (coerce alpha 'complex-matrix-element-type) x y))

#+(or :cmu :sbcl)
(defmethod axpy! ((alpha #+:cmu kernel::complex-double-float
                         #+:sbcl sb-kernel::complex-double-float)
                  (x real-matrix)
                  (y complex-matrix))
  (let* ((nxm (number-of-elements y))
	 (store-x (store x))
	 (store-y (store y))
	 (realpart (realpart alpha))
	 (imagpart (imagpart alpha)))
    (declare (type fixnum nxm)
	     (type complex-matrix-element-type realpart imagpart)
	     (type (real-matrix-store-type (*)) store-x)
	     (type (complex-matrix-store-type (*)) store-y))

    (daxpy nxm realpart store-x 1 store-y 2)
    (with-vector-data-addresses ((addr-y store-y)
				 (addr-x store-x))
       (incf-sap :double-float addr-y)
       (blas::fortran-daxpy nxm imagpart addr-x 1 addr-y 2))

    y))

#+(or :cmu :sbcl)
(defmethod axpy! ((alpha complex) (x real-matrix) (y complex-matrix))
  (axpy! (complex-coerce alpha) x y))


#+:allegro
(defmethod axpy! ((alpha complex) (x real-matrix) (y complex-matrix))
  (let* ((nxm (number-of-elements y)))
    (declare (type fixnum nxm))

    (setq alpha (complex-coerce alpha))

    (dotimes (i nxm)
	(declare (type fixnum i))
	(setf (matrix-ref y i) (+ (matrix-ref y i) (* alpha (matrix-ref x i)))))

    y))

(defmethod axpy! ((alpha #+:cmu kernel::complex-double-float
                         #+:sbcl sb-kernel::complex-double-float
			 #+:allegro complex) (x complex-matrix) (y complex-matrix))
  (let ((nxm (number-of-elements y)))
    (declare (type fixnum nxm))

    #+:allegro (setq alpha (complex-coerce alpha))

    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zaxpy nxm *1x1-complex-array* (store x) 1 (store y) 1)
    y))

#+(or :cmu :sbcl)
(defmethod axpy! ((alpha complex) (x complex-matrix) (y complex-matrix))
  (axpy! (complex-coerce alpha) x y))


