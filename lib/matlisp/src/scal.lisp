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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Raymond Toy
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: scal.lisp,v 1.5 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: scal.lisp,v $
;;; Revision 1.5  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
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

#+nil (export '(scal!
	  scal))

(defgeneric scal (alpha x)
  (:documentation
"
  Sytnax
  ======
  (SCAL alpha x)

  Purpose
  =======
  Computes and returns a new matrix equal to

             alpha * X

  where alpha is a scalar and X is a matrix.

"))

(defgeneric scal! (alpha x)
  (:documentation
"
  Sytnax
  ======
  (SCAL! alpha x)

  Purpose
  =======
  Same as SCAL except that the result is
  stored in X.
"))

(defmethod scal ((alpha number) (x number))
  (* alpha x))

(defmethod scal ((alpha double-float) (x real-matrix))
  (let ((nxm (number-of-elements x))
	(result (copy x)))
    (declare (type fixnum nxm))
    
    (dscal nxm alpha (store result) 1)
    result))

(defmethod scal ((alpha real) (x real-matrix))
  (scal (coerce alpha 'real-matrix-element-type) x))

(defmethod scal ((alpha #+:cmu kernel::complex-double-float
                        #+:sbcl sb-kernel::complex-double-float
			#+:allegro complex) (x real-matrix))
  (let* ((nxm (number-of-elements x))
	 (n (nrows x))
	 (m (ncols x))
	 (result (make-complex-matrix-dim n m)))
    (declare (type fixnum n m nxm))
    
    #+:allegro (setq alpha (complex-coerce alpha))

    (copy! x result)
    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* (store result) 1)

    result))

#+(or :cmu :sbcl)
(defmethod scal ((alpha complex) (x real-matrix))
  (scal (complex-coerce alpha) x))

(defmethod scal ((alpha double-float) (x complex-matrix))
  (let ((nxm (number-of-elements x))
	(result (copy x)))
    (declare (type fixnum nxm))
    (zdscal nxm alpha (store result) 1)
    
    result))

(defmethod scal ((alpha real) (x complex-matrix))
  (scal (coerce alpha 'real-matrix-element-type) x))

(defmethod scal ((alpha #+:cmu kernel::complex-double-float
                        #+:sbcl sb-kernel::complex-double-float
			#+:allegro complex) (x complex-matrix))
  (let ((nxm (number-of-elements x))
	(result (copy x)))
    (declare (type fixnum nxm))
 
    #+:allegro (setq alpha (complex-coerce alpha))

    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* (store result) 1)

    result))

#+(or :cmu :sbcl)
(defmethod scal ((alpha complex) (x complex-matrix))
  (scal (complex-coerce alpha) x))


(defmethod scal! ((alpha number) (x number))
  (error "cannot SCAL! two scalars, arg X must 
be a matrix to SCAL!"))

(defmethod scal! ((alpha double-float) (x real-matrix))
  (let ((nxm (number-of-elements x)))
    (declare (type fixnum nxm))
    
    (dscal nxm alpha (store x) 1)
    x))

(defmethod scal! ((alpha real) (x real-matrix))
  (scal! (coerce alpha 'real-matrix-element-type) x))

(defmethod scal! ((alpha complex) (x real-matrix))
  (error "cannot SCAL! a REAL-MATRIX by a COMPLEX, don't know
how to coerce COMPLEX to REAL"))

(defmethod scal! ((alpha double-float) (x complex-matrix))
  (let ((nxm (number-of-elements x)))
    (declare (type fixnum nxm))
    (zdscal nxm alpha (store x) 1)
    
    x))

(defmethod scal! ((alpha real) (x complex-matrix))
  (scal! (coerce alpha 'real-matrix-element-type) x))

(defmethod scal! ((alpha #+:cmu kernel::complex-double-float
                         #+:sbcl sb-kernel::complex-double-float
			 #+:allegro complex) (x complex-matrix))
  (let ((nxm (number-of-elements x)))
    (declare (type fixnum nxm))

    #+:allegro (setq alpha (complex-coerce alpha))

    (setf (aref *1x1-complex-array* 0) (realpart alpha))
    (setf (aref *1x1-complex-array* 1) (imagpart alpha))
    (zscal nxm *1x1-complex-array* (store x) 1)

    x))

#+(or :cmu :sbcl)
(defmethod scal! ((alpha complex) (x complex-matrix))
  (scal! (complex-coerce alpha) x))


