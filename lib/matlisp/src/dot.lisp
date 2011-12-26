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
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley,
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: dot.lisp,v 1.6 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: dot.lisp,v $
;;; Revision 1.6  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.5  2002/07/28 14:59:31  rtoy
;;; Clean up method dot for complex matrices.
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
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

#+nil (export 'dot)

(defgeneric dot (x y &optional conjugate-p)
  (:documentation
"
  Sytnax
  ======
  (DOT x y [conjugate-p])

  Purpose
  =======
  Computes the inner product of X,Y.

  CONJUGATE-P       Computed Result
  ---------------------------------
                         H
  T (default)           X * Y
                         T
  NIL                   X * Y


  If X is real then CONJUGATE-P has no 
  effect since for real vectors:

                H   T
               X = X

  X,Y must be vectors but it doesn't matter
  if they are row or column vectors.

  If X and Y are both scalars then this is the same
  as (* (CONJUGATE X) Y) if CONJUAGTE-P and (* X Y)
  otherwise.
"))

(defmethod dot ((x number) (y number) &optional (conjugate-p t))
  (if conjugate-p
      (* (conjugate x) y)
    (* x y)))

(defmethod dot :before ((x standard-matrix) (y standard-matrix) &optional conjugate-p)
  (declare (ignore conjugate-p))
  (if (not (row-or-col-vector-p x))
      (error "argument X to DOT is not a row or column vector")
    (if (not (row-or-col-vector-p y))
	(error "argument Y to DOT is not a row or column vector")
      (let ((nxm-x (number-of-elements x))
	    (nxm-y (number-of-elements y)))
	(declare (type fixnum nxm-x nxm-y))
	(if (not (= nxm-x nxm-y))
	    (error "arguments X,Y to DOT are not of the same size"))))))

(defmethod dot ((x real-matrix) (y real-matrix) &optional conjugate-p)
  (declare (ignore conjugate-p))
  (let ((nxm (number-of-elements x)))
    (declare (type fixnum nxm))
    (ddot nxm (store x) 1 (store y) 1)))

#+(or :cmu :sbcl)
(defmethod dot ((x real-matrix) (y complex-matrix) &optional conjugate-p)
  (declare (ignore conjugate-p))
  (let ((nxm (number-of-elements x))
	(store-x (store x))
	(store-y (store y)))
    (declare (type fixnum nxm)
	     (type (real-matrix-store-type (*)) store-x)
	     (type (complex-matrix-store-type (*)) store-y))

    (let ((realpart (ddot nxm store-x 1 store-y 2))
	  (imagpart (with-vector-data-addresses ((addr-x store-x)
						 (addr-y store-y))
			  (incf-sap :double-float addr-y)
			  (blas::fortran-ddot nxm addr-x 1 addr-y 2))))

      (declare (type complex-matrix-element-type realpart imagpart))

      #+:complex-arg-implies-complex-result
      (complex realpart imagpart)
      #-:complex-arg-implies-comples-result
      (if (zerop imagpart)
	  realpart
	(complex realpart imagpart))
      )))


#+:allegro
(defmethod dot ((x real-matrix) (y complex-matrix) &optional conjugate-p)
  (declare (ignore conjugate-p))
  (let ((nxm (number-of-elements x)))
    (declare (type fixnum nxm))

    (let ((realpart 0.0d0)
	  (imagpart 0.0d0))
      (declare (type complex-matrix-element-type realpart imagpart))

      (dotimes (i nxm)
        (declare (type fixnum i))
        (let ((x-elt (matrix-ref x i))
	      (y-elt (matrix-ref y i)))
	  (incf realpart (+ x-elt (realpart y-elt)))
	  (incf imagpart (+ x-elt (imagpart y-elt)))))


      #+:complex-arg-implies-complex-result
      (complex realpart imagpart)
      #-:complex-arg-implies-comples-result
      (if (zerop imagpart)
	  realpart
	(complex realpart imagpart))
      )))

#+(or :cmu :sbcl)
(defmethod dot ((x complex-matrix) (y real-matrix) &optional (conjugate-p t))
  (let ((nxm (number-of-elements x))
	(store-x (store x))
	(store-y (store y)))
    (declare (type fixnum nxm)
	     (type (real-matrix-store-type (*)) store-y)
	     (type (complex-matrix-store-type (*)) store-x))

    (let ((realpart (ddot nxm store-x 2 store-y 1))
	  (imagpart (with-vector-data-addresses ((addr-x store-x)
						  (addr-y store-y))
			  (incf-sap :double-float addr-x)
			  (blas::fortran-ddot nxm addr-x 2 addr-y 1))))

      (declare (type complex-matrix-element-type realpart imagpart))

      (if conjugate-p
	  (setq imagpart (- imagpart)))

      #+:complex-arg-implies-complex-result
      (complex realpart  imagpart)
      #-:complex-arg-implies-comples-result
      (if (zerop imagpart)
	  realpart
	(complex realpart imagpart))
      )))

#+:allegro
(defmethod dot ((x complex-matrix) (y real-matrix) &optional (conjugate-p t))
  (let ((nxm (number-of-elements x)))
    (declare (type fixnum nxm))

    (let ((realpart 0.0d0)
	  (imagpart 0.0d0))
      (declare (type complex-matrix-element-type realpart imagpart))

      (dotimes (i nxm)
        (declare (type fixnum i))
        (let ((x-elt (matrix-ref x i))
	      (y-elt (matrix-ref y i)))
	  (incf realpart (+ y-elt (realpart x-elt)))
	  (incf imagpart (+ y-elt (imagpart x-elt)))))

      (if conjugate-p
	  (setq imagpart (- imagpart)))

      #+:complex-arg-implies-complex-result
      (complex realpart  imagpart)
      #-:complex-arg-implies-comples-result
      (if (zerop imagpart)
	  realpart
	(complex realpart imagpart))
      )))

(defmethod dot ((x complex-matrix) (y complex-matrix) &optional (conjugate-p t))
  (let* ((nxm (number-of-elements x))
	 (store-x (store x))
	 (store-y (store y))
	 (dot (if conjugate-p
		  (zdotc nxm store-x 1 store-y 1)
		  (zdotu nxm store-x 1 store-y 1))))
    
    #-:complex-arg-implies-complex-result
    (if (zerop (imagpart dot))
	(realpart dot)
	dot)
    #+:complex-arg-implies-complex-result
    dot))
