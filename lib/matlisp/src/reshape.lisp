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
;;; $Id: reshape.lisp,v 1.8 2001/10/26 13:35:00 rtoy Exp $
;;;
;;; $Log: reshape.lisp,v $
;;; Revision 1.8  2001/10/26 13:35:00  rtoy
;;; o Fix typo in reshape! for complex matrices that caused totally bogus
;;;   results.  (Noted by M. Koerber.)
;;; o Rename the n/m variables to something longer so we don't get
;;;   confused on what n/m is and so it's easier to distinguish n from m.
;;;
;;; Revision 1.7  2001/06/22 12:52:41  rtoy
;;; Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;; instead of using the error-prone make-array.
;;;
;;; Revision 1.6  2001/05/07 15:47:09  rtoy
;;; In the Great Renaming of slots n,m to nrows,ncols, I missed a few
;;; spots.  Fix them.
;;;
;;; Revision 1.5  2001/04/18 15:56:26  rtoy
;;; Fix stupid typo:  :mcols -> :ncols.
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

#+nil (export '(reshape!
	  reshape))

(defgeneric reshape (mat n m)
  (:documentation 
"
  Syntax
  ======
  (RESHAPE a n m)

  Purpose
  =======
  Construct an NxM matrix from the elements of A, which are ordered
  as column major (Fortran compatability).
  Elements are discarded if needed and new elements are set to zero,
  if needed.  The original matrix is not modified.
"))

(defgeneric reshape! (mat n m)
  (:documentation 
"
  Syntax
  ======
  (RESHAPE! a n m)

  Purpose
  =======
  Construct an NxM matrix from the elements of A, which are ordered
  as column major (Fortran compatability).
  Elements are discarded if needed and new elements are set to zero,
  if needed.  The original matrix is modified in place and returned.
"))

(defmethod reshape ((mat real-matrix) new-nrows new-ncols)

  (declare (type fixnum new-nrows new-ncols))
  (let* ((old-size (number-of-elements mat))
	 (new-size (* new-nrows new-ncols))
	 (new-store (allocate-real-store new-size)))
    (declare (fixnum old-size new-size)
	     (type (real-matrix-store-type (*)) new-store))

    (dcopy (min old-size new-size) (store mat) 1 new-store 1)

    (make-instance 'real-matrix :nrows new-nrows :ncols new-ncols :store new-store)))


(defmethod reshape! ((mat real-matrix) new-nrows new-ncols)
  (declare (fixnum new-nrows new-ncols))
  (let ((old-size (number-of-elements mat))
	(new-size (* new-nrows new-ncols)))
    (declare (fixnum old-size new-size))
    (when (< old-size new-size)
      ;; We need to allocate new space to hold the result since
      ;; it's bigger than the old size.  Allocate it and copy the
      ;; elements over.
      (let ((new-store (allocate-real-store new-size)))
	(declare (type (real-matrix-store-type (*)) new-store))

	(dcopy new-size (store mat) 1 new-store 1)
	(setf (slot-value mat 'store) new-store)))
    (setf (nrows mat) new-nrows)
    (setf (ncols mat) new-ncols)
    (setf (number-of-elements mat) new-size)
    (setf (store-size mat) new-size)
    mat))

(defmethod reshape ((mat complex-matrix) new-nrows new-ncols)
  (declare (fixnum new-nrows new-ncols))
  (let* ((old-size (number-of-elements mat))
	 (new-size (* new-nrows new-ncols))
	 (new-store (allocate-complex-store new-size)))
    (declare (fixnum old-size new-size)
	     (type (complex-matrix-store-type (*)) new-store))

    (zcopy (min old-size new-size) (store mat) 1 new-store 1)
    (make-instance 'complex-matrix :nrows new-nrows :ncols new-ncols :store new-store)))


(defmethod reshape! ((mat complex-matrix) new-nrows new-ncols)
  (declare (fixnum new-nrows new-ncols))
  (let ((old-size (number-of-elements mat))
	(new-size (* new-nrows new-ncols)))
    (declare (fixnum old-size new-size))
    (when (< old-size new-size)
      ;; We need to allocate new space to hold the result since
      ;; it's bigger than the old size.  Allocate it and copy the
      ;; elements over.
      (let ((new-store (allocate-complex-store new-size)))
	(declare (type (complex-matrix-store-type (*)) new-store))

	(zcopy new-size (store mat) 1 new-store 1)
	(setf (slot-value mat 'store) new-store)))

    (setf (nrows mat) new-nrows)
    (setf (ncols mat) new-ncols)
    (setf (number-of-elements mat) new-size)
    (setf (store-size mat) (* 2 new-size))
    mat))
