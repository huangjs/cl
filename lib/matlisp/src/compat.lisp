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
;;; $Id: compat.lisp,v 1.4 2001/07/23 17:38:48 rtoy Exp $
;;;
;;; $Log: compat.lisp,v $
;;; Revision 1.4  2001/07/23 17:38:48  rtoy
;;; Oops.  mref forgot the matrix arg.
;;;
;;; Revision 1.3  2001/02/21 07:58:09  simsek
;;; o Fixed typo in mref
;;;
;;; Revision 1.2  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

(export '(number-of-rows
	  number-of-cols
	  number-of-elems
	  float-matrix-element-type
	  float-matrix-array-type
	  complex-matrix-array-type
	  float-matrix
	  make-float-matrix-dim
	  make-float-matrix-array
	  make-float-matrix-seq
	  make-float-matrix-seq-of-seq
	  make-float-matrix-sequence
	  make-float-matrix
	  mref))

(declaim (inline number-of-rows
		 number-of-cols
		 number-of-elems
		 make-float-matrix-dim
		 make-float-matrix-array
		 make-float-matrix-seq
		 make-float-matrix-seq-of-seq
		 make-float-matrix-sequence
		 make-float-matrix
		 mref))


(defun number-of-rows (mat)
  "Calls NROWS on its arguments"
  (nrows mat))

(defun number-of-cols (mat)
  "Calls NCOLS on its arguments"
  (ncols mat))

(defun number-of-elems (mat)
  "Calls NUMBER-OF-ELEMENTS on its arguments"
  (number-of-elements mat))

(deftype float-matrix-element-type ()
  "Defines the same type as REAL-MATRIX-ELEMENT-TYPE"
  'real-matrix-element-type)

(deftype float-matrix-array-type (size) 
  "Defines the same type as (REAL-MATRIX-STORE-TYPE (*))"
  `(simple-array double-float ,size))

(deftype complex-matrix-array-type (size) 
  "Defines the same type as (COMPLEX-MATRIX-STORE-TYPE (*))"
  `(simple-array double-float ,size))

(deftype float-matrix ()
  "Defines the same type as REAL-MATRIX"
  'real-matrix)

(defun make-float-matrix-dim (&rest args)
  "Calls MAKE-REAL-MATRIX-DIM on its arguments"
  (apply #'make-real-matrix-dim args))

(defun make-float-matrix-array (&rest args)
  "Calls MAKE-REAL-MATRIX-ARRAY on its arguments"
  (apply #'make-real-matrix-array args))

(defun make-float-matrix-seq (&rest args)
  "Calls MAKE-REAL-MATRIX-SEQ on its arguments"
  (apply #'make-real-matrix-seq args))

(defun make-float-matrix-seq-of-seq (&rest args)
  "Calls MAKE-REAL-MATRIX-SEQ-OF-SEQ on its arguments"
  (apply #'make-real-matrix-seq-of-seq args))

(defun make-float-matrix-sequence (&rest args)
  "Calls MAKE-REAL-MATRIX-SEQUENCE on its arguments"
  (apply #'make-real-matrix-sequence args))

(defun make-float-matrix (&rest args)
  "Calls MAKE-REAL-MATRIX on its arguments"
  (apply #'make-real-matrix args))

(defun join-matrix (&rest args)
  "Calls JOIN on its arguments"
  (apply #'join args))

(defun mref (m i &optional j)
  "Same as MATRIX-REF"
  (if j
    (matrix-ref m i j)
   (matrix-ref m i)))
