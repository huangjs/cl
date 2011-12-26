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
;;; $Id: special.lisp,v 1.5 2001/06/22 12:52:41 rtoy Exp $
;;;
;;; $Log: special.lisp,v $
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

#+nil (export '(eye
	  ones
	  zeros
	  rand))

(defun eye (n &optional (m n))
  "
  Syntax
  ======
  (EYE n [m])

  Purpose
  =======
  If M is the same as N (the default), then creates
  an NxN identity matrix.  If M is different from N
  the creates an NxM matrix with 1's on the diagonal
  and 0's elsewhere.

  See ONES, ZEROS
"
  (unless (and (typep m '(integer 1))
	       (typep n '(integer 1)))
    (error "the number of rows (~d) and columns (~d) must be positive integers" n m))

  (let ((result (make-real-matrix-dim n m)))
    (setf (aref *1x1-real-array* 0) 1.0d0)
    (dcopy (min n m) *1x1-real-array* 0 (store result) (1+ n))
    result))

(defun zeros (n &optional (m n))
  "
  Syntax
  ======
  (ZEROS n [m])

  Purpose
  =======
  Creates an NxM (default NxN) real matrix filled with zeros.

  See EYE, ONES
"
  
  (unless (and (typep m '(integer 1))
	       (typep n '(integer 1)))
    (error "the number of rows (~d) and columns (~d) must be positive integers" n m))
  (make-real-matrix-dim n m))
  
(defun ones (n &optional (m n))
  "
  Syntax
  ======
  (ONES n [m])

  Purpose
  =======
  Creates an NxM (default NxN) real matrix filled with ones

  See EYE, ZEROS
"
  (unless (and (typep m '(integer 1))
	       (typep n '(integer 1)))
    (error "the number of rows (~d) and columns (~d) must be positive integers" n m))
  (make-real-matrix-dim n m 1.0d0))

(defun rand (n &optional (m n) (state *random-state*))
  "
  Syntax
  ======
  (RAND n [m] [state])

  Purpose
  =======
  Creates an NxM (default NxN) real matrix filled with uniformly 
  distributed pseudo-random numbers between 0 and 1.  
  STATE (default *RANDOM-STATE*), if given, should be a RANDOM-STATE.

  See RANDOM, INIT-RANDOM-STATE, MAKE-RANDOM-STATE, *RANDOM-STATE*
"
  (unless (and (typep m '(integer 1))
	       (typep n '(integer 1)))
    (error "the number of rows (~d) and columns (~d) must be positive integers" n m))
  (unless (typep state 'random-state)
    (error "STATE must be a RANDOM-STATE, not a ~A" (type-of state)))

  (locally (declare (type fixnum n m))
    (let* ((size (* n m))
	   (store (allocate-real-store size))
	   (unity #.(coerce 1 'real-matrix-element-type)))
	     
      (declare (fixnum size)
	       (type (real-matrix-store-type (*)) store))
      (dotimes (k size)
	(declare (fixnum k))
	(setf (aref store k) (random unity state)))
	     
      (make-instance 'real-matrix :nrows n :ncols m :store store))))

