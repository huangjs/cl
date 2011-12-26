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
;;; $Id: msqrt.lisp,v 1.5 2000/07/11 18:02:03 simsek Exp $
;;;
;;; $Log: msqrt.lisp,v $
;;; Revision 1.5  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.4  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.3  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.2  2000/05/05 21:39:29  simsek
;;; o Fixed some documentation bugs
;;;
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (export '(msqrt))

(defgeneric msqrt (matrix)
  (:documentation
     "
 Syntax
 ======
 (MSQRT a)

 Purpose:
 ========
 Given a matrix A, which must be square, computes

           1/2          1/2    -1
          A    =  V  * E    * V

 where V is the matrix of right eigenvectors and
 E is (presumably) a diagonal matrix of eigenvalues.

 This makes sense especially when A is symmetric
 and positive definite.
"))

(defmethod msqrt :before ((a standard-matrix))
  (if (not (square-matrix-p a))
      (error "argument A given to MSQRT must be a square matrix")))

(defun %negative-p (mat)
  (let ((n (nrows mat))
	(m (ncols mat)))
    (declare (type fixnum n m))
    (dotimes (i n)
      (declare (type fixnum i))
      (dotimes (j m)
	(declare (type fixnum j))
	(let ((val (matrix-ref mat i j)))
	  (declare (type real-matrix-element-type val))
	  (if (< val 0.0d0)
	      (return-from %negative-p t)))))
    nil))

(defmethod msqrt ((a real-matrix))
  (multiple-value-bind (v e info)
      (geev a t)
    (if info
	(if (%negative-p e)
	    (progn
	      (setq e (map-matrix! #'sqrt (copy! e
						 (make-complex-matrix-dim (nrows e) (ncols e)))))
	      (m*! v (m* e (m/ v))))
	  (m*! v (m* (map-matrix #'sqrt e) (m/ v))))
      (error "could not compute MSQRT of A, could not compute eigenvalues"))))

(defmethod msqrt ((a standard-matrix))
  (multiple-value-bind (v e info)
      (geev a t)
    (if info
	(m*! v (m* (map-matrix! #'sqrt e) (m/ v)))
      (error "could not compute MSQRT of A, could not compute eigenvalues"))))
