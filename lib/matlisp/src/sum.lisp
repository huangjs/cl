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
;;; $Id: sum.lisp,v 1.4 2000/07/11 18:02:03 simsek Exp $
;;;
;;; $Log: sum.lisp,v $
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

#+nil (export '(sum))

(defgeneric sum (a)
  (:documentation
   "
  Syntax
  ======
  (SUM a)

  Purpose
  =======
  If A is a vector, computes a1+a2+...+an.
  If A is an NxM matrix, computes the Nx1 vector

              [ A11 + A12 + ... + A1M ]
              [ A12 + A22 + ... + A2M ]
              [      :                ]
              [ AN1 + AN2 + ... + ANM ]   
"))

(defmethod sum ((x number))
  x)

(defmethod sum ((a real-matrix))
  (if (row-or-col-vector-p a)
      (let ((result 0.0d0)
	    (store (store a))
	    (nxm (number-of-elements a)))
	(declare (type fixnum nxm)
		 (type real-matrix-element-type result)
		 (type (real-matrix-store-type (*)) store))
	(dotimes (i nxm)
	 (declare (type fixnum i))
           (incf result (aref store i)))
	result)
    
    (let* ((n (nrows a))
	   (m (ncols a))
	   (result (make-real-matrix-dim n 1))
	   (store-a (store a))
	   (store-result (store result)))
      (declare (type fixnum n m)
	       (type (real-matrix-store-type (*)) store-a store-result))
      (dotimes (i n)
	(declare (type fixnum i))
	(setf (aref store-result i)
	      (let ((val 0.0d0))
		(declare (type real-matrix-element-type val))
		(dotimes (j m val)
		  (declare (type fixnum j))
		  (incf val (aref store-a (fortran-matrix-indexing  i j n)))))))
      result)))

(defmethod sum ((a complex-matrix))
  (if (row-or-col-vector-p a)
      (let ((realpart 0.0d0)
	    (imagpart 0.0d0)
	    (store (store a))
	    (nxm (number-of-elements a)))
	(declare (type fixnum nxm)
		 (type complex-matrix-element-type imagpart realpart)
		 (type (complex-matrix-store-type (*)) store))
	(dotimes (i nxm)
	 (declare (type fixnum i))
	   (let ((k (* 2 i)))
	     (declare (type fixnum k))
	     (incf realpart (aref store k))
	     (incf imagpart (aref store (1+ k)))))
	#+:complex-arg-implies-complex-result
	(complex realpart imagpart)
	#-:complex-arg-implies-complex-result
	(if (zerop imagpart)
	    realpart
	  (complex realpart imagpart))

	)
    
    (let* ((n (nrows a))
	   (m (ncols a))
	   (result (make-complex-matrix-dim n 1))
	   (store-a (store a))
	   (store-result (store result)))
      (declare (type fixnum n m)
	       (type (complex-matrix-store-type (*)) store-a store-result))
      (dotimes (i n)
	(declare (type fixnum i))
	(let ((realpart 0.0d0)
	      (imagpart 0.0d0))
	  (declare (type complex-matrix-element-type realpart imagpart))
	  (dotimes (j m)
	     (declare (type fixnum j))
	     (let ((k (fortran-complex-matrix-indexing i j n)))
	       (incf realpart (aref store-a k))
	       (incf imagpart (aref store-a (1+ k)))))
	  
	  (setf (aref store-result (* 2 i)) realpart)
	  (setf (aref store-result (1+ (* 2 i))) imagpart)))

      result)))
