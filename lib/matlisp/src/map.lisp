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
;;; $Id: map.lisp,v 1.8 2001/04/29 15:51:37 rtoy Exp $
;;;
;;; $Log: map.lisp,v $
;;; Revision 1.8  2001/04/29 15:51:37  rtoy
;;; o Use declaim instead of proclaim for clog10
;;; o Divide by a real instead of a complex (faster and more accurate).
;;;
;;; Revision 1.7  2001/04/28 22:36:39  rtoy
;;; Rename dlog10 to real-log10 (to prevent collision with F2CL version).
;;;
;;; Revision 1.6  2000/10/05 17:36:18  simsek
;;; o Fixed a typo
;;;
;;; Revision 1.5  2000/10/04 23:55:29  simsek
;;; o Hacked methods specializing to functions
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

#+nil (export '(map-matrix!
		map-matrix))

(defun matrix-map (func matrix)
  (map-matrix func matrix))

(defun matrix-map! (func matrix)
  (map-matrix! func matrix))

(defgeneric map-matrix! (func  matrix)
  (:documentation
   "
  Syntax
  ======
  (MAP-MATRIX! func matrix)

  Purpose
  =======
  Given a function FUNC and a matrix MATRIX,
  applies FUNC to each element of matrix
  and stores the result in MATRIX.

  Returns MATRIX.
"))


(defgeneric map-matrix (func matrix)
  (:documentation
   "
  Syntax
  ======
  (MAP-MATRIX func matrix)

  Purpose
  =======
  Same as MAP-MATRIX! except that the 
  result is stored in a new matrix.

  The new matrix is returned.
"))

	     
;; can we specialize to a function of 1 arg here?
#|
;;; CMUCL 18b has problems with specializing methods
;;; to functions (this should have been fixed in 
;;; later releases of CMUCL, but I'll check for
;;; type manually so that we can support 18b)

(defmethod map-matrix! ((func function) (mat real-matrix))
  (let ((nxm (number-of-elements mat))
	(store (store mat)))
    (declare (type fixnum nxm)
	     (type (real-matrix-store-type (*)) store))
    (dotimes (k nxm mat)
      (declare (type fixnum k))
      (setf (aref store k) (funcall func (aref store k))))))

(defmethod map-matrix ((func function) (mat real-matrix))
  (let ((res (copy mat)))
    (map-matrix! func res)))
|#

(defmethod map-matrix! (func (mat real-matrix))
  (unless (functionp func)
    (error "argument ~a given to ~a should have been a ~a"
	   func
	   'map-matrix!
	   'function))
  (let ((nxm (number-of-elements mat))
	(store (store mat)))
    (declare (type fixnum nxm)
	     (type (real-matrix-store-type (*)) store))
    (dotimes (k nxm mat)
      (declare (type fixnum k))
      (setf (aref store k) (funcall func (aref store k))))))

(defmethod map-matrix (func (mat real-matrix))
  (unless (functionp func)
    (error "argument ~a given to ~a should have been a ~a"
	   func
	   'map-matrix!
	   'function))
  (let ((res (copy mat)))
    (map-matrix! func res)))


;; For now, we define simple basic transcendental functions for
;; matrices.  For simplicity, we use the standard Lisp versions of
;; these functions.  If speed is required, we may replace these with
;; faster C versions.

(defmacro make-generic-mapper (func-sym &optional real-func)
  (declare (ignore real-func))
  (let ((mapper-name (concatenate 'string "M" (symbol-name `,func-sym)))
	(lisp-name (symbol-name `,func-sym)))
  `(defgeneric ,(intern mapper-name) (mat)
      (:documentation
 ,(concatenate 'string
" 
  Syntax
  ======
  ("  mapper-name " matrix)

  Purpose
  =======
  Applies the function " lisp-name " to each element
  of MATRIX, stores the result in a new matrix and returns
  the new matrix.
")))))

(export '(msin mcos mtan
	  masin macos matan
	  mexp mlog mlog10
	  msinh mcosh mtanh
	  masinh macosh matanh))

(make-generic-mapper sin)
(make-generic-mapper cos)
(make-generic-mapper tan)
(make-generic-mapper asin)
(make-generic-mapper acos)
(make-generic-mapper atan)
;; Note: this doesn't work well,
;; and MSQRT has a different meaning
;; for MATLAB/OCTAVE compatability
;;(make-generic-mapper sqrt)
(make-generic-mapper exp)
(make-generic-mapper log)
(make-generic-mapper log10 dlog10)
(make-generic-mapper sinh)
(make-generic-mapper cosh)
(make-generic-mapper tanh)
(make-generic-mapper asinh)
(make-generic-mapper acosh)
(make-generic-mapper atanh)


(defmacro make-real-mapper (func-sym &optional real-func)
  `(defmethod ,(intern (concatenate 'string "M" (symbol-name func-sym)))
      ((mat real-matrix))
    (let ((result (copy mat)))
	  (map-matrix! #',(or real-func func-sym) result))))

(declaim (inline real-log10))
(defun real-log10 (x)
  (declare (type double-float x))
  (/ (log x) #.(log 10.0d0)))

(make-real-mapper sin)
(make-real-mapper cos)
(make-real-mapper tan)
(make-real-mapper asin)
(make-real-mapper acos)
(make-real-mapper atan)
;; Note: this doesn't work well,
;; and MSQRT has a different meaning
;; for MATLAB/OCTAVE compatability
;;(make-real-mapper sqrt)
(make-real-mapper exp)
(make-real-mapper log)
(make-real-mapper log10 real-log10)
(make-real-mapper sinh)
(make-real-mapper cosh)
(make-real-mapper tanh)
(make-real-mapper asinh)
(make-real-mapper acosh)
(make-real-mapper atanh)

#|
;;; CMUCL 18b has problems with specializing methods
;;; to functions (this should have been fixed in 
;;; later releases of CMUCL, but I'll check for
;;; type manually so that we can support 18b)
(defmethod map-matrix! ((func function) (mat complex-matrix))
  (let ((nxm (number-of-elements mat)))
    (declare (type fixnum nxm))
    (dotimes (k nxm mat)
      (declare (type fixnum k))
      (setf (matrix-ref mat k) (funcall func (matrix-ref mat k))))))

(defmethod map-matrix ((func function) (mat complex-matrix))
  (let ((res (copy mat)))
    (map-matrix! func res)))
|#

(defmethod map-matrix! (func (mat complex-matrix))
  (unless (functionp func)
    (error "argument ~a given to ~a should have been a ~a"
	   func
	   'map-matrix!
	   'function))
  (let ((nxm (number-of-elements mat)))
    (declare (type fixnum nxm))
    (dotimes (k nxm mat)
      (declare (type fixnum k))
      (setf (matrix-ref mat k) (funcall func (matrix-ref mat k))))))

(defmethod map-matrix (func (mat complex-matrix))
  (unless (functionp func)
    (error "argument ~a given to ~a should have been a ~a"
	   func
	   'map-matrix!
	   'function))
  (let ((res (copy mat)))
    (map-matrix! func res)))

#|
(macrolet ((frob (func)
	     `(defmethod ,(intern (concatenate 'string "matrix-" (symbol-name func))) ((mat real-matrix))
	       (map-matrix! mat #',func))))
  (dolist (f '(sin cos tan asin acos atan exp log))
    (frob f)))
|#

(defmacro make-complex-mapper (func-sym &optional real-func)
  `(defmethod ,(intern (concatenate 'string "M" (symbol-name func-sym)))
      ((mat complex-matrix))
    (let ((result (copy mat)))
	  (map-matrix! #',(or real-func func-sym) result))))

(declaim (inline clog10))
(defun clog10 (x)
  (declare (type (complex double-float) x))
  (/ (log x) #.(log 10.0d0)))

;; For now, we define simple basic transcendental functions for
;; matrices.  For simplicity, we use the standard Lisp versions of
;; these functions.  If speed is required, we may replace these with
;; faster C versions.

(make-complex-mapper sin)
(make-complex-mapper cos)
(make-complex-mapper tan)
(make-complex-mapper asin)
(make-complex-mapper acos)
(make-complex-mapper atan)
;; Note: this doesn't work well,
;; and MSQRT has a different meaning
;; for MATLAB/OCTAVE compatability
;;(make-complex-mapper sqrt)
(make-complex-mapper exp)
(make-complex-mapper log)
(make-complex-mapper log10 clog10)
(make-complex-mapper sinh)
(make-complex-mapper cosh)
(make-complex-mapper tanh)
(make-complex-mapper asinh)
(make-complex-mapper acosh)
(make-complex-mapper atanh)











