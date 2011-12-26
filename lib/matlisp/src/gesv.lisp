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
;;; $Id: gesv.lisp,v 1.4 2000/07/11 18:02:03 simsek Exp $
;;;
;;; $Log: gesv.lisp,v $
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

#+:nil (use-package "BLAS")
#+:nil (use-package "LAPACK")
#+:nil (use-package "FORTRAN-FFI-ACCESSORS")

#+:nil (export '(gesv!
		 gesv))


#+:pre-allocate-workspaces
(defvar *ipiv* (make-array *ipiv-size* :element-type '(unsigned-byte 32))) 

(defgeneric gesv! (a  b &key ipiv)
  (:documentation
   "
  Syntax
  ======
  (GESV! a b [:IPIV ipiv])

  Purpose
  =======
  Computes the solution to the system of linear equations

                    A * X = B

  where A is an NxN matrix and X,B are NxM matrices.

  If the optional argument IPIV is provided it must
  be a (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)) of dimension >= N.

  IPIV is filled with the pivot indices that define the permutation
  matrix P:
           
        row i of the matrix was interchanged with row IPIV(i).
  
  where   A = P * L * U in the LU decomposition with partial pivoting,
  and L is unit lower triangular, U is upper triangular.

  If IPIV is not provided, it is allocated by GESV.

  Return Values
  =============
  [1] The NxM matrix X. (overwriting B)
  [2] IPIV
  [3] The factors L and U from the factorization A = P*L*U  where the 
      unit diagonal elements of L are not stored. (overwriting A)
  [4] INFO = T: successful
             i:  U(i,i) is exactly zero.  The LU factorization
                 used in the computation has been completed, 
                 but the factor U is exactly singular.
                 Solution could not be computed.
"))

(defgeneric gesv (a b)
  (:documentation
 "
  Sytnax
  ======
  (GESV a b)

  Purpose
  =======
  Same as GESV! except that A,B are not overwritten.
"))

(defmethod gesv! :before ((a standard-matrix) (b standard-matrix) &key ipiv)
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b)))
    (if (not (= n-a m-a n-b))
	(error "dimensions of A,B given to GESV do not match"))
    (if ipiv
	(progn
	  (check-type ipiv (simple-array (unsigned-byte 32) (*)))
	  (if (< (length ipiv) n-a)
	      (error "argument IPIV given to GESV! must dimension >= N,
where NxN is the dimension of argument A given to GESV!"))))))


(defmethod gesv! ((a real-matrix) (b real-matrix) &key ipiv)

  (let* ((n (nrows a))
	 (m (ncols b))
	 (ipiv #+:pre-allocate-workspaces
	       (or ipiv *ipiv*)
	       #-:pre-allocate-workspaces
	       (or ipiv (make-array n :element-type '(unsigned-byte 32)))))

    (declare (type fixnum n m)
	     (type (simple-array (unsigned-byte 32) (*)) ipiv))

    (multiple-value-bind (factors 
			  ipiv
			  x
			  info)
	(dgesv n
	       m
	       (store a)
	       n
	       ipiv
	       (store b)
	       n
	       0)
	
	(values 
	 (make-instance 'real-matrix :nrows n :ncols m :store x) 
	 ipiv 
	 (make-instance 'real-matrix :nrows n :ncols n :store factors) 
	 (if (zerop info)
	     t
	   info)))))

(defmethod gesv! ((a complex-matrix) (b complex-matrix) &key ipiv)

  (let* ((n (nrows a))
	 (m (ncols b))
	 (ipiv #+:pre-allocate-workspaces
	       (or ipiv *ipiv*)
	       #-:pre-allocate-workspaces
	       (or ipiv (make-array n :element-type '(unsigned-byte 32)))))

    (declare (type fixnum n m)
	     (type (simple-array (unsigned-byte 32) (*)) ipiv))

    (multiple-value-bind (factors 
			  ipiv
			  x
			  info)
	(zgesv n
	       m
	       (store a)
	       n
	       ipiv
	       (store b)
	       n
	       0)
	
	(values 
	 (make-instance 'complex-matrix :nrows n :ncols m :store x) 
	 ipiv 
	 (make-instance 'complex-matrix :nrows n :ncols n :store factors) 
	 (if (zerop info)
	     t
	   info)))))

(defmethod gesv! ((a standard-matrix) (b standard-matrix) &key ipiv)
  (let ((a (typecase a
	     (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
	     (complex-matrix a)
	     (t (error "argument A given to GESV! is not a REAL-MATRIX or COMPLEX-MATRIX"))))
	(b (typecase b
	     (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
	     (complex-matrix b)
	     (t (error "argument B given to GESV! is not a REAL-MATRIX or COMPLEX-MATRIX")))))

    (gesv! a b :ipiv ipiv)))

(defmethod gesv :before ((a standard-matrix) (b standard-matrix))
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b)))
    (if (not (= n-a m-a n-b))
	(error "dimensions of A,B given to GESV do not match"))))

(defmethod gesv ((a real-matrix) (b real-matrix))
  (gesv! (copy a) (copy b)))

(defmethod gesv ((a complex-matrix) (b complex-matrix))
  (gesv! (copy a) (copy b)))

(defmethod gesv ((a standard-matrix) (b standard-matrix))
  (let ((a (typecase a
	     (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
	     (complex-matrix (copy a))
	     (t (error "argument A given to GESV! is not a REAL-MATRIX or COMPLEX-MATRIX"))))
	(b (typecase b
	     (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
	     (complex-matrix (copy b))
	     (t (error "argument B given to GESV! is not a REAL-MATRIX or COMPLEX-MATRIX")))))

    (gesv! a b)))
