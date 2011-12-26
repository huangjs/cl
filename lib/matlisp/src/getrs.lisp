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
;;; Written by Nicolas Neuss (analogous to gesv.lisp by R. Toy)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: getrs.lisp,v 1.1 2002/09/30 18:28:27 simsek Exp $
;;;
;;; $Log: getrs.lisp,v $
;;; Revision 1.1  2002/09/30 18:28:27  simsek
;;; o Added changes by N.Neuss for getrs functions
;;;
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric getrs! (a ipiv b &key trans)
  (:documentation
   "
  Syntax
  ======
  (GETRS! a ipiv b [:trans :N])

  Purpose
  =======
  Solves a system of linear equations
      A * X = B  or  A' * X = B
  with a general N-by-N matrix A using the LU factorization computed
  by GETRF.  A and IPIV are the results from GETRF, TRANS specifies
  the form of the system of equations: 
           = 'N':  A * X = B  (No transpose)
           = 'T':  A'* X = B  (Transpose)
           = 'C':  A'* X = B  (Conjugate transpose)

  Return Values
  =============
  [1] The NxM matrix X. (overwriting B)
  [4] INFO = T: successful
             i:  U(i,i) is exactly zero.  The LU factorization
                 used in the computation has been completed, 
                 but the factor U is exactly singular.
                 Solution could not be computed.
"))

(defgeneric getrs (a ipiv b &key trans)
  (:documentation
 "
  Sytnax
  ======
  (GETRS a ipiv b [:trans :N])

  Purpose
  =======
  Same as GETRS! except that B is not overwritten.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Method definitions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod getrs! :before ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (declare (ignore trans))
  (let ((n-a (nrows a))
        (m-a (ncols a))
        (n-b (nrows b)))
    (if (not (= n-a m-a n-b))
        (error "Dimensions of A,B given to GETRS! do not match"))
    ;;(check-type ipiv (simple-array (unsigned-byte 32) (*)))
    (if (< (length ipiv) n-a)
        (error "The argument IPIV given to GETRS! must have dimension >= N,
where NxN is the dimension of the argument A given to GETRS!"))))

(defmethod getrs! ((a real-matrix) ipiv (b real-matrix) &key trans)
  (let* ((n (nrows a))
         (m (ncols b)))

    (declare (type fixnum n m)
             (type (simple-array (unsigned-byte 32) (*)) ipiv))

    (multiple-value-bind (x info)
        (dgetrs (case trans
                  (:C "C")
                  (:T "T")
                  (t "N"))
                n
                m
                (store a)
                n
                ipiv
                (store b)
                n
                0)
        
        (values 
         (make-instance 'real-matrix :nrows n :ncols m :store x)
         (if (zerop info)
             t
           info)))))

(defmethod getrs! ((a complex-matrix) ipiv (b complex-matrix) &key trans)

  (let* ((n (nrows a))
         (m (ncols b)))

    (declare (type fixnum n m)
             (type (simple-array (unsigned-byte 32) (*)) ipiv))

    (multiple-value-bind (x info)
        (zgetrs (case trans
                  (:C "C")
                  (:T "T")
                  (t "N"))
                n
                m
                (store a)
                n
                ipiv
                (store b)
                n
                0)
        
        (values 
         (make-instance 'complex-matrix :nrows n :ncols m :store x) 
         (if (zerop info)
             t
           info)))))

(defmethod getrs! ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (let ((a (typecase a
             (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
             (complex-matrix a)
             (t (error "argument A given to GETRS! is not a REAL-MATRIX or COMPLEX-MATRIX"))))
        (b (typecase b
             (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
             (complex-matrix b)
             (t (error "argument B given to GETRS! is not a REAL-MATRIX or COMPLEX-MATRIX")))))
    (getrs! a ipiv b :trans trans)))

(defmethod getrs :before ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (declare (ignore trans))
  (if (not (= (nrows a) (ncols a) (nrows b) (length ipiv)))
      (error "dimensions of A,B,ipiv given to GETRS do not match")))

(defmethod getrs ((a real-matrix) ipiv (b real-matrix) &key trans)
  (getrs! a ipiv (copy b) :trans trans))

(defmethod getrs ((a complex-matrix) ipiv (b complex-matrix) &key trans)
  (getrs! a ipiv (copy b) :trans trans))

(defmethod getrs ((a standard-matrix) ipiv (b standard-matrix) &key trans)
  (let ((a (typecase a
             (real-matrix (copy! a (make-complex-matrix-dim (nrows a) (ncols a))))
             (complex-matrix (copy a))
             (t (error "argument A given to GETRS is not a REAL-MATRIX or COMPLEX-MATRIX"))))
        (b (typecase b
             (real-matrix (copy! b (make-complex-matrix-dim (nrows b) (ncols b))))
             (complex-matrix (copy b))
             (t (error "argument B given to GETRS is not a REAL-MATRIX or COMPLEX-MATRIX")))))
    (getrs! a ipiv b :trans trans)))

