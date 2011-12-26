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
;;; $Id: svd.lisp,v 1.5 2001/06/22 12:52:41 rtoy Exp $
;;;
;;; $Log: svd.lisp,v $
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
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

#+nil (export '(svd))


(defgeneric svd (a &optional job)
  (:documentation
  "
  Syntax
  ======
  (SVD a [job])

  Purpose
  =======
  Computes the singular value decomposition (SVD) of the 
  NxM matrix A. The SVD of A is given by:

                 A = U * SIGMA * V'

  where, taking p = min(n,m):

          U = [u1 u2 ... un] an NxN othogonal matrix
          
               [s1  0  0  ... 0]
      SIGMA =  [0  s2  0  ... 0]  if N < M
               [:   :  \\      :]
               [0   0  sp ... 0]
               
               [s1  0  0 ...  0]         
            =  [0  s2  0 ...  0]  if M > N
               [:   :  \\ ...  0]
               [:   :    \\    0]
               [0   0  0 ... sp]
               [0   0  0 ...  0]
               [:   :  :      :]
               [0   0  0 ...  0]

              [v1']
          V = [v2'] an MxM orthogonal matrix
              [ : ]
              [vm']

   The diagonal elements of SIGMA are the singular values of A.
   s1,...,sp are real, non-negative and arranged so that s1 >= s2 >= ... >= sp
   The first p columns of U are the left singular vectors of A.
   The first p rows of V' are the right singular vectors of A.

  Return Values
  =============

  JOB              Return Value
  -------------------------------------------------
  :N (default)    [1] (DIAG SIGMA)     The p diagonal elements
                                       of SIGMA as a column vector.
                  [2] INFO             T if successful, NIL otherwise.

  :A              [1] U
                  [2] SIGMA            The singular value decomposition.
                  [3] V'
                  [4] INFO             T if successful, NIL otherwise.

  :S              [1] Up               The first p columns of U.
                  [2] SIGMAp           The p elements of SIGMA as a 
                                       diagonal pxp matrix. 
                  [3] Vp               The first p rows of V'.
                  [4] INFO             T if successful, NIL otherwise.
  "))

(defmethod svd ((a real-matrix) &optional (job :n))
  (let* ((n (nrows a))
	 (m (ncols a))
	 (p (min n m))
	 (lwork (max (+ (* 3 (min n m)) (max n m))
		     (* 5 (min n m))))
	 (work (allocate-real-store lwork))
	 (a (copy a))
	 (xxx (allocate-real-store 1)))


    (case job
      (:a
       (let ((s (make-real-matrix-dim p 1))
	     (s1 (make-real-matrix-dim n m))
	     (u (make-real-matrix-dim n n))
	     (vt (make-real-matrix-dim m m)))
	 (multiple-value-bind (new-a 
			       new-s 
			       new-u 
			       new-vt 
			       new-work 
			       new-info)
	     (dgesvd "A"        ;; JOBU
		     "A"        ;; JOBVT
		     n          ;; M
		     m          ;; N (unfortunately, LAPACK takes N,M opposite of MATLISP)
		     (store a)  ;; A
		     n          ;; LDA
		     (store s)  ;; S
		     (store u)  ;; U
		     n          ;; LDU
		     (store vt) ;; VT
		     m          ;; LDVT
		     work       ;; WORK
		     lwork      ;; LWORK
		     0)         ;; INFO
	     (declare (ignore new-a new-s new-u new-vt new-work))
	     (setf (diag s1) s)
	     (values u s1 vt (zerop new-info)))))
      (:s
       (let ((s (make-real-matrix-dim p 1))
	     (u (make-real-matrix-dim n p))
	     (vt (make-real-matrix-dim p m)))
	 (multiple-value-bind (new-a 
			       new-s 
			       new-u 
			       new-vt 
			       new-work 
			       new-info)
	     (dgesvd "S"        ;; JOBU
		     "S"        ;; JOBVT
		     n          ;; M
		     m          ;; N (unfortunately, LAPACK takes N,M opposite of MATLISP)
		     (store a)  ;; A
		     n          ;; LDA
		     (store s)  ;; S
		     (store u)  ;; U
		     n          ;; LDU
		     (store vt) ;; VT
		     p          ;; LDVT
		     work       ;; WORK
		     lwork      ;; LWORK
		     0)         ;; INFO
	     (declare (ignore new-a new-s new-u new-vt new-work))
	     (values u (diag s) vt (zerop new-info)))))
      (t ;; (:n n)
       (let ((s (make-real-matrix-dim p 1)))
	 (multiple-value-bind (new-a 
			       new-s 
			       new-u 
			       new-vt 
			       new-work 
			       new-info)
	     (dgesvd "N"        ;; JOBU
		     "N"        ;; JOBVT
		     n          ;; M
		     m          ;; N (unfortunately, LAPACK takes N,M opposite of MATLISP)
		     (store a)  ;; A
		     n          ;; LDA
		     (store s)  ;; S
		     xxx        ;; U
		     1          ;; LDU
		     xxx        ;; VT
		     1          ;; LDVT
		     work       ;; WORK
		     lwork      ;; LWORK
		     0)         ;; INFO
	     (declare (ignore new-a new-s new-u new-vt new-work))
	     (values s (zerop new-info)))))
      )))



(defmethod svd ((a complex-matrix) &optional (job :n))
  (let* ((n (nrows a))
	 (m (ncols a))
	 (p (min n m))
	 (lwork (+ (* 2 (min n m)) (max n m)))
	 (work  (allocate-complex-store lwork))
	 (rwork (allocate-real-store (* 5 (min n m))))
	 (a (copy a))
	 (xxx (allocate-real-store 2)))


    (case job
      (:a
       (let ((s (make-real-matrix-dim p 1))
	     (s1 (make-real-matrix-dim n m))
	     (u (make-complex-matrix-dim n n))
	     (vt (make-complex-matrix-dim m m)))
	 (multiple-value-bind (new-a 
			       new-s 
			       new-u 
			       new-vt 
			       new-work 
			       new-info)
	     (zgesvd "A"        ;; JOBU
		     "A"        ;; JOBVT
		     n          ;; M
		     m          ;; N (unfortunately, LAPACK takes N,M opposite of MATLISP)
		     (store a)  ;; A
		     n          ;; LDA
		     (store s)  ;; S
		     (store u)  ;; U
		     n          ;; LDU
		     (store vt) ;; VT
		     m          ;; LDVT
		     work       ;; WORK
		     lwork      ;; LWORK
		     rwork      ;; RWORK
		     0)         ;; INFO
	     (declare (ignore new-a new-s new-u new-vt new-work))
	     (setf (diag s1) s)
	     (values u s1 vt (zerop new-info)))))
      (:s
       (let ((s (make-real-matrix-dim p 1))
	     (u (make-complex-matrix-dim n p))
	     (vt (make-complex-matrix-dim p m)))
	 (multiple-value-bind (new-a 
			       new-s 
			       new-u 
			       new-vt 
			       new-work 
			       new-info)
	     (zgesvd "S"        ;; JOBU
		     "S"        ;; JOBVT
		     n          ;; M
		     m          ;; N (unfortunately, LAPACK takes N,M opposite of MATLISP)
		     (store a)  ;; A
		     n          ;; LDA
		     (store s)  ;; S
		     (store u)  ;; U
		     n          ;; LDU
		     (store vt) ;; VT
		     p          ;; LDVT
		     work       ;; WORK
		     lwork      ;; LWORK
		     rwork      ;; RWORK
		     0)         ;; INFO
	     (declare (ignore new-a new-s new-u new-vt new-work))
	     (values u (diag s) vt (zerop new-info)))))
      (t ;; (:n n)
       (let ((s (make-real-matrix-dim p 1)))
	 (multiple-value-bind (new-a 
			       new-s 
			       new-u 
			       new-vt 
			       new-work 
			       new-info)
	     (zgesvd "N"        ;; JOBU
		     "N"        ;; JOBVT
		     n          ;; M
		     m          ;; N (unfortunately, LAPACK takes N,M opposite of MATLISP)
		     (store a)  ;; A
		     n          ;; LDA
		     (store s)  ;; S
		     xxx        ;; U
		     1          ;; LDU
		     xxx        ;; VT
		     1          ;; LDVT
		     work       ;; WORK
		     lwork      ;; LWORK
		     rwork      ;; RWORK
		     0)         ;; INFO
	     (declare (ignore new-a new-s new-u new-vt new-work))
	     (values s (zerop new-info)))))
      )))
