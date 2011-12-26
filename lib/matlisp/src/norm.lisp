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
;;; $Id: norm.lisp,v 1.7 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: norm.lisp,v $
;;; Revision 1.7  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.6  2001/06/22 12:52:41  rtoy
;;; Use ALLOCATE-REAL-STORE and ALLOCATE-COMPLEX-STORE to allocate space
;;; instead of using the error-prone make-array.
;;;
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
;;; Revision 1.2  2000/04/14 00:44:11  simsek
;;; o With some Fortran compilers you get a floating point
;;;   exception if you call SVD like this: (SVD a).  But it seems to work like this: (SVD a :a)
;;;   This is a problem with the Fortran compiler and the LAPACK Fortran routine.
;;;   Until a better fix is found, I'm changing the calls to SVD in this file with the :A options.
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

#+nil (export 'norm)

(defgeneric norm (a &optional p)
  (:documentation
  "
  Syntax
  ======
  (NORM a [p])

  Purpose
  =======
  Computes the p-norm of A accroding to the following table.

  A        p                   norm
  -------------------------------------------------------------
  matrix   1                   1-norm  = max abs column sum
   \"      2 (default)         2-norm  = max abs singular value
   \"      :inf                oo-norm = max abs row sum
   \"      :f                  Frobenius norm
                                          /------------------
                                    =   \/  x11+x22+...+xnn

                               where X = A' * A

                                             
  vector   p (default 2)       p-norm 
                                       p    p        p  (1/p) 
                                = ( |x1|+|x2|+...+|xn| )

   \"      :inf                oo-norm = abs max element
   \'      :-inf              -oo-norm = abs min element

  Nicknames
  =========
  The following aliases may be used:

  :inf  <-->  :infinity :i :oo
  :-inf <-->  :-infinity :-i :-oo 
  :f    <-->  :frobenius :frob :fro
  "))


(defmethod norm ((a number) &optional p)
  (declare (ignore p))
  (abs a))

#+(or :cmu :sbcl)
(defmethod norm ((a real-matrix) &optional (p 2))
  (let ((n (nrows a))
	(m (ncols a))
	(nxm (number-of-elements a))
	(store (store a)))
    (declare (type fixnum n m nxm)
	     (type (real-matrix-store-type (*)) store))

    (if (row-or-col-vector-p a)
	(case p
	 ((1 :1) (dasum nxm store 1))
	 ((2 :2) (dnrm2 nxm store 1))
	 ((:oo :inf :i :infinity)
	                         (let ((k (idamax nxm store 1)))
				    (abs (aref store (1- k)))))
	 ((:-oo :-inf :-i :-infinity)
	                           (let ((nrm 0.0d0))
				     (dotimes (k nxm)
				       (declare (type fixnum k))
				       (setq nrm (min nrm (abs (aref store k)))))
				     nrm))
	 (t (if (and (integerp p)
		     (> p 2))
		(let ((nrm 0.0d0))
		  (dotimes (i nxm)
		    (declare (type fixnum i))
		    (incf nrm (expt (abs (aref store i)) (the fixnum p))))
		  (expt nrm (/ p)))
	      (error "don't know how to take a ~a-norm of a vector" p))))
      
      (case p
	((1 :1) (let ((nrm 0.0d0))
	     (dotimes (j m)
	       (declare (type fixnum j))
	       (setq nrm (max nrm 
			      (with-vector-data-addresses ((addr-store store))
				  (incf-sap :double-float addr-store (* j n))
				  (blas::fortran-dasum n addr-store 1)))))
	     nrm))
	((2 :2) (multiple-value-bind (up sigma vp status)
		    (svd a :a)
		  (declare (ignore up vp))
	      (if status
		  (matrix-ref sigma 0)
		(error "SVD did not converge, cannot compute 2-norm of matrix"))))
	((:oo :inf :i :infinity)
	    (let ((nrm 0.0d0))
	      (dotimes (i n)
		(declare (type fixnum i))
		(setq nrm (max nrm
			       (with-vector-data-addresses ((addr-store store))
				  (incf-sap :double-float addr-store i)
				  (blas::fortran-dasum m addr-store n)))))
	      nrm))
	((:f :fro :frob :frobenius)
	         (let ((nrm 0.0d0))
		   (dotimes (j m)
		     (declare (type fixnum j))
		     (incf nrm (with-vector-data-addresses ((addr-store store))
                                  (incf-sap :double-float addr-store (* j n))
				  (blas::fortran-ddot m addr-store 1 addr-store 1))))
		   (sqrt nrm)))
	(t (error "don't know how to take a ~a-norm of a matrix" p))
	))))



#+:allegro
(defmethod norm ((a real-matrix) &optional (p 2))
  (let ((n (nrows a))
	(m (ncols a))
	(nxm (number-of-elements a))
	(store (store a)))
    (declare (type fixnum n m nxm)
	     (type (real-matrix-store-type (*)) store))

    (if (row-or-col-vector-p a)
	(case p
	 ((1 :1) (dasum nxm store 1))
	 ((2 :2) (dnrm2 nxm store 1))
	 ((:oo :inf :i :infinity)
	                         (let ((k (idamax nxm store 1)))
				    (abs (aref store (1- k)))))
	 ((:-oo :-inf :-i :-infinity)
	                           (let ((nrm 0.0d0))
				     (dotimes (k nxm)
				       (declare (type fixnum k))
				       (setq nrm (min nrm (abs (aref store k)))))
				     nrm))
	 (t (if (and (integerp p)
		     (> p 2))
		(let ((nrm 0.0d0))
		  (dotimes (i nxm)
		    (declare (type fixnum i))
		    (incf nrm (expt (abs (aref store i)) (the fixnum p))))
		  (expt nrm (/ p)))
	      (error "don't know how to take a ~a-norm of a vector" p))))
      
      (case p
	((1 :1) (let ((nrm 0.0d0))
	     (dotimes (j m)
	       (declare (type fixnum j))
	       (setq nrm (max nrm 
			      (let ((colsum 0.0d0))
				(dotimes (i n)
				    (declare (type fixnum i))
				    (incf colsum (abs (matrix-ref a i j))))
				colsum))))
	     nrm))
	((2 :2) (multiple-value-bind (up sigma vp status)
		    (svd a :a)
		  (declare (ignore up vp))
	      (if status
		  (matrix-ref sigma 0)
		(error "SVD did not converge, cannot compute 2-norm of matrix"))))
	((:oo :inf :i :infinity)
	    (let ((nrm 0.0d0))
	      (dotimes (i n)
		(declare (type fixnum i))
		(setq nrm (max nrm
			       (let ((rowsum 0.0d0))
				 (dotimes (j m)
				    (declare (type fixnum j))
				    (incf rowsum (abs (matrix-ref a i j))))
				 rowsum))))
	      nrm))
	((:f :fro :frob :frobenius)
	         (let ((nrm 0.0d0))
		   (dotimes (j m)
		     (declare (type fixnum j))
		     (incf nrm 
			   (let ((colsqrtsum 0.0d0))
				 (dotimes (i n)
				    (declare (type fixnum i))
				    (let ((abs (abs (matrix-ref a i j))))
				      (incf colsqrtsum (* abs abs))))
				 colsqrtsum)))
		   (sqrt nrm)))
	(t (error "don't know how to take a ~a-norm of a matrix" p))
	))))



;; there may be a theoretical bug here, with the use of DZASUM for 
;; 1,oo norms ... either that or matlab has a bug.
;;
;; in either case, the doc for this function should be better defined.

#+(or :cmu :sbcl)
(defmethod norm ((a complex-matrix) &optional (p 2))
  (let ((n (nrows a))
	(m (ncols a))
	(nxm (number-of-elements a))
	(store (store a)))
    (declare (type fixnum n m nxm)
	     (type (complex-matrix-store-type (*)) store))

    (if (row-or-col-vector-p a)
	(case p
	 ((1 :1) (dzasum nxm store 1))
	 ((2 :2) (dznrm2 nxm store 1))
	 ((:oo :inf :i :infinity) (let ((k (izamax nxm store 1)))
				    (abs (matrix-ref a (1- k)))))
	 ((:-oo :-inf :-i :-infinity)
	                           (let ((nrm 0.0d0))
				     (dotimes (k nxm)
				       (declare (type fixnum k))
				       (setq nrm (min nrm (abs (matrix-ref a k)))))
				     nrm))
	 (t (if (and (integerp p)
		     (> p 2))
		(let ((nrm 0.0d0))
		  (dotimes (i nxm)
		    (declare (type fixnum i))
		    (incf nrm (expt (abs (matrix-ref a i)) (the fixnum p))))
		  (expt nrm (/ p)))
	      (error "don't know how to take a ~a-norm of a vector" p))))
      
      (case p
	((1 :1) (let ((nrm 0.0d0))
	     (dotimes (j m)
	       (declare (type fixnum j))
	       (setq nrm (max nrm 
			      (with-vector-data-addresses ((addr-store store))
				  (incf-sap :complex-double-float addr-store (* j n))
				  (blas::fortran-dzasum n addr-store 1)))))
	     nrm))
	((2 :2) (multiple-value-bind (up sigma vp status)
		    (svd a :a)
		  (declare (ignore up vp))
		  (if status
		      (matrix-ref sigma 0 0)
		(error "SVD did not converge, cannot compute 2-norm of matrix"))))
	((:oo :inf :i :infinity)
	    (let ((nrm 0.0d0))
	      (dotimes (i n)
		(declare (type fixnum i))
		(setq nrm (max nrm
			       (with-vector-data-addresses ((addr-store store))
				  (incf-sap :complex-double-float addr-store i)
				  (blas::fortran-dzasum m addr-store n)))))
	      nrm))
	((:f :fro :frob :frobenius)
	         (let ((nrm 0.0d0)
		       (xxx (allocate-complex-store 1)))
		   (dotimes (j m)
		     (declare (type fixnum j))
		     (incf nrm (progn
				(with-vector-data-addresses ((addr-store store)
							     (addr-xxx xxx))							    
				  (incf-sap :double-float addr-store (* j n))
				  (blas::fortran-zdotc addr-xxx m addr-store 1 addr-store 1))
				(aref xxx 0))))
		   (sqrt nrm)))
	(t (error "don't know how to take a ~a-norm of a matrix" p))
	))))




#+:allegro
(defmethod norm ((a complex-matrix) &optional (p 2))
  (let ((n (nrows a))
	(m (ncols a))
	(nxm (number-of-elements a))
	(store (store a)))
    (declare (type fixnum n m nxm)
	     (type (complex-matrix-store-type (*)) store))

    (if (row-or-col-vector-p a)
	(case p
	 ((1 :1) (dzasum nxm store 1))
	 ((2 :2) (dznrm2 nxm store 1))
	 ((:oo :inf :i :infinity) (let ((k (izamax nxm store 1)))
				    (abs (matrix-ref a (1- k)))))
	 ((:-oo :-inf :-i :-infinity)
	                           (let ((nrm 0.0d0))
				     (dotimes (k nxm)
				       (declare (type fixnum k))
				       (setq nrm (min nrm (abs (matrix-ref a k)))))
				     nrm))
	 (t (if (and (integerp p)
		     (> p 2))
		(let ((nrm 0.0d0))
		  (dotimes (i nxm)
		    (declare (type fixnum i))
		    (incf nrm (expt (abs (matrix-ref a i)) (the fixnum p))))
		  (expt nrm (/ p)))
	      (error "don't know how to take a ~a-norm of a vector" p))))
      
      (case p
	((1 :1) (let ((nrm 0.0d0))
	     (dotimes (j m)
	       (declare (type fixnum j))
	       (setq nrm (max nrm 
			      (let ((colsum 0.0d0))
				(dotimes (i n)
				    (declare (type fixnum i))
				    (incf colsum (abs (matrix-ref a i j))))
				colsum))))
	     nrm))
	((2 :2) (multiple-value-bind (up sigma vp status)
		    (svd a :a)
		  (declare (ignore up vp))
		  (if status
		      (matrix-ref sigma 0 0)
		(error "SVD did not converge, cannot compute 2-norm of matrix"))))
	((:oo :inf :i :infinity)
	    (let ((nrm 0.0d0))
	      (dotimes (i n)
		(declare (type fixnum i))
		(setq nrm (max nrm
			       (let ((rowsum 0.0d0))
				 (dotimes (j m)
				    (declare (type fixnum j))
				    (incf rowsum (abs (matrix-ref a i j))))
				 rowsum))))
	      nrm))
	((:f :fro :frob :frobenius)
	         (let ((nrm 0.0d0))
		   (dotimes (j m)
		     (declare (type fixnum j))
		     (incf nrm 
			   (let ((colsqrtsum 0.0d0))
				 (dotimes (i n)
				    (declare (type fixnum i))
				    (let ((abs (abs (matrix-ref a i j))))
				      (incf colsqrtsum (* abs abs))))
				 colsqrtsum)))
		   (sqrt nrm)))
	(t (error "don't know how to take a ~a-norm of a matrix" p))
	))))


















