;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :blas; Base: 10 -*-
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
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley
;;; 1999, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: blas.lisp,v 1.6 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: blas.lisp,v $
;;; Revision 1.6  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.5  2001/02/21 19:31:13  simsek
;;; o Fixed minor typos and doc strings
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.2  2000/06/19 22:21:45  rtoy
;;; Define packages elsewhere.
;;;
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defpackage "BLAS"
#+:cmu  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
#+:sbcl  (:use "COMMON-LISP" "SB-ALIEN" "SB-C" "FORTRAN-FFI-ACCESSORS")
#+:allegro  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS" "FORTRAN-FFI-ACCESSORS")
  (:export
"IDAMAX" "DASUM" "DDOT" "DNRM2"
"DROT" "DSCAL" "DSWAP" "DCOPY" "DAXPY"
"DCABS1" "DZASUM" "DZNRM2" "IZAMAX"
"ZDSCAL" "ZSCAL" "ZSWAP" "ZCOPY" "ZAXPY" "ZDOTC" "ZDOTU"
"DGEMV" "DSYMV" "DTRMV" "DTRSV" "DGER" "DSYR" "DSYR2"
"ZGEMV" "ZHEMV" "ZTRMV" "ZTRSV" "ZGERC" "ZGERU" "ZHER2"
"DGEMM" "DSYRK" "DSYR2K" "DTRMM" "DTRSM"
"ZGEMM" "ZTRMM" "ZTRSM" "ZHERK" "ZHER2K" ))

(in-package "BLAS")

(def-fortran-routine daxpy :void
  "
  Syntax
  ======
 
  (DAXPY n a x incx y incy)

  Purpose
  =======

  Y <- A*X + Y
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  A     (input) DOUBLE-FLOAT
  X     (input) (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(INCX), ... , X((N-1)*INCX)

  Y     (input/output) (SIMPLE-ARRAY DOUBLE-FLOAT (*))
  INCY  (input) FIXNUM 
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(INCY), ... , Y((N-1)*INCY)
"        
  (n :integer :input)
  (da :double-float :input)
  (dx (* :double-float))
  (incx :integer :input)
  (dy (* :double-float) :output)
  (incy :integer :input)
)

(def-fortran-routine dcopy :void
  "
  Syntax
  ======
 
  (DCOPY n x incx y incy)

  Purpose
  =======

  Y <- X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  X     (input) (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(INCX), ... , X((N-1)*INCX)

  Y     (input/output) (SIMPLE-ARRAY DOUBLE-FLOAT (*))
  INCY  (input) FIXNUM 
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(INCY), ... , Y((N-1)*INCY)
"      
  (n :integer :input)
  (dx (* :double-float))
  (incx :integer :input)
  (dy (* :double-float) :output)
  (incy :integer :input)
)

(def-fortran-routine drot :void
  (n :integer :input)
  (dx (* :double-float) :output)
  (incx :integer :input)
  (dy (* :double-float) :output)
  (incy :integer :input)
  (c :double-float :output)
  (s :double-float :output)
  )

#|
(def-fortran-routine drotg :void
  (da :double-float :output)
  (db :double-float :output)
  (c :double-float :output)
  (s :double-float :output)
)
|#

(def-fortran-routine dscal :void
  "
  Syntax
  ======
 
  (DSCAL n a x incx)

  Purpose
  =======

  X <- A*X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X to be operated on.
 
  X     (input) (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(INCX), ... , X((N-1)*INCX)
"   
  (n :integer :input)
  (da :double-float :input)
  (dx (* :double-float) :output)
  (incx :integer :input)
)

(def-fortran-routine dswap :void
  "
  Syntax
  ======
 
  (DSWAP n x incx y incy)

  Purpose
  =======

  Y <-> X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  X     (input) (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(INCX), ... , X((N-1)*INCX)

  Y     (input/output) (SIMPLE-ARRAY DOUBLE-FLOAT (*))
  INCY  (input) FIXNUM 
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(INCY), ... , Y((N-1)*INCY)
"      
  (n :integer :input)
  (dx (* :double-float) :output)
  (incx :integer :input)
  (dy (* :double-float))
  (incy :integer :input)
  )

(def-fortran-routine zaxpy :void
  "
  Syntax
  ======
 
  (ZAXPY n a x incx y incy)

  Purpose
  =======

  Y <- A*X + Y
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  A     (input) (COMPLEX DOUBLE-FLOAT)
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)

  Y     (input/output) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCY  (input) FIXNUM
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(2*INCY), ... , Y(2*(N-1)*INCY)
"        
  (n :integer :input)
  (za :complex-double-float)
  (zx (* :complex-double-float))
  (incx :integer :input)
  (zy (* :complex-double-float) :output)
  (incy :integer :input)
  )

(def-fortran-routine zcopy :void
  "
  Syntax
  ======
 
  (ZCOPY n x incx y incy)

  Purpose
  =======

  Y <- X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  A     (input) (COMPLEX DOUBLE-FLOAT)
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)

  Y     (input/output) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCY  (input) FIXNUM
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(2*INCY), ... , Y(2*(N-1)*INCY)
"    
  (n :integer :input)
  (zx (* :complex-double-float))
  (incx :integer :input)
  (zy (* :complex-double-float) :output)
  (incy :integer :input)
  )

(def-fortran-routine zdscal :void
  "
  Syntax
  ======
 
  (ZDSCAL n a x incx)

  Purpose
  =======

  X <- A*X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X to be operated on.
 
  A     (input) DOUBLE-FLOAT
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)
"      
  (n :integer :input)
  (da :double-float :input)
  (zx (* :complex-double-float) :output)
  (incx :integer :input)
  )

#|
(def-fortran-routine zrotg :void
  (ca (* :complex-double-float) :output)
  (cb (* :complex-double-float) :output)
  (c :double-float :output)
  (s (* :complex-double-float) :output)
  )
|#

(def-fortran-routine zscal :void
 "
  Syntax
  ======
 
  (ZSCAL n a x incx)

  Purpose
  =======

  X <- A*X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X to be operated on.
 
  A     (input) (COMPLEX DOUBLE-FLOAT)
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)
"  
 (n :integer :input)
  (za :complex-double-float)
  (zx (* :complex-double-float) :output)
  (incx :integer :input)
  )

(def-fortran-routine zswap :void
  "
  Syntax
  ======
 
  (ZSWAP n x incx y incy)

  Purpose
  =======

  Y <-> X
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  A     (input) (COMPLEX DOUBLE-FLOAT)
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)

  Y     (input/output) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCY  (input) FIXNUM
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(2*INCY), ... , Y(2*(N-1)*INCY)
"     
  (n :integer :input)
  (zx (* :complex-double-float) :output)
  (incx :integer :input)
  (zy (* :complex-double-float))
  (incy :integer :input)
  )

(def-fortran-routine zdotu :complex-double-float
  "
  Syntax
  ======
 
  (ZDOTU n x incx y incy)

  Purpose
  =======

  ZDOTU <- X^T Y

  Complex precision inner product of X,Y.
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)

  Y     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCY  (input) FIXNUM
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(2*INCY), ... , Y(2*(N-1)*INCY)
"
  (n :integer :input)
  (zx (* :complex-double-float) :input)
  (incx :integer :input)
  (zy (* :complex-double-float) :input)
  (incy :integer :input)
  )


(def-fortran-routine zdotc :complex-double-float
 "
  Syntax
  ======
 
  (ZDOTC n x incx y incy)

  Purpose
  =======

  ZDOTC <- X^H Y

  Complex precision inner product of X conjugate and Y.
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  X     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)

  Y     (input) (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)) represented as (SIMPLE-ARRAY DOUBLE-FLOAT (*)) 
  INCY  (input) FIXNUM
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(2*INCY), ... , Y(2*(N-1)*INCY)
"  (n :integer :input)
  (zx (* :complex-double-float) :input)
  (incx :integer :input)
  (zy (* :complex-double-float) :input)
  (incy :integer :input)
  )

#|
(declaim (inline fortran-daxpy))
(def-alien-routine ("daxpy_" fortran-daxpy) void
  (n int :in-out)
  (da double-float :in-out)
  (dx (* double-float))
  (incx int :in-out)
  (dy (* double-float))
  (incy int :in-out)
)
(defun daxpy (
               n
               da
               dx
               incx
               dy
               incy
                    )
  (declare
     (type fixnum n)
     (type double-float da)
     (type (simple-array double-float (*)) dx)
     (type fixnum incx)
     (type (simple-array double-float (*)) dy)
     (type fixnum incy)
                                          )
      (with-vector-data-addresses (
                                   (addr-dx dx)
                                   (addr-dy dy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
                                                     )
            (fortran-daxpy
                                n
                                da
                                addr-dx
                                incx
                                addr-dy
                                incy
                                                   )
	    (declare (ignore return-value new-n new-incx new-incy))
         (values
                  dy
		      ))))

(declaim (inline fortran-dcopy))
(def-alien-routine ("dcopy_" fortran-dcopy) void
  (n int :in-out)
  (dx (* double-float))
  (incx int :in-out)
  (dy (* double-float))
  (incy int :in-out)
)
(defun dcopy (
               n
               dx
               incx
               dy
               incy
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) dx)
     (type fixnum incx)
     (type (simple-array double-float (*)) dy)
     (type fixnum incy)
                                          )
      (with-vector-data-addresses (
                                   (addr-dx dx)
                                   (addr-dy dy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
                                                     )
            (fortran-dcopy
                                n
                                addr-dx
                                incx
                                addr-dy
                                incy
                                                   )
            (declare (ignore return-value new-n new-incx new-incy))
         (values
                 dy
		     ))))


(declaim (inline fortran-drot))
(def-alien-routine ("drot_" fortran-drot) void
  (n int :in-out)
  (dx (* double-float))
  (incx int :in-out)
  (dy (* double-float))
  (incy int :in-out)
  (c double-float :in-out)
  (s double-float :in-out)
)
(defun drot (
               n
               dx
               incx
               dy
               incy
               c
               s
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) dx)
     (type fixnum incx)
     (type (simple-array double-float (*)) dy)
     (type fixnum incy)
     (type double-float c)
     (type double-float s)
                                          )
      (with-vector-data-addresses (
                                   (addr-dx dx)
                                   (addr-dy dy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
				      new-c
				      new-s
                                                     )

            (fortran-drot
                                n
                                addr-dx
                                incx
                                addr-dy
                                incy
                                c
                                s
                                                   )
            (declare (ignore return-value new-n new-incx new-incy))
         (values
                 dx dy new-c new-s   ))))

(declaim (inline fortran-drotg))
(def-alien-routine ("drotg_" fortran-drotg) void
  (da double-float :in-out)
  (db double-float :in-out)
  (c double-float :in-out)
  (s double-float :in-out)
)
(defun drotg (
               da
               db
               c
               s
                    )
  (declare
     (type double-float da)
     (type double-float db)
     (type double-float c)
     (type double-float s)
                                          )
      (with-vector-data-addresses (
                                               )
         (multiple-value-bind ( return-value new-da new-db new-c new-s
                                                     )
            (fortran-drotg
                                da
                                db
                                c
                                s
                                                   )
	   (declare (ignore return-value))
	   (values
               new-da new-db new-c new-s     ))))

(declaim (inline fortran-dscal))
(def-alien-routine ("dscal_" fortran-dscal) void
  (n int :in-out)
  (da double-float :in-out)
  (dx (* double-float))
  (incx int :in-out)
)
(defun dscal (
               n
               da
               dx
               incx
                    )
  (declare
     (type fixnum n)
     (type double-float da)
     (type (simple-array double-float (*)) dx)
     (type fixnum incx)
                                          )
      (with-vector-data-addresses (
                                   (addr-dx dx)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                                     )
            (fortran-dscal
                                n
                                da
                                addr-dx
                                incx
                                                   )
            (declare (ignore return-value new-n new-incx))
         (values
               dx     ))))

(declaim (inline fortran-dswap))
(def-alien-routine ("dswap_" fortran-dswap) void
  (n int :in-out)
  (dx (* double-float))
  (incx int :in-out)
  (dy (* double-float))
  (incy int :in-out)
)
(defun dswap (
               n
               dx
               incx
               dy
               incy
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) dx)
     (type fixnum incx)
     (type (simple-array double-float (*)) dy)
     (type fixnum incy)
                                          )
      (with-vector-data-addresses (
                                   (addr-dx dx)
                                   (addr-dy dy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
                                                     )
            (fortran-dswap
                                n
                                addr-dx
                                incx
                                addr-dy
                                incy
                                                   )
	    (declare (ignore return-value new-n new-incx new-incy))
         (values
               dx     ))))


(declaim (inline fortran-zaxpy))
(def-alien-routine ("zaxpy_" fortran-zaxpy) void
  (n int :in-out)
  (za (* double-float))
  (zx (* double-float))
  (incx int :in-out)
  (zy (* double-float))
  (incy int :in-out)
)
(defun zaxpy (
               n
               za
               zx
               incx
               zy
               incy
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) za)
     (type (simple-array double-float (*)) zx)
     (type fixnum incx)
     (type (simple-array double-float (*)) zy)
     (type fixnum incy)
                                          )
      (with-vector-data-addresses (
                                   (addr-za za)
				   (addr-zx zx)
                                   (addr-zy zy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
                                                     )
            (fortran-zaxpy
                                n
                                addr-za
                                addr-zx
                                incx
                                addr-zy
                                incy
                                                   )
            (declare (ignore return-value new-n new-incx new-incy))
         (values
                zy    ))))

(declaim (inline fortran-zcopy))
(def-alien-routine ("zcopy_" fortran-zcopy) void
  (n int :in-out)
  (zx (* double-float))
  (incx int :in-out)
  (zy (* double-float))
  (incy int :in-out)
)
(defun zcopy (
               n
               zx
               incx
               zy
               incy
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) zx)
     (type fixnum incx)
     (type (simple-array double-float (*)) zy)
     (type fixnum incy)
                                          )
      (with-vector-data-addresses (
                                   (addr-zx zx)
                                   (addr-zy zy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
                                                     )
            (fortran-zcopy
                                n
                                addr-zx
                                incx
                                addr-zy
                                incy
                                                   )
            (declare (ignore return-value new-n new-incx new-incy))
         (values
                zy    ))))


(declaim (inline fortran-zdscal))
(def-alien-routine ("zdscal_" fortran-zdscal) void
  (n int :in-out)
  (da double-float :in-out)
  (zx (* double-float))
  (incx int :in-out)
)
(defun zdscal (
               n
               da
               zx
               incx
                    )
  (declare
     (type fixnum n)
     (type double-float da)
     (type (simple-array double-float (*)) zx)
     (type fixnum incx)
                                          )
      (with-vector-data-addresses (
                                   (addr-zx zx)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                                     )
            (fortran-zdscal
                                n
                                da
                                addr-zx
                                incx
                                                   )
            (declare (ignore return-value new-n new-incx))
         (values
                zx    ))))

(declaim (inline fortran-zrotg))
(def-alien-routine ("zrotg_" fortran-zrotg) void
  (ca (* double-float))
  (cb (* double-float))
  (c double-float :in-out)
  (s (* double-float))
)
(defun zrotg (
               ca
               cb
               c
               s
                    )
  (declare
     (type (simple-array double-float (*)) ca)
     (type (simple-array double-float (*)) cb)
     (type double-float c)
     (type (simple-array double-float (*)) s)
                                          )
  (with-vector-data-addresses (
			       (addr-ca ca)
			       (addr-cb cb)
			       (addr-s s)
                                               )
         (multiple-value-bind ( return-value  new-c
                                                     )
            (fortran-zrotg
                                addr-ca
                                addr-cb
                                c
                                addr-s
                                                   )
	   (declare (ignore return-value))
	   (values
              ca cb new-c s      ))))

(declaim (inline fortran-zscal))
(def-alien-routine ("zscal_" fortran-zscal) void
  (n int :in-out)
  (za (* double-float))
  (zx (* double-float))
  (incx int :in-out)
)
(defun zscal (
               n
               za
               zx
               incx
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) za)
     (type (simple-array double-float (*)) zx)
     (type fixnum incx)
                                          )
      (with-vector-data-addresses (
                                   (addr-za za)
				   (addr-zx zx)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                                     )
            (fortran-zscal
                                n
                                addr-za
                                addr-zx
                                incx
                                                   )
            (declare (ignore return-value new-n new-incx))
         (values
                zx    ))))

(declaim (inline fortran-zswap))
(def-alien-routine ("zswap_" fortran-zswap) void
  (n int :in-out)
  (zx (* double-float))
  (incx int :in-out)
  (zy (* double-float))
  (incy int :in-out)
)
(defun zswap (
               n
               zx
               incx
               zy
               incy
                    )
  (declare
     (type fixnum n)
     (type (simple-array double-float (*)) zx)
     (type fixnum incx)
     (type (simple-array double-float (*)) zy)
     (type fixnum incy)
                                          )
      (with-vector-data-addresses (
                                   (addr-zx zx)
                                   (addr-zy zy)
                                               )
         (multiple-value-bind ( return-value
                                      new-n
                                      new-incx
                                      new-incy
                                                     )
            (fortran-zswap
                                n
                                addr-zx
                                incx
                                addr-zy
                                incy
                                                   )
            (declare (ignore return-value new-n new-incx new-incy))
         (values
                 zx   ))))

|#
(def-fortran-routine idamax :integer
"
"
  (n :integer :input)
  (dx (* :double-float) :input)
  (incx :integer :input)
)

(def-fortran-routine dasum :double-float
"
"
  (n :integer :input)
  (dx (* :double-float) :input)
  (incx :integer :input)
)

(def-fortran-routine ddot :double-float
  "
  Syntax
  ======
 
  (DDOT n x incx y incy)

  Purpose
  =======

  DDOT <- X^T Y

  Double precision inner product of X,Y.
 
  Arguments
  =========

  N     (input) FIXNUM
        Number of elements of X,Y to be operated on.
 
  X     (input) (SIMPLE-ARRAY DOUBLE-FLOAT (*))
  INCX  (input) FIXNUM 
        Determines the position of the elements in X.  Usually
        INCX is 1.  If INCX is bigger than 1 then the elements
        considered in the operations are:

            X(0),X(2*INCX), ... , X(2*(N-1)*INCX)

  Y     (input) (SIMPLE-ARRAY DOUBLE-FLOAT (*))
  INCY  (input) FIXNUM
        Determines the position of the elements in Y.  Usually
        INCY is 1.  If INCY is bigger than 1 then the elements
        considered in the operations are:

            Y(0),Y(2*INCY), ... , Y(2*(N-1)*INCY)
 "        
  (n :integer :input)
  (dx (* :double-float) :input)
  (incx :integer :input)
  (dy (* :double-float) :input)
  (incy :integer :input)
)

(def-fortran-routine dnrm2 :double-float
"
"
  (n :integer :input)
  (x (* :double-float) :input)
  (incx :integer :input)
)

(def-fortran-routine dgemv :void
"
   Purpose
   =======
 
   DGEMV  performs one of the matrix-vector operations
 
      y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
 
   where alpha and beta are scalars, x and y are vectors and A is an
   m by n matrix.
 
   Parameters
   ==========
 
   TRANS  - CHARACTER*1.
            On entry, TRANS specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
 
               TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
 
               TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry, M specifies the number of rows of the matrix A.
            M must be at least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry, the leading m by n part of the array A must
            contain the matrix of coefficients.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, m ).
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of DIMENSION at least
            ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
            and at least
            ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
            Before entry, the incremented array X must contain the
            vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION.
            On entry, BETA specifies the scalar beta. When BETA is
            supplied as zero then Y need not be set on input.
            Unchanged on exit.
 
   Y      - DOUBLE PRECISION array of DIMENSION at least
            ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
            and at least
            ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
            Before entry with BETA non-zero, the incremented array Y
            must contain the vector y. On exit, Y is overwritten by the
            updated vector y.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (trans :string :input)
  (m :integer )
  (n :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (x (* :double-float) )
  (incx :integer )
  (beta :double-float )
  (y (* :double-float) :output)
  (incy :integer )
)

(def-fortran-routine dsymv :void
"
   Purpose
   =======
 
   DSYMV  performs the matrix-vector  operation
 
      y := alpha*A*x + beta*y,
 
   where alpha and beta are scalars, x and y are n element vectors and
   A is an n by n symmetric matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the upper or lower
            triangular part of the array A is to be referenced as
            follows:
 
               UPLO = 'U' or 'u'   Only the upper triangular part of A
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the lower triangular part of A
                                   is to be referenced.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular part of the symmetric matrix and the strictly
            lower triangular part of A is not referenced.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular part of the symmetric matrix and the strictly
            upper triangular part of A is not referenced.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION.
            On entry, BETA specifies the scalar beta. When BETA is
            supplied as zero then Y need not be set on input.
            Unchanged on exit.
 
   Y      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y. On exit, Y is overwritten by the updated
            vector y.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (n :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (x (* :double-float) )
  (incx :integer )
  (beta :double-float )
  (y (* :double-float) :output)
  (incy :integer )
)

(def-fortran-routine dtrmv :void
"
   Purpose
   =======
 
   DTRMV  performs one of the matrix-vector operations
 
      x := A*x,   or   x := A'*x,
 
   where x is an n element vector and  A is an n by n unit, or non-unit,
   upper or lower triangular matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry, TRANS specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   x := A*x.
 
               TRANS = 'T' or 't'   x := A'*x.
 
               TRANS = 'C' or 'c'   x := A'*x.
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit
            triangular as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular matrix and the strictly lower triangular part of
            A is not referenced.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular matrix and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u', the diagonal elements of
            A are not referenced either, but are assumed to be unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x. On exit, X is overwritten with the
            tranformed vector x.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (diag :string :input)
  (n :integer )
  (a (* :double-float) )
  (lda :integer )
  (x (* :double-float) :output)
  (incx :integer )
)

(def-fortran-routine dtrsv :void
"
   Purpose
   =======
 
   DTRSV  solves one of the systems of equations
 
      A*x = b,   or   A'*x = b,
 
   where b and x are n element vectors and A is an n by n unit, or
   non-unit, upper or lower triangular matrix.
 
   No test for singularity or near-singularity is included in this
   routine. Such tests must be performed before calling this routine.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry, TRANS specifies the equations to be solved as
            follows:
 
               TRANS = 'N' or 'n'   A*x = b.
 
               TRANS = 'T' or 't'   A'*x = b.
 
               TRANS = 'C' or 'c'   A'*x = b.
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit
            triangular as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular matrix and the strictly lower triangular part of
            A is not referenced.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular matrix and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u', the diagonal elements of
            A are not referenced either, but are assumed to be unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element right-hand side vector b. On exit, X is overwritten
            with the solution vector x.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (diag :string :input)
  (n :integer )
  (a (* :double-float) )
  (lda :integer )
  (x (* :double-float) :output)
  (incx :integer )
)

(def-fortran-routine dger :void
"
   Purpose
   =======
 
   DGER   performs the rank 1 operation
 
      A := alpha*x*y' + A,
 
   where alpha is a scalar, x is an m element vector, y is an n element
   vector and A is an m by n matrix.
 
   Parameters
   ==========
 
   M      - INTEGER.
            On entry, M specifies the number of rows of the matrix A.
            M must be at least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( m - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the m
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   Y      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y.
            Unchanged on exit.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry, the leading m by n part of the array A must
            contain the matrix of coefficients. On exit, A is
            overwritten by the updated matrix.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (m :integer )
  (n :integer )
  (alpha :double-float )
  (x (* :double-float) )
  (incx :integer )
  (y (* :double-float) )
  (incy :integer )
  (a (* :double-float) :output)
  (lda :integer )
)

(def-fortran-routine dsyr :void
"
   Purpose
   =======
 
   DSYR   performs the symmetric rank 1 operation
 
      A := alpha*x*x' + A,
 
   where alpha is a real scalar, x is an n element vector and A is an
   n by n symmetric matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the upper or lower
            triangular part of the array A is to be referenced as
            follows:
 
               UPLO = 'U' or 'u'   Only the upper triangular part of A
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the lower triangular part of A
                                   is to be referenced.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular part of the symmetric matrix and the strictly
            lower triangular part of A is not referenced. On exit, the
            upper triangular part of the array A is overwritten by the
            upper triangular part of the updated matrix.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular part of the symmetric matrix and the strictly
            upper triangular part of A is not referenced. On exit, the
            lower triangular part of the array A is overwritten by the
            lower triangular part of the updated matrix.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (n :integer )
  (alpha :double-float )
  (x (* :double-float) )
  (incx :integer )
  (a (* :double-float) :output)
  (lda :integer )
)

(def-fortran-routine dsyr2 :void
"
   Purpose
   =======
 
   DSYR2  performs the symmetric rank 2 operation
 
      A := alpha*x*y' + alpha*y*x' + A,
 
   where alpha is a scalar, x and y are n element vectors and A is an n
   by n symmetric matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the upper or lower
            triangular part of the array A is to be referenced as
            follows:
 
               UPLO = 'U' or 'u'   Only the upper triangular part of A
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the lower triangular part of A
                                   is to be referenced.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   X      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   Y      - DOUBLE PRECISION array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y.
            Unchanged on exit.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular part of the symmetric matrix and the strictly
            lower triangular part of A is not referenced. On exit, the
            upper triangular part of the array A is overwritten by the
            upper triangular part of the updated matrix.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular part of the symmetric matrix and the strictly
            upper triangular part of A is not referenced. On exit, the
            lower triangular part of the array A is overwritten by the
            lower triangular part of the updated matrix.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (n :integer )
  (alpha :double-float )
  (x (* :double-float) )
  (incx :integer )
  (y (* :double-float) )
  (incy :integer )
  (a (* :double-float) :output)
  (lda :integer )
)

(def-fortran-routine dgemm :void
"
   Purpose
   =======
 
   DGEMM  performs one of the matrix-matrix operations
 
      C := alpha*op( A )*op( B ) + beta*C,
 
   where  op( X ) is one of
 
      op( X ) = X   or   op( X ) = X',
 
   alpha and beta are scalars, and A, B and C are matrices, with op( A )
   an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
 
   Parameters
   ==========
 
   TRANSA - CHARACTER*1.
            On entry, TRANSA specifies the form of op( A ) to be used in
            the matrix multiplication as follows:
 
               TRANSA = 'N' or 'n',  op( A ) = A.
 
               TRANSA = 'T' or 't',  op( A ) = A'.
 
               TRANSA = 'C' or 'c',  op( A ) = A'.
 
            Unchanged on exit.
 
   TRANSB - CHARACTER*1.
            On entry, TRANSB specifies the form of op( B ) to be used in
            the matrix multiplication as follows:
 
               TRANSB = 'N' or 'n',  op( B ) = B.
 
               TRANSB = 'T' or 't',  op( B ) = B'.
 
               TRANSB = 'C' or 'c',  op( B ) = B'.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry,  M  specifies  the number  of rows  of the  matrix
            op( A )  and of the  matrix  C.  M  must  be at least  zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry,  N  specifies the number  of columns of the matrix
            op( B ) and the number of columns of the matrix C. N must be
            at least zero.
            Unchanged on exit.
 
   K      - INTEGER.
            On entry,  K  specifies  the number of columns of the matrix
            op( A ) and the number of rows of the matrix op( B ). K must
            be at least  zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
            k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
            Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
            part of the array  A  must contain the matrix  A,  otherwise
            the leading  k by m  part of the array  A  must contain  the
            matrix A.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. When  TRANSA = 'N' or 'n' then
            LDA must be at least  max( 1, m ), otherwise  LDA must be at
            least  max( 1, k ).
            Unchanged on exit.
 
   B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
            n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
            Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
            part of the array  B  must contain the matrix  B,  otherwise
            the leading  n by k  part of the array  B  must contain  the
            matrix B.
            Unchanged on exit.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in the calling (sub) program. When  TRANSB = 'N' or 'n' then
            LDB must be at least  max( 1, k ), otherwise  LDB must be at
            least  max( 1, n ).
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION.
            On entry,  BETA  specifies the scalar  beta.  When  BETA  is
            supplied as zero then C need not be set on input.
            Unchanged on exit.
 
   C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
            Before entry, the leading  m by n  part of the array  C must
            contain the matrix  C,  except when  beta  is zero, in which
            case C need not be set on entry.
            On exit, the array  C  is overwritten by the  m by n  matrix
            ( alpha*op( A )*op( B ) + beta*C ).
 
   LDC    - INTEGER.
            On entry, LDC specifies the first dimension of C as declared
            in  the  calling  (sub)  program.   LDC  must  be  at  least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (transa :string :input)
  (transb :string :input)
  (m :integer )
  (n :integer )
  (k :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (b (* :double-float) )
  (ldb :integer )
  (beta :double-float )
  (c (* :double-float) :output)
  (ldc :integer )
)

(def-fortran-routine dsyrk :void
"
   Purpose
   =======
 
   DSYRK  performs one of the symmetric rank k operations
 
      C := alpha*A*A' + beta*C,
 
   or
 
      C := alpha*A'*A + beta*C,
 
   where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
   and  A  is an  n by k  matrix in the first case and a  k by n  matrix
   in the second case.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On  entry,   UPLO  specifies  whether  the  upper  or  lower
            triangular  part  of the  array  C  is to be  referenced  as
            follows:
 
               UPLO = 'U' or 'u'   Only the  upper triangular part of  C
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the  lower triangular part of  C
                                   is to be referenced.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry,  TRANS  specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   C := alpha*A*A' + beta*C.
 
               TRANS = 'T' or 't'   C := alpha*A'*A + beta*C.
 
               TRANS = 'C' or 'c'   C := alpha*A'*A + beta*C.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry,  N specifies the order of the matrix C.  N must be
            at least zero.
            Unchanged on exit.
 
   K      - INTEGER.
            On entry with  TRANS = 'N' or 'n',  K  specifies  the number
            of  columns   of  the   matrix   A,   and  on   entry   with
            TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
            of rows of the matrix  A.  K must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
            k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            part of the array  A  must contain the matrix  A,  otherwise
            the leading  k by n  part of the array  A  must contain  the
            matrix A.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            then  LDA must be at least  max( 1, n ), otherwise  LDA must
            be at least  max( 1, k ).
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION.
            On entry, BETA specifies the scalar beta.
            Unchanged on exit.
 
   C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
            Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
            upper triangular part of the array C must contain the upper
            triangular part  of the  symmetric matrix  and the strictly
            lower triangular part of C is not referenced.  On exit, the
            upper triangular part of the array  C is overwritten by the
            upper triangular part of the updated matrix.
            Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
            lower triangular part of the array C must contain the lower
            triangular part  of the  symmetric matrix  and the strictly
            upper triangular part of C is not referenced.  On exit, the
            lower triangular part of the array  C is overwritten by the
            lower triangular part of the updated matrix.
 
   LDC    - INTEGER.
            On entry, LDC specifies the first dimension of C as declared
            in  the  calling  (sub)  program.   LDC  must  be  at  least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (n :integer )
  (k :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (beta :double-float )
  (c (* :double-float) :output)
  (ldc :integer )
)

(def-fortran-routine dsyr2k :void
"
   Purpose
   =======
 
   DSYR2K  performs one of the symmetric rank 2k operations
 
      C := alpha*A*B' + alpha*B*A' + beta*C,
 
   or
 
      C := alpha*A'*B + alpha*B'*A + beta*C,
 
   where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
   and  A and B  are  n by k  matrices  in the  first  case  and  k by n
   matrices in the second case.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On  entry,   UPLO  specifies  whether  the  upper  or  lower
            triangular  part  of the  array  C  is to be  referenced  as
            follows:
 
               UPLO = 'U' or 'u'   Only the  upper triangular part of  C
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the  lower triangular part of  C
                                   is to be referenced.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry,  TRANS  specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   C := alpha*A*B' + alpha*B*A' +
                                         beta*C.
 
               TRANS = 'T' or 't'   C := alpha*A'*B + alpha*B'*A +
                                         beta*C.
 
               TRANS = 'C' or 'c'   C := alpha*A'*B + alpha*B'*A +
                                         beta*C.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry,  N specifies the order of the matrix C.  N must be
            at least zero.
            Unchanged on exit.
 
   K      - INTEGER.
            On entry with  TRANS = 'N' or 'n',  K  specifies  the number
            of  columns  of the  matrices  A and B,  and on  entry  with
            TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
            of rows of the matrices  A and B.  K must be at least  zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
            k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            part of the array  A  must contain the matrix  A,  otherwise
            the leading  k by n  part of the array  A  must contain  the
            matrix A.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            then  LDA must be at least  max( 1, n ), otherwise  LDA must
            be at least  max( 1, k ).
            Unchanged on exit.
 
   B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
            k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            part of the array  B  must contain the matrix  B,  otherwise
            the leading  k by n  part of the array  B  must contain  the
            matrix B.
            Unchanged on exit.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            then  LDB must be at least  max( 1, n ), otherwise  LDB must
            be at least  max( 1, k ).
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION.
            On entry, BETA specifies the scalar beta.
            Unchanged on exit.
 
   C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
            Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
            upper triangular part of the array C must contain the upper
            triangular part  of the  symmetric matrix  and the strictly
            lower triangular part of C is not referenced.  On exit, the
            upper triangular part of the array  C is overwritten by the
            upper triangular part of the updated matrix.
            Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
            lower triangular part of the array C must contain the lower
            triangular part  of the  symmetric matrix  and the strictly
            upper triangular part of C is not referenced.  On exit, the
            lower triangular part of the array  C is overwritten by the
            lower triangular part of the updated matrix.
 
   LDC    - INTEGER.
            On entry, LDC specifies the first dimension of C as declared
            in  the  calling  (sub)  program.   LDC  must  be  at  least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (n :integer )
  (k :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (b (* :double-float) )
  (ldb :integer )
  (beta :double-float )
  (c (* :double-float) :output)
  (ldc :integer )
)

(def-fortran-routine dtrmm :void
"
   Purpose
   =======
 
   DTRMM  performs one of the matrix-matrix operations
 
      B := alpha*op( A )*B,   or   B := alpha*B*op( A ),
 
   where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
   non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
 
      op( A ) = A   or   op( A ) = A'.
 
   Parameters
   ==========
 
   SIDE   - CHARACTER*1.
            On entry,  SIDE specifies whether  op( A ) multiplies B from
            the left or right as follows:
 
               SIDE = 'L' or 'l'   B := alpha*op( A )*B.
 
               SIDE = 'R' or 'r'   B := alpha*B*op( A ).
 
            Unchanged on exit.
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix A is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANSA - CHARACTER*1.
            On entry, TRANSA specifies the form of op( A ) to be used in
            the matrix multiplication as follows:
 
               TRANSA = 'N' or 'n'   op( A ) = A.
 
               TRANSA = 'T' or 't'   op( A ) = A'.
 
               TRANSA = 'C' or 'c'   op( A ) = A'.
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit triangular
            as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry, M specifies the number of rows of B. M must be at
            least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of B.  N must be
            at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry,  ALPHA specifies the scalar  alpha. When  alpha is
            zero then  A is not referenced and  B need not be set before
            entry.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
            when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
            Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
            upper triangular part of the array  A must contain the upper
            triangular matrix  and the strictly lower triangular part of
            A is not referenced.
            Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
            lower triangular part of the array  A must contain the lower
            triangular matrix  and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u',  the diagonal elements of
            A  are not referenced either,  but are assumed to be  unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
            LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
            then LDA must be at least max( 1, n ).
            Unchanged on exit.
 
   B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
            Before entry,  the leading  m by n part of the array  B must
            contain the matrix  B,  and  on exit  is overwritten  by the
            transformed matrix.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in  the  calling  (sub)  program.   LDB  must  be  at  least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (side :string :input)
  (uplo :string :input)
  (transa :string :input)
  (diag :string :input)
  (m :integer )
  (n :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (b (* :double-float) :output)
  (ldb :integer )
)

(def-fortran-routine dtrsm :void
"
   Purpose
   =======
 
   DTRSM  solves one of the matrix equations
 
      op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
 
   where alpha is a scalar, X and B are m by n matrices, A is a unit, or
   non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
 
      op( A ) = A   or   op( A ) = A'.
 
   The matrix X is overwritten on B.
 
   Parameters
   ==========
 
   SIDE   - CHARACTER*1.
            On entry, SIDE specifies whether op( A ) appears on the left
            or right of X as follows:
 
               SIDE = 'L' or 'l'   op( A )*X = alpha*B.
 
               SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
 
            Unchanged on exit.
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix A is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANSA - CHARACTER*1.
            On entry, TRANSA specifies the form of op( A ) to be used in
            the matrix multiplication as follows:
 
               TRANSA = 'N' or 'n'   op( A ) = A.
 
               TRANSA = 'T' or 't'   op( A ) = A'.
 
               TRANSA = 'C' or 'c'   op( A ) = A'.
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit triangular
            as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry, M specifies the number of rows of B. M must be at
            least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of B.  N must be
            at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION.
            On entry,  ALPHA specifies the scalar  alpha. When  alpha is
            zero then  A is not referenced and  B need not be set before
            entry.
            Unchanged on exit.
 
   A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
            when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
            Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
            upper triangular part of the array  A must contain the upper
            triangular matrix  and the strictly lower triangular part of
            A is not referenced.
            Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
            lower triangular part of the array  A must contain the lower
            triangular matrix  and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u',  the diagonal elements of
            A  are not referenced either,  but are assumed to be  unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
            LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
            then LDA must be at least max( 1, n ).
            Unchanged on exit.
 
   B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
            Before entry,  the leading  m by n part of the array  B must
            contain  the  right-hand  side  matrix  B,  and  on exit  is
            overwritten by the solution matrix  X.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in  the  calling  (sub)  program.   LDB  must  be  at  least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (side :string :input)
  (uplo :string :input)
  (transa :string :input)
  (diag :string :input)
  (m :integer )
  (n :integer )
  (alpha :double-float )
  (a (* :double-float) )
  (lda :integer )
  (b (* :double-float) :output)
  (ldb :integer )
)

(def-fortran-routine dcabs1 :double-float
"
"
  (z :complex-double-float :input)
)

(def-fortran-routine dzasum :double-float
"
"
  (n :integer :input)
  (zx (* :complex-double-float) :input)
  (incx :integer :input)
)

(def-fortran-routine dznrm2 :double-float
"
"
  (n :integer :input)
  (x (* :complex-double-float) :input)
  (incx :integer :input)
)

(def-fortran-routine izamax :integer
"
"
  (n :integer :input)
  (zx (* :complex-double-float) :input)
  (incx :integer :input)
)

(def-fortran-routine zgemv :void
"
   Purpose
   =======
 
   ZGEMV  performs one of the matrix-vector operations
 
      y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,   or
 
      y := alpha*conjg( A' )*x + beta*y,
 
   where alpha and beta are scalars, x and y are vectors and A is an
   m by n matrix.
 
   Parameters
   ==========
 
   TRANS  - CHARACTER*1.
            On entry, TRANS specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
 
               TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
 
               TRANS = 'C' or 'c'   y := alpha*conjg( A' )*x + beta*y.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry, M specifies the number of rows of the matrix A.
            M must be at least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry, the leading m by n part of the array A must
            contain the matrix of coefficients.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, m ).
            Unchanged on exit.
 
   X      - COMPLEX*16       array of DIMENSION at least
            ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
            and at least
            ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
            Before entry, the incremented array X must contain the
            vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   BETA   - COMPLEX*16      .
            On entry, BETA specifies the scalar beta. When BETA is
            supplied as zero then Y need not be set on input.
            Unchanged on exit.
 
   Y      - COMPLEX*16       array of DIMENSION at least
            ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
            and at least
            ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
            Before entry with BETA non-zero, the incremented array Y
            must contain the vector y. On exit, Y is overwritten by the
            updated vector y.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (trans :string :input)
  (m :integer )
  (n :integer )
  (alpha :complex-double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (x (* :complex-double-float) )
  (incx :integer )
  (beta :complex-double-float )
  (y (* :complex-double-float) :output)
  (incy :integer )
)

(def-fortran-routine zhemv :void
"
   Purpose
   =======
 
   ZHEMV  performs the matrix-vector  operation
 
      y := alpha*A*x + beta*y,
 
   where alpha and beta are scalars, x and y are n element vectors and
   A is an n by n hermitian matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the upper or lower
            triangular part of the array A is to be referenced as
            follows:
 
               UPLO = 'U' or 'u'   Only the upper triangular part of A
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the lower triangular part of A
                                   is to be referenced.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular part of the hermitian matrix and the strictly
            lower triangular part of A is not referenced.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular part of the hermitian matrix and the strictly
            upper triangular part of A is not referenced.
            Note that the imaginary parts of the diagonal elements need
            not be set and are assumed to be zero.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
   X      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   BETA   - COMPLEX*16      .
            On entry, BETA specifies the scalar beta. When BETA is
            supplied as zero then Y need not be set on input.
            Unchanged on exit.
 
   Y      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y. On exit, Y is overwritten by the updated
            vector y.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (n :integer )
  (alpha :complex-double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (x (* :complex-double-float) )
  (incx :integer )
  (beta :complex-double-float )
  (y (* :complex-double-float) :output)
  (incy :integer )
)

(def-fortran-routine ztrmv :void
"
   Purpose
   =======
 
   ZTRMV  performs one of the matrix-vector operations
 
      x := A*x,   or   x := A'*x,   or   x := conjg( A' )*x,
 
   where x is an n element vector and  A is an n by n unit, or non-unit,
   upper or lower triangular matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry, TRANS specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   x := A*x.
 
               TRANS = 'T' or 't'   x := A'*x.
 
               TRANS = 'C' or 'c'   x := conjg( A' )*x.
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit
            triangular as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular matrix and the strictly lower triangular part of
            A is not referenced.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular matrix and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u', the diagonal elements of
            A are not referenced either, but are assumed to be unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
   X      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x. On exit, X is overwritten with the
            tranformed vector x.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (diag :string :input)
  (n :integer )
  (a (* :complex-double-float) )
  (lda :integer )
  (x (* :complex-double-float) :output)
  (incx :integer )
)

(def-fortran-routine ztrsv :void
"
   Purpose
   =======
 
   ZTRSV  solves one of the systems of equations
 
      A*x = b,   or   A'*x = b,   or   conjg( A' )*x = b,
 
   where b and x are n element vectors and A is an n by n unit, or
   non-unit, upper or lower triangular matrix.
 
   No test for singularity or near-singularity is included in this
   routine. Such tests must be performed before calling this routine.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry, TRANS specifies the equations to be solved as
            follows:
 
               TRANS = 'N' or 'n'   A*x = b.
 
               TRANS = 'T' or 't'   A'*x = b.
 
               TRANS = 'C' or 'c'   conjg( A' )*x = b.
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit
            triangular as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular matrix and the strictly lower triangular part of
            A is not referenced.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular matrix and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u', the diagonal elements of
            A are not referenced either, but are assumed to be unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
   X      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element right-hand side vector b. On exit, X is overwritten
            with the solution vector x.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (diag :string :input)
  (n :integer )
  (a (* :complex-double-float) )
  (lda :integer )
  (x (* :complex-double-float) :output)
  (incx :integer )
)

(def-fortran-routine zgerc :void
"
   Purpose
   =======
 
   ZGERC  performs the rank 1 operation
 
      A := alpha*x*conjg( y' ) + A,
 
   where alpha is a scalar, x is an m element vector, y is an n element
   vector and A is an m by n matrix.
 
   Parameters
   ==========
 
   M      - INTEGER.
            On entry, M specifies the number of rows of the matrix A.
            M must be at least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   X      - COMPLEX*16       array of dimension at least
            ( 1 + ( m - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the m
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   Y      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y.
            Unchanged on exit.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry, the leading m by n part of the array A must
            contain the matrix of coefficients. On exit, A is
            overwritten by the updated matrix.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (m :integer )
  (n :integer )
  (alpha :complex-double-float )
  (x (* :complex-double-float) )
  (incx :integer )
  (y (* :complex-double-float) )
  (incy :integer )
  (a (* :complex-double-float) :output)
  (lda :integer )
)

(def-fortran-routine zgeru :void
"
   Purpose
   =======
 
   ZGERU  performs the rank 1 operation
 
      A := alpha*x*y' + A,
 
   where alpha is a scalar, x is an m element vector, y is an n element
   vector and A is an m by n matrix.
 
   Parameters
   ==========
 
   M      - INTEGER.
            On entry, M specifies the number of rows of the matrix A.
            M must be at least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   X      - COMPLEX*16       array of dimension at least
            ( 1 + ( m - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the m
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   Y      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y.
            Unchanged on exit.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry, the leading m by n part of the array A must
            contain the matrix of coefficients. On exit, A is
            overwritten by the updated matrix.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (m :integer )
  (n :integer )
  (alpha :complex-double-float )
  (x (* :complex-double-float) )
  (incx :integer )
  (y (* :complex-double-float) )
  (incy :integer )
  (a (* :complex-double-float) :output)
  (lda :integer )
)

(def-fortran-routine zher2 :void
"
   Purpose
   =======
 
   ZHER2  performs the hermitian rank 2 operation
 
      A := alpha*x*conjg( y' ) + conjg( alpha )*y*conjg( x' ) + A,
 
   where alpha is a scalar, x and y are n element vectors and A is an n
   by n hermitian matrix.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the upper or lower
            triangular part of the array A is to be referenced as
            follows:
 
               UPLO = 'U' or 'u'   Only the upper triangular part of A
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the lower triangular part of A
                                   is to be referenced.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the order of the matrix A.
            N must be at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   X      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCX ) ).
            Before entry, the incremented array X must contain the n
            element vector x.
            Unchanged on exit.
 
   INCX   - INTEGER.
            On entry, INCX specifies the increment for the elements of
            X. INCX must not be zero.
            Unchanged on exit.
 
   Y      - COMPLEX*16       array of dimension at least
            ( 1 + ( n - 1 )*abs( INCY ) ).
            Before entry, the incremented array Y must contain the n
            element vector y.
            Unchanged on exit.
 
   INCY   - INTEGER.
            On entry, INCY specifies the increment for the elements of
            Y. INCY must not be zero.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, n ).
            Before entry with  UPLO = 'U' or 'u', the leading n by n
            upper triangular part of the array A must contain the upper
            triangular part of the hermitian matrix and the strictly
            lower triangular part of A is not referenced. On exit, the
            upper triangular part of the array A is overwritten by the
            upper triangular part of the updated matrix.
            Before entry with UPLO = 'L' or 'l', the leading n by n
            lower triangular part of the array A must contain the lower
            triangular part of the hermitian matrix and the strictly
            upper triangular part of A is not referenced. On exit, the
            lower triangular part of the array A is overwritten by the
            lower triangular part of the updated matrix.
            Note that the imaginary parts of the diagonal elements need
            not be set, they are assumed to be zero, and on exit they
            are set to zero.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. LDA must be at least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 2 Blas routine.
 
   -- Written on 22-October-1986.
      Jack Dongarra, Argonne National Lab.
      Jeremy Du Croz, Nag Central Office.
      Sven Hammarling, Nag Central Office.
      Richard Hanson, Sandia National Labs.
 
 
"
  (uplo :string :input)
  (n :integer )
  (alpha :complex-double-float )
  (x (* :complex-double-float) )
  (incx :integer )
  (y (* :complex-double-float) )
  (incy :integer )
  (a (* :complex-double-float) :output)
  (lda :integer )
)

(def-fortran-routine zgemm :void
"
   Purpose
   =======
 
   ZGEMM  performs one of the matrix-matrix operations
 
      C := alpha*op( A )*op( B ) + beta*C,
 
   where  op( X ) is one of
 
      op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ),
 
   alpha and beta are scalars, and A, B and C are matrices, with op( A )
   an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
 
   Parameters
   ==========
 
   TRANSA - CHARACTER*1.
            On entry, TRANSA specifies the form of op( A ) to be used in
            the matrix multiplication as follows:
 
               TRANSA = 'N' or 'n',  op( A ) = A.
 
               TRANSA = 'T' or 't',  op( A ) = A'.
 
               TRANSA = 'C' or 'c',  op( A ) = conjg( A' ).
 
            Unchanged on exit.
 
   TRANSB - CHARACTER*1.
            On entry, TRANSB specifies the form of op( B ) to be used in
            the matrix multiplication as follows:
 
               TRANSB = 'N' or 'n',  op( B ) = B.
 
               TRANSB = 'T' or 't',  op( B ) = B'.
 
               TRANSB = 'C' or 'c',  op( B ) = conjg( B' ).
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry,  M  specifies  the number  of rows  of the  matrix
            op( A )  and of the  matrix  C.  M  must  be at least  zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry,  N  specifies the number  of columns of the matrix
            op( B ) and the number of columns of the matrix C. N must be
            at least zero.
            Unchanged on exit.
 
   K      - INTEGER.
            On entry,  K  specifies  the number of columns of the matrix
            op( A ) and the number of rows of the matrix op( B ). K must
            be at least  zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
            k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
            Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
            part of the array  A  must contain the matrix  A,  otherwise
            the leading  k by m  part of the array  A  must contain  the
            matrix A.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program. When  TRANSA = 'N' or 'n' then
            LDA must be at least  max( 1, m ), otherwise  LDA must be at
            least  max( 1, k ).
            Unchanged on exit.
 
   B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is
            n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
            Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
            part of the array  B  must contain the matrix  B,  otherwise
            the leading  n by k  part of the array  B  must contain  the
            matrix B.
            Unchanged on exit.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in the calling (sub) program. When  TRANSB = 'N' or 'n' then
            LDB must be at least  max( 1, k ), otherwise  LDB must be at
            least  max( 1, n ).
            Unchanged on exit.
 
   BETA   - COMPLEX*16      .
            On entry,  BETA  specifies the scalar  beta.  When  BETA  is
            supplied as zero then C need not be set on input.
            Unchanged on exit.
 
   C      - COMPLEX*16       array of DIMENSION ( LDC, n ).
            Before entry, the leading  m by n  part of the array  C must
            contain the matrix  C,  except when  beta  is zero, in which
            case C need not be set on entry.
            On exit, the array  C  is overwritten by the  m by n  matrix
            ( alpha*op( A )*op( B ) + beta*C ).
 
   LDC    - INTEGER.
            On entry, LDC specifies the first dimension of C as declared
            in  the  calling  (sub)  program.   LDC  must  be  at  least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (transa :string :input)
  (transb :string :input)
  (m :integer )
  (n :integer )
  (k :integer )
  (alpha :complex-double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (b (* :complex-double-float) )
  (ldb :integer )
  (beta :complex-double-float )
  (c (* :complex-double-float) :output)
  (ldc :integer )
)

(def-fortran-routine ztrmm :void
"
   Purpose
   =======
 
   ZTRMM  performs one of the matrix-matrix operations
 
      B := alpha*op( A )*B,   or   B := alpha*B*op( A )
 
   where  alpha  is a scalar,  B  is an m by n matrix,  A  is a unit, or
   non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
 
      op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).
 
   Parameters
   ==========
 
   SIDE   - CHARACTER*1.
            On entry,  SIDE specifies whether  op( A ) multiplies B from
            the left or right as follows:
 
               SIDE = 'L' or 'l'   B := alpha*op( A )*B.
 
               SIDE = 'R' or 'r'   B := alpha*B*op( A ).
 
            Unchanged on exit.
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix A is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANSA - CHARACTER*1.
            On entry, TRANSA specifies the form of op( A ) to be used in
            the matrix multiplication as follows:
 
               TRANSA = 'N' or 'n'   op( A ) = A.
 
               TRANSA = 'T' or 't'   op( A ) = A'.
 
               TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit triangular
            as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry, M specifies the number of rows of B. M must be at
            least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of B.  N must be
            at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry,  ALPHA specifies the scalar  alpha. When  alpha is
            zero then  A is not referenced and  B need not be set before
            entry.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, k ), where k is m
            when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
            Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
            upper triangular part of the array  A must contain the upper
            triangular matrix  and the strictly lower triangular part of
            A is not referenced.
            Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
            lower triangular part of the array  A must contain the lower
            triangular matrix  and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u',  the diagonal elements of
            A  are not referenced either,  but are assumed to be  unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
            LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
            then LDA must be at least max( 1, n ).
            Unchanged on exit.
 
   B      - COMPLEX*16       array of DIMENSION ( LDB, n ).
            Before entry,  the leading  m by n part of the array  B must
            contain the matrix  B,  and  on exit  is overwritten  by the
            transformed matrix.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in  the  calling  (sub)  program.   LDB  must  be  at  least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (side :string :input)
  (uplo :string :input)
  (transa :string :input)
  (diag :string :input)
  (m :integer )
  (n :integer )
  (alpha :complex-double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (b (* :complex-double-float) :output)
  (ldb :integer )
)

(def-fortran-routine ztrsm :void
"
   Purpose
   =======
 
   ZTRSM  solves one of the matrix equations
 
      op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
 
   where alpha is a scalar, X and B are m by n matrices, A is a unit, or
   non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
 
      op( A ) = A   or   op( A ) = A'   or   op( A ) = conjg( A' ).
 
   The matrix X is overwritten on B.
 
   Parameters
   ==========
 
   SIDE   - CHARACTER*1.
            On entry, SIDE specifies whether op( A ) appears on the left
            or right of X as follows:
 
               SIDE = 'L' or 'l'   op( A )*X = alpha*B.
 
               SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
 
            Unchanged on exit.
 
   UPLO   - CHARACTER*1.
            On entry, UPLO specifies whether the matrix A is an upper or
            lower triangular matrix as follows:
 
               UPLO = 'U' or 'u'   A is an upper triangular matrix.
 
               UPLO = 'L' or 'l'   A is a lower triangular matrix.
 
            Unchanged on exit.
 
   TRANSA - CHARACTER*1.
            On entry, TRANSA specifies the form of op( A ) to be used in
            the matrix multiplication as follows:
 
               TRANSA = 'N' or 'n'   op( A ) = A.
 
               TRANSA = 'T' or 't'   op( A ) = A'.
 
               TRANSA = 'C' or 'c'   op( A ) = conjg( A' ).
 
            Unchanged on exit.
 
   DIAG   - CHARACTER*1.
            On entry, DIAG specifies whether or not A is unit triangular
            as follows:
 
               DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 
               DIAG = 'N' or 'n'   A is not assumed to be unit
                                   triangular.
 
            Unchanged on exit.
 
   M      - INTEGER.
            On entry, M specifies the number of rows of B. M must be at
            least zero.
            Unchanged on exit.
 
   N      - INTEGER.
            On entry, N specifies the number of columns of B.  N must be
            at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16      .
            On entry,  ALPHA specifies the scalar  alpha. When  alpha is
            zero then  A is not referenced and  B need not be set before
            entry.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, k ), where k is m
            when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
            Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
            upper triangular part of the array  A must contain the upper
            triangular matrix  and the strictly lower triangular part of
            A is not referenced.
            Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
            lower triangular part of the array  A must contain the lower
            triangular matrix  and the strictly upper triangular part of
            A is not referenced.
            Note that when  DIAG = 'U' or 'u',  the diagonal elements of
            A  are not referenced either,  but are assumed to be  unity.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
            LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
            then LDA must be at least max( 1, n ).
            Unchanged on exit.
 
   B      - COMPLEX*16       array of DIMENSION ( LDB, n ).
            Before entry,  the leading  m by n part of the array  B must
            contain  the  right-hand  side  matrix  B,  and  on exit  is
            overwritten by the solution matrix  X.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in  the  calling  (sub)  program.   LDB  must  be  at  least
            max( 1, m ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
 
"
  (side :string :input)
  (uplo :string :input)
  (transa :string :input)
  (diag :string :input)
  (m :integer )
  (n :integer )
  (alpha :complex-double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (b (* :complex-double-float) :output)
  (ldb :integer )
)

(def-fortran-routine zherk :void
"
   Purpose
   =======
 
   ZHERK  performs one of the hermitian rank k operations
 
      C := alpha*A*conjg( A' ) + beta*C,
 
   or
 
      C := alpha*conjg( A' )*A + beta*C,
 
   where  alpha and beta  are  real scalars,  C is an  n by n  hermitian
   matrix and  A  is an  n by k  matrix in the  first case and a  k by n
   matrix in the second case.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On  entry,   UPLO  specifies  whether  the  upper  or  lower
            triangular  part  of the  array  C  is to be  referenced  as
            follows:
 
               UPLO = 'U' or 'u'   Only the  upper triangular part of  C
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the  lower triangular part of  C
                                   is to be referenced.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry,  TRANS  specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'   C := alpha*A*conjg( A' ) + beta*C.
 
               TRANS = 'C' or 'c'   C := alpha*conjg( A' )*A + beta*C.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry,  N specifies the order of the matrix C.  N must be
            at least zero.
            Unchanged on exit.
 
   K      - INTEGER.
            On entry with  TRANS = 'N' or 'n',  K  specifies  the number
            of  columns   of  the   matrix   A,   and  on   entry   with
            TRANS = 'C' or 'c',  K  specifies  the number of rows of the
            matrix A.  K must be at least zero.
            Unchanged on exit.
 
   ALPHA  - DOUBLE PRECISION            .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
            k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            part of the array  A  must contain the matrix  A,  otherwise
            the leading  k by n  part of the array  A  must contain  the
            matrix A.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            then  LDA must be at least  max( 1, n ), otherwise  LDA must
            be at least  max( 1, k ).
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION.
            On entry, BETA specifies the scalar beta.
            Unchanged on exit.
 
   C      - COMPLEX*16          array of DIMENSION ( LDC, n ).
            Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
            upper triangular part of the array C must contain the upper
            triangular part  of the  hermitian matrix  and the strictly
            lower triangular part of C is not referenced.  On exit, the
            upper triangular part of the array  C is overwritten by the
            upper triangular part of the updated matrix.
            Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
            lower triangular part of the array C must contain the lower
            triangular part  of the  hermitian matrix  and the strictly
            upper triangular part of C is not referenced.  On exit, the
            lower triangular part of the array  C is overwritten by the
            lower triangular part of the updated matrix.
            Note that the imaginary parts of the diagonal elements need
            not be set,  they are assumed to be zero,  and on exit they
            are set to zero.
 
   LDC    - INTEGER.
            On entry, LDC specifies the first dimension of C as declared
            in  the  calling  (sub)  program.   LDC  must  be  at  least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
   -- Modified 8-Nov-93 to set C(J,J) to DBLE( C(J,J) ) when BETA = 1.
      Ed Anderson, Cray Research Inc.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (n :integer )
  (k :integer )
  (alpha :double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (beta :double-float )
  (c (* :complex-double-float) :output)
  (ldc :integer )
)

(def-fortran-routine zher2k :void
"
   Purpose
   =======
 
   ZHER2K  performs one of the hermitian rank 2k operations
 
      C := alpha*A*conjg( B' ) + conjg( alpha )*B*conjg( A' ) + beta*C,
 
   or
 
      C := alpha*conjg( A' )*B + conjg( alpha )*conjg( B' )*A + beta*C,
 
   where  alpha and beta  are scalars with  beta  real,  C is an  n by n
   hermitian matrix and  A and B  are  n by k matrices in the first case
   and  k by n  matrices in the second case.
 
   Parameters
   ==========
 
   UPLO   - CHARACTER*1.
            On  entry,   UPLO  specifies  whether  the  upper  or  lower
            triangular  part  of the  array  C  is to be  referenced  as
            follows:
 
               UPLO = 'U' or 'u'   Only the  upper triangular part of  C
                                   is to be referenced.
 
               UPLO = 'L' or 'l'   Only the  lower triangular part of  C
                                   is to be referenced.
 
            Unchanged on exit.
 
   TRANS  - CHARACTER*1.
            On entry,  TRANS  specifies the operation to be performed as
            follows:
 
               TRANS = 'N' or 'n'    C := alpha*A*conjg( B' )          +
                                          conjg( alpha )*B*conjg( A' ) +
                                          beta*C.
 
               TRANS = 'C' or 'c'    C := alpha*conjg( A' )*B          +
                                          conjg( alpha )*conjg( B' )*A +
                                          beta*C.
 
            Unchanged on exit.
 
   N      - INTEGER.
            On entry,  N specifies the order of the matrix C.  N must be
            at least zero.
            Unchanged on exit.
 
   K      - INTEGER.
            On entry with  TRANS = 'N' or 'n',  K  specifies  the number
            of  columns  of the  matrices  A and B,  and on  entry  with
            TRANS = 'C' or 'c',  K  specifies  the number of rows of the
            matrices  A and B.  K must be at least zero.
            Unchanged on exit.
 
   ALPHA  - COMPLEX*16         .
            On entry, ALPHA specifies the scalar alpha.
            Unchanged on exit.
 
   A      - COMPLEX*16       array of DIMENSION ( LDA, ka ), where ka is
            k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            part of the array  A  must contain the matrix  A,  otherwise
            the leading  k by n  part of the array  A  must contain  the
            matrix A.
            Unchanged on exit.
 
   LDA    - INTEGER.
            On entry, LDA specifies the first dimension of A as declared
            in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            then  LDA must be at least  max( 1, n ), otherwise  LDA must
            be at least  max( 1, k ).
            Unchanged on exit.
 
   B      - COMPLEX*16       array of DIMENSION ( LDB, kb ), where kb is
            k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            part of the array  B  must contain the matrix  B,  otherwise
            the leading  k by n  part of the array  B  must contain  the
            matrix B.
            Unchanged on exit.
 
   LDB    - INTEGER.
            On entry, LDB specifies the first dimension of B as declared
            in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            then  LDB must be at least  max( 1, n ), otherwise  LDB must
            be at least  max( 1, k ).
            Unchanged on exit.
 
   BETA   - DOUBLE PRECISION            .
            On entry, BETA specifies the scalar beta.
            Unchanged on exit.
 
   C      - COMPLEX*16          array of DIMENSION ( LDC, n ).
            Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
            upper triangular part of the array C must contain the upper
            triangular part  of the  hermitian matrix  and the strictly
            lower triangular part of C is not referenced.  On exit, the
            upper triangular part of the array  C is overwritten by the
            upper triangular part of the updated matrix.
            Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
            lower triangular part of the array C must contain the lower
            triangular part  of the  hermitian matrix  and the strictly
            upper triangular part of C is not referenced.  On exit, the
            lower triangular part of the array  C is overwritten by the
            lower triangular part of the updated matrix.
            Note that the imaginary parts of the diagonal elements need
            not be set,  they are assumed to be zero,  and on exit they
            are set to zero.
 
   LDC    - INTEGER.
            On entry, LDC specifies the first dimension of C as declared
            in  the  calling  (sub)  program.   LDC  must  be  at  least
            max( 1, n ).
            Unchanged on exit.
 
 
   Level 3 Blas routine.
 
   -- Written on 8-February-1989.
      Jack Dongarra, Argonne National Laboratory.
      Iain Duff, AERE Harwell.
      Jeremy Du Croz, Numerical Algorithms Group Ltd.
      Sven Hammarling, Numerical Algorithms Group Ltd.
 
   -- Modified 8-Nov-93 to set C(J,J) to DBLE( C(J,J) ) when BETA = 1.
      Ed Anderson, Cray Research Inc.
 
 
"
  (uplo :string :input)
  (trans :string :input)
  (n :integer )
  (k :integer )
  (alpha :complex-double-float )
  (a (* :complex-double-float) )
  (lda :integer )
  (b (* :complex-double-float) )
  (ldb :integer )
  (beta :double-float )
  (c (* :complex-double-float) :output)
  (ldc :integer )
)
