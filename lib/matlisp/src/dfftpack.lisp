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
;;; May 5th, 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: dfftpack.lisp,v 1.5 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: dfftpack.lisp,v $
;;; Revision 1.5  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
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
;;; Revision 1.1  2000/05/05 21:35:54  simsek
;;; o Initial revision
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defpackage "DFFTPACK"
#+:cmu  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
#+:sbcl  (:use "COMMON-LISP" "SB-ALIEN" "SB-C" "FORTRAN-FFI-ACCESSORS")
#+:allegro  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS" "FORTRAN-FFI-ACCESSORS")
  (:export "ZFFTI" "ZFFTF" "ZFFTB"))

(in-package "DFFTPACK")


(def-fortran-routine zffti :void
  "
subroutine zffti initializes the array wsave which is used in
both zfftf and zfftb. the prime factorization of n together with
a tabulation of the trigonometric functions are computed and
stored in wsave.

input parameter

n       the length of the sequence to be transformed

output parameter

wsave   a work array which must be dimensioned at least 4*n+15
        the same work array can be used for both zfftf and zfftb
        as long as n remains unchanged. different wsave arrays
        are required for different values of n. the contents of
        wsave must not be changed between calls of zfftf or zfftb.

 "        
  (n :integer :input)
  (wsave (* :double-float) :output))

(def-fortran-routine zfftf :void
  "
subroutine zfftf computes the forward complex discrete fourier
transform (the fourier analysis). equivalently , zfftf computes
the fourier coefficients of a complex periodic sequence.
the transform is defined below at output parameter c.

the transform is not normalized. to obtain a normalized transform
the output must be divided by n. otherwise a call of zfftf
followed by a call of zfftb will multiply the sequence by n.

the array wsave which is used by subroutine zfftf must be
initialized by calling subroutine zffti(n,wsave).

input parameters


n      the length of the complex sequence c. the method is
       more efficient when n is the product of small primes. n

c      a complex array of length n which contains the sequence

wsave   a real work array which must be dimensioned at least 4n+15
        in the program that calls zfftf. the wsave array must be
        initialized by calling subroutine zffti(n,wsave) and a
        different wsave array must be used for each different
        value of n. this initialization does not have to be
        repeated so long as n remains unchanged thus subsequent
        transforms can be obtained faster than the first.
        the same wsave array can be used by zfftf and zfftb.

output parameters

c      for j=1,...,n

           c(j)=the sum from k=1,...,n of

                 c(k)*exp(-i*(j-1)*(k-1)*2*pi/n)

                       where i=sqrt(-1)

wsave   contains initialization calculations which must not be
        destroyed between calls of subroutine zfftf or zfftb
 "        
  (n :integer :input)
  (c (* :complex-double-float) :output)
  (wsave (* :double-float) :input))


(def-fortran-routine zfftb :void
  "
subroutine zfftb computes the backward complex discrete fourier
transform (the fourier synthesis). equivalently , zfftb computes
a complex periodic sequence from its fourier coefficients.
the transform is defined below at output parameter c.

a call of zfftf followed by a call of zfftb will multiply the
sequence by n.

the array wsave which is used by subroutine zfftb must be
initialized by calling subroutine zffti(n,wsave).

input parameters


n      the length of the complex sequence c. the method is
       more efficient when n is the product of small primes.

c      a complex array of length n which contains the sequence

wsave   a real work array which must be dimensioned at least 4n+15
        in the program that calls zfftb. the wsave array must be
        initialized by calling subroutine zffti(n,wsave) and a
        different wsave array must be used for each different
        value of n. this initialization does not have to be
        repeated so long as n remains unchanged thus subsequent
        transforms can be obtained faster than the first.
        the same wsave array can be used by zfftf and zfftb.

output parameters

c      for j=1,...,n

           c(j)=the sum from k=1,...,n of

                 c(k)*exp(i*(j-1)*(k-1)*2*pi/n)

                       where i=sqrt(-1)

wsave   contains initialization calculations which must not be
        destroyed between calls of subroutine zfftf or zfftb
 "        
  (n :integer :input)
  (c (* :complex-double-float) :output)
  (wsave (* :double-float) :input))
