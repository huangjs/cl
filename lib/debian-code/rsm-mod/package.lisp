;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for Modular arithmetic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.4 2003/10/03 02:21:53 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.mod
  (:use #:cl)
  (:shadow #:+ #:* #:^)
  (:documentation
   "This package supports modular arithmetic.

Export Summary:

+: Add numbers over Z mod n.
*: Multiply numbers over Z mod n.
^: Exponentiate over Z mod n.


euler-phi: Return the Euler phi function of a number.
factors  : Return the factors of a number.
gcd-with-pair: Gets the gcd of two numbers a and b returning also 
    the integer pair, (r s), such that r*a + s*b = gcd(a,b).
has-inverse-p: Does a number have in inverse in Z mod n?
inverse  : Find the inverse (if it exists) in Z mod n.
ppow     : Exponentiate over Z mod p where p is prime.

rational-approx: Returns a simple rational approximation 
                 within a given tolerance.
solve-congruence-system: Solve for x: x = a_i mod m_i; i in [1,N]
")
   (:export 
   #:+ 
   #:* 
   #:^ 
   #:euler-phi
   #:factors
   #:gcd-with-pair
   #:has-inverse-p
   #:inverse
   #:ppow
   #:rational-approx
   #:solve-congruence-system))


