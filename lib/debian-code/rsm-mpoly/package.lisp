;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for Multivariate polynomial Arithmetic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.mpoly
  (:use #:cl rsm.filter)
  (:shadow #:+ #:*)
  (:documentation
   "This package allows users to perform rudimentary computations with
multivariate polynomials. The user may also change the arithmetic modulus of the
computation to any integer greater than 1. When the modulus is a prime, the
default is to assume that the polynomials are over the field Z_p. In this case
any variable power X^p is equivalent to X; as such, this information is used to
reduce polynomials powers. If this behavior is not desired one may use the
function use-power-modulus with a null value to turn this behavior off.

REQUIRES: package rsm.filter.

Export Summary:

poly (&rest (c (&rest powers))) : Construct a polynomial.
lt (poly) : Returns the leading term (using the current ordering) 
            as (coeff . #(power-vector)).
lp (poly) : Returns the leading power (using the current ordering) 
            as #(power-vector).
lc (poly) : Returns the leading coefficient (using the current ordering).

+  (poly1 &rest polys)  : Add one or more polynomials.
*  (poly1 &rest polys)  : Multiply one or more polynomials.
^  (poly n)             : Raise a polynomial to a non-negative integer power.

poly-p (arg)  : True if argument is a polynomial.

mul-poly-term : Multiply a polynomial by a term.
poly-scalar-mul (c poly) : Multiply a polynomial by a scalar.
poly-scalar-add (c poly) : Add a scalar to  a polynomial.

set-order (lex-type) : Sets the order by which terms are compared.
get-order (lex-type) : Gets the order by which terms are compared.

set-modulus (n) : Sets the modulus. If <n> is nil, no modulus is used.
get-modulus (n) : Gets the modulus - nil by default 
                  (That is, no modulus is used by default.)
use-power-modulus (Bool) : Turn on or off the simplification x^modulus = x.
report-state () : Report the state of parameters that control 
                  polynomial computation.
get-state ()    : Get the state of parameters that control 
                  polynomial computation.
")  
  (:export 
   #:poly                               ; Construct a polynomial.
   #:lt                                 ; Return the leading term 
                                        ;   (using the current ordering) as 
                                        ;   (coeff . #(power-vector)).
   #:lp                                 ; Return the leading power 
                                        ;  (using the current ordering) 
                                        ;  as #(power-vector).
   #:lc                                 ; Return the leading coefficient 
                                        ;  (using the current ordering).
   #:+                                  ; Add one or more polynomials.
   #:*                                  ; Multiply one or more polynomials.
   #:^                                  ; Raise a polynomial to a non-negative 
                                        ;  integer power.
   #:poly-p                             ; Predicate to recognize a polynomial.
   #:mul-poly-term                      ; Multiply polynomial by a term.
   #:make-zero-poly                     ; Make a multivariate zero polynomial
                                        ; of a certain type.
   #:poly-scalar-mul                    ; Multiply a polynomial by a scalar.
   #:poly-scalar-add                    ; Add a scalar to a polynomial.
   #:set-order                          ; Set the order by which terms are 
                                        ;  compared.
   #:get-order                          ; Get the order by which terms are 
                                        ;  compared.
   #:set-modulus                        ; Set the arithmetic modulus.
   #:get-modulus                        ; Get the arithmetic modulus.
   #:report-state                       ; Report the state of parameters that 
                                        ;  control polynomial computation.
   #:get-state                          ; Get the state of parameters that 
                                        ;  control polynomial computation.
   #:term                               ; Construct a term (2 X1^3 X2^5)
   #:use-power-modulus                  ; Turn on or off the power modulus.
   ))
