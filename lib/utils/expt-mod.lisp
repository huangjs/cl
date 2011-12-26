;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:nil -*-

(in-package #:hjs.util.math)


#|
One of the proposed Common Lisp utilities, modular exponentiation is important for many algorithms like Miller-Rabin and RSA. Although it can be implemented portably (see below), it would benefit from implementation-specific (i.e. fast) implementations.

Here is an example by Jochen Schmidt of how EXPT-MOD can be implemented in portable Common Lisp:
|#

(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))  
  (loop with result = 1
	 for i of-type fixnum from 0 below (integer-length exponent)
	 for sqr = n then (mod (* sqr sqr) modulus)
	 when (logbitp i exponent) do
	 (setf result (mod (* result sqr) modulus))
	 finally (return result)))

#|
Q: Why standardize an interface (well, a function name)? Lisp implementations that want to provide an accelerated (expt (mod ...)) can do that with a compiler macro, right? A: Yes, but if you rely on that compiler macro as programmer you will get horrible performance, if your implementation does not have an optimized algorithm for (expt (mod ...))
|#
