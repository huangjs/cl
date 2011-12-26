;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for bit compression.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.1 2003/09/10 22:19:23 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.bitcomp
  (:use #:cl #:rsm.queue)
  (:shadow #:merge #:or #:and #:not #:diff #:intersect)
  (:documentation
   "Overview: This package provides a set of functions to 
create, and perform boolean operations on compressed bit strings.
Effectively, a run length encoding of bits is used as a representation.

REQUIRES: package rsm.queue.

REPRESENTATION DESCRIPTION: 
Operations on compressed bit strings.
The representation of compressed bit strings is 
of the form '((start1 . duration1) (start2 . duration2)...).

Example: The compressed bit string 
         '((1 . 3) (6 . 2) (10 . 4))
         is interpreted to mean the bit string:
         1 1 1 0 0 1 1 0 0 1 1 1 1
         That is, the first three bits are 1, 
         bits 6 and 7 are 1, and bits 10 through 13 are 1.


A compressed bit string may be created using the constructor make-compressed 
with 4 key words: :list, :number, :comp, and :rep.

Example: Example bit string constructions and the resulting compressed pairs:
         Note: Compressed pairs are not what is returned by make-compressed
               but representative of the returned structure.

(rsm.bitcomp:make-compressed :list '(0 1 0 0 1 1 1 0 0 1))
  ((2 . 1) (5 . 3) (10 . 1))

(rsm.bitcomp:make-compressed :number 11)  ; The bits of 11 are considered 
                                          ; running from lowest to highest.
  ((1 . 2) (4 . 1))

(rsm.bitcomp:make-compressed :comp '((1 . 2) (2 . 3))) ; Make from compressed 
                                                       ; pairs.
  ((1 . 4))

(rsm.bitcomp:make-compressed :rep '(((1 . 2) (4 . 3)) (4 . 3)) ; Make from 
                                                             ; compressed queue.
  ((1 . 2) (4 . 3))

  Note: Use get-compressed-pairs to return a list of compressed pairs from 
  a object of type compressed-p.
  
Export Summary:
  
and: And zero or more compressed bit strings.
or : Or zero or more compressed bit strings.
not: Negate a compressed bit string.
xor: Xor zero or more compressed bit strings.

get-number-of-bits   : The the number of bits which are 1.
get-compressed-pairs : Get the list of compressed pairs.
make-compressed      : Make a compressed bit-string structure.
compressed-p         : Is an object a compressed structure?
")
  (:export 
   #:and                                ; And bit strings.
   #:not                                ; Complement of a bit string.
   #:or                                 ; Or bit strings.
   #:xor                                ; Xor bit strings.
   #:get-number-of-bits                 ; Get the number of bits in the rep.
   #:get-compressed-pairs               ; Get the list of compressed pairs.
   #:make-compressed                    ; Make a compressed bit string.
   #:compressed-data                    ; Get the queue representing 
                                        ;  the compressed bit string.
   #:compressed-p))                     ; Bit string predicate.
