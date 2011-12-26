;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for Delayed lists.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/08/21 19:57:11 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.delayed
  (:use #:cl)
  (:shadow #:car #:cdr #:cons #:list #:mapcar #:nth)
  (:documentation
   "Overloads the names of the usual set of list functions with stream versions
(delayed lists). These functions will work with ordinary lists or ordinary lists
combined with delayed lists.

REQUIRES: packages rsm.queue and rsm.filter.

Export Summary:

car       : Delayed list version of car.
cdr       : Delayed list version of cdr.
cycle     : Form a stream consisting of a repeated cycle.
drop      : Remove some number of elements from a delayed list.
delayed   : Type for delayed list.
delayed-p : Predicate for a delayed list.
list      : Delayed list version of list.
mapcar    : Mapcar for delayed lists.
nth       : Nth for delayed lists.
repeat    : Form a delayed list consisting of a repeated value.
take      : Return a list formed by retrieving some number of elements of
            a stream.
zip-with  : Form a delayed list by \"zipping\" together two
            delayed lists using a zip function.")
  (:export
   #:car
   #:cdr
   #:cons
   #:cycle
   #:drop
   #:filter
   #:delayed-p
   #:delayed
   #:list
   #:mapcar
   #:nth
   #:repeat
   #:take
   #:zip-with
   ))
