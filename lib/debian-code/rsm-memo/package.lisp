;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for memoizing functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.1 2003/08/21 19:57:33 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.memo
  (:use #:cl)
  (:documentation
   "Provide function memoizers and function memo-caching functions of a single
input.

Export Summary:

defmemo : A macro that will define a memoized function.
defcache: A macro that will define a memoized function with a storage limit.
")
  (:export 
   #:defmemo
   #:defcache))
