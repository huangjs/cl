;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for caching functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.cache
  (:use #:cl)
  (:documentation
   "Provides a class that caches a limited number of numerically ranked objects.
The constructor make-standard-cache creates a cache and the method 
cache-if-large is used to cache objects of large rank.

REQUIRES: package rsm.queue.

Export Summary:

make-standard-cache: Create an instance of a cache.
cache-if-large     : Cache an object if it is large than the current values in 
                     the cache.
cache-min-value    : Find the minimum value of the cache.

clear-and-set-obj-cache-size: Clear the cache and set its size.

retrieve-obj-cache : Retrieve the cache as a list. Each element of the 
                     list has the form (value (list-of-objs)).
")
  (:export 
   #:abstract-cache
   #:standard-cache
   #:make-standard-cache
   #:cache-if-large
   #:cache-min-value
   #:clear-and-set-obj-cache-size 
   #:retrieve-obj-cache))

