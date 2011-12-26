;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for filtering functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.3 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.filter
  (:use #:cl)
  (:documentation 
   "This package provides filtering and mapping functions.

REQUIRES: package rsm.queue.

Export Summary:

filter    : Filter a list based on a predicate.
flatten   : Flatten a tree to a list.
linearize : Flatten a tree removing duplicates.
map-tree  : Map a function over the leaves of a tree.
prune-tree: Prune a tree using a predicate.
tree-hom  : Create a function which prunes and transforms trees.
tree-sig  : Create a tree of the same structure with t in every leaf.")
  (:export 
   #:filter
   #:flatten
   #:linearize
   #:map-tree
   #:prune-tree
   #:tree-hom
   #:tree-sig))
