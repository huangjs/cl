;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vars.lisp
;;;; Purpose:       Variables And Constants.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: vars.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; PARAMETERS/CONSTANTS

;;;; Need this to get compile time evaluation.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defconstant +epsilon+ 1.0e-6)
  (defconstant +adjs+ 'adjs)
  (defconstant +vars+ 'vars)
  (defconstant +one-third+ 0.33333333)
  (defconstant +one-half+ 0.5)
  (defconstant +zero+ 0.0))


(defvar *fuzzy-sys* nil
  "The current Fuzzy System.")

(defvar *fuzzy-hash* (make-hash-table :test #'equal)
  "A hash table of fuzzy systems.")
