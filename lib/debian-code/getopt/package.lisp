;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for getopt package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2003
;;;;
;;;; $Id: package.lisp 8573 2004-01-29 23:30:50Z kevin $
;;;;
;;;; *************************************************************************

(in-package cl-user)

(defpackage getopt
  (:use #:cl)
  (:export
   #:match-unique-abbreviation
   #:getopt
   ))
