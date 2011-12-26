;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          foreign-loader.lisp
;;;; Purpose:       Loads foreign libraries
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Feb 2002
;;;;
;;;; $Id: foreign-loader.lisp,v 1.5 2003/08/23 16:16:36 kevinrosenberg Exp $
;;;;
;;;; *************************************************************************

;;; For CMUCL, it's necessary to load foreign files separate from their
;;; usage

(in-package #:cl-user)

(unless (uffi:load-foreign-library 
	 (uffi:find-foreign-library "random" 
				    (list
				     (pathname-directory *load-truename*)
				     "/usr/lib/rsm-random/"
				     "../ff/"))
	 :supporting-libraries '("c")
	 :module "rsm-random")
  (warn "Unable to load rsm-random library"))

