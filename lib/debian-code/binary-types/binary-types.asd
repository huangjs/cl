;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          binary-types.asd
;;;; Purpose:       ASDF definition file for Binary-Types
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: binary-types.asd 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; *************************************************************************

(in-package :asdf)

(defsystem :binary-types
  :name "cl-binary-types"
  :author "Frode Vatvedt Fjeld <frodef@acm.org>"
  :version "0.84"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU Lesser General Public License"
  :description "Library for reading and writing binary files"
  :long-description "Binary-types is a Common Lisp package for reading and writing binary files. Binary-types provides macros that are used to declare the mapping between lisp objects and some binary (i.e. octet-based) representation."
  
  :perform (load-op :after (op binary-types)
	    (pushnew :binary-types cl:*features*))
  
  :components
  ((:file "binary-types")))


