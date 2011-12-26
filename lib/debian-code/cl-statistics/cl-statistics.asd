;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cl-statistics.system
;;;; Purpose:       ASDF definition for cl-statistics package
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  06 Sep 2002
;;;;
;;;; Users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU Public License
;;;; *************************************************************************

(in-package :asdf)

(defsystem :cl-statistics
  :name "cl-statistics"
  :author "Larry Hunter <larry.hunter@uchsc.edu>"
  :version "1.0.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU General Public License"
  :description "Statistics package for Common Lisp"
  :long-description "cl-statistics provides numerous statistical functions for use in Common Lisp programs."
  
  :perform (load-op :after (op cl-statistics)
		    (pushnew :cl-statistics cl:*features*))
  :components 
  ((:file "cl-statistics"))
  )

