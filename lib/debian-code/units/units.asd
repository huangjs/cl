;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          units.asd
;;;; Purpose:       ASDF definition file for Units
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  April 21, 2003
;;;;
;;;; $Id: units.asd 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; *************************************************************************

(in-package :asdf)

(defsystem :units
  :name "cl-units"
  :author "Franz, Inc"
  :version "2003.04.18"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU General Public License"
  :description "Units conversion software"
  
  :components
  ((:file "unitsc")
   (:file "units" :depends-on ("unitsc"))))

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :units))))
  "lsp")
