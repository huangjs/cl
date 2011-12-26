;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          paip.asd
;;;; Purpose:       ASDF definition file for Paip
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: paip.asd 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file, part of cl-paip, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; cl-paip users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU Lesser General Public License 
;;;; (http://www.gnu.org/licenses/lgpl.html)
;;;; *************************************************************************

(in-package :asdf)

(defclass paip-module (module) ())

(defmethod perform ((op load-op) (c paip-module))
  nil)

(defmethod perform ((op compile-op) (c paip-module))
  nil)

(defsystem :paip
    :name "cl-paip"
    :author "Peter Norvig"
    :version "1.0"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "Peter Norvig's DFSG-compliant license"
    :description "Paradigms of Artificial Intelligence Programming Source Code"
    :long-description "Paradigms of Artificial Intelligence Program by Peter Norvig."
    :components ((:file "auxfns")
		 (:paip-module :paip :depends-on ("auxfns"))))

