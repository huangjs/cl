;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          inflate.asd
;;;; Purpose:       ASDF definition file for Inflate
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: inflate.asd,v 1.4 2002/11/08 16:51:40 kevin Exp $
;;;;
;;;; This file, part of cl-inflate, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; cl-inflate users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU Lesser General Public License 
;;;; (http://www.gnu.org/licenses/lgpl.html)
;;;; *************************************************************************

(in-package :asdf)

#-allegro
(defsystem :inflate
  :name "cl-inflate"
  :author "John Foderaro, Franz, Inc"
  :version "1.1.4.2"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
  :licence "GNU Lesser General Public License"
  :description "Franz's Inflate compressed files module"
  :long-description "Inflate decompresses gzip, winzip and jar files."
  
  :perform (load-op :after (op inflate)
	    (pushnew :inflate cl:*features*))
  
  :components
  ((:file "inflate")))


