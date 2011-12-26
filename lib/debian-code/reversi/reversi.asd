;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           reversi.asd
;;;;  Purpose:        Defsystem for reversi
;;;;  Programer:      Kevin M. Rosenberg
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: reversi.asd 7995 2003-10-19 23:16:03Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg 
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(defpackage #:reversi-system (:use #:asdf #:cl))
(in-package #:reversi-system)

#+(and allegro linux (not allegro-cl-trial)) (require :climxm)
#+(and allegro mswindows (not allegro-cl-trial)) (require :climnt)
#+(and lispworks (not lispworks-personal-edition)) (require "clim")

(defsystem :reversi 
    :components 
    ((:file "package")
     (:file "utils" :depends-on ("package"))
     (:file "base" :depends-on ("utils"))
     (:file "io" :depends-on ("base"))
     (:file "edge-table" :depends-on ("io"))
     (:file "edge-table-storage"  :depends-on ("edge-table"))
     (:file "strategies" :depends-on ("edge-table-storage"))
     #+clim (:file "io-clim" :depends-on ("strategies"))
    ))
