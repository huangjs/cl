;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cluck.asd
;;;; Purpose:       ASDF definition file for CLUCK
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  March 2007
;;;;
;;;; $Id: cluck.asd 11565 2007-03-09 07:39:31Z kevin $
;;;;
;;;; *************************************************************************

(defpackage #:cluck-system (:use #:asdf #:cl))
(in-package #:cluck-system)

(defsystem cluck
  :name "cluck"
  :maintainer "Kevin M. Rosenberg"
  :licence "BSD"
  :description "Common Lisp uController Clock Calculator"
  :long-description "CLUCK provides functions to programming timers and selecting crystals for microcontrollers."
  :components
  ((:file "cluck")))
