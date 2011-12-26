;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-mpoly.asd
;;;; Purpose:       ASDF Definition File For Package rsm.mpoly.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-mpoly.asd,v 1.3 2003/10/20 11:55:43 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.mpoly.system (:use #:asdf #:cl))
(in-package rsm.mpoly.system)


(defsystem :rsm-mpoly
  :name "rsm-mpoly"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.1"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Modular arithmetic."

  :depends-on (rsm-filter)
  
  :components
  ((:file "package")
   (:file "mpoly" :depends-on ("package"))
   ))


(defsystem :rsm-mpoly-test
  :depends-on (rsm-mpoly ptester)
  :components
  ((:file "mpoly-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-mpoly-test))))
  (operate 'load-op 'rsm-mpoly-test)
  (or (funcall (intern (symbol-name '#:run-mpoly-tests)
		       (find-package 'rsm.mpoly.test)))
      (error "test-op failed")))
