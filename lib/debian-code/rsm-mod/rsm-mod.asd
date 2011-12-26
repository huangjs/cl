;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-mod.asd
;;;; Purpose:       ASDF Definition File For Package rsm.mod.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-mod.asd,v 1.5 2003/10/21 20:59:44 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm-mod-system (:use #:asdf #:cl))
(in-package rsm-mod-system)


(defsystem :rsm-mod
  :name "rsm-mod"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.2"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Modular arithmetic."
    
  :components
  ((:file "package")
   (:file "mod" :depends-on ("package"))
   ))


(defsystem :rsm-mod-test
  :depends-on (rsm-mod ptester)
  :components
  ((:file "mod-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-mod-test))))
  (operate 'load-op 'rsm-mod-test)
  (or (funcall (intern (symbol-name '#:run-mod-tests)
		       (find-package 'rsm.mod.test)))
      (error "test-op failed")))
