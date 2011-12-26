;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-bool-comp.asd
;;;; Purpose:       ASDF Definition File For Package rsm.bool-comp.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-bool-comp.asd,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.bool-comp.system (:use #:asdf #:cl))
(in-package rsm.bool-comp.system)


(defsystem :rsm-bool-comp
  :name "rsm-bool-comp"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Modular arithmetic."
    
  :components
  ((:file "package")
   (:file "bool-comp" :depends-on ("package"))
   ))


(defsystem :rsm-bool-comp-test
  :depends-on (rsm-bool-comp ptester)
  :components
  ((:file "bool-comp-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-bool-comp-test))))
  (operate 'load-op 'rsm-bool-comp-test)
  (or (funcall (intern (symbol-name '#:run-bool-comp-tests)
		       (find-package 'rsm.bool-comp.test)))
      (error "test-op failed")))
