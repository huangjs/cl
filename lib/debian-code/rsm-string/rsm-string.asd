;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-string.asd
;;;; Purpose:       ASDF Definition File For Package rsm.string.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-string.asd,v 1.5 2003/10/17 03:29:54 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.string.system (:use #:asdf #:cl))
(in-package rsm.string.system)


(defsystem :rsm-string
  :name "rsm-string"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.3"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "String utilities."
    
  :components
  ((:file "package")
   (:file "string" :depends-on ("package"))
   ))


(defsystem :rsm-string-test
  :depends-on (rsm-string ptester)
  :components
  ((:file "string-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-string-test))))
  (operate 'load-op 'rsm-string-test)
  (or (funcall (intern (symbol-name '#:run-string-tests)
		       (find-package 'rsm.string.test)))
      (error "test-op failed")))

