;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-finance.asd
;;;; Purpose:       ASDF Definition File For Package rsm.finance.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-finance.asd,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.finance.system (:use #:asdf #:cl))
(in-package rsm.finance.system)


(defsystem :rsm-finance
  :name "rsm-finance"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Financial loan utilities."
    
  :components
  ((:file "package")
   (:file "finance" :depends-on ("package"))
   ))


(defsystem :rsm-finance-test
  :depends-on (rsm-finance ptester)
  :components
  ((:file "finance-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-finance-test))))
  (operate 'load-op 'rsm-finance-test)
  (or (funcall (intern (symbol-name '#:run-finance-tests)
		       (find-package 'rsm.finance.test)))
      (error "test-op failed")))
