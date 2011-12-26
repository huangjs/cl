;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-filter.asd
;;;; Purpose:       ASDF Definition File For Package rsm.filter.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-filter.asd,v 1.5 2003/09/17 01:50:03 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.filter.system (:use #:asdf #:cl))
(in-package rsm.filter.system)


(defsystem :rsm-filter
  :name "rsm-filter"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.1"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Filters for lists and trees."

  :depends-on (rsm-queue)
  
  :components
  ((:file "package")
   (:file "filter" :depends-on ("package"))
   ))


(defsystem :rsm-filter-test
  :depends-on (rsm-filter ptester)
  :components
  ((:file "filter-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-filter-test))))
  (operate 'load-op 'rsm-filter-test)
  (or (funcall (intern (symbol-name '#:run-filter-tests)
		       (find-package 'rsm.filter.test)))
      (error "test-op failed")))
