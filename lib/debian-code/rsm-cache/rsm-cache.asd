;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-cache.asd
;;;; Purpose:       ASDF Definition File For Package rsm.cache.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-cache.asd,v 1.5 2003/09/17 02:06:02 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.cache.system (:use #:asdf #:cl))
(in-package rsm.cache.system)


(defsystem :rsm-cache
  :name "rsm-cache"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.1"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Cache objects of high numeric rank."

  :depends-on (rsm-queue)
  
  :components
  ((:file "package")
   (:file "cache" :depends-on ("package"))
   ))


(defsystem :rsm-cache-test
  :depends-on (rsm-cache ptester)
  :components
  ((:file "cache-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-cache-test))))
  (operate 'load-op 'rsm-cache-test)
  (or (funcall (intern (symbol-name '#:run-cache-tests)
		       (find-package 'rsm.cache.test)))
      (error "test-op failed")))
