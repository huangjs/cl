;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.delayed.system -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-delayed.asd
;;;; Purpose:       ASDF definition file for package rsm.delayed.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-delayed.asd,v 1.1 2003/08/23 16:18:45 kevinrosenberg Exp $
;;;; *************************************************************************


(in-package cl-user)

(defpackage rsm.delayed.system (:use #:asdf #:cl))
(in-package rsm.delayed.system)


(defsystem rsm-delayed
  :name "rsm-delayed"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Delayed lists."

  :depends-on (rsm-queue rsm-filter)
  
  :components
  ((:file "package")
   (:file "delayed" :depends-on ("package"))
   ))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-delayed))))
  (operate 'load-op 'rsm-delayed-test)
  (operate 'test-op 'rsm-delayed-test :force t))

(defsystem rsm-delayed-test
  :depends-on (rsm-delayed ptester)
  :components
  ((:file "delayed-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-delayed-test))))
  (operate 'load-op 'rsm-delayed-test)
  (or (funcall (intern (symbol-name '#:run-delayed-tests)
		       (find-package 'rsm.delayed.test)))
      (error "test-op failed")))
