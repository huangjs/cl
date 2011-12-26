;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-modal.asd
;;;; Purpose:       ASDF Definition File For Package rsm.modal.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-modal.asd,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.modal.system (:use #:asdf #:cl))
(in-package rsm.modal.system)


(defsystem :rsm-modal
  :name "rsm-modal"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Modal Logic."
    
  :components
  ((:file "package")
   (:file "modal" :depends-on ("package"))
   ))


(defsystem :rsm-modal-test
  :depends-on (rsm-modal ptester)
  :components
  ((:file "modal-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-modal-test))))
  (operate 'load-op 'rsm-modal-test)
  (or (funcall (intern (symbol-name '#:run-modal-tests)
		       (find-package 'rsm.modal.test)))
      (error "test-op failed")))
