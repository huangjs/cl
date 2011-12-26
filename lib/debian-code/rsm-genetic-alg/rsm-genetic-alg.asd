;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-genetic-alg.asd
;;;; Purpose:       ASDF Definition File For Package rsm.genetic-alg.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-genetic-alg.asd,v 1.3 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.genetic-alg.system (:use #:asdf #:cl))
(in-package rsm.genetic-alg.system)


(defsystem :rsm-genetic-alg
  :name "rsm-genetic-alg"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.0"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Genetic Algorithms."

  :depends-on (rsm-queue rsm-cache)
  
  :components
  ((:file "package")
   (:file "genetic-alg" :depends-on ("package"))
   ))


(defsystem :rsm-genetic-alg-test
  :depends-on (rsm-genetic-alg ptester)
  :components
  ((:file "genetic-alg-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-genetic-alg-test))))
  (operate 'load-op 'rsm-genetic-alg-test)
  (or (funcall (intern (symbol-name '#:run-genetic-alg-tests)
		       (find-package 'rsm.genetic-alg.test)))
      (error "test-op failed")))
