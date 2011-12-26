;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsm-fuzzy.asd
;;;; Purpose:       ASDF Definition File For Package rsm.fuzzy.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsm-fuzzy.asd,v 1.3 2003/09/17 02:13:34 rscottmcintire Exp $
;;;; *************************************************************************


(in-package #:cl-user)

(defpackage rsm.fuzzy.system (:use #:asdf #:cl))
(in-package rsm.fuzzy.system)


(defsystem :rsm-fuzzy
  :name "rsm-fuzzy"
  :author "R. Scott McIntire <rscottmcintire@users.sourceforge.net>."
  :version "1.1"
  :maintainer "Kevin M. Rosenberg <kmr@debian.org>."
  :licence "BSD-style"
  :description "Fuzzy Logic."
    
  :components
  ((:file "package")
   (:file "classes"      :depends-on ("package"))
   (:file "vars"         :depends-on ("package"))
   (:file "check"        :depends-on ("package" "vars"))
   (:file "introspect"   :depends-on ("package" "vars"))
   (:file "util"         :depends-on ("package" "introspect"))
   (:file "constructors" :depends-on ("package" "check" "vars"))
   (:file "protocol"     :depends-on ("package" "classes" "vars"))
   ))


(defsystem :rsm-fuzzy-test
  :depends-on (rsm-fuzzy ptester)
  :components
  ((:file "fuzzy-test")))


(defmethod perform ((o test-op) (c (eql (find-system 'rsm-fuzzy-test))))
  (operate 'load-op 'rsm-fuzzy-test)
  (or (funcall (intern (symbol-name '#:run-fuzzy-tests)
		       (find-package 'rsm.fuzzy.test)))
      (error "test-op failed")))
