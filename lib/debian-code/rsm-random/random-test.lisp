;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.random.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          random-test.lisp
;;;; Purpose:       Regression testing for rsm.random
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: random-test.lisp,v 1.2 2003/08/21 19:57:54 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package #:cl-user)


(defpackage rsm.random.test
  (:use #:cl #:ptester #:rsm.random)
  (:documentation
   "Provides a test harness for random.")
  )


(in-package rsm.random.test)



;;;; RUN THE TESTS.


(defun do-tests ()

  (setf *break-on-test-failures* t)
  
  (with-tests (:name "RANDOM TESTS")

    (test (progn (init 199) t) t
          :fail-info "init call")

    (test (integerp (i-rand)) t
          :fail-info "i-rand integer")

    (test (typep (u-rand) 'double-float) t
	  :fail-info "u-rand double")

    (test (typep (b-rand) 'integer) t
	  :fail-info "b-rand integer")

    )
  
  ;; if made it here without breaking, all tests successful
  t)
