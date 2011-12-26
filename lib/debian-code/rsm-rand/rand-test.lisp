;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.rand.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rand-test.lisp
;;;; Purpose:       Regression testing for package rsm.rand.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rand-test.lisp,v 1.3 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.rand.test
  (:use #:cl #:ptester)
  (:documentation
   "Provides a test harness for discrete random number generators.")
  )


(in-package rsm.rand.test)


;;;; SET UP A RAND TO TEST.

(defparameter *rand* (rsm.rand:make-standard-randgen 
                      '((1 0.2) (2 0.25) (3 0.25) (4 0.3))))



;;;; RUN THE TESTS.

(defun run-rand-tests ()

  (with-tests (:name "RAND TESTS")
    
    
    (test #(0.2 0.45 0.7 1.0)
          (rsm.rand:rand-dist *rand*)
          :test #'equalp
          :fail-info "Test 1")
    
    (test '((10 0.25) (11 0.25) (12 0.2) (13 0.3))
          (setf (rsm.rand:rand-val-dens *rand*) 
            '((10 0.25) (11 0.25) (12 0.2) (13 0.3)))
          :test #'equalp
          :fail-info "Test 2")

    (test #(0.25 0.5 0.7 1.0)
          (rsm.rand:rand-dist *rand*)
          :test #'equalp
          :fail-info "Test 3")

    )
  t
  )



