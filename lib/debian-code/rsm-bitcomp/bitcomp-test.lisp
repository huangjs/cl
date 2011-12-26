;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.bitcomp.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bitcomp-test.lisp
;;;; Purpose:       Regression testing for bit compression.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: bitcomp-test.lisp,v 1.1 2003/09/10 22:19:23 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.bitcomp.test
  (:use #:cl #:ptester)
  (:documentation
   "Provides a test harness for bit compression code.")
  )


(in-package rsm.bitcomp.test)


;;;; SET UP THE BIT COMPRESS DATA TO TEST.

(defparameter *b1* (rsm.bitcomp:make-compressed :list '(0 1 0 0 1 1 1 0 0 1)))
(defparameter *b2* (rsm.bitcomp:make-compressed :number 11))
(defparameter *b3* (rsm.bitcomp:make-compressed :list '(1 1 0 1)))
(defparameter *b4* (rsm.bitcomp:make-compressed :comp '((1 . 2) (2 . 3))))
(defparameter *b5* (rsm.bitcomp:make-compressed :comp '((1 . 4))))



;;;; RUN THE TESTS.

(defun run-bitcomp-tests ()
  (with-tests (:name "BIT COMPRESSION TESTS")

    (test (rsm.bitcomp:get-compressed-pairs *b2*)
          (rsm.bitcomp:get-compressed-pairs *b3*)
          :test #'equal
          :fail-info "Test 1")

    (test (rsm.bitcomp:get-compressed-pairs *b4*)
          (rsm.bitcomp:get-compressed-pairs *b5*)
          :test #'equal
          :fail-info "Test 2")

    (test '((2 . 1))
          (rsm.bitcomp:get-compressed-pairs (rsm.bitcomp:and *b1* *b2*))
          :test #'equal
          :fail-info "Test 3")
    
    (test '((1 . 2) (4 . 4) (10 . 1))
          (rsm.bitcomp:get-compressed-pairs (rsm.bitcomp:or *b1* *b2*))
          :test #'equal
          :fail-info "Test 4")
    
    (test '((1 . 1) (4 . 4) (10 . 1))
          (rsm.bitcomp:get-compressed-pairs (rsm.bitcomp:xor *b1* *b2*))
          :test #'equal
          :fail-info "Test 5")
    
    (test '((1 . 2) (4 . 1))
          (rsm.bitcomp:get-compressed-pairs *b2*)
          :test #'equal
          :fail-info "Test 6")
    
    (test '((1 . 1) (3 . 2) (8 . 2) (11 . 2))
          (rsm.bitcomp:get-compressed-pairs (rsm.bitcomp:not 1 12 *b1*))
          :test #'equal
          :fail-info "Test 7")

    (test nil
          (rsm.bitcomp:get-compressed-pairs (rsm.bitcomp:make-compressed 
                                             :rep (rsm.queue:create)))
          :test #'equal
          :fail-info "Test 8")

    (test '((1 . 6) (10 . 2))
          (rsm.bitcomp:get-compressed-pairs 
           (rsm.bitcomp:or
            (rsm.bitcomp:make-compressed :comp '((1 . 3) (5 . 2)))
            (rsm.bitcomp:make-compressed :comp '((4 . 2) (10 . 2)))))
          :test #'equal
          :fail-info "Test 9")

    (test '((5 . 1))
          (rsm.bitcomp:get-compressed-pairs 
           (rsm.bitcomp:and
            (rsm.bitcomp:make-compressed :comp '((1 . 3) (5 . 2)))
            (rsm.bitcomp:make-compressed :comp '((4 . 2) (10 . 2)))))
          :test #'equal
          :fail-info "Test 10")
    
    (test '((1 . 4) (6 . 1) (10 . 2))
          (rsm.bitcomp:get-compressed-pairs 
           (rsm.bitcomp:xor 
            (rsm.bitcomp:make-compressed :comp '((1 . 3) (5 . 2)))
            (rsm.bitcomp:make-compressed :comp '((4 . 2) (10 . 2)))))
          :test #'equal
          :fail-info "Test 11")
    
    
    (test '((0 . 2) (3 . 2) (8 . 2) (11 . 1))
          (rsm.bitcomp:get-compressed-pairs 
           (rsm.bitcomp:not 0 11 (rsm.bitcomp:make-compressed 
                                  :list '(0 1 0 0 1 1 1 0 0 1))))
          :test #'equal
          :fail-info "Test 12")
    )
  t
  )
