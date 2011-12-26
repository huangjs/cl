;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.filter.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter-test.lisp
;;;; Purpose:       Regression testing for filtering functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: filter-test.lisp,v 1.3 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.filter.test
  (:use #:cl rsm.filter #:ptester)
  (:documentation
   "Provides a test harness for the rsm.filter package.")
  )

(in-package rsm.filter.test)


;;;; SET UP A DATA TO TEST FILTER CODE.

(defparameter *list* (list 1 2 3 4 5 6 7 8 9 10))
(defparameter *hom* nil)

;;;; RUN THE TESTS.


(defun run-filter-tests ()

  (with-tests (:name "FILTER TESTS")
    
    (test (list 2 4 6 8 10)
          (filter *list* #'oddp)
          :test #'equal
          :fail-info "Test 1")
    
    (test '(2 (4 (6) 4) 2)
          (prune-tree '(1 2 (3 4 (5) (6 7) 4) 2) #'oddp)
          :test #'equal
          :fail-info "Test 2")
    
    (test '(t t (t t (t) (t t) t) t)
          (tree-sig '(1 2 (3 4 (5) (6 7) 4) 2))
          :test #'equal
          :fail-info "Test 3")
    
    (test '(2 3 (4 5 (6) 7 8) 9)
          (map-tree  '(1 2 (3 4 (5) 6 7) 8) #'1+)
          :test #'equal
          :fail-info "Test 4")
    
    (test '(2 3 (4 5 (6) 7 8) 9)
          (map-tree  '(1 2 (3 4 (5) 6 7) 8) #'1+)
          :test #'equal
          :fail-info "Test 5")
    
    (test '(1 2 3 4 5 6 7 8)
          (flatten '(1 2 (3 4 (5) 6 7) 8))
          :test #'equal
          :fail-info "Test 6")
    
    (test '(c d a b e f g)
          (linearize '(a (b c a) (d e f) g a b e f g))
          :test #'equal
          :fail-info "Test 7")

    (test '(a b c d e f g)
          (linearize '(a (b c a) (d e f) g a b e f g) :from-end t)
          :test #'equal
          :fail-info "Test 8")

    
    (setf *hom* (tree-hom #'evenp #'(lambda (x) (+ x 10))))
    
    (test '(11 13 (13 15 (15 17) (17) (19)))
          (funcall *hom* '(1 2 3 (3 4 5 (5 6 7) (7) 8 (9 10))))
          :test #'equal
          :fail-info "Test 9")
    )
  t
  )

