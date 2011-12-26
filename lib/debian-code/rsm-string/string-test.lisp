;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.string.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          string-test.lisp
;;;; Purpose:       Regression tests for the String Utilities package, 
;;;;                rsm.string.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: string-test.lisp,v 1.3 2003/09/10 22:19:26 rscottmcintire Exp $
;;;; *************************************************************************

(defpackage rsm.string.test
  (:use #:cl rsm.string #:ptester)
  (:documentation
   "Provides a test harness for the string utility code.")
  )


(in-package rsm.string.test)


;;;; RUN THE TESTS.


(defun run-string-tests ()

  (with-tests (:name "STRING TESTS")
    (test (list 123 t)
          (string->number "123")
          :multiple-values t
          :test #'equal
          :fail-info "Test 1")
    
    (test (list "abc" "def" "ghi")
          (split "abc def ghi")
          :test #'equal
          :fail-info "Test 2")
    
    (test "abc_def_ghi"
          (join '("abc" "def" "ghi") :join-string "_")
          :test #'equal
          :fail-info "Test 3")
    
    (test (list "abc" "ghi")
          (contains '("abc" "def" "ghi") '("bc" "gh"))
          :test #'equal
          :fail-info "Test 4")
    
    (test (list "def")
          (does-not-contain '("abc" "def" "ghi") '("ab" "gh"))
          :test #'equal
          :fail-info "Test 5")
    
    (test "123 + 345 + 567"
          (fluff-string "123+345+567" "+" :fluff-char #\Space )
          :test #'equal
          :fail-info "Test 6")
    )
  t
  )

