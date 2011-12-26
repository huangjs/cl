;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.mod.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mod-test.lisp
;;;; Purpose:       Regression testing for modular arithmetic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: mod-test.lisp,v 1.7 2003/10/21 20:59:44 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.mod.test
  (:use #:cl #:ptester)
  (:documentation
   "Provides a test harness for modular arithmetic.")
  )


(in-package rsm.mod.test)



;;;; RUN THE TESTS.


(defun run-mod-tests ()
  
  (with-tests (:name "MOD TESTS")

    (test 2
          (rsm.mod:+ 3 3 5)
          :fail-info "Test 1")
    
    (test 1
          (rsm.mod:* 3 2 5)
          :fail-info "Test 2")
    
    (test 2
          (rsm.mod:ppow 12 100 7)
          :fail-info "Test 3")
    
    (test 170
          (rsm.mod:^ 213317 527131763 173)
          :fail-info "Test 4")
    
    (test '(2 5)
          (rsm.mod:factors 100)
          :test #'equal
          :fail-info "Test 5")
    
    (test '(2 2 5 5)
          (rsm.mod:factors 100 :no-dups nil)
          :test #'equal
          :fail-info "Test 6")

    (test 12
          (rsm.mod:euler-phi 13)
          :fail-info "Test 7")
    
    (test 40
          (rsm.mod:euler-phi 100)
          :fail-info "Test 8")
    
    (test 9
          (rsm.mod:inverse 9 10)
          :fail-info "Test 9")

    (test 1529
          (rsm.mod:inverse 2341 7919)
          :fail-info "Test 10")

    (test 15651
          (rsm.mod:ppow 7919 7232937498729837429 104729)
          :fail-info "Test 11")
    
    (test 777/898
          (rsm.mod:rational-approx (/ 2.71828 3.14159) 
                                   0.000001)
          :fail-info "Test 12")
    
    (test 22/7
          (rsm.mod:rational-approx pi 0.002)
          :fail-info "Test 13")

    (test 355/113
          (rsm.mod:rational-approx pi 0.001)
          :fail-info "Test 14")

    (test 152974058/176796123
          (rsm.mod:rational-approx 27182845904523536/31415926535897932
                                   0.0000000000000001)
          :fail-info "Test 15")

    (test '(12 (1 0))
          (rsm.mod:gcd-with-pair 12 60)
          :test #'equal
          :multiple-values t
          :fail-info "Test 16")
    
    (test '(2 (1 -2))
          (rsm.mod:gcd-with-pair 14 6)
          :test #'equal
          :multiple-values t
          :fail-info "Test 17")
    
    (test '(2 (-2 1))
          (rsm.mod:gcd-with-pair 6 14)
          :test #'equal
          :multiple-values t
          :fail-info "Test 18")
    
    (test '(1 (-1035 676))
          (rsm.mod:gcd-with-pair 1529 2341)
          :test #'equal
          :multiple-values t
          :fail-info "Test 19")
    
    (test '(2 (-502648 26455))
          (rsm.mod:gcd-with-pair 123456 2345678)
          :test #'equal
          :multiple-values t)
    
    (test 15651
          (rsm.mod:ppow 7919 7232937498729837429 104729)
          :fail-info "Test 21")
    
    (test 21762
          (rsm.mod:ppow 7919 72329374987298374298 104729)
          :fail-info "Test 22")
    
    (test 43685
          (rsm.mod:ppow 7919 723293749872983742983 104729)
          :fail-info "Test 23")

    (test 43685
          (rsm.mod:^ 7919 723293749872983742983 104729 
                     :e-phi 104728)
          :fail-info "Test 24")
    
    (test 43685
          (rsm.mod:^ 7919 723293749872983742983 104729)
          :fail-info "Test 25")

    (test 56170
          (rsm.mod:^ 79111 723293749872983742983 104727)
          :fail-info "Test 26")
    
    (test 355/113
          (rsm.mod:rational-approx pi 0.0000003)
          :fail-info "Test 27")

    (test 12317
          (rsm.mod:solve-congruence-system '(1 2 2 4 8 6) '(2 3 5 7 11 13))
          :fail-info "Test 28")
    
    (test 29243
          (rsm.mod:solve-congruence-system '(1 2 3 4 5 6) '(2 3 5 7 11 13))
          :fail-info "Test 29")
    
    (test 54916118429448
          (rsm.mod:solve-congruence-system '(1 2 3 4 5 6)
                                           '(7909 101 13 37 97 2003))
          :fail-info "Test 30")
    
    (test 0
          (rsm.mod:solve-congruence-system '(0 0 0) '(2 3 5))
          :fail-info "Test 31")
    
    (test 23
          (rsm.mod:solve-congruence-system '(1 2 3) '(2 3 5))
          :fail-info "Test 32")
    
    (test t
          (rsm.mod:has-inverse-p 123 713)
          :fail-info "Test 33")
    
    (test nil
          (rsm.mod:has-inverse-p 123 717)
          :fail-info "Test 34")
    
    (test nil
          (rsm.mod:has-inverse-p 3 12)
          :fail-info "Test 35")
    
    (test t
          (rsm.mod:has-inverse-p 3 8)
          :fail-info "Test 36")
  
    (test 16041953
          (rsm.mod:solve-congruence-system '(1 2 3 4 5)
                                           '(8 9 25 77 221))
          :fail-info "Test 37")
    
    (test 0
          (rsm.mod:inverse 8 10)
          :fail-info "Test 38")
    
    (test nil
          (rsm.mod:inverse 8 10 nil nil)
          :fail-info "Test 39")
    
    (test-error (rsm.mod:inverse 8 10 t)
                :fail-info "Test 40")
    
    (test -1
          (rsm.mod:inverse 8 10 nil -1)
          :fail-info "Test 41")
    
    (test 3
          (rsm.mod:inverse 7 10 nil -1)
          :fail-info "Test 42")
    
    (test 28
          (rsm.mod:^ 7 2134145213317 33 :e-phi 20)
          :fail-info "Test 43")
    )
  t
  )
