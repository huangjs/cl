;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.finance.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          finance-test.lisp
;;;; Purpose:       Regression testing for financial loan utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: finance-test.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.finance.test
  (:use #:cl rsm.finance #:ptester)
  (:documentation
   "Provides a test harness for financial loan utilities.")
  )


(in-package rsm.finance.test)


;;;; INFRASTRUCTURE FUNCTIONS AND VARIABLES.

(defvar *tolerance* 1.0d-5
  "The tolerance used to decide floating point equality.")

(defun float-equal (x y)
  "Equal if relative of x and y is less than +tolerance+.
When +tolerance+ is 1.0d-4, then equality means that <x> and <y> 
are equal with a relative error of a hundredth of a percent."
  (if (and (= 0.0d0 x) (= 0.0d0 y))
      t
    (let ((max (max (abs x) (abs y))))
      (< (/ (abs (- x y)) max) *tolerance*))))



;;;; RUN THE TESTS.

(defun run-finance-tests ()

  (with-tests (:name "LOAN FINANCE TESTS")
    
    ;; Group 1
    (test 1330.60d0
          (calc-monthly-payment 200000.0 30 7.0)
          :test #'float-equal
          :fail-info "Test 1")
    
    (test 200000.0d0
          (calc-initial-debt 30 7.0 1330.60)
          :test #'float-equal
          :fail-info "Test 2")
    
    (test 7.0d0
          (calc-interest-rate 200000.0 1330.60 30)
          :test #'float-equal
          :fail-info "Test 3")
    
    (test '(361 30 1 479022.11787691835d0 6.117876918363281d0)
          (calc-num-payments 200000.0 7.0 1330.60)
          :test #'float-equal
          :multiple-values t
          :fail-info "Test 4")

    
    ;; Group 2
    (test 716.43d0
          (calc-monthly-payment 100000.0 20 6.0)
          :test #'float-equal
          :fail-info "Test 5")
    
    (test 100000.0d0
          (calc-initial-debt 20 6.0 716.43)
          :test #'float-equal
          :fail-info "Test 6")
    
    (test 6.0d0
          (calc-interest-rate 100000.0 716.43 20)
          :test #'float-equal
          :fail-info "Test 7")

    (test '(241 20 1 171943.69556928397d0 0.4924442839637882d0)
          (calc-num-payments 100000.0 6.0 716.43)
          :test #'float-equal
          :multiple-values t
          :fail-info "Test 8")

    
    ;; Group 3
    (test 278.49d0
          (calc-monthly-payment 15000.0 5 4.33d0)
          :test #'float-equal
          :fail-info "Test 9")
    
    (test 15000.0d0
          (calc-initial-debt 5 4.33 278.49)
          :test #'float-equal
          :fail-info "Test 10")
    
    (let ((*tolerance* 1.0d-4))
      
      (test 4.33d0
            (calc-interest-rate 15000.0 278.49 5)
            :test #'float-equal
            :fail-info "Test 11")
      
      (test '(60 5 0 16708.217007688505d0 277.30685143850604d0)
            (calc-num-payments 15000.0 4.33 278.49)
            :test #'float-equal
            :multiple-values t
            :fail-info "Test 12")

      
      ;; Group 4
      (test 0.0054549d0
            (calc-interest-rate 200000.0 556.00 30)
            :test #'float-equal
            :fail-info "Test 13")
      
      (test 556.0d0
            (calc-monthly-payment 200000.0 30 0.0054549)
            :test #'float-equal
            :fail-info "Test 14")
      
      (test 200000.0d0
            (calc-initial-debt 30 0.0054549 556.00)
            :test #'float-equal
            :fail-info "Test 15")
      
      (test '(361 30 1 200164.14959972943d0 4.149599729439264d0)
            (calc-num-payments 200000.0 0.0054549 556.00)
            :test #'float-equal
            :multiple-values t
            :fail-info "Test 16")
      )


    ;; Group 5
    (test nil
          (calc-interest-rate 200000.0 555.00 30)
          :fail-info "Test 17")

    
    ;; Group 6
    (test-error 
     (calc-interest-rate 200000.0 555.00 -30)
     :fail-info "Test 18")
    
    (test-error 
     (calc-interest-rate 200000.0 -555.00 30)
     :fail-info "Test 19")
    
    (test-error 
     (calc-interest-rate -200000.0 555.00 30)
     :fail-info "Test 20")
    
    (test-error 
     (calc-interest-rate -200000.0 555.00 -30)
     :fail-info "Test 21")
    
    (test-error 
     (calc-interest-rate -200000.0 -555.00 30)
     :fail-info "Test 22")
    
    (test-error 
     (calc-interest-rate -200000.0 -555.00 -30)
     :fail-info "Test 23")

    
    ;; Group 7
    (test-error 
     (calc-num-payments -15000.0 4.33 278.49)
     :fail-info "Test 24")
    
    (test-error 
     (calc-num-payments 15000.0 -4.33 278.49)
     :fail-info "Test 25")
    
    (test-error 
     (calc-num-payments 15000.0 4.33 -278.49)
     :fail-info "Test 26")
    
    (test-error 
     (calc-num-payments -15000.0 -4.33 278.49)
     :fail-info "Test 27")

    (test-error 
     (calc-num-payments -15000.0 -4.33 -278.49)
     :fail-info "Test 28")

    
    ;; Group 8
    (test-error
     (calc-monthly-payment -15000.0 5 4.33d0)
     :fail-info "Test 29")

    (test-error
     (calc-monthly-payment 15000.0 -5 4.33d0)
     :fail-info "Test 30")
    
    (test-error
     (calc-monthly-payment 15000.0 5 -4.33d0)
     :fail-info "Test 31")
    
    (test-error
     (calc-monthly-payment -15000.0 -5 4.33d0)
     :fail-info "Test 32")

    (test-error
     (calc-monthly-payment -15000.0 5 -4.33d0)
     :fail-info "Test 33")

    
    (test-error
     (calc-monthly-payment -15000.0 -5 -4.33d0)
     :fail-info "Test 34")


    
    ;; Group 9
    (test-error
     (calc-initial-debt -30 7.0 1330.60)
     :fail-info "Test 35")

    (test-error
     (calc-initial-debt 30 -7.0 1330.60)
     :fail-info "Test 36")

    (test-error
     (calc-initial-debt 30 7.0 -1330.60)
     :fail-info "Test 37")

    
    (test-error
     (calc-initial-debt -30 7.0 -1330.60)
     :fail-info "Test 38")

    (test-error
     (calc-initial-debt -30 -7.0 1330.60)
     :fail-info "Test 39")


    (test-error
     (calc-initial-debt 30 -7.0 -1330.60)
     :fail-info "Test 40")

    
    (test-error
     (calc-initial-debt -30 -7.0 -1330.60)
     :fail-info "Test 41")

    )
  t
  )
