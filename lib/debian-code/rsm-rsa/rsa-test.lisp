;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.rsa.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rsa-test.lisp
;;;; Purpose:       Regression testing for RSA Encryption Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rsa-test.lisp,v 1.3 2003/09/10 22:19:26 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)


(defpackage rsm.rsa.test
  (:use #:cl #:rsm.rsa #:ptester)
  (:documentation
   "Provides a test harness for the RSA encryption code.")
  )


(in-package rsm.rsa.test)


;;;; RUN THE TESTS.


(defun run-rsa-tests ()
  
  (with-tests (:name "RSA TESTS")
    
    (test t
          (prob-prime-p 7019)
          :fail-info "Test 1")
    
    (let ((rsa-keys
           ;; Generate the encryption and decryption keys based on two primes.
           (generate-keys 1097050686241906813 
                                  1852326530172341711 
                                  "my-keys1"))
          (str "A test string"))
      
      (key->key-ring rsa-keys)
      (let ((enc (encrypt (rsa-encrypt-key rsa-keys) str)))
        (test (listp enc) t :fail-info "Test 2")
        (test (numberp (car enc)) t :fail-info "Test 3")
        (test (decrypt (rsa-decrypt-key rsa-keys) enc)
              str 
              :test #'string=
              :fail-info "Test 4: decrypt fails")))
    )
  t
  )
