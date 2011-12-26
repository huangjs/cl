;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.cache.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cache-test.lisp
;;;; Purpose:       Regression testing for cache package.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: cache-test.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.cache.test
  (:use #:cl rsm.cache #:ptester)
  (:documentation
   "Provides a test harness for cache package.")
  )


(in-package rsm.cache.test)


;;;; SET UP VARS

(defparameter *all* nil)
(defparameter *obj1* nil)
(defparameter *obj2* nil)
(defparameter *obj3* nil)
(defparameter *obj4* nil)
(defparameter *obj5* nil)
(defparameter *obj6* nil)
(defparameter *val1* nil)
(defparameter *val2* nil)
(defparameter *val3* nil)
(defparameter *val4* nil)
(defparameter *val5* nil)
(defparameter *val6* nil)


(defparameter *cache* (make-standard-cache "test1" 5 :threshold 0))


;;;; RUN THE TESTS.


(defun run-cache-tests ()

  (with-tests (:name "CACHE TESTS")
    
    (setf *obj1* (list 'a 'b 'c))
    (setf *val1* 1)
    (cache-if-large *cache* *obj1* *val1*)

    (test *val1* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 1")
    
    (setf *obj2* (list 'a 'b))
    (setf *val2* 2)
    (cache-if-large *cache* *obj2* *val2*)
    
    (test *val1* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 2")
    
    (setf *obj3* (list 'a 'c))
    (setf *val3* 3)
    (cache-if-large *cache* *obj3* *val3*)
    
    (test *val1* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 3")
    
    (setf *obj4* (list 'b 'c))
    (setf *val4* 4)
    (cache-if-large *cache* *obj4* *val4*)
    
    (test *val1* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 4")
    
    (setf *obj5* (list 'c))
    (setf *val5* 5)
    (cache-if-large *cache* *obj5* *val5*)
    
    (test *val1* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 5")
    
    (setf *obj6* (list 'd))
    (setf *val6* 6)
    (cache-if-large *cache* *obj6* *val6*)
    
    (test *val2* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 6")
    
    (cache-if-large *cache* *obj6* *val6*)
    
    (test *val2* 
          (cache-min-value *cache*)
          :test #'equal
          :fail-info "Test 7")
    
    (setf *all* 
        '((6 ((d))) (5 ((c))) (4 ((b c))) (3 ((a c))) (2 ((a b)))))
    
    (test *all*
          (retrieve-obj-cache *cache*)
          :test #'equal
          :fail-info "Test 8")

    (setf *cache* (make-standard-cache "test2" 5 
                                       :threshold 0 
                                       :cache-list-limit 3)) 
    
    (cache-if-large *cache* 'a 10)
    (cache-if-large *cache* 'b 2)
    (cache-if-large *cache* 'c 3)
    (cache-if-large *cache* 'x 4)
    (cache-if-large *cache* 'y 5)
    (cache-if-large *cache* 'z 6)
    (cache-if-large *cache* 'b 10)
    (cache-if-large *cache* 'd 10)
    (cache-if-large *cache* 'e 10)
    (cache-if-large *cache* 'd 10)

    
    (test '((10 (d b a)) (6 (z)) (5 (y)) (4 (x)) (3 (c)))
          (retrieve-obj-cache *cache*)
          :test #'equal
          :fail-info "Test 9")

    
    (setf *cache* (make-standard-cache "test3" 4 
                                       :threshold 0 
                                       :cache-list-limit 2))
    
    (cache-if-large *cache* 'a 10)
    (cache-if-large *cache* 'b 2)
    (cache-if-large *cache* 'c 3)
    (cache-if-large *cache* 'b 10)
    (cache-if-large *cache* 'd 10)
    (cache-if-large *cache* 'e 10)
    (cache-if-large *cache* 'd 10)

    
    (test '((10 (b a)) (3 (c)) (2 (b)))
          (retrieve-obj-cache *cache*)
          :test #'equal
          :fail-info "Test 10")

    )
  t
  )
