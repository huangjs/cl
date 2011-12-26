;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.delayed.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delayed-test.lisp
;;;; Purpose:       Regression testing for delayed lists.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: delayed-test.lisp,v 1.2 2003/08/21 19:57:11 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.delayed.test
  (:use #:cl #:ptester)
  (:documentation
   "Provides a test harness for the delayed package.")
  )


(in-package rsm.delayed.test)


;;;; SET UP FUNCTIONS AND DELAYED LISTS TO TEST.

;;; Integers
(defun integers (&optional (start 0))
  "The integers."
  (rsm.delayed:cons start (integers (1+ start))))

;;; Fibonacci sequence
(defun fib ()
  "The Fibonacci sequence."
  (labels 
      ((rec (x y)
         (rsm.delayed:cons x (rec y (+ x y)))))
    (rec 1 1)))

(defun fib2 ()
  "Another version of the Fibonacci sequence."
  (rsm.delayed:cons 1 
                    (rsm.delayed:cons 1 
                    (rsm.delayed:zip-with (fib2) 
                    (rsm.delayed:drop 1 (fib2)) #'+))))

(defun fib3 (n)
  (let ((n0 1)
        (n1 1))
    (loop 
      :repeat (1- n) :do
      (psetq 
          n0 n1
          n1 (+ n0 n1)))
    n0))


(defun primes ()
  (let ((primes-so-far (rsm.queue:list->queue (list 2 3 5))))
    (labels ((rec (n even)
               (let (next-n sqrt-next-n)
                 (if even
                     (setf next-n (+ n 2))
                   (setf next-n (+ n 4)))
                 (setf sqrt-next-n (isqrt next-n))
                 (if (loop :for prime :in 
                       (rsm.queue:nget-list primes-so-far) :do
                       (cond ((> prime sqrt-next-n)
                              (return next-n))
                             ((= (mod next-n prime) 0)
                              (return nil)))
                       :finally (return next-n))
                     (progn
                       (rsm.queue:enqueue next-n primes-so-far)
                       (rsm.delayed:cons next-n (rec next-n (not even))))
                   (rec next-n (not even))))))
      (rsm.delayed:cons 2 (rsm.delayed:cons 3 
                                            (rsm.delayed:cons 5 (rec 5 t)))))))


(defun random-stream (n)
  (rsm.delayed:cons (random n) (random-stream n)))


(defparameter *tmp* nil)



;;;; RUN THE TESTS.

(defun run-delayed-tests ()

  (with-tests (:name "DELAYED LIST TESTS")

    (setf *tmp* (primes))
    
    (test '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 
            89 97)
          (rsm.delayed:take 25 *tmp*)
          :test #'equal
          :fail-info "Test 1")

    (test 101
          (rsm.delayed:car (rsm.delayed:drop 25 *tmp*))
          :test #'equal
          :fail-info "Test 2")

    (test '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
          (rsm.delayed:take 20 (integers 1))
          :test #'equal
          :fail-info "Test 3")

    (test 6
          (rsm.delayed:nth 5 (integers 1))
          :fail-info "Test 4")

    (test 8
          (rsm.delayed:nth 5 (fib2))
          :fail-info "Test 5")
    
    (test 8
          (rsm.delayed:nth 5 (fib))
          :fail-info "Test 6")

    (test 6
          (rsm.delayed:nth 5 '(1 2 3 4 5 6 7))
          :fail-info "Test 7")

    (test '(1 3 5 7 9)
          (rsm.delayed:take 5 (rsm.delayed:filter (integers 1) #'evenp))
          :test #'equal
          :fail-info "Test 8")

    (test '(1 3 5 7 9)
          (rsm.delayed:take 5 (rsm.delayed:filter '(1 2 3 4 5 6 7 8 9 10) 
                                                  #'evenp))
          :test #'equal
          :fail-info "Test 9")

    (test 11
          (rsm.delayed:car (rsm.delayed:drop 5 (rsm.delayed:filter 
                                                '(1 2 3 4 5 6 7 8 9 10 11) 
                                                #'evenp)))
          :test #'equal
          :fail-info "Test 10")

    
    (test '(2 3 5 7 10)
          (rsm.delayed:take 5 (rsm.delayed:mapcar #'+ (integers 1) (fib2)))
          :test #'equal
          :fail-info "Test 11")

    
    (test '(3 5 7 9 11)
          (rsm.delayed:take 5 (rsm.delayed:mapcar #'+ '(1 2 3 4 5) 
                                                  '(2 3 4 5 6)))
          :test #'equal
          :fail-info "Test 12")

    (test '(2 4 6 8 10)
          (rsm.delayed:mapcar #'+ '(1 2 3 4 5) (integers 1))
          :test #'equal
          :fail-info "Test 13")
    
    
    (test '(1 1 1 1 1)
          (rsm.delayed:take 5 (rsm.delayed:repeat 1))
          :test #'equal
          :fail-info "Test 14")
    
    (test '(2 3 5 7 10)
          (rsm.delayed:take 5 (rsm.delayed:zip-with (integers 1) (fib2) #'+))
          :test #'equal
          :fail-info "Test 15")

    
    (test '(2 3 4 5 6)
          (rsm.delayed:take 5 (rsm.delayed:zip-with '(1 2 3 4 5 6) 
                                                    '(1 1 1 1 1 1) #'+))
          :test #'equal
          :fail-info "Test 16")
    
    
    (test '(2 3 4 5 6 7)
          (rsm.delayed:zip-with '(1 2 3 4 5 6) '(1 1 1 1 1 1) #'+)
          :test #'equal
          :fail-info "Test 17")
    
    
    (test '(2 4 6 8 10)
          (rsm.delayed:zip-with '(1 2 3 4 5) (integers 1) #'+)
          :test #'equal
          :fail-info "Test 18")
    
    (test '(2 4 6 8 10)
          (rsm.delayed:take 5 (rsm.delayed:zip-with 
                               (rsm.delayed:list 1 2 3 4 5) '(1 2 3 4 5) #'+))
          :test #'equal
          :fail-info "Test 19")

    (test '(2 4 6 8 10)
          (rsm.delayed:zip-with (rsm.delayed:list 1 2 3 4 5) '(1 2 3 4 5) #'+)
          :test #'equal
          :fail-info "Test 20")

    (test '(2 4 6 8 10)
          (rsm.delayed:zip-with '(1 2 3 4 5) (rsm.delayed:list 1 2 3 4 5)  #'+)
          :test #'equal
          :fail-info "Test 21")
    
    (test nil
          (rsm.delayed:drop 5 '(1 2 3 4 5))
          :test #'equal
          :fail-info "Test 22")

    (test '(6)
          (rsm.delayed:drop 5 '(1 2 3 4 5 6))
          :test #'equal
          :fail-info "Test 23")
    
    (test nil
          (rsm.delayed:drop 5 '(1 2 3 4))
          :test #'equal
          :fail-info "Test 24")

    
    (test nil
          (rsm.delayed:drop 5 (rsm.delayed:list 1 2 3 4 5))
          :test #'equal
          :fail-info "Test 25")

    (test 6
          (rsm.delayed:car (rsm.delayed:drop 5 (rsm.delayed:list 1 2 3 4 5 6)))
          :test #'equal
          :fail-info "Test 26")
    
    (test nil
          (rsm.delayed:drop 5 (rsm.delayed:list 1 2 3 4))
          :test #'equal
          :fail-info "Test 27")

    (test nil
          (rsm.delayed:car nil)
          :fail-info "Test 28")
    
    (test nil
          (rsm.delayed:cdr nil)
          :fail-info "Test 29")
    
    (test '(1 2 3 4 5)
          (rsm.delayed:take 5 (rsm.delayed:list 1 2 3 4 5))
          :test #'equal
          :fail-info "Test 30")
    
    (test '(1 2 3 4 5)
          (rsm.delayed:take 7 (rsm.delayed:list 1 2 3 4 5))
          :test #'equal
          :fail-info "Test 31")

    (test nil
          (rsm.delayed:drop 5 (rsm.delayed:list 1 2 3 4 5))
          :test #'equal
          :fail-info "Test 32")

    (test nil
          (rsm.delayed:drop 10 (rsm.delayed:list 1 2 3 4 5))
          :test #'equal
          :fail-info "Test 33")

    
    (test nil
          (rsm.delayed:nth 10 (rsm.delayed:list 1 2 3 4 5))
          :test #'equal
          :fail-info "Test 34")

    (test '(2 4 6 8 10)
          (rsm.delayed:take 5 (rsm.delayed:zip-with 
                               (rsm.delayed:list 1 2 3 4 5 6) '(1 2 3 4 5) #'+))
          :test #'equal
          :fail-info "Test 35")
    
    (test '(2 4 6 8 10)
          (rsm.delayed:take 5 
                    (rsm.delayed:zip-with (rsm.delayed:list 1 2 3 4 5) 
                                  (rsm.delayed:list 1 2 3 4 5 6) #'+))
          :test #'equal
          :fail-info "Test 36")

    (test '(2 4 6 8 10)
          (rsm.delayed:take 6
                    (rsm.delayed:zip-with (rsm.delayed:list 1 2 3 4 5) 
                                  (rsm.delayed:list 1 2 3 4 5 6) #'+))
          :test #'equal
          :fail-info "Test 37")

    (test '(2 4 6 8 10)
          (rsm.delayed:zip-with (rsm.delayed:list 1 2 3 4 5) '(1 2 3 4 5 6) #'+)
          :test #'equal
          :fail-info "Test 38")

    (test '(2 4 6 8 10)
          (rsm.delayed:zip-with '(1 2 3 4 5 6) 
                                (rsm.delayed:list 1 2 3 4 5)  #'+)
          :test #'equal
          :fail-info "Test 39")

    (test '(2 4 6 8 10)
          (rsm.delayed:zip-with (rsm.delayed:list 1 2 3 4 5) '(1 2 3 4 5 6) #'+)
          :test #'equal
          :fail-info "Test 40")

    (test '(2 4 6 8 10)
          (rsm.delayed:take 5 (rsm.delayed:mapcar #'+ 
                                                  (rsm.delayed:list 1 2 3 4 5) 
                                                  (rsm.delayed:list 1 2 3 4 5)))
          :test #'equal
          :fail-info "Test 41")

    (test '(2 4 6 8 10)
          (rsm.delayed:take 5 
                            (rsm.delayed:mapcar #'+ 
                                                (rsm.delayed:list 1 2 3 4 5) 
                                                (rsm.delayed:list 1 2 3 4 5 6)))
          :test #'equal
          :fail-info "Test 42")

    (test '(2 4 6 8 10)
          (rsm.delayed:take 5 (rsm.delayed:mapcar #'+ 
                                                  (rsm.delayed:list 1 2 3 4 5 6)
                                                  (rsm.delayed:list 1 2 3 4 5)))
          :test #'equal
          :fail-info "Test 43")

    (test '(3 6 9)
          (rsm.delayed:take 5 (rsm.delayed:mapcar #'+ 
                                                  (rsm.delayed:list 1 2 3 4 5 6)
                                                  (rsm.delayed:list 1 2 3 4 5)
                                                  '(1 2 3)))
          :test #'equal
          :fail-info "Test 44")

    (test '(3 6 9)
          (rsm.delayed:take 5 (rsm.delayed:mapcar #'+
                                  (rsm.delayed:list 1 2 3 4 5 6)
                                  '(1 2 3)
                                  (rsm.delayed:list 1 2 3 4 5)))
          :test #'equal
          :fail-info "Test 45")
    
    
    (setf *tmp* (integers 1))
    (setf *tmp* (rsm.delayed:drop 5 *tmp*))
    
    (test 6
          (rsm.delayed:car *tmp*)
          :fail-info "Test 46")
    
    (test '(2 2 2 2 2)
          (rsm.delayed:take 5 (rsm.delayed:zip-with 
                               (rsm.delayed:repeat 1) 
                               (rsm.delayed:repeat 1) #'+))
          :test #'equal
          :fail-info "Test 47")

    (test '(1 2 3 1 2)
          (rsm.delayed:take 5 (rsm.delayed:cycle '(1 2 3)))
          :test #'equal
          :fail-info "Test 48")
    
    (test 11
          (rsm.delayed:car 
           (rsm.delayed:drop 5 (rsm.delayed:filter 
                                (rsm.delayed:list 1 2 3 4 5 6 7 8 9 10 11) 
                                #'evenp)))
          :test #'equal
          :fail-info "Test 49")
    )
  t
  )
