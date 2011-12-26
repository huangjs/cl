;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.queue.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          queue-test.lisp
;;;; Purpose:       Regression Testing For Queuing Package.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: queue-test.lisp,v 1.4 2003/10/20 11:26:53 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.queue.test
  (:use #:cl rsm.queue #:ptester)
  (:documentation
   "Provides a test harness for queuing functions.")
  )


(in-package rsm.queue.test)


;;;; SET UP A QUEUE TO TEST.

(defparameter *queue* (create (list 'a 'b)))
(defparameter *queue2* (create))



;;;; RUN THE TESTS.

(defun run-queue-tests ()

  (with-tests (:name "QUEUE TESTS")
    
    (enqueue '(b c) *queue*)
    
    (test (list 'a 'b)
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 1")
    
    (enqueue 'a *queue*)
    (enqueue 'b *queue*)
    (enqueue 'c *queue*)
    
    (test nil
          (empty-p *queue*)
          :fail-info "Test 2")
    
    (test (list 'b 'c)
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 3")
    (test 'a
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 4")
    (test 'b
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 5")
    (test 'c
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 6")
    
    (test t
          (empty-p *queue*)
          :fail-info "Test 7")
    
    (enqueue 'a *queue*)
    (enqueue 'b *queue*)
    (enqueue 'c *queue*)

    (test nil
          (empty-p *queue*)
          :fail-info "Test 8")

    (test 'a
          (dequeue *queue*)
          :fail-info "Test 9")
    
    (test nil
          (empty-p *queue*)
          :fail-info "Test 10")

    (test 'b
          (dequeue *queue*)
          :fail-info "Test 11")
    
    (test nil
          (empty-p *queue*)
          :fail-info "Test 12")

    (test 'c
          (dequeue *queue*)
          :fail-info "Test 13")
    
    (test t
          (empty-p *queue*)
          :fail-info "Test 14")

    (enqueue 'a *queue*)
    
    (test nil
          (empty-p *queue*)
          :fail-info "Test 15")

    (test 'a
          (dequeue *queue*)
          :fail-info "Test 16")

    (test t
          (empty-p *queue*)
          :fail-info "Test 17")

    (test nil
          (dequeue *queue*)
          :fail-info "Test 18")

    (test nil
          (dequeue *queue*)
          :fail-info "Test 19")
    
    (enqueue 'a *queue*)
    (enqueue 'b *queue*)
    

    (test nil
          (empty-p *queue*)
          :fail-info "Test 20")

    (test 'a
          (dequeue *queue*)
          :fail-info "Test 21")

    
    (test nil
          (empty-p *queue*)
          :fail-info "Test 22")

    (test 'b
          (dequeue *queue*)
          :fail-info "Test 23")

    (test nil
          (dequeue *queue*)
          :fail-info "Test 24")
    
    (test t
          (empty-p *queue*)
          :fail-info "Test 25")
    
    
    (enqueue (list 'a 'b) *queue*)
    
    (enqueue (list 'c 'd) *queue*)
    
    (enqueue (list 'e 'f) *queue*)


    
    (test (list 'a 'b)
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 26")

    (test (list 'c 'd)
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 27")

    (test (list 'e 'f)
          (dequeue *queue*)
          :test #'equal
          :fail-info "Test 28")

    (test t
          (queue-p *queue*)
          :fail-info "Test 29")
    
    (enqueue (list 'a 'b) *queue*)

    (test t
          (queue-p *queue*)
          :fail-info "Test 30")

    (enqueue (list 'c 'd) *queue*)
    
    (enqueue (list 'e 'f) *queue*)


    (do-nqueue (item *queue*)
      (enqueue item *queue2*))


    (test t
          (queue-p *queue*)
          :fail-info "Test 31")

    (test t
          (empty-p *queue*)
          :fail-info "Test 32")

    (test (list 'a 'b)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 33")
    
    (test (list 'c 'd)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 34")

    (test (list 'e 'f)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 35")

    
    (enqueue (list 'a 'b) *queue*)

    (test t
          (queue-p *queue*)
          :fail-info "Test 36")

    (enqueue (list 'c 'd) *queue*)
    
    (enqueue (list 'e 'f) *queue*)

    (test (list (list 'a 'b) (list 'c 'd) (list 'e 'f))
          (queue->list *queue*)
          :test #'equal
          :fail-info "Test 37")

    (test nil
          (empty-p *queue*)
          :fail-info "Test 38")

    ;; Test the "do" control macro.
    (do-queue (item *queue*)
      (enqueue item *queue2*))

    
    (test t
          (queue-p *queue*)
          :fail-info "Test 39")

    (test nil
          (empty-p *queue*)
          :fail-info "Test 40")

    (test (list 'a 'b)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 41")
    
    (test (list 'c 'd)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 42")

    (test (list 'e 'f)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 43")

    (setf *queue2* 
      (list->queue (list (list 'a 'b) (list 'c 'd) (list 'e 'f))))
    
    (test (list 'a 'b)
          (get-first *queue2*)
          :test #'equal
          :fail-info "Test 44")

    (test (list 'e 'f)
          (get-last *queue2*)
          :test #'equal
          :fail-info "Test 45")

    (test t
          (queue-p *queue2*)
          :fail-info "Test 46")
    
    (test nil
          (empty-p *queue2*)
          :fail-info "Test 47")

    (test (list 'a 'b)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 48")
    
    (test (list 'c 'd)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 49")

    (test (list 'e 'f)
          (dequeue *queue2*)
          :test #'equal
          :fail-info "Test 50")

  ;;; Get the internal list of *queue*.
    (test (list (list 'a 'b) (list 'c 'd) (list 'e 'f))
          (nget-list *queue*)
          :test #'equal
          :fail-info "Test 51")

  ;;; This appears to be creating a queue with an element nil, but
  ;;; it creates an empty queue.
    (setf *queue* (create nil))
    
  ;;; The queue should be empty.
    (test t
          (empty-p *queue*)
          :fail-info "Test 52")
    
  ;;; Now queue up a nil.
    (enqueue nil *queue*)
    
  ;;; The length of the queue should be 1.
    (test 1
          (length (queue->list *queue*))
          :fail-info "Test 54")

  ;;; Queue up another nil.
    (enqueue nil *queue*)

  ;;; The queue should have two nils which we drain.
    (do-nqueue (item *queue*)
      (let ((message (format nil "Test 55 item = ~s" item)))
        (test nil
              item
              :fail-info message)))
    
  ;;; The queue should now be empty.
    (test t
          (empty-p *queue*)
          :fail-info "Test 56")
    
    ;; Should get nil when dequeuing an empty queue.
    (test nil
          (dequeue *queue*)
          :fail-info "Test 57")
    
    (setf *queue* (create))
    (enqueue "c" *queue*)
    (enqueue "b" *queue*)
    (enqueue "d" *queue*)
    (enqueue "a" *queue*)

    (setf *queue2* (sort-queue *queue* #'string<))
    
    (test (list "a" "b" "c" "d")
          (nget-list *queue2*)
          :test #'equal
          :fail-info "Test 58")

    
    (setf *queue* (nsort-queue *queue* #'string<))
    
    (test (list "a" "b" "c" "d")
          (nget-list *queue*)
          :test #'equal
          :fail-info "Test 59")

    (setf *queue* (create))
    (enqueue "c" *queue*)
    (enqueue "b" *queue*)
    (enqueue "d" *queue*)
    (enqueue "a" *queue*)
    
    (nsort-queue *queue* #'string<)
    
    (test (list "a" "b" "c" "d")
          (nget-list *queue*)
          :test #'equal
          :fail-info "Test 60")

    )
  t
  )

