;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package Definition For Queuing Functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.4 2003/10/20 11:26:53 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.queue
  (:use #:cl)
  (:documentation
   "This package provides the usual queuing functions.

Export Summary:

append-queue : Create a new queue which concatenates queues.
nappend-queue: Similar to nconc, but for queues.
create       : Create a new queue.
do-queue     : Control structure for queue: Like dolist but for queues. 
               Each element is formed by dequeuing a copy of the original queue.
do-nqueue    : Control structure for queue: Like dolist but for queues. 
               Each element is formed by dequeuing the original queue. 
               This function is destructive.
empty-p      : Is this queue empty?
enqueue      : Enqueue an element onto a queue.
dequeue      : Dequeue a queue (returning the dequeued element).
get-first    : Get the first element of the queue. The next element to be
               dequeued.
get-last     : Get the last element of the queue. The last element to be 
               dequeued.
nget-list    : Get the list of the queue. Does NOT make a copy.
list->queue  : Returns a copy of a list as a queue.
queue        : A type for queues.
queue-p      : Returns true if argument is a queue.
queue->list  : Returns a copy of the list of the queue.
nsort-queue  : Sort a queue in place.
sort-queue   : Sort a copy of a queue.")
  (:export 
   #:nappend-queue
   #:append-queue
   #:copy-queue
   #:create
   #:do-nqueue
   #:do-queue
   #:empty-p
   #:enqueue
   #:dequeue
   #:get-first
   #:get-last
   #:nget-list
   #:list->queue
   #:queue
   #:queue-p
   #:queue->list
   #:nsort-queue
   #:sort-queue
   ))
