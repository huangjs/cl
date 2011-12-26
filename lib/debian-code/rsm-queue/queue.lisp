;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          queue.lisp
;;;; Purpose:       Queuing functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: queue.lisp,v 1.4 2003/10/20 11:26:53 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.queue)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; A queue is represented as a cons cell as: 
;;;; queue-list . pointer-to-last-cons-cell-of queue-list.
;;;; The most recent addition is at the end of the queue-list; the oldest
;;;; at the beginning of queue-list.

(declaim (inline nget-list))
(defun nget-list (queue)
  "Get the internal list of queue, <queue>. The integrity of the queue cannot be
guaranteed if this list is destructively modified."
  (car queue))

(declaim (inline get-first))
(defun get-first (queue)
  "Get the next element the queue would dequeue. Does not affect the queue."
  (caar queue))

(declaim (inline get-last))
(defun get-last (queue)  
  "Get the last element the queue would dequeue. Does not affect the queue."
  (cadr queue))

(defun append-queue (&rest queues)
  "Create a new queue which appends the other queues. The original queues are
not changed."
  (let ((que (create)))
    (dolist (queue queues)
      (dolist (el (nget-list queue))
        (enqueue el que)))
    que))


(declaim (ftype (function () list) create))

(defun create (&optional obj)
  "Create a queue. If <obj> is non nil queue it up. In order to create a queue
with nil as the first element, call queue with no arguments and then call
enqueue with nil as the value to queue."
  (let ((queue (cons nil nil)))
    (if obj
        (enqueue obj queue)
      queue)))

(defun copy-queue (que)
  "Copy a queue."
  (let ((new-que (create))
        (new-list (copy-list (nget-list que))))
    (setf (car new-que) new-list)
    (setf (cdr new-que) (last new-list))
    new-que))


(defun non-empty-queues (queues)
  "Return a list of the non-empty queues."
  (let ((que-que (create)))
    (dolist (queue queues)
      (unless (empty-p queue)
        (enqueue queue que-que)))
    (nget-list que-que)))

(defun nappend-queue (&rest queues)
  "Append (destructively (like nconc) the queues <queues> essentially by
nconsing the internal list of the first nonempty queue with the lists from the
rest of the non empty queues.  
Note: After this operation, do not use the other queues."
  (let ((non-empty-queues (non-empty-queues queues))
        base-que)
    (if (null non-empty-queues)
        (create)
      (let ((rest-ques (cdr non-empty-queues)))
        (setf base-que (car non-empty-queues))
        (dolist (queue rest-ques)
          (setf (cdr (cdr base-que)) (nget-list queue))
          (setf (cdr base-que) (cdr queue)))))
    base-que))


(declaim (ftype (function (t list) list) enqueue))

(defun enqueue (obj queue)
  "Enqueue an object. Return the queue."
  (if (cdr queue)
      (setf (cdr (cdr queue)) (list obj)
            (cdr queue) (cdr (cdr queue)))
    (setf (cdr queue) (list obj)
          (car queue) (cdr queue)))
  queue)

(declaim (ftype (function (list) t) dequeue))

(defun dequeue (queue)
  "Dequeue an object. Return the object queued."
  (when (cdr queue)
    (prog1
        (caar queue)
      (if (eq (cdr queue)
              (car queue))
          (setf (car queue) nil
                (cdr queue) nil))
      (setf (car queue) (cdr (car queue))))))


(declaim (ftype (function (list) t) empty-p))

(defun empty-p (queue)
  "Is the queue empty?"
  (and (not (cdr queue)) t))


(declaim (ftype (function (list) list) list->queue))

(defun list->queue (list)
  "Return a copy of the list as a queue."
  (let ((queue (create)))
    (loop 
      :for elem :in list :do
      (enqueue elem queue))
    queue))


(defun queue->list (que)
  "Return a copy of the queue as a list, from 'first in' to 'last in'."
  (copy-list (car que)))


(defun queue-p (que)
  "Returns true if <que> is a queue."
  (and (consp que)
       (listp (car que))
       (null (cdr (cdr que)))))

(deftype queue ()
  "Type for a queue."
  '(satisfies queue-p))


(defmacro do-queue ((var que &optional result) &body body)
  "Loop construct for queues that sets <var> to the successive values of a copy
of the queue, <que>, (by dequeuing) and then evaluates <body>. If the symbol 
<result> is supplied, its value is returned when the iteration is finished.
Example: (rsm.queue:do-queue (item que) (format t \"queue item = ~s~%\" item)) 
This drains a copy of the queue, <que>, printing each of the elements 
of the queue."
  (let ((copied-queue (gensym))
        (is-empty? (gensym)))
    `(do* ((,copied-queue (rsm.queue:copy-queue ,que))
           (,is-empty? (rsm.queue:empty-p ,copied-queue)
                       (rsm.queue:empty-p ,copied-queue))
           (,var (rsm.queue:dequeue ,copied-queue)
                 (rsm.queue:dequeue ,copied-queue)))
         ()
       (when ,is-empty?
         (return ,result))
       ,@body
       )))


(defmacro do-nqueue ((var que &optional result) &body body)
  "Loop construct for queues that sets <var> to the successive values of the
queue, <que>, (by dequeuing) and then evaluates <body>. If the symbol <result> 
is supplied, its value is returned when the iteration is finished.
Example: (rsm.queue:do-nqueue (item que) (format t \"queue item = ~s~%\" item)) 
This drains the queue, <que>, printing each of the elements of the queue.  
Note: This is a destructive function. If <body> mutates <que>, this 
construct could go into an infinite loop."
  (let ((is-empty? (gensym)))
    `(do ((,is-empty? (rsm.queue:empty-p ,que)
                      (rsm.queue:empty-p ,que))
          (,var (rsm.queue:dequeue ,que) 
                (rsm.queue:dequeue ,que)))
         ()
       (when ,is-empty?
         (return ,result))
       ,@body
       )))


(defun sort-queue (queue sort-func)
  "Sort a copy of queue, <queue>, using sort function <sort-func>."
  (let ((list (sort (copy-list (nget-list queue)) sort-func))
        (new-que (create)))
    (dolist (elem list)
      (enqueue elem new-que))
    new-que))

(defun nsort-queue (queue sort-func)
  "Sort a queue, <queue>, in place using sort function <sort-func>."
  (let ((list (sort (nget-list queue) sort-func)))
    (setf (car queue) list)
    (setf (cdr queue) (last list)))
  queue)

