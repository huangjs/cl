(in-package :cl-user)

(defpackage :hjs.control.thread-pool
  (:use #:common-lisp #:iterate
	#:hjs.meta.lisp #:hjs.meta.functional #:hjs.meta.type
	#:hjs.meta.macro #:hjs.meta.essential)
  (:export #:make-thread-pool
	   #:stop-pool
	   #:start-pool
	   #:new-task))

(in-package :hjs.control.thread-pool)

(defun nop ())

;;; task
(define-class task ()
    ((thunk :initform #'nop :type function)))

;;; task-queue
(define-class task-queue ()
    ((tasks :initform (make-instance 'arnesi:queue :element-type 'task) :type arnesi:queue)
     (mutex :initform (sb-thread:make-mutex) :type sb-thread:mutex))
  (:documentation
   "A simple task queue contains all the remaining tasks to be done"))

(defmethod enqueue ((thing task) (queue task-queue))
  (sb-thread:with-mutex ((mutex queue))
    (arnesi:enqueue (tasks queue) thing)))

(defmethod dequeue ((queue task-queue))
  (sb-thread:with-mutex ((mutex queue))
    (arnesi:dequeue (tasks queue))))


;;; thread-pool
(define-class thread-pool ()
    ((workers :initform '() :type list)
     (mutex :initform (sb-thread:make-mutex) :type sb-thread:mutex)
     (task-queue :initform (make-instance 'task-queue) :type task-queue)
     (running-p :initform t :type boolean))
  (:documentation
   "A simple thread pool that contains a group of workers and a task
  queue."))

;;; workers
(defmethod get-task ((pool thread-pool))
  (with-accessors ((m mutex) (q task-queue) (r running-p)) pool
    (loop
       (sb-thread:with-mutex (m)
	 (if (not r)
	     (sleep 0.05)
	     (let ((task (dequeue q)))
	       (if task
		   (return-from get-task task)
		   (sleep 0.05))))))))

(define-class worker ()
    ((idle-p :initform t :type boolean)
     thread
     (mutex :initform (sb-thread:make-mutex))
     (thread-pool :initform (error "thread-pool must be provided.")))
  (:documentation
   "An active worker that is able to fetch and run delivered tasks."))

(defmethod fetch-and-do-task-loop ((w worker))
  (with-accessors ((p thread-pool) (i idle-p) (m mutex)) w
    (loop
       (let ((task (get-task p)))
	 (unwind-protect
	      (progn
		(sb-thread:with-mutex (m)
		  (setf i nil))
		(funcall (thunk task)))
	   (sb-thread:with-mutex (m)
	     (setf i t)))))))

(defmethod initialize-instance :after ((w worker) &key &allow-other-keys)
  (sb-thread:with-mutex ((mutex w))
    (setf (thread w)
	  (sb-thread:make-thread (lambda () (fetch-and-do-task-loop w))))))


;;; api
(defun make-thread-pool (&key (num-of-workers 4))
  (let ((pool (make-instance 'thread-pool)))
    (sb-thread:with-mutex ((mutex pool))
      (dotimes (i num-of-workers)
	(push (make-instance 'worker :thread-pool pool)
	      (workers pool))))
    pool))

(defmethod stop-pool ((pool thread-pool))
  (with-accessors ((m mutex) (r running-p)) pool
    (sb-thread:with-mutex (m)
      (setf r nil))))

(defmethod start-pool ((pool thread-pool))
  (with-accessors ((m mutex) (r running-p)) pool
    (sb-thread:with-mutex (m)
      (setf r t))))

(defmethod new-task ((p thread-pool) (task task))
  (with-accessors ((q task-queue) (m mutex)) p
    (sb-thread:with-mutex (m)
      (enqueue task q))))

