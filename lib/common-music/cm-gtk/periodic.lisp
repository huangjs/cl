;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; *********************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2006/03/29 14:44:27 $

(in-package :cm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (import (list (find-symbol "*MAX-EVENT-TO-SEC*" :sb-impl)
                (find-symbol "*MAX-EVENT-TO-USEC*" :sb-impl)
                (find-symbol "*PERIODIC-POLLING-FUNCTION*" :sb-impl)
                ))
  #+cmu
  (import (list (find-symbol "*MAX-EVENT-TO-SEC*" :lisp)
                (find-symbol "*MAX-EVENT-TO-USEC*" :lisp)
                (find-symbol "*PERIODIC-POLLING-FUNCTION*" :lisp)
                ))
  #+openmcl
  (progn (ccl::open-shared-library "Carbon.framework/Carbon")
         (ccl::use-interface-dir :carbon)
         (require "pascal-strings")
         (defvar *max-event-to-sec*)
         (defvar *max-event-to-usec*)
         (defvar *periodic-polling-function*)
	 (defparameter *periodic-polling-timer* nil)
	 (defparameter *periodic-polling-thread* nil)
	 (defparameter *periodic-polling-callback* nil)
	 (defun %string->cfstringref (string)
	   (ccl::with-cstrs ((str string))
	     (#_CFStringCreateWithCString (ccl::%null-ptr) str
					  #$kCFStringEncodingMacRoman)))
	 
	 (defun %release-cfstring (string)
	   (#_CFRelease string))
	 )
  )
                        
(defvar *periodic-tasks* (list ))

(defun set-periodic-task-rate! (rate meas)
  (if *periodic-tasks*
      (error "set-periodic-task-rate!: Periodic tasks currently running.")
    (let (divs)
      (ecase meas
	((:second :seconds :sec :s)
	 (setf divs 1))
	((:millisecond :milliseconds :ms :m)
	 (setf divs 1000))
	((:nanosecond :nanoseconds :usec :usecs :u)
	 (setf divs 1000000)))
      (multiple-value-bind (sec rem)
	  (floor rate divs)
	(setf *max-event-to-sec* sec)
        (setf *max-event-to-usec* (floor (* rem (/ 1000000 divs)))))
      (values))))

(defun periodic-task-rate ()
  (+ (* *max-event-to-sec* 1000000)
     *max-event-to-usec*))

(defun periodic-task-running? (&optional owner)
  (if *periodic-tasks*
      (if owner 
          (and (assoc owner *periodic-tasks* :test #'eq) t)
          t)
      nil))

(defun add-periodic-task! (owner task)
  (cond ((null *periodic-tasks*)
         (push (cons owner task) *periodic-tasks*)
         (setf *periodic-polling-function* #'run-periodic-tasks)
	 (funcall *periodic-polling-function* ))
        ((assoc owner *periodic-tasks* :test #'eq)
         (error "add-periodic-task!: task already running for ~s."
                owner))
        (t
         (push (cons owner task) *periodic-tasks*)))
  (values))

(defun remove-periodic-task! (owner)
  (if (eq owner t)
      (setf *periodic-polling-function* nil
            *periodic-tasks* (list))
      (let ((e (assoc owner *periodic-tasks* :test #'eq)))
        (cond ((null e)
               (error "remove-periodic-task!: No task for owner ~s."
                      owner))
              (t
               (setf *periodic-tasks* (delete e *periodic-tasks*))
               (if (null *periodic-tasks*)
                   (setf *periodic-polling-function* nil))))))
  (values))

(defun map-periodic-tasks (fn)
  (do ((tail *periodic-tasks* (cdr tail)))
      ((null? tail)
       (values))
    (funcall fn (car tail))))

#-openmcl
(defun run-periodic-tasks ()
  (dolist (e *periodic-tasks*) (funcall (cdr e)))
  (values))

#+openmcl
(defun run-periodic-tasks ()
  (let ((timer-context (%string->cfstringref "cm-periodic")))
    (ccl::defcallback *periodic-polling-callback*
      (:<CFR>un<L>oop<T>imer<R>ef timer (:* t) info)
      (declare (ignore timer info))
      (if *periodic-tasks*
          (dolist (e *periodic-tasks*) (funcall (cdr e)))
	(progn
	  (#_CFRunLoopStop (#_CFRunLoopGetCurrent))
	  (ccl::process-kill *periodic-polling-thread*))))
    (setf *periodic-polling-thread*
	  (ccl::process-run-function "periodic-polling-thread"
	    (lambda ()
	      (ccl::external-call "_CFRunLoopGetCurrent" :address)
	      (ccl::external-call "__CFRunLoopSetCurrent"
                                  :address
                                  (ccl::external-call
                                   "_CFRunLoopGetMain" :Address)) 
	      (ccl::%stack-block ((psn 8))
				 (ccl::external-call "_GetCurrentProcess"
                                                     :address psn)
				 (ccl::with-cstrs ((name "cm rt"))
				   (ccl::external-call
                                    "_CPSSetProcessName"
                                    :address psn :address name)))
	      
	      (setf *periodic-polling-timer*
		    (#_CFRunLoopTimerCreate
                     (ccl::%null-ptr)
                     (#_CFAbsoluteTimeGetCurrent) 
                     (coerce (+ *max-event-to-sec*
                                (/ *max-event-to-usec* 1000000))
                             'double-float)
                     0 0 *periodic-polling-callback* (ccl::%null-ptr)))
	      (#_CFRunLoopAddTimer (#_CFRunLoopGetCurrent)
                                   *periodic-polling-timer* timer-context)
	      (#_CFRunLoopRunInMode timer-context 10000000.D0 #$false)
	      (#_CFRunLoopTimerInvalidate *periodic-polling-timer*)
	      (#_CFRelease *periodic-polling-timer*)
	      (%release-cfstring timer-context))))))