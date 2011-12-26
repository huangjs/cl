;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.9 $
;;; $Date: 2006/06/01 03:43:43 $

(in-package :cm)

;;; enqueuing a Lisp object into the C scheduler involves adding the
;;; object to a hash table and then passing its integer hash key
;;; (handle) to the C scheduler, which then passes the handle back to
;;; lisp at the approprite time for Lisp-side processing. The type of
;;; processing that is applied to the object depends on a "type" value
;;; encoded in the lowest nibble of the handle. (see top of
;;; cm/src/scheduler.scm for the list of types).  when an object is
;;; initially sprouted (either by a user or by a musical process) its
;;; handle contains ONLY type information. this allows the enqueuing
;;; function to determine if an object has to be added to the hash
;;; table or not by testing the bits above the lower nibble: if the
;;; bits are zero then the object is added as a new entry in the hash
;;; tabl, otherwise the object is already in the table and the
;;; function simply requeues the handle back into the scheduler.
;;; enqueuing of any sort has to deterimine if the object has come
;;; from the lisp thread (repl) or from a musical process. if it came
;;; from the repl then we have to protect the hash table from a
;;; callback accessing the table while the new entry is being added

(defparameter *qentries*
  (make-hash-table :size 256 :test #'eq))

(defvar *qcount* 0) ; for generating unique handles (hash keys)

(defvar *rtsdebug* nil)

(defmacro rtsdebug (&rest args)
  `(if *rtsdebug* (format ,@args)))

(defmacro %newhandle (type)
  `(logior (ash (incf *qcount*) 4) ,type))

(defmacro %handle-index (handle)
  `(ash (logand ,handle #xFFFFFFF0) -4))

(defmacro %handle-type (handle)
  `(logand ,handle #xF))

(defmacro %handle-typename (handle)
  `(elt '("unknown" "process" "seq" "object" "message" "pointer"
	  "err" "err" "err" "err" "err" "err" "err" "err" "err" "err")
	(%handle-type ,handle)))

;;;
;;;
;;;

(defun rts-enqueue (handle object time start sched)
  ;; time is either in sec msec or usec
  ;; sched is either :rts or nil, nil means from repl
  ;; add check for sprout without rts running.
  (let ((repl? (not (eql sched ':rts)))
	(data 0)
	(flag 0))
    (cond ((= (logand handle #xF) *qentry-message*)
	   ;; integer midi messages can be inserted directly into the
	   ;; scheduler as data. could do C pointers this way too.
	   (setq data object))
	  ((= 0 (logandc2 handle #xF))  ; new entry, add to table
	   ;; if its a seq or a process we also have to cache the
	   ;; start time of the object: (<object> . start)
	   ;; start time is in *time-format* units 
	   (when (or (= handle *qentry-seq*)
		     (= handle *qentry-process*))
	     (setq object (cons object start)))
	   ;; handle only has type information, make full handle
	   (setq handle (%newhandle handle))
	   ;; lock scheduler out during table set if in repl
	   (if repl? (rts:scheduler-lock-lisp))
	   ;; add new entry to table
	   (setf (gethash handle *qentries*) object)
	   ;; unlock table
	   (if repl? (rts:scheduler-unlock-lisp))
	   )
	  )
    (rtsdebug t "~%enqueing: data=~d type=~d time=~d repl=~d"
	      data handle time (if repl? 1 0))
    ;; convert to msec
    (setq flag (rts:scheduler-enqueue data
				      handle
				      ;; convert seconds to usec for C
				      (if (eq rts:*time-format* ':sec)
					  (floor (* time 1000000))
					  time)
				      (if repl? 1 0)))
    (unless (eql flag 0)
      (case flag
        ((1) (error "enqueue: no room in scheduler for ~S." object))
        ((2) (error "enqueue: RTS not running."))))
    (values)))

(defun cm-hook (data handle time)
  ;; C SIDE HAS LOCKED LISP DURING EXTENT OF CALLBACK
  (let ((entry nil)
        (etype (%handle-type handle))) ; get entry type from low nibble
    ;; C time is usec or msec, convert to SEC if necessary
    (when (eq? rts:*time-format* ':sec)
      (setq time (/ time 1000000.0)))
    (setq entry (if (= etype *qentry-message*)
		    data
		    (or (gethash handle *qentries*)
			(error "No RTS entry for handle ~D."
			       handle))))   
    (cond ((= etype *qentry-process*)
           ;; entry is (<process> . <start>)
	   (rtsdebug t "~&process=~s, start=~s time=~s~%"
		     (car entry) (cdr entry) time)
           (scheduler-do-process (car entry)
                                 time
                                 (cdr entry)
                                 *rts-out*
                                 handle
				 ':rts))
          ((= etype *qentry-seq*)
	   ;; entry is ( (<seq> . <subobjects>) . <start>)
	   (rtsdebug t "~&seq=~s, start=~d time=~d~%" 
		     (caar entry) (cdr entry) time)
           (scheduler-do-seq (car entry)
			     time
			     (cdr entry)
			     *rts-out*
			     handle
			     ':rts))
          (t
	   (rtsdebug t "~&object=~s time=~s~%" entry time)
           (write-event entry *rts-out* time)))
    (values)))

;;;
;;; user level functions
;;; 

(defun rts-now () (rts:scheduler-time))

(defun rts (&rest args)
  (unless (rts:scheduler-state? ':stopped)
    (error "rts: scheduler is already running or paused."))
  (unless (or (null? args) (keywordp (car args)))
    (setq *rts-out* (pop args)))
  (unless (null? *rts-out*)  ;; initalize must be called for microtuning
    (initialize-io *rts-out*))
  (apply #'rts:scheduler-start args)
  (format t "~&; RTS running~%")
  (values))

(defun rts? (&optional arg)
  (apply #'rts:scheduler-state? arg))

(defun rts-pause () 
  (rts:scheduler-pause)
  (values))

(defun rts-continue ()
  (rts:scheduler-continue)
  (values))
(defun rts-flush ()
  (rts:scheduler-flush)
  (values))

(defun rts-hush ()
  (rts-flush)
  (when *rts-out*
    (do ((i 0 (+ i 1)))
	((>= i 16))
      (write-event (make <midi-control-change> :time 0
			 :controller 64 :value 0 :channel i)
		   *rts-out* 0)
      (write-event (make <midi-control-change> :time 0
			 :controller 123 :value 0)
		   *rts-out* 0)))
  (values))

(defun rts-reset () 
  ;;(print :rts-reset)
  #+openmcl (ccl:gc)
  #+sbcl (sb-ext:gc)
  (setq *qcount* 0)
  (clrhash *qentries*)
  (rts-reset-globals)
  (values))

(defun rts-reset-globals ()
  ;;(print :rts-reset-globals)
  (setq *rts-pstart* nil)
  (setq *rts-qtime* nil)
  (values))

(defun rts-stop ()
  (rts:scheduler-stop)
  (format t "~&; RTS stopped.~%")
  (values))

(defun rts-thread? () (rts:rts-thread? ))

(eval-when (:load-toplevel :execute)
  (rts:scheduler-hook-set! #'cm-hook)
  (rts:scheduler-add-hook! ':before-start #'rts-reset)
  (rts:scheduler-add-hook! ':after-stop #'rts-reset)
  (rts:scheduler-add-hook! ':error-continue #'rts-reset-globals)
  )




