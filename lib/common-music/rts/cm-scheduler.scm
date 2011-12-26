;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.6 $
;;; $Date: 2006/06/01 03:43:43 $

;(in-package :cm)

(import rts)

(define *qentries*
  (make-hash-table 'eqv?))

(define *qcount* 0) ; for generating unique handles (hash keys)

(define *rtsdebug* #f)

(define-macro (rtsdebug . args)
  `(if *rtsdebug* (format ,@args)))

(define-macro (%newhandle type)
  `(logior (ash (incf *qcount*) 4) ,type))

(define-macro (%handle-index handle)
  `(ash (logand ,handle #xFFFFFFF0) -4))

(define-macro (%handle-type handle)
  `(logand ,handle #xF))

(define-macro (%handle-typename handle)
  `(vector-ref #("unknown" "process" "seq" "object" "message" "pointer"
		 "err" "err" "err" "err" "err" "err" "err" "err" "err" "err")
	       (%handle-type ,handle)))

;;;
;;; enqueue lisp object in c scheduler
;;;

(define (rts-enqueue handle object time start sched)
  ;; time is either in sec msec or usec
  ;; add check for sprout without rts running.
  ;; sched is :rts or #f
  (let ((repl? (not (eq? sched :rts)))
	(data 0)
	(flag 0))
    (cond ((= (logand handle #xF) *qentry-message*)
	   ;; integer midi messages can be inserted directly into the
	   ;; scheduler as data. could do C pointers this way too.
	   (set! data object))
	  ((= 0 (logand handle #xFFFFFFF0))  ; new entry, add to table
	   ;; if its a seq or a process we also have to cache the
	   ;; start time of the object: (<object> . start)
	   ;; start time is in *time-format* units 
	   (when (or (= handle *qentry-seq*)
		     (= handle *qentry-process*))
	     (set! object (cons object start)))
	   ;; handle only has type information, make full handle
	   (set! handle (%newhandle handle))
	   ;; lock scheduler out during table set if in repl
	   (if repl? (rts:scheduler-lock-lisp))
	   ;; add new entry to table
	   (hash-table-put! *qentries* handle object)
	   ;; unlock table
	   (if repl? (rts:scheduler-unlock-lisp))
	   )
	  )
    (rtsdebug #t "enqueing: data=~d type=~d time=~d repl=~d~%"
	      data handle time (if repl? 1 0))
    ;; convert to msec
    (set! flag (rts:scheduler-enqueue data
				      handle
				      ;; convert seconds to usec for C
				      (if (eq? rts:*time-format* ':sec)
					  (inexact->exact
					   (floor (* time 1000000)))
					  time)
				      (if repl? 1 0)))
    (unless (= flag 0)
      (case flag
        ((1) (err "enqueue: no room in scheduler for ~S." object))
        ((2) (err "enqueue: RTS not running."))))
    (values)))

(define (cm-hook data handle time)
  ;; C SIDE HAS LOCKED LISP DURING EXTENT OF CALLBACK
  (let ((entry #f)
        (etype (%handle-type handle))) ; get entry type from low nibble
    ;; C time is usec or msec, convert to SEC if necessary
    (when (eq? rts:*time-format* ':sec)
      (set! time (/ time 1000000.0)))
    (set! entry (if (= etype *qentry-message*)
		    data ; int messages are immediate data
		    ;; C SIDE HAS LOCKED LISP DURING EXTENT OF CALLBACK
		    (or (hash-table-get *qentries* handle)
			(error "No RTS entry for handle ~D."
			       handle))))
    (cond ((= etype *qentry-process*)
           ;; entry is (<process> . <start>)
	   (rtsdebug #t "process=~s, start=~s time=~s~%"
		     (car entry) (cdr entry) time)
           (scheduler-do-process (car entry)
                                 time
                                 (cdr entry)
                                 *rts-out*
                                 handle
				 ':rts))
          ((= etype *qentry-seq*)
	   ;; entry is ( (<seq> . <subobjects>) . <start>)
	   (rtsdebug #t "seq=~s, start=~d time=~d~%" 
		     (caar entry) (cdr entry) time)
           (scheduler-do-seq (car entry)
			     time
			     (cdr entry)
			     *rts-out*
			     handle
			     ':rts))
          (else
	   (rtsdebug #t "object=~s time=~s~%" entry time)
           (write-event entry *rts-out* time)))
    (values)))

;;;
;;; user level functions
;;; 

(define (rts-now ) (rts:scheduler-time))

(define (rts . args)
  (unless (rts:scheduler-state? ':stopped)
    (err "rts: scheduler is already running or paused."))
  (unless (or (null? args) (keyword? (car args)))
    (set! *rts-out* (pop args)))
  (unless (null? *rts-out*)
    (initialize-io *rts-out*))  ;; initalize must be called for microtuning
  (apply (function rts:scheduler-start) args)
  (format #t "; RTS running~%")
  (values))

(define rts? rts:scheduler-state?)

(define (rts-pause )
  (rts:scheduler-pause)
  (values))

(define (rts-continue )
  (rts:scheduler-continue)
  (values))

(define (rts-flush)
  (rts:scheduler-flush)
  (values))

(define (rts-hush )
  (rts-flush)
  (if *rts-out*
      (do ((i 0 (+ i 1)))
	  ((>= i 16))
	(write-event (make <midi-control-change> :time 0
			   :controller 64 :value 0 :channel i)
		     *rts-out* 0)
	(write-event (make <midi-control-change> :time 0
			   :controller 123 :value 0 :channel i)
		     *rts-out* 0)))
  (values))

(define (rts-reset ) 
  (set! *qcount* 0)
  ;;(clrhash *qentries*)	
  (hash-table-for-each *qentries*
		       (lambda (k v)
			 (hash-table-delete! *qentries* k)))
  (rts-reset-globals)
  (gc) ; do gc before and after
  (values))

(define (rts-reset-globals )
  (set! *rts-pstart* #f)
  (set! *rts-qtime* #f)
  (values))

(define (rts-stop )
  (rts:scheduler-stop)
  (format #t "; RTS stopped.~%")
  (values))

(define (rts-thread? ) (rts:rts-thread? ))

;; set hooks

(rts:scheduler-hook-set! (function cm-hook))
(rts:scheduler-add-hook! ':before-start (function rts-reset))
(rts:scheduler-add-hook! ':after-stop (function rts-reset))
(rts:scheduler-add-hook! ':error-continue (function rts-reset-globals))

;;; EOF
