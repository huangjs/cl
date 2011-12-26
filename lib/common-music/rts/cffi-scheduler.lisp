;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.12 $
;;; $Date: 2006/05/30 13:08:29 $

#-cm
(defpackage :rts
  (:use :common-lisp)
  (:export #:scheduler-start #:scheduler-stop
	   #:scheduler-pause #:scheduler-continue #:scheduler-flush
	   #:scheduler-state? #:scheduler-enqueue
	   #:scheduler-lock-lisp #:scheduler-unlock-lisp
	   #:scheduler-add-hook! #:scheduler-remove-hook!
	   #:scheduler-hook-set! #:scheduler-time #:*time-format*
	   #:*priority* #:*policy* #:*resolution* #:*error-format*
	   #:current-thread #:rts-thread?
	   #:random-seed #:between #:rescale #:odds #:pickl #:drunk 
	   #:wander #:shuffle #:interpl #:rhythm ))

(in-package :rts)

(let* ((ext #+win32 "dll"
	    #+(or darwin macos macosx) "dylib"
	    #+(or linux (and clisp unix (not macos))) "so")
       (lib (make-pathname :name "librts" :type ext
			   :defaults (truename *load-pathname*))))
  (if (probe-file lib)
      (cffi:load-foreign-library lib)
      (error "librts library not found: ~S.~%
Forget to make the C library? Do
    cd ~a
    ./configure
    make
and then try again."
	     (namestring lib)
	     (namestring (make-pathname :name nil :type nil
					:defaults *load-pathname*)))))

;;;
;;; FFI
;;;

(defconstant +sched-rr+ 2)
(defconstant +sched-fifo+ 4)
(defconstant +scheduler-stopped+ 0)
(defconstant +scheduler-running+ 1)
(defconstant +scheduler-paused+ 2)
(defconstant +time-unit-usec+ 1)
(defconstant +time-unit-msec+ 2)
(defconstant +lisp-flavor+  ; unknown, cmu, gauche, openmcl, sbcl
  #-(or cmu openmcl sbcl) 0 #+cmu 1 #+openmcl 3 #+sbcl 4)
(defparameter *priority* 70)
(defparameter *policy* +sched-rr+)
(defparameter *time-format* ':sec)
(defparameter *resolution* 1000)
(defparameter *error-mode* ':continue)
(defparameter *scheduler-hook* nil)

(defparameter *rts-error-handling* nil)

;;; begin cffi interface

(cffi:define-foreign-type qtime_t () :unsigned-long)
(cffi:define-foreign-type qdata_t () :unsigned-long)
(cffi:define-foreign-type qtype_t () :unsigned-long)

(cffi:defcstruct qentry_node
  (time qtime_t)
  (data qdata_t)
  (typp qtype_t)
  (next :pointer))

(cffi:define-foreign-type qentry_t () 'qentry_node)

(cffi:defcfun ("rts_scheduler_start" rts-scheduler-start) :int
  (priority :int) (policy :int) (qsize :int) (tunit :int) (lflav :int)
  (callb :pointer))
(cffi:defcfun ("rts_scheduler_stop" rts-scheduler-stop) :int)
(cffi:defcfun ("rts_scheduler_pause" rts-scheduler-pause) :int)
(cffi:defcfun ("rts_scheduler_continue" rts-scheduler-continue) :int)
(cffi:defcfun ("rts_scheduler_state" rts-scheduler-state) :int)
(cffi:defcfun ("rts_scheduler_time_msec" rts-scheduler-time-msec) qtime_t)
(cffi:defcfun ("rts_scheduler_time_usec" rts-scheduler-time-usec) qtime_t)
(cffi:defcfun ("rts_scheduler_time_sec" rts-scheduler-time-sec) :float)
(cffi:defcfun ("rts_scheduler_lock_lisp" rts-scheduler-lock-lisp) :int)
(cffi:defcfun ("rts_scheduler_unlock_lisp" rts-scheduler-unlock-lisp) :int)
(cffi:defcfun ("rts_scheduler_enqueue" rts-scheduler-enqueue) :int
  (data qdata_t) (type qtype_t) (at qtime_t) (replp :int))

(cffi:defcfun ("current_thread" current-thread) :pointer)
(cffi:defcfun ("rts_thread_p" rts-thread-p) :int)

(cffi:defcfun ("rts_scheduler_flush" rts-scheduler-flush) :int)
(cffi:defcfun ("rts_queue_print" qprint) :void) ; debugging
(cffi:defcfun ("qentry_free" qentry-free) :void (qentry :pointer))
(cffi:defcfun ("qentry_data" qentry-data) qdata_t (qentry :pointer))
(cffi:defcfun ("qentry_time" qentry-time) qtime_t (qentry :pointer))
(cffi:defcfun ("qentry_type" qentry-type) qtype_t (qentry :pointer))
(cffi:defcfun ("qentry_next" qentry-next) :pointer (qentry :pointer))
(cffi:defcfun ("rts_test" rts-test) :int (rate :int) (policy :int) (callb :pointer))
;;; tools.c
(cffi:defcfun ("rts_random_seed" rts-random-seed) :int (state :int))
(cffi:defcfun ("rts_between" rts-between) :int (low :int) (high :int))
(cffi:defcfun ("rts_rescale" rts-rescale) :int (x :int) (x1 :int) (x2 :int) (y1 :int) (y2 :int))
(cffi:defcfun ("rts_rhythm2msec" rts-rhythm->msec) :int (rhy :int) (tempo :int))

;;;
;;; scheduling hooks.
;;;

(defparameter *hooks*
  (list :before-start (list) :before-continue (list)
	:after-pause (list) :after-stop (list)
	:error-continue (list)))

(defun get-hook (name)
  ;; return tail of *hooks* with name's hooks in car
  (let ((tail (member name *hooks*)))
    (if (null tail)
	(error "rts:get-hook: no hook named ~s." name)
	(cdr tail))))

(defun scheduler-add-hook! (name fn)
  (let ((tail (get-hook name)))
    (setf (car tail) (nconc (car tail) (list fn)))
    (values)))

(defun run-hook (name)
  (let ((tail (get-hook name)))
    (do ((l (car tail) (cdr l)))
	((null l) (values))
      (funcall (car l)))))

(defun scheduler-remove-hook! (name &optional hook)
  (let ((tail (get-hook name)))
    (if (not hook)
	(setf (car tail) (list))
	(setf (car tail) (remove hook (car tail))))
    (values)))

;;;
;;; Scheduling Callback and Hook
;;;

(defun scheduler-hook-set! (fn)
  (setf *scheduler-hook* fn))

;; this is the actual callback that gets sent to C
(cffi:defcallback scheduler_callback :void ((data qdata_t) 
					    (type qtype_t)
					    (time qtime_t))
  ;; if error trapping is too slow we can have a callback version
  ;; without error checks and allow the user to choose between them
  ;; when rts is opened.
  (case *rts-error-handling*
    ((:continue )
     (handler-case (if *scheduler-hook*
		       (funcall *scheduler-hook* 
				data type time))
       (error (c)
	 (warn "Caught ~a error under RTS callback!~%~
                Error message: ~a~%~
                Callback args: data: ~a type: ~a time: ~a.~%~
                Dropping entry and continuing."
	       (type-of c) c data type time)
	 (run-hook ':error-continue)
	 (force-output *standard-output*))))
    ((:stop )
     (handler-case (if *scheduler-hook*
		       (funcall *scheduler-hook*
				data type time))
       (error (c)
	 (progn
	   (format t "RTS stopped due to ~a error!~%~
                      Error message: ~a~%~
                      Callback args: data: ~a type: ~a time: ~a.~%" 
		   (type-of c) c data type time)
	   (scheduler-flush)
	   (scheduler-stop)
	   (force-output)))))
    ((:debug )
     (restart-case
	 (if *scheduler-hook*
	     (funcall *scheduler-hook* data type time))
       (abort-rts ()
	 :report "Abort RTS, stopping scheduler."
	 (scheduler-flush)
	 (scheduler-stop))
       (continue-rts ()
	 :report "Delete current entry and continue."
	 (run-hook :error-continue))
       (flush-queue ()
	 :report "Flush pending events from scheduler."
	 (scheduler-flush)
	 (run-hook ':error-continue))
       (remove-callback ()
	 :report "Remove callback function."
	 (setf *scheduler-hook* nil)
	 (run-hook ':error-continue))))))

;;;
;;; scheduler start
;;;

(defun scheduler-start (&key (priority *priority*)
			(policy *policy*) 
			(error-mode *error-mode*)
			(resolution *resolution*) 
			gc
			(time-format *time-format*)
			)
  gc ; gauche only
  (let ((s (rts-scheduler-state)))
    (cond ((= s +scheduler-stopped+)
	   (let (flag)
	     (case error-mode
	       ((:continue :stop :debug)
		(setq *rts-error-handling* error-mode))
	       (t (error "scheduler-start: error mode ~s is not :continue :stop or :debug." error-mode)))
	     (ecase time-format
	       ((:msec )
		(setq *time-format* time-format)
		(setq time-format +time-unit-msec+))
	       ((:usec )
		(setq *time-format* time-format)
		(setq time-format +time-unit-usec+))
	       ((:sec )
		;; :sec runs scheduler usec, lisp converts to seconds
		(setq *time-format* time-format)
		(setq time-format +time-unit-usec+)))
	     (run-hook ':before-start)
	     (setf flag (rts-scheduler-start priority
					     policy
					     resolution
					     time-format
					     +lisp-flavor+
					     (cffi:callback
					      scheduler_callback)))
	     (unless (= flag 0)
	       (error "rts:scheduler-start: error ~D" flag)))) 
	  ((= s +scheduler-running+)
	    )
	  ((= s +scheduler-paused+)
	   (scheduler-continue)))
    (values)))

(defun scheduler-stop ()
  (rts-scheduler-stop)
  (run-hook ':after-stop)
  (values))

(defun scheduler-pause ()
  (rts-scheduler-pause)
  (run-hook ':after-pause)
  (values))

(defun scheduler-continue ()
  (run-hook ':before-continue)
  (rts-scheduler-continue)
  (values))

(defun scheduler-state () (rts-scheduler-state))
(defun scheduler-enqueue (data type time repl) 
  (rts-scheduler-enqueue data type time repl))
(defun scheduler-flush () (rts-scheduler-flush))
(defun scheduler-lock-lisp () (rts-scheduler-lock-lisp))
(defun scheduler-unlock-lisp () (rts-scheduler-unlock-lisp))
(defun scheduler-time-usec () (rts-scheduler-time-usec))
(defun scheduler-time-msec () (rts-scheduler-time-msec))
(defun scheduler-time-sec () (rts-scheduler-time-sec))

(defun scheduler-time ()
  (cond ((eq *time-format* ':msec)
	 (rts-scheduler-time-msec))
	((eq *time-format* ':sec)
	 (rts-scheduler-time-sec))
	((eq *time-format* ':usec)
	 (rts-scheduler-time-usec))
	(t (error "scheduler-time: time format: ~s not :sec :msec or :usec."
	    *time-format*))))

(defun scheduler-state? (&optional status)
  (let ((state (scheduler-state)))
    (if (not status)
	(elt '(:stopped :running :paused) state)
	(ecase status
	  ((0 :stopped) (= state +scheduler-stopped+))
	  ((1 :running) (= state +scheduler-running+))
	  ((2 :paused) (= state +scheduler-paused+))))))

(defun rts-thread? ()
  (/= (rts-thread-p) 0))

;;;
;;; tools
;;;

(defun random-seed (s) (rts-random-seed s))

(defun between (a b &optional avoid)
  (if (not avoid) (rts-between a b)
      (do ((c (rts-between a b) (rts-between a b)))
	  ((/= c avoid) c))))

(defun rescale (x x1 x2 y1 y2) (rts-rescale x x1 x2 y1 y2))

(defun odds (pct &optional (tru t) (fal nil))
  (if (< (rts-between 0 100) pct) tru fal))

(defun pickl (vals &optional len)
  (elt vals (rts-between 0 (or len (length vals)))))

(defun drunk (pos step)
  (+ pos (odds 50 (- step) step)))

(defun wander (pos step &optional avoid)
  (+ pos (between (- step) step avoid)))

(defun shuffle (seq &optional (beg 0) (end (length seq)))
  (do ((i beg (+ i 1))
       (j (rts-between beg end) (rts-between beg end))
       (v nil))
      ((= i end) seq)
    (setq v (elt seq i))
    (setf (elt seq i) (elt seq j))
    (setf (elt seq j) v)))

(defun interpl (x list)
  (let* ((x1 (car list))
	 (y1 (if (null (cdr list))
		 (error "rts:interpl: malformed coordinate list ~s." list)
		 (cadr list)))
	 (x2 x1)
	 (y2 y1))
    (do ((tail (cddr list) (cddr tail)))
	((or (null tail) (> x2 x))
	 (cond ((>= x x2) y2)
	       ((<= x x1) y1)
	       (t (rts-rescale x x1 x2 y1 y2))))
      (setq x1 x2)
      (setq y1 y2)
      (setq x2 (car tail))
      (setq y2 (if (null (cdr tail))
		   (error "rts:interpl: malformed coordinate list ~s." list)
		   (cadr tail))))))

(defparameter *rhythms* (make-hash-table :test #'equal))

(flet ((dorhys (syms init then)
	 (do ((tail syms (cdr tail))
	      (val init (floor (* val then))))
	     ((null tail)  nil)
	   (setf (gethash (symbol-name (car tail)) *rhythms*) val))))
  (dorhys '(w h q e s t x) (* 480 4) 1/2)
  (dorhys '(tq th tq te ts tt tx) (* 480 4 2/3)  1/2)
  (dorhys '(w. h. q. e. s. t. x.) (* 480 4 3/2) 1/2)
  (dorhys '(w.. h.. q.. e.. s.. t.. x..) (* 480 4 7/4) 1/2))

(defun rhythm (rhy &optional (tempo 60))
  (rts-rhythm->msec (if (symbolp rhy)
			(or (gethash (symbol-name rhy) *rhythms*)
			    (error "rts:rhythm: ~s is not a rhythm." rhy))
			rhy)
		    tempo))

(pushnew ':rts *features*)

;;; eof
