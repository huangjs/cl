;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.11 $
;;; $Date: 2006/05/30 13:08:29 $

(select-module rts)

(let* ((fil (sys-realpath (port-name (current-load-port))))
       (dir (sys-dirname fil))
       (lib (format #f "~a/libgaucherts.~a" dir
		    (if (equal? (car (sys-uname)) "Darwin")
			"dylib"
			"so"))))
  (if (file-exists? lib)
      (c-load-library lib)
      (errorf "libgaucherts library not found: ~S.~%
Forget to make the C library? Do
    cd ~a
    ./configure
    make
and then try again." lib dir)
      ))

(define-constant +sched-rr+ 2)
(define-constant +sched-fifo+ 4)
(define-constant +scheduler-stopped+ 0)
(define-constant +scheduler-running+ 1)
(define-constant +scheduler-paused+ 2)
(define-constant +time-unit-usec+ 1)
(define-constant +time-unit-msec+ 2)
(define-constant +lisp-flavor+ 2) ; unknown, cmu, gauche, openmcl, sbcl

(define rts:*priority* 70)
(define rts:*policy* +sched-rr+)
(define rts:*time-format* ':sec)
(define rts:*resolution* 1000)
(define rts:*error-mode* ':continue)
(define rts:*scheduler-hook* #f)

;; genwrapper rts /usr/include/sys/types.h ./scheduler.h ./librts.dylib > gauche-ffi.scm

(define <c-struct:_pthread_handler_rec> (alloc-c-struct '_pthread_handler_rec))
(init-c-struct! <c-struct:_pthread_handler_rec> `((routine unquote (make-c-func-ptr <c-void> (list (ptr <c-void>)))) (arg unquote (ptr <c-void>)) (next unquote (ptr <c-struct:_pthread_handler_rec>))))
(define <c-struct:_opaque_pthread_t> (alloc-c-struct '_opaque_pthread_t))
(init-c-struct! <c-struct:_opaque_pthread_t> `((sig unquote <c-long>) (cleanup_stack unquote (ptr <c-struct:_pthread_handler_rec>)) (opaque unquote (make-c-array <c-char> 596))))
(define <pthread_t> (ptr <c-struct:_opaque_pthread_t>))
(define <qtime_t> <c-ulong>)
(define <qdata_t> <c-ulong>)
(define <qtype_t> <c-ulong>)
(define <c-struct:qentry_node> (alloc-c-struct 'qentry_node))
(init-c-struct! <c-struct:qentry_node> `((time unquote <qtime_t>) (data unquote <qdata_t>) (typp unquote <qtype_t>) (next unquote (ptr <c-struct:qentry_node>))))
(define <qentry_t> <c-struct:qentry_node>)
(define <rts_lisp_callback> (make-c-func-ptr <c-void> (list <qdata_t> <qtype_t> <qtime_t>)))
(define rts_scheduler_start (make-c-func 'rts_scheduler_start <c-int> (list <c-int> <c-int> <c-int> <c-int> <c-int> <rts_lisp_callback>)))
(define rts_scheduler_stop (make-c-func 'rts_scheduler_stop <c-int> (list)))
(define rts_scheduler_reset (make-c-func 'rts_scheduler_reset <c-void> (list)))
(define rts_scheduler_flush (make-c-func 'rts_scheduler_flush <c-int> (list)))
(define rts_scheduler_pause (make-c-func 'rts_scheduler_pause <c-int> (list)))
(define rts_scheduler_continue (make-c-func 'rts_scheduler_continue <c-int> (list)))
(define rts_scheduler_state (make-c-func 'rts_scheduler_state <c-int> (list)))
(define rts_scheduler_time_sec (make-c-func 'rts_scheduler_time_sec <c-float> (list)))
(define rts_scheduler_time_usec (make-c-func 'rts_scheduler_time_usec <qtime_t> (list)))
(define rts_scheduler_time_msec (make-c-func 'rts_scheduler_time_msec <qtime_t> (list)))
(define rts_scheduler_lock_lisp (make-c-func 'rts_scheduler_lock_lisp <c-int> (list)))
(define rts_scheduler_unlock_lisp (make-c-func 'rts_scheduler_unlock_lisp <c-int> (list)))
(define rts_scheduler_enqueue (make-c-func 'rts_scheduler_enqueue <c-int> (list <qdata_t> <qtype_t> <qtime_t> <c-int>)))
(define rts_thread_p (make-c-func 'rts_thread_p <c-int> (list)))
(define current_thread (make-c-func 'rts_current_thread <pthread_t> (list)))

(define QSIZE 1024)
(define qentry_nodes_init (make-c-func 'qentry_nodes_init <c-void> (list <c-int>)))
(define qentry_alloc (make-c-func 'qentry_alloc (ptr <qentry_t>) (list <qdata_t> <qtype_t> <qtime_t>)))
(define qentry_data (make-c-func 'qentry_data <qdata_t> (list (ptr <qentry_t>))))
(define qentry_type (make-c-func 'qentry_type <qtype_t> (list (ptr <qentry_t>))))
(define qentry_time (make-c-func 'qentry_time <qtime_t> (list (ptr <qentry_t>))))
(define qentry_next (make-c-func 'qentry_next (ptr <qentry_t>) (list (ptr <qentry_t>))))
(define qentry_free (make-c-func 'qentry_free <c-void> (list (ptr <qentry_t>))))
(define qentry_print (make-c-func 'qentry_print <c-void> (list (ptr <qentry_t>))))
(define rts_queue_init (make-c-func 'rts_queue_init <c-void> (list <c-int>)))
(define rts_queue_free (make-c-func 'rts_queue_free <c-void> (list)))
(define rts_queue_empty_p (make-c-func 'rts_queue_empty_p <c-int> (list)))
(define rts_queue_peek (make-c-func 'rts_queue_peek (ptr <qentry_t>) (list)))
(define rts_queue_pop (make-c-func 'rts_queue_pop (ptr <qentry_t>) (list)))
(define rts_queue_last (make-c-func 'rts_queue_last (ptr <qentry_t>) (list)))
(define rts_queue_add (make-c-func 'rts_queue_add <c-void> (list (ptr <qentry_t>))))
(define rts_queue_prepend (make-c-func 'rts_queue_prepend <c-void> (list (ptr <qentry_t>))))
(define rts_queue_append (make-c-func 'rts_queue_append <c-void> (list (ptr <qentry_t>))))
(define rts_queue_print (make-c-func 'rts_queue_print <c-void> (list)))
(define rts_queue_test (make-c-func 'rts_queue_test <c-void> (list <c-int>)))
(define rts_test (make-c-func 'rts_test <c-int> (list <c-int> <c-int> <rts_lisp_callback>)))
;;; tools.c
(define rts-random-seed (make-c-func 'rts_random_seed <c-int> (list <c-int>)))
(define rts-between (make-c-func 'rts_between <c-int> (list <c-int> <c-int>)))
(define rts-rescale (make-c-func 'rts_rescale <c-int> (list <c-int> <c-int> <c-int> <c-int> <c-int> )))
(define rts-rhythm->msec (make-c-func 'rts_rhythm2msec <c-int> (list <c-int> <c-int> )))

;;; end generated

(define rts-test rts_test)

;;;
;;; scheduling hooks.
;;;

(define *rts-hooks*
  (list :before-start (list) :before-continue (list)
	:after-pause (list) :after-stop (list)
	:error-continue (list)))

(define (get-hook name)
  ;; return tail of *hooks* with name's hooks in car
  (let ((tail (member name *rts-hooks*)))
    (if (null? tail)
	(errorf "rts:get-hook: no hook named ~s." name)
	(cdr tail))))

(define (rts:scheduler-add-hook! name fn)
  (let ((tail (get-hook name)))
    (set-car! tail (append! (car tail) (list fn)))
    (values)))

(define (rts:run-hook name)
  (let ((tail (get-hook name)))
    (do ((l (car tail) (cdr l)))
	((null? l) (values))
      ;; call each hook
      ( (car l) ))))

(define (rts:scheduler-remove-hook! name . hook)
  (let ((tail (get-hook name)))
    (if (null? hook)
	(set-car! tail (list))
	(set-car! tail (do ((l (car tail) (cdr l))
			    (n (list)))
			   ((null? l) 
			    (reverse! n))
			 (if (not (eq? (car hook) (car l)))
			     (set! n (cons (car l) n))))))
    (values)))

;;;
;;; Scheduling Callback and Hook
;;;

(define *rts-error-handling* #f)

(define (rts:scheduler-hook-set! fn)
  (set! rts:*scheduler-hook* fn))

;; this is the actual callback that gets sent to C
(define %scheduler_callback
  (lambda (data type time)
    (if rts:*scheduler-hook* 
	(rts:*scheduler-hook* data type time))))

;;;
;;; scheduler-start
;;;

(define (rts:scheduler-start . args)
  (let ((s (rts_scheduler_state)))
    (cond ((= s +scheduler-stopped+)
	   (let ((priority (get-keyword ':priority args
					rts:*priority*))
		 (policy (get-keyword ':policy args
				      rts:*policy*))
		 (resolution (get-keyword ':resolution args
					  rts:*resolution*))
		 (error-mode (get-keyword ':error-mode args
					  rts:*error-mode*))
		 (time-format (get-keyword ':time-format args
					   rts:*time-format*))
		 (gcmode (get-keyword ':gc args #t))
		 (flag #f))
	     (case error-mode
	       ((:continue :stop)
		(set! *rts-error-handling* error-mode))
	       (else
		(errorf "scheduler-start: error mode ~s is not :continue or :stop."
			error-mode)))
	     (case time-format
	       ((:msec )
		(set! rts:*time-format* time-format)
		(set! time-format +time-unit-msec+))
	       ((:usec )
		(set! rts:*time-format* time-format)
		(set! time-format +time-unit-usec+))
	       ((:sec )
		;; :sec runs scheduler usec, lisp converts to seconds
		(set! rts:*time-format* time-format)
		(set! time-format +time-unit-usec+))
	       (else
		(errorf "scheduler-start: time format ~s not :sec :msec or :usec")))
	     (cond ((eq? gcmode #t) (set! gcmode 0))
		   ((eq? gcmode #f) (set! gcmode -1))
		   ((and (exact? gcmode) (> gcmode 0)) #f)
		   (else
		    (error "scheduler-start: gc mode ~s not true, false or integer greater than zero." gcmode)))
	     (rts:run-hook ':before-start)
	     (set! flag (rts_scheduler_start priority
					     policy
					     resolution
					     time-format
					     +lisp-flavor+
					     %scheduler_callback
					     ))
	   (if (not (= flag 0))
	       (errorf "rts:scheduler-start: error ~D" flag))))
	  ((= s +scheduler-running+)
	   #f)
	  ((= s +scheduler-paused+)
	   (scheduler-continue))))
  (values))

(define (rts:scheduler-stop )
  (rts_scheduler_stop)
  (rts:run-hook ':after-stop)
  (values))

(define (rts:scheduler-pause )
  (rts_scheduler_pause)
  (rts:run-hook ':after-pause)
  (values))

(define (rts:scheduler-continue )
  (rts:run-hook ':before-continue)
  (rts_scheduler_continue)
  (values))

(define rts:scheduler-state rts_scheduler_state)
(define rts:scheduler-enqueue rts_scheduler_enqueue)
(define rts:scheduler-flush rts_scheduler_flush)
(define rts:scheduler-lock-lisp rts_scheduler_lock_lisp)
(define rts:scheduler-unlock-lisp rts_scheduler_unlock_lisp)
(define rts:scheduler-time-usec rts_scheduler_time_usec)
(define rts:scheduler-time-msec rts_scheduler_time_msec)
(define rts:scheduler-time-sec rts_scheduler_time_sec)

(define (rts:scheduler-time )
  (cond ((eq? rts:*time-format* ':msec)
	 (rts_scheduler_time_msec))
	((eq? rts:*time-format* ':sec)
	 (rts_scheduler_time_sec))
	((eq? rts:*time-format* ':usec)
	 (rts_scheduler_time_usec))
	(else (errorf "scheduler-time: time format: ~s not :sec :msec or :usec."
	       rts:*time-format*))))

(define (rts:scheduler-state? . status)
  (let ((state (rts_scheduler_state)))
    (if (null? status)
	(list-ref '(:stopped :running :paused) state)
	(case (car status)
	  ((0 :stopped) (= state +scheduler-stopped+))
	  ((1 :running) (= state +scheduler-running+))
	  ((2 :paused) (= state +scheduler-paused+))
	  (else
	   (errorf "scheduler-state?: illegal status: ~s" 
		   (car status)))))))

(define (rts:rts-thread?)
  (> (rts_thread_p) 0))

(define rts:current-thread current_thread)

;;;
;;; tools
;;;

(define rts:random-seed rts-random-seed)

(define (rts:between a b . args)
  (let ((avoid (if (null? args) #f (car args))))
    (if (not avoid) (rts-between a b)
	(do ((c (rts-between a b) (rts-between a b)))
	    ((/= c avoid) c)))))

(define rts:rescale rts-rescale)

(define (rts:odds pct . args)
  (let ((tru (if (null? args) #t (car args)))
	(fal (if (or (null? args) (null? (cdr args)))
		 #f (cadr args))))
    (if (< (rts-between 0 100) pct) tru fal)))

(define (rts:pickl vals . args)
  (let ((len (if (null? args) #f (car args))))
    (list-ref vals (rts-between 0 (or len (length vals))))))

(define (rts:drunk pos step)
  (+ pos (rts:odds 50 (- step) step)))

(define (rts:wander pos step . args)
  (let ((avoid (if (null? args) #f (car args))))
    (+ pos (rts:between (- step) step avoid))))

(define (rts:shuffle seq . args)
  (let ((beg (if (null? args) 0 (car args)))
	(end (if (or (null? args) (null? (cdr args)))
		 (length seq) (cadr args))))
    (do ((i beg (+ i 1))
	 (j (rts-between beg end) (rts-between beg end))
	 (v #f))
	((= i end) seq)
      (set! v (list-ref seq i))
      (set-car! (list-tail seq i) (list-ref seq j))
      (set-car! (list-tail seq j) v))))

(define (rts:interpl x list)
  (let* ((x1 (car list))
	 (y1 (if (null? (cdr list))
		 (err "rts:interpl: malformed coordinate list ~s." list)
		 (cadr list)))
	 (x2 x1)
	 (y2 y1))
    (do ((tail (cddr list) (cddr tail)))
	((or (null? tail) (> x2 x))
	 (cond ((>= x x2) y2)
	       ((<= x x1) y1)
	       (else (rts-rescale x x1 x2 y1 y2))))
      (set! x1 x2)
      (set! y1 y2)
      (set! x2 (car tail))
      (set! y2 (if (null? (cdr tail))
		   (err "rts:interpl: malformed coordinate list ~s." list)
		   (cadr tail))))))

(define *rhythms* (make-hash-table 'equal?))

(let ((dorhys (lambda (syms init then)
		(do ((tail syms (cdr tail))
		     (val init (inexact->exact (floor (* val then)))))
		    ((null? tail) #f)
		  (hash-table-put! *rhythms* (symbol->string (car tail))
				   val)))))
  (dorhys '(w h q e s t x) (* 480 4) 1/2)
  (dorhys '(tq th tq te ts tt tx) (* 480 4 2/3) 1/2)
  (dorhys '(w. h. q. e. s. t. x.) (* 480 4 3/2) 1/2)
  (dorhys '(w.. h.. q.. e.. s.. t.. x..) (* 480 4 7/4) 1/2))

(define (rts:rhythm rhy . args)
  (let ((tempo (if (null? args) 60 (car args))))
    (rts-rhythm->msec (if (symbol? rhy)
			  (or (hash-table-get *rhythms* (symbol->string rhy))
			      (err "rts:rhythm: ~s is not a rhythm." rhy))
			  rhy)
		      tempo)))

(provide "rts")
