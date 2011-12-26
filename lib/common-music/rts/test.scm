;;; **********************************************************************
;;; AVOID SLIME DURING TESTING
;;;
;;; I. RTS TESTS WITHOUT CM LOADED
;;;

(load "./gauche-scheduler.scm")
(select-module rts)


;;; test 1: basic queue insertino

(begin
  (define (tq . n) (rts_queue_test (if (null? n) 0 (car n))))
  (define (pq ) (rts_queue_print))
  )

(tq 0)   ; initialize
(tq 5)   ; add 5 nodes at random times. data is order of node creation
(tq 15)
(tq 0)   ; (re)initialize
(tq 10)
(tq 10)
(tq 0)
(exit)

;;; test 2: basic callback

(load "/Lisp/rts/gauche-scheduler.scm")

(begin
  (select-module rts)
  (define (rts-test1 count type time)
    (format #t "count: ~s type: ~s time: ~s~%"
	    count type time))
  (define rts-test rts_test))

(rts-test 20 500 rts-test1)
(exit)

;;; test 3: callbacks with repl mutex locking

(begin
  (select-module rts)
  (define *calls* 0)
  (define (rts-trycallback count type time)
    count time type
    (set! *calls* (+ *calls* 1))
    ;;(print *calls*)
    )
  (define %rts_callback  rts-trycallback)
  (define (cb . args )
    ;; start/stop pthread callback at specified rate
    (let ((rate (if (null? args) 500 (car args)))
	  (priority (if (or (null? args) (null? (cdr args)))
			20 (cadr args))))
      
      (cond ((rts:scheduler-state? ':running)
	     (rts:scheduler-stop)
	     )
	    (else
	     (format #t "scheduling callbacks, rate=~Dms~%" rate)
	     (set! *calls* 0)
	     (rts-test priority rate rts-trycallback))
	     )
      (values)))

  (define (cv . val)
    ;; get the lisp lock mutex and get/set vars set by callback
    (rts:lock-lisp)
    (if (not (null? val))
	(begin (set! *calls* (car val)) (set! val *calls*))
	(set! val *calls*))
    (rts:unlock-lisp)
    val)
  )

(cb 500)  ; start pthead doing callback evey 100ms
(cb)      ; stop
*calls*

(cb 10) ; start pthead doing callback every 10ms
(cv)   ; read var incremented by callback
(cv -1000) ; set var affected by callback
(cv)
(cb) ; stop callbacks

(cb 1) ; start pthead doing callback every 1ms
(cv)   ; read var incremented by callback
(cv -10000) ; set var affected by callback
(cv)
(cb) ; stop callbacks
(exit)

;;;
;;; callbacks using queue
;;;

(load "./gauche-scheduler.scm")
(select-module rts)

(define rts:*scheduler-callback*
  (lambda (a b c) (format #t "~S ~S ~S~%" a b c)))

(rts:scheduler-start :time-format :msec)

(rts:scheduler-time)

(rts:scheduler-enqueue 0 1 (+ (rts:scheduler-time) 1000) 1)

(do ((now (+ (rts:scheduler-time) 500) (+ now 100))
     (num 1 (+ num 1)))
    ((= num 10) #t)
  (rts:scheduler-enqueue num 0 now 1))


;;; test 4 basic time accuracy of pthread callback

(load "./gauche-scheduler.scm")

(begin
  (select-module rts)
  (define *rate* 0)
  (define *calls* 0)
  (define *date* 0)
  (define (rts-trycallback data type time)
    (set! *calls* data)
    (set! *date* time))
  (define (cb rate priority)
    ;; start/stop pthread callback at specified rate
    (cond ((rts:scheduler-state? ':running)
	   (rts:scheduler-stop)
	   (format #t "callbacks=~d, time=~dms, time/cb=~dms~%"
		   *calls*
		   (inexact->exact (floor (/ *date* 1000)))
		   (inexact->exact (floor (/ (/ *date* 1000) *calls*)))))
	  (else
	   (format #t "scheduling callbacks, rate=~Dms~%" rate)
	   (set! *calls* 0)
	   (set! *date* 0)
	   (set! *rate* rate)
	   (rts-test priority rate rts-trycallback)
	   ))
    (values))
  )

(cb 50 20)
(cb #f #f)

;;;
;;; CM tests
;;;

;;; cm -l gosh

(add-load-path "/Lisp/rts/")
(use rts)

(define (foo n ra id)
  (process repeat n
	   do (format #t "foo[~s]: hiho! now=~s~%"
		      id (now))
	   wait ra)
  )

(events (foo 10 .5 0) #f)

(rts)

(sprout (foo 10 .5 0) :at (+ (now) 1))

(sprout (list (foo 12 .5 0)
	      (foo 12 (pick .25  1) 1))
	:at (+ (now ) 1))
