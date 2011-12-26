;;; **********************************************************************
;;; AVOID SLIME DURING TESTING
;;;
;;; I. RTS TESTS WITHOUT CM LOADED
;;;

(progn (require :asdf)
       (push "/Lisp/rts/rts.asd" asdf::*central-registry*)
       (asdf::oos 'asdf::load-op :rts)
       (in-package :rts)
       )

;;; test 1: basic queue insertino

(progn
  (cffi:defcfun ("rts_queue_test" test-queue) :void (n :int))
  (cffi:defcfun ("rts_queue_print" print-queue) :void)
  (defun tq (&optional (n 0)) (test-queue n) (force-output))
  (defun pq () (print-queue) (force-output))
  )

(tq 0)   ; initialize
(tq 5)   ; add 5 nodes at random times. data is order of node creation
(tq 15)
(tq 0)   ; (re)initialize
(tq 10)
(tq 10)
(tq 0)
(quit)

;;; test 2: callback with mutex locking

(progn 
  (cffi:defcfun ("rts_test" rts-test) 
      :int (priority :int) (rate :int) (callb :pointer))
  (cffi:defcallback rts-trycallback :void ((data qdata_t) 
					   (type qtype_t)
					   (time qtime_t))
    (print (list :callback data type time)))
  )

(rts-test 20 1000 (cffi:callback rts-trycallback))
(ccl:quit)

;;; test 3  callbacks and repl accessing variable


(progn
  (defparameter *calls* 0)
  (cffi:defcallback rts-trycallback :void ((count qdata_t) 
					   (type qtype_t)
					   (time qtime_t))
		    (incf *calls*)
		    count time type)

  (cffi:defcfun ("rts_test" rts-test) 
      :int (priority :int) (rate :int) (callb :pointer))

  (defun cb ( &optional (rate 500) (priority 20))
    ;; start/stop pthread callback at specified rate
    (cond ((rts?)
	   (scheduler-stop)
	   )
	  (t
	   (format t "~&scheduling callbacks, rate=~Dms~%" rate)
	   (setq *calls* 0)
	   (rts-test priority rate (cffi:callback rts-trycallback))
	   ))
    (values))

  (defun cv (&optional val)
    ;; get the lisp lock mutex and get/set vars set by callback
    (rts-scheduler-lock-lisp)
    (if val
	(setq *calls* val )
	(setq val *calls*))
    (rts-scheduler-unlock-lisp)
    val)
  )

(cb 100)  ; start pthead doing callback evey 100ms
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
(quit)

;;;
;;; test 3: basic time accuracy of pthread callback
;;;

(progn
  (defparameter *rate* 0)
  (defparameter *calls* 0)
  (defparameter *date* 0)
  (cffi:defcallback rts-trycallback :void ((data qdata_t) 
					   (type qtype_t)
					   (time qtime_t))
		    type
		    (setq *calls* data)
		    (setq *date* time))

  (cffi:defcfun ("rts_test" rts-test) 
      :int (priority :int) (rate :int) (callb :pointer))

  (defun cb ( &optional (rate 500) (priority 20))
    ;; start/stop pthread callback at specified rate
    (cond ((rts?)
	   (scheduler-stop)
	   (format t "~&callbacks=~d, time=~dms, time/cb=~dms~%"
		   *calls* (floor *date* 1000)
		   (floor (/ *date* 1000) *calls* )))
	  (t
	   (format t "~&scheduling callbacks, rate=~Dms~%" rate)
	   (setq *calls* 0 *date* 0 *rate* rate)
	   (rts-test priority rate (cffi:callback rts-trycallback))
	   ))
    (values))
  )

(cb 100) 
;; wait...
(cb)
(quit)

;;; test 4: enquing individual entries from lisp

(progn
  (scheduler-start)
  (scheduler-time-msec)
  (defparameter *count* 0)
  (setq *scheduler-callback*
	(lambda (a b c)
	  a b c
	  (incf *count*)
	  (format t "~%data=~d type=~s time=~s"a  b c)
	  (force-output)))
       )

(dotimes (i 40)
  (scheduler-enqueue i 0 (+ (scheduler-time-msec) (* 300 i)) 1))

(dotimes (i 20)
  (scheduler-enqueue i 0 (+ (scheduler-time-msec) (* 200 i)) 1))

(quit)

;;;
;;; test 5: scheduling portmidi notes with queue
;;;

(progn
  (load "rts.asd")
  (asdf:oos 'asdf:load-op :rts)
  (push "/Lisp/portmidi/portmidi.asd" asdf:*central-registry*)
  (asdf:oos 'asdf:load-op :portmidi)  ; assumes ../portmidi exists
  )

(progn
  (in-package :rts)
  (pm:start)
  (pm:GetDeviceInfo))

(defparameter *pmdev* 1) ; SET THIS TO YOUR OUTPUT DEVICE

(progn
  (cffi:defcfun ("rts_test2" rts-test2) :int (num :int) (at :int) (rate :int))   
  (defparameter *pmout* (pm:OpenOutput *pmdev* 256 5)) ; 5ms latency
  (defparameter *pmdur* 100)
  (defparameter *pmfun*
    (lambda (data type time) 
      data time type
      (let ((key (+ (random 60) 30))
	    (now (pm:time)))
	(pm:WriteShort *pmout* now (pm:message #b10010000 key 90))
	(pm:WriteShort *pmout* (+ now *pmdur*)
		       (pm:message #b10000000 key 127))))))

(funcall *pmfun* 1 2 3) ; test sound first

(setq *scheduler-callback* *pmfun*)
(rts:scheduler-start )
(rts-test2 4 (+ (scheduler-time-msec) 2000) 1000)  ; reps ahead rate
(rts-test2 4 (+ (scheduler-time-msec) 1000) 500)
(rts-test2 20 (+ (scheduler-time-msec) 500) 50)

(rts-test2 200 (+ (scheduler-time-msec) 1000) 20)
(scheduler-pause)
(scheduler-continue)
(scheduler-stop)

;;;
;;; II. CM TESTS
;;;

(in-package :cm)
(use-system :rts)

(rts?)  ; :stopped --  maybe it should return nil?

rts:*time-format*  ; default is :sec

(rts:scheduler-time) ; -1
(rts)

(rts?) ; :running

(list (rts::scheduler-time-usec)
      (rts::scheduler-time-msec)
      (rts::scheduler-time-sec))

(now )

;;; see how much consing floats generate.

(progn (defun flotest (n)
         (loop repeat n do (rts::scheduler-time-sec)))
       (compile 'flotest)
       (defun inttest (n)
         (loop repeat n do (rts::scheduler-time-msec)))
       (compile 'inttest)
       )
(time (flotest 1000))
(time (inttest 1000))

;;; run a process, no output

(defparameter *foo* nil)

(defun foo (n ra id)
  n ra id
  (process repeat n
	   do 
	   #+openmcl
	   (progn (format t "~&foo[~s]: hiho! now=~D~%" id (now))
		  (force-output))
	   ;; ARRRG cmucl/sbcl dont print in repl under callback
	   #-openmcl (push (now) *foo*)
	   wait ra))
 
*scheduler*  ; NIL (only :rts under callback)

(rts?)  ; :running

(events (foo 4 .5 0 ) nil)  ; events can run in parallel

(sprout (foo 4 .5 0) :at (+ (now ) 1))

(sprout (list (foo 12 .5 0)
	      (foo 12 (pick .25  1) 1))
	:at (+ (now ) 1))

;; test sprouting other objects
(sprout (new seq) :at (+ (now) 1))

(sprout (new midi :time (+ (now) 2)))

;; error trapping:

(defun bar (n)
  (process repeat n
	   output (car n)
	   wait 1))

(sprout (bar 10) :at (+ (now) 1))

(rts-stop)
(quit)

;;;
;;; test midishare
;;;

(progn (use-system :rts)
       (use-system :midishare)
       (defparameter mp (midishare-open))
       (defun foo (n k r d)
         (process repeat n
                  output (new midi :time (now) :keynum k :duration d)
                  wait r))
       (defparameter endnum 0)
       (defun endless (n k w d)
	 (let ((m (new midi :time 0 :keynum k :duration d)))
	   (incf endnum)
	   (process repeat n
		    output m
		    wait w
		    finally (sprout (endless n (between 30 90) w d)))))
       )

(rts mp)

(sprout (foo 30 (between 40 90) .1 .1) )

(dotimes (i 4) (sprout (foo 30 (between 40 90) .1 .1)))
(dotimes (i 8) (sprout (foo 30 (between 40 90) .1 .1)))

;;; endless testing

(rts mp :time-format :msec)
(now)
(sprout (foo 20 60 100 100))
(setq endnum 0)
(sprout (endless 50 (between 30 90) 100 100) )
(rts-stop)

;;;
;;; portmidi test.
;;;
(defparameter mp (portmidi-open :output 3))




(let ((MYOUTPUT 5))
  (use-system :portmidi)
  (pprint (pm:getDeviceInfo))
  (defparameter mp (portmidi-open :latency 5 :output MYOUTPUT))
  (defun reps (l k r d)
    (process repeat l
	     output (new midi :time (now) :keynum k :duration d)
	     wait r))
  (use-system :rts)
  (defun foo (n k r d)
    (process repeat n 
	     output (new midi :time (now) :keynum k
			 :duration d)
	     wait r)))

(defun foo (num wai)
  (process repeat num
	   output (new midi :time (now) :keynum (between 30 90) :amplitude 100 :duration .2)
	   wait wai))

(events (foo 3))
(rts mp)
(sprout (foo 300 .1))

(length g)
(/ (apply #'+ g) 298.0)






(events (foo 4 60 .5 .4) mp)

(rts mp)

(sprout (new midi :time 0 :keynum 60) )
(sprout (new midi :time 0 :keynum 60) :at (+ (now) 1.0))

(sprout (foo 4 60 .5 .4))
;; try this one several time rapidly
(sprout (reps 40 (between 40 90) .1 .1))

(dotimes (i 4) (sprout (foo 30 (between 40 90) .1 .1)))
(dotimes (i 8) (sprout (foo 30 (between 40 90) .1 .1)))


(rts-stop)

;; millisecon timing. warning note duration is still sec!!

(rts mp :time-format :msec)

(now)

(sprout (foo 4 (between 30 90) 100 100) ))

--------------------------------------------------------------------------
BUGS----------------------------------------------------------------------
--------------------------------------------------------------------------
1 OPENMCL: SPROUTING LOTS OF NOTES TO MIDISHARE CAUSES OUTPUT TO
STOP AND DPPCCL CPU CLIMB TO 100% OR MORE. DOING (RTS:QPRINT)
SHOWS RTS APPARENTLY RUNNING NORMALLY WITH EMPTY QUEUE, IE
EVERYTHING SEEMS TO HAVE GOTTON PROCESSED.
   (dotimes (i 8) (sprout (foo 30 (between 40 90) .1 .1)))

2 OPENMCL: LISP CRASHED AFTER LEAVING THE ROOM FOR AN HOUR AND
THEN SPROUTING.

I think it happened because usec time overflowed signed
long.
? (sprout (reps 12 (between 40 90) .5 .4))
> Error in process foreign(2): value 2147568640 is not of the expected type (SIGNED-BYTE 32).
> While executing: RTS::RTS-SCHEDULER-ENQUEUE
;;;
;;; #<PROCESS foreign(2) [Foreign thread callback] #x868FD1E> requires access to Shared Terminal Input
;;;

FIX: qtime_t now unsigned long
EVEN THEN THE FLOAT AND USEC FORMATS DONT LAST VERY LONG:
(integer-length (usec (* 60 20) :sec))

--------------------------------------------------------------------------
SBCL:
PASTING THIS MULTIPLE TIMES IN THE REPL TRIGGERED AN ERROR.

(sprout (reps 12 (between 40 90) .1 .1))

IT MIGHT BE A TERMINAL INPUT ISSUE WHEN SPROUTING IN REPL WHILE
FAST CALLBACK IS HAPPENING, BUT DONT REALLY KNOW -- the io error
message might just be due to recursive errors.



(SB-IMPL::SUB-SERVE-EVENT NIL 0)
0[5] Help! 11 nested errors. SB-KERNEL:*MAXIMUM-ERROR-DEPTH* exceeded.
0: (SB-IMPL::SUB-SERVE-EVENT NIL 0)
1: (SB-SYS:WAIT-UNTIL-FD-USABLE 11 :INPUT NIL)
2: (SB-IMPL::REFILL-BUFFER/FD #<SB-SYS:FD-STREAM for "the terminal" {10B1FAE1}>)
3: (SB-IMPL::INPUT-CHAR/UTF-8 #<SB-SYS:FD-STREAM for "the terminal" {10B1FAE1}> NIL #:EOF-OBJECT)
4: (READ-CHAR #<SB-SYS:FD-STREAM for "the terminal" {10B1FAE1}> NIL #:EOF-OBJECT #<unused argument>)
5: (READ-CHAR #<SYNONYM-STREAM :SYMBOL SB-SYS:*TTY* {10B1FBD9}> NIL #:EOF-OBJECT #<unused argument>)
6: (READ-CHAR #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10B1FCB9}> NIL #:EOF-OBJECT #<unused argument>)
7: (READ-PRESERVING-WHITESPACE #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10B1FCB9}> NIL (NIL) T)
8: (READ-PRESERVING-WHITESPACE #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10B1FCB9}> NIL (NIL) NIL)
9: (READ #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10B1FCB9}> NIL (NIL) NIL)
10: (SB-DEBUG::DEBUG-READ #<SYNONYM-STREAM :SYMBOL *TERMINAL-IO* {10B1FCB9}>)
11: (SB-DEBUG::DEBUG-LOOP-FUN)
12: (INTERNAL-DEBUG)
13: (SB-DEBUG::%INVOKE-DEBUGGER #<TYPE-ERROR {10BB56A1}>)
14: ((LAMBDA NIL))
15: (SB-IMPL::CALL-WITH-SANE-IO-SYNTAX #<CLOSURE (LAMBDA NIL) {10BB5755}>)
16: (SB-IMPL::%WITH-STANDARD-IO-SYNTAX #<CLOSURE (LAMBDA NIL) {10BB572D}>)
17: (INVOKE-DEBUGGER #<TYPE-ERROR {10BB56A1}>)
18: (ERROR TYPE-ERROR)



(progn (use-system :rts)
       (use-system :midishare)
       (defparameter mp (midishare-open))
       (defun foo (n k r d)
         (process repeat n
                  output (new midi :time (now) :keynum k :duration d)
                  wait r))
       (defparameter endnum 0)
       (defun endless (n k w d)
	 (let ((m (new midi :time 0 :keynum k :duration d)))
	   (incf endnum)
	   (process repeat n
		    output m
		    wait w
		    finally (sprout (endless n (between 30 90) w d)))))
       )

(rts mp)

(dotimes (i 4) (sprout (foo 30 (between 40 90) .1 .1)))

(defun foo (num wai)
  (process repeat num
	   output (new midi :time (now) :keynum (between 30 90) :amplitude 100 :duration .2)
	   wait wai))

(sprout (foo 100 .1))


;; testing restarts


(progn (use-system :rts :warnings t)
       (use-system :portmidi)
       (pprint (pm:getDeviceInfo))
       (defparameter mp (portmidi-open  :output 5)) ; todd=3
       )

(rts mp :error-mode :stop)

;; a function that should produce no errors
(progn
  (defun foo-noerr (num wai)
    (process repeat num
	     output (new midi :time (now) :keynum (between 30 90)
			 :amplitude 100 :duration .2)
	     wait wai))
  ;;will produce a divide by zero error
  (defun foo-err (num wai)
    (process repeat num
	     output (new midi :time (now) :keynum (/ 100 (random 1))
			 :amplitude 100  :duration .2)
	     wait wai))
  )

;;this one should work
(sprout (foo-noerr 20 .1))

;; this should cause an error and kill rts
(sprout (foo-err 20 .1))

*pstart* ; pstart should be nil
(now)  ; should be -1
(rts?) ; :stopped

;;; trying to start rts again and sprout a non-error function
;;; should be ok

(rts mp)
(rts:error-mode) ; error mode is sticky, should be :stop
(sprout (foo-noerr 20 .1))

;;; this will set rts to delete queue entries that 
;;; generate errors and print a warning to standard-output

(rts:error-mode-set! :continue)

;; works fine
(sprout (foo-noerr 20 .1))

;; print warning but generate no events since 
;; the entry contains an error
(sprout (foo-err 20 .1))

*pstart* ; pstart should be nil

;; works fine again
(sprout (foo-noerr 20 .1))

;;can mix entries that error and some that don't
;; without problem

(progn
  (sprout (foo-noerr 20 .1))
  (sprout (foo-err 20 .1))
  (sprout (foo-noerr 20 .12)))

;;; under restarts

(rts:error-mode-set! :debug)

;; works fine
(sprout (foo-noerr 20 .1))

;; send into restarts - choose delete entry
(sprout (foo-err 20 .1))

*pstart* ; should be nil

;; works fine again
(sprout (foo-noerr 20 .1))

;; will be sent into restarts. if choose to delete entry. will 
;; continue playing good entries
(progn
  (sprout (foo-noerr 20 .1))
  (sprout (foo-err 20 .1))
  (sprout (foo-noerr 20 .12)))










