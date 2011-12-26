;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.4 $
;;; $Date: 2006/03/10 17:28:32 $

;;;
;;; porting code for Gauche Scheme:
;;; http://www.shiro.dreamhost.com/scheme/gauche/index.html
;;;

(use gauche.net) ; needed for socket communication
(use gauche.fcntl) ;need to set socket to non-blocking
(use gauche.sequence)

;;gauche specific stuff for sending osc messages over udp sockets.

(define (u8vector->byte-string vec)
  (let* ((vec-len (u8vector-length vec))
         (byte-string (make-byte-string vec-len)))
    (do ((i 0 (+ i 1)))
        ((= i vec-len))
      (string-byte-set! byte-string i (u8vector-ref vec i)))
    byte-string))

;;;
;;; threads
;;;
(define thread-alive?
  (let ((unique (list 'unique)))
    (lambda (thread)
      (eq? (thread-join! thread 0 unique) unique))))

(define (thread-current-time )
  (time->seconds (current-time)))

;;;
;;; socket/udp support
;;;

;; because i want to use sendto on a unconnected socket
;; i want to encapsulate the remote host and port
;; information along with the socket.
;; udp-socket class does this .
;;
;; functions/methods needed
;; (make-udp-socket host port local-port) x
;; (udp-socket-close sock) x
;; (udp-socket-shutdown sock &optional) x
;; (udp-socket-recv sock bytes &optional flags) x
;; (udp-socket-send sock mess len) x
;; (send-osc mess osc-stream len) x
;; (make-osc-timetag offset out) x
;; (u8vector->double vec) 
;; (osc-bundle? vec) x
;; (osc-parse-timestamp vec) 
;; (osc-parse-contents vec) x
;; (osc-parse-message vec) x
;; (osc-parse-bundle vec) x
;; (osc-vector->osc-message vec) x
;;

(define-class* <udp-socket> ()
  ((socket :init-value #f)
   (remote-port :init-value #f :init-keyword :remote-port)
   (remote-host :init-value #f :init-keyword :remote-host)
   (local-port :init-value #f :init-keyword :local-port)
   (local-addr-in :init-value #f)
   (host-addr-in :init-value #f))
  :name 'udp-socket)

 (define (make-udp-socket host port local-port)
   (let ((usock (make <udp-socket> :remote-host host :remote-port port :local-port local-port)))
     (slot-set! usock 'socket (make-socket PF_INET SOCK_DGRAM))
     (sys-fcntl (socket-fd (slot-ref usock 'socket)) F_SETFL O_NONBLOCK)
     (slot-set! usock 'local-addr-in (make <sockaddr-in> :host "127.0.0.1" :port local-port))
     (socket-bind (slot-ref usock 'socket) (slot-ref usock 'local-addr-in))
     (slot-set! usock 'host-addr-in (make <sockaddr-in> :host (slot-ref usock 'remote-host)
                                          :port (slot-ref usock 'remote-port)))
     usock))

(define (udp-socket-close sock)
  (socket-close (slot-ref sock 'socket)))                          

;; is this even needed?? since
;; socket is not connection orientated
(define (udp-socket-shutdown sock . args)
  (with-args (args &optional how)
    (unless how
      (set! how 2))
    (socket-shutdown (slot-ref sock 'socket) how)))

;; this does a recvfrom on socket, however must
;; handle EAGAIN error since this indicates
;; the socket is non-blocking and no data
;; is waiting. in this case, return #f

(define (udp-socket-recv-no-block sock bytes . args)
   (with-args (args &optional flags)
     (unless flags
       (set! flags 0))
     (guard (exc ((<system-error> exc) (if (= 35 (slot-ref exc 'errno)) #f (err exc))))
            (string->u8vector (socket-recvfrom (slot-ref sock 'socket) bytes flags)))))

(define (udp-socket-recv sock bytes . args)
   (with-args (args &optional flags)
     (unless flags
       (set! flags 0))
     (string->u8vector (socket-recvfrom (slot-ref sock 'socket) bytes flags))))

(define (udp-socket-send sock mess bytes)
  bytes
  (socket-sendto (slot-ref sock 'socket) (u8vector->byte-string mess)
                 (slot-ref sock 'host-addr-in )))



;;test
;; (define *send-socket* (make-udp-socket "127.0.0.1" 9000 22011))
;; (define *recv-socket* (make-udp-socket "127.0.0.1" 22011 9000))
;; (let ((mess (make-u8vector 64 2)))
;;   (udp-socket-send *send-socket* mess 64))

;; (udp-socket-recv *recv-socket* 128)
;; (udp-socket-close *send-socket*)
;; (udp-socket-close *recv-socket*)


(define (send-osc mess osc-stream len)
   len
   (udp-socket-send (slot-ref osc-stream 'socket) mess len))


(define (make-osc-timetag offset out)
  (let* ((now (current-time))
         (offset-time (seconds->time offset))
         (target-time (seconds->time (+ (time->seconds now) (time->seconds offset-time) (slot-ref out 'latency))))
         (vec #f))
    (set! vec (make-byte-vector (+ 2208988800 (slot-ref target-time 'second))))
    (u8vector-append vec (make-byte-vector (inexact->exact (* (modf (time->seconds target-time)) #xffffffff))))))

(define (u8vector->double vec)
  (let ((dv (uvector-alias <f64vector> vec)))
    (f64vector-ref dv 0)))

(define *bundle-header-bytes* #u8(35 98 117 110 100 108 101 0))

(define (osc-bundle? vec)
  (let ((res #t))
    (if (u8vector? vec)
        (dotimes (i 8)
          (when (not (= (u8vector-ref vec i) (u8vector-ref *bundle-header-bytes* i)))
            (set! res #f)))
      (set! res #f))
    res))
          
(define (osc-parse-timestamp vec)
  (let ((ts #f))
    (setf ts (u8vector->uint (u8vector-subseq arr 0 4)))
    (setf ts (+ ts
             (exact->inexact
              (/ (u8vector->uint (u8vector-subseq arr 4 8))
                 4294967295))))))

;(find-index (lambda (x) (= x 0)) *bundle-header-bytes*)

(define (osc-parse-contents arr)
  (let ((lst (list))
	(mess #f)
	(pos #f)
	(sym-vector #f)
	(sym #f)
	(sym-len 0)
	(first-token-len #f)
	(type-list #f)
	(type-list-len #f))
    (set! first-token-len (find-index (lambda (x) (= x 0)) arr))
    (if first-token-len
	(begin
	  (set! lst (append! lst (list (string->symbol
                                        (string-upcase (u8vector->string (u8vector-subseq arr 0 first-token-len)))))))
	  (set! mess (u8vector-subseq arr (find-index (lambda (x) (= x 44))  arr)))
	  (set! type-list (loop for i across (subseq mess 0 (position 0 mess))
				collect (code-char i)))
	  (set! type-list-len (length type-list))
	  (set! mess (u8vector-subseq mess (+ type-list-len (- 4 (mod type-list-len 4)))))
	  (set! pos 0)
	  (dolist (j type-list)
            (cond ((eq? j #\i)
                   (set! lst (append! lst (list (u8vector->int (u8vector-subseq mess pos (+ pos 4))))))
                   (set! pos (+ pos 4)))
                  ((eq? j #\f)
                   (set! lst (append! lst (list (u8vector->float (u8vector-subseq mess pos (+ pos 4))))))
                   (set! pos (+ pos 4)))
                  ((eq? j #\d)
                   (set! lst (append! lst (list (u8vector->double (u8vector-subseq mess pos (+ pos 8))))))
                   (set! pos (+ pos 8)))
                  ((eq? j #\s)
                   (set! sym-vector (u8vector-subseq mess pos))
                   (set! sym (u8vector->string sym-vector))
                   (set! lst (append! lst (list (string-trim-right sym #\newline) #\/)))
                   (set! sym-len (string-length sym))
                   (if (= 0 (mod sym-len 4))
                       (set! pos (+ pos sym-len))
                     (set! pos (+ (+ sym-len (- 4 (mod sym-len 4))) pos)))))))
      (set! lst (append! lst (list (string->symbol (u8vector->string arr))))))
    lst))

(define (osc-parse-message vec)
  (osc-parse-contents vec))

(define (osc-parse-bundle arr)
  (let ((msg-len #f)
        (pos 0)
        (arr-len (u8vector-length arr))
        (bundle (list)))
    (do ()
        ((>= pos arr-len))
      (set! msg-len (u8vector->int (u8vector-subseq arr pos 4)))
      (set! pos (+ pos 4))
      (setf bundle (append! bundle (osc-parse-contents (u8vector-subseq arr pos (+ pos msg-len)))))
      (set! pos (+ pos msg-len)))
    bundle))

(define (osc-vector->osc-message arr)
  (let ((timestamp #f)
	(msg #f))
    (if (osc-bundle? arr)
	(begin 
	  (set! timestamp (osc-parse-timestamp (u8vector-subseq arr 8 16)))
	  (set! msg (osc-parse-bundle (u8vector-subseq arr 16))))
      (set! msg (osc-parse-contents arr)))
    (list msg timestamp)))



;;; osc udp stream class and methods
;;;

;;; each implementation needs a make-osc-timetag function
;;; need a send-osc function
;;; also need osc-vector->osc-message parse-osc function and 
;;; u8vector->double

(define *osc-receive-rate* .001)
;(defparameter *osc-receive-rate* .001)
;(set! *osc-receive-rate* .001)

(define-class* <osc-stream> (<event-stream>)
  ((remote-port :init-value #f :init-keyword :remote-port)
   (remote-host :init-value #f :init-keyword :remote-host)
   (local-port :init-value #f :init-keyword :local-port)
   ;; data list: (<thread> #'stopper )
   (receive-data :init-value (list #f #f)
     :accessor rt-stream-receive-data)
   (receive-mode :init-value :message :init-keyword :receive-mode
                 :accessor rt-stream-receive-mode)
   (latency :init-value 0.0 :init-keyword :latency)
   (buffer-size :init-value 512 :init-keyword :buffer-size)
   (socket :init-value #f))
  :name 'osc-stream
  :metaclass <io-class>)

(define-method* (open-io (obj <osc-stream>) dir . args)
  dir
  (let ((rh #f) (rp #f) (lp #f))
    (if (pair? args)
        (begin
          (when (> (length args) 0)
            (set! rh (list-ref args 0)))
          (when (> (length args) 1)
            (set! rp (list-ref args 1)))
          (when (> (length args) 2)
            (set! lp (list-ref args 2)))))
    (unless (io-open obj)
      (slot-set! obj 'socket
                 (make-udp-socket
                  (if rh rh (slot-ref obj 'remote-host))
                  (if rp rp (slot-ref obj 'remote-port))
                  (if lp lp (slot-ref obj 'local-port))))
      (slot-set! obj 'open #t))
    (set! *out* obj)
    obj))

(define (osc-open . args)
  (apply open-io "osc.udp" #t args))

(define (osc-open? . args)
  (with-args (args &optional (osc (find0object "osc.udp")))
    (if osc
        (let ((io (io-open osc)))
          (if io
              osc
            #f)))))

(define-method* (close-io (obj <osc-stream>) . mode)
  mode
  (when (io-open obj)
    (udp-socket-close (slot-ref obj 'socket))
    (slot-set! obj 'open #f))
  (set! *out* #f))

;;; send-msg
;;; (send-msg <list> <osc-stream>)
;;; (send-msg '("foo" 1 2 3 4 5) *osc*)


(define-method* (send-msg message (io <osc-stream>))
  (multiple-value-bind (mess len)
    (format-osc message)
    (send-osc mess io len)))


;;;send-bundle
;;; (send-bundle <time> <list> <osc-stream>)
;;; (send-bundle 1.5 '("foo" 1 2 3 4 'a) *osc*)
;;; can also nest messages
;;; (send-bundle 1.5 '(("foo" 1 2 3 4) ("baz" 1 2 3 4 5 'b)))


(define-method* (send-bundle offset message (io <osc-stream>))
  (let ((arr (make-byte-vector "#bundle"))
        (mess-len 0))
    (set! arr (u8vector-append arr (make-osc-timetag offset io)))
    ;;this should be smarter
    (if (list? (list-ref message 0))
        (begin
          (dolist (bundle-mess message)
            (multiple-value-bind (mess len)
              (format-osc bundle-mess)
              (set! arr
                    (u8vector-append arr
                                     (make-byte-vector len)
                                     mess))
              (set! mess-len (+ mess-len len))))
          (set! mess-len (+ mess-len 20))
          (send-osc arr io mess-len))
      (multiple-value-bind (mess len)
        (format-osc message)
        (set! arr
              (u8vector-append arr
                               (make-byte-vector len)
                               mess))
        (set! mess-len (+ len 8 8 4))
        (send-osc arr io mess-len)))))



(define-method* (set-receive-mode! (str <osc-stream>) mode)
  (unless (member mode '(:message :raw))
    (err "receive: ~s is not a osc receive mode." mode))
  (slot-set! str 'recmode mode))

(define-method* (stream-receive-init (str <osc-stream>) mode)
  args
  (let ((data (rt-stream-receive-data str))
        (mode (rt-stream-receive-mode str))) 
    (cond ((not (member mode '(:message :raw)))
           (error "receive: ~s is not a osc receive mode."
                  mode))
          ((not (procedure? func))
           (error "Receive: hook is not a procedure: ~s" func))
          ((not (osc-open? str))
           (error "Stream not open for input: ~S." str))
          ((first data)
           (error "Can't set input hook: another hook is running!")))
    (let* ((in (io-open str))
           (fn #f))
      (if (eq mode ':raw)
          (setf fn func)
        (setf fn (lambda (m) (funcall func (osc-vector->osc-message m)))))
      (list-set! data 0 fn)
      (list-set! data 1 in))))

(define-method* (stream-receive-deinit (str <osc-stream>))
  (let ((data (rt-stream-receive-data str)))
    (when (first data)
      (list-set! data 0 #f)
      (list-set! data 1 #f))))


;;pthread is the only receive type in gauche
(define-method* (stream-receive-thunks (stream <osc-stream>) type . other)
  other
  (let* ((data (rt-stream-receive-data stream))
         (hook (first data))            ; user's hook
         (runp #t))                       ; flag for stopping
    (case type
      (':pthreads ;;pthreads mode uses blocking receive and therefore does not need wait 
       (values (lambda ()
		 (do ()
                     ((not runp) #t)
		   (let ((n 0))
		     (setf n (udp-socket-recv (slot-value stream 'socket)
					      (slot-value stream 'buffer-size)))
		     (if n
			 (funcall hook n)))))
	       (lambda () (set! runp #f)))))))


;;
;;  rts
;;



;; (define-macro (with-mutex-grabbed args . body)
;;   (let ((mutex (car args)))
;;     `(with-locking-mutex ,mutex (lambda () ,@body))))

(define-macro (with-mutex-grabbed args . body)
  (let ((mutex (car args)))
    `(dynamic-wind
         (mutex-lock! ,mutex)
         (funcall (lambda () ,@body))
         (mutex-unlock! ,mutex))))


(define-macro (without-interrupts . body)
  `(begin
     ,@body))



(define (absolute-time)
  (let ((now (call-with-values sys-gettimeofday cons)))
    (+ (car now) (/ (cdr now) 1000000.0))))

;;why?
(define (get-current-time-of-day)
  (absolute-time))

(define (thread-wait condi mutex mode delay)
  (let ((delaytime 0)
        (timeout 0))
    (case mode
      (':sec
       (set! delaytime delay))
      (':msec
       (set! delaytime (/ delay 1000.0))))
    
    (set! timeout (seconds->time (+ (time->seconds (current-time)) delaytime)))
    ;(format #t "threadwaiting! at ~s until ~s~%" (current-time) timeout)
    (mutex-unlock! mutex condi timeout)
    ;(format #t "threaddonewaiting!~%")
    ))


(define (run-pthread thunk . args)
  args
  (let ((th (make-thread  thunk)))
    (thread-start! th)))
  


(define *rts-priority* 80);;not really applicable

(define *rts-run* #f)
(define *rts-wakeup-cond* #f)
(define *rts-thread-lock* #f)
(define *rts-time-offset* #f)
(define *rts-pause-cache* #f) ; (<time> . queue...)

(define (rts-time)
  (if (not *rts-time-offset*)
      (begin (set! *rts-time-offset* (absolute-time))
             0.0)
    (- (absolute-time) *rts-time-offset*)))

(define (rts-sprout obj at)
  (let ((wakeup #f))
    (if (not *pstart*)
        (begin
          (mutex-lock! *qlock*)
          ;(format #t "locked~%")
          (set! wakeup (or (null? (%q-head *queue*))
                           (< at (%qe-time (%q-head *queue*)))))
          (sprout-aux obj at)
          (mutex-unlock! *qlock*)
          ;(format #t "unlocked~%")
          )
      (begin
        (set! wakeup (or (null? (%q-head *queue*))
                         (< at (%qe-time (%q-head *queue*)))))
        (sprout-aux obj at)))
    (if wakeup (rts-wakeup))
    (values)))

(define (rts-stop)
  (set! *rts-run* #f)
  (rts-wakeup)
  (rts-reset)
  (values))

(define (rts-wakeup)
  (when *rts-wakeup-cond*
    (condition-variable-broadcast! *rts-wakeup-cond*))
  (values))

(define (rts-pause)
  (when (list? *rts-pause-cache*)
    (error "rts-pause: rts already paused."))
  (set! *rts-pause-cache* (list (absolute-time)))
  ;; remove the queue so rts waits
  (with-mutex-grabbed (*qlock*)
    (unless (null? (%q-head *queue*))
      (set! (cdr *rts-pause-cache*) (%q-head *queue*))
      (set! (%q-head *queue*) (list))))
  (values))

(define (rts-continue)
  (unless (list? *rts-pause-cache*)
    (error "rts-continue: rts not paused."))
  (let ((diff (- (absolute-time)
                 (car *rts-pause-cache*))))
    (cond ((null? (cdr *rts-pause-cache*))
           (set! *rts-pause-cache* #f))
          (#t
           ;; increment queue entries by time paused
           (do ((ptr (cdr *rts-pause-cache*) (%qe-next ptr)))
               ((null? ptr) #f)
             (%qe-time-set! ptr (+ (%qe-time ptr) diff))
             (when (%qe-start ptr)
               (%qe-start-set! ptr (+ (%qe-start ptr) diff))))
           (set! (%q-head *queue*) (cdr *rts-pause-cache*))
           ;;    (setq *qtime* (rts-time))
           (set! *rts-pause-cache* #f)
           (rts-wakeup)))
    (values)))

(define (rts-flush)
  (begin
    (mutex-lock! *qlock*)
    (%q-flush *queue*)
    (mutex-unlock! *qlock*)
    (values)))

(define (rts-hush)
  ;; flush queue and stop any sounding note ons if its a midi
  ;; stream. i guess there should really be an io-hush generic for
  ;; this.
  (rts-flush)
  (when *out*
    (write-event (make <midi-control-change> :time 0
                       :controller 123 :value 0)
                 *out* 0))
  (values))

(define (rts-reset)
  ;; only call this after an error
  (when (and *queue* (not (null? (%q-head *queue*))))
    (%q-flush *queue*))
  (set! *queue* #f)
  (set! *scheduler* #f)
  (set! *rts-run* #f)
  (set! *pstart* #f)
  (set! *qnext* #f)
  (set! *qtime* #f)
  (values))


(define (rts?)
  (cond ((not *rts-run*) #f)
        (*rts-pause-cache* ':paused)
        (#t ; could also distinguish between :idle and :running...
         ':running)))



(define (rts-scheduler)
  (unwind-protect
    (begin
      ;; start by locking the queue
      (when (mutex-lock! *qlock*) ;;return #t when locked
        (begin
          (set! *rts-run* #t)
          (do ((qempty #f) (curtime #f))
              ((not *rts-run*))
            (set! curtime (rts-time))
            (set! qempty (null? (%q-head *queue*)))
            (do ((entry #f) (start #f) (thing #f) (qetime #f))
                ((or qempty
                     (> (%qe-time (%q-peek *queue*)) curtime)))
              (set! entry (%q-pop *queue*))
              (set! start (%qe-start entry))
              (set! thing (%qe-object entry))
              (set! qetime (%qe-time entry))
                                        ;(format t "start ~s, qetime ~s~%" start qetime)
                                        ;(if (< (- curtime qetime) 0.001D0)
                                        ;    (setf qetime curtime))
              (%qe-dealloc *queue* entry)
              ;; process events should not attempt to lock the queue
              ;; unless our mutex supports recursive locking from the same thread
              (process-events thing curtime start *out*)
              (set! curtime (rts-time))
              (set! qempty (null? (%q-head *queue*))))
            ;; there was a race condition here where a new event
            ;; could be inserted and the scheduler woken up, but
            ;; it has not started blocking yet
            ;; so it will miss the new event
            ;; however now *qlock* is locked the entire time until
            ;; we reach one of the pthread_cond calls
            (if (not qempty)
                (let ((delay #f))
                  ;; here perhaps we should not use rts-time since we
                  ;; may want a more accurate amount of time to sleep
                  ;;(pthreads:with-mutex-grabbed (*qlock*)
                  ;;(setf curtime (rts-time))
                  (set! curtime (- (absolute-time) *rts-time-offset*))
                  (set! delay (- (%qe-time (%q-peek *queue*)) curtime))
                  ;; delay in seconds
                  (when (> delay 0)
                    ;; avoid sleeping for very short durations
                    ;; (when (< delay 0.001) (setf delay 0.001))
                    (thread-wait *rts-wakeup-cond* *qlock*  ;; needs rewriting
                                 :sec delay)))               ;; to srfi
              (begin
                ;; queue is empty -- wait for wakeup event
                (mutex-unlock! *qlock* *rts-wakeup-cond* )))))
        (begin
          (mutex-unlock! *qlock*)
          (set! *rts-run* #f))))))



(define (rts . args)
  (if (rts?)
      (error "rts already running."))
  (let* ((stream (if (or (null? args)
                         (keyword? (car args)))
                     (current-output-stream)
                   (pop args)))
         (verbose #t)
         (priority *rts-priority*)
         (receiver #f))
;; tmi - will fix later
    ;; (if (not (null? args))
;;         (begin
;;           (verbose (list-prop args  ':verbose))
;;           (priority (list-prop args ':priority))
;;           (receiver (list-prop args ':receiver))))
    ;;(declare (ignore receiver))
    (set! *queue* %q)
    (set! *out* stream)
    (set! *scheduler* ':pthread)
    (when (not *qlock*)
      (set! *qlock* (make-mutex)))
    (when (not *rts-thread-lock*)
      (set! *rts-thread-lock* (make-mutex)))
    (when (not *rts-wakeup-cond*)
      (set! *rts-wakeup-cond* (make-condition-variable)))
    ;; hkt offset now cached on first call to rt-time
    ;;(setq *rts-time-offset* (pthreads:absolute-time))
    ;; start up a scheduler thread
    (when verbose (format #t "~&; rts running!~%"))
    (set! *rts-time-offset* #f)
    (run-pthread rts-scheduler :priority priority)
    (values)))



;; (define *sc* (sc-open))
;; (sc-dumposc #t)
;; (rts #f *sc*)
;; (define *foo* '(0))

;; (define (sc-simple-1 num wai)
;;   (process repeat num with delta = (now)
;;            do
;; 	   (output (new simple :time (now) 
;; 		       :freq (between 300 700)
;; 		       :dur (between 10 20)
;; 		       :amp .1
;; 		       :pan (pickl '(-1.0 0 1.0))) :to *sc*)
;;            do (begin
;;                 (push (- (now) delta) *foo*)
;;                 (set! delta (now)))
;; 	   wait wai))
;;
;; (sprout (sc-simple-1 30 .1))
;; (sprout (sc-simple-1 30 .01))