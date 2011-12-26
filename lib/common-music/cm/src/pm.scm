;;; **********************************************************************
;;; Copyright (C) 2005, 2006 Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the terms of this agreement.
;;; **********************************************************************

;;; $Revision: 1.47 $
;;; $Date: 2006/05/30 12:55:37 $

(in-package :cm)

;;; each midiport stream can handle 1 input and/or 1 output
;;; connection.  open streams are held as a list: (<input><output>)
;;; where either value can be a pointer to an open PortMidiStream or
;;; #f

(define *portmidi-default-input* #f)
(define *portmidi-default-output* #f)
(define *portmidi-default-latency* 5)
(define *portmidi-default-inbuf-size* 512)
(define *portmidi-default-outbuf-size* 2048)
(define *portmidi-default-filter* 0)
(define *portmidi-default-mask* 0)

(define-class* <portmidi-stream> (<rt-stream> <midi-stream-mixin>)
  ((input :init-value *portmidi-default-input* :init-keyword :input
         :accessor portmidi-input)
   (output :init-value *portmidi-default-output* :init-keyword :output
           :accessor portmidi-output)
   (latency :init-value *portmidi-default-latency* :init-keyword :latency
            :accessor rt-stream-latency)
   (inbufsize :init-value *portmidi-default-inbuf-size*
              :init-keyword :inbuf-size
              :accessor portmidi-inbuf-size)
   (outbufsize :init-value *portmidi-default-outbuf-size* 
               :init-keyword :outbuf-size
               :accessor portmidi-outbuf-size)
   ;; data list: (<thread> #'stopper <evbuf> <len>)
   (receive-data :init-value (list #f #f #f #f)
            :accessor rt-stream-receive-data)
   (receive-mode :init-value :message :init-keyword :receive-mode
            :accessor rt-stream-receive-mode)
   (filter :init-value *portmidi-default-filter* :init-keyword :filter
           :accessor portmidi-filter)
   (mask :init-value *portmidi-default-mask* :init-keyword :channel-mask
         :accessor portmidi-channel-mask)
   (offset :init-value 0 :init-keyword :offset 
           :accessor portmidi-offset))
  :name 'portmidi-stream
  :metaclass <io-class>
  :file-types '("*.pm"))

(define-object-printer* ((obj <portmidi-stream>) port)
  (let ((name (object-name obj))
        (pids (event-stream-stream obj))
        (*print-case* ':downcase)) ; noop in scheme
    (set! name
          (if name 
              (format #f "~a \"~a\"" (class-name (class-of obj)) name)
              (format #f "~a" (class-name (class-of obj)))))
    (if pids
        (if (car pids)
            (if (cadr pids)
                (format port "#<~a (in:~d out:~d)>"
                        name (car pids) (cadr pids))
                (format port "#<~a (in:~d)>"
                        name (car pids)))
            (if (cadr pids)
                (format port "#<~a (out:~d)>" 
                        name (cadr pids))
                (format port "#<~a>" name)))
        (format port "#<~a>" name))))
            
(define-method* (open-io (obj <portmidi-stream>) dir . args)
  dir args
  (when (not (io-open obj)) ; already open...
    (unless (pm:PortMidi)
      (err "Can't open PortMidi connection: PortMidi not loaded."))
    (unless (event-stream-stream obj)
      (let ((getd (lambda (i d l)
                    ;; return device description of user spec (string or int)
                    (cond ((not i) #f)
                          ((eq? i #t)
                           ;; default
                           (pm:GetDeviceInfo
                            (if (eq? d ':input)
                                (pm:GetDefaultInputDeviceID)
                                (pm:GetDefaultOutputDeviceID)))
                           )
                          ((not (eq? d (list-prop l ':type))) #f)
                          ((string? i)
                           (if (string-ci=? i (list-prop l ':name))
                               l ;(list-prop l :id)
                               #f))
                          ((integer? i)
                           (if (eq? (list-prop l ':id) i)
                               l ;i
                               #f))
                          (else #f))))
            (devs (pm:GetDeviceInfo) )
            (bsiz (portmidi-outbuf-size obj))
            (idev #f)
            (odev #f)
            (pids (list #f #f))
            (data (list #f #f)))
        (do ((tail devs (cdr tail))
             (i (portmidi-input obj))
             (o (portmidi-output obj)))
            ((null? tail) #f)
          (if (and i (not idev))
              (set! idev ( getd i ':input (car tail))))
          (if (and o (not odev))
              (set! odev ( getd o ':output (car tail)))))
        ;; error checks: no devices, bad devices, device already open,
        ;; missing devices
        (cond ((null? devs)
               (err "open-io: no PortMidi devices available."))
              ((and idev (list-prop idev ':open))
               (err "open-io: PortMidi input device ~D (~S) is already open."
                    (list-prop idev ':id) (list-prop idev ':name)))
              ((and odev (list-prop odev ':open))
               (err "open-io: PortMidi output device ~D (~S) is already open."
                    (list-prop odev ':id) (list-prop odev ':name)))
              ((and (not idev)  (portmidi-input obj))
               (err "open-io: '~S' is not a valid :input id. Available devices are: ~S."
                    (portmidi-input obj)
                    devs))
              ((and (not odev)  (portmidi-output obj))
               (err "open-io: '~S' is not a valid :output id. Available devices are: ~S."
                    (portmidi-output obj)
                    devs))
              ((and (not idev) (not odev))
               (err "open-io: Missing :input and/or :output id. Valid devices: ~S."
                    devs)))
        (pt:Start)
        (set! (object-time obj) 0)
        (when idev
          (set! idev (list-prop idev ':id))
          (set-car! pids idev)
          (set-car! data 
                    (pm:OpenInput
                     idev (if (pair? bsiz) (car bsiz) bsiz)))
         
          (list-set! (rt-stream-receive-data obj) 0 (car data)) ;pointer to input
          (list-set! (rt-stream-receive-data obj) 1 idev) ;input device number
          (list-set! (rt-stream-receive-data obj) 2 (if (pair? bsiz) (car bsiz) bsiz)))
        (when odev
          (set! odev (list-prop odev ':id))
          (set-car! (cdr pids) odev)
          (set-car! (cdr data) 
                    (pm:OpenOutput
                     odev (if (pair? bsiz) (cadr bsiz) bsiz)
                     (rt-stream-latency obj))))
        (set! (event-stream-stream obj) pids)
        (set! (io-open obj) data))))
  obj)

(define-method* (close-io (obj <portmidi-stream>) . mode)
  (when (and (eq? (car mode) ':force)
             (io-open obj))
    (let ((data (io-open obj)))
      (if (car data)
          (pm:Close (car data)))
      (if (cadr data)
          (pm:Close (cadr data)))
      (set! (event-stream-stream obj) #f)
      (list-set! (rt-stream-receive-data obj) 0 #f)
      (list-set! (rt-stream-receive-data obj) 1 #f)
      (set! (io-open obj) #f)))
  (values))

(define-method* (initialize-io (obj <portmidi-stream>))
  ;; this method will have to distinguish between rt and non-rt
  ;; scheduling and also input vs output.
  (let ((io (io-open obj))
        (fn (lambda (o s)
              (let ((f (portmidi-filter o))
                    (m (portmidi-channel-mask o)))
                (if (pair? f)
                    (apply (function pm:SetFilter) s f)
                    (pm:SetFilter s f))
                (pm:SetChannelMask s m)))))
    ;; input stream
    (if (car io) ( fn obj (car io)))
    ;; output stream
    (if (cadr io) ( fn obj (cadr io)))
    ;;cache current MILLISECOND time offset
    (set! (portmidi-offset obj) (pt:Time))
    ;; stream's time is kept in seconds. maybe stream should have a
    ;; scaler like midi-file so user could work with whatever quanta
    ;; they wanted...
    (set! (object-time obj) 0)
    (channel-tuning-init obj)))

;; a "midi.port" convenience for working with just one stream...

(define (portmidi-open . args)
  (apply (function open-io) "midi-port.pm" #t args))

(define (portmidi-open? . args)
  (with-args (args &optional (port (find-object "midi-port.pm" )))
    (if port 
        (let ((io (io-open port)))
          (if io
              (if (car io)
                  (if (cadr io) :inout :in)
                  (if (cadr io) :out #f))
              #f))
        #f)))

(define (portmidi-close . args)
  (with-args (args &optional (port (find-object "midi-port.pm" )))
    (if (portmidi-open? port)
        (begin
          (if (equal? ':running (recv? port))
              (recv-stop port))
          (close-io port ':force))
      port)
    port))

;;;
;;; message format conversion
;;;

(define (pm-message->midi-message pmm)
  (let ((status (pm:Message.status pmm)))
    (if (< status #xf0)                 ; channel message
        (if (logtest status #b10000000) ; normal status
            (let ((stat (ash (logand status #xf0) -4))
                  (dat2 (pm:Message.data2 pmm)))
              (when (and (= stat +ml-note-on-opcode+)
                         (= 0 dat2))
                (set! stat +ml-note-off-opcode+)
                (set! dat2 127))
              (make-channel-message stat
                                    (logand status #x0f)
                                    (pm:Message.data1 pmm)
                                    dat2))
            (err "pm-message->midi-message: running status :("))
        (if (= status #xff)
            (err "pm-message->midi-message: meta message??!")
            (let ((type (logand status #x0f)))
              (if (= type 0)            ; sysex
                  (err "pm-message->midi-message: sysex :(")
                  (make-system-message (logior (ash type 4) #xf)
                                       0 
                                       (pm:Message.data1 pmm)
                                       (pm:Message.data2 pmm))))))))

(define (midi-message->pm-message mm)
  (pm:Message (logior (ash (midimsg-upper-status mm) 4)
                      (midimsg-lower-status mm))
              (channel-message-data1 mm)
              (channel-message-data2 mm)))

(define-method* (midi-write-message (obj <integer> )
                                    (str <portmidi-stream>)
                                    scoretime data)
  data
  (cond ((sysex-p obj)
         ;; add in later..
         )
        ((or (channel-message-p obj)
             (system-message-p obj))
         (pm:WriteShort 
          (second (io-open str))  ; output stream
          (if (scheduling-mode? ':events)
	      (+ (inexact->exact (round (* scoretime 1000)))
		 (portmidi-offset str))
	      (pt:Time))
          (midi-message->pm-message obj)))))

;;; portmidi behaves almost like a midi-file: (1) handles only true
;;; midi messages (ie no durations); (2) data must always be sent in
;;; time-increasing order; (3) messages are just bytes.

(define-method* (write-event (obj <midi> ) (str <portmidi-stream>)
                             scoretime)
  (let ((keyn (midi-keynum obj))
        (chan (midi-channel obj))
        (ampl (midi-amplitude obj))
	(sched (scheduling-mode)))
    ;; if amplitude is zero then don't output anything
    (ensure-velocity ampl keyn)
    (ensure-microtuning keyn chan str)
    ;; if "resting" then dont update anything
    (unless (< keyn 0)                  ; rest
      ;; Optimize sending <midi> by calling WriteShort directly
      ;; pass time value if scheduling.
      (pm:WriteShort
       (second (io-open str))
       (if (eq? sched ':events)
	   (+ (inexact->exact (round (* scoretime 1000)))
	      (portmidi-offset str))
	   (pt:Time))
       (pm:Message (logior #x90 (logand chan #xf) )
                   (logand keyn #x7f)
                   (logand ampl #x7f)))
      ;; enqueue a note off in scheduler's queue. time formats
      ;; MUST be the same
      (enqueue *qentry-message*
	       (make-note-off chan keyn 127)
               (+ scoretime (midi-duration obj))
               #f sched))
    (values)))

(define-method* (write-event (obj <midi-event>) (str <portmidi-stream>)
                             scoretime)
  (midi-write-message (midi-event->midi-message obj) str scoretime #f))


(define-method* (write-event (obj <integer>) (str <portmidi-stream>)
                             scoretime)
  (midi-write-message obj str scoretime #f) )

;;;
;;; pm:output function and clause for  output portmidi events, buffers,
;;; messages and CM midi messages in realtime

(define (pm:now )
  (pt:Time))

(define (pm:output msg . args)
  (with-args (args &key at (to *out*) raw)
    (cond ((number? msg)
           (pm:WriteShort (second (io-open to))
                          (or at (pt:Time))
                          (if raw msg (midi-message->pm-message msg))))
          ((string? msg)
           (pm:WriteSysEx (second (io-open to))
                          (or at (pt:Time))
                          msg))
          (else
           (pm:Write (second (io-open to))
                     msg
                     raw)))
    (values)))

(define (parse-pm-output forms clauses ops)
  clauses ops
  (let ((head forms)
        (oper (pop forms))
        (expr #f)
        (args (list))
        (to #f)
        (loop '()))
    (when (null? forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (set! expr (pop forms))
    (do ((stop #f))
        ((or stop (null? forms)))
      (case (car forms)
        (( to :to)
         (when (null? (cdr forms))
           (loop-error ops head "Missing '" oper " to' expression."))
         (set! args (append! args (list ':to (cadr forms))))
         (set! to #t)
         (set! forms (cddr forms)))
        (( at :at )
         (when (null? (cdr forms))
           (loop-error ops head "Missing '" oper " at' expression."))
         (set! args (append! args (list ':at (cadr forms))))
         (set! forms (cddr forms)))
        (( raw :raw )
         (when (null? (cdr forms))
           (loop-error ops head "Missing '" oper " raw' expression."))
         (set! args (append! args (list ':raw (cadr forms))))
         (set! forms (cddr forms)))        
        (else
         (set! stop #t))))
    (unless to (set! args (append! args (list ':to '*out*))))
    (set! loop (list `(,oper ,expr ,@args)))
    (values (make-loop-clause 'operator oper 'looping loop)
            forms)))

(define *process-operators*
  (append *process-operators*
          (list (list 'pm:output (function parse-pm-output)
                      'task 'to 'at 'raw))))


;;;
;;; midi recording
;;;

;; (define (portmidi-record! seq . mp)
;;   (let* ((str (if (pair? mp) (car mp)
;;                   (find-object "midi-port.pm" )))
;;          (opn (if (null? str) #f
;;                   (portmidi-open? str))))
;;     (cond ((not opn)
;;            (err "portmidi-record!: portmidi stream not open for input."))
;;           ((eq? opn ':out)
;;            (err "portmidi-record!: ~s only open for output." str))
;;           (else
;;            ;; otherwise set true if we also perform midi thru
;;            (set! opn (if (eq? opn :inout) #t #f))))
;;     (if (not seq)
;;         (remove-receiver! str)
;;         (let ((ins (if (subobjects seq) #t #f)) ; insert into existing seq
;;               (off #f)                          ; time offset
;;               (map (make-list 16 (list)))) ; channel map for on/off pairing
;;           (if (receiver? str)
;;               (err "portmidi-record!: receiver already active."))
;;           (set-receiver!
;;            (lambda (mm ms)
;;              (if opn (write-event mm str ms)) ; midi thru 
;;              (if (not off) (set! off ms)) ; cache time of first message
;;              (cond ((or (note-off-p mm)
;;                         (and (note-on-p mm) (= 0 (note-on-velocity mm))))
;;                     (let* ((chn (note-off-channel mm))
;;                            (key (note-off-key mm))
;;                            (ons (list-ref map chn)))
;;                       ;; ons is time ordered list ((<on> . <ms>) ...)
;;                       (when (pair? ons) ; have on to pair with
;;                         (let ((aon #f)  ; pair with earliest on
;;                               (obj #f))
;;                           (cond ((= (note-on-key (car (car ons))) key)
;;                                  (set! aon (car ons))
;;                                  (list-set! map chn (cdr ons)))
;;                                 (else
;;                                  ;; search for corresponding on, splice out
;;                                  (do ()
;;                                      ((or (null? (cdr ons)) aon)
;;                                       #f)
;;                                    (if (= key (note-on-key
;;                                                (car (car (cdr ons)))))
;;                                        (begin (set! aon (car (cdr ons)))
;;                                               (set-cdr! ons (cddr ons)))
;;                                        (set! ons (cdr ons))))))
;;                           ;; aon is (<msg> . <ms>)
;;                           (when aon
;;                             (set! obj (make <midi> 
;;                                             :time (/ (- (cdr aon) off)
;;                                                      1000.0)
;;                                             :duration (/ (- ms (cdr aon))
;;                                                          1000.0)
;;                                             :keynum (note-on-key (car aon))
;;                                             :amplitude (/ (note-on-velocity
;;                                                            (car aon))
;;                                                           127.0)
;;                                             :channel (note-on-channel
;;                                                       (car aon))))
;;                             ;; insert if seq contained objects else append
;;                             (if ins (insert-object seq obj)
;;                                 (append-object obj seq)))))))
;;                    ((note-on-p mm)
;;                     (let* ((chn (note-on-channel mm))
;;                            (ons (list-ref map chn)))
;;                       ;; append mm to time ordered ons ((<on> . <ms>) ...)
;;                       (if (null? ons)
;;                           (list-set! map chn (list (cons mm ms)))
;;                           (append! ons (list (cons mm ms))))))
;;                    ((channel-message-p mm)
;;                     (midi-message->midi-event mm :time (/ (- ms off)
;;                                                           1000.0)))))
;;            str)))))
