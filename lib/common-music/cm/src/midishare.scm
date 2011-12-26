;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube
;;; 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the terms of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.15 $
;;; $Date: 2007/07/01 00:06:47 $

(define *ms* #f) ; midi.port's MidiShare client refnum.

(define *midi-connections*
  ;; input       output
  '("MidiShare" "MidiShare"))

(define-class* <midishare-stream> (<rt-stream> <midi-stream-mixin>)
  ((connections :init-thunk (lambda () *midi-connections*)
               :init-keyword :connections
               :accessor midishare-stream-connections)
   (receive-data :init-value (list #f #f)
                 :accessor rt-stream-receive-data)
   (receive-mode :init-value :raw :init-keyword :receive-mode
                 :accessor rt-stream-receive-mode)
   (latency :init-value 0 :init-keyword :latency
            :accessor midishare-stream-latency)
   )
  :name 'midishare-stream
  :metaclass <io-class>
  :file-types '("*.ms")
  )

;(define-class* <midishare-port> (<midishare-stream>)
;  ()
;  :name 'midishare-port
;  :metaclass <io-class>
;  :file-types '("midi.port"))

(define-method* (midishare-stream-refnum (obj <midishare-stream>))
  ;; return client refnum if open
  (let ((ref (event-stream-stream obj)))
    (if (pair? ref)
      (car ref)
      #f)))

(define-method* (open-midishare-client (obj <midishare-stream>)
                                      name)
  (ms:MidiOpen name))

(define-method* (close-midishare-client (obj <midishare-stream>))
  (ms:MidiClose (midishare-stream-refnum obj)))
                                      
(define-method* (open-io (obj <midishare-stream>) dir . args)
  ;; this method is used by both midishare and player streams.
  ;; if stream is set then return open else open and cache refnum
  dir args
  ;; check to make sure midishare is actually loaded.
  ;; apparently cmu returns 0/1 not nil/t
  (let ((res (ms:MidiShare)))
    (unless (equal? res 1)
      (err "Can't open MidiShare connection: MidiShare not loaded.")))
  (let ((name (object-name obj)))
    (unless (event-stream-stream obj)
      (let* ((client (if (or (not name)
                             (string-ci=? name "midi-port.ms"))
                       "Common Music"
                       (filename-name name)))
             (applid (ms:MidiGetNamedAppl client))
             (refnum  (if (= applid -4)
                        (open-midishare-client obj client)
                        applid )))
        (when (= refnum -4)
          (err "Cannot open Midishare client ~S."
               client))
        (set! (event-stream-stream obj)
              (midishare-set-connections obj refnum))
        (set! (io-open obj)
              (event-stream-stream obj))
        (set! (object-time obj) 0)
        ))
    obj))

(define (midishare-set-connections obj client)
  ;; refnum is id of client app, usually "Common Music"
  (let ((conn (midishare-stream-connections obj))
        (inref #f)
        (outref #f)
        (conn? (lambda (x y) (= 1 (ms:MidiIsConnected x y))))
        (getref
         (lambda (x)
           (cond ((string? x)
                  (if (string=? x "") #f 
                    (let ((a (ms:MidiGetNamedAppl x)))
                      (if (= a -4)
                        (err "Not a Midishare client: ~s." x)
                        a))))
                 ((integer? x)
                  (let ((a #f))
                    (dotimes (i (ms:MidiCountAppls))
                      (if (= x (ms:MidiGetIndAppl (+ i 1))) 
                        (set! a x)))
                    (or a (err "Not a Midishare client refnum: ~S." x))))
                 ((eq? x #t)
                  ;; default is to connect to midishare
                  0)
                 ((not x)
                  #f)
                 (else
                  (err "Not a MidiShare client: ~s." x))))))
    (cond ((pair? conn)
           (set! inref (getref (first conn)))
           (when inref
             (unless (conn? inref client)
               (ms:MidiConnect inref client -1)
               (unless (conn? inref client)
                 (err "Input connection from ~S to ~S failed."
                      (ms:MidiGetName inref) (ms:MidiGetName client)))))
           (when (pair? (cdr conn))
             (set! outref (getref (second conn)))
             (when outref
               (unless (conn? client outref)
                 (ms:MidiConnect client outref -1)
                 (unless (conn? client outref)
                   (err "Output connection from ~S to ~S failed."
                        (ms:MidiGetName client)
                        (ms:MidiGetName outref)))))))
          (else
           (set! outref (getref conn))
           (when outref
             (unless (conn? client outref)
               ;; -1 = #t
               (ms:MidiConnect client outref -1)
               (unless (conn? client outref)
                 (err "Output connection from ~S to ~S failed."
                      (ms:MidiGetName client)
                      (ms:MidiGetName outref)))))))
    ;; value is:
    ;; <clientref> <inref> <outref> <tracknum>
    ;; the tracknum is only used by player streams.
    (list client inref outref #f)))

(define-method* (close-io (obj <midishare-stream>) . mode)
  (if (eq? (car mode) ':force)
    (begin
     (close-midishare-client obj)
     (set! (event-stream-stream obj) #f)
     (set! (io-open obj) #f)))
  (values))

;;;
;;; midishare-port routines. represents the CM application for MidiShare. 
;;; there is only one instance of midishare-port per session.
;;;

;(define-method* (open-io (obj <midishare-port>) dir . args)
;  dir args
;  (when (not (io-open obj))
;    (next-method)
;    (let ((dat (io-open obj)))
;      ;; (client inref outref ...)
;      (if (second dat) (set! *in* obj))
;      (if (third dat) (set! *out* obj))
;      )
;    (set! *ms* (midishare-stream-refnum obj)))
;  obj)

;(define-method* (close-io (obj <midishare-port>) . mode)
;  ;; dont close unless error.
;  (when (eq? (car mode) ':force)
;    ;; flush any exusting receive hook!
;    (if (ms:receive?) (ms:receive))
;    (let ((dat (io-open obj)))
;      (if (second dat) (set! *in* #f))
;      (if (third dat) (set! *out* #f)))
;    (next-method)
;    (set! *ms* #f))
;  (values))

(define (midishare-open . args)
  (apply (function open-io) "midi-port.ms" #t args))

(define (midishare-open? . ms )
  (let ((p (if (null? ms) (find-object "midi-port.ms")
              (car  ms))))
    (let ((d (and p (io-open p))))
      (if d (if (second d)
                (if (third d) :inout :in)
                (if (third d) :out #f))
          #f))))

(define (midishare-close . ms)
  (let ((p (if (null? ms) (find-object "midi-port.ms")
               (car ms))))
    (if (and p (midishare-open? p))
        (close-io (find-object "midi-port.ms") ':force))
    (values)))

;(define (midi-reset . args)
;  (with-args (args &key (port 0))
;    (if (midi-open?)
;      (ms:MidiSend *ms* (ms:new typeReset :port port))
;      (warn "MidiShare not open."))))
;
;(define (midi-all-notes-off )
;  (if (midi-open? )
;    (ms:MidiSend *ms* (ms:new typeCtrlChange :controller 120))
;    (warn "MidiShare not open.")))
;
;(define (midi-hush . args)
;  ;; MidiShare has no way of flushing pending messages.
;  ;; This closes and then reopens the midi connection.
;  (with-args (args &optional ms)
;    (let ((s (if (not ms)
;               (find-object "midi.port" #f)
;               (if (string? ms)
;                 (find-object ms #f)
;                 ms))))
;      (when (and (is-a? s <midishare-stream>)
;                 (io-open s))
;        (close-io s ':force)
;        (open-io s #t))
;      s)))

;;;
;;; initialize-stream and deinitialize-stream
;;;

(define-method* (initialize-io (obj <midishare-stream>))
  ;; cache current time offset
  (set! (object-time obj)
        (+ (ms:MidiGetTime)
           (midishare-stream-latency obj)))
  (channel-tuning-init obj))

(define-method* (deinitialize-io (obj <midishare-stream>))
  (set! (object-time obj) 0))

;;;
;;; ms:new -- "high level" MidiEv constructor
;;;

(define (ms:new . args)
  (with-args (args type &key (date 0) (port 0) (chan 0) 
                   (pitch 60) (vel 64) (dur 500)
                   (pressure 0) (controller 0) (change 0)
                   (bend 0) (program 0)
                   (numerator 4) (denominator 4) (clocks 24) (32nds 8)
                   (sign 0) (mode 0) (tempo 120) (number 0) (prefix 0)
                   (text "") (position 0) (song 0) (data '())
                   (offset '()))
    (let ((ev (ms:MidiNewEv type)))
      ;; common to all MidiEvs
      (ms:date ev date)
      (ms:port ev port)
      (ms:chan ev chan)
      (cond ((<= ms:typeNote type ms:typeKeyOff)
             (ms:field ev 0 pitch)
             (ms:field ev 1 vel)
             (when (eq? type ms:typeNote)
               (ms:field ev 2 dur)))  ; dur=16bits 
            ((= type ms:typeKeyPress)
             (ms:field ev 0 pitch)
             (ms:field ev 1 pressure))
            ((= type ms:typeCtrlChange)
             (ms:field ev 0 controller)
             (ms:field ev 1 change))
            ((= type ms:typeProgChange)
             (ms:field ev 0 program))
            ((= type ms:typeChanPress)
             (ms:field ev 0 pressure))
            ((= type ms:typePitchBend)
             ;; :bend is -8192 to 8191, 0 is no bend.
             (ms:bend ev (if (<= -8192 bend 8191) bend
                             (err ":bend value ~s not between -8192 8191."
                                  bend))))
            ;;
            ;; system common and realtime
            ;;
            ((= type ms:typeSongPos)
             (let ((num (/ position 6)))
               (multiple-value-bind (msb lsb)
                   (clfloor num 128)
                 (ms:field ev 0 msb)
                 (ms:field ev 1 lsb))))
            ((= type ms:typeSongSel)
             (ms:field ev 0 song))
            ((<= typeClock type ms:typeReset) ; no data bytes
             #f)
            ((= type ms:typeSysEx)
             (do ((l data (cdr l)))
                 ((null? l) #f)
               (ms:MidiAddField ev (car l))))
            ;;
            ;; meta messages. cannot be sent to external synth
            ;;
            ((= type ms:typeSeqNum)
             (ms:field ev 0 number))
            ((<= ms:typeTextual type ms:typeCuePoint)
             (ms:text ev text))
            ((= type ms:typeChanPrefix)
             (ms:field ev 0 prefix))
            ((= type ms:typeEndTrack)
             #f)
            ((= type ms:typeTempo)
             (ms:field ev 0 (inexact->exact 
                          (floor (/ 60000000 tempo)))))
            ((= type ms:typeSMPTEOffset)
             (unless (= (length offset) 5)
               (err ":offset value ~s not (hr min sec frame subframe)"
                    offset))
             (ms:field ev 0 (+ (* (list-ref offset 0) 3600)
                            (* (list-ref offset 1) 60)
                            (list-ref offset 2)))
             (ms:field ev 1 (+ (* (list-ref offset 3) 100)
                            (list-ref offset 4))))
            ((= type ms:typeTimeSign)
             (ms:field ev 0 numerator)
             (ms:field ev 1 denominator)
             (ms:field ev 2 clocks)
             (ms:field ev 3 32nds))
            ((= type ms:typeKeySign)
             (ms:field ev 0 (if (<= -7 sign 7)
                           (if (< sign 0) (+ sign 256) sign)
                           (err ":sign value ~s is not between -7 and 7."
                                sign)))
             (ms:field ev 1 mode))
            (else
             (err "Unimplemented MidiShare event type opcode: ~s."
                  type)))
      ev)))

;;;
;;; MidiEv  printing
;;;

(define MidiEvNames
  #("Note" "KeyOn" "KeyOff" "KeyPress" "CtrlChange" "ProgChange"
    "ChanPress" "PitchBend" "SongPos" "SongSel" "Clock" "Start"
    "Continue" "Stop" "Tune" "ActiveSens" "Reset" "SysEx" "SeqNum" 
    "Text" "Copyright" "SeqName" "InstrName" "Lyric" "Marker" "CuePoint"
    "ChanPrefix" "EndTrack" "Tempo" "SMPTEOffset" "TimeSign" "KeySign"))

(define (ms:MidiPrintEv ev . args)
  (with-args (args &key (stream #t) (eol #t))
    (let ((to stream)
          (ty (ms:evType ev))
          (cl #f))
      (cond ((<= ms:typeNote ty ms:typeSysEx)
             (set! cl (vector-ref MidiEvNames ty)))
            ((<= ms:typeSeqNum ty ms:typeKeysign)
             (set! cl (vector-ref MidiEvNames
                                  (+ (- ty ms:typeSeqNum) ms:typeSysEx 1))))
            (else #f))
      (when (eq? eol ':before) (terpri stream))
      (if (not cl)
          (format to "~s" ev)
          (let ((ch (ms:chan ev))
                (po (ms:port ev))
                (da (ms:date ev)))
            (format to "#<MidiEv ~a port=~s chan=~s date=~s" cl po ch da)
            ;; value printing is really primitive for now
            (cond ((<= ms:typeNote ty ms:typeKeyOff)
                   (if (= ty ms:typeNote)
                       (format to " dur=~s" (ms:field ev 3)))
                   (format to " pitch=~s vel=~s" (ms:field ev 0)
                           (ms:field ev 1)))
                  ((= ty ms:typePitchBend)
                   ;; could print bend value
                   (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
                  ((= ty ms:typeProgChange)
                   ;; could print GM Program name
                   (format to " ~s" (ms:field ev 0)))
                  ((= ty ms:typeCtrlChange)
                   ;; could print controller type
                   (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
                  ((= ty ms:typeSongPos)
                   (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
                  ((= ty ms:typeSongSel)
                   (format to " ~s" (ms:field ev 0)))
                  ((= ty ms:typeSysEx)
                   ;; could print ID and length
                   #f)
                  ((= ty ms:typeSeqNum)
                   (format to " ~s" (ms:field ev 0)))
                  ((<= ms:typeTextual ty ms:typeCuePoint)
                   ;; could print lisp text string
                   #f)
                  ((= ty ms:typeChanPrefix)
                   (format to " ~s" (ms:field ev 0)))
                  ((= ty ms:typeEndTrack)
                   #f)
                  ((= ty ms:typeTempo)
                   (format to " ~susec" (ms:field ev 0)))
                  ((= ty ms:typeSMPTEOffset)
                   (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
                  ((= ty ms:typeTimeSign)
                   (format to " ~s ~s ~s ~s" (ms:field ev 0) (ms:field ev 1)
                           (ms:field ev 2) (ms:field ev 3)))
                  ((= ty ms:typeKeySign)
                   (format to " ~s ~s" (ms:field ev 0) (ms:field ev 1)))
                  (else
                   #f))
            (format to ">")))
      (when (eq? eol #t) (format to "~%"))
      )))

(define-method* (write-event (obj <midi>) (stream <midishare-stream>)
                             scoretime)
  (let* ((key (midi-keynum obj))
         (amp (midi-amplitude obj))
         (loc (logical-channel (midi-channel obj)
                               (midi-stream-channel-map stream)))
         (prt (car loc))
         (chn (cadr loc))
	 (dur #f) 
	 (at #f)
	 (sched (scheduling-mode))
         (evt #f))
    (cond ((eq? sched ':events)
	   ;; events: add stream offset (includes latency)
	   (set! dur (inexact->exact
		      (floor (* (midi-duration obj) 1000))))  
	   (set! at
		 (+ (object-time stream) 
		    (inexact->exact (floor (* scoretime 1000))))))
	  ((eq? sched ':rts)
	   ;; rts: add latency to ms:now.
	   (set! dur
		 (cond ((eq? rts:*time-format* ':msec)
			(midi-duration obj))
		       ((eq? rts:*time-format* ':sec)
			(inexact->exact
			 (floor (* (midi-duration obj) 1000))))
		       (else
			(inexact->exact
			 (floor (midi-duration obj) 1000)))))
	   (set! at 
		 (+ (midishare-stream-latency stream)
		    (ms:MidiGetTime))))
	  (else
	   ;; repl: 0=ms:now, <int>=msec <float>=ahead in sec
	   (set! dur (inexact->exact
		      (floor (* (midi-duration obj) 1000))))
	   (set! at (let ((tim (object-time obj)))
		      (if (= tim 0) (ms:MidiGetTime)
			  (if (integer? tim) tim
			      (+ (inexact->exact (floor (* tim 1000)))
				 (ms:MidiGetTime))))))))
    (cond ((and (exact? amp)
                (<= 0 amp 127))
           #f)
          ((and (inexact? amp)
                (<= 0.0 amp 1.0))
           (set! amp (inexact->exact
                      (floor (* amp 127)))))
          (else
           (err "Can't convert amplitude ~s to midi velocity."
                amp)))
    (ensure-microtuning key chn stream)
    (set! evt (ms:new ms:typeNote :port prt :chan chn
                      :pitch key :vel amp :dur dur))
    ;;(ms:port evt prt)
    ;;(ms:chan evt chn)
    ;;(ms:pitch evt key)
    ;;(ms:dur evt dur)
    ;;(ms:vel evt amp)
    
    (ms:MidiSendAt (midishare-stream-refnum stream)
                   evt
		   at)
    (values)))

;;; translate mi_d opcodes to midishare opcodes, only supports channel
;;; messages. will be thrown out once cm's low-level byte messages
;;; have been removed from the system.
;;;

(define opcodes-evtypes
  (list
   ;; channel
   (list (list +ml-note-on-opcode+ 1) ;typeKeyOn
         (list +ml-note-off-opcode+ 2) ;typeKeyOff
         (list +ml-key-pressure-opcode+ 3) ;typeKeyPress
         (list +ml-control-change-opcode+ 4) ;typeCtrlChange
         (list +ml-program-change-opcode+ 5) ;typeProgChange
         (list +ml-channel-pressure-opcode+ 6) ;typeChanPress
         (list +ml-pitch-bend-opcode+ 7)) ;typePitchBend
   ;; system
;;    (list (list (ash +ml-msg-sysex-type+ -4) ms:typeSysEx)
;;          (list (ash +ml-msg-mtc-quarter-frame-type+ -4) #f)
;;          (list (ash +ml-msg-song-position-type+ -4) ms:typeSongPos)
;;          (list (ash +ml-msg-song-select-type+ -4) ms:typeSongSel)
;;          (list (ash +ml-msg-cable-select-type+ -4) #f)
;;          (list (ash +ml-msg-tune-request-type+ -4) ms:typeTune )
;;          (list (ash +ml-msg-eox-type+ -4) #f)
;;          (list (ash +ml-msg-timing-clock-type+ -4) ms:typeClock)
;;          (list (ash +ml-msg-timing-tick-type+ -4) #f)
;;          (list (ash +ml-msg-start-type+ -4) ms:typeStart)
;;          (list (ash +ml-msg-continue-type+ -4) ms:typeContinue)
;;          (list (ash +ml-msg-stop-type+ -4) ms:typeStop)
;;          (list (ash +ml-msg-active-sensing-type+ -4) ms:typeActiveSens)
;;          (list (ash +ml-msg-system-reset-type+ -4) ms:typeReset))
;;    ;; meta
;;    (list (list +ml-file-sequence-number-opcode+ ms:typeSeqNum)
;;          (list +ml-file-text-event-opcode+ 
;;                (list +ml-file-text-event-opcode+ ms:typeTextual)
;;                (list +ml-file-copyright-note-opcode+ ms:typeCopyright)
;;                (list +ml-file-sequence/track-name-opcode+ ms:typeSeqName)
;;                (list +ml-file-instrument-name-opcode+ ms:typeInstrName)
;;                (list +ml-file-lyric-opcode+ ms:typeLyric)
;;                (list +ml-file-marker-opcode+ ms:typeMarker)
;;                (list +ml-file-cue-point-opcode+ ms:typeCuePoint))
;;          (list +ml-file-midi-channel-opcode+ ms:typeChanPrefix)
;;          (list +ml-file-midi-port-opcode+ ms:typePortPrefix)
;;          (list +ml-file-eot-opcode+ ms:typeEndTrack)
;;          (list +ml-file-tempo-change-opcode+ ms:typeTempo)
;;          (list +ml-file-smpte-offset-opcode+ ms:typeSMPTEOffset)
;;          (list +ml-file-time-signature-opcode+ ms:typeTimeSign)
;;          (list +ml-file-key-signature-opcode+ ms:typeKeySign)
;;          (list +ml-file-sequencer-event-opcode+ ) #f)
   ))

(define (midi-op->evtype op set)
  (let ((e (assoc op (list-ref opcodes-evtypes set))))
    (if e
      (or (cadr e) 
          (err "No MidiShare evType for event opcode ~s."
               op))
      (err "No MidiShare evType for event opcode ~s."
           op))))
         
(define-method* (write-event (obj <midi-channel-event>)
                             (stream <midishare-stream>) 
                             scoretime)
  (let* ((opr (slot-ref obj 'opcode))
         (typ (midi-op->evType opr 0))
         (loc (logical-channel (midi-event-channel obj)
                               (midi-stream-channel-map stream)))
         (dat (midi-event-data2 obj))
	 (sched (scheduling-mode))
         (evt (ms:MidiNewEv typ)))
    (ms:port evt (car loc))
    (ms:chan evt (cadr loc))
    ;;(ms:date evt beg)
    (ms:field evt 0 (midi-event-data1 obj))
    (if (= typ ms:typePitchBend)
        (ms:bend evt (midi-pitch-bend-bend obj))
        (if dat (ms:field evt 1 dat)))
    (ms:MidiSendAt (midishare-stream-refnum stream)
		   evt
		   (cond ((eq? sched ':events)
			  ;; events: add stream offset (includes latency)
			  (+ (object-time stream) 
			     (inexact->exact (floor (* scoretime 1000)))))
			 ((eq? sched ':rts)
			  ;; rts: add latency to ms:now.
			  (+ (midishare-stream-latency stream)
			     (ms:MidiGetTime)))
			 ((not sched)
			  ;; repl: 0=ms:now, <int>=msec <float>=ahead in sec
			  (let ((tim (object-time obj)))
			    (if (= tim 0) (ms:MidiGetTime)
				(if (integer? tim) tim
				    (+ (inexact->exact (floor (* tim 1000)))
				       (ms:MidiGetTime))))))))))

;; REMOVED, use ms:output instead.
;;(define-method* (write-event (obj <top>)
;;                             (stream <midishare-stream>) 
;;                             scoretime)
;;  ;; obj had  better be a midiEv !!
;;  (cond ((eq? *scheduler* ':asap)
;;         (ms:MidiSendAt (midishare-stream-refnum stream) obj
;;                        (+ (object-time stream) 
;;                           (inexact->exact (floor (* scoretime 1000))))))
;;        ((not *scheduler*)
;;         (ms:MidiSendAt (midishare-stream-refnum stream) obj
;;                        (+ (ms:now) scoretime)))
;;        (else
;;         ;; rts running
;;         (ms:MidiSendIm (midishare-stream-refnum stream) obj)
;;         ))
;;  (values))

;;;
;;; reading and writing MidiEvs.  the proctable stuff is take from
;;; Example 21 of Grame's Midishare-Tutorial.lisp
;;;

(define (make-proctable num)
  (reset-proctable (make-vector num)))

(define (reset-proctable tbl)
  ;; set each pos in tabl to index of next (free) pos
  ;; last pos points to nil
  ;; (elt 0) always holds next available
  (do ((num (vector-length tbl))
       (pos 0 (+ pos 1)))
      ((= pos num)
       (vector-set! tbl (- num 1) #f)
       tbl)
    (vector-set! tbl pos (+ pos 1))))

(define *proctable* (make-proctable 64))

(define (add-proc proc)
  ;; install proc at current free loc, first storing the next
  ;; free loc at pos 0. return nil if no more room.
  (let ((free (vector-ref *proctable* 0)))
    (if free
      (begin
        (vector-set! *proctable* 0 (vector-ref *proctable* free))
        (vector-set! *proctable* free proc)
        free)
      #f)))

(define (rem-proc index)
  (vector-set! *proctable* index 
               (vector-ref *proctable* 0))
  (vector-set! *proctable* 0 index)
  (values))

;; (defcallback run-proc ...) in openmcl.lisp

(define (ms:sprout obj ahead out)
  ;; add process obj to MidiShare's queue
  ;; RUN-PROC is a callback defined in openmcl.lis
  obj ahead out
;;  (let ((id (add-proc obj )))
;;    (if id
;;        (ms:MidiTask run-proc (+ (ms:MidiGetTime)
;;                                 (or ahead 0) )
;;                     (midishare-stream-refnum out) id 0 0)
;;        (warn "Can't sprout, no room left in process table!")))
  (err "ms:sprout unsupported (see openmcl.lisp for more info)")
  (values))

(define (parse-ms-output forms clauses ops)
  clauses ops  ; gag 'unused var' message from cltl compilers
  (let ((head forms)
        (oper (pop forms))
        (expr #f)
        (to #f)
        (args (list))
        (loop '()))
    (when (null? forms)
      (loop-error ops head "Missing '" oper "' expression."))
    (set! expr (pop forms))
    (do ((stop #f))
        ((or stop (null? forms)))
      (case (car forms)
        (( to :to )
         (when (null? (cdr forms))
           (loop-error ops head "Missing '" oper " to' expression."))
         (set! to #t)
         (set! args (append! args (list :to (cadr forms))))
         (set! forms (cddr forms)))
         (( at :at )
          (when (null? (cdr forms))
            (loop-error ops head "Missing '" oper " ahead' expression."))
          (set! args (append! args (list :at (cadr forms))))
          (set! forms (cddr forms)))
        (else
         (set! stop #t))))
    (unless to (set! args (append! args (list ':to '*out*))))
    (set! loop (list `(,oper ,expr ,@args)))
    (values (make-loop-clause 'operator oper 'looping loop)
            forms)))

(define *process-operators*
  (append *process-operators*
          (list (list 'ms:output (function parse-ms-output)
                      'task 'to))))

(define (ms:output ev &key (to *out*) at)
  ;; output ev to Midishare
  (cond ((scheduling-mode? ':events)
	 (ms:MidiSendAt (midishare-stream-refnum to)
			ev
			(+ (object-time to) 
			   (ms:date ev)
			   (inexact->exact
			    (floor (* *pstart* 1000))))))
	(else
	 (when (= (ms:date ev) 0)
	   (ms:date ev (or at (ms:MidiGetTime))))
	 (ms:MidiSend (midishare-stream-refnum to) ev))))

(define (ms:now)
  (ms:MidiGetTime))

;;;
;;; real time input
;;;

;;(define *receive-hook* #f)

;; (defcallback midi-receive-hook (:unsigned-halfword refnum)
;;   (do ((go #t)
;;        (ev (ms:MidiGetEv refnum) (ms:MidiGetEv refnum)))
;;       ((or (not go) (ms:nullptrp ev))
;;        (values))
;;     (if *receive-hook*
;;       (funcall *receive-hook* ev)
;;       (set! go #f))))

;;(define (ms:receive? )
;;  (and *receive-hook* #t))

;;(define (ms:receive . args)
;;  ;; MIDI-RECEIVE-HOOK is a callback defined in openmcl.lisp
;;  (with-args (args &optional hook)
;;    (if (midishare-open?)
;;      (if hook
;;        (if *receive-hook*
;;          (format #t "Already receiving MIDI, type (ms:receive) to stop.~%")
;;          (begin (set! *receive-hook* hook)
;;                 (ms:MidiSetRcvAlarm *ms* midi-receive-hook)))
;;        (if (not *receive-hook*)
;;          (format #t "Not currently receiving MIDI.~%")
;;          (begin (set! *receive-hook* #f)
;;                 (ms:MidiSetRcvAlarm *ms* (ms:nullptr)))))
;;      (format #t "MIDI not open!"))
;;    (values)))

;;;
;;; message receiving
;;;

;(push (list :midishare-callback midishare-start-recevive 
;            midishare-stop-receve))

(define-method* (stream-receive-init (str <midishare-stream>) hook args)
  ;; hook is 2 arg lambda or nil, type is :threaded or :periodic
  args
  (let ((data (rt-stream-receive-data str)) ; (<thread> <stop> <buf> <len>)
        (mode (rt-stream-receive-mode str)))
    (cond ((not (procedure? hook))
           (err "Receive: hook is not a function: ~s" hook))
          ((not (member mode '( :raw )))
           (err "receive: ~s is not a midishare receive mode." mode))
          ((not (member (midishare-open? str) '(:in :inout)))
           (err "Stream not open for input: ~S." str))
          ((first data)
           (err "Can't set input hook: another hook is running!")))
    ;; ready to go
    (let ((refn (first (io-open str))))
      (when (> (ms:MidiCountEvs refn) 0) 
        (ms:Midiflushevs refn))
      (list-set! data 0 hook)
      (list-set! data 1 refn))))

(define-method* (stream-receive-deinit (str <midishare-stream>))
  ;; called by remove-receiver after the periodic task has been withdrawn
  (let ((data (rt-stream-receive-data str))) ; (<thread> <stop> )
    (when (first data)
      (list-set! data 0 #f)
      (list-set! data 1 #f))))




      
