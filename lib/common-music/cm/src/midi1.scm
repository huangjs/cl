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
;;; $Revision: 1.28 $
;;; $Date: 2007/07/01 00:06:47 $

;;;
;;; MIDI MESSAGE TYPES
;;; 
;;; MIDI messages types are determined by their status byte. A status byte
;;; in a stream of messages is defined as a byte whose MSB is set.  
;;;
;;; NOTE: The following bitfields comply with the MIDI standard.
;;;       The actual constants, however, have their nibbles swapped.)
;;;
;;; NOTE: Some message names differ from the official specification as
;;;       published by the MMA (cf. http://www.midi.org/table1.htm).  In
;;;       particular, the following message types should read (=>):
;;;
;;;         Key Pressure	=> Polyphonic Key Pressure
;;;         Pitch Bend		=> Pitch Wheel Change
;;;         Sysex		=> System Exclusive
;;;         Song Position	=> Song Position Pointer
;;;         Eox (unused)	=> End of Exclusive
;;;         System Reset	=> Reset
;;;
;;;
;;;                          status-byte     data1-byte   data2-byte
;;;    -------------------------------------------------------------------
;;;
;;;    [channel messages]
;;;      note-off            1000 ----       key           velocity
;;;      note-on             1001 ----       key           velocity
;;;      key-pressure        1010 ----       key           pressure
;;;      control-change      1011 ----       controller    value
;;;      program-change      1100 ----       program       -
;;;      channel-pressure    1101 ----       pressure      -
;;;      pitch-bend          1110 ----       lsb           msb


;;; 
;;; Byte specifications and offsets (Encoded MIDI Messages) WARNING:
;;; if these bytespecs are changed the edit make-channel-message --
;;; its hand coded without bytespecs to avoid consing.

(define +enc-route-byte+		(byte 10 22))
(define +enc-lower-status-byte+	(byte  4 18))
(define +enc-upper-status-byte+	(byte  4 14))
(define +enc-swapped-status-byte+ 	(byte  8 14))
(define +enc-logical-channel-byte+	(byte 14 18))
(define +enc-opcode-byte+		+enc-upper-status-byte+)
(define +enc-data-1-byte+		(byte  7  7))
(define +enc-data-2-byte+		(byte  7  0))

(define +enc-route-offs+		-22)
(define +enc-lower-status-offs+    -18)
(define +enc-upper-status-offs+    -10) ; keep byte-aligned
(define +enc-swapped-status-offs+ 	-14) ; not byte-aligned
(define +enc-logical-channel-offs+ +enc-lower-status-offs+)
(define +enc-opcode-offs+          +enc-upper-status-offs+)
(define +enc-data-1-offs+          -7)
(define +enc-data-2-offs+          0)

;;; AND with this mask to turn an encoded note on message into a note off
;;; message with zero velocity. 

(define +enc-note-off-mask+         #xfffe3f80)
(define +ml-note-off-opcode+                       #b1000)
(define +ml-note-on-opcode+                        #b1001)
(define +ml-key-pressure-opcode+                   #b1010)
(define +ml-control-change-opcode+                 #b1011)
(define +ml-program-change-opcode+                 #b1100)
(define +ml-channel-pressure-opcode+               #b1101)
(define +ml-pitch-bend-opcode+                     #b1110)

(define +ml-default-note-on-velocity+  64)
(define +ml-default-note-off-velocity+ 64)

;;;    [channel mode messages]
;;;      (these are actually just control changes with special mode
;;;      selecting controller numbers)

;;;
;;;    [system common messages]
;;;      sysex               1111 0000       <variable length>
;;;      mtc-quarter-frame   1111 0001       SMPTE bits    -    (*)
;;;      song-position       1111 0010       lsb           msb  (**)
;;;      song-select         1111 0011       song number   -
;;;      <undefined>         1111 0100       -             -
;;;      cable-select        1111 0101       -             -    (***)
;;;      tune-request        1111 0110       -             -
;;;      eox                 1111 0111       -             -

(define +ml-msg-sysex-type+                        #b00001111)
(define +ml-msg-mtc-quarter-frame-type+            #b00011111)
(define +ml-msg-song-position-type+                #b00101111)
(define +ml-msg-song-select-type+                  #b00111111)
(define +ml-msg-cable-select-type+                 #b01011111)
(define +ml-msg-tune-request-type+                 #b01101111)
(define +ml-msg-eox-type+                          #b01111111)

;;; (*):   transmits the full SMPTE code in 8 messages (= 2 frames)
;;; (**):  this is a 14-bit counter where 1 count = 6 timing clocks.
;;; (***): though officially undefined, some MIDI interfaces use this
;;;        message to control cable access; a single data byte that follows
;;;        designates the cable number on which subsequent MIDI messages are
;;;        routed. 
;;; 
;;;    [system realtime messages]
;;;      timing-clock        1111 1000       (24 ticks per quarter)
;;;      timing-tick         1111 1001       (1 tick = 10 milliseconds)
;;;      start               1111 1010 
;;;      continue            1111 1011                 
;;;      stop                1111 1100              
;;;      <undefined>         1111 1101                  
;;;      active sensing      1111 1110       (sent every 300ms or more often)
;;;      system reset        1111 1111        

(define +ml-msg-timing-clock-type+                 #b10001111)
(define +ml-msg-timing-tick-type+                  #b10011111)
(define +ml-msg-start-type+                        #b10101111)
(define +ml-msg-continue-type+                     #b10111111)
(define +ml-msg-stop-type+                         #b11001111)
(define +ml-msg-active-sensing-type+               #b11101111)
(define +ml-msg-system-reset-type+                 #b11111111)

;;;
;;; NOTE: Meta messages are of variable length and have the syntax 
;;;
;;;          #xFF <type> <length> {<data>}*
;;;       
;;;       However, since #xFF collides with the system reset realtime
;;;       message, we substitute a #x00 status byte to flag a meta message
;;;       and treat them like sysex otherwise (i.e., attach the FULL meta
;;;       message as a string, including the initial #xff byte.
;;; 
;;;       MIDI file types are NOT swapped, since they don't travel through
;;;       the C interface.
;;;       
;;;   [meta messages]
;;;     eot                  0000 0000       0010 1111     <length>    ...
;;;     tempo-change         0000 0000       0101 0001     <length>    ...
;;;     time-signature       0000 0000       0101 1000     <length>    ...

(define +ml-meta-type+                             #b00000000)
(define +ml-file-meta-marker+                      #xff)

(define +ml-file-sequence-number-opcode+           #x00)

(define +ml-file-text-event-opcode+                #x01)
(define +ml-file-copyright-note-opcode+            #x02)
(define +ml-file-sequence/track-name-opcode+       #x03)
(define +ml-file-instrument-name-opcode+           #x04)
(define +ml-file-lyric-opcode+                     #x05)
(define +ml-file-marker-opcode+                    #x06)
(define +ml-file-cue-point-opcode+                 #x07)

(define +ml-file-midi-channel-opcode+              #x20)
(define +ml-file-midi-port-opcode+                 #x21)
(define +ml-file-eot-opcode+                       #x2f)

(define +ml-file-tempo-change-opcode+              #x51)
(define +ml-file-smpte-offset-opcode+              #x54)
(define +ml-file-time-signature-opcode+            #x58)
(define +ml-file-key-signature-opcode+             #x59)
(define +ml-file-sequencer-event-opcode+           #x7f)

;;;
;;; basic accessors and type predicates
;;;

(define (midimsg-data1 message)
  (ldb +enc-data-1-byte+ message))

(define (midimsg-data2 message)
  (ldb +enc-data-2-byte+ message))

(define (midi-channel-message-p message)
  (< 0 (ldb +enc-opcode-byte+ message) #xf))

(define (midi-system-message-p message)
  (= (ldb +enc-upper-status-byte+ message) #xf))

(define (midi-meta-message-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-meta-type+))

;;;
;;; Provide a quick way of looking up the number of bytes (including the
;;; status byte) of a message.
;;;
;;;          -1 = <undefined>
;;;           0 = variable length
;;;   1,2, or 3 = length

(define +channel-message-sizes+
  #(3                                   ; note-off
    3                                   ; note-on
    3                                   ; key-pressure
    3                                   ; control-change
    2                                   ; program-change
    2                                   ; channel-pressure
    3                                   ; pitch-bend
    ))

(define +system-message-sizes+ 
  #(0                                   ; sysex
    2                                   ; mtc-quarter-frame
    3                                   ; song-position
    2                                   ; song-select
    -1                                  ; <undefined>
    2                                   ; cable-select
    1                                   ; tune-request
    1                                   ; eox
    1                                   ; timing-clock
    1                                   ; timing-tick
    1                                   ; start
    1                                   ; continue
    1                                   ; stop
    -1                                  ; <undefined>
    1                                   ; active-sensing
    1                                   ; system-reset
    ))

(define +ml-channel-msg-type-strings+
  #("Note-Off" "Note-On" "Key-Pressure" "Control-Change" "Program-Change"
    "Channel-Pressure" "Pitch-Bend"))

(define +ml-msg-type-strings+
  #("Sysex" "MTC Quarter Frame" "Song-Position" "Song-Select"  "Undefined"
    "Cable-Select" "Tune-Request" "Eox" "Timing-Clock" "Timing-Tick"
    "Start" "Continue" "Stop" "Undefined" "Active-Sensing" "System-Reset"))

(define +ml-meta-msg-type-strings+
  #((0 . "Sequence Number") (1 . "Text Event") (2 . "Copyright Note")
    (3 . "Sequence/Track Name") (4 . "Instrument Name") (5 . "Lyric")
    (6 . "Marker") (7 . "Cue Point") (#x20 . "MIDI Channel")
    (#x21 . "MIDI Port") (#x2f . "End of Track") (#x51 . "Tempo Change")
    (#x54 . "SMPTE Offset") (#x58 . "Time Signature")
    (#x59 . "Key Signature") (#x7f . "Sequencer Event")))

(define (get-meta-msg-type-string type)
  ;;(find type +ml-meta-msg-type-strings+ :test (function =) :key (function car))
  (let ((res (do ((i 0 (+ i 1))
                  (l (vector-length +ml-meta-msg-type-strings+))
                  (x #f)
                  (f #f))
                 ((or f (= i l)) f)
               (set! x (vector-ref +ml-meta-msg-type-strings+ i))
               (if (= (car x) type) (set! f x)))))
    (if res (cdr res) "Unknown Meta Event")))

;;;
;;; low-level utilities used by mf.lisp and midi.lisp
;;;

(define (midimsg-logical-channel m)
  (ldb +enc-logical-channel-byte+ m))

(define (midimsg-route m)
  (ldb +enc-route-byte+ m))

(define (midimsg-opcode m)
  (ldb +enc-opcode-byte+ m))

(define (midimsg-upper-status m)    ; private
  (ldb +enc-upper-status-byte+ m))

(define (midimsg-lower-status m)    ; private
  (ldb +enc-lower-status-byte+ m))

(define (midimsg-status m)
  (ldb +enc-swapped-status-byte+ m))

(define (midimsg-size m)            ; private
  (if (midi-channel-message-p m) 
    (vector-ref +channel-message-sizes+
                (logand (ash m +enc-swapped-status-offs+) #b111))
    (vector-ref +system-message-sizes+
                (logand (ash m +enc-lower-status-offs+) #b1111))))

(define (channel-note-hash m) 
   ;; hash value chan&key 
   (logior (ash (ldb +enc-logical-channel-byte+ m) 8)
           (midimsg-data1 m)))

(define (%midi-encode-channel-message bytes size)
  (if (= size 3)
    (make-channel-message (ash (logand bytes #xf00000) -20)
                          (ash (logand bytes #x0f0000) -16)    
                          (ash (logand bytes #x007f00) -8)
                          (logand bytes #x7f))
    (if (= size 2)
      (make-channel-message (ash (logand bytes #xf000) -12)
                            (ash (logand bytes #x0f00) -8)       
                            (logand bytes #x7f)
			    0)
      (err "Size ~s cannot be a channel message." size))))

(define *midi-open* #f)
(define *midi-time* -1)

(define-macro (define-message-set! accessor bytespec)
  ;; defined in level1 because bqoute translation hopeless!
  (make-midi-message-set! accessor bytespec))

;;;
;;; midi-copy-message
;;;

(define (midi-copy-message msg . args)
  ;; return new msg with byte alteration
  (with-args (args &key opcode channel data1 data2)
    (when opcode
      (set! msg (dpb opcode +enc-opcode-byte+ msg)))
    (when channel
      (set! msg (dpb channel +enc-logical-channel-byte+ msg)))
    (when data1
      (set! msg (dpb data1 +enc-data-1-byte+ msg)))
    (when data2
      (set! msg (dpb data2 +enc-data-2-byte+ msg)))
    msg))

;;; ==========================================================================
;;;
;;; Channel (and Channel Mode) Messages
;;;

(define (make-channel-message opcode channel data1 data2)
  ;; rewrote because DPB conses in most lisps!
;;  (dpb channel +enc-logical-channel-byte+
;;       (dpb opcode +enc-opcode-byte+
;;            (dpb data1 +enc-data-1-byte+
;;                 (dpb data2 +enc-data-2-byte+ 0))))
  (logior (ash (logand channel #b11111111111111) 18)
	  (ash (logand opcode #b1111) 14)
	  (ash (logand data1 #b1111111) 7) 
	  (logand data2 #b1111111)))

(define (channel-message-p message)
  (midi-channel-message-p message))

(define (channel-message-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (channel-message-opcode message)
  (ldb +enc-opcode-byte+ message))

(define (channel-message-data1 message)
  (ldb +enc-data-1-byte+ message))

(define (channel-message-data2 message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! channel-message-channel
    +enc-logical-channel-byte+)
(define-message-set! channel-message-opcode  +enc-opcode-byte+)
(define-message-set! channel-message-data1   +enc-data-1-byte+)
(define-message-set! channel-message-data2   +enc-data-2-byte+)


;;;
;;; :note-off

(define (make-note-off channel key velocity)
  (make-channel-message +ml-note-off-opcode+ channel key velocity))

(define (note-off-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-note-off-opcode+))

(define (note-off-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (note-off-key message)
  (ldb +enc-data-1-byte+ message))

(define (note-off-velocity message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! note-off-channel  +enc-logical-channel-byte+)
(define-message-set! note-off-key      +enc-data-1-byte+)
(define-message-set! note-off-velocity +enc-data-2-byte+)


;;;
;;; :note-on

(define (make-note-on channel key velocity)
  (make-channel-message +ml-note-on-opcode+ channel key velocity))

(define (note-on-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-note-on-opcode+))

(define (note-on-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (note-on-key message)
  (ldb +enc-data-1-byte+ message))

(define (note-on-velocity message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! note-on-channel  +enc-logical-channel-byte+)
(define-message-set! note-on-key      +enc-data-1-byte+)
(define-message-set! note-on-velocity +enc-data-2-byte+)


;;;
;;; :key-pressure

(define (make-key-pressure channel key pressure)
  (make-channel-message +ml-key-pressure-opcode+ channel key pressure))

(define (key-pressure-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-key-pressure-opcode+))

(define (key-pressure-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (key-pressure-key message)
  (ldb +enc-data-1-byte+ message))

(define (key-pressure-pressure message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! key-pressure-channel  +enc-logical-channel-byte+)
(define-message-set! key-pressure-key      +enc-data-1-byte+)
(define-message-set! key-pressure-pressure +enc-data-2-byte+)


;;;
;;; :control-change

(define (make-control-change channel controller value)
  (make-channel-message +ml-control-change-opcode+
                        channel controller value))

(define (control-change-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-control-change-opcode+))

(define (control-change-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (control-change-controller message)
  (ldb +enc-data-1-byte+ message))

(define (control-change-value message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! control-change-channel
    +enc-logical-channel-byte+)

(define-message-set! control-change-controller +enc-data-1-byte+)

(define-message-set! control-change-value  +enc-data-2-byte+)


;;;
;;; :program-change
;;;

(define (make-program-change channel program)
  (make-channel-message +ml-program-change-opcode+ channel program 0))

(define (program-change-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-program-change-opcode+))

(define (program-change-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (program-change-program message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! program-change-channel
    +enc-logical-channel-byte+)
(define-message-set! program-change-program +enc-data-1-byte+)


;;;
;;; :channel-pressure

(define (make-channel-pressure channel pressure)
  (make-channel-message +ml-channel-pressure-opcode+ channel pressure 0))

(define (channel-pressure-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-channel-pressure-opcode+))

(define (channel-pressure-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (channel-pressure-pressure message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! channel-pressure-channel
    +enc-logical-channel-byte+)

(define-message-set! channel-pressure-pressure +enc-data-1-byte+)


;;;
;;; :pitch-bend

(define (make-pitch-bend channel value . args)
  (let ((width (if (null? args) 2 (car args))))
    (let ((bend (inexact->exact
		 (floor
		  (rescale value (- width) width 0 16383)))))
      (make-channel-message +ml-pitch-bend-opcode+ channel
                            (ldb (byte 7 0) bend)
                            (ldb (byte 7 7) bend)))))

(define (pitch-bend-p message)
  (= (ldb +enc-opcode-byte+ message) +ml-pitch-bend-opcode+))

(define (pitch-bend-channel message)
  (ldb +enc-logical-channel-byte+ message))

(define (pitch-bend-lsb message)
  (ldb +enc-data-1-byte+ message))

(define (pitch-bend-msb message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! pitch-bend-channel +enc-logical-channel-byte+)
(define-message-set! pitch-bend-lsb     +enc-data-1-byte+)
(define-message-set! pitch-bend-msb     +enc-data-2-byte+)

;;; ======================================================================
;;;
;;; System (Common and Real-Time) Messages
;;;

(define (make-system-message type route . args)
  (with-args (args &optional (data1 0) (data2 0))
    (dpb route +enc-route-byte+
         (dpb type +enc-swapped-status-byte+
              (dpb data1 +enc-data-1-byte+
                   (dpb data2 +enc-data-2-byte+ 0))))))

(define (system-message-p message)
  (midi-system-message-p message))

(define (system-message-route message)
  (ldb +enc-route-byte+ message))

(define (system-message-status message)
  (ldb +enc-swapped-status-byte+ message))

(define (system-message-data1 message)
  (ldb +enc-data-1-byte+ message))

(define (system-message-data2 message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! system-message-route +enc-route-byte+)
(define-message-set! system-message-status +enc-swapped-status-byte+)
(define-message-set! system-message-data1  +enc-data-1-byte+)
(define-message-set! system-message-data2  +enc-data-2-byte+)

;;;
;;; :sysex
;;;
;;; if data is pair apply make-sysex-data, else it has to be an array.

(define (make-sysex-data . args)
  (let ((len 2)
        (i 0) ; gets pre-incremented during stuffing
        (msg #() ))
    (letrec ((incflen 
              (lambda (args)
                (dolist (a args)
                  (cond ((char? a)
                         (incf len))
                        ((and (integer? a)
                              (<= 0 i #b11111111))
                         (incf len))
                        ;; add 1 for trailing '\0'
                        ((string? a)
                         (incf len (+ (string-length a) 1)))
                        ((pair? a)
                         (incflen a))
                        (else
                         (err "~s not char, byte or string."
                              a))))))
             (stuff 
              (lambda (byte)
                (incf i)
                (vector-set! msg i byte)))
             (stuffdata 
              (lambda (args)
                (dolist (a args)
                  (cond ((char? a)
                         (stuff (char->integer a)))
                        ((string? a)
                         (loop for i below (string-length a)
                               for c = (string-ref a i)
                               do (stuff (char->integer c))
                               finally (stuff 0)))
                        ((and (integer? a)
                              (<= 0 i #b11111111))
                         (stuff a))
                        ((pair? a)
                         (stuffdata a))
                        (else
                         (err "~s not char, byte or string." a)))))))
            (incflen args)
            ;; allocate data
            (set! msg (make-vector len 0))
            ;; stuff data
            (stuffdata args)
            ;; add tags
            (vector-set! msg 0 #xF0)
            (vector-set! msg (- len 1) #xF7)
            msg)))

(define (make-sysex route data)
  (values (make-system-message +ml-msg-sysex-type+ route)
          (if (pair? data)
            (apply (function make-sysex-data) data)
            (if (vector? data) data
                (err "~s is not a pair or a vector." data)))))

(define (sysex-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-sysex-type+))

(define (sysex-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! sysex-route +enc-route-byte+)

;;;
;;; :mtc-quarter-frame

(define (make-mtc-quarter-frame route tag nibble)
  (make-system-message +ml-msg-mtc-quarter-frame-type+ route
		       (logior (ash tag 4) nibble)))

(define (mtc-quarter-frame-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-mtc-quarter-frame-type+))

(define (mtc-quarter-frame-route message)
  (ldb +enc-route-byte+ message))

(define (mtc-quarter-frame-tag message)
  (logand (ldb +enc-data-1-byte+ message) #b1110000))

(define (mtc-quarter-frame-nibble message)
  (logand (ldb +enc-data-1-byte+ message) #b1111))

(define-message-set! mtc-quarter-frame-route +enc-route-byte+)


;;;
;;; :song-position

(define (make-song-position route lsb msb)
  (make-system-message +ml-msg-song-position-type+ route lsb msb))

(define (song-position-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-song-position-type+))

(define (song-position-route message)
  (ldb +enc-route-byte+ message))

(define (song-position-lsb message)
  (ldb +enc-data-1-byte+ message))

(define (song-position-msb message)
  (ldb +enc-data-2-byte+ message))

(define-message-set! song-position-route +enc-route-byte+)
(define-message-set! song-position-lsb +enc-data-1-byte+)
(define-message-set! song-position-msb +enc-data-2-byte+)

;;;
;;; :song-select

(define (make-song-select route song)
  (make-system-message +ml-msg-song-select-type+ route song))

(define (song-select-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-song-select-type+))

(define (song-select-route message)
  (ldb +enc-route-byte+ message))

(define (song-select-song message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! song-select-route +enc-route-byte+)
(define-message-set! song-select-song +enc-data-1-byte+)


;;;
;;; :cable-select

(define (make-cable-select route cable)
  (make-system-message +ml-msg-cable-select-type+ route cable))

(define (cable-select-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-cable-select-type+))

(define (cable-select-route message)
  (ldb +enc-route-byte+ message))

(define (cable-select-cable message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! cable-select-route +enc-route-byte+)
(define-message-set! cable-select-cable +enc-data-1-byte+)


;;;
;;; :tune-request

(define (make-tune-request route)
  (make-system-message +ml-msg-tune-request-type+ route))

(define (tune-request-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-tune-request-type+))

(define (tune-request-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! tune-request-route +enc-route-byte+)


;;;
;;; :eox (unused, but who cares...)

(define (make-eox route)
  (make-system-message +ml-msg-eox-type+ route))

(define (eox-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-eox-type+))

(define (eox-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! eox-route +enc-route-byte+)


;;;
;;; :timing-clock

(define (make-timing-clock route)
  (make-system-message +ml-msg-timing-clock-type+ route))

(define (timing-clock-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-timing-clock-type+))

(define (timing-clock-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! timing-clock-route +enc-route-byte+)


;;;
;;; :timing-tick

(define (make-timing-tick route)
  (make-system-message +ml-msg-timing-tick-type+ route))

(define (timing-tick-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-timing-tick-type+))

(define (timing-tick-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! timing-tick-route +enc-route-byte+)


;;;
;;; :start

(define (make-start route)
  (make-system-message +ml-msg-start-type+ route))

(define (start-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-start-type+))

(define (start-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! start-route +enc-route-byte+)


;;;
;;; :continue

(define (make-continue route)
  (make-system-message +ml-msg-continue-type+ route))

(define (continue-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-continue-type+))

(define (continue-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! continue-route +enc-route-byte+)


;;;
;;; :stop

(define (make-stop route)
  (make-system-message +ml-msg-stop-type+ route))

(define (stop-p message)
  (= (ldb +enc-swapped-status-byte+ message) +ml-msg-stop-type+))

(define (stop-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! stop-route +enc-route-byte+)


;;;
;;; :active-sensing

(define (make-active-sensing route)
  (make-system-message +ml-msg-active-sensing-type+ route))

(define (active-sensing-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-active-sensing-type+))

(define (active-sensing-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! active-sensing-route +enc-route-byte+)


;;;
;;; :system-reset

(define (make-system-reset route)
  (make-system-message +ml-msg-system-reset-type+ route))

(define (system-reset-p message)
  (= (ldb +enc-swapped-status-byte+ message)
     +ml-msg-system-reset-type+))

(define (system-reset-route message)
  (ldb +enc-route-byte+ message))

(define-message-set! system-reset-route +enc-route-byte+)



;;; ==========================================================================
;;;
;;; MIDI File Meta Messages
;;;

;;;
;;; meta messages are of the form:  
;;;
;;;   +ml-meta-type+ <type>           are cm-encoded in a fixnum
;;;   <length> <data>                 array
;;;
;;; the <length> field is of variable-length and prepended to the message.
;;;
;;; Since meta messages are for MIDI files only, we omit the route field.

(define (make-meta-message type . data-bytes)
  (values (dpb +ml-meta-type+ +enc-swapped-status-byte+
               (dpb type +enc-data-1-byte+ 0))
          ;; 
          (let ((l (length data-bytes))
                (v #f) (d #f))
            (set! v (if (< l #x80) 1
                        (if (< l #x4000) 2
                            (if (< l #x200000) 3
                                (if (< l #x10000000) 4
                                    (err "Illegal length: ~s" l))))))
            (set! d (make-vector (+ v l) 0 ))
            (do ((i 0 (+ 1 i))
                 (offs (* (- v 1) 7) (- offs 7)))
                ((not (< i v)) #f)
              (vector-set! d i
                           (if (= offs 0)
                             (ldb (byte 7 offs) l)
                             (logior (ldb (byte 7 offs) l) #x80))))
            (do ((i v (+ i 1))
                 (b data-bytes (cdr b)))
                ((null? b) #f)
              (vector-set! d i (car b)))
            d)))

(define (meta-message-p message)
  (midi-meta-message-p message))

(define (meta-message-type message)
  (ldb +enc-data-1-byte+ message))

(define-message-set! meta-message-type +enc-data-1-byte+)


;;;
;;; :sequence-number

(define (make-sequence-number num)
  (when (>= num #x10000)
    (err "~s too big for a sequence number." num))
  (make-meta-message +ml-file-sequence-number-opcode+
                     (ldb (byte 8 8) num) (ldb (byte 8 0) num)))

(define (sequence-number-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-sequence-number-opcode+)))

;;(define (sequence-number-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! sequence-number-type +enc-data-1-byte+)


;;;
;;; :text-event
;;;
;;; The constructor (but NOT the predicate) is the workhorse for all
;;; other text events. 

(define (make-text-event string . args)
  (with-args (args &optional (type +ml-file-text-event-opcode+))
    (apply (function make-meta-message) type
           (loop for i below (string-length string)
                 collect (char->integer (string-ref string i))))))


(define (text-event-p message)
    (and (midi-meta-message-p message)
	 (= (ldb +enc-data-1-byte+ message)
	    +ml-file-text-event-opcode+)))

;;(define (text-event-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! text-event-type +enc-data-1-byte+)

;;;
;;; Utilities for all text meta events:

(define +text-meta-event-types+
  (list +ml-file-text-event-opcode+
        +ml-file-copyright-note-opcode+
        +ml-file-sequence/track-name-opcode+
        +ml-file-instrument-name-opcode+
        +ml-file-lyric-opcode+
        +ml-file-marker-opcode+
        +ml-file-cue-point-opcode+))

(define (text-meta-event-p message)
  (and (midi-meta-message-p message)
       ;;(find (ldb +enc-data-1-byte+ message) +text-meta-event-types+)
       (member (ldb +enc-data-1-byte+ message) +text-meta-event-types+)
       #t))

(define (text-meta-event-data-to-string data)
  (let ((len (vector-ref data 0)))
    (when (= (vector-ref data len) 0)
      (set! len (max (- len 1) 0)))
    (loop with str = (make-string len)
          for i below len
          do (string-set! str i (integer->char (vector-ref data (+ i 1))))
          finally (return str))))

;(text-meta-event-data-to-string #(3 102 111 111))       => "foo"
;(text-meta-event-data-to-string #(3 102 111 111 0))     => "foo"
;(text-meta-event-data-to-string #(4 102 111 111 0))     => "foo"
;(text-meta-event-data-to-string #(0))                   => ""

;;;
;;; :copyright-note

(define (make-copyright-note string)
  (make-text-event string +ml-file-copyright-note-opcode+))

(define (copyright-note-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-copyright-note-opcode+)))

;;(define (copyright-note-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! copyright-note-type +enc-data-1-byte+)


;;;
;;; :sequence/track-name

(define (make-sequence/track-name string)
  (make-text-event string +ml-file-sequence/track-name-opcode+))

(define (sequence/track-name-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-sequence/track-name-opcode+)))

;;(define (sequence/track-name-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! sequence/track-name-type +enc-data-1-byte+)


;;;
;;; :instrument-name

(define (make-instrument-name string)
  (make-text-event string +ml-file-instrument-name-opcode+))

(define (instrument-name-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-instrument-name-opcode+)))

;;(define (instrument-name-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! instrument-name-type +enc-data-1-byte+)


;;;
;;; :lyric

(define (make-lyric string)
  (make-text-event string +ml-file-lyric-opcode+))

(define (lyric-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-lyric-opcode+)))

;;(define (lyric-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! lyric-type +enc-data-1-byte+)


;;;
;;; :marker

(define (make-marker string)
  (make-text-event string +ml-file-marker-opcode+))

(define (marker-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-marker-opcode+)))

;;(define (marker-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! marker-type +enc-data-1-byte+)


;;;
;;; :cue-point

(define (make-cue-point string)
  (make-text-event string +ml-file-cue-point-opcode+))

(define (cue-point-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-cue-point-opcode+)))

;;(define (cue-point-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! cue-point-type +enc-data-1-byte+)


;;;
;;; :midi-channel

(define (make-midi-channel channel)
  (make-meta-message +ml-file-midi-channel-opcode+ channel))

(define (midi-channel-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-midi-channel-opcode+)))


;;;
;;; :midi-port

(define (make-midi-port port)
  (make-meta-message +ml-file-midi-port-opcode+ port))

(define (midi-port-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-midi-port-opcode+)))


;;;
;;; :eot

(define (make-eot )
  (make-meta-message +ml-file-eot-opcode+))

(define (eot-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-eot-opcode+)))

;;(define (eot-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! eot-type +enc-data-1-byte+)


;;;
;;; :tempo-change
;;;
;;; Tempo is in microseconds per MIDI quarter note

(define (make-tempo-change usecs-per-beat)
  (apply (function make-meta-message)
         +ml-file-tempo-change-opcode+
          (loop for pos from 16 by 8 downto 0
                collect (ldb (byte 8 pos) usecs-per-beat))))

(define (tempo-change-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-tempo-change-opcode+)))

;;(define (tempo-change-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! tempo-change-type +enc-data-1-byte+)


;;;
;;; :smpte-offset

(define (make-smpte-offset hours mins secs frames fractional-frames)
  (make-meta-message +ml-file-smpte-offset-opcode+
                     hours mins secs frames fractional-frames))

(define (smpte-offset-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-smpte-offset-opcode+)))

;;(define (smpte-offset-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! smpte-offset-type +enc-data-1-byte+)


;;;
;;; :time-signature

(define (make-time-signature numerator denominator . args)
  (with-args (args &optional (clocks 24) (32nds 8))
    (multiple-value-bind (f r) (clfloor (log2 denominator ))
      (unless (zero? r)
        (err "Time signature denominator ~s is not a power of 2." 
             denominator))
      (make-meta-message +ml-file-time-signature-opcode+
		         numerator f clocks 32nds))))

(define (time-signature-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-time-signature-opcode+)))

;;(define (time-signature-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! time-signature-type +enc-data-1-byte+)

;;;
;;; :key-signature

(define (make-key-signature key . args)
  (with-args (args &optional (mode ':major) )
    (let ((sf #f))
      (set! mode (case mode
                   ((:major major 0) 0)
                   ((:minor minor 1) 1)
                   (else (err "key signature mode not :major or :minor"))))
      (cond
        ((number? key)
         (unless (<= -7 key 7)
           (err "Key signature must be between -7 (b) and 7 (#)."))
         (set! sf key))
        (else
         (err "~s is not a number or symbol." key)))
      (set! sf (if (< sf 0) (+ sf 256) sf))
      (make-meta-message +ml-file-key-signature-opcode+ sf mode))))

(define (key-signature-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-key-signature-opcode+)))

;;(define (key-signature-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! key-signature-type +enc-data-1-byte+)


;;;
;;; :sequencer-event

(define (make-sequencer-event . data)
  (apply (function make-meta-message)
         +ml-file-sequencer-event-opcode+ data))

(define (sequencer-event-p message)
  (and (midi-meta-message-p message)
       (= (ldb +enc-data-1-byte+ message)
	  +ml-file-sequencer-event-opcode+)))

;;(define (sequencer-event-type message)
;;  (ldb +enc-data-1-byte+ message))

;;(define-message-set! sequencer-event-type +enc-data-1-byte+)


;;;
;;; make-midimsg provides a general interface to message construction
;;; commented out, not really neede
;
;(define (make-midimsg type . args)
;  (apply (case type
;	   ;; channel messages
;	   ((:channel-message channel-message) 
;            (function make-channel-message))
;	   ((:note-off note-off) 
;            (function make-note-off))
;	   ((:note-on note-on) 
;            (function make-note-on))
;	   ((:key-pressure key-pressure)
;            (function make-key-pressure))
;	   ((:control-change control-change) 
;            (function make-control-change))
;	   ((:program-change program-change) 
;            (function make-program-change))
;	   ((:channel-pressure channel-pressure)
;            (function make-channel-pressure))
;	   ((:pitch-bend pitch-bend)
;            (function make-pitch-bend))
;	   ;; system messages
;	   ((:system-message system-message) 
;            (function make-system-message))
;	   ((:sysex sysex)
;            (function make-sysex))
;	   ((:mtc-quarter-frame mtc-quarter-frame)
;            (function make-mtc-quarter-frame))
;	   ((:song-position song-position)
;            (function make-song-position))
;	   ((:song-select song-select) 
;            (function make-song-select))
;	   ((:cable-select cable-select)
;            (function make-cable-select))
;	   ((:tune-request tune-request) 
;            (function make-tune-request))
;	   ((:eox eox) 
;            (function make-eox))
;	   ((:timing-clock timing-clock) 
;            (function make-timing-clock))
;	   ((:timing-tick timing-tick)
;            (function make-timing-tick))
;	   ((:start start) 
;            (function make-start))
;	   ((:continue continue) 
;            (function make-continue))
;	   ((:stop stop) 
;            (function make-stop))
;	   ((:active-sensing active-sensing) 
;            (function make-active-sensing))
;	   ((:system-reset system-reset) 
;            (function make-system-reset))
;	   ;; meta messages
;	   ((:meta-message meta-message) 
;            (function make-meta-message))
;	   ((:sequence-number sequence-number) 
;            (function make-sequence-number))
;	   ((:text-event text-event) 
;            (function make-text-event))
;	   ((:copyright-note copyright-note) 
;            (function make-copyright-note))
;	   ((:sequence/track-name sequence/track-name)
;	    (function make-sequence/track-name))
;	   ((:instrument-name instrument-name) 
;            (function make-instrument-name))
;	   ((:lyric lyric) 
;            (function make-lyric))
;	   ((:marker marker) 
;            (function make-marker))
;	   ((:cue-point cue-point) 
;            (function make-cue-point))
;	   ((:midi-channel midi-channel) 
;            (function make-midi-channel))
;	   ((:midi-port midi-port) 
;            (function make-midi-port))
;	   ((:eot eot) 
;            (function make-eot))
;	   ((:tempo-change tempo-change) 
;            (function make-tempo-change))
;	   ((:smpte-offset smpte-offset) 
;            (function make-smpte-offset))
;	   ((:time-signature time-signature) 
;            (function make-time-signature))
;	   ((:key-signature key-signature) 
;            (function make-key-signature))
;	   ((:sequencer-event sequencer-event) 
;            (function make-sequencer-event)))
;	 args))
;
;(define-macro (midimsg-case message . body)
;  (let ((msgtst
;         (lambda (type msg)
;           (unless (string? type)
;             (set! type (symbol->string type)))
;           (cond ((string-ci=? type "NOTE-OFF")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-note-off-opcode+))
;                 ((string-ci=? type "NOTE-ON")
;                  `(= (ldb +enc-opcode-byte+ ,msg) 
;                      +ml-note-on-opcode+))
;                 ((string-ci=? type "KEY-DOWN")
;                  `(and (= (ldb +enc-opcode-byte+ ,msg)
;                           +ml-note-on-opcode+)
;                        (> (ldb +enc-data-2-byte+ ,msg) 0)))
;                 ((string-ci=? type "KEY-UP")
;                  `(or (= (ldb +enc-opcode-byte+ ,msg)
;                          +ml-note-off-opcode+)
;                       (and (= (ldb +enc-opcode-byte+ ,msg)
;                               +ml-note-on-opcode+)
;                            (= (ldb +enc-data-2-byte+ ,msg)
;                               0))))
;                 ((string-ci=? type "KEY-PRESSURE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-key-pressure-opcode+))
;                 ((string-ci=? type "PITCH-BEND")
;                  `(= (ldb +enc-opcode-byte+ ,msg) 
;                      +ml-pitch-bend-opcode+))
;                 ((string-ci=? type "CONTROL-CHANGE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-control-change-opcode+))
;                 ((string-ci=? type "PROGRAM-CHANGE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-program-change-opcode+))
;                 ((string-ci=? type "CHANNEL-PRESSURE")
;                  `(= (ldb +enc-opcode-byte+ ,msg)
;                      +ml-channel-pressure-opcode+))
;                 ((string-ci=? type "SYSEX")
;                  `(sysex-p ,msg))
;                 ((string-ci=? type "EOX")
;                  `(eox-p ,msg))
;                 ((or (string-ci=? type "#T")
;                      (string-ci=? type "ELSE"))
;                  #t)
;                 (else
;                  (err "~A is not a midi message type."
;                                type))))))
;    (let ((msg (gensym)))
;      `(let ((,msg ,message))
;         (cond
;          ,@ (map (lambda (form)
;                    `(,(if (pair? (car form))
;                         `(or ,@ (map (lambda (f) (msgtst f msg))
;                                      (car form)))
;                         (msgtst (car form) msg))
;                      ,@ (cdr form)))
;                  body))))))


;;;
;;;
;;;

(define (midi-print-channel-message stream gm? size type channel
				    data1 data2
                                    time-string)
  (let ((name (vector-ref +ml-channel-msg-type-strings+ 
                          (logand type #b111))))
    (format stream "~a~a" name time-string)
    (when gm?
      (let ((p (gm-percussion-channel-p channel)))
        (if p (set! channel "P"))
        (cond ((= type #b1100)  ; program change
               (set! data1 (gm-patch-name data1)))
              ((= type #b1011)  ; control change
               (set! data1 (midi-controller-name data1)))
              ((or (= type #b1010)  ; key pressure
                   (= type #b1000)  ; note on
                   (= type #b1001)) ; note on
               (if p
                 (set! data1 (gm-drum-kit-name data1)))))))
    (format stream " ~a" channel)
    (if (= size 3)
      (format stream " ~s ~s" data1 data2)
      (format stream " ~s" data1))))

;;;
;;; MIDI Message Printing
;;;

;;; Set this to T to enable verbose GM MIDI Message printing.

(define *midi-gm-mode* #t)

;;; Print <max> bytes of (<length> bytes of) data pointed to by <data>.
;;; Line has 16 message bytes, using 70 bytes of storage (including the 
;;; trailing newline.
;
; 1         2         3         4         5         6        
; 123456789012345678901234567890123456789012345678901234567890123456789
;------------------------------------------------------------------------
;system exclusive:   0  11 104        25407
; | 000000:  f000 0104 0001 0605 042f 666f 6f2e 6169   ........./foo.ai
; | 000010:  6666 0063 6861 6e67 6520 7072 6573 6574   ff.change preset
; |          [... (204 Bytes remaining)]
; | 000020:  00f7                                      ..              
;------------------------------------------------------------------------

(define (%print-sysex-aux stream data bytes rest indent)
  (let ((offs 0) 
        (toprint bytes)
        (n 0) 
        (oldn #f)
        (blank #f))
    (loop while (> toprint 0)
       do 
       ;; print lines
       (when indent (format stream indent))
       ;; offset 
       ;;(format stream "~6,'0d:  " offs)
       (format stream (format-integer offs 6 #\0))
       (format stream ":")
       ;; bytes
       (set! oldn n)                    ; cache n
       (do ((i 0 (+ i 1)))
           ((or (= i 16)
                (>= n bytes))
            (set! blank (- 16 i)))
         (format stream
                 (format-integer (vector-ref data n) 2 #\0))
         (if (odd? i) (format stream " "))
         (incf n))
       ;; padding 
       ;;(format stream (format nil "~~~d@t" 
       ;;                       (- (+ (* blank 3) 2)
       ;;                          (floor (/ blank 2)))))
       (dotimes (i (- (+ (* blank 3) 2)
                      (floor (/ blank 2))))
         (format stream " "))

       ;; chars
       (set! n oldn)                    ; restore n
       (do ((i 0 (+ i 1))
            (b #f))
           ((or (not (< i 16))
                (not (< n  bytes)))
            (decf toprint i))
          (set! b (vector-ref data n))
         (if (< 31 b 127)
             (format stream (make-string 1 (integer->char b)))
             (format stream "."))
         (incf n))
       (format stream "~%")
       (incf offs 16))
    
    ;; print remark, if necessary
    (when (> rest 0)
      (format stream "         [... (~s Bytes remaining)]~%" rest))
    (values)))

;(print-sysex-data #t #f 1 #(0 1 2 3 23 91 92 95 91 66) 10)
;(print-sysex-data #t #f 8 (make-vector 256 61) #f)

(define (print-sysex-data stream string lines data length)
  ;; accept only :stdout and :stderr, to be comptible with the C version
  (let ((bytes 0)
        (rest 0))
    (unless length
      (set! length (vector-length data)))
    (set! lines (if (number? lines) lines 0))
    (set! bytes (if (and (> lines 0)
                         (< (* lines 16) length))
                  (* (- lines 1) 16)  ; reserve last line for remark
                  length))
    (set! rest (- length bytes))
    ;; if we print to both a string and stream,
    ;; do string first, then print it
    (if string
      (err "string output not supported")
      (when stream
        (%print-sysex-aux stream data bytes rest #f)))))

(define (midi-print-message msg . args)
  (with-args (args time
                   &key data length
                   (stream #t)
                   (time-format #t)
                   (time-string "")
                   (gm *midi-gm-mode*)
                   (eol #t)
                   (delimit #t))
    (when (and time time-format)
      (format stream "~a " (format-integer time 8 #\space)))
    (when delimit (format stream "#<" ))
    (cond ((midi-channel-message-p msg)
           (let* ((size (midimsg-size msg))
                  (op (channel-message-opcode msg))
                  (chan (channel-message-channel msg))
                  (data1 (channel-message-data1 msg))
                  (data2 (channel-message-data2 msg)))
             
             (midi-print-channel-message stream gm size op chan 
                                         data1 data2 time-string)))
          ((midi-system-message-p msg)
           (let ((name (vector-ref +ml-msg-type-strings+
                                   (ldb +enc-lower-status-byte+ msg)))
                 (route (ldb +enc-route-byte+ msg))
                 (size (midimsg-size msg)))
             (format stream "~a~a ~s" name time-string route)
             (cond ((= size 3)
                    (format stream " ~s ~s"
                            (midimsg-data1 msg)
                            (midimsg-data2 msg)))
                   ((= size 2)
                    (format stream " ~s" (midimsg-data1 msg)))
                   (else (when delimit 
                           (format stream ">")
                           (set! delimit #f))
                         (when (sysex-p msg)
                           (format stream "~%")
                           (print-sysex-data #t #f 0 data length))))))
          ((midi-meta-message-p msg)
           (cond ((or (midi-channel-p msg)
                      (midi-port-p msg))
                  (format stream "~a~a ~s"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string
                          (if data (vector-ref data 1) "?")))
                 ((tempo-change-p msg)
                  (format stream "~a~a ~s ms"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string
                          (/ (+ (ash (vector-ref data 1) 16)
                                (ash (vector-ref data 2)  8)
                                (vector-ref data 3))
                             1000)))
                 ((time-signature-p msg)
                  (format stream "~a~a ~s/~s (~s clocks, ~s 32nds)"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string
                          (vector-ref data 1) 
                          (expt 2 (vector-ref data 2))
                          (vector-ref data 3)
                          (vector-ref data 4)))
                 ((key-signature-p msg)
                  (let ((a (vector-ref data 1))
                        (s #f))
                    (set! a (or (and (logbit? a #x80) 
                                     (- a #x100)) a))
                    
                    (set! s (if (> (abs a) 1) "s" ""))
                    (format stream "~a~a ~a ~a~a, ~a"
                            (get-meta-msg-type-string
                             (ldb +enc-data-1-byte+ msg))
                            time-string
                            (if (zero? a) "no" (abs a))
                            (case (signum a)
                              ((-1) "flat")
                              (( 0) "accidentals")
                              (( 1) "sharp"))
                            s 
                            (if (zero? (vector-ref data 2)) 
                              "major" "minor"))))
                 (else
                  (format stream "~a~a"
                          (get-meta-msg-type-string
                           (ldb +enc-data-1-byte+ msg))
                          time-string)
                  (if data
                    (format stream " ~s"
                            (if (text-meta-event-p msg)
                              (text-meta-event-data-to-string data)
                              data))
                    (format stream ">")))))
          (else
           (format stream "Bogus Midi Message~a]" time-string)))
    (when delimit (format stream ">"))
    (when eol (newline stream))
    msg))

;;;
;;; MIDI Constants and Utilities
;;;

;;;
;;; deflabel defines a constant and inserts its namestring in a vector at
;;; the given offset.

(define %deflabelvar% (gensym))

(define-macro (deflabel sym val vector str pos)
  `(begin
    (define ,sym ,val)
    ,(if str
	 `(vector-set! ,vector ,pos ,str)
	 `(let ((*print-case* ':downcase)) ; looks gross in scheme code...
	   (set! %deflabelvar% (format #f "~a" ',sym))
	   (do ((i 0 (+ i 1))
		(e (string-length %deflabelvar%)))
	       ((not (< i e)) #f)
	     (if (char=? (string-ref %deflabelvar% i) #\space)
	       (string-set! %deflabelvar% i #\-)))
	   (vector-set! ,vector ,pos
                        (substring %deflabelvar%
                                   1 (- (string-length %deflabelvar%)
                                        1)))))))


;;; ======================================================================
;;;
;;; Controller Numbers

(define +midi-controller-strings+
  (make-vector 128 ""))

(define-macro (defcontroller sym val . str)
  `(deflabel ,sym ,val +midi-controller-strings+
    ,(if (null? str) #f (car str))
    ,val))

;;;
;;; Utilities

(define (midi-opcode-name vec code lb ub delta)
  (cond ((and (exact? code)
	      (<= lb code ub))
	 (vector-ref vec (+ code delta)))
	((string? code) code)
	(else
	 (err "MIDI opcode ~s not string or int ~s-~s."
              code lb ub))))

(define (midi-controller-name c)
  (midi-opcode-name +midi-controller-strings+ c 0 127 0))


; (midi-controller-name 71)
; (midi-controller-name "unset")

;;;
;;; There are 128 possible controller numbers (ie, 0 to 127).  Some numbers
;;; are defined for specific purposes. Others are undefined, and reserved
;;; for future use. 
;;; 
;;; Naming according to the MIDI Manufacturers Association (cf. 
;;; http://www.midi.org/table3.html) with the exception 
;;; of "Fine" controllers (32-51, 98 and 100).

;;;
;;; Controllers 0-31 should be used for coarse adjustments

;;; Specific Continuous Controllers 0-16,383 (Coarse, MSB of 14 bits)

(defcontroller +bank-select+		  0) ; no-op unless followed by a
					     ; program change
(defcontroller +modulation-wheel+	  1) ; 0 is no modulation effect
(defcontroller +breath-control+		  2) ; 0 is minimum breath pressure
(defcontroller +foot-controller+	  4) ; 0 is minimum effect
(defcontroller +portamento-time+	  5) ; 0 is slowest rate
(defcontroller +data-entry+		  6) ; value of a previously set
					     ; registered or non-registered
					     ; parameter.  0 is minimum effect
(defcontroller +channel-volume+		  7) ; formerly "main volume"
(define +volume+			  7) ; (alternative name)
(defcontroller +balance+		  8) ; balance before pan. 0 is left
(defcontroller +pan+			 10) ; 0 is left
(defcontroller +expression-controller+	 11) ; percentage of volume. 0 is off
(defcontroller +effect-control-1+	 12) ; 0 is minimum effect
(defcontroller +effect-control-2+	 13) ; 0 is minimum effect
(defcontroller +general-purpose-controller-1+	 16
  "General-Purpose Controller 1")
(defcontroller +general-purpose-controller-2+	 17
  "General-Purpose Controller 2")
(defcontroller +general-purpose-controller-3+	 18
  "General-Purpose Controller 3")
(defcontroller +general-purpose-controller-4+	 19
  "General-Purpose Controller 4")

;;;
;;; Controllers 32-63 should be used for fine adjustments

;;; Specific Continuous Controllers 0-16,383 (Fine, LSB of 14 bits)
;;;
;;; Some devices do not implement these fine adjust counterparts to the
;;; coarse specific continuous controllers above.  However, devices that
;;; implement 14-bit resolution are required to deal with either the coarse
;;; or fine controller message being sent without its counterpart following.

(defcontroller +bank-select-fine+		 32 "Bank Select (Fine)")
(defcontroller +modulation-wheel-fine+		 33 "Modulation Wheel (Fine)")
(defcontroller +breath-control-fine+		 34 "Breath Control (Fine)")
(defcontroller +foot-controller-fine+		 36 "Foot Controller (Fine)")
(defcontroller +portamento-time-fine+		 37 "Portamento Time (Fine)")
(defcontroller +data-entry-fine+		 38 "Data Entry (Fine)")
(defcontroller +channel-volume-fine+		 39 "Channel Volume (Fine)")
(define +volume-fine+			 39) ; (alternative name)
(defcontroller +balance-fine+			 40 "Balance (Fine)")
(defcontroller +pan-fine+			 42 "Pan (Fine)")
(defcontroller +expression-controller-fine+	 43
  "Expression Controller (Fine)")
(defcontroller +effect-control-1-fine+		 44 "Effect Control 1 (Fine)")
(defcontroller +effect-control-2-fine+		 45 "Effect Control 2 (Fine)")
(defcontroller +general-purpose-controller-1-fine+ 48 
  "General-Purpose Controller 1 (Fine)")
(defcontroller +general-purpose-controller-2-fine+ 49 
  "General-Purpose Controller 1 (Fine)")
(defcontroller +general-purpose-controller-3-fine+ 50 
  "General-Purpose Controller 1 (Fine)")
(defcontroller +general-purpose-controller-4-fine+ 51 
  "General-Purpose Controller 1 (Fine)")

;;; Common Switches 0(-63): on/(64-)127: off

(defcontroller +hold-1+			 64) ; when on, also postpones any 
					     ; All-Notes-Off controller
					     ; message on the same channel
(define +sustain+			 64) ; (alternative name)
(define +damper-pedal+		 64) ; (alternative name)
(defcontroller +portamento+		 65)
(defcontroller +sostenuto+		 66) ; only sustain sounding notes
					     ; when on, also postpones any 
					     ; All-Notes-Off controller
					     ; message on the same channel
					     ; for the notes held 
(defcontroller +soft-pedal+		 67) 
(defcontroller +legato-footswitch+	 68)
(defcontroller +hold-2+			 69) ; lengthen release times

;;; Sound Controllers 0-127.  0 is always minimum setting

(defcontroller +sound-control-1+	 70) ; anything, really
(define +sound-variation+		 70) ; (alternative name)
(defcontroller +sound-control-2+	 71) ; VCF envelope brightness control
(define +sound-timbre+		 71) ; (alternative name)
(defcontroller +sound-control-3+	 72)
(define +sound-release-time+	 72) ; (alternative name)
(defcontroller +sound-control-4+	 73)
(define +sound-attack-time+	 73) ; (alternative name)
(defcontroller +sound-control-5+	 74) ; VCF cutoff brightness control
(define +sound-brightness+		 74) ; (alternative name)
(defcontroller +sound-control-6+	 75)
(defcontroller +sound-control-7+	 76)
(defcontroller +sound-control-8+	 77)
(defcontroller +sound-control-9+	 78)
(defcontroller +sound-control-10+	 79)

;;; Additional (Coarse) Controllers 0-127
(defcontroller +general-purpose-controller-5+	 80
  "General-Purpose Controller 5")
(defcontroller +general-purpose-controller-6+	 81
  "General-Purpose Controller 6")
(defcontroller +general-purpose-controller-7+	 82
  "General-Purpose Controller 7")
(defcontroller +general-purpose-controller-8+	 83
  "General-Purpose Controller 8")

(defcontroller +portamento-control+	 84) ; uses source note

;;; Level Controllers 0-127
(defcontroller +effects-1-depth+	 91)
(define +effects-level+		 91) ; (alternative name)
(defcontroller +effects-2-depth+	 92)
(define +tremolo-level+		 92) ; (alternative name)
(defcontroller +effects-3-depth+	 93)
(define +chorus-level+		 93) ; (alternative name)
(defcontroller +effects-4-depth+	 94)
(define +detune-level+		 94) ; (alternative name)
(defcontroller +effects-5-depth+	 95)
(define +phasor-level+		 95) ; (alternative name)

;;; Data Entry Step Controllers (without Value Byte)
(defcontroller +data-entry-+1+		 96) ; increment a previously set
					     ; registered or non-registered
					     ; parameter
(define +data-entry-increment+	 96) ; (alternative name)
(defcontroller +data-entry--1+		 97 "Data Entry -1") 
					     ; decrement a previously set
					     ; registered or non-registered
					     ; parameter
(define +data-entry-decrement+	 97) ; (alternative name)

;;; Parameter Number Selection 0-16,383 (LSB or MSB of 14 bits)
(defcontroller +non-registered-parameter-number-fine+	 98
  "Non-Registered Parameter Number (Fine)")
(defcontroller +non-registered-parameter-number+	 99
  "Non-Registered Parameter Number")
(defcontroller +registered-parameter-number-fine+	100
  "Registered Parameter Number (Fine)")
(defcontroller +registered-parameter-number+		101) ; (coarse)

;;; Channel Mode Messages (without Value Byte, unless stated otherwise)
;;; Note: the 4 omni/poly messages must be received on the device's Base
;;; Channel.
(defcontroller +all-sound-off+		 120)
(defcontroller +reset-all-controllers+	 121)
(defcontroller +local-control+		 122) ; 0(-63): on/(64-)127: off
(defcontroller +all-notes-off+		 123)
(defcontroller +omni-mode-off+		 124) ; + all notes off
(defcontroller +omni-mode-on+		 125) ; + all notes off
(defcontroller +poly-mode-on/off+	 126) ; value equals the number of
					      ; channels, or zero if the
					      ; number of channels equals
					      ; the number of voices in the
					      ; receiver. + all notes off if 
					      ; value > 1 ("Mono Off")
(defcontroller +poly-mode-on+		 127) ; + all notes off

;;;
;;; Registered Parameter Numbers

(define +rpn-pitch-bend-sensitivity+   '(#x00 #x00))
(define +rpn-fine-tuning+              '(#x00 #x01))
(define +rpn-coarse-tuning+            '(#x00 #x02))
(define +rpn-reset+                    '(#x3f #xff)) ; clear current RPN


;;; ======================================================================
;;; ======================================================================
;;;
;;; General MIDI Definitions
;;;

(define +gm-patch-strings+
  (make-vector 128 '""))

;;; drum sounds are defined from 35-81, so we shift them back -32 to 3-49

(define +gm-drum-kit-strings+
  (make-vector 50 '""))

(define-macro (defgmpatch sym val . str)
  `(deflabel ,sym ,val +gm-patch-strings+ 
    ,(if (null? str) #f (car str))
    ,val))

(define-macro (defgmdrum sym val . str)
  `(deflabel ,sym ,val +gm-drum-kit-strings+
    ,(if (null? str) #f (car str))
    ,(- val 32)))

;;;
;;; Utilities

(define (gm-patch-name patch)
  (midi-opcode-name +gm-patch-strings+ patch 0 127 0))

(define (gm-drum-kit-name key)
  (midi-opcode-name +gm-drum-kit-strings+ key 35 81 -32))

; (gm-patch-name 71)		=> "Clarinet"
; (gm-patch-name "unset")	=> "unset"
; (gm-drum-kit-name 60)		=> "Hi Bongo"

;;;
;;; General MIDI Instrument Patches

;;; Piano
(defgmpatch +acoustic-grand-piano+	  0)
(defgmpatch +bright-acoustic-piano+	  1)
(defgmpatch +electric-grand-piano+	  2)
(defgmpatch +honky-tonk-piano+		  3)
(defgmpatch +electric-piano-1+		  4)
(defgmpatch +electric-piano-2+		  5)
(defgmpatch +harpsichord+		  6)
(defgmpatch +clavi+			  7)

;;; Chromatic Percussion
(defgmpatch +celesta+			  8)
(defgmpatch +glockenspiel+		  9)
(defgmpatch +music-box+			 10)
(defgmpatch +vibraphone+		 11)
(defgmpatch +marimba+			 12)
(defgmpatch +xylophone+			 13)
(defgmpatch +tubular-bells+		 14)
(defgmpatch +dulcimer+			 15)

;;; Organ
(defgmpatch +drawbar-organ+		 16)
(defgmpatch +percussive-organ+		 17)
(defgmpatch +rock-organ+		 18)
(defgmpatch +church-organ+		 19)
(defgmpatch +reed-organ+		 20)
(defgmpatch +accordion+			 21)
(defgmpatch +harmonica+			 22)
(defgmpatch +tango-accordion+		 23)

;;; Guitar
(defgmpatch +acoustic-guitar-nylon+	 24)
(defgmpatch +acoustic-guitar-steel+	 25)
(defgmpatch +electric-guitar-jazz+	 26)
(defgmpatch +electric-guitar-clean+	 27)
(defgmpatch +electric-guitar-muted+	 28)
(defgmpatch +overdriven-guitar+		 29)
(defgmpatch +distortion-guitar+		 30)
(defgmpatch +guitar-harmonics+		 31)

;;; Bass
(defgmpatch +acoustic-bass+		 32)
(defgmpatch +electric-bass-finger+	 33)
(defgmpatch +electric-bass-pick+	 34)
(defgmpatch +fretless-bass+		 35)
(defgmpatch +slap-bass-1+		 36)
(defgmpatch +slap-bass-2+		 37)
(defgmpatch +synth-bass-1+		 38)
(defgmpatch +synth-bass-2+		 39)

;;; Solo strings
(defgmpatch +violin+			 40)
(defgmpatch +viola+			 41)
(defgmpatch +cello+			 42)
(defgmpatch +contrabass+		 43)
(defgmpatch +tremolo-strings+		 44)
(defgmpatch +pizzicato-strings+          45)
(defgmpatch +orchestral-strings+	 46)
(defgmpatch +timpani+			 47)

;;; Ensemble
(defgmpatch +string-ensemble-1+          48)
(defgmpatch +string-ensemble-2+          49)
(defgmpatch +synthstrings-1+		 50)
(defgmpatch +synthstrings-2+		 51)
(defgmpatch +choir-aahs+		 52)
(defgmpatch +voice-oohs+		 53)
(defgmpatch +synth-voice+		 54)
(defgmpatch +orchestra-hit+		 55)

;;; Brass
(defgmpatch +trumpet+			 56)
(defgmpatch +trombone+			 57)
(defgmpatch +tuba+			 58)
(defgmpatch +muted-trumpet+		 59)
(defgmpatch +french-horn+		 60)
(defgmpatch +brass-section+		 61)
(defgmpatch +synthbrass-1+		 62)
(defgmpatch +synthbrass-2+		 63)

;;; Reed
(defgmpatch +soprano-sax+		 64)
(defgmpatch +alto-sax+	                 65)
(defgmpatch +tenor-sax+                  66)
(defgmpatch +baritone-sax+		 67)
(defgmpatch +oboe+			 68)
(defgmpatch +english-horn+		 69)
(defgmpatch +bassoon+			 70)
(defgmpatch +clarinet+			 71)

;;; Pipe
(defgmpatch +piccolo+			 72)
(defgmpatch +flute+			 73)
(defgmpatch +recorder+			 74)
(defgmpatch +pan-flute+			 75)
(defgmpatch +blown-bottle+		 76)
(defgmpatch +skakuhachi+		 77)
(defgmpatch +whistle+			 78)
(defgmpatch +ocarina+			 79)

;;; Synth banks are multiply defined for their nicknames.
;;; Only the default name is defgmpatch'ed

;;; Synth Lead
(defgmpatch +lead-1-square+		 80 "Lead 1 (Square)")
(define +lead-1+			 80)
(define +square-lead+		 80)
(define +square+			 80)
(defgmpatch +lead-2-sawtooth+		 81 "Lead 2 (Sawtooth)")
(define +lead-2+			 81)
(define +sawtooth-lead+		 81)
(define +sawtooth+			 81)
(defgmpatch +lead-3-calliope+		 82 "Lead 3 (Calliope)")
(define +lead-3+			 82)
(define +calliope-lead+		 82)
(define +calliope+			 82)
(defgmpatch +lead-4-chiff+		 83 "Lead 4 (Chiff)")
(define +lead-4+			 83)
(define +chiff-lead+		 83)
(define +chiff+			 83)
(defgmpatch +lead-5-charang+		 84 "Lead 5 (Charang)")
(define +lead-5+			 84)
(define +charang-lead+		 84)
(define +charang+			 84)
(defgmpatch +lead-6-voice+		 85 "Lead 6 (Voice)")
(define +lead-6+			 85)
(define +voice-lead+		 85)
(define +voice+			 85)
(defgmpatch +lead-7-fifths+		 86 "Lead 7 (Fifths)")
(define +lead-7+			 86)
(define +fifths-lead+		 86)
(define +fifths+			 86)
(defgmpatch +lead-8-bass+lead+		 87 "Lead 8 (Bass+Lead)")
(define +lead-8+			 87)
(define +bass+lead-lead+		 87)
(define +bass+lead+		 87)

;;; Synth Pad
(defgmpatch +pad-1-new-age+		 88 "Pad 1 (New Age)")
(define +pad-1+			 88)
(define +new-age-pad+		 88)
(define +new-age+			 88)
(defgmpatch +pad-2-warm+		 89 "Pad 2 (Warm)")
(define +pad-2+			 89)
(define +warm-pad+			 89)
(define +warm+			 89)
(defgmpatch +pad-3-polysynth+		 90 "Pad 3 (Polysynth)")
(define +pad-3+			 90)
(define +polysynth-Pad+		 90)
(define +polysynth+		 90)
(defgmpatch +pad-4-choir+		 91 "Pad 4 (Choir)")
(define +pad-4+			 91)
(define +choir-pad+		 91)
(define +choir+			 91)
(defgmpatch +pad-5-bowed+		 92 "Pad 5 (Bowed)")
(define +pad-5+			 92)
(define +bowed-pad+		 92)
(define +bowed+			 92)
(defgmpatch +pad-6-metallic+		 93 "Pad 6 (Metallic)")
(define +pad-6+			 93)
(define +metallic-pad+		 93)
(define +metallic+			 93)
(defgmpatch +pad-7-halo+		 94 "Pad 7 (Halo)")
(define +pad-7+			 94)
(define +halo-pad+			 94)
(define +halo+			 94)
(defgmpatch +pad-8-sweep+		 95 "Pad 8 (Sweep)")
(define +pad-8+			 95)
(define +sweep-pad+		 95)
(define +sweep+			 95)

;;; Synth Effects
(defgmpatch +fx-1-rain+			 96 "FX 1 (Rain)")
(define +fx-1+			 96)
(define +rain-fx+			 96)
(define +rain+			 96)
(defgmpatch +fx-2-soundtrack+		 97 "FX 2 (Soundtrack)")
(define +fx-2+			 97)
(define +soundtrack-fx+		 97)
(define +soundtrack+		 97)
(defgmpatch +fx-3-crystal+		 98 "FX 3 (Crystal)")
(define +fx-3+			 98)
(define +crystal-fx+		 98)
(define +crystal+			 98)
(defgmpatch +fx-4-atmosphere+		 99 "FX 4 (Atmosphere)")
(define +fx-4+			 99)
(define +atmosphere-fx+		 99)
(define +atmosphere+		 99)
(defgmpatch +fx-5-brightness+		100 "FX 5 (Brightness)")
(define +fx-5+			100)
(define +brightness-fx+		100)
(define +brightness+		100)
(defgmpatch +fx-6-goblins+		101 "FX 6 (Goblins)")
(define +fx-6+			101)
(define +goblins-fx+		101)
(define +goblins+			101)
(defgmpatch +fx-7-echoes+		102 "FX 7 (Echoes)")
(define +fx-7+			102)
(define +echoes-fx+		102)
(define +echoes+			102)
(defgmpatch +fx-8-sci-fi+		103 "FX 8 (Sci-Fi)")
(define +fx-8+			103)
(define +sci-fi-fx+		103)
(define +sci-fi+			103)

;;; Ethnic
(defgmpatch +sitar+			104)
(defgmpatch +banjo+			105)
(defgmpatch +shamisen+			106)
(defgmpatch +koto+			107)
(defgmpatch +kalimba+			108)
(defgmpatch +bagpipe+			109)
(defgmpatch +fiddle+			110)
(defgmpatch +shanai+			111)

;;; Percussive
(defgmpatch +tinkle-bell+		112)
(defgmpatch +agogo+			113)
(defgmpatch +steel-drums+		114)
(defgmpatch +woodblock+			115)
(defgmpatch +taiko-drum+		116)
(defgmpatch +melodic-tom+		117)
(defgmpatch +synth-drum+		118)
(defgmpatch +reverse-cymbal+		119)

;;; Sound Effects
(defgmpatch +guitar-fret-noise+		120)
(defgmpatch +breath-noise+		121)
(defgmpatch +seashore+			122)
(defgmpatch +bird-tweet+		123)
(defgmpatch +telephone-ring+		124)
(defgmpatch +helicopter+		125)
(defgmpatch +applause+			126)
(defgmpatch +gunshot+			127)

;;;
;;; General MIDI Drum Kit

;;; Drum sounds are by convention received on channel 9, but could be any
;;; sequence of logical channels.

(define *gm-percussion-channels*	#(9))

(define (gm-percussion-channel-p chan)
  (do ((i 0 (+ i 1))
       (f #f)
       (e (vector-length *gm-percussion-channels*)))
      ((or f (not (< i e))) f)
    (set! f (= (vector-ref *gm-percussion-channels* i)
	       chan))))

(defgmdrum +acoustic-bass-drum+		 35)
(defgmdrum +bass-drum-1+		 36)
(defgmdrum +side-stick+			 37)
(defgmdrum +acoustic-snare+		 38)
(defgmdrum +hand-clap+			 39)
(defgmdrum +electric-snare+		 40)
(defgmdrum +low-floor-tom+		 41)
(defgmdrum +closed-hi-hat+		 42)
(defgmdrum +high-floor-tom+		 43)
(defgmdrum +pedal-hi-hat+		 44)
(defgmdrum +low-tom+			 45)
(defgmdrum +open-hi-hat+		 46)
(defgmdrum +low-mid-tom+		 47)
(defgmdrum +hi-mid-tom+			 48)
(defgmdrum +crash-cymbal-1+		 49)
(defgmdrum +high-tom+			 50)
(defgmdrum +ride-cymbal-1+		 51)
(defgmdrum +chinese-cymbal+		 52)
(defgmdrum +ride-bell+			 53)
(defgmdrum +tambourine+			 54)
(defgmdrum +splash-cymbal+		 55)
(defgmdrum +cowbell+			 56)
(defgmdrum +crash-cymbal-2+		 57)
(defgmdrum +vibraslap+			 58)
(defgmdrum +ride-cymbal-2+		 59)
(defgmdrum +hi-bongo+			 60)
(defgmdrum +low-bongo+			 61)
(defgmdrum +mute-hi-conga+		 62)
(defgmdrum +open-hi-conga+		 63)
(defgmdrum +low-conga+			 64)
(defgmdrum +high-timbale+		 65)
(defgmdrum +low-timbale+		 66)
(defgmdrum +high-agogo+			 67)
(defgmdrum +low-agogo+			 68)
(defgmdrum +cabasa+			 69)
(defgmdrum +maracas+			 70)
(defgmdrum +short-whistle+		 71)
(defgmdrum +long-whistle+		 72)
(defgmdrum +short-guiro+		 73)
(defgmdrum +long-guiro+			 74)
(defgmdrum +claves+			 75)
(defgmdrum +hi-wood-block+		 76)
(defgmdrum +low-wood-block+		 77)
(defgmdrum +mute-cuica+			 78)
(defgmdrum +open-cuica+			 79)
(defgmdrum +mute-triangle+		 80)
(defgmdrum +open-triangle+		 81)

;;; ======================================================================
;;;
;;; Sysex Message Utilities
;;;


;;; 
;;; System Exclusive messages have the general form
;;;
;;;    F0 <Manufacturer-ID> <data> ... F7
;;;

;;;
;;; Manufacturer's IDs.  0 is reserved for multi-byte ID's.
;;; 
;;; Stolen from http://www.io.com/~jimm/midi_ref.html#Manufacturers with 
;;; some company designators omitted where it seemed reasonable.

;;; USA Manufacturers
(define +sequential-circuits-id+	      #x01)
(define +idp-id+			      #x02)
(define +voyetra-id+		      #x03) ; voyetra/octave plateau
(define +moog-id+			      #x04) ; moog music
(define +passport-id+		      #x05) ; passport designs
(define +lexicon-id+		      #x06)
(define +kurzweil-id+		      #x07)
(define +fender-id+		      #x08)
(define +gulbransen-id+		      #x09)
(define +akg-id+			      #x0a) ; akg acoustics
(define +voyce-id+			      #x0b) ; voyce music
(define +waveframe-id+		      #x0c)
(define +ada-id+			      #x0d)
(define +garfield-id+		      #x0e) ; garfield electronics
(define +ensoniq-id+		      #x0f)
(define +oberheim-id+		      #x10)
(define +apple-id+			      #x11) ; apple computer
(define +grey-matter-id+		      #x12)
(define +digidesign-id+		      #x13)
(define +palm-tree-id+		      #x14) ; palm tree instruments
(define +jl-cooper-id+		      #x15)
(define +lowrey-id+		      #x16)
(define +adams-smith-id+		      #x17)
(define +e-mu-id+			      #x18) ; e-mu systems
(define +harmony-id+		      #x19) ; harmony systems
(define +art-id+			      #x1a)
(define +baldwin-id+		      #x1b)
(define +eventide-id+		      #x1c)
(define +inventronics-id+		      #x1d)
(define +key-concepts-id+		      #x1e)
(define +clarity-id+		      #x1f)

;;; europe
(define +passac-id+		      #x20)
(define +siel-id+			      #x21)
(define +synthaxe-id+		      #x22)
(define +stepp-id+			      #x23)
(define +hohner-id+		      #x24)
(define +twister-id+		      #x25)
(define +solton-id+		      #x26)
(define +jellinghaus-id+		      #x27)
(define +southworth-id+		      #x28)
(define +ppg-id+			      #x29)
(define +jen-id+			      #x2a)
(define +solid-state-id+		      #x2b) ; solid stat logic
(define +audio-vertrieb-id+	      #x2c)
(define +hinton-id+		      #x2d) ; hinton instruments
(define +soundtracs-id+		      #x2e)
(define +elka-id+			      #x2f)
(define +dynachord-id+		      #x30)
(define +clavia-id+		      #x33) ; clavia digital instr.
(define +audio-architecture-id+	      #x34)
(define +soundcraft-id+		      #x39) ; soundcraft electronics
(define +wersi-id+			      #x3b)
(define +avab-id+			      #x3c) ; avab electronik
(define +digigram-id+		      #x3d)
(define +waldorf-id+		      #x3e) ; waldorf electronics
(define +quasimidi-id+		      #x3f)

;;; japan
(define +kawai-id+			      #x40)
(define +roland-id+		      #x41)
(define +korg-id+			      #x42)
(define +yamaha-id+		      #x43)
(define +casio-id+			      #x44)
(define +moridaira-id+		      #x45)
(define +kamiya-id+		      #x46)
(define +akai-id+			      #x47)
(define +japan-victor-id+		      #x48)
(define +meisosha-id+		      #x49)
(define +hoshino-gakki-id+		      #x4a)
(define +fujitsu-id+		      #x4b)
(define +sony-id+			      #x4c)
(define +nishin-onpa-id+		      #x4d)
(define +teac-id+			      #x4e)
(define +matsushita-electric-id+	      #x50)
(define +fostex-id+		      #x51)
(define +zoom-id+			      #x52)
(define +midori-id+		      #x53) ; midori electronics
(define +matsushita-communication-id+    #x54)
(define +suzuki-id+		      #x55)

;;; usa extended
(define +warner-id+	     '(0 #x00 #x01)) ; warner new media
(define +digital-music-id+ '(0 #x00 #x07)) ; digital music corp.
(define +iota-id+	     '(0 #x00 #x08)) ; iota systems
(define +new-england-id+   '(0 #x00 #x09)) ; new england digital
(define +artisyn-id+	     '(0 #x00 #x0a))
(define +ivl-id+	     '(0 #x00 #x0b)) ; ivl technologies
(define +southern-music-id+     '(0 #x00 #x0c)) ; southern music systems
(define +lake-butler-id+	     '(0 #x00 #x0d)) ; lake butler sound co.
(define +alesis-id+	     '(0 #x00 #x0e))
(define +dod-id+		     '(0 #x00 #x10)) ; dod electronics
(define +studer-id+	     '(0 #x00 #x11)) ; studer editech
(define +perfect-fretworks-id+  '(0 #x00 #x14))
(define +kat-id+		     '(0 #x00 #x15))
(define +opcode-id+	     '(0 #x00 #x16))
(define +rane-id+		     '(0 #x00 #x17)) ; rane corporation
(define +spatial-sound-id+	     '(0 #x00 #x18)) ; spatial sound/anadi inc.
(define +kmx-id+		     '(0 #x00 #x19))
(define +allen-&-heath-id+	     '(0 #x00 #x1a)) ; allen & heath brenell
(define +peavey-id+	     '(0 #x00 #x1b)) ; peavey electronics
(define +360-id+		     '(0 #x00 #x1c)) ; 360 systems
(define +spectrum-id+	     '(0 #x00 #x1d)) ; spectrum design & dev.
(define +marquis-musi-id+	     '(0 #x00 #x1e))
(define +zeta-id+		     '(0 #x00 #x1f)) ; zeta systems
(define +axxes-id+		     '(0 #x00 #x20))
(define +orban-id+		     '(0 #x00 #x21))
(define +kti-id+		     '(0 #x00 #x24))
(define +breakaway-id+	     '(0 #x00 #x25)) ; breakaway technologies
(define +cae-id+		     '(0 #x00 #x26))
(define +rocktron-id+	     '(0 #x00 #x29)) ; rocktron corp.
(define +pianodisc-id+	     '(0 #x00 #x2a))
(define +cannon-id+	     '(0 #x00 #x2b)) ; cannon research corp.
(define +rogers-id+	     '(0 #x00 #x2d)) ; rogers instrument corp.
(define +blue-sky-id+	     '(0 #x00 #x2e)) ; blue sky logic
(define +encore-id+	     '(0 #x00 #x2f)) ; encore electronics
(define +uptown-id+	     '(0 #x00 #x30))
(define +voce-id+		     '(0 #x00 #x31))
(define +cti-id+		     '(0 #x00 #x32)) ; cti audio
(define +s&s-id+		     '(0 #x00 #x33)) ; s&s research
(define +broderbund-id+	     '(0 #x00 #x34)) ; broderbund software
(define +allen-organ-id+	     '(0 #x00 #x35)) ; allen organ co.
(define +music-quest-id+	     '(0 #x00 #x37))
(define +aphex-id+		     '(0 #x00 #x38))
(define +gallien-krueger-id+    '(0 #x00 #x39))
(define +ibm-id+		     '(0 #x00 #x3a))
(define +hotz-id+		     '(0 #x00 #x3c)) ; hotz instruments techn.
(define +eta-id+		     '(0 #x00 #x3d)) ; eta lighting
(define +nsi-id+		     '(0 #x00 #x3e)) ; nsi corporation
(define +ad-lib-id+	     '(0 #x00 #x3f))
(define +richmond-id+	     '(0 #x00 #x40)) ; richmond sound design
(define +microsoft-id+	     '(0 #x00 #x41))
(define +software-toolworks-id+ '(0 #x00 #x42)) ; the software toolworks
(define +rjmg/niche-id+	     '(0 #x00 #x43))
(define +intone-id+	     '(0 #x00 #x44))
(define +gt-electronics-id+     '(0 #x00 #x47)) ; gt electr./groove tubes
(define +intermidi-id+	     '(0 #x00 #x48))
(define +lone-wolf-id+	     '(0 #x00 #x55))
(define +musonix-id+	     '(0 #x00 #x64))

(define +sgi-id+		     '(0 #x01 #x04)) ; silicon graphics, inc.

;;; europe extended
(define +dream-id+		     '(0 #x20 #x00))
(define +strand-id+	     '(0 #x20 #x00)) ; strand lighting
(define +amek-id+            '(0 #x20 #x00)) ; amek systems & controls
(define +dr.boehm-id+	     '(0 #x20 #x00)) ; dr boehm/musician int'l
(define +trident-id+	     '(0 #x20 #x00))
(define +real-world-id+	     '(0 #x20 #x00)) ; real world design
(define +yes-id+		     '(0 #x20 #x00)) ; yes technology
(define +audiomatica-id+	     '(0 #x20 #x00))
(define +bontempi-id+	     '(0 #x20 #x00)) ; bontempi/farfisa
(define +fbt-id+		     '(0 #x20 #x00)) ; f.b.t. electronica
(define +larking-id+	     '(0 #x20 #x00)) ; larking audio
(define +zero-88-id+	     '(0 #x20 #x00)) ; zero 88 lighting
(define +micon-id+		     '(0 #x20 #x10)) ; micon audio electronics
(define +forefront-id+	     '(0 #x20 #x10)) ; forefront technology
(define +kenton-id+	     '(0 #x20 #x10)) ; kenton electronics
(define +adb-id+		     '(0 #x20 #x10))
(define +marshall-id+	     '(0 #x20 #x10)) ; jim marshall products
(define +dda-id+		     '(0 #x20 #x10))
(define +tc-id+		     '(0 #x20 #x10)) ; tc electronic


;;;
;;; universal (manufacturer-independent) id's

;;;
;;; private-use sysex messages may use the non-commercial id at own risk.

(define +non-commercial-id+	      #x7d) ; aka "eductaional use"

;;;
;;; sysex messages of interest for various manufacturer-independent devices
;;; may make use of the Real-Time and Non-Real-Time ID's.  The general
;;; syntax of such messages is 
;;;
;;;   F0  <ID>  <Device-ID>  <Sub-ID-1>  <Sub-ID-2>  <data>  ...  F7
;;;
;;; <Device-ID> is a channel number between 0 and 7F.  7F denotes a
;;; "global" universal sysex message.
;;;
;;; NOTE: The base Device-ID may or may not be independent from the
;;; device's (or parts) Base Channel.

(define +non-real-time-id+		      #x7e)

;;; non-real-time universal sysex message sub-ids
(define +sample-dump-header-sub-id+		#x01)
(define +sample-dump-packet-sub-id+		#x02)
(define +dump-request-sub-id+			#x03)
(define +midi-time-code-setup-sub-id+		#x04)
(define +sample-dump-extensions-sub-id+		#x05)
(define +inquiry-message-sub-id+			#x06)
(define +file-dump-sub-id+				#x07)
(define +midi-tuning-standard-sub-id+		#x08)
(define +general-midi-message-sub-id+		#x09)
(define +end-of-file-sub-id+			#x7b)
(define +wait-sub-id+				#x7c)
(define +cancel-sub-id+				#x7d)
(define +nak-sub-id+				#x7e)
(define +ack-sub-id+				#x7f)

(define +real-time-id+		      #x7f)

;;; real-time universal sysex message sub-ids
(define +long-form-mtc-sub-id+			#x01)
(define +midi-show-control-sub-id+			#x02)
(define +notation-information-sub-id+		#x03)
(define +device-control-sub-id+			#x04)
(define +real-time-mtc-cueing-sub-id+		#x05)
(define +midi-machine-control-command-sub-id+	#x06)
(define +midi-machine-control-response-sub-id+	#x07)
(define +single-note-retune-sub-id+		#x08)

;;;
;;; Conversion Utilities

(define (n-bit-twoscomp-p num bits signed? . args)
  (let ((error? (if (null? args) #f (car args))))
    (if signed?
      (if (and (< num 0) 
	       (<= (integer-length num) bits))
	#t
	(if error?
	  (err "Not a signed ~s-bit byte: ~s."
               bits num)
	  #f))
      (if (and (not (< num 0))
	       (<= (integer-length num) bits))
	#t
	(if error?
	  (err "Not an unsigned ~s-bit byte: ~s."
               bits num)
	  #f)))))

(define (n-bit-twoscomp num bits signed?)
  (n-bit-twoscomp-p num bits signed? #t)
  (if (< num 0)
      (+ num (expt 2 bits))
    num))

(define (n-bit-bytes num bytes bits lsb-first?)
  (n-bit-twoscomp-p num (* bytes bits) (< num 0) #t)
  (let ((l '()))
    (do ((i 0 (+ i 1)))
	((not (< i bytes)) #f)
      (set! l (cons (ldb (byte bits (* i bits)) num)
		    l)))
    (if lsb-first?
      (reverse l)
      l)))

;;; turn a sequence of bytes into a list of nibbles, either lsb-first or
;;; msb-first.  data may be either bytes or chars or strings or lists of
;;; these. 

(define (nibblize lsb-first? . data)
  (let ((res '()) 
	(o1 (if lsb-first? 0 4))
	(o2 (if lsb-first? 4 0)))
    (letrec ((nblize
              (lambda (x)
                (cond ((string? x)
                       (do ((i 0 (+ i 1))
                            (e (string-length x)))
                           ((not (< i e)) #f)
                         (nblize (string-ref x i)))
                       (nblize 0))
		      ((vector? x)
                       (do ((i 0 (+ i 1))
                            (e (vector-length x)))
                           ((not (< i e)) #f)
                         (nblize (vector-ref x i))))
                      ((pair? x)
                       (do ((i 0 (+ i 1))
                            (e (length x)))
                           ((not (< i e)) #f)
                         (nblize (list-ref x i))))
                      ((char? x)
                       (set! x (char->integer x))
                       (push (ldb (byte 4 o1) x) res)
                       (push (ldb (byte 4 o2) x) res))
                      ((exact? x)
                       (push (ldb (byte 4 o1) x) res)
                       (push (ldb (byte 4 o2) x) res))
                      (else
                       #f)))))
      (loop for d in data do (nblize d))
      (reverse! res))))

; (use-modules (ice-9 format))
; (format t "#x~x" (n-bit-twoscomp #x3fff 14 #f))	=> #x3fff
; (format t "#x~x" (n-bit-twoscomp #x4000 14 #f))	=> Error
; (format t "#x~x" (n-bit-twoscomp #x-1 14 #f))		=> Error
; (format t "#x~x" (n-bit-twoscomp #x1fff 14 #t))		=> #x1fff
; (format t "#x~x" (n-bit-twoscomp #x2000 14 #t))		=> Error
; (format t "#x~x" (n-bit-twoscomp #x-2000 14 #t))		=> #x2000
; (format t "#x~x" (n-bit-twoscomp #x-1 14 #t))		=> #x3fff

; (n-bit-bytes #x3F80 2 7 t)		=> (0 127)
; (n-bit-bytes #xFF00 2 8 #f)		=> (255 0)
; (n-bit-bytes #x-1 2 3 #f)		=> (7 7)
; (n-bit-bytes #b10010110 8 1 #f)	=> (1 0 0 1 0 1 1 0)

; (nibblize #f "abc")
;    => (6 1 6 2 6 3  0 0) ; = #x61 #x62 #x63 #x00
; (nibblize t 1 2 3 4) 
;   => (1 0 2 0 3 0 4 0)
; (nibblize #f "abc" #f '(#x12 #x34 #(#x56 #x78) #x90))
;   => (6 1 6 2 6 3  0 0 1 2 3 4 5 6 7 8 9 0)
;       --- --- ---  ---
;       'a' 'b' 'c' '\0'

;;;
;;; For instance, to enable or disable the General MIDI System for all
;;; devices on a MIDI bus, one would send the sysex message
;;;
;;;   F0 7E 7F 09 xx F7
;;;
;;; where <xx> is either 0 for disable or 1 for enable.  Note, too, that
;;; the <Device-ID> is set to 7F, indicating a "global" message to 
;;; be received by all devices.

(define +all-device-ids+ #x7F)

(define (make-gm-mode-sysex-data gm-on? . args)
  (with-args (args &key (device-id +all-device-ids+))
    (make-sysex-data +non-real-time-id+
		     device-id
		     +general-midi-message-sub-id+
		     (if gm-on? 1 0))))

; (make-gm-mode-sysex-data t)
;   => #(240 126 127 9 1 247)
; (make-gm-mode-sysex-data #f)
;   => #(240 126 127 9 0 247)

;;;
;;; Similarly, the device's master volume may be set using the following
;;; Real-Time sysex message:
;;;
;;;   F0 7F 7F 04 01 ll mm F7
;;;
;;; where <ll> and <mm> are the least-significant and most-significant 7
;;; bits of a 14-bit volume.  Again, <Device-ID> is set to 7F,
;;; indicating a "global" message to be received by all devices.

(define +master-volume-sub-id-2+ #x01)

(define (make-master-volume-sysex-data . args)
  (with-args (args &key coarse fine
                   (device-id +all-device-ids+))
    (unless (or coarse fine)
      (err "No volume specified."))
    (unless (or (and coarse (n-bit-twoscomp-p coarse 7 #f))
	        (and fine (n-bit-twoscomp-p fine 14 #f)))
      
      (err "~a volume ~a is not a ~a-bit value."
           (if fine "Fine" "Course")
           (or coarse fine)
           (if fine "14" "7")))
    (make-sysex-data +real-time-id+
		     device-id
		     +device-control-sub-id+
		     +master-volume-sub-id-2+
		     (if fine (logand fine #b1111111) coarse)
		     (if fine (ash (logand fine #b11111110000000) -7) 
                         coarse))))

; (make-master-volume-sysex-data :coarse 127)
;   => #(240 127 127 4 1 127 127 247)	; |127|127| = "1111111 1111111"
; (make-master-volume-sysex-data :coarse 5)
;   => #(240 127 127 4 1 5 5 247)		; |5|5|     = "0000101 0000101"
; (make-master-volume-sysex-data :fine #x3fff)
;   => #(240 127 127 4 1 127 127 247)	; |127|127| = "1111111 1111111"

;;;
;;; Time-Code-Related Sysex Messages
;;;

;;;
;;; SMPTE Format
;;; 
;;; In each frame, 26 of the 80 bits carry the SMPTE time or 'address', in
;;; binary coded decimal.  32 bits are assigned as 8 groups of 4 USER BITS. 
;;; This capacity is generally used to carry extra info such as reel number
;;; and date.  Bits 43 and 59 are assigned as the Binary Group Flag Bits
;;; BGFB and are used to indicate when a standard character set is used to
;;; format the User Bits data. The Binary Group Flag Bits should be used
;;; only as shown in the truth table below. The Unassigned entries in the
;;; table should not be used, as they may be allocated specific meanings in
;;; the future.
;;;                                   Bit 43  Bit 59
;;;    No User Bits format specified      0       0
;;;    Eight-bit character set            1       0
;;;    Unassigned (Reserved)              0       1
;;;    Unassigned (Reserved)              1       1 
;;;
;;; The last sixteen Bits make up the SYNC WORD. A timecode reader uses
;;; these Bits to find the frame boundary, the tape direction, and the
;;; bit-rate of the sync tone. The values of these Bits are fixed as 0011
;;; 1111 1111 1101. 
;;;
;;;
;;;                         7         6         5         4
;;;    Bits 40-79: 9876543210987654321098765432109876543210
;;;                ----------------                           Sync Word
;;;                                ----                       User Bits 8
;;;                                    -                      BGFB
;;;                                     -                     <Reserved>
;;;                                      --                   Hours Tens
;;;                                        ----               User Bits 7
;;;                                            ----           Hours Ones
;;;                                                ----       User Bits 6
;;;                                                    -      BGFB
;;;                                                     ---   Minutes Tens
;;;                         3         2         1         0
;;;    Bits  0-39: 9876543210987654321098765432109876543210
;;;                ----                                       User Bits 5
;;;                    ----                                   Minutes Ones
;;;                        ----                               User Bits 4
;;;                            -                              Bi-Phase Corr.
;;;                             ---                           Seconds Tens
;;;                                ----                       User Bits 3
;;;                                    ----                   Seconds Ones
;;;                                        ----               User Bits 2
;;;                                            -              Drop-Frame Flag
;;;                                             -             Color Frame Flag
;;;                                              --           Frames Tens
;;;                                                ----       User Bits 1
;;;                                                    ----   Frames Ones

;;;
;;; SMPTE Full Frame Message
;;;
;;; The Full Frame simply cues a slave to a particular SMPTE time. The slave
;;; doesn't actually start running until it starts receiving Quarter Frame
;;; messages. (Which implies that a slave is stopped whenever it is not
;;; receiving Quarter Frame messages). The master should pause after sending
;;; a Full Frame, and before sending a Quarter Frame, in order to give the
;;; slave time to cue to the desired SMPTE time. 
;;;
;;; Syntax:
;;;
;;;   F0 +Real-Time-ID+ <Device-ID> 01 01 hr mn sc fr F7
;;; 
;;; The hr, mn, sc, and fr are the hours, minutes, seconds, and frames of
;;; the current SMPTE time. The hours byte also contains the SMPTE Type as
;;; per the Quarter Frame's Hours High Nibble message: 
;;; 
;;;   0 = 24 fps
;;;   1 = 25 fps
;;;   2 = 30 fps (Drop-Frame)
;;;   3 = 30 fps

(define +smpte-full-frame-sub-id-2+ #x01)
(define +smpte-user-bits-sub-id-2+  #x02)


(define +smpte-format-24fps+	 0)
(define +smpte-format-25fps+	 1)
(define +smpte-format-30fps-drop+	 2)
(define +smpte-format-30fps+	 3)

(define (encode-smpte-data hr mn sc fr . args)
  (with-args (args &key subframes format)
    (unless (and (<= 0 hr 23) 
                 (<= 0 mn 59)
                 (<= 0 sc 59)
	         (<= 0 fr
                     (cond ((= format +smpte-format-24fps+) 23)
                           ((= format +smpte-format-25fps+) 24)
                           ((or (= format +smpte-format-30fps-drop+)
                                (= format +smpte-format-30fps+)) 29)
                           (else (err "Not a valid SMPTE format ID: ~a."
                                      format))))
	         (or (not subframes)
		     (<= 0 subframes 99)))
      (err "SMPTE values out of range: ~a ~a ~a ~a ~s."
           hr mn sc fr subframes))
    (set! hr (dpb format (byte 3 5) hr))
    (if subframes
      (list hr mn sc fr subframes)
      (list hr mn sc fr))))

; (encode-smpte-data 4 5 6 7 :format 3)
;   => (100 5 6 7)			; 100 = #b11 00100 = format 3, hour 4
; (encode-smpte-data 4 5 6 7 :format 0 :subframes 45)
;   => (4 5 6 7 45)
; (encode-smpte-data 23 59 59 29 :format +smpte-format-30fps+)
;   => (119 59 59 29)			; 119 = #b11 10111 = format 3, hour 23

(define (make-smpte-full-frame-sysex-data hr mn sc fr . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (make-sysex-data +real-time-id+ device-id
		     +long-form-mtc-sub-id+ +smpte-full-frame-sub-id-2+
		     (encode-smpte-data hr mn sc fr :format format))))

; (make-SMPTE-full-frame-sysex-data 1 2 3 4)
;   => #(240 127 127 1 1 97 2 3 4 247)	; 97 = #b11 00001 = format 3, hour 1

;;;
;;; SMPTE also provides for 32 "user bits", information for special
;;; functions which vary with each product.  Up to 4 characters or 8 digits
;;; can be written. Examples of use are adding a date code or reel number to
;;; a tape. The user bits tend not to change throughout a run of time code,
;;; so rather than stuffing this information into a Quarter Frame, MTC
;;; provides a separate SysEx message to transmit this info.
;;;
;;;   F0 7F cc 01 02 u1 u2 u3 u4 u5 u6 u7 u8 u9 F7
;;;
;;; cc is the SysEx channel (0 to 127). Only the low nibble of each of the
;;; first 8 data bytes is used. Only the 2 low bits of u9 is used.  These
;;; nibbles decode into an 8-bit format of aaaabbbb ccccdddd eeeeffff
;;; gggghhhh ii. It forms 4 8-bit characters, and a 2 bit Format Code. u1
;;; through u8 correspond to the SMPTE Binary Groups 1 through 8. u9 are the
;;; 2 Binary Group Flag Bits, defined by SMPTE.

(define +smpte-user-bits-raw+   #b00)
(define +smpte-user-bits-chars+ #b10)

(define (make-smpte-user-bits-sysex-data format data . args)
  (with-args (args &key (device-id +all-device-ids+))
    (let* ((fmt (case format
                  ((:raw ) 0)
                  ((:bytes ) 1)
                  ((:chars ) 2)
                  ((:string ) 3)
                  (else
                   (err ":format not one of :raw :bytes :chars :string"))))
	   (size (if (= fmt 0) 4 8))
           (len #f)
           (ref #f))
      
      (cond ((string? data)
             (set! len (string-length data))
             (set! ref (function string-ref)))
            ((vector? data)
             (set! len (vector-length data))
             (set! ref (function vector-ref)))
            ((list? data)
             (set! len (length len))
             (set! ref (function list-ref))))
      
      (unless (= len (if (= size 4) 8 4))
        (err "~a format requires ~d data bytes, got: ~s."
             format 
             (if (= size 4) 8 4)
             data))
      (unless (case fmt
	        ((0 1)
                 (do ((f #t)
                      (i 0 (+ i 1))
                      (z #f))
                     ((or (not (< i len)) (not f)) f)
                   (set! z ( ref data))
                   (set! f (and (exact? z) (<= 0 z size))))) 
                ((2  )
                 (do ((f #t)
                      (i 0 (+ i 1)))
                     ((or (not (< i len)) (not f)) f)
                   (set! f (char? ( ref data) ))))
                ((3 )
                 (string? data)))
        (err "Not valid ~d-bit data: ~s" size data))
      (let ((datafn
             (lambda (n)
               (if (= fmt 0)
                 ( ref data n)
                 (let ((d ( ref data (inexact->exact (floor n 2)))))
                   (when (>= fmt 2)
                     (set! d (char->integer d)))
                   (if (even? n)
                     (ash (logand d #b11110000) -4)
                     (logand d #b1111)))))))
        (make-sysex-data +real-time-id+
		         device-id
		         +long-form-mtc-sub-id+
		         +smpte-user-bits-sub-id-2+
		         (datafn 0)
                         (datafn 1)
                         (datafn 2)
                         (datafn 3)
		         (datafn 4)
                         (datafn 5)
                         (datafn 6)
                         (datafn 7)
		         (if (= fmt 2)
			   +smpte-user-bits-chars+
			   +smpte-user-bits-raw+))))))



; (make-smpte-user-bits-sysex-data :raw '(1 2 3 4 5 6 7 8))
;   => #(240 127 127 1 2 1 2 3 4 5 6 7 8 0 247) ; all in nibbles
; (make-smpte-user-bits-sysex-data :bytes '(128 129 130 131))
;   => #(240 127 127 1 2 8 0 8 1 8 2 8 3 0 247) ; #x80 #x81 #x82 #x83, fmt=0
; (make-smpte-user-bits-sysex-data :chars '(#\t #\e #\s #\t))
;   => #(240 127 127 1 2 5 4 4 5 5 3 5 4 2 247) ; #x54455354 = :|test|, fmt=2
; (make-smpte-user-bits-sysex-data :string "TEST")
;   => #(240 127 127 1 2 5 4 4 5 5 3 5 4 2 247) ; as above

;;; 
;;; Notation Information
;;;

;;; The Bar Marker message indicates the start of a musical measure. It
;;; could also be used to setup and mark off bars of an introductory "count
;;; down". 
;;;
;;;   F0 7F cc 03 01 lb mb F7
;;; 
;;; cc is the SysEx channel (0 to 127).  lb mb is the desired bar number as
;;; a signed 14-bit value. Zero and negative numbers up to -8,190 indicate
;;; count off measures. For example, a value of -1 (ie, lb mb = 7F 7F) means
;;; that there is a one measure introduction. A value of zero would indicate
;;; no count off. Positive values indicate measures of the piece. The first
;;; measure is bar 1 (ie, lb mb = 01 00). A maximum neg number (lb mb = 00
;;; 40) indicates "stopped play" condition. A maximum positive value (lb mb
;;; = 7E 3F) indicates running condition,but no idea about measure
;;; number. This would be used by a device wishing to mark the passage of
;;; measures without keeping track of the actual measure number.

(define +bar-marker-sub-id-2+ #x01)

(define (make-measure-number-sysex-data num . args)
  (with-args (args &key (countoff #f)
                   (device-id +all-device-ids+))
    (when countoff 
      (set! num (- (abs num))))		; ensure num is negative
    ;; convert to 14-bit two's-complement
    (set! num (n-bit-twoscomp num 14 #t))
    (make-sysex-data +real-time-id+
		     device-id
		     +notation-information-sub-id+
		     +bar-marker-sub-id-2+
		     (ldb (byte 7 0) num)
		     (ldb (byte 7 7) num))))

; (make-measure-number-sysex-data #x1FFF)
;   => #(240 127 127 3 1 127 63 247)	; |63|127| = 0111111 1111111
; (make-measure-number-sysex-data #x-2000)
;   => #(240 127 127 3 1 0 64 247)	; |64|0|   = 1000000 0000000
; (make-measure-number-sysex-data #x-1FFF)
;   => #(240 127 127 3 1 1 64 247)	; |64|1|   = 1000000 0000001

;;;
;;; Time Signature
;;; 
;;;   F0 7F <Device-ID> 03 <Sub-ID2> ln nn dd qq [nn dd...] F7
;;; 
;;; ln is the number of data bytes following this field (3 if not a compound
;;; time signature).  nn dd are the numerator and denominator, defined as in 
;;; MIDI file time signatures.  Similarly, qq is the number of notated 32nd
;;; notes in a MIDI quarter note.  Additional pairs of nn/dd's are for 
;;; compound time signatures only.

(define +time-signature-now-sub-id-2+          #x02)
(define +time-signature-next-measure-sub-id-2+ #x42)

;;; compound meters:
;;; 
;;;   3+3+2                   3    3
;;;   -----  as (3 3 2) 8,    - + --  as (3 3) (4 16), etc. 
;;;     8                     4   16

(define (make-time-signature-sysex-data numerators denominators . args)
  (with-args (args &key (32nds 8)
                   (defer #f)
                   (device-id +all-device-ids+))
    ;; i should fix this for vectors...
    (unless (list? numerators)
      (set! numerators (list numerators)))
    (unless (list? denominators)
      (set! denominators (list denominators)))
    (let* ((len (max (length numerators) (length denominators)))
	   (args (loop with f and r
		       for i from 0
		       for n = (or (list-ref numerators i) n)
		       for d = (or (list-ref denominators i) d)
		       repeat len
		       do (multiple-value-setq (f r) 
                            (floor (log2 d)))
		       collect n
		       collect (if (not (zero? r))
				 (err "Not a power of 2: ~s" d)
			         f))))
      (make-sysex-data +real-time-id+
		       device-id
		       +notation-information-sub-id+
		       (if defer
                         +time-signature-next-measure-sub-id-2+
			 +time-signature-now-sub-id-2+)
		       (+ (* 2 len) 1)
		       (first args) (second args) 32nds
		       (cdr (cdr args))))))

; (make-time-signature-sysex-data 3 4 :defer t)
;   => #(240 127 127 3 66 3 3 2 8 247)
; (make-time-signature-sysex-data 3 '(2 4)) 
;   => #(240 127 127 3  2 5 3 1 8 3 2 247)
; (make-time-signature-sysex-data '(3 3 2) 8) 
;   => #(240 127 127 3  2 7 3 3 8 3 3 2 3 247)
; (make-time-signature-sysex-data '(3 3) '(4 16))
;   => #(240 127 127 3  2 5 3 2 8 3 4 247)

;;;
;;; MTC Setup Messages
;;; 
;;; The Setup message can be used to implement one of 19
;;; defined "events". A master device uses this message to tell slave units
;;; what "events" to perform, and when to perform those events.
;;;
;;;   F0 7E <Device-ID> 04 <Sub-ID-2> hr mn sc fr ff sl sm ... F7
;;; 
;;; hr mn sc fr ff is the SMPTE time when the event is to occur. This is
;;; just like the Full Frame message,except that there is also a fractional
;;; frame parameter, ff, which is 1/100 of a frame (ie, a value from 0 to
;;; 99).  Again, hr contains the time code format in bits 4-5.  sl sm is this
;;; event's 14-bit Event Number.  id tells what this Event Type is. 
;;;
;;; The extended (`+Setup-Xtnd-*') messages have additional information
;;; consisting of a nibblized MIDI data stream, LS nibble first.  The
;;; exception is Set-Up Type OE, where the additional information is
;;; nibblized ASCII, LS nibble first.  An ASCII newline is accomplished by
;;; sending CR and LF in the ASCII. CR alone functions solely as a carriage
;;; return, and LF alone functions solely as a Line-Feed.
;;;
;;; For example, a MIDI Note On message such as 91 46 7F would be nibblized
;;; and sent as 01 09 06 04 0F 07.  In this way, any device can decode any
;;; message regardless of who it was intended for.  Device-specific messages
;;; should be sent as nibblized MIDI System Exclusive messages.

(define +setup-special-sub-id-2+                #x00)
(define +setup-punch-in-point-sub-id-2+         #x01)
(define +setup-punch-out-point-sub-id-2+        #x02)
(define +setup-delete-punch-in-point-sub-id-2+  #x03)
(define +setup-delete-punch-out-point-sub-id-2+	#x04)
(define +setup-event-start-point-sub-id-2+      #x05)
(define +setup-event-stop-point-sub-id-2+       #x06)
(define +setup-xtnd-event-start-point-sub-id-2+ #x07)
(define +setup-xtnd-event-stop-point-sub-id-2+  #x08)
(define +setup-delete-event-start-point-sub-id-2+ #x09)
(define +setup-delete-event-stop-point-sub-id-2+ #x0a)
(define +setup-cue-point-sub-id-2+              #x0b)
(define +setup-xtnd-cue-point-sub-id-2+         #x0c)
(define +setup-delete-cue-point-sub-id-2+       #x0d)
(define +setup-event-name-sub-id-2+		#x0e)

(define (%make-setup-data dev subid2 fmt hr mn sc fr ff num . args)
  (with-args (args &optional xtnd)
    (make-sysex-data +non-real-time-id+ 
                     dev
                     +midi-time-code-setup-sub-id+ 
                     subid2
		     (encode-smpte-data hr mn sc fr :subframes ff :format fmt)
		     (cond ((and (list? num) (= (length num) 2))
                            (n-bit-twoscomp-p (car num) 7
                                              (< (car num) 0) #t)
                            (n-bit-twoscomp-p (cadr num) 7 
                                              (< (cadr num) 0) #t)
                            num)
                           ((and (vector? num) (= (vector-length num) 2))
                            (n-bit-twoscomp-p (vector-ref num 0) 7 
                                              (< (vector-ref num 0) 0)
                                              #t)
                            (n-bit-twoscomp-p (vector-ref num 1) 7 
                                              (< (vector-ref num 1) 0)
                                              #t)
                            num)
                           (else
                            (n-bit-bytes num 2 7 #t)))
		     (nibblize #t xtnd))))

;;; Special Setup messages
;;; 
;;; Special refers to the set-up information that affects a unit globally
;;; (as opposed to individual tracks, sounds, programs,sequences, etc.). In
;;; this case, the Special Type takes the place of the Event Number. Six
;;; are defined.  Note that types 01 00 through 03 00 ignore the event time
;;; field. (These bytes are already in LSB-first order.)
;;;
;;; NOTE: The standard proposal states "Five are defined" and that "types 01 
;;;       00 through 04 00 ignore the event time".  I reckon that's wrong.

(define +setup-special-time-code-offset-type+	   '(#x00 #x00))
(define +setup-special-enable-event-list-type+	   '(#x01 #x00))
(define +setup-special-disable-event-list-type+	   '(#x02 #x00))
(define +setup-special-clear-event-list-type+	   '(#x03 #x00))
(define +setup-special-system-stop-type+	   '(#x04 #x00))
(define +setup-special-event-list-request-type+	   '(#x05 #x00))


;;; specification of a time code offset is typically needed to synchronize
;;; different devices/media.

(define (make-time-code-offset-sysex-data hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+)
	           (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-special-sub-id-2+
		      format hr mn sc fr ff
		      +setup-special-time-code-offset-type+)))

; (make-time-code-offset-sysex-data 1 2 3 4 5 :format +smpte-format-25fps+)
;   => #(240 126 127 4 0 33 2 3 4 5 0 0 247) ; 33 = #b1 00001 = format 1, hour 1

;;; enable event list means for a slave to enable execution of events in its
;;; internal "list of events" when each one's respective SMPTE time
;;; occurs. Disable Event List means for a slave to disable execution of
;;; events in its internal "list of events", but not to erase the list.
;;; Clear Event List means for a slave to erase all events in its internal
;;; list.

(define (make-enable-event-list-sysex-data . args)
  (with-args (args &key (device-id +all-device-ids+))
    (%make-setup-data device-id +setup-special-sub-id-2+
		      0 0 0 0 0 0
		      +setup-special-enable-event-list-type+)))

(define (make-disable-event-list-sysex-data . args)
  (with-args (args &key (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-special-sub-id-2+
		      0 0 0 0 0 0
		      +setup-special-disable-event-list-type+)))

(define (make-clear-event-list-sysex-data  . args)
  (with-args (args &key (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-special-sub-id-2+
		      0 0 0 0 0 0
		      +setup-special-clear-event-list-type+)))

; (make-enable-event-list-sysex-data)
;   => #(240 126 127 4 0 0 0 0 0 0 1 0 247)
; (make-disable-event-list-sysex-data)
;   => #(240 126 127 4 0 0 0 0 0 0 2 0 247)
; (make-clear-event-list-sysex-data)
;   => #(240 126 127 4 0 0 0 0 0 0 3 0 247)

;;; system stop refers to a time when the slave may shut down. this serves
;;; as a protection against Event Starts without Event Stops, tape machines
;;; running past the end of a reel, etc.

(define (make-system-stop-sysex-data hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (%make-setup-data device-id
                      +setup-special-sub-id-2+
		      format hr mn sc fr ff
		      +setup-special-system-stop-type+)))

; (make-system-stop-sysex-data 1 2 3 4 5 :format +smpte-format-25fps+)
;   => #(240 126 127 4 0 33 2 3 4 5 4 0 247)


;;; event list request is sent by the master, and requests the slave to send
;;; all events in its list as a series of setup messages, starting from the
;;; smpte time in this message. 

(define (make-event-list-request-sysex-data hr mn sc fr ff  . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (%make-setup-data device-id +setup-special-sub-id-2+
		      format hr mn sc fr ff
		      +setup-special-event-list-request-type+)))


; (make-event-list-request-sysex-data 1 2 3 4 5 :format +smpte-format-25fps+)
;   => #(240 126 127 4 0 33 2 3 4 5 4 0 247)

;;;
;;; punch in and punch out refer to the enabling and disabling of record
;;; mode on a unit. The Event Number refers to the track to be recorded.
;;; Multiple punch in/punch out points (and any of the other event types
;;; below) may be specified by sending multiple Set-Up messages with
;;; different times.  Delete Punch In or Out deletes the matching point
;;; (time and event number) from the Cue List. 

(define (make-punch-in-point-sysex-data track hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-punch-in-point-sub-id-2+
		      format hr mn sc fr ff
		      track)))

(define (make-punch-out-point-sysex-data track hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-punch-out-point-sub-id-2+
		      format hr mn sc fr ff
		      track)))

(define (make-delete-punch-in-point-sysex-data track hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id
                      +setup-delete-punch-in-point-sub-id-2+
		      format hr mn sc fr ff
		      track)))

(define (make-delete-punch-out-point-sysex-data track hr mn sc fr ff
                                                . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-delete-punch-out-point-sub-id-2+
		      format hr mn sc fr ff
		      track)))

; (make-punch-in-point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 1 119 59 59 29 99 7 0 247)	; 33 = #b11.10111 = fmt 3, h 23
; (make-punch-out-point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 2 119 59 59 29 99 7 0 247)
; (make-delete-punch-in-point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 3 119 59 59 29 99 7 0 247)
; (make-delete-punch-out-point-sysex-data 7 23 59 59 29 99)
;   => #(240 126 127 4 4 119 59 59 29 99 7 0 247)

;;;
;;; Event Start and Stop refer to the running or playback of an event, and
;;; imply that a large sequence of events or a continuous event is to be
;;; started or stopped. The event number refers to which event on the
;;; targeted slave is to be played. A single event (ie. playback of a
;;; specific sample, a fader movement on an automated console, etc.) may
;;; occur several times throughout a given list of cues.  These events will
;;; be represented by the same event number, with different Start and Stop
;;; times.
;;;
;;; Event Start and Stop with Additional Information refer to an event (as
;;; above) with additional parameters transmitted in the Set Up message
;;; between the Time and EOX. The additional parameters may take the form of
;;; an effects unit's internal parameters, the volume level of a sound
;;; effect, etc.
;;;
;;; Delete Event Start/Stop means to delete the matching (event number and
;;; time) event (with or without additional information) from the Cue List. 

(define (make-event-start-point-sysex-data event-nr hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id
                      +setup-event-start-point-sub-id-2+
		      format hr mn sc fr ff
		      event-nr)))

(define (make-event-stop-point-sysex-data event-nr hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-event-stop-point-sub-id-2+
		      format hr mn sc fr ff
		      event-nr)))

(define (make-xtnd-event-start-point-sysex-data event-nr hr mn sc fr 
                                                ff data . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-xtnd-event-start-point-sub-id-2+
		      format hr mn sc fr ff
		      event-nr data)))

(define (make-xtnd-event-stop-point-sysex-data event-nr hr mn sc fr
                                               ff data . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id
                      +setup-xtnd-event-stop-point-sub-id-2+
		      format hr mn sc fr ff
		      event-nr data)))

(define (make-delete-event-start-point-sysex-data event-nr hr mn sc fr
                                                  ff . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-delete-event-start-point-sub-id-2+
		      format hr mn sc fr ff
		      event-nr)))

(define (make-delete-event-stop-point-sysex-data event-nr hr mn sc fr
                                                 ff . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (%make-setup-data device-id 
                      +setup-delete-event-stop-point-sub-id-2+
		      format hr mn sc fr ff
		      event-nr)))

; (make-event-start-point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 5 119 59 59 29 99 0 127 247)
; (make-event-stop-point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 6 119 59 59 29 99 0 127 247)
; (make-xtnd-event-start-point-sysex-data #x3f80 23 59 59 29 99 "test")
;   => #(240 126 127 4 7 119 59 59 29 99 0 127 4 7 5 6 3 7 4 7 247)
; (make-xtnd-event-stop-point-sysex-data #x3f80 23 59 59 29 99 "test")
;   => #(240 126 127 4 8 119 59 59 29 99 0 127 4 7 5 6 3 7 4 7 247)
; (make-delete-event-start-point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 9 119 59 59 29 99 0 127 247)
; (make-delete-event-stop-point-sysex-data #x3f80 23 59 59 29 99)
;   => #(240 126 127 4 10 119 59 59 29 99 0 127 247)

;;;
;;; cue point refers to individual event occurences, such as marking "hit"
;;; points for sound effects, reference points for editing, and so on.  Each
;;; Cue number may be assigned to a specific reaction, such as a specific
;;; one-shot sound event (as opposed to a continuous event, which is handled
;;; by Start/Stop).  A single cue may occur several times throughout a given
;;; list of cues.  These events will be represented by the same event
;;; number, with different Start and Stop times. 
;;;
;;; Cue Point with Additional Information is exactly like Event Start/Stop
;;; with Additional Information, except that the event represents a Cue
;;; Point rather than a Start/Stop Point.
;;;
;;; Delete Cue Point means to Delete the matching (event number and time)
;;; Cue Event with or without additional information from the Cue List.

(define (make-cue-point-sysex-data cue-nr hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id +setup-cue-point-sub-id-2+
		      format hr mn sc fr ff
		      cue-nr)))

(define (make-xtnd-cue-point-sysex-data cue-nr hr mn sc fr ff data . args)
  (with-args (args &key (format +smpte-format-30fps+) 
                   (device-id +all-device-ids+))
    (%make-setup-data device-id +setup-xtnd-cue-point-sub-id-2+
		      format hr mn sc fr ff
		      cue-nr data)))

(define (make-delete-cue-point-sysex-data cue-nr hr mn sc fr ff . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (%make-setup-data device-id +setup-delete-cue-point-sub-id-2+
		      format hr mn sc fr ff
		      cue-nr)))

; (make-cue-point-sysex-data 17 1 2 3 4 5)
;   => #(240 126 127 4 11 97 2 3 4 5 17 0 247)
; (make-xtnd-cue-point-sysex-data 17 1 2 3 4 5 '("test" #\a #(-1 -8)))
;   => #(240 126 127 4 12 97 2 3 4 5 17 0 4 7 5 6 3 7 4 7 0 0 1 6 15 15 8 15 247)
;                        ^^         ^^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^ ^^^^^ ^^^^
;                        #b11.00001 17   74h 65h 73h 74h  \0 61h   ffh  f8h

; (make-delete-cue-point-sysex-data 17 1 2 3 4 5)
;   => #(240 126 127 4 13 97 2 3 4 5 17 0 247)


;;;
;;; event name in additional information merely assigns a name to a given
;;; event number.  it is for human logging purposes.

(define (make-event-name-sysex-data event-nr hr mn sc fr ff name . args)
  (with-args (args &key (format +smpte-format-30fps+)
                   (device-id +all-device-ids+))
    (%make-setup-data device-id +setup-event-name-sub-id-2+
		      format hr mn sc fr ff
		      event-nr name)))

; (make-event-name-sysex-data 17 0 0 10 23 0 "test")
;   => #(240 126 127 4 14 96 0 10 23 0 17 0 4 7 5 6 3 7 4 7 0 0 247)

;;;
;;;


; Missing: 
; ========
;   - MIDI File implementation doesn't deal with Downloadable Sounds
;   - REALTIME messages may interleave sysex messages!
;   - ?? time code message??
;   - GS Standard additions
;   - DLS Level 1 support
;   - MIDI Show Control
;   - MIDI Tuning
;   - Master Balance (RPN?)

(define-generic* midi-event-data1)
(define-generic* midi-event-data2)

(define-class* <midi-event> (<event>)
  ((opcode :accessor midi-event-opcode :allocation :class))
  :name 'midi-event)

(define-method* (midi-event-data1 (obj <midi-event>))
  obj
  #f)

(define-method* (midi-event-data2 (obj <midi-event>))
  obj
  #f)

(define-class* <midi-channel-event> (<midi-event>)
  ((channel :init-value 0 :init-keyword :channel
            :accessor midi-event-channel))
  :name 'midi-channel-event)

;;; GOOPS BUG (?) redeclaring opcode for :init-value obliterates
;;; its :accessor  and :allocation settings declared by <midi-event>!

(define-class* <midi-system-event> (<midi-event>)
  ((opcode :init-value #xf0)
   (type :init-value 0 :init-keyword :type 
         :accessor midi-event-data1)
   (data :init-value #f :init-keyword :data
         :accessor midi-event-data2))
  :name 'midi-system-event)

(define-class* <midi-meta-event> (<midi-event>)
  ((opcode :init-value #xff))
  :name 'midi-meta-event)

;;;
;;; event->message 
;;; at some point soon i will get rid of messages
;;; see midi3.scm for message->event conversion
;;;

(define-method* (midi-event->midi-message (event <midi-channel-event>))
  ;; GOOPS BUG (?) declaring slots for :init-values obliterates
  ;; the :accessor and :allocation settings declared by <midi-event>!
  (values
   (make-channel-message (slot-ref event 'opcode)
                         (midi-event-channel event)
                         (midi-event-data1 event)
                         (or (midi-event-data2 event)
                             0))
   #f))

(define-method* (midi-event->midi-message (event <midi-system-event>))
  (let* ((type (midi-event-data1 event))
         (code (logior #xf0 type))
         (data (midi-event-data2 event)))
    (cond ((eq? type 0)
           (make-sysex 0 data))
          ((<= 1 type 3)                ; qframe, songpos, songsel
           (make-system-message 0 code (midi-event-data2 event)))
          (else
           (make-system-message 0 code)))))

(define-method* (midi-event->midi-message (event <midi-meta-event>))
  (let ((op (slot-ref event 'opcode)))
    (cond ((eq? op +ml-file-sequence-number-opcode+)
           (make-sequence-number (midi-event-data1 event)))
          ((eq? op +ml-file-text-event-opcode+)
           ;; pass string as first arg
           (make-text-event (midi-event-data2 event)
                            (midi-event-data1 event)))
          ;;((eq op +ml-file-midi-channel-opcode+) )
          ;;((eq op +ml-file-midi-port-opcode+) )
          ((eq? op +ml-file-eot-opcode+)
           (make-eot ))
          ((eq? op +ml-file-tempo-change-opcode+)
           ;; tempo is usecs per midi quarter note.
           (make-tempo-change (midi-event-data1 event)))
          ((eq? op +ml-file-smpte-offset-opcode+)
           (apply (function make-smpte-offset)
                  (midi-event-data1 event)))
          ((eq? op +ml-file-time-signature-opcode+)
           (make-time-signature (midi-event-data1 event)
                                (midi-event-data2 event)
                                (midi-event-data3 event)
                                (midi-event-data4 event)))
          ((eq? op +ml-file-key-signature-opcode+)
           (make-key-signature (midi-event-data1 event)
                               (midi-event-data2 event)))
          ((eq? op +ml-file-sequencer-event-opcode+)
           (make-sequencer-event (midi-event-data1 event)))
          (else
           (err "Unimplemented meta-event opcode: ~s" op)))))


