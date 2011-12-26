;; load gauche ffi

(load "/Lisp/portmidi/portmidi.scm")
(select-module portmidi)

;; initialize portmidi lib

(pm:PortMidi)

;; timer testing

(pt:Start )
(pt:Started)
(pt:Time)
(pt:Time)
(pt:Time)

;; device testing

(pm:CountDevices)
(pm:GetDeviceInfo )
(define inid (pm:GetDefaultInputDeviceID ))
(pm:GetDeviceInfo inid)
(define outid (pm:GetDefaultOutputDeviceID ))
(pm:GetDeviceInfo outid)

;; output testing

(define outid 5)      ; 5 = my midi synth
(define outst (pm:OpenOutput outid 128 1000))
(pm:GetDeviceInfo outid) ; :OPEN should be #t

;; message tests

(define (pm m)
  (format #t "#<message :op ~d :ch ~d :data1 ~d :data2 ~d>"
          (ash (logand (pm:Message.status m) #xf0) -4)
          (logand (pm:Message.status m) #x0f)
          (pm:Message.data1 m)
          (pm:Message.data2 m))
  (values))
(define on (pm:Message #b10010000 60 64))
(pm on)
(pm:Message.status on)
(logand (ash (pm:Message.status on) -4) #x0f)
(pm:Message.data1 on)
(pm:Message.data2 on)

(pm:WriteShort outst (+ (pt:Time) 100) on)

(define off (pm:Message #b10000000 60 64))
(pm off)
(pm:WriteShort outst (+ (pt:Time) 100) off)
(pm:Close outst)

;; event buffer testing

(define buff (pm:EventBufferNew 8))
(do ((e 8)
     (i 0 (+ i 1)))
    ((not (< i e)) #t)
  (let ((x (pm:EventBufferElt buff i)))
     (pm:Event.message x (pm:Message #b1001000 (+ 60 i) (+ 100 i)))
     (pm:Event.timestamp x (* 1000 i))))

(do ((e 8)
     (l (list))
     (i 0 (+ i 1)))
    ((not (< i e)) (reverse! l))
  (let ((x (pm:EventBufferElt buff i)))
    (set! l (cons (list (pm:Event.timestamp x)
			(pm:Message.data1 (pm:Event.message x))
			(pm:Message.data2 (pm:Event.message x)))
		  l))))

(pm:EventBufferFree buff)

;; input testing -- requires external midi keyboard

(pm:GetDeviceInfo )
(define inid 2)       ; 2 = input port of my external keyboard
(define inst (pm:OpenInput inid 0)) 
(pm:GetDeviceInfo inid) ; :OPEN should be T
(pm:SetFilter inst pm:filt-realtime) ; ignore active sensing etc.
(pm:Poll inst)

;;
;; ...play midi keyboard, then ...
;;

(pm:Poll inst)
(define buff (pm:EventBufferNew 32))
(define num (pm:Read inst buff 32))
(pm:EventBufferMap (lambda (a b) b  (pm a))
                   buff num)
(pm:Poll inst)
(pm:EventBufferFree buf)

;;; testing recv

(load "/usr/local/lisp/portmidi/portmidi.scm")
(load "/usr/local/lisp/portmidi/portmidi-recv.scm")

(use portmidi)


(load "/usr/local/lisp/rts/rts.scm")

*** ERROR: cannot find file "gauche-scheduler.scm" in *load-path* ("/usr/local/lisp/cm/src/" "/usr/local/share/gauche/site/lib" "/usr/local/share/gauche/0.8.7/lib")
Stack Trace:
_______________________________________

;;load by hand

(load "/usr/local/lisp/rts/gauche-scheduler.scm")
(load "/usr/local/lisp/rts/cm-scheduler.scm")

(use rts)




(pm:GetDeviceInfo )

(define mp (portmidi-open :latency 0 :input 0 :output 5))

gosh> *** ERROR: unbound variable: pm:PortMidi
Stack Trace:
_______________________________________
  0  (pm:PortMidi)
        At line 79 of "/usr/local/lisp/cm/src//pm.scm"
  1  args

  2  args

  3  (portmidi-open :latency 0 :input 0 :output 5)
        At line 12 of "(stdin)"

;;reload 
(load "/usr/local/lisp/cm/src/pm.scm")

;;try again
(define mp (portmidi-open :latency 0 :input 0 :output 5))



(rts mp)
(recv mp)


(define (sprout1 num key vel)
  (process repeat num 
	   output (new midi :keynum (+ key (* 12 (between -3 3))) :amplitude vel :time (now) :duration .8)
	   wait .25))


(recv-set! mp (lambda (mess)
                (if (note-on-p mess)
                    (sprout (sprout1 12 (note-on-key mess) (note-on-velocity mess))))))


gosh> gosh> *** ERROR: exact integer required, but got #<c-ptr:<c-void> 0x161e000>
Stack Trace:
_______________________________________
  0  (pm:Message.status pmm)
        At line 226 of "/usr/local/lisp/cm/src/pm.scm"
  1  (pm-message->midi-message mess)
        At line 37 of "(stdin)"
  2  (map (lambda (c-type pointer) (scm-cast (deref c-type pointer))) ( ...
        At line 791 of "/usr/local/share/gauche/site/lib/c-wrapper/c-types.scm"
  3  (cast (ret-type-of fp-class) (apply proc (map (lambda (c-type poin ...
        At line 790 of "/usr/local/share/gauche/site/lib/c-wrapper/c-types.scm"



(recv-stop mp)


;; round 3 tests of recv

; in gauche-recv.scm change the path to the portmidi directory

;; in these test cm is already loaded.

(use-system :portmidi)

(load "/usr/local/lisp/portmidi/gauche-recv.scm")
(load "/usr/local/lisp/portmidi/cm-recv.scm")


(pm:GetDeviceInfo )

(define mp (portmidi-open :latency 0 :input 0 :output 5))


gosh> *** ERROR: unbound variable: pm:PortMidi
Stack Trace:
_______________________________________
  0  (pm:PortMidi)
        At line 79 of "/usr/local/lisp/cm/src//pm.scm"
  1  args

  2  args

  3  (portmidi-open :latency 0 :input 0 :output 5)
        At line 17 of "(stdin)"

;;reload the pm.scm file

(load "/usr/local/lisp/cm/src/pm.scm")

;;now this works. at the tine pm.scm was loaded. pm:PortMidi was undefined

(define mp (portmidi-open :latency 0 :input 0 :output 5))


(recv mp)

gosh> ; recv running on stream #<portmidi-stream "midi-port.pm" (in:0 out:5)>

(recv-set! mp (lambda (mess)
                (if (note-on-p mess)
                    (format #t "note on~%"))))
;;play a note

gosh> *** ERROR: exact integer required, but got #<c-ptr:<c-void> 0x7fc000>
Stack Trace:
_______________________________________
  0  (pm:Message.status pmm)
        At line 226 of "/usr/local/lisp/cm/src/pm.scm"
  1  (pm-message->midi-message m)
        At line 47 of "/usr/local/lisp/portmidi/cm-recv.scm"
  2  (map (lambda (c-type pointer) (scm-cast (deref c-type pointer))) ( ...
        At line 791 of "/usr/local/share/gauche/site/lib/c-wrapper/c-types.scm"
  3  (cast (ret-type-of fp-class) (apply proc (map (lambda (c-type poin ...
        At line 790 of "/usr/local/share/gauche/site/lib/c-wrapper/c-types.scm"



(recv-set! mp #f)


(recv-set! mp (lambda (mess)
                (if (note-on-p mess)
                    (format #t "keynum: ~s velocity: ~s ~%"
                            (note-on-key mess) (note-on-velocity mess)))))

(recv-stop mp)















