;;; Midishare testing for Gauche Scheme. See midishare-test.lisp for the
;;; explanitory comments

(load "/Lisp/midishare/midishare.scm")

(select-module midishare)

(ms:MidiShare)

(ms:MidiGetVersion)

(ms:MidiFreeSpace)

(ms:MidiCountDrivers)

(ms:MidiCountAppls)

(define *refnum* (ms:MidiOpen "Gauche"))

*refnum*

(ms:MidiCountDrivers)

(define (print-drivers )
  (format #t "~%Drivers:")
  (do ((i (ms:MidiNewMidiDriverInfos )) ; i is struct not pointer
       (d 1 (+ d 1)))
      ((> d (ms:MidiCountDrivers))
       (ms:MidiFreeMidiDriverInfos i)
       #f)
    (let ((r (ms:MidiGetIndDriver d)))
      (ms:MidiGetDriverInfos r (ptr i))
      (format #t "~%  Index=~S Refnum=~S Name:~S version=~S slots=~S"
	      d r (ms:md-name i) (ms:md-version i)
	      (ms:md-slots i)
	      ))))

(print-drivers )

(ms:MidiCountAppls)

(define (list-appls )
  (format #t "List of MidiShare client applications ~%")
  (do ((a (ms:MidiCountAppls))
       (i 0 (+ i 1)))
      ((= i a) #f)
    (let ((ref (ms:MidiGetIndAppl (+ i 1))))
      (format #t "~d : reference number ~d, name : ~s~%" 
              (+ i 1) ref (ms:MidiGetName ref)))))

(list-appls )

(ms:MidiGetNamedAppl "Gauche")

(ms:MidiGetNamedAppl "XYZ?!")

(ms:MidiSetName (ms:MidiGetNamedAppl "Gauche") "Scheme")

(list-appls )

(ms:MidiConnect *refnum* 0 -1)

(ms:MidiIsConnected *refnum* 0)

(ms:MidiIsConnected 0 *refnum*)

(define (list-dest ref1)
  (format #t "List of the destinations of '~a' (ref num = ~d) ~%" 
          (ms:MidiGetName ref1) ref1)
  (do ((n (ms:MidiCountAppls))
       (i 0 (+ i 1)))
      ((= i n) #f)
    (let ((ref2 (ms:MidiGetIndAppl (+ i 1))))
      (if (= (ms:MidiIsConnected ref1 ref2) 1)
          (format #t " --> '~a' (ref num = ~d) ~%"
                  (ms:MidiGetName ref2)  
                  ref2)))))

(list-dest *refnum* )

(define (list-src ref1)
  (format #t "List of the sources of '~a' (ref num = ~d) ~%" 
          (ms:MidiGetName ref1) ref1)
  (do ((l (ms:MidiCountAppls))
       (i 0 (+ i 1)))
      ((= i l) #f)
    (let ((ref2 (ms:MidiGetIndAppl (+ i 1))))
      (if (= (ms:MidiIsConnected ref2 ref1) 1)
          (format #t " <-- '~a' (ref num = ~d) ~%"
                  (ms:MidiGetName ref2)  
                  ref2)))))

(list-src 0 )

(define (send-note pitch)
  (let ((ev (ms:MidiNewEv typeNote))) ; ask for a new note event
    (unless (null-ptr? ev)   ; if the allocation was succesfull
      (ms:chan ev 0)                  ; 0 means channel 1
      (ms:port ev 0)                  ; set the destination port to 0
      (ms:field ev 0 pitch)           ; set the pitch field
      (ms:field ev 1 64)              ; set the velocity field
      (ms:field ev 2 1000)	           ; set the duration field to 1 second
      (ms:MidiSendIm *refnum* ev))    ; send the note immediatly
    ))

(send-note 60)

(define (send-multiple-notes n pit del)
  (let ((event (ms:MidiNewEv ms:typeNote))	; ask for a new note event
        (date (ms:MidiGetTime)))		; remember the current time
    (unless (null-ptr? event)	; if the allocation was succesful
      (ms:chan event 0)			; 0 means channel 1
      (ms:port event 0)			; set the destination port to 0
      (ms:field event 0 pit)		; set the pitch field
      (ms:field event 1 64)		; set the velocity field
      (ms:field event 2 (- del 1))	; set the duration field
      (do ((i 0 (+ i 1)))			; loop for number of events
	  ((>= i n) #f)
        (ms:MidiSendAt *refnum*		; send a copy of the original note
		       (ms:MidiCopyEv event)	 
		       (+ date (* i del))))
      (ms:MidiFreeEv event) )		; dispose the original note
    ))

(send-multiple-notes 10 72 1000)

(begin (send-multiple-notes 6 60 800)
       (send-multiple-notes 8 72 600))

(define (transform transpose delay stopkey)
  (ms:MidiConnect *refnum* 0 -1)           ; connect MCL to MidiShare
  (ms:MidiConnect 0 *refnum* -1)           ; connect MidiShare to MCL
  (ms:MidiFlushEvs *refnum*)               ; flush old events in the rcv fifo
  (do ()
      ((eq? stopkey #t) #t)
    (do ((types (list ms:typeNote ms:typeKeyOn ms:typeKeyOff))
	 (event (ms:MidiGetEv *refnum*) (ms:MidiGetEv *refnum*)))
	((null-ptr? event) #f)     ; in the rcv fifo
      ;; note, keyOn and KeyOff events
      (if (member (ms:evType event) types)
	  (if (equal? stopkey (ms:pitch event))
	      (begin (set! stopkey #t)
		     (ms:MidiFlushEvs *refNum*))
	      (begin
	       (ms:pitch event (+ transpose (ms:pitch event))) ; are transposed
	       (ms:date event (+ delay (ms:date event))) ;   delayed
	       (ms:MidiSend *refnum* event))) ;   and sent.
          (ms:MidiFreeEv event)))) ; other events are deleted
  (ms:MidiConnect 0 *refnum* 0)    ; break the connection 	
  )

(transform 12 1000 61)



