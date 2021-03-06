(provide 'snd-track-colors.scm)

(if (not (provided? 'snd-rgb.scm)) (load-from-path "rgb.scm"))

(catch 'no-such-track
       (lambda ()
	 (do ((i 1 (1+ i)))
	     ((= i 45))
	   (if (not (track? i))
	       (let ((val (make-track)))
		 (if (< val i)
		     (do ((j (1+ val) (1+ j)))
			 ((= j val))
		       (make-track))))))
	 (set! (track-color 1) orange)
	 (set! (track-color 2) blue)
	 (set! (track-color 3) white)
	 (set! (track-color 4) gold)
	 (set! (track-color 5) yellow4)
	 (set! (track-color 6) antique-white)
	 (set! (track-color 7) pink)
	 (set! (track-color 8) orange-red)
	 (set! (track-color 9) dark-salmon)
	 (set! (track-color 10) coral)
	 (set! (track-color 11) slateblue1)
	 (set! (track-color 12) lightskyblue1)
	 (set! (track-color 13) orchid)
	 (set! (track-color 14) slategray1)
	 (set! (track-color 15) rosy-brown)
	 (set! (track-color 16) dark-sea-green)
	 (set! (track-color 17) sandy-brown)
	 (set! (track-color 18) tomato)
	 (set! (track-color 19) plum)
	 (set! (track-color 20) dark-orange)
	 (set! (track-color 21) black)
	 (set! (track-color 22) black)
	 (set! (track-color 23) black)
	 (set! (track-color 24) black)
	 (set! (track-color 25) black)
	 (set! (track-color 26) black)
	 (set! (track-color 27) black)
	 (set! (track-color 28) black)
	 (set! (track-color 29) black)
	 (set! (track-color 30) black)
	 (set! (track-color 31) black)
	 (set! (track-color 32) black)
	 (set! (track-color 33) black)
	 (set! (track-color 34) black)
	 (set! (track-color 35) black)
	 (set! (track-color 36) black)
	 (set! (track-color 37) black)
	 (set! (track-color 38) black)
	 (set! (track-color 39) black)
	 (set! (track-color 40) black)
	 (set! (track-color 41) black)
	 (set! (track-color 42) black)
	 (set! (track-color 43) black)
	 (set! (track-color 44) black)
	 (set! (track-color 45) black))
       (lambda args #t))

	 
