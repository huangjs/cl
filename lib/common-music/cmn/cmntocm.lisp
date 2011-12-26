;;;  cmntocm.lisp (5/15/'92)
;;;
;;;  A simple suite of lisp functions that, when called by translate-file,
;;;  will take a simple Common Music Notation (.cmn) file as input,  and return
;;;  a Common Music (.cm) file which can be immediately converted into a 
;;;  NeXT scorefile and played by the NeXT MusicKit
;;;
;;;  by Malcolm M. Sanders (mms@aretha.jax.org)
;;;
;;;  Usage:  Assuming piece.cmn exists in the current working directory,
;;;          then,
;;;                (translate-file "piece" 'key)
;;;          where key is one of a, bf, c, d, e,ef, g   (for now!)
;;;          will put piece.cm in the same directory.  If the lisp
;;;          image also contains an image of CM, then :ld piece.cm
;;;          will produce and play a scorefile from piece.cm.
;;;
;;; Limitations:
;;;                  - Only single staff pieces can be translated.
;;;                    But it does recognize and play chords in a
;;;                    single staff.
;;;                  - Repeat bars are ignored
;;;                  - Only does the keys, a-major,bflat-major,
;;;                    c-major,d-major,e-major,eflat-major for now
;;;                  
(defun astrip-cmn (q)  ; strips out unwanted atoms in cmn list
  (loop for thing in q
	if (not(atom thing))
	     collect thing
	else
	  if (and (not (eq thing 'bar)) (<= (length(string thing)) 3))
	    collect thing 
	  else if (eq thing 'eighth-rest)
		append '(r e)
	       else  if (eq thing 'quarter-rest)
	          append  '(r q)
		     ))

(defun lstrip-cmn (q)  ;burrows into cmn sublists and saves chords,else discard
   (loop for thing in q
	 if (atom thing)
	     collect thing
	 else
	 if (eq (car thing) 'chord)
	     collect (cdadr thing)
	      and collect (caddr thing)
	     else if (and (<= (length(string (car thing))) 3)
			  (not (eq (car thing) 'bar))
			  (not (eq (car thing) 'key)))
	      collect (car thing)
	      and collect (cadr thing)
))

(defun down-to-cmn (q)  ;strips down to (cmn ...)
	(loop for thing in q
	    when (and (not(atom thing)) (eq (car thing) 'cmn))
	    append (cdr thing)))

(defun down-to-staff (q) ;strips (cmn ... ) down to (staff ...)
	(loop for newthing in q
		when (and (not(atom newthing)) (eq (car newthing) 'staff))
		append (cdr newthing)
	))

;;; following functions do the right key changes
;;; other keys to be added as needed
(defun gkey (notes) 
	(loop for note in notes
		when (eq  note 'f4)
		collect 'fs4
		else when (eq note 'f5)
		collect 'fs5
		else
		collect note))
(defun dkey (notes)  
	(loop for note in notes
		when (eq  note 'f4)
		collect 'fs4
		else when (eq note 'f5)
		collect 'fs5
		else
		when (eq  note 'c4)
		collect 'cs4
		else when (eq note 'c5)
		collect 'cs5
		else
		collect note))
(defun akey (notes)  
	(loop for note in notes
		when (eq  note 'f4)
		collect 'fs4
		else when (eq note 'f5)
		collect 'fs5
		else
		when (eq  note 'c4)
		collect 'cs4
		else when (eq note 'c5)
		collect 'cs5
		else when (eq note 'g4)
		collect 'gs4
		else when (eq note 'g5)
		collect 'gs5 
		else when (eq note 'g3)
		collect 'gs3
		else
		collect note))
(defun ekey (notes)  
	(loop for note in notes
		when (eq  note 'f4)
		collect 'fs4
		else when (eq note 'f5)
		collect 'fs5
		else
		when (eq  note 'c4)
		collect 'cs4
		else when (eq note 'c5)
		collect 'cs5
		else when (eq note 'g4)
		collect 'gs4
		else when (eq note 'g5)
		collect 'gs5 
		else when (eq note 'g3)
		collect 'gs3
		else when (eq note 'd4)
		collect 'ds4
		else when (eq note 'd5)
		collect 'ds5
		else
		collect note))
(defun fkey (notes) 
	(loop for note in notes
		when (eq  note 'b3)
		collect 'bf3
		else when (eq note 'b4)
		collect 'bf4
		else when (eq note 'b5)
		collect 'bf5
		else
		collect note))
(defun bfkey (notes) 
	(loop for note in notes
		when (eq  note 'b3)
		collect 'bf3
		else when (eq note 'b4)
		collect 'bf4
		else when (eq note 'b5)
		collect 'bf5
		when (eq  note 'e4)
		collect 'ef4
		when (eq  note 'e5)
		collect 'ef5
		else
		collect note))
(defun efkey (notes) 
	(loop for note in notes
		when (eq  note 'b3)
		collect 'bf3
		else when (eq note 'b4)
		collect 'bf4
		else when (eq note 'b5)
		collect 'bf5
		when (eq  note 'e4)
		collect 'ef4
		when (eq  note 'e5)
		collect 'ef5
		when (eq  note 'a3)
		collect 'af3
		when (eq  note 'a4)
		collect 'af4
		when (eq  note 'a5)
		collect 'af5
		else
		collect note))
(defun lpitches (q)  ;contained in lsplit-it
  (loop for thing in q by #'cddr
	if (atom thing)
	  collect thing
	else
	  collect '\[
	  and append thing
	  and collect '\]
))
(defun ldurations (q)  ;contained in lsplit-it
  (loop for thing in (cdr q) by #'cddr
	collect thing
))

(defun lsplit-it (q)
  (loop for note in q by #'cddr
	for duration in (cdr q) by #'cddr
	if (atom note)
	  collect note into pitches
	  and collect duration into durations
	else
	  collect '\[ into pitches
	  and append note into pitches
	  and collect '\] into pitches
	  and collect duration into durations
	finally
	(return (cons pitches durations))
))	

(defun cmntocm (list)
  (lsplit-it (astrip-cmn (lstrip-cmn (down-to-staff list)))))

(defun translate-file (cmnfile key)
  (setq *cm-readtable* (copy-readtable))
 (with-open-file (inlist (concatenate 'string cmnfile ".cmn")) 
		(loop while (setq lists (read inlist))
		   when (eq (car lists) 'cmn)
			do (return (setq music (cmntocm lists)))))
  (cond ((eq key 'g)
	 (setq music (cons (gkey (car music)) (cdr music))))
	((eq key 'd)
	 (setq music (cons (dkey (car music)) (cdr music))))
	((eq key 'a)
	 (setq music (cons (akey (car music)) (cdr music))))
	((eq key 'e)
	 (setq music (cons (ekey (car music)) (cdr music))))
	((eq key 'f)
	 (setq music (cons (fkey (car music)) (cdr music))))
	((eq key 'bf)
	 (setq music (cons (bfkey (car music)) (cdr music))))
	((eq key 'ef)
	 (setq music (cons (efkey (car music)) (cdr music)))))
  (with-open-file (outlist (concatenate 'string cmnfile ".cm")
			   :direction :output :if-exists :supersede)
		  (pprint (list 'in-syntax `:musicKit) outlist)
		  (pprint (list 'defscorefile (list 'pathname cmnfile)	
			(list 'with-part 'PluckPoly 
			      '(sustain .1 bright .2 amp 
				    3.0 print-once '(sustain))
		  (list 'setf 'freq (list 'item (cons 'notes (car music))
				   ':kill 't))
		  (list 'unless-chording (list 'setf 'rhythm ( list 'item
				  (cons 'rhythms (append (cdr music) 
					'(tempo 240)))))))) outlist))
  (setq *readtable* *cm-readtable*))



