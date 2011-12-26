(in-package :cmn)

;;; Handle arbitarily nested rhythmic structures
;;;
;;; (rqq rhythm-info &rest objects)
;;;    applies the rhythmic structure of rhythm-info to the objects (cmn style input)
;;;
;;; (rqq (n (R1 R2 ...)) &rest), n=number of beats, Ri either int or list of ints
;;;   (rqq (1 (1 1 1)) &rest) => 1 beat divided into triplets
;;;   (rqq (2 (1 1 1 1 1)) &rest) => 5 in time of 2 (4?)
;;;   (rqq (1 (1 5 3)) &rest) => 9-tuplet in 1 beat
;;;   (rqq (1 (1 (2 (1 2 2)))) &rest) => 1 beat, triple division (eighth+quarter), 
;;;       second 2 of triplet divided as quintuplet 1 2 2
;;;
;;; the &rest arg contains all the associated notes in the pattern
;;; for example:
;;; (cmn treble c4 q (rqq '(1 (1 1 1)) c4 staccato c4 c4) g4 q) 
;;;
;;; the result of rqq is an engorged list containing the newly modified note data.
;;; this is based on cmn's beat-subdivision and explicit beams.
;;;
;;; with many improvements thanks to Gerard Assayag

#|
(cmn treble 
     (c4 (rq 1/3) (begin-beam (explicit-beams (list (list 0 1 1 0) (list 0 1 0 0) (list 0 0 0 0)))))
     (c4 (rq 2/15)) (c4 (rq 4/15)) (c4 (rq 4/15) (end-beam)))

(cmn treble 
     (c4 te 
	 (setf hi (beat-subdivision- (subdivision (text "7:3" (font-name "Times-Bold") (font-scaler .4))) (dy .5)))
	 (setf ho (beat-subdivision- (subdivision (text "2:3" (font-name "Times-Bold") (font-scaler .3))))))
     (c4 te (-beat-subdivision- hi) 
	    (-beat-subdivision ho))
     (c4 te (-beat-subdivision hi)))
|#

(defun pow2-p (n)
  (cond 
   ((zerop n) nil)
   ((= n 1) t)
   ((zerop (mod n 2)) (pow2-p (/ n 2)))))

(defun flattenize (l)
  (labels ((flatten* (l)
	     (if (null l) nil
	       (if (atom (first l)) (list l)
		 (cons (second l) (flatten* (first l)))))))
    (reverse (flatten* l))))

;;; GA. we give the subdivision context to how-many-dots. This solves many missing-dot bugs.
(defun local-how-many-dots (rat &optional subdivisions)
  (let* ((ratio (if subdivisions (if (ratiop rat) rat (/ (first (ratify rat)) (second (ratify rat))))))
         (num (if ratio
		  (/ (* (numerator ratio) 
			(/ (apply #'* (mapcar 'fifth subdivisions)) (denominator ratio)))
		     (apply #'* (mapcar 'fourth subdivisions)))
		(if (ratiop rat) (numerator rat) (first (ratify rat))))))
    (case num
      ((3 6 12 24) 1)
      ((7 14 28) 2)
      ((15 30) 3)
      (31 4)
      (t 0))))

(defun local-rq (rat &optional subdivisions)
  (make-instance 'rhythm 
   :dots (local-how-many-dots rat (and subdivisions (flattenize subdivisions)))
   :flags (how-many-flags rat)
   :quarters rat))

(defun fixup-tied-rhythms (rhys)
  ;; add the extra rhyhtm (5->4+1, 9->8+1)
  (let ((r nil))
    (loop for rhy in (second rhys) do
      (if (listp rhy)
	  (let ((nr (fixup-tied-rhythms rhy)))
	    (setf r (append r (list nr))))
	(case rhy
	  (5 (setf r (append r (list 4 1))))
	  (9 (setf r (append r (list 8 1))))
	  (10 (setf r (append r (list 8 2))))
	  (11 (setf r (append r (list 8 3))))
	  (13 (setf r (append r (list 12 1))))
	  (17 (setf r (append r (list 16 1))))
	  (18 (setf r (append r (list 16 2))))
	  (19 (setf r (append r (list 16 3))))
	  (20 (setf r (append r (list 16 4))))
	  (21 (setf r (append r (list 15 6))))
	  (22 (setf r (append r (list 16 6))))
	  (23 (setf r (append r (list 16 7))))
	  (25 (setf r (append r (list 24 1)))) ;;; GA this one does not seem to work...
	  (t  (setf r (append r (list rhy)))))))
    (list (first rhys) r)))

(defun get-ties (rhys)
  ;; check for 5's and 9's -- these need to be handled specially since music notation doesn't have a "half-dot"
  (let ((ties nil))
    (loop for rhy in rhys do
      (if (listp rhy)
	  (let ((nt (get-ties (second rhy))))
	    (loop for r in nt do (push r ties)))
	(if (= rhy 5) (push :half ties)
	  (if (member rhy '(9 10 11 13 17 18 19 20 21 22 23 25 26 27 29)) (push :quarter ties)
	    (push :none ties)))))
    (nreverse ties)))

(defun massage-rhys (urhys notes)
  ;; look for 5's and 9's, if found fixup the various lists (copying the note and adding the tie etc)
  (let* ((rhys (second urhys))
	 (tsd nil)
	 ;(total-beats (loop for rhy in rhys sum (if (listp rhy) (first rhy) rhy)))
	 (ties (get-ties rhys)))
    (if (or (not ties) (every #'(lambda (n) (eq n :none)) ties))
	(values urhys notes nil)
      (let ((new-rhys (fixup-tied-rhythms urhys))
	    (new-notes nil))
	;; need to turn 5's into 4+1, 9's into 8+1, copy the corresponding note and add begin/end-tie
	;; the new rhythm list is still structured but the new note list is flat
	(loop for n in notes and nt in ties do
	  (if (eq nt :none)
	      (progn  
		(push n new-notes)
		(push nil tsd))
	    (let ((n1 (audify n (rq (/ 1 (denominator (quarters n))))))) ;copy original but change duration
	      (push n new-notes)
	      (push n1 new-notes)
	      (push :bt tsd)	       ;begin/end tie needs to wait (fixup is immediate so needs to be in order)
	      (push :et tsd))))
	(values new-rhys (nreverse new-notes) (nreverse tsd))))))
		 
(defun gbd (dur rhys depth)
  ;; return duration, beaming and subdivision marking info
  ;; duration comes back as a flattened list of ratios
  ;; beaming and subdivision info comes back as a list of list of lists annotating the stucture
  ;; (we use the structure in the beaming to cut back to the top beam at structure boundaries)
  (let* ((total-beats (loop for rhy in rhys sum (if (listp rhy) (first rhy) rhy)))
	 (ratios nil)
	 (subs nil)
	 (len (length rhys))
	 (sublab (gentemp "SUB-"))
	 (substate :s))
    (loop for rhy in rhys and j from 0 do
      (if (= j (1- len)) (setf substate :e))
      (if (listp rhy)
	  (multiple-value-bind
	      (rats sbs)
	      (gbd (first rhy) (second rhy) (1+ depth))
	    (loop for r in rats do (push (/ (* dur r) total-beats) ratios))
	    ;; merge sub lists
	    (let ((slen (length sbs)))
	      (loop for s in sbs and k from 0 do
		(push (list s
			    (list (if (and (eq substate :e) (< k (1- slen))) :i substate)
				  sublab depth dur total-beats)) subs)
		(if (eq substate :s) (setf substate :i)))))
	(progn
	  (push (/ (* dur rhy) total-beats) ratios)
	  (push (list substate sublab depth dur total-beats) subs)))
      (setf substate :i))
    (values (nreverse ratios) (nreverse subs))))

(defun get-max-depth (subs)
  ;; can be fooled if the bottom subdivision is flushed because it is silly (3:3 or whatever)
  (loop for sub in subs maximize 
    (if (listp (first sub))
	(get-max-depth sub)
      (third sub))))

(defun translate-subdivision (sub fulldepth)
  ;; make subdivision info, take into account nesting level of brackets etc
  (let ((im (integer-multiple (fourth sub) (fifth sub))))
    (when (not (and im (pow2-p im))) ;;; GA this seems to be the general test
      ;; it's not something silly like 3:3 or relatively obvious like 6:3
      (if (eq (first sub) :s) 
        (set (second sub)              
             (beat-subdivision-
              (subdivision (text (if (= (fourth sub) 1) 
				     ;;use only the numerator if denominator is 1 (3:2 might  also be simplified)
				     (format nil "~D" (fifth sub))
				   ;; next 2 added 14-June-00 -- this is a bottomless pit...
				   (if (< (* 4 (fourth sub)) (fifth sub))
				       (format nil "~D:~D" (fifth sub) (* 4 (fourth sub)))
				     (if (< (* 2 (fourth sub)) (fifth sub))
					 (format nil "~D:~D" (fifth sub) (* 2 (fourth sub)))
				       (format nil "~D:~D" (fifth sub) (fourth sub)))))
                                 (font-name "Times-Bold")
                                 ;; nested groups use smaller fonts
                                 (font-scaler (- .4 (* .05 (third sub))))))
              ;; place nested subdivision labels (and brackets) within the outer ones
              ;; this code currently assumes the brackets are above the notes
              (dy (+ -.05 (* .45 (- fulldepth (third sub)))))))
        (if (eq (first sub) :e)
          (-beat-subdivision (symbol-value (second sub)))
          (-beat-subdivision- (symbol-value (second sub))))))))

(defun get-subdivs (subs fulldepth)
  ;; create subdivision arg list (later applied to notes)
  (let ((sublist nil))
    (if (listp (first subs))
	(progn
	  (loop for sub in subs do
	    (if (listp (first sub))
		(setf sublist (append (get-subdivs sub fulldepth) sublist))
	      (push (translate-subdivision sub fulldepth) sublist)))
	  (nreverse sublist))
      (list (translate-subdivision subs fulldepth)))))

(defun get-struct-position (subs)
  ;; are we at a structure boundary
  (let ((res nil))
    (if (listp (first subs))
	(loop for sub in subs while (not res) do
	  (if (listp (first sub))
	      (setf res (get-struct-position sub))
	    (setf res (if (eq (first sub) :s) :s (if (eq (first sub) :e) :e)))))
      (setf res (if (eq (first subs) :s) :s (if (eq (first subs) :e) :e))))
    res))

(defun get-explicit-beams (dur rhys subs)
  (declare (ignore dur))
  ;; make beaming decsions based on flags in rhys and structure boundaries in subs
  (let ((fb nil)
	(rb nil)
	(lb nil)
	(pf0 nil)
	(lastfb 0))
	;(base-flags (how-many-flags dur))
    (loop for rat0 in rhys and rat1 in (cdr rhys) and sub0 in subs do
      ;; (flags rat) = rq's idea of beams needed on current note
      (let* ((f0 (flags rat0))
	     (f1 (flags rat1))
	     (uf0 f0) (uf1 f1)
	     (minf (min f0 f1))
	     (s0 (get-struct-position sub0)))
	(if (> minf 0) 
	    (if (not (eq s0 :e))
		(progn
		  (push minf fb)
		  (decf f0 (max lastfb minf))
		  ;; bug:
		  ;;       rqq drops plb flag in: (cmn staff (staff-lines 1)(meter 4 4) (rqq '(1 (4 1 2)) e4 e4 e4))
		  ;;       the flag is ok if we check for cadr fb > 0 here, but that breaks other stuff
		  (decf f1 minf))
	      (progn
		(push 1 fb)
		(decf f0 (max 1 lastfb))
		(decf f1 1)))
	  (progn
	    (push 0 fb)
	    (decf f0 lastfb)))
	(if (> f0 0)
	    (if (or (eq s0 :s) (> uf1 uf0))
		(progn
		  (push f0 rb)
		  (setf f0 0)
		  (push 0 lb))
	      (if (or (eq s0 :e) (> uf0 uf1))
		  (progn
		    (push f0 lb)
		    (setf f0 0)
		    (push 0 rb))))
	  (progn
	    (push 0 rb)
	    (push 0 lb)))
	(setf lastfb (first fb))
	(setf pf0 f1)))
    (push 0 fb)
    (push 0 rb)
    (push pf0 lb)
    (list (nreverse fb) (nreverse rb) (nreverse lb))))

(defun count-notes (notes)
  ;; scan rqq objects list for actual notes/rests/chords
  (let ((ctr 0))
    (loop for n in notes do
      (if (or (note-p n) (chord-p n) (rest-p n)) (incf ctr)))
    ctr))

(defun rqq (urhythm-info &rest note-data)
  ;; make the list of rhythms and marks, build the explicit beaming list,
  ;; run down &rest arg parcelling out the new data, return the modified data list
  ;; for now assume nothing fancy is in the data list (i.e. no engorge or section or whatever)
  (let ((new-staff (make-instance 'staff)))
    (multiple-value-bind
      (unotes actions local-brace)
      (notify new-staff note-data)
      ;; turn the &rest argument list into a canonical list of notes and so on
      ;;   that is, c4 staccato (g4) c4 becomes (c4 staccato) (g4) (c4)
      ;;   we can then edit the objects using ur-note ur-rest ur-chord etc
      (declare (ignore actions local-brace))
      ;; now look for special 5 and 9 cases where we have to insert the tied note for the "half dot"
      (multiple-value-bind 
        (rhythm-info notes ties)
        (massage-rhys urhythm-info unotes)
	(let ((len (count-notes notes))) 
	  (multiple-value-bind 
            (rats subs) 
            (gbd (first rhythm-info) (second rhythm-info) 0) 
	    (if (/= len (length rats)) 
              (warn "(rqq '~A~{ ~(~A~)~}): ~D notes but ~D rhythms" 
                    rhythm-info (mapcar #'identify note-data) len (length rats)))
	    (let* ((rhys (mapcar #'local-rq rats subs))  ;;; GA we pass subs to rq 
		   (beams (get-explicit-beams (first rhythm-info) rhys subs))
		   (full-depth (get-max-depth subs))
		   (note-ctr 0))
	      (loop for n in notes and s in subs do 
	        (if (or (note-p n) (chord-p n) (rest-p n))
		    (let* ((rhy (pop rhys))
			   (beam (if (= note-ctr 0) (begin-beam (explicit-beams beams))
				   (if (= note-ctr (1- len)) (end-beam))))
			   (args (append (list rhy) 
					 (get-subdivs s full-depth) 
					 (list beam)
					 (if ties
					     (let ((nt (pop ties)))
					       (if (eq nt :bt) (list (begin-tie))
						 (if (eq nt :et) (list (end-tie)))))))))
		      (incf note-ctr) 
		      (if (note-p n)  (apply #'ur-note n args) 
			(if (rest-p n)  (let ((obj (apply #'ur-rest n args))) (setf (odb-duration obj) (quarters obj)) obj)
			  (apply #'ur-chord n args) ))))))))
	(engorge notes)))))

#|
  (cmn treble c4 q (rqq '(1 (1 1 1)) c4 staccato c4 c4) g4 q) 
  (cmn treble (rqq '(1 (1 (2 (1 1 1 1 1)))) c4 staccato c4 c4  c4 c4 c4))
  (cmn treble (rqq '(1  (1 (1 (1 2)) 1)) c4 staccato c4 c4 c4) 
              (rqq '(1 ((1 (2 1)) 1 1)) c4 staccato c4 c4 c4) 
	      (rqq '(1 (1 1 (1 (2 1)))) c4 staccato c4 c4 c4))
  (cmn treble c4 q (rqq '(1 (1 1 1 2)) c4 staccato c4 c4 c4) g4 q) 
  (cmn treble (rqq '(1  (1 (1 (1 2)) 1/2 1/2)) c4 staccato c4 c4 c4 c4))
  (cmn treble (rqq '(1  (1 (1 (1 2)) 3/4 1/4)) c4 staccato c4 c4 c4 c4)) 
  (cmn treble (rqq '(1 (2 3 2 3 2)) c4 c4 c4 c4 c4)) ;testing dots
  (cmn treble (rqq '(1 (2 3 2 3 3)) c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 (4 3 2 3 4)) c4 c4 c4 c4 c4))
  (cmn treble (rqq '(2 (4 3 2 3 4)) c4 c4 c4 c4 c4)) ;testing auto-tie over-ride
  (cmn treble (rqq '(3 (4 3 2 3 4)) c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 (1 1 1)) c4 c4 c4) (rqq '(2 (1 1 1 1 1)) c4 c4 c4 c4 c4) (rqq '(1 (1 (2 (1 2 2)))) c4 c4 c4 c4))
  (cmn treble (rqq '(1 ((1 (1 2)) (1 ((3 (1 2 4)) 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 (1 (1 (1 1 1)) 7/8 1/8 (2 (1 1 1 (2 (1 1 1)))))) g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4))
  (cmn treble (rqq '(1 ((1 (1 1 1)) (1 (1 1 1)) (1 (1 1 1)))) g4 g4 g4 g4 g4 g4 g4 g4 g4 )) ;test beam groupings
  (cmn treble (rqq '(2 ((1 (1 1 1)) (1 (1 2)) (1 (1 1 1)))) g4 fs4 sixteenth-rest (chord c4 ef4) eighth-rest af4 bf4 gf4))
  (cmn treble (rqq '(2 ((1 (1 1 1 2)) (1 (1 2)) (1 (1 2 4)))) g4 fs4 sixteenth-rest en4 (chord c4 ef4) eighth-rest af4 bf4 gf4))  
  (cmn treble (rqq '(1 ((1 (1 2)) (1 ((3 (1 1 1)) 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 ((1 (1 2)) (1 ((3 (1 2 3)) 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(5 (1 (1 (1 2)) 1 1)) c4 c4 c4 c4 c4)) ;test flagged note in quarter
  (cmn treble (rqq '(1 (2 5)) c4 c4 c4)) ;test auto-tie
  (cmn (size 18)                         ;test overall spacing
    (staff treble (rqq '(2 ((1 (1 1 1)) (1 (1 2)) (1 (1 1 1)))) g4 fs4 sixteenth-rest (chord c4 ef4) eighth-rest af4 bf4 gf4))
    (staff treble (rqq '(1 (1 1 1)) c4 c4 c4) (rqq '(1 (1 (2 (1 2 2)))) c4 c4 c4 c4)) 
    (staff treble c4 e c4 e c4 e c4 e c4 q))
  (cmn treble (rqq '(1 ((1 (1 2)) (1 (3 2)) (1 (2 1)))) c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 ((1 (2 5)) (1 (1 6)) (1 (6 1)))) c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(2 ((1 (2 5)) (1 (1 6 3)) (1 (6 1)))) c4 c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 ((1 (2 5)) (1 (1 9)) (1 (9 1)))) c4 c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 ((1 ((1 ((1 (1 1 1)) 1 1)) 1 1)) 1 1)) c4 c4 c4 c4 c4 c4 c4 c4 c4))
  (cmn treble (rqq '(1 ((1 (2 5)) (1 (1 9)) (1 (9 5)))) c4 c4 c4 c4 c4 c4 c4))

  
  ;; -------------------------------- rqq bugs --------------------------------
  ;; 2 of triplet should be a half note
  (cmn treble (rqq '(4 (1 (2 (1 2)) 1 1)) c4 c4 c4 c4 c4))

  ;; first flag misplaced vertically(!)
  (cmn treble (rqq '(1 (2 9)) c4 c4 c4)) ;test auto-tie

  (cmn  treble (rqq '(2 ((1 (1 1 1 2)) (1 (1 2)) (1 (1 2 4)))) g4 fs4  en4 (chord c4 ef4) eighth-rest af4 bf4 gf4 gf4))
  ;; this is a very strange bug : if we substitute the first g4 by fs4 (or anything below g4) then the display is correct!

  (cmn treble (rqq '(1 ((1 (2 5)) (1 (1 9)) (1 (9 5)))) c4 c4 c4 c4 c4 c4 ))
  ;;in the last 14uplet, the 9 group should be a 8th linked to a 64th
  ;;(in fact, the missing partial beams are here but on the wrong side of the stem and at a wrong height)
  ;; also:
  (cmn  treble   (rqq ' (1 (5 3)) c4 c4  ))
  ;;the 5 is not correct.
  (cmn treble (rqq '(1 ( 5 2)) c4 c4))
  (cmn  treble   (rqq ' (1 ((4 (3 5)) 5 3)) c4 c4  c4  c4))
  (cmn  treble   (rqq ' (2 ((4 (3 5)) 5 3)) c4 c4  c4  c4))
  (cmn treble (rqq '(1 (  17 3 1)) c4 c4 c4))

  (cmn  treble (rqq '(4 (1 2 1 1 )) c4 c4 c4 c4))
  (cmn  treble (rqq '(4 (1 3 1  )) c4 c4 c4 ))
  (cmn  treble (rqq '(4 (1 (2 (1 2)) 1 1)) c4 c4 c4 c4 c4))
  ;;in 5tuplet over 4 quarter notes the grouping do not work well.

  ;;in triplets over 4 quarter notes the grouping do not work well.
  (cmn  treble (rqq '(4 (1 2)) c4 c4 c4))

  ;; related bugs:
  ;; no dot on 3/6
  (cmn treble c4 (rq 1/6) c4 (rq 2/6) c4 (rq 3/6))
  (cmn treble (rqq '(1 (1 2 3)) c4 c4 c4)) ; ok

  ;; all screwed up (the 3/9 becomes 1/3 and everything goes to hell)
  (cmn treble c4 (rq 2/9) c4 (rq 3/9) c4 (rq 4/9))
  (cmn treble (rqq '(1 (2 3 4)) c4 c4 c4)) ; ok

  ;; this one used to work but does not work anymore
  (cmn treble (rqq '(1  (1 (1 (1 2)) 3/4 1/4)) c4 staccato c4 c4 c4 c4))

  ;; maybe we should impose the equivalent notation (that works fine)
  (cmn treble (rqq '(1  (1 (1 (1 2)) (1 (3 1)))) c4 staccato c4 c4 c4 c4))
  (cmn  (system treble)  (system treble  (rqq '(1 (1 (1 (1 1 1)) 7/8 1/8 (2 (1 1 1 (2 (1 1 1)))))) g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4)))
  (cmn  (system treble)  (system treble  (rqq '(1 (1 (1 (1 1 1)) (1 (7 1)) (2 (1 1 1 (2 (1 1 1)))))) g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4 g4)))
|#
