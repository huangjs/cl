;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; continuation of cmn3.lisp

(in-package :cmn)


;;; ----------------    staff, system, score merging

(defun staff-engorge (objects)
  (let ((current-clef nil)
	(current-meter nil)
	(current-key nil)
	(new-data nil)
	(local-brace nil))
    (loop for stf in objects do
      ;; at the start of each staff after the first, toss the redundant set-up stuff
      (let ((stf-data (staff-data stf))
	    (happy t))
	(if (and (not local-brace) (staff-local-brace stf))
          (setf local-brace (copy (staff-local-brace stf))))
	(when (or current-clef current-meter current-key)
	  (loop while happy do
	    (let ((topdat (first stf-data)))
	      (if (or (metrical-bar-p topdat)
		      (and (clef-p topdat) (eq (clef-name topdat) (clef-name current-clef)))
		      (and (meter-p topdat)
                           (= (den topdat) (den current-meter))
                           ;; [jack] beats-per-measure is better than num
                           ;;(= (num topdat) (num current-meter))
                           (= (beats-per-measure topdat) (beats-per-measure current-meter)))
		      (and (key-p topdat) (equalp (signature topdat) (signature current-key))))
		  (setf stf-data (cdr stf-data))
		(setf happy nil)))))
	(loop for datum in stf-data do
	  (push datum new-data)
	  (if (meter-p datum) (setf current-meter datum)
	    (if (clef-p datum) (setf current-clef datum)
	      (if (key-p datum) (setf current-key datum)))))))
    (setf (staff-data (first objects)) (nreverse new-data))
    (setf (staff-local-brace (first objects)) local-brace)
    (first objects)))

#|
(cmn (section (staff treble c4 q) (staff) (staff bass d3 q)) 
     (section (staff treble e4 q) (staff brace g4 q) (staff bass e3 q)))
|#

(defun absolutely-true-staff (stf)
  (if (true-staff (true-staff stf))
      (absolutely-true-staff (true-staff stf))
    (true-staff stf)))

(defun system-engorge (objects)
  (let* ((top-system (first objects))
	 (true-staves (count-if #'null (staves top-system) :key #'true-staff))
	 (staves (make-array true-staves :initial-element nil)))
    (loop for sys in objects do
      (if sys
	  (let ((ctr 0))
	    (loop for stf in (staves sys) do
	      (if (true-staff stf)
		  (push stf (back-staff (absolutely-true-staff stf)))
		(progn
		  (push stf (aref staves ctr))
		  (incf ctr)))))))
    (loop for i from 0 below true-staves do 
      (let ((back-stfs (loop for stf in (aref staves i) append (back-staff stf)))
	    (nstf (staff-engorge (reverse (aref staves i)))))
	(loop for bstf in back-stfs do 
	  (setf (true-staff bstf) nstf))
	(setf (aref staves i) (append (list nstf) back-stfs))))
    (setf (staves top-system) (loop for i from 0 below true-staves append (aref staves i)))
    top-system))

(defun score (&rest objects)
  (scorify objects))

(defun score-engorge (objects)
  (let* ((top-score (first objects))
	 (total-systems (length (systems top-score)))
	 (systems (make-array total-systems)))
    (loop for i from 0 below total-systems do
      (setf (aref systems i) (system-engorge (loop for obj in objects collect (nth i (systems obj))))))
    (setf (systems top-score) (loop for i from 0 below total-systems collect (aref systems i)))
    top-score))

#|
(setf v1 (staff treble (meter 3 4) c4 q c4 h))
(setf v2 (staff treble (meter 3 4) d4 h d4 q))
(cmn (staff-engorge (list v1 v2)))

(setf s1 (system (staff treble (meter 3 4) c4 q c4 h) (staff bass (meter 3 4) c3 h c3 q)))
(setf s2 (system (staff treble (meter 3 4) d4 q d4 h) (staff bass (meter 3 4) d3 h d3 q)))
(cmn (system-engorge (list s1 s2)))

(setf sc1 (score staff treble c4 h c4 q))
(setf sc2 (score (initial-onset 3) staff treble (d4 q (onset 3)) (d4 h (onset 4))))
(cmn (score-engorge (list sc1 sc2)))
|#



;;; 
;;; ----------------    marks
;;;
;;; most trivial marks are handled directly by display-sundry or whatever.
;;; markify deals with those that depend on the placement of more than one note.


(defun markify (score)
  (map-over-staves #'markify-staff score)
  (if (automatic-beat-subdivision-numbers score)
      (map-over-staves #'check-for-irregular-beat-subdivisions score))
  (if (and (automatic-measure-numbers score)
	   (member (automatic-measure-numbers score) '(:by-line :by-page)))
      (uncover-line-or-page-measure-numbers score))
  score)

(defun markify-staff (score staff)
  (let ((waiting-rest nil)
	(waiting-trem nil)
	(pending-end nil)
	(bar-x0 0)
	(last-object nil)
	(measure-rest-ok t)
	(pending-max-line 0))
    (loop for object in (staff-data staff) do
      (if (and (visible-p object)
	       (marks object))
	  (loop for mark in (marks object) do
	    (if (and (crescendo-p mark)
		     (tag-note mark)
		     (/= (staff-y0 object) (staff-y0 (tag-note mark))))
		(split-crescendo mark object staff score))))
      (if (metrical-bar-p object)
	  (let ((save-max pending-max-line))
	    (when waiting-rest
	      (setf (box-x0 waiting-rest) bar-x0)
	      (if (or (not last-object)
		      (not (clef-p last-object)))
		  (setf (box-x1 waiting-rest) (box-x0 object))
		(setf (box-x1 waiting-rest) (box-x0 last-object)))
	      (setf waiting-rest nil))
	    (setf measure-rest-ok t)
	    (when (and pending-end
		       (equal object (tag-note pending-end)))
	      (setf (max-line pending-end) (max pending-max-line (or (max-line pending-end) 0)))
	      (setf pending-max-line 0)
	      (setf pending-end nil))
	    (when (bar-marks object)
	      (setf pending-end (find-if #'ending-p (bar-marks object)))
	      (if (and pending-end (eq (tag-type pending-end) :second)) (setf (max-line pending-end) save-max))))
	(if (and (rest-p object)
		 measure-rest-ok
		 (member (rest-mark object) (list :rest1 :measurerest :repsign)))
	    (setf waiting-rest object)
	  (if (audible-p object)
	      (progn
		(setf measure-rest-ok nil) ;only fixup measure rest if it's alone in the measure
		(setf waiting-rest nil)
		(if (and (zerop (flags object))
			 (>= (quarters object) 0.5))
		    (let* ((trem (get-tremolo object))
			   (type (and trem (tremolo-type trem))))
		      (if pending-end (setf pending-max-line (max pending-max-line (maximum-line object))))
		      (if (and trem (member type '(:left :right)))
			  (if waiting-trem
			      (if (eq type :left)
				  (cmn-warn "two active begin-tremolos?")
				(let ((trem-note (tremolo-note waiting-trem)))
				  (setf (tremolo-note waiting-trem) object)
				  (if (and (measured waiting-trem) 
					   (> (quarters trem-note) .5))
				      (annotate-beam score nil (box-y0 staff) (list trem-note object) 1.0))
				  (setf waiting-trem nil)))
			    (if (eq type :right)
				(cmn-warn "end tremolo without begin tremolo?")
			      (progn
				(setf (tremolo-note trem) object)
				(setf waiting-trem trem)))))))))))
      (if (not (rest-p object)) 
	  (if (bar-p object) 
	      (setf bar-x0 (box-x0 object)) 
	    (if (not waiting-rest)
		(setf bar-x0 (box-x1 object)))))
      (setf last-object object))))

;;; (cmn staff bar treble measure-rest bass bar measure-rest bar)



;;; fillify looks for needed data that has been omitted from the user's score description
;;; if meter, it looks for barlines missing, fills out measures, inserts tied notes etc
;;; if a system has musical data, it makes sure there's a clef
;;; if an empty bar, or an unfull bar, or a note beginning after the current end, fill with rests (and bars)

(defun get-back-staff-clef (staff)
  (if (true-staff staff)
      (let ((stf (absolutely-true-staff staff)))
	(loop for objects on (staff-data stf) do
	  (let ((datum (first objects)))
	    (if (clef-p datum)
		(return-from get-back-staff-clef datum))))))
  nil)
  
(defun find-or-insert-clef (score staff)
  (let ((old-data nil)
	(new-clef (get-back-staff-clef staff)))
    (loop for objects on (staff-data staff) do
      (let ((datum (first objects)))
	(if (clef-p datum)
	    (return-from find-or-insert-clef)
	  (if (audible-p datum)
	      (progn
		(if (not new-clef)
		    (let ((pitch (if (note-p datum) 
				     (+ (* 12 (or (octave datum) old-octave)) (or (pitch datum) 0))
				   (+ (* 12 (or (octave (first (chord-data datum))) old-octave)) 
				      (or (pitch (first (chord-data datum))) 0)))))
		      (setf new-clef (clef (if (> pitch 47) treble bass) (scale 0 0))))
		  (setf new-clef (clef new-clef (scale 0 0))))
		(setf (staff-data staff) (append (nreverse old-data) (list new-clef) objects))
		(return-from find-or-insert-clef))
	    (push datum old-data))))))
  score)

(defun set-odb (score staff)
  ;; here we make sure the onset/duration/beat fields are filled and onsets are in order (also rhythm-mixin fields)
  (let ((beg 0.0)
	(dur 0.0)
	(measure-beats 0)		;beats / bar 
	(measure-beat 0)		;where are we in current bar (0-based, whereas beats are 1-based)
	(measure-length 4)		;needed mostly to fill in durations of whole/measure rests
	(measure-onset 0))
    (loop for object in (staff-data staff) do
      (if (or (rest-p object)
	      (note-p object)
	      (chord-p object))
	    (let ((onset (odb-onset object)))
	      (if onset
		  (if (and (not (numberp onset)) (functionp onset))
		      (setf onset (setf (odb-onset object) (funcall onset)))
		    (if (< onset beg)
			(cmn-error "begin times out of order: ~1,3F < ~1,3F" onset beg)))
		(progn
		  (if (odb-beat object)
		      (progn
			(if (zerop (odb-beat object)) (cmn-error "cmn beats are numbered from 1, not 0"))
			(if (< (odb-beat object) (+ measure-beat 1)) (incf measure-onset measure-length))
			(setf onset (+ measure-onset (1- (odb-beat object))))
			(setf (odb-beat object) nil))
		    (setf onset (fratify (+ beg dur))))
		  (setf (odb-onset object) onset)))
	      (when (chord-p object)	;needed for backpatching to individual notes within a chord
		(loop for nt in (chord-data object) do (setf (odb-onset nt) onset)))
	      (setf beg onset)
	      (if (not (odb-beat object)) 
		  (setf (odb-beat object) (fratify onset)))
	      (if (odb-duration object)
		  (setf dur (odb-duration object))
		(if (quarters object)
		    (setf dur (quarters object))
		  (if (rest-p object)	;whole rest or measure rest -- whole rest maxes out at 4 -- is this a good idea?
		      (let ((dr (if (and (> measure-length 4)
					 (eq (rest-mark object) :rest1))
				    4
				  measure-length)))
			(setf (quarters object) dr)
			(setf (duration object) dr)
			(if (zerop measure-beats) (push :unmetered (store-data object)))
			(setf dur dr))
		    (cmn-error "no duration or rhythm provided to ~A:~A" object (descry object)))))
	      (setf (duration object) dur)
	      (if (marks object)
		  (loop for mark in (marks object) do
		    (if (and (trill-p mark)
			     (wavy-line mark)
			     (not (wavy-time mark)))
			(setf (wavy-time mark) (+ beg dur))
		      (if (and (crescendo-p mark)
			       (not (odb-onset mark)))
			  (setf (odb-onset mark) onset)))))
	      (when (not (quarters object))
		(let ((rhy (duration-to-rhythm score dur)))
		  (setf (quarters object) (quarters rhy))
		  (if (note-p object) (setf (head-quarters object) (quarters rhy)))
		  (setf (dots object) (dots rhy))
		  (setf (flags object) (flags rhy)))))
	(progn
	  (if (eq (odb-onset object) :at-end)
	      (progn
		(setf (odb-onset object) (+ measure-onset measure-length))
		(setf (odb-beat object) (odb-onset object))
		(if (not (odb-duration object)) (setf (duration object) 0)))
	    (when (not (odb-onset object))
	      (if (odb-beat object)
		  (progn
		    (if (zerop (odb-beat object)) (cmn-error "cmn beats are numbered from 1, not 0"))
		    (if (< (odb-beat object) (+ measure-beat 1)) (incf measure-onset measure-length))
		    (setf (odb-onset object) (fratify (+ measure-onset (1- (odb-beat object))))))
		(setf (odb-onset object) (fratify (+ beg dur))))
	      (if (not (odb-duration object)) (setf (duration object) 0))
	      (setf (odb-beat object) (odb-onset object))))
	  (if (meter-p object)
	      (progn
		(setf measure-beats (beats-per-measure object))
		(if (not (zerop measure-beats))
		    (setf measure-length (fratify (* measure-beats (beat-duration object))))
		  (setf measure-length 4)))
	    (if (metrical-bar-p object)
		(progn
		  (setf measure-onset (odb-onset object))
		  (setf beg measure-onset)
		  (setf dur 0))
	      )))) ; if (or (rest-p...)) 
      (when (not (zerop measure-length))
	(let ((current-onset (odb-onset object)))
	  ;; get current beat (measure-beat) and reset measure-onset if necessary
	  (loop while (>= current-onset (+ measure-onset measure-length)) do
		(incf measure-onset measure-length))
	  (setf measure-beat (- current-onset measure-onset)))))
    )
  score)

;;;(cmn staff treble (meter 2 4) (c4 q (beat 2)) (cs4 e (beat 1)) (d4 e (beat 2.5)))

(defun fixup-line (object current-clef)
  (if (note-p object)
      (setf (note-line object) (place-of-note-on-clef object current-clef))
    (if (chord-p object)
	(loop for note in (chord-data object) do
	  (fixup-line note current-clef)))))

(defvar *overlap-noteheads* nil)

(defun check-if-notes-collide (note1 note2)
  (if (and note1 note2 
	   (note-p note1) (note-p note2)
	   (= (odb-onset note1) (odb-onset note2)))
      (let ((l1 (line note1))
	    (l2 (line note2)))
	(if (and (numberp l1)
		 (numberp l2))
	    (if (< (abs (- l1 l2)) 2) 
		(if (> l1 l2)
		    (progn
		      (setf (note-collision note2) note1)
		      (setf (note-collider note1) note2))
		  (if (or (not *overlap-noteheads*)
			  (< l1 l2))
		      (progn
			(setf (note-collision note1) note2)
			(setf (note-collider note2) note1)))))))))

(defun fixup-grace-note-line (object current-clef)
  (if (chord-p object)
      (loop for n in (chord-data object) do
	    (fixup-grace-note-line n current-clef)))
  (if (marks object) 
      (let ((grace (find-if #'grace-note-p (marks object))))
	(if grace
	    (loop for gnote in (grace-data grace) do
		  (fixup-line gnote current-clef))))))

(defun set-clef-y0-and-lines (score staff)
  ;; the last pass -- make sure everything is consistent and complete and ready for CMN to crank it out.
  (let ((y0 (box-y0 staff))
	(current-clef nil)
	(last-audible-object nil))
    (loop for object in (staff-data staff) do
      (if (clef-p object)
	  (setf current-clef object)
	(progn
	  (when (staff-relative-mixin-p object)
	    (setf (staff-y0 object) y0))
	  (if (audible-p object)
	      (progn
		(setf (local-line-separation object) (inner-line-separation staff))
		(fixup-line object current-clef)
		(if last-audible-object (check-if-notes-collide object last-audible-object))
		(fixup-grace-note-line object current-clef)
		(setf last-audible-object object))
	    (progn
	      (setf last-audible-object nil)
	      (if (and current-clef
		       (key-p object)
		       (not (clef-position object)))
		  (dcopy current-clef object))))))))
  (if (true-staff staff)     ;it's a tied-to staff, so check for collisions with the real staff, added 17-Aug-98
      ;; (cmn (setf h1 (staff treble c4 q c4 q c4 q)) (staff (tied-to h1) g4 q c4 q g4 q))
      (let ((tobjs (staff-data (true-staff staff)))
	    (nobjs (staff-data staff)))
	(loop while (and tobjs nobjs) do
	  (loop while (and tobjs (not (audible-p (first tobjs)))) do (setf tobjs (cdr tobjs)))
	  (loop while (and nobjs (not (audible-p (first nobjs)))) do (setf nobjs (cdr nobjs)))
	  (when (and nobjs tobjs)
	    (if (= (onset (first tobjs)) (onset (first nobjs)))
		(progn
		  (check-if-notes-collide (first tobjs) (first nobjs))
		  (setf tobjs (cdr tobjs))
		  (setf nobjs (cdr nobjs)))
	      (if (< (onset (first tobjs)) (onset (first nobjs)))
		  (setf tobjs (cdr tobjs))
		(setf nobjs (cdr nobjs))))))))
  score)

(defun fractional-part (a b) 
  (multiple-value-bind (int frac) 
      (floor a b) 
    (declare (ignore int)) 
    frac))

#|
;;;  KSM bugs involving 7's and beat length = 2
(cmn (system (staff (METER 2 2) 
             (gs4 (rq 4/7)) (REST (RQ 2/7)) (f5 (rq 8/7) begin-tie) 
             (F5 (RQ 12/7) end-tie) (FS4 (RQ 2/7))))) 


(cmn (system (staff (METER 4 4) 

             ;; bar 1 (+ 4) = 4 
             (rest (rq 4)) 

             ;; bar 2 (+ 3/2 1/3 1/3 1/6 1/6 1 1/3 1/6) = 4 
             (rest (rq 3/2)) 
             (GS5 (RQ 1/3)) (B3 (RQ 1/3)) (D6 (RQ 1/6)) 
             (B4 (RQ 1/6)) (G6 (RQ 1)) (REST (RQ 1/3)) (C7 (RQ 1/6)) 

             ;; bar 3 (+ 1/3 1/3 2/3 1/2 7/6) = 3 
             (METER 3 4) 
             (D6 (RQ 1/3)) 
             (AS4 (RQ 1/3)) 
             (REST (RQ 2/3)) 
             (A5 (RQ 1/2)) 
             (REST (RQ 7/6)))))
|#

(defun fill-out-beat (ur-dur current-beat beat-length)
  ;; dur is either the desired duration or what's left in a measure,
  ;; current-beat is where we are in the measure
  ;; beat-length is how long (in quarters) the basic beat is
  (when (> ur-dur .0001)
    ;; slightly generalized 22-May-97 -- see cmn2.lisp under how-many-flags for some tests
    (let* ((dur (fratify ur-dur))
	   (beat-fraction (fratify (fractional-part current-beat beat-length)))
	   (beat-dur-left (fratify (- beat-length beat-fraction))))
      (if (or (<= dur beat-dur-left) (zerop beat-dur-left))  ; we are in the current beat
	  (if (or (< dur 1/4)
		  (and (integerp dur) (< dur 5))
		  (and (= dur beat-dur-left) (< dur beat-length) (< (abs (- dur beat-dur-left)) .001)) ; added 12-Feb-02 for 2/5 beat filling
		  (and (ratiop ur-dur) (not (member (numerator ur-dur) (list 5 9)))))
	      (list dur)
	    (let ((closest-dur (if (and (ratiop ur-dur) (member (numerator ur-dur) (list 5 9)))
				   (/ (1- (numerator ur-dur)) (denominator ur-dur))
				 (find-if #'(lambda (n) (>= dur n)) (list 4 3 2 3/2 1 3/4 1/2 3/8 1/4 1/5 1/7 1/8 0)))))
	      (if (zerop beat-fraction)
		  (append (list closest-dur) (fill-out-beat (- dur closest-dur) (+ current-beat closest-dur) beat-length))
		(append (fill-out-beat (- dur closest-dur) current-beat beat-length) (list closest-dur)))))
	;; here we are going past the current beat
	;; this should check the ur-dur denominator
	(if (or (and (or (zerop (mod dur 2))
			 (member dur (list 1 3 3/2 4/3 :test #'=)))
		     (or (zerop beat-fraction)
			 (and (= beat-fraction 1) ;try to catch half notes that need not be divided
			      (member beat-dur-left '(1 3 5 7)))))
		(and (= beat-length 1/4) (zerop beat-fraction) (member dur '(1/2 3/4 1))) ;(cmn staff treble (meter 5 16) (c4 e) (c4 e.))
		(and (= dur 1) (= beat-fraction 1/2))
		(and (= dur 3/2) (= beat-length 1) (= beat-fraction 1/2))
		(and (= dur 2/3) (= beat-length 1) (= beat-fraction 2/3))
		(and (= dur 4/3) (member beat-length '(1 2) :test #'=) (member beat-fraction '(1/3 2/3) :test #'=))
		(and (= dur 2/5) (= beat-length 1) (= beat-fraction 4/5))
		(and (= dur 4/5) (= beat-length 1) (member beat-fraction '(1/5 2/5 3/5 4/5) :test #'=))
		(and (= dur 2/7) (= beat-length 1) (= beat-fraction 6/7)))
	    (if (zerop (mod dur 4))
		(loop for i from 0 below (floor dur 4) collect 4)
	      (if (zerop (mod dur 2))
		  (if (and (= dur 6) (= beat-length 2))
		      (list 6)
		    (append (list 2) (loop for i from 0 below (floor (- dur 2) 4) collect 4)))
		(list dur)))
	  ;; now we have a beat-crossing that isn't obvious
	  (if (and (> dur 4) (zerop beat-fraction))
	      (append (loop for i from 0 below (floor dur 4) collect 4)
		      (fill-out-beat (- dur (* 4 (floor dur 4))) (+ current-beat (* 4 (floor dur 4))) beat-length))
	    (append (fill-out-beat beat-dur-left current-beat beat-length)
		    (fill-out-beat (- dur beat-dur-left) (+ current-beat beat-dur-left) beat-length))))))))

#|
(cmn (staff treble (meter 1 4) (c4 (rq 1/5)) (c4 (rq 3/5)) (c4 (rq 1/5))))
(cmn (staff treble (meter 1 4) (c4 (rq 1/5) (setf hi (beat-subdivision- (subdivision 5) )))
        (c4 (rq 3/5) (-beat-subdivision- hi)) (c4 (rq 1/5) (-beat-subdivision hi))))
(cmn (staff treble (meter 2 4) (c4 (rq 1/7) (setf hi (beat-subdivision- (subdivision 7) )))
        (c4 (rq 5/7) (-beat-subdivision- hi)) (c4 (rq 1/7) (-beat-subdivision hi))))
(cmn (staff treble (meter 2 4) (c4 (rq 1/7) (setf hi (beat-subdivision- (subdivision 7) )))
        (c4 (rq 4/7) (-beat-subdivision- hi)) (c4 (rq 1/7) (-beat-subdivision- hi)) (c4 (rq 1/7) (-beat-subdivision hi))))
(cmn (staff treble (meter 1 4) (c4 tq) (c4 te)))
|#

(defun fill-out-measure (dur current-beat measure-length beat-length)
  ;; it's important to maintain the ratios here to keep the beat-filler from getting confused by floating point inaccuracies
  (let* ((dur-left (if (ratiop dur) 
		       (iratify (min dur (- (iratify measure-length) (iratify current-beat))))
		     (min dur (- measure-length current-beat))))
	 (dur-list (nreverse (fill-out-beat dur-left current-beat beat-length)))
	 (taken-dur (apply #'+ dur-list))
	 (cur-dur (- dur taken-dur)))
    (if dur-list (incf current-beat taken-dur))
    (loop while (and (> cur-dur .001)  ; was plusp but floating point cruft can turn that into an infinite loop
		     (< (+ current-beat .001) measure-length)) do ; .3333 as 1/3 can lead to infinitesimal loss here
      (let* ((fd (nreverse (fill-out-beat (min cur-dur beat-length) current-beat beat-length)))
	     (td (apply #'+ fd)))
	(setf dur-list (append fd dur-list))
	(decf cur-dur td)
	(incf current-beat td)))
    (nreverse dur-list)))

(defun fill-out-measures (dur current-beat measure-length beat-length &key with-bars)
  (let* ((dur-list (nreverse (fill-out-measure dur current-beat measure-length beat-length))))
    (decf dur (apply #'+ dur-list))
    (when (plusp dur)
      (loop while (> dur measure-length) do
	(if with-bars (push :bar dur-list))
	(push measure-length dur-list)
	(decf dur measure-length))
      (when (not (ratiop dur))
	(let ((idur (round (* dur 10000))))
	  (setf dur (/ idur 10000))))
      (when (plusp dur)
	(if with-bars (push :bar dur-list))
	(setf dur-list (append (nreverse (fill-out-measure dur 0 measure-length beat-length)) dur-list))))
    (nreverse dur-list)))


(defmethod audify ((note note-mixin) &rest args) 
  (apply #'note note args))

(defmethod audify ((chord chord) &rest args) 
  (let ((new-chord (copy-chord-for-ties chord)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) new-chord (argument arg))))
    new-chord))

(defmethod audify ((rest rest-mixin) &rest args)  ;must copy arg
  (apply #'rest rest args))

(defmethod raudify ((note note-mixin) &rest args) 
  (loop for act in args do
    (if (self-acting-p act)
	(funcall (action act) note (argument act))))
  note)

(defmethod raudify ((chord chord) &rest args) 
  (loop for arg in args do
    (if (self-acting-p arg)
	(funcall (action arg) chord (argument arg))))
  chord)

(defmethod raudify ((rest rest-mixin) &rest args)
  (loop for act in args do
    (if (self-acting-p act)
	(funcall (action act) rest (argument act))))
  rest)


(defvar use-sort nil)

(defun feq (a b) (or (= a b) (< (abs (- a b)) .00001)))
(defun fleq (a b) (or (<= a b) (< (abs (- a b)) .00001)))

(defun insert-bars-ties-and-rests (score staff staff-num)
  
  ;; keep list of pending tied notes, last-note-end, measure-onset, measure-length
  ;; at each explicit bar, fill out current (= object at (+ measure-onset measure-length) with 0 dur)
  ;; at explicit rest, put it in, set last-note-end to (max (rest-onset+rest-duration) last-note-end)
  ;; at explicit note or chord, fill to onset, if necessary, then handle like rest
  ;;   in last cases, if (onset-measure-onset)>=measure-length, add bar to staff data
  ;;   if dur-list filling, add bars if needed.
  
  (let* ((last-end (initial-onset score))
	 (infinite 200000)		;default is unmeasured (but still watch ties and rests etc)
	 (measure-length infinite)	;if a bar appears during unmeasured score it is at current onset time
	 (beat-length 1.0)
	 (measure-onset (initial-onset score))
	 (last-onset (initial-onset score))
	 (pending-notes nil)
	 (invisible-rests (true-staff staff)) ;tied-to staff initial auto-rests are invisible
	 (new-data nil)
	 (within-explicit-beam nil)
	 (add-bars (automatic-bars score))
	 (invisible-bar (true-staff staff)))
    (loop for object in (staff-data staff) do
      (if (page-p object) (push object (pages score))
	(if (line-p object) (progn (setf (line-staff object) staff-num) (push object (lines score)))
	  (let ((desired-onset (odb-onset object)))
	    (if (and pending-notes 
		     (fleq (odb-onset (first pending-notes)) desired-onset))
		(loop while (and pending-notes 
				 (fleq (odb-onset (first pending-notes)) desired-onset)) do
		  (let* ((current-note (pop pending-notes))
			 (current-onset (odb-onset current-note)))
		    (when (feq (- current-onset measure-onset) measure-length)
		      (if add-bars
			  (push (cmn-bar :onset current-onset :beat current-onset :duration 0 :hidden invisible-bar) new-data))
		      (setf measure-onset current-onset))
		    (if (automatic-ties score)
			(push current-note new-data))
		    (setf last-end (max last-end (+ current-onset (odb-duration current-note))))
		    (setf last-onset current-onset))))
	    (if (and (not pending-notes)
		     (< last-end desired-onset))
		(let ((rest-durs (fill-out-measures (- desired-onset last-end)
						    (- last-end measure-onset) 
						    measure-length
						    beat-length
						    :with-bars t)))
		  ;; (if (and (= (length rest-durs) 1) (= (first rest-durs) 4/3)) (setf rest-durs (list 1 1/3)))
		  (loop for dur in rest-durs do
		    (if (numberp dur)
			(if (not (zerop dur))
			    (let ((new-rest (ur-rest (make-instance 'wrest) (rhythm dur))))
			      (setf (odb-onset new-rest) last-end)
			      (setf (odb-beat new-rest) last-end)
			      (incf last-end dur)
			      (if invisible-rests (setf (matrix new-rest) (list 0 0 0 0 0 0)))
			      (if (automatic-rests score)
				  (push new-rest new-data))))
		      (progn
			(if add-bars
			    (push (cmn-bar :onset last-end :beat last-end :duration 0 :hidden invisible-bar) new-data))
			(incf measure-onset measure-length))))))
	    (setf last-end (max last-end 
				(+ desired-onset 
				   (if (or (note-p object) 
					   (chord-p object) 
					   (rest-p object)
					   (sundry-p object))
				       (duration object) 
				     0))))
	    (setf last-onset desired-onset)
	    (when (and (not (metrical-bar-p object)) 
		       (not (clef-p object)) 
		       (feq (- (odb-onset object) measure-onset) measure-length))
	      (if add-bars
		  (push (cmn-bar :onset (odb-onset object) :beat (odb-onset object) :duration 0 :hidden invisible-bar) new-data))
	      (setf measure-onset (odb-onset object)))
	    
	    (if (or (audible-p object)
		    (rest-p object))
		(let ((durs (fill-out-measures (odb-duration object) 
					       (- desired-onset measure-onset) 
					       measure-length beat-length
					       :with-bars t)))
		  (setf invisible-rests nil)
		  (if (store-data object)
		      (if (some #'(lambda (n) 
				    (or (and (listp n) 
					     (eq (first n) :begin-beam)
					     (beam-p (second n))
					     (explicit-beams (second n)))))
				(store-data object))
			  (setf within-explicit-beam t)))

		  (if (or (feq (length durs) 1)
			  (not (automatic-ties score))
			  within-explicit-beam)
		      (push object new-data)
		    (let* ((tied (audible-p object))
			   (first-dur (pop durs))
			   (left-overs (length durs))
			   (current-onset (+ (odb-onset object) first-dur))
			   (current-tie (and tied (let ((nt (new-tie)))
						    (setf (tie-added nt) t)
						    nt)))
			   (tied-note (raudify object (rhythm first-dur)))) 
		      (when tied
			(setf (tie-note current-tie) tied-note)
			(add-to-ties tied-note current-tie))
		      (push tied-note pending-notes) ;new-data
		      (loop for dur in durs and i from 1 do
			(if (and (numberp dur)
				 (plusp dur))
			    (let* ((tied-note (audify object 
						      (rhythm dur) 
						      (onset current-onset) 
						      (beat current-onset)))
				   (old-tie (and tied (tie-tie current-tie))))
			      (when tied
				(setf (tie-note old-tie) tied-note)
				(setf (ties tied-note) nil)
				(add-to-ties tied-note old-tie)
				(setf (slurs tied-note) nil)
				(when (note-p tied-note) 
				  (push (list :sign (note-sign tied-note)) (store-data tied-note))
				  (setf (note-sign tied-note) nil)))
			      (setf (marks tied-note) nil)
			      ;; this is wrong for pauses -- they should be removed from the first tied note and transferred to the last
			      (when (and tied (< i left-overs))
				(setf current-tie (new-tie))
				(setf (tie-added current-tie) t)
				(setf (tie-note current-tie) tied-note)
				(add-to-ties tied-note current-tie))
			      (push tied-note pending-notes)
			      (incf current-onset dur))
			  (progn
			    (if add-bars
				(push (cmn-bar :onset current-onset :beat current-onset :duration 0 :hidden invisible-bar) pending-notes))
			    (incf measure-onset measure-length))))
		      (setf pending-notes (sort pending-notes #'< :key #'odb-onset))))

		  (if (store-data object)
		      (if (some #'(lambda (n) (or (and (listp n) (eq (first n) :end-beam)) (eq n :end-beam))) (store-data object))
			  (setf within-explicit-beam nil))))
	      (progn
		(when (meter-p object)
		  (if (and (member (beats-per-measure object) '(3 6 9 12 15))
			   (feq (beat-duration object) .5))
		      (setf beat-length 1.5)
		    (setf beat-length (beat-duration object)))
		  (if (= (beats-per-measure object) 0) 
		      (setf measure-length infinite)
		    (setf measure-length (* (beats-per-measure object) (beat-duration object)))))
		(if (and (not (clef-p object))
			 (not (and (bar-p object)
				   (within-measure object))))
		    ;; (cmn treble (meter 4 4) (c4 q) (bar (dashed t) (within-measure t)) (c4 q)(c4 q)(c4 q) (c4 q) (c4 q)(c4 q)(c4 q))
		    (setf measure-onset (odb-onset object)))
		(setf (odb-onset object) last-onset)
		(setf (duration object) 0)
		(setf (odb-beat object) last-onset)
		(push object new-data)))))))

    (loop while pending-notes do
      (let* ((current-note (pop pending-notes))
	     (current-onset (odb-onset current-note)))
	(when (feq (- current-onset measure-onset) measure-length)
	  (if add-bars
	      (push (cmn-bar :onset current-onset :beat current-onset :duration 0 :hidden invisible-bar) new-data))
	  (setf measure-onset current-onset))
	(if (automatic-ties score) (push current-note new-data))
	(setf last-end (max last-end (+ current-onset (duration current-note))))
	(setf last-onset current-onset)))
    
    (when new-data
      (if use-sort
	  ;; a noticeable amount of cmn total compute time is here just to get bars in the right place.
	  (setf (staff-data staff) (sort (nreverse new-data) 
					 #'(lambda (a b) 
					     (let ((onset-a (odb-onset a))
						   (onset-b (odb-onset b)))
					       (or (< onset-a onset-b)
						   (and (feq onset-a onset-b)
							(bar-p a) 
							(inserted a)
							(not (clef-p b))))))))
	
	(let* ((reversed-data (nreverse new-data))
	       (baddies nil)
	       (a (first reversed-data))
	       (onset-a (odb-onset a)))
	  (loop for b in (cdr reversed-data) do
	    (let ((onset-b (odb-onset b)))
	      (if (or (> onset-a onset-b)
		      (and (feq onset-a onset-b)
			   (bar-p b) 
			   (inserted b)
			   (not (clef-p a))))
		  (push b baddies)
		(setf onset-a onset-b))
	      (setf a b)))
	  (if (null baddies)		;the easy case -- nothing got out of order
	      (setf (staff-data staff) reversed-data)
	    (let ((clean-data (delete-if #'(lambda (a) (member a baddies)) reversed-data)))
	      (setf (staff-data staff) (merge 'list clean-data (nreverse baddies)
					      #'(lambda (a b)
						  (<= (odb-onset a) (odb-onset b)))))))))
      ;; this doesn't work if the arguments are in the reverse order (why?) and probably
      ;; isn't right anyway -- what about the straight < case?  (Apparently if a and b
      ;; are not "strictly less than", the reverse comparison is done to see if they're "equal").
      ))

  score)


(defvar *cmn-measure-number* nil)

(defun insert-measure-numbers (score)
  ;; automatic-measure-numbers is either t or an integer = count interval (in this case)
  ;; we need only place the measure-number mark on the top system's bottom staff's bars
  (let* ((bottom-staff (first (last (staves (first (systems score))))))
	 (bar-num (or (first-measure-number score) 1))
	 (bar-mod (automatic-measure-numbers score))
	 (funny-case (and (not (numberp bar-mod)) (member bar-mod '(:by-line :by-page)))))
    (if (not (numberp bar-mod)) (setf bar-mod 1))
    (loop for obj in (cddr (staff-data bottom-staff)) do
      (when (metrical-bar-p obj)
	(incf bar-num)
	(if (zerop (mod bar-num bar-mod))
	    (if funny-case
		(add-to-marks obj (list (measure-number bar-num invisible)))
	      (add-to-marks obj (list (measure-number bar-num)))))))
    (setf *cmn-measure-number* bar-num)))
	
(defun uncover-line-or-page-measure-numbers (score)
  (let* ((bottom-staff (first (last (staves (first (systems score))))))
	 (bar-mod (automatic-measure-numbers score))
	 (page-active nil))
    (loop for obj in (staff-data bottom-staff) do
      (if (page-p obj) 
	  (setf page-active t)
	(if (and (metrical-bar-p obj) 
		 (or page-active
		     (and (eq bar-mod :by-line)
			  (eq (justification obj) :left))))
	    (let ((mn (find-if #'measure-mark-p (marks obj))))
	      (if mn (setf (matrix mn) (list 1 0 0 1 0 0)))
	      (setf page-active nil)))))))

(defun set-octave-sign (octize oct)
  ;; from first to last decrement all relevant pitch entities to match oct offset (-2 .. 2)
  ;; add the octave mark to the first and save the first and last notes for later post-justification matching
  (let ((lines (* oct 7)))
    (loop for note in octize do
      (if (note-p note)
	  (progn
	    (decf (note-line note) lines)
	    (decf (octave note) oct))
	(if (chord-p note)
	    (loop for cn in (chord-data note) do
	      (decf (note-line cn) lines)
	      (decf (octave cn) oct)))))
    (add-to-marks (first octize) (prepare-octave-signs octize oct))))

(defun insert-octave-signs (score staff)
  (declare (ignore score))
  (let ((octize nil)
	(oct 0))
    (loop for object in (staff-data staff) do
      (if (audible-p object)
	  (let ((max-line (maximum-line object))
		(min-line (minimum-line object)))
	    (if (and (> max-line 16) (not (octaved object)))
		(progn
		  (if (or (null octize)
			  (minusp oct))
		      (progn
			(if (> (length octize) 3) (set-octave-sign (reverse octize) oct))
			(setf octize nil)))
		  (push object octize)
		  (setf oct (if (and (/= oct 1) (> max-line 23)) 2 1)))
	      (if (and (< min-line -7) (not (octaved object)))
		  (progn
		    (if (or (null octize)
			    (plusp oct))
			(progn
			  (if (> (length octize) 3) (set-octave-sign (reverse octize) oct))
			  (setf octize nil)))
		    (push object octize)
		    (setf oct (if (and (/= oct -1) (< min-line -14)) -2 -1)))
		(progn
		  (if (> (length octize) 3) (set-octave-sign (reverse octize) oct))
		  (setf octize nil)))))
	(if (clef-p object)
	    (progn
	      (if (> (length octize) 3) (set-octave-sign (reverse octize) oct))
	      (setf octize nil)))))
    (if (> (length octize) 3) (set-octave-sign (reverse octize) oct))))



;;; key signatures,  stickiness setting, bar-lines, appoggiaturas+grace-notes, chords

(defun check-note-for-needed-natural (object current-accidentals current-key-signature-accidentals current-durations cancel octaves)
  ;; cancel: cancel-redundant-accidentals
  (let ((this-cclass (cclass object))
	(this-sign (note-sign object))
	(tied (and (ties object) (find :right (ties object) :key #'tie-type))))
    (when (not tied)
      (if (aref current-accidentals this-cclass)
	  (if (not this-sign)
	      (if (not (eq (aref current-key-signature-accidentals this-cclass)
			   (aref current-accidentals this-cclass)))
		  (if (not (aref current-key-signature-accidentals this-cclass))
		      (if (not (eq (aref current-accidentals this-cclass) natural))
			  (setf (note-sign object) natural))
		    (setf (note-sign object) (aref current-key-signature-accidentals this-cclass))))
	    (if (and cancel
		     ;; now look for redundant accidental -- the problem with this version is that
		     ;;   it will leave in accidentals that are technically redundant if the same
		     ;;   pitch occurs in a different octave in between, and that makes (for example)
		     ;;   repeated octave jumps kinda fussy looking.
		     (eq this-sign (aref current-accidentals this-cclass))
		     (/= (aref octaves this-cclass) -1)
		     (= (aref octaves this-cclass) (octave object)))
		(setf (note-sign object) nil)))

	(progn
	  ;;; From Larry Troxler: I don't understand what this form does; I leave it in 
	  ;;; because for my personal use, it will never get hit. 
	  (if (and (aref current-key-signature-accidentals this-cclass)
		   (not this-sign))
	      (progn
		;; bug fix thanks to Jin S. Choi 3-Nov-96
		;; (setf this-sign natural)
		;; (setf (note-sign object) natural))))
		(setf this-sign (or (aref current-key-signature-accidentals this-cclass) natural))
		(if (not cancel) (setf (note-sign object) this-sign))))

	  ;;; From Larry Troxler: This seems to fix my problem of accidentals being redundantly
	  ;;; notated although they are in the current key signature. (3-Jan-03)
	  (if (and cancel
		   this-sign
		   (eq (or (aref current-key-signature-accidentals this-cclass) natural) this-sign))
	      (setf (note-sign object) nil)))))

    (setf (aref octaves this-cclass) (octave object))
    (setf (aref current-durations this-cclass) 0)
    (setf (aref current-accidentals this-cclass) this-sign)))

#|
(cmn (automatic-naturals t) (redundant-accidentals nil)  staff treble
       g-major (meter 4 4)
       cs4 cs4 cs5 cs4 
       fs5 fn5 ef5 fs4
       dn4 ds4 dn4 fs4
       ds4 dn4 dn4 ds4)
(cmn (redundant-accidentals nil) staff bar treble (key d-minor) (meter 1 4) bs4 bar bf4 bar bn4 bar b4 double-bar)
(cmn (redundant-accidentals nil) staff bar treble (key d-minor) (meter 1 4) b4 s bf4 s b4 s b4 s)
|#

(defun check-chord-for-needed-naturals (chord objects current-accidentals current-key-signature-accidentals current-durations cancel octaves)
  (when (or (not (ties chord)) 
	    (not (find :right (ties chord) :key #'tie-type)))
    ;; this is not redundant because the chord ties are not the same as the individual note ties
    (loop for cnote in objects do
      (check-note-for-needed-natural cnote current-accidentals current-key-signature-accidentals current-durations cancel octaves))))

(defun insert-naturals (score staff)
  ;; added redundant-accidentals 23-Jul-93 so the function name is somewhat inaccurate now
  (let* ((implicit-style (implicit-accidental-style score)) ;can also be :old-style=infinite notes/bar, :only-chords, :paranoid
	 (implicit-duration (or (implicit-accidental-duration score)
				(if (not (redundant-accidentals score)) 10000
				  (if (eq implicit-style :new-style) 1
				    (if (eq implicit-style :only-chords) 0
				      10000)))))
	 (current-key-signature-accidentals (make-array 12 :initial-element nil))
	 (current-accidentals (make-array 12 :initial-element nil))
	 (current-durations (make-array 12 :element-type 'fixnum :initial-element 0))
	 (cancel-redundant-accidentals (not (redundant-accidentals score)))
	 (current-octaves (make-array 12 :element-type 'fixnum :initial-element -1)))

    (loop for object in (staff-data staff) do
      ;; if key signature, clear out everything and set new key sig
      ;; if bar line, clear out everything
      ;; if note, check cclass and sign, incr all durs, set current dur to 0, for all  durs>stickiness, clear sign
      ;; if chord get all signs in chord (for lookahead -- (notes g3 gs4)), then run note sequence on each
      ;; ties to the current note cancel this silliness

      (if (or (key-p object) 
	      (and (bar-p object) 
		   (not (eq implicit-style :paranoid))))
	  (loop for i from 0 below 12 do
	    (setf (aref current-accidentals i) nil)
	    (setf (aref current-durations i) 0)
	    (setf (aref current-octaves i) -1)))

      (if (key-p object)
	  ;; remember user-defined key signatures and :cancel as third
	  (let* ((accs (signature object))
		 (signs (if (and accs
				 (not (eq (third accs) :cancel)))
			    (if (eq (first accs) :special)
				(second accs)
			     (if (eq (first accs) sharp)
				 (subseq (list fs4 cs4 gs4 ds4 as4 es4 bs4) 0 (second accs))
			       (subseq (list bf4 ef4 af4 df4 gf4 cf4 ff4) 0 (second accs)))))))
	    ;; (print (format nil "accs:~A ~A" (loop for acc in signs collect (cclass acc)) signs))
	    (loop for i from 0 below 12 do
	      (setf (aref current-key-signature-accidentals i) nil))
	    (loop for acc in signs do
	      (setf (aref current-key-signature-accidentals (cclass acc)) (note-sign acc))))

	(when (or (note-p object) 
		  (chord-p object))
	  ;; here tied note should not cancel accidentals 
	  (if (and (not (member implicit-style '(:paranoid :old-style)))
		   (not (ties object)))
	      (loop for i from 0 below 12 do
		(when (> (aref current-durations i) implicit-duration)
		  (setf (aref current-durations i) 0)
		  (setf (aref current-accidentals i) nil))))

	  ;; first look for grace notes and appoggiaturas (on either notes or chords)
	  (if (marks object)
	      (let ((gn (find-if #'grace-note-p (marks object)))) ;grace-note-p covers appoggiaturas as well
		(when gn
		  ;; notes are in grace-data and can include chords...
		  (loop for gnote in (grace-data gn) do
		    (if (note-p gnote)
			(check-note-for-needed-natural 
			 gnote current-accidentals current-key-signature-accidentals current-durations 
			 cancel-redundant-accidentals current-octaves)
		      (if (chord-p gnote)
			  (check-chord-for-needed-naturals 
			   gnote (chord-data gnote) current-accidentals current-key-signature-accidentals current-durations 
			   cancel-redundant-accidentals current-octaves)))
		    (loop for i from 0 below 12 do (incf (aref current-durations i)))
		    (if (not (member implicit-style '(:paranoid :old-style)))
			(loop for i from 0 below 12 do
			  (when (> (aref current-durations i) implicit-duration)
			    (setf (aref current-durations i) 0)
			    (setf (aref current-octaves i) -1)
			    (setf (aref current-accidentals i) nil))))))))

	  (if (note-p object)
	      (check-note-for-needed-natural 
	       object current-accidentals current-key-signature-accidentals current-durations 
	       cancel-redundant-accidentals current-octaves)
	    (check-chord-for-needed-naturals 
	     object (chord-data object) current-accidentals current-key-signature-accidentals current-durations 
	     cancel-redundant-accidentals current-octaves))

	  (loop for i from 0 below 12 do (incf (aref current-durations i))))))))

#|
(cmn (automatic-naturals t) (implicit-accidental-style :old-syle) staff treble (meter 2 4) cs4 q c4 q c4 q cs4 q)
(def-key foo-major bf4 ef5 fs5)
(cmn (automatic-naturals t) (implicit-accidental-duration 1) 
     staff treble (key foo-major) c4 q cs4 q f4 q cs4 q (chord (notes c4 g4 gs5) q) e4 q e4 q g4 q)
(cmn (automatic-naturals t) staff treble ds4 q begin-tie d4 q end-tie)
(cmn (automatic-naturals t) staff treble (chord (notes ds4 gf4) q begin-tie) (chord (notes d4 g4) q end-tie))
(cmn (redundant-accidentals nil) staff treble (meter 4 4) cs4 q cs4 q cs4 q cs4 q cs4 q cs4 q cs4 q cs4 q )
(cmn (redundant-accidentals nil) (staff alto (meter 3 4) fs4 s d4 s cs4 s as3 s gs3 s e3 s ds3 s cs3 s cs4 q bar))
(cmn (redundant-accidentals nil) staff bar treble (chord e (notes b5 a4)) (chord s (notes ds6 a4)) (chord s (notes b5 a4))
      (chord q (notes c6 a4)) (chord s (notes cs6 a4)) (chord s (notes ds6 a4)) (chord e (notes dn6 a4)) (chord e (notes d6 a4)) bar)
|#


(defun fillify (score)
  (if (listp score)
      (let ((fixup-needed (some #'(lambda (n) (or (plusp (section-onset n)) (zerop (initial-onset n)))) (cdr score))))
	(loop for scr in score do
	  (if (automatic-bars scr) 
	      (map-over-staves #'(lambda (nscore staff)
				   (declare (ignore nscore))
				   (if (and (staff-data staff) (not (bar-p (first (last (staff-data staff))))))
				       (setf (staff-data staff) (append (staff-data staff) (list (bar (onset :at-end)))))))
			       scr)))
	(setf *cmn-measure-number* 1)
	(let ((scores (loop for scr in score collect (progn
						       (if (and (automatic-measure-numbers scr)
								(not (first-measure-number scr)))
							   (setf (first-measure-number scr) *cmn-measure-number*))
						       (fillify scr)))))
	  (loop for scr in scores do
		(map-over-staves #'(lambda (nscore staff)
				     (loop for obj in (staff-data staff) do (setf (visible-section obj) nscore)))
				 scr))
	  (if fixup-needed
	      (let ((next-onset 0)
		    (this-onset 0))
		(loop for scr in scores do
		  (let ((fixup (or (plusp (section-onset scr)) (zerop (initial-onset scr)))))
		    (if (plusp (section-onset scr))
			(setf this-onset (section-onset scr)))
		    (map-over-staves #'(lambda (nscore staff)
					 (declare (ignore nscore))
					 (loop for dat in (staff-data staff) do
					   (when fixup
					     (incf (onset dat) this-onset)
					     (incf (beat dat) this-onset))
					   (setf next-onset (max next-onset (+ (onset dat) (duration dat))))))
				     scr))
		  (setf this-onset next-onset))))
	  (score-engorge scores)))
    (progn
      (map-over-staves #'find-or-insert-clef score)
      (map-over-staves #'set-odb score)
      (let ((staff-ctr 0))
	(map-over-staves #'(lambda (score staff)
			     (incf staff-ctr)
			     (insert-bars-ties-and-rests score staff staff-ctr))
			 score))
      (map-over-staves #'set-clef-y0-and-lines score)
      (if (automatic-octave-signs score)
	  (map-over-staves #'insert-octave-signs score))
      (if (or (automatic-naturals score)
	      (not (redundant-accidentals score)))
	  (map-over-staves #'insert-naturals score))
      (if (automatic-measure-numbers score)
	  (insert-measure-numbers score))
      (if (automatic-beams score)
	  ;; this has to be done before the compactify pass so that we know where stem-up flagged notes are
	  (map-over-staves #'beamify-staff score))
      score)))

#|
(cmn (section (redundant-accidentals nil) (staff treble (meter 4 4) cs4 q cs4 q cs4 q cs4 q)) 
     (section (staff treble cs4 q cs4 q cs4 q cs4 q)) )

;; need to insert final bars if barred and section end on a bar and doesn't have one
(cmn (section (curvy-flags nil) 
	      (system (staff treble (meter 2 4) c4 e g4 q c4 e) (staff bass (meter 2 4) c3 h)) (staff alto (meter 2 4) c4 h))
     (section (curvy-flags t) 
	      (system (staff treble c4 e g4 q c4 e) (staff bass c3 h)) (staff alto c4 h)))
|#


(defun handle-hidden-sections ()
  (when *cmn-hidden-sections*
    (loop for hidden-section in *cmn-hidden-sections* do
      (initialize hidden-section)
      (setf (size hidden-section) (size *cmn-score*)))
    (setf *cmn-hidden-sections* nil)))


;;; boxify runs through the data getting bounding box and center point info, set beat for later alignment

(defun boxify (score)
  ;; at top level, everything is a system object.
  (let ((old-size (scr-size score)))
    (handle-hidden-sections)
    (setf (scr-size score) 1.0)
    (setf *old-cmn-score-size* *cmn-score-size*)
    ;; we need to know the true score size when justifying text (display method applied to text)
    ;;   if only the font-size has been specified (to get the staff-relative text size we have
    ;;   to divide the font-size by the score size).
    (setf *cmn-score-size* 1.0)
    (map-over-staves #'(lambda (score staff)
			 (loop for object in (staff-data staff) do
			   (house object score)))
		     score)
    (setf (scr-size score) old-size)
    (setf *old-cmn-score-size* nil)
    (setf *cmn-score-size* old-size))
  score)




;;; compactify lines up all the staves, does the first pass at collecting space information

(defun minimum-space (object)
  (+ (- (box-x1 object) (box-x0 object))
     (or (and (walls object) 
	      (+ (first (walls object))
		 (second (walls object))))
	 0.0)))

(defun wall-left-space (object)
  (or (first (walls object)) 0))

(defun rx-space (object)
  (- (box-x1 object) (box-x0 object)))

(defun wall-right-space (object)
  (or (second (walls object)) 0))

(defun fence-left-space (object)
  (or (first (fences object)) 0))

(defun fence-right-space (object)
  (or (second (fences object)) 0))

(defun expand-left-space (object)
  (or (first (expanders object)) 0))

(defun expand-right-space (object)
  (or (second (expanders object)) 0))

(defstruct (tld 
	    (:print-function
	     (lambda (d s k)
	       (declare (ignore k))
	       (format s "~%      (<tld> :time ~a ~a:x ~a :cx ~a :cx0 ~1,3f :cx1 ~1,3f :fx0 ~1,3f :fx1 ~1,3f :ex0 ~d :ex1 ~d ~
                                         ~a:bx0 ~a :bf0 ~a :be0 ~a :bar-x ~a :acc-x ~a)" 
		       (tld-time d) 
		       (if (tld-canceled-key d) " (canceled-key) " "")
		       (if (numberp (tld-x d)) (format nil "~1,3F" (tld-x d)) (tld-x d))
		       (if (numberp (tld-cx d)) (format nil "~1,3F" (tld-cx d)) (tld-cx d))
		       (tld-cx0 d) (tld-cx1 d)
		       (tld-fx0 d) (tld-fx1 d) (tld-ex0 d) (tld-ex1 d) 
		       (if (tld-type d) (format nil ":type ~A " (tld-type d)) "")
		       (tld-bx0 d) (tld-bf0 d) (tld-be0 d) (tld-bar-x d) 
		       (if (numberp (tld-acc-x d)) (format nil "~1,3F" (tld-acc-x d)) (tld-acc-x d))))))
  time x cx cx0 cx1 fx0 fx1 ex0 ex1 type bx0 bf0 be0 bar-x canceled-key acc-x)

;;; this structure tracks the white space around an object.
;;; the main notion is that in cmn certain objects have to be lined up vertically --
;;; note heads, bar lines, rests, some articulation marks, etc -- tld-cx points to the
;;; "center" of the object (this is not the graphical center -- it's the musical beat point).
;;; cx0 and cx1 are the left and right excursions of the object from the center point --
;;; justification routines should never write in this space.  the fx0 and fx1 are
;;; the desired white space bounds (if there's room) -- these requests are filled as
;;; soon as the absolute needs are filled.  if there's still room left over, the
;;; ex0 and ex1 values are the expansion requests.  time is the musical onset time
;;; and x is the left side of the cx style values (for complete compaction).
;;; When a line break is inserted at a bar, bx0, bf0, and be0 mark the white space
;;; before that bar.  Upon starting the next line, these are subtracted out, and the 
;;; overall line is fixed up for whatever is inserted.
;;;
;;; This procedure sometimes makes a mess of multi-staff music where the overlaps
;;; are actually on different staves.  The result tends to make runs ragged and
;;; uses more space than an engraver would.  To fix it, perhaps the loop in compactify
;;; should be divided in two, the first going through each staff by itself, then
;;; another loop setting true compacted points.  This would then need to be followed
;;; by a beat regularizer before letting the white spaces expand.  So, there would
;;; probably have to be fix ups all along the line from here to drawify -- a daunting
;;; prospect!  Another mistake in the current time-line is that the complications
;;; on the left of the musical beat can be extreme -- for example, we might have
;;; a clef, key signature, bar, and/or new meter, all before the first note of the bar.
;;; But to fix it requires either a smarter time-line (with multiple entries possible
;;; at any given time), or another division of the notion of white space -- a middle
;;; space, as it were.  Even then the bar line will be trouble -- I think I need
;;; time-line entries for left-objects left-bars middle-objects musical-objects.


(defun survey (object)			;relative to cur-pos left at 0
  (if (and (bar-p object)
	   (visible-justification object))
      (if (eq (visible-justification object) :left)
	  (make-tld :x 0 :cx 0 :cx0 0 :cx1 0 :fx0 0 :fx1 0 :ex0 0 :ex1 0)
	(if (eq (visible-justification object) :right)
	    (make-tld :x 0 :cx 0 :cx0 .1 :cx1 0 :fx0 .1 :fx1 0 :ex0 1 :ex1 0))) ;was ex0 1 (16-nov), ex1 0 (24-mar), ex0 2 (11-Dec) etc
    (if (and (clef-p object)
	     (eq (visible-justification object) :none))
	(make-tld :x 0 :cx 0 :cx0 0 :cx1 0 :fx0 0 :fx1 0 :ex0 0 :ex1 0 :type nil)
      (let* ((xc (or (center object) 0))
	     (wl-x0 (+ (wall-left-space object) (- xc (box-x0 object)))))
	(make-tld :x 0 
		  :cx wl-x0
		  :cx0 wl-x0
		  :cx1  (+ (wall-right-space object) (- (box-x1 object) xc))
		  :fx0 (fence-left-space object)
		  :fx1 (fence-right-space object)
		  :ex0 (expand-left-space object)
		  :ex1 (expand-right-space object)
		  :canceled-key (canceled-key-p object)
		  :type (if (bar-p object) :bar))))))

(defun scale-tld (td scl)
  (when (/= scl 1.0)
    (setf (tld-cx td) (* scl (tld-cx td)))
    (setf (tld-cx0 td) (* scl (tld-cx0 td)))
    (setf (tld-cx1 td) (* scl (tld-cx1 td)))
    (setf (tld-fx0 td) (* scl (tld-fx0 td)))
    (setf (tld-fx1 td) (* scl (tld-fx1 td)))
    (setf (tld-ex0 td) (* scl (tld-ex0 td)))
    (setf (tld-ex1 td) (* scl (tld-ex1 td))))
  td)

#|
;;; test scaled staff fixup
   (cmn (staff (staff-size 1.25) treble (meter 2 4) c4 e c4 e c4 e c4 e bar whole-rest bar c4 e c4 e c4 e c4 e) 
        (staff (staff-size .50) bar bass (meter 2 4) whole-rest bar c4 e c4 e c4 e c4 e bar whole-rest bar))
|#

(defun compactify (score)
  (declare (optimize (speed 3) (safety 0)))
  ;; we need to run through all systems together setting min-locs for all non-overlappable glyphs
  ;; time lines are in (onset object), centered graphically at (center object)
  ;; bounded by a box of size (box object).
  ;; min-loc is stored in (x0 object)

  (let ((time-line nil)
	(staves 0))
    (map-over-staves #'(lambda (scre stff) 
			 (declare (ignore scre stff)) 
			 (incf staves)) 
		     score)
    (let* ((staffs (make-array staves))	;staves to keep track of in parallel
	   (sizes (make-array staves))
	   (x 0)
	   (last-note-onset -1)
	   (t-type nil)
	   (time 0)
	   (i 0))
      (map-over-staves #'(lambda (score stf)
			   (declare (ignore score))
			   (setf (aref staffs i) (staff-data stf))
			   (setf (aref sizes i) (or (staff-size stf) 1.0)) ;tied-to staves??
			   (incf i))
		       score)
      
      (loop for i from 0 below staves do
	(if (and (first (aref staffs i))
		 (metrical-bar-p (first (aref staffs i))))
	    (setf (visible-justification (first (aref staffs i))) :left)))
      
      (loop while (notevery #'null staffs) do
	;; pos is the current position of whatever we find in the data list
	(let ((max-cx0 0)
	      (max-cx1 0)
	      (max-ex0 0)
	      (max-ex1 0)
	      (max-fx0 0)
	      (max-fx1 0)
	      (max-cx 0)
	      (bx0 0)
	      (bf0 0)
	      (be0 0)
	      (found-canceled-key nil)
	      (overall-max-ncx0 0))
	  
	  (setf time (or (loop for i from 0 below staves 
			   if (aref staffs i)
			   minimize (odb-onset (first (aref staffs i))))
			 0))
	  
	  (setf t-type nil)
	  
	  (loop for i from 0 below staves do
	    (let ((cur-pos x)
		  (cx0 0)
		  (cx1 0)
		  (ex0 0)
		  (ex1 0)
		  (fx0 0)
		  (fx1 0)
		  (ncx0 0)
		  (ncx1 0)
		  (nfx0 0)
		  (nfx1 0)
		  (nex0 0)
		  (nex1 0))
	      (loop while (and (aref staffs i)
			       (= time (odb-onset (first (aref staffs i))))) do
		(let* ((cobj (first (aref staffs i)))
		       (cobjx (scale-tld (survey cobj) (aref sizes i))))
		  (setf found-canceled-key (or found-canceled-key (tld-canceled-key cobjx)))
		  (if (and (not t-type) (eq (tld-type cobjx) :bar)) (setf t-type :bar))
		  (if (or (audible-p cobj) (rest-p cobj))
		      (progn
			(setf last-note-onset (odb-onset cobj))
			
			(setf nex0 (max nex0 (tld-ex0 cobjx)))
			(setf nex1 (max nex1 (tld-ex1 cobjx)))
			
			(setf nfx0 (max nfx0 (tld-fx0 cobjx)))
			(setf nfx1 (max nfx1 (tld-fx1 cobjx)))
			
			(setf ncx0 (max ncx0 (tld-cx0 cobjx)))
			(setf ncx1 (max ncx1 (tld-cx1 cobjx)))
			
			(setf (aref staffs i) (cdr (aref staffs i))))
		    
		    (let ((barp (bar-p cobj)))
		      (incf ex0 (+ ex1 (tld-ex0 cobjx)))
		      (setf ex1 (tld-ex1 cobjx))
		      
		      (incf fx0 (+ fx1 (tld-fx0 cobjx)))
		      (setf fx1 (tld-fx1 cobjx))
		      
		      (incf cx0 (+ cx1 (tld-cx0 cobjx)))
		      (setf cx1 (tld-cx1 cobjx))
		      
		      (when barp 
			(setf found-canceled-key nil) ;this can be confused by one staff with a canceled-key followed by one without
			(setf bx0 (max bx0 cx0))
			(setf bf0 (max bf0 fx0))
			(setf be0 (max be0 ex0)))
		      
		      (if (or (and (not barp) 
				   (not (clef-p cobj)))
			      (not (member (visible-justification cobj) '(:none :left))))
			  (incf cur-pos (minimum-space cobj)))
		      
		      (setf (aref staffs i) (cdr (aref staffs i)))
		      
		      (if (and barp 
			       (not (aref staffs i)) 
			       (/= (odb-onset cobj) last-note-onset))
			  (setf (visible-justification cobj) :right))))))
	      ;; if the onsets are the same, this bar is at the start of the last measure
	      
	      (setf overall-max-ncx0 (max overall-max-ncx0 ncx0))
	      ;; if clefs in midline. we need to leave room in the subsequent (possible) bar's
	      ;;  white space for the largest note white-space needed in all the staves
	      
	      (setf max-ex0 (max max-ex0 (+ ex0 ex1 nex0))) ;assume any non-audible at the current time is before the musical data
	      (setf max-ex1 (max max-ex1 nex1))
	      (setf max-fx0 (max max-fx0 (+ fx0 fx1 nfx0)))
	      (setf max-fx1 (max max-fx1 nfx1))
	      (setf max-cx (max max-cx (+ overall-max-ncx0 cur-pos)))
	      (setf max-cx0 (max max-cx0 (+ cx0 cx1 overall-max-ncx0)))
	      (setf max-cx1 (max max-cx1 ncx1))))
	  
	  (push (make-tld :time time	;bare list is hard to remember
			  :x x
			  :cx max-cx
			  :cx0 max-cx0
			  :cx1 max-cx1
			  :ex0 max-ex0
			  :ex1 max-ex1
			  :fx0 max-fx0
			  :fx1 max-fx1
			  :bx0 bx0
			  :bar-x bx0
			  :bf0 bf0
			  :be0 be0
			  :canceled-key found-canceled-key
			  :type t-type)
		time-line)
	  (incf x (+ max-cx0 max-cx1))))
      (setf (time-line score) (nreverse time-line))
      (setf (staves score) staves)))
  (if (spacing-hook score) (funcall (spacing-hook score) score))
  score)




;;; linify takes the compacted form, looks for line breaks (user can explicitly say (line-mark)),
;;; inserts line breaks wherever a good break is found.  (fix up line starts and ends -- clefs, key sigs, ties, bars, etc)

(defun make-right-bar (td)
  (let ((new-tld (survey (make-instance 'bar :justification :right))))
    (setf (tld-cx0 new-tld) (tld-bx0 td))
    (decf (tld-cx0 td) (tld-bx0 td))
    (setf (tld-bx0 td) 0)
    (setf (tld-fx0 new-tld) (tld-bf0 td))
    (decf (tld-fx0 td) (tld-bf0 td))
    (setf (tld-bf0 td) 0)
    (setf (tld-ex0 new-tld) (tld-be0 td))
    (decf (tld-ex0 td) (tld-be0 td))
    (setf (tld-be0 td) 0)

    (when (tld-canceled-key td)
      (setf (tld-cx1 new-tld) (max .2 (- (tld-cx0 td) .4)))
      (setf (tld-cx0 td) (max .4 (- (tld-cx0 td) (tld-cx1 new-tld)))))
    (setf (tld-x new-tld) (tld-x td))
    (setf (tld-cx new-tld) (+ (tld-x new-tld) (tld-cx0 new-tld)))
    (incf (tld-x td) (tld-cx0 new-tld))

    (when (tld-canceled-key td)
      (incf (tld-x td) (tld-cx1 new-tld)))
    (setf (tld-time new-tld) (tld-time td))
    (setf (tld-type new-tld) :line)
    new-tld))

(defun amalgamate-line-data (old-line new-line)
  ;; merge dx/dy data fields
  ;; dx is a list of (beat offset) pairs, ordered by beat, if beats= -> error
  ;; dy is a list of (staff offset) pairs, order by staff, if = weird!! -> cmn error
  (flet ((merge-dxy (l0 l1)
	   (remove-duplicates (merge 'list l0 l1 #'< :key #'first) :test #'= :key #'first)))
    (make-instance 'line 
      :staff-y0 (staff-y0 old-line)
      :line-staff (line-staff old-line)
      :dx (or (and (list-p (vis-dx old-line))
		   (list-p (vis-dx new-line))
		   (merge-dxy (vis-dx old-line) (vis-dx new-line)))
	      (vis-dx old-line)
	      (vis-dx new-line))
      :dy (or (and (list-p (vis-dy old-line))
		   (list-p (vis-dy new-line))
		   (merge-dxy (vis-dy old-line) (vis-dy new-line)))
	      (vis-dy old-line)
	      (vis-dy new-line)))))

(defun linify-without-added-breaks (score score-lines score-pages)
  (let ((new-time-line nil)
	(time-lines nil)
	(sl score-lines)
	(sp score-pages))
    (loop for td in (time-line score) do
      (when (and sl (>= (tld-time td) (odb-onset (first sl))) (pop sl))
	(when (eq (tld-type td) :bar) 
	  (push (make-right-bar td) new-time-line)
	  (setf (tld-type td) nil))
	(let ((new-line-mark (make-instance 'line :type (if (and sp (>= (tld-time td) (odb-onset (first sp))) (pop sp)) :page :line)
					:onset (tld-time td))))
	  (push new-line-mark new-time-line)
	  (push new-line-mark time-lines)))
      (push td new-time-line))
    (push (make-instance 'line :onset (tld-time (first new-time-line)) :type :line) time-lines)
    (setf (time-line score) (nreverse new-time-line))
    (setf (lines score) (length time-lines))
    (setf (time-lines score) (reverse time-lines))))

(defun total-up-x-and-fx (line-data)
  (let ((cur-fx 0.0)
	(bars nil))
    (loop for td in line-data do
      (incf cur-fx (+ (tld-fx0 td) (tld-fx1 td)))
      (setf (tld-acc-x td) (+ (tld-cx td) cur-fx))
      (when (eq (tld-type td) :bar) 
	(push (tld-acc-x td) bars)))
    (nreverse bars)))

(defun linify-with-added-breaks (score score-lines score-pages)
  (let* ((time-lines nil)
	 (new-time-line nil)
	 (last-bar-onset 0)
	 (line-length (inches-to-ps (- (page-width score) (+ (right-margin score) (left-margin score)))))
	 (line-length-scaled (/ line-length (or *cmn-score-size* (scr-size score))))
	 (expansion-factor (free-expansion-factor score))
	 (section-start-x 0.0)
	 (section-end-x -100.0)
	 (section-current-line-end-x 0.0)
	 (section-fuzz-factor 0.0)
	 (section-lines 0)
	 (section-line-x 0.0)
	 (line-first-x 0.0)
	 (line-first-cx 0.0)
	 (sl score-lines)
	 (sp score-pages)
	 (bars (total-up-x-and-fx (time-line score))))
    (loop for td in (time-line score) and tdl on (time-line score) do
      (let ((time (tld-time td))
	    (x (tld-acc-x td))
	    (cx (tld-cx td))
	    (typ (tld-type td)))
	(when (<= section-end-x x)
	  ;; here we scan forward checking the current section (i.e. the time-line data between the
	  ;;   current user-specified line/page-breaks, if any), to see if any extra line-breaks
	  ;;   need to be inserted.
	  (setf section-start-x x)
	  (setf line-first-x x)
	  (setf line-first-cx cx)
	  (let ((slptr tdl)
		(end-time (if sl (odb-onset (first sl)) 100000.0)))
	    (loop while (and slptr (cdr slptr) (< (tld-time (first slptr)) end-time)) do (setf slptr (cdr slptr)))
	    (setf section-end-x (tld-acc-x (first slptr)))
	    (if (> section-end-x section-start-x)
		(let ((space-needed (* expansion-factor (- section-end-x section-start-x))))
		  (setf section-lines (fceiling space-needed line-length-scaled))
		  (if (> section-lines 1) ;look for very extreme rounding cases (like 1.01 => 2)
		      (let ((expfrac (- expansion-factor (floor expansion-factor))))
			(if (< 0.0 expfrac .5) 
			    ;;if expansion-factor is exactly 1, we have to use ceiling -- otherwise 
			    ;; the music trails off the end of the line.
			    (multiple-value-bind
				(int frac)
				(ffloor space-needed line-length-scaled)
			      (when (and (/= int section-lines) 
					 (< frac (* .5 expfrac)))
				(setf section-lines int))))))
		  (setf section-line-x (/ (- section-end-x section-start-x) section-lines))
		  (setf section-fuzz-factor (* (if bars .4 .25) section-line-x))
		  (setf section-current-line-end-x (+ section-start-x section-line-x)))
	      (setf section-lines 1))))
	(when (eq typ :bar) 
	  (setf bars (cdr bars))
	  (setf last-bar-onset time))
	(when (or (and sl
		       (>= time (odb-onset (first sl)))
		       (pop sl))
		  (and (> section-lines 1)
		       (cddr tdl)
		       (< section-current-line-end-x (+ x section-fuzz-factor))
		       (or (and (eq typ :bar)
				(or (not bars);it's the last bar, I think
				    (> (* expansion-factor (- x line-first-x)) line-length-scaled)
				    (> (- (first bars) line-first-x) line-length-scaled)))
			   ;; "first bars" is the upcoming bar, not the current one -- this is a very
			   ;;   conservative check, perhaps too much so.
			   (and (or (= time (floor time))
				    (= (- time last-bar-onset) 
				       (floor (- time last-bar-onset))))
				(or (not bars)
				    (> (- (first bars) line-first-x) line-length-scaled)))
				    ;; that is, either there aren't any bars or the next bar is too far away anyway
				    ;; the rest of this "or" branch is looking for extreme cases of measures
				    ;;   that won't fit all on one line.  It has cost much toil and cursing...
			   (< line-length-scaled (- cx line-first-cx)))))
	  ;; time to insert a line-break
	  (when (eq typ :bar) 
	    (push (make-right-bar td) new-time-line)
	    (setf (tld-type td) nil))
	  (let ((new-line-mark (make-instance 'line :type (if (and sp (>= time (odb-onset (first sp))) (pop sp)) :page :line)
					  :onset (tld-time td))))
	    (push new-line-mark new-time-line)
	    (push new-line-mark time-lines))
	  (setf line-first-x x)
	  (setf line-first-cx cx)
	  (incf section-current-line-end-x section-line-x)
	  (if (>= (+ section-current-line-end-x section-fuzz-factor) section-end-x)
	      (setf section-fuzz-factor 0.0)))
	(push td new-time-line)))
    (push (make-instance 'line :onset (tld-time (first new-time-line)) :type :line) time-lines)
    (setf (time-line score) (nreverse new-time-line))
    (setf (lines score) (length time-lines))
    (setf (time-lines score) (reverse time-lines))))

(defun linify (score)
  ;; run down the time line looking for good line break positions -- insert :line wherever one is found
  ;; if user specified any lines/pages, they are in (lines score) and (pages score) in no particular order
  (flet ((pad-lines (pages lines)
	   (let ((new-lines lines))
	     (loop for page in pages do
	       (push (make-instance 'line :onset (odb-onset page)) new-lines))
	     new-lines))
	 (fixup-line-data (ln)
	   ;; dx should already be ok, dy can be a number or a list staff num or a list of lists
	   (if (listp (dy ln))
	       (if (not (listp (first (dy ln))))
		   (setf (dy ln) (list (dy ln))))
	     (if (not (zerop (dy ln)))
		 (setf (dy ln) (list (list (line-staff ln) (dy ln))))))
	   ln))
    (if (not (time-line score))
	(progn
	  (setf (lines score) 1)
	  (push (make-instance 'line :onset 0 :type :line) (time-lines score))
	  (setf (time-line score) (list (make-tld :time 0 :x 0 :cx 0 :cx0 0 :cx1 0 :fx0 0 :fx1 0 :ex0 0 :ex1 0))))
      (let ((score-lines (lines score))
	    (score-pages (pages score)))
	(if score-lines
	    ;; set up line-data, accumulating all =onset line instances into one, and all dx/dy per given line into one
	    (loop for ln in score-lines do
	      (let ((onset (odb-onset ln))
		    (data (fixup-line-data ln)))
		(if (line-data score)
		    (let ((old-line (find onset (line-data score) :key #'odb-onset)))
		      (if old-line
			  (amalgamate-line-data old-line data)
			(push data (line-data score))))
		  (push data (line-data score))))))
	(if (or score-lines score-pages)
	    (setf score-lines 
	      (sort (remove-duplicates (pad-lines score-pages score-lines) 
				       :test #'= 
				       :key #'odb-onset) 
		    #'< :key #'odb-onset)))
	(if score-pages 
	    (setf score-pages 
	      (sort (remove-duplicates score-pages 
				       :test #'= 
				       :key #'odb-onset) 
		    #'< :key #'odb-onset)))
	(if (and score-lines (zerop (odb-onset (first score-lines)))) 
	    (setf score-lines (cdr score-lines)))
	(if (automatic-line-breaks score)
	    (linify-with-added-breaks score score-lines score-pages)
	  (linify-without-added-breaks score score-lines score-pages)))))
  score)



;;; pagify takes the lined (still compacted) list of systems and puts in page breaks

;;; staff-justification: {for convenience these can be set on the score or system and passed on down)
;;;    nil always display all "true" staves
;;;    t = only display on lines where staff has notes
;;;    :old-style = t with all staves displayed on first line
;;;    :new-style = partial staves
;;;    '((a b) (c d)...) = displayed only between times a and b, c and d, ...

(defun set-up-line-list (score)	
  (append
   (list (or (initial-onset score) 0.0))
   (loop for ln in (time-lines score) collect (odb-onset ln))))

(defvar *cmn-new-style-staff-hysteresis* 1)

(defun set-up-staff-times (score staff)
  (let ((just (justification staff)))
    (if (and just
	     (not (eq just :locked))) ;nil = all times displayed, otherwise build the list of visible times
	(if (listp just)		;list of times = believe whatever user set it to
	    (setf (times staff) just)
	  (let* ((visible-times nil)	; justification = :new-style, :old-style, or t
		 (beg nil)
		 (end nil))
	    (if (eq just :new-style)	; here we ignore line onsets since we don't care about full line portions
		(let ((bar-beg nil))
		  (loop for obj in (staff-data staff) do
		    (if (metrical-bar-p obj) 
			(setf bar-beg (odb-onset obj))
		      (if (rest-p obj)
			  (if (and beg end (> (odb-onset obj) (+ end *cmn-new-style-staff-hysteresis*)))
			      (progn
				(push (list (or beg 0) (or bar-beg end 0)) visible-times)
				(setf beg nil)
				(setf end nil)))
			(if (audible-p obj)
			    (progn
			      (if (not beg) (setf beg (or bar-beg (odb-onset obj))))
			      (setf end (max (or end -1) (+ (odb-onset obj) (odb-duration obj)))))))))
		  (if (and beg end) (push (list beg (max end (or bar-beg end))) visible-times)))
	      (let* ((line-times (set-up-line-list score)) ;:old-style or t go for full lines
		     (next-line-time (second line-times)))
		(if (not (eq just :old-style)) ; if t include initial time no matter what
		    (progn
		      (setf beg (first line-times))
		      (setf end (second line-times))))
		(loop for obj in (staff-data staff) do
		  (when (not (invisible-p obj))
		    (when (>= (odb-onset obj) next-line-time)
		      (if (and beg end)
			  (if (<= end (first line-times))
			      (progn
				(push (list beg end) visible-times)
				(setf beg nil)
				(setf end nil))))
		      (loop while (and line-times (>= (odb-onset obj) next-line-time)) do 
			(setf line-times (cdr line-times))
			(setf next-line-time (or (second line-times) 10000))))
		    (if (or (audible-p obj) (rest-p obj))
			(progn
			  (if (not beg) (setf beg (first line-times)))
			  (setf end (max (or end next-line-time) (+ (odb-onset obj) (odb-duration obj))))))))
		(if (and beg end) (push (list beg end) visible-times))))
	    (setf (times staff) (nreverse visible-times)))))))

(defun active-time (beg end times)	;line beg and end, times contain all staff (beg end) pairs
  (and times
       (let ((staff-beg (+ .001 (first (first times))))
	     (staff-end (- (second (first times)) .001)))
	 (or (and (<= beg staff-beg)
		  (>= end staff-beg))
	     (and (<= staff-beg beg)
		  (>= staff-end beg))
	     (active-time beg end (cdr times))))))

(defun active-staff (staff beg end)
  (or (not (justification staff))
      (eq (justification staff) :locked)
      (active-time beg end (times staff))))

(defun active-staves (score beg end)
  (let ((true-staves nil))		;count tied staves only if the main staff would otherwise be left out
    (map-over-staves #'(lambda (score staff)
			 (declare (ignore score))
			 (if (active-staff staff beg end)
			     (pushnew (if (true-staff staff)
					  (absolutely-true-staff staff)
					staff)
				      true-staves)))
		     score)
    true-staves))

(defun active-systems (score active-staves)
  (let ((ctr 0))
    (loop for sys in (systems score) do
      (if (intersection (staves sys) active-staves) (incf ctr)))
    ctr))

(defun line-height (score beg end)
  (let* ((actual-staves (active-staves score beg (if (= beg end) (+ beg 1.0) end)))
	 (staves (length actual-staves))
	 (systems (active-systems score actual-staves)))
    (+ staves
       (if (and (title score) (= beg (initial-onset score))) (title-space (title score)) 0)
       (line-separation score)
       (* (1- staves) (staff-separation score))
       (* (1- systems) (system-separation score))
       (let ((sum 0))
	 (map-over-staves #'(lambda (score staff)
			      (declare (ignore score))
			      (incf sum (abs (dy staff))))
			  score)
	 sum))))

(defun current-line-fixup (score line-onset)
  ;; line-data score is a list of line instances (as supplied by the caller)
  ;; beats are numbered from 1, staves are numbered from 1
  ;; here we are being asked for the full fixup of all the staves in the current line
  (if (line-data score)
      (let ((line-dat (find-if #'(lambda (ln) 
				   (and (= (odb-onset ln) line-onset)
					(list-p (dy ln))))
			       (line-data score))))
	(if line-dat
	    (- (apply #'+ (loop for dat in (dy line-dat) collect (second dat))))
	  0))
    0))


(defun pagify (score)
  ;; run down the time line inserting page breaks (i.e. changing an occasional :line to :page)
  ;; first pass down any score/system justifications
  (let ((line-h 0))
    (if (= (staves score) 1) (setf (visible-justification score) nil))
    (if (visible-justification score) 
	(loop for sys in (systems score) do
	  (if (not (visible-justification sys)) (setf (visible-justification sys) (visible-justification score)))))
    (loop for sys in (systems score) do
      (if (visible-justification sys) 
	  (loop for stf in (staves sys) do
	    (if (not (visible-justification stf)) (setf (visible-justification stf) (visible-justification sys))))))
    (map-over-staves #'set-up-staff-times score)
    ;; now all the (times staff) fields are ready for calculating the running line-heights
    (let* ((full-page-h (inches-to-font-units score (- (page-height score) (+ (header-margin score) (footer-margin score)))))
	   (too-much-of-page-h (* .25 full-page-h)) ;try to cram them more tightly to avoid huge blank spaces
	   (line-sep (line-separation score))
	   (pages 1)
	   (beg (or (initial-onset score) 0.0))
	   (page-h full-page-h)
	   (end nil)
	   (last-tlind nil))
      (loop for tlind in (time-lines score) do 
	(setf end (odb-onset tlind))
	(let* ((fix-up (current-line-fixup score beg)))
	  (setf line-h (+ (line-height score beg end) fix-up))
	  (setf (size tlind) line-h)
	  (if (> line-h full-page-h)
	      (cmn-warn "we don't have room on the page for this score: try a font size less than ~d" 
			(round (* (scr-size score) (/ full-page-h line-h))))
	    (if (eq (line-type tlind) :page)
		(progn
		  (setf (line-space tlind) (- page-h line-h))
		  (setf page-h full-page-h)
		  (incf pages))
	      (if (or (and (> line-h page-h)	;gotta back up one
			   (or (< page-h too-much-of-page-h)  ; not too much white space left
			       (> line-h (+ page-h line-sep)))) ;line is too big to cram in
		      (and last-tlind (one-line-per-page score)))
		  (progn
		    (setf (line-type last-tlind) :page)
		    (setf (line-space last-tlind) page-h)
		    (setf page-h (- full-page-h line-h))
		    (incf pages))
		(decf page-h line-h)))))
	(setf last-tlind tlind)
	(setf beg end))
      (when (and last-tlind 
		 (or (not (line-space last-tlind))
		     (one-line-per-page score)))
	(setf (line-type last-tlind) :page)
	(setf (line-space last-tlind) (min page-h (max 4.0 line-h))))
      (setf (pages score) pages)
      score)))




;;;
;;; -------- justify
;;;
;;; justify takes each line (system of staves) and adds in white space to fill out the line,
;;; keeping all center lines aligned, of course.

(defun find-clef (objects)
  (loop for object in objects do
    (if (clef-p object) (return-from find-clef object))))

(defun effective-length (name)
  (let ((len (length name)))
    (if (> len 8)
	(if (string-equal "-flat" (subseq name 1 6))
	    (- len 5)
	  (if (string-equal "-sharp" (subseq name 1 7))
	      (- len 6)
	    len))
      len)))

(defun rx-length (score name)
  (if (text-p name)
      (* (length (letters name)) .45 
	 (if (font-size name) 
	     (/ (font-size name) (scr-size score)) 
	   (if (= (font-scaler name) 1.0)
	       (staff-name-font-scaler score)
	     (font-scaler name))))
    (* (effective-length name) .45 (staff-name-font-scaler score))))

(defun maximum-staff-name-room (score names brackets braces)
  (or (loop for i from 0 below (length names)
       maximize (room-for-staff-name score (aref names i) (aref brackets i) (aref braces i)))
      0))

(defun room-for-staff-name (score name bracket brace)
  (+ (rx-length score name)
     .25
     (if bracket .4 0)
     (if brace .5 0)))

(defun check-for-name (score marks name bracket brace)
  (if (not name)
      marks
    (let* ((mark (find-if #'(lambda (n) 
			      (and (sundry-p n)
				   (eq :staff-name (sundry-name n))))
			  marks)))
      (if mark
	  (setf (box-x0 mark) (- (+ .25 (room-for-staff-name score name bracket brace)))))
      marks)))

(defun find-time-line-position-of (time leftx rightx line)
  (loop for tx0 in line and tx1 in (cdr line) do
    (if (and (tld-p tx0) (tld-p tx1))
	(if (<= (tld-time tx0) time (tld-time tx1))
	    (return-from find-time-line-position-of 
	      (+ leftx 
		 (tld-cx tx0) 
		 (* (- (tld-cx tx1) (tld-cx tx0))
		    (divide (- time (tld-time tx0))
		       (- (tld-time tx1) (tld-time tx0)))))))))
  rightx)

(defun found-staff-name (score)
  (map-over-staves #'(lambda (score stf)
		       (declare (ignore score))
		       (if (staff-name stf) (return-from found-staff-name (staff-name stf))))
		   score)
  nil)

(defun bracket-also (brackets ys y1s i)
  (let ((lim (length brackets))
	(y (aref ys i)))
    (loop for j from i below lim do
      (if (and (aref brackets j) 
	       (aref y1s j)
	       (>= (aref y1s j) y))
	  (return-from bracket-also t)))
    nil))

(defun fixup-brackets-and-braces 
    (score brackets local-braces ys y1s new-staff-data i cur-x stfy0 cur-y-scaled left-bar actual-staffs pm last-staff-number)
    (declare (ignore score))
  (when (aref brackets i)
    (let ((new-bracket (copy (aref brackets i))))
      (if (bracket-p new-bracket)
	  (setf (box-x0 new-bracket) cur-x)
	(setf (box-x0 new-bracket) (- cur-x *brace-space*)))
      (setf (box-y0 new-bracket) stfy0)
      (setf (box-y1 new-bracket) (+ cur-y-scaled (aref y1s i)))
      (push new-bracket (aref new-staff-data i))))
  (when (and (aref local-braces i)
	     (/= i 0))
    (let* ((new-brace (copy (aref local-braces i)))
	   ;; here we also have to take into account cases where one or the other staff is inactive for a line
	   (index (1+ (- i (brace-staves new-brace)))))
      (when (minusp index)
	(warn "weird -- brace-staves is ~D but it's starting from staff ~D" (brace-staves new-brace) i)
	(setf index i))
      (when (> (brace-staves new-brace) 2)
	(let ((dummies 0)
	      (reals 1))
	  (loop for k from 1 to i while (< reals (brace-staves new-brace)) do
	    (if (and (aref actual-staffs (- i k)) (true-staff (aref actual-staffs (- i k)))) 
		(incf dummies)
	      (incf reals)))
	  (decf index dummies)))
      (loop while (and (> index 0)
		       (aref actual-staffs index)
		       (true-staff (aref actual-staffs index))) do
	(decf index))			;move up the list looking for the first actual (not tied) staff
      (when (aref ys index)		;include the brace only if the higher staff is active (should this be on a switch?)
	(setf (box-x0 new-brace) (- cur-x *brace-space* (if (bracket-also brackets ys y1s i) .3 0)))
	(setf (box-y0 new-brace) stfy0)
	(setf (box-y1 new-brace) (+ cur-y-scaled 
				    (aref ys index) 
				    (or (staff-size (aref actual-staffs index)) 1.0)))
	(push new-brace (aref new-staff-data i)))))
  (if (and (or left-bar pm) (= i last-staff-number))
      (let ((left-line (copy bar)))
	(setf (visible-justification left-line) :left)
	(setf (box-x0 left-line) cur-x)
	(setf (box-y0 left-line) stfy0)
	(if (not left-bar) (setf (matrix left-line) (list 0 0 0 0 0 0)))
	(setf (box-y1 left-line) (+ cur-y-scaled (or (aref y1s 0) (find-if #'numberp y1s) 0.0)))
	(push left-line (aref new-staff-data i)))))

#|
;;; a test of the tied-to local brace stuff:
 (cmn (size 60)
      (staff (staff-size .5) treble (engorge (loop for i from 0 to 18 collect (c4 q))))
      (setf hi (staff treble (engorge (loop for i from 0 to 18 collect (d4 q)))) )
      (staff (tied-to hi) (treble invisible) g4 q)
      (staff (staff-size .75) (brace (brace-staves 3)) bass (engorge (loop for i from 0 to 18 collect (e4 q)))))
|#

(defun line-data-x (current-line cur-dx)
  ;; run through current-line fixing up all x and cx based on cur-dx data
  ;; all x and cx numbers are in staff-units, times are in "beats" (i.e. quarters)
  ;; beats are numbered from 1
  (when (and current-line (list-p cur-dx))
    (let ((time0 (tld-time (first current-line)))
	  (lastx (tld-x (first (last current-line))))
	  (xs nil))
      (loop for dx in cur-dx do
	(push (list (find-time-line-position-of (+ time0 (1- (first dx))) 0 lastx current-line) (second dx)) xs))
      (setf xs (nreverse xs))
      (let* ((nx (pop xs))
	     (px0 0)
	     (ux0 0)
	     (px1 (first nx))
	     (ux1 (+ px1 (second nx)))	;no px1 if not relative
	     (scale (divide ux1 px1)))
	(loop for td in current-line do
	  (when (> (tld-x td) px1)
	    (setf nx (pop xs))
	    (setf px0 px1)
	    (setf px1 (if nx (first nx) lastx))
	    (setf ux0 ux1)
	    (setf ux1 (if nx (+ px1 (second nx)) lastx)) ; ditto
	    (setf scale (divide (- ux1 ux0) (- px1 px0))))
	  (setf (tld-x td) (+ ux0 (* scale (- (tld-x td) px0))))
	  (setf (tld-cx td) (+ ux0 (* scale (- (tld-cx td) px0)))))))))


(defun compactify-first-onset (data scl)
  (when data
    (let ((first-onset (odb-onset (first data)))
	  (happy t)
	  (cx0 0)
	  (cx1 0)
	  (ex0 0)
	  (ex1 0)
	  (fx0 0)
	  (fx1 0)
	  (ncx0 0)
	  (ncx1 0)
	  (nfx0 0)
	  (nfx1 0)
	  (nex0 0)
	  (nex1 0))
      (loop for cobj in data while happy do
	(if (= first-onset (odb-onset cobj))
	    (let ((cobjx (scale-tld (survey cobj) scl)))
	      (if (or (audible-p cobj) (rest-p cobj))
		  (progn
		    (setf nex0 (max nex0 (tld-ex0 cobjx)))
		    (setf nex1 (max nex1 (tld-ex1 cobjx)))
		    (setf nfx0 (max nfx0 (tld-fx0 cobjx)))
		    (setf nfx1 (max nfx1 (tld-fx1 cobjx)))
		    (setf ncx0 (max ncx0 (tld-cx0 cobjx)))
		    (setf ncx1 (max ncx1 (tld-cx1 cobjx))))
		(progn
		  (incf ex0 (+ ex1 (tld-ex0 cobjx)))
		  (setf ex1 (tld-ex1 cobjx))
		  (incf fx0 (+ fx1 (tld-fx0 cobjx)))
		  (setf fx1 (tld-fx1 cobjx))
		  (incf cx0 (+ cx1 (tld-cx0 cobjx)))
		  (setf cx1 (tld-cx1 cobjx)))))
	  (setf happy nil)))
      
      (make-tld :cx (+ cx0 cx1 ncx0)
		:cx0 (+ cx0 cx1 ncx0)
		:cx1 ncx1
		:ex0 (+ ex0 ex1 nex0)
		:ex1 nex1
		:fx0 (+ fx0 fx1 nfx0)
		:fx1 nfx1))))


(defun regularize-time-line (score fx-factor ex-factor current-line)

  ;; this is a very conservative function -- it tries to find slightly irregular
  ;; steady-rhythm runs, gets the possible regular spacing, then looks
  ;; again to see that there's no possibility of collisions.
  ;; If we had the individual staff spacing needs, we could make this
  ;; much smarter.

  (when (and (>= fx-factor 1.0)
	     (plusp ex-factor)
	     (> (staves score) 1)
	     (> (length current-line) 3))
    (let* ((regulars nil)
	   (cur-incr 0)
	   (triplets nil)
	   (regnum (or (and (regularize score) 
			    (numberp (regularize score)) 
			    (regularize score))
		       3)))
      (push (first current-line) regulars)
      (loop for curtd in current-line and nexttd in (cdr current-line) do
	(let ((incr (- (tld-time nexttd) (tld-time curtd))))
	  (if (or (= incr 1/6) (= incr 1/3)) (setf triplets t))
	  (if (and (not (tld-type nexttd))
		   (or (= cur-incr 0)
		       (= incr cur-incr)))
	      (progn
		(push nexttd regulars)
		(setf cur-incr incr))
	    (progn
	      (if (> (length regulars) regnum)
		  (let* ((reg-diff (/ (- (tld-cx (first regulars)) (tld-cx (first (last regulars)))) (1- (length regulars)))))
		    (let ((maxwall (or (loop for nt in regulars and nnt in (cdr regulars) maximize (+ (tld-cx0 nt) (tld-cx1 nnt))) 0)))
		      (if (or (regularize score) (< maxwall reg-diff))
			  #-sbcl (loop for nt in regulars and curcx from (tld-cx (first regulars)) by (- reg-diff) do
				   (setf (tld-cx nt) curcx))
			  #+sbcl (loop for nt in regulars and curcx downfrom (tld-cx (first regulars)) by reg-diff do
				   (setf (tld-cx nt) curcx))
			(let ((new-regs nil))
			  (push (first regulars) new-regs)
			  (loop for nt in regulars and nnt in (cdr regulars) do
			    (if (< (+ (tld-cx0 nt) (tld-cx1 nnt)) maxwall)
				(push nnt new-regs)
			      (progn
				(regularize-time-line score fx-factor ex-factor new-regs)
				(setf new-regs nil)
				(push nt new-regs))))
			  (regularize-time-line score fx-factor ex-factor new-regs))))))
	      (setf regulars nil)	;avoid dotted lists
	      (push nexttd regulars)
	      (setf cur-incr 0)))))
      (if (> (length regulars) 3)
	  (let* ((reg-diff (/ (- (tld-cx (first regulars)) (tld-cx (first (last regulars)))) (1- (length regulars)))))
	    #-sbcl (loop for nt in regulars and curcx from (tld-cx (first regulars)) by (- reg-diff) do
		     (setf (tld-cx nt) curcx))
	    #+sbcl (loop for nt in regulars and curcx downfrom (tld-cx (first regulars)) by reg-diff do
		     (setf (tld-cx nt) curcx))
	    ))
      ;; now if regularize < 3 and we noticed possible triplet/duplet cases in the preceding, try to prettify them
      (when (and triplets (< regnum 3))
	(let ((base 0))
	  (setf triplets nil)
	  ;; looking for the specific (common) pattern 1/3 1/6 1/6 1/3
	  (loop for curtd in current-line and nexttd in (cdr current-line) do
	    (let ((incr (- (tld-time nexttd) (tld-time curtd))))
	      (if (not triplets)
		  (if (or (= incr 1/3) (= incr 2/3) (= incr 1/6))
		      (progn
			(setf base incr)
			(push curtd triplets)))
		(if (= incr base)
		    (if (= (length triplets) 3)
			(let ((total-space (- (tld-cx nexttd) (tld-cx (first (last triplets))))))
			  ;;(print (format nil "total: ~1,3F at ~1,3F starting at ~1,3F to ~1,3F" 
			  ;;	           total-space (tld-time (first (last triplets))) 
			  ;;               (tld-cx (first (last triplets))) (tld-cx curtd)))
			  (setf (tld-cx curtd) (- (tld-cx nexttd) (* 1/3 total-space)))
			  (setf (tld-cx (first triplets)) (- (tld-cx nexttd) (* 1/2 total-space)))
			  (setf (tld-cx (second triplets)) (- (tld-cx nexttd) (* 2/3 total-space)))
			  (setf triplets nil))
		      (progn 
			(setf triplets nil) 
			(push curtd triplets)))
		  (if (= incr (/ base 2))
		      (if (< (length triplets) 4)
			  (push curtd triplets)
			(setf triplets nil))
		    (setf triplets nil))))))))
      )))


(defun layout-line (line-h ssize score active-staffs ys y1s left-x0-scaled right-x1-scaled brackets local-braces staff-onset)
  (let* ((dy 0)
	 (sys-y1 0)
	 (cur-y (/ line-h ssize))
	 (i 0)
	 (internal-line (find-if #'(lambda (ln) 
				     (and (= (odb-onset ln) staff-onset)
					  (list-p (dy ln))))
				 (line-data score)))
	 (internal-line-dy (and internal-line (dy internal-line))))

    (if (and (title score) (= staff-onset (initial-onset score))) (decf cur-y (title-space (title score))))

    (loop for k from 0 below (staves score) do 
      (setf (aref ys k) nil) 
      (setf (aref y1s k) nil) 
      (setf (aref brackets k) nil) 
      (setf (aref local-braces k) nil))

    (loop for system in (systems score) do
      (let ((len (length (staves system)))
	    (last-active-staff-number nil))
	(loop for k from 0 below len do
	  (if (aref active-staffs (+ k i))
	      (setf last-active-staff-number k)))

	(if last-active-staff-number
	    (progn
	      (setf sys-y1 nil)
	      (if (not (zerop dy)) (incf dy (system-separation score)))
	      (loop for staff in (staves system) and stf-ctr from 0 do
		(when (aref active-staffs i)
		  (if (not (zerop dy)) (incf dy (staff-separation score)))
		  (let* ((staff-dyp (and internal-line-dy (find (1+ i) internal-line-dy :test #'= :key #'first)))
			 ;; (1+ i) because we pre-increment staff-ctr for insert-bars-ties-and-rests
			 (staff-dy (or (and staff-dyp (second staff-dyp)) 0)))
		    (incf cur-y staff-dy))
		  (if (not (true-staff staff)) ;this is an independent staff
		      (progn
			(decf cur-y (- dy (vis-dy staff)))
			(setf (aref ys i) (- cur-y (* (or (staff-size staff) 1.0)
						      (if (staff-lines staff)
							  (* .25 (if (= (staff-lines staff) 1)
								     (+ (start-line staff) 1)
								   (1- (staff-lines staff))))
							  1.0))))
			(if (not sys-y1) (setf sys-y1 cur-y))
			(setf (aref y1s i) sys-y1)
			(setf (box-y0 staff) (aref ys i)))
		    (progn		;this is a tied-to staff
		      (setf (box-y0 staff) (box-y0 (true-staff staff)))
		      (setf (aref ys i) (staff-y0 staff))
		      (setf (aref y1s i) sys-y1)))
		  (if (zerop (box-x0 staff)) (setf (box-x0 staff) left-x0-scaled))
		  (if (zerop (box-x1 staff)) (setf (box-x1 staff) right-x1-scaled))
		  (setf dy 1.0)		;staff-h
	      
		  (if (and (= stf-ctr last-active-staff-number)
			   (bracketing system))
		      (setf (aref brackets i) (bracketing system))
		    (setf (aref brackets i) nil))
		  (if (and (staff-local-brace staff)
			   (bracket-p (staff-local-brace staff)))
		      (setf (aref brackets i) (staff-local-brace staff))
		    (setf (aref local-braces i) (staff-local-brace staff))))
		(incf i)))
	  (incf i len))))))

(defun line-per-page-fixup (score pages page)
  ;; find how many lines on current page, if possible distribute most of the left over space between them
  (if (and (> (length pages) 1)
	   (< page (length pages)))
      (let* ((our-page (nth page pages))
	     (lines-on-page (- our-page (nth (1- page) pages)))
	     (pg (nth (1- our-page) (time-lines score)))
	     (page-space (max 0 (- (or (if (page-p pg) (page-space pg) (line-space pg)) 0) (line-separation score)))))
	(if (> lines-on-page 1)
	    (/ page-space (1+ lines-on-page))
	  0))
    0))

(defun find-line-intersection (times beg end)
  ;; return a list of lists (or nil) containing (beg end) of any intersections of the 
  ;;   interval beg..end with the list of similar intervals in times.
  (let ((stimes times)
	(new-times nil))
    (if (= end beg) (incf end))
    (loop while (and stimes (< (second (first stimes)) beg)) do (setf stimes (cdr stimes)))
    (when stimes
      (loop while (and stimes (> end (first (first stimes)))) do
	(let ((bg (max beg (first (first stimes))))
	      (ed (min end (second (first stimes)))))
	  (if (or (/= bg ed) (= beg (1+ end)))
	      (push (list bg ed) new-times)))
	(setf stimes (cdr stimes))))
    (nreverse new-times)))

(defvar staff-backpatches nil)

(defun new-staff (score i staff anewdata x0 y0 x1 &optional (nx0 0) alines aname amarks abracket abrace cline)
  (if (not (eq (justification staff) :new-style))
      (progn
	(push 
	 (make-instance 'staff :x0 x0 :y0 y0 :x1 x1 :staff-name-x0 nx0 :back-staff staff
		     :lines (if alines (aref alines i) 5)
		     :start-line (staff-start-line staff)
		     :name (and aname (aref aname i))
		     :draw-func (draw-func staff)
		     :marks (if (and amarks (aref amarks i))
				(check-for-name score 
						(aref amarks i) 
						(and aname (aref aname i)) 
						(and abracket (aref abracket i)) 
						(and abrace (aref abrace i)))))
	 (aref anewdata i))
	nil)
    (let* ((line-onset (tld-time (first cline)))
	   (line-offset (tld-time (first (last cline))))
	   (intersection-times (find-line-intersection (times staff) line-onset line-offset))
	   (staff-line-coincide nil))
      (loop while intersection-times do
	(let ((curtime (pop intersection-times))
	      (newstf 
	       (make-instance 'staff :x0 x0 :y0 y0 :x1 x1 :staff-name-x0 nx0 :back-staff staff
			   :lines (if alines (aref alines i) 5)
			   :start-line (staff-start-line staff)
			   :name (and aname (aref aname i))
			   :draw-func (draw-func staff)
			   :marks (if (and amarks (aref amarks i))
				      (check-for-name score 
						      (aref amarks i) 
						      (and aname (aref aname i)) 
						      (and abracket (aref abracket i)) 
						      (and abrace (aref abrace i)))))))
	  (push newstf (aref anewdata i))
	  (setf (staff-user-data newstf) (staff-user-data staff))
	  (setf (odb-onset newstf) (first curtime))
	  (if (= (first curtime) line-onset) (setf staff-line-coincide t))
	  (if (/= (first curtime) line-onset)
	      (if (/= (second curtime) line-offset)
		  (push (list newstf (first curtime) (second curtime)) staff-backpatches)
		(push (list newstf (first curtime) nil) staff-backpatches))
	    (if (/= (second curtime) line-offset)
		(push (list newstf nil (second curtime)) staff-backpatches)))))
      staff-line-coincide)))

(defun check-staff-limits (cur-time x0 x1)
  (let ((dones nil))
    (loop for curback in staff-backpatches do
      (let* ((stf (first curback))
	     (ons (second curback))
	     (offs (third curback)))
	(when (and ons (= ons cur-time))
	  (setf (box-x0 stf) x0)
	  (setf (second curback) nil))
	(when (and offs (= offs cur-time))
	  (setf (box-x1 stf) x1)
	  (setf (third curback) nil))
	(when (and (not (second curback)) (not (third curback)))
	  (push curback dones))))
    (when dones
      (setf staff-backpatches (set-difference staff-backpatches dones)))))


(defun justify (score)
  ;; global justification.
  ;;   1. place overall score on page 
  ;;   2. decide staff vertical placement over entire page
  ;;      The outlines of this are established for each line in linify and each page in pagify, 
  ;;      saving the line and page sizes for the justify call.  We use these numbers to set up 
  ;;      left side brackets, braces, bars through systems, etc
  ;;   3. loop through all staff data (if any)
  ;;        add page-mark if we just ejected a page and are starting a new one
  ;;        get current line of time-line-data (tld) and reset x axis to start at (logical) 0 -- left-x0 points to "0"
  ;;          decide local staff positioning throughout the current line (line-mark dy, etc)
  ;;          get room for staff names (these can be abbreviated or omitted on lines after the first)
  ;;          set overall horizontal extent of musical data
  ;;	      check for clefs and keys that need to be carried along and repeated by cmn on each new line
  ;;          if staff-data already present (from a previous line) add line-break markers to the last thing on the previous line
  ;;          fixup the brackets and what-not that appear at the start of the staves in this line
  ;;            if a measure number is needed, add it to the marks list of the top staff in the current line
  ;;            if other marks lurking for the current line's staves, collect them, and prepare to backpatch their x values
  ;;            add staff name (if needed)
  ;;          add  the current ruling clefs and key signatures to the staff data
  ;;   4.     set horizontal justification for all entities on the current line
  ;;            first take into account the clefs and keys that might have been added
  ;;            total up all the walls, fences, and expanders for this line
  ;;            justify the line (via the time-line-data list)
  ;;   5.       pass justification decisions down to all the entities on this line
  ;;              check for voices colliding with each other (one note has to be moved over)
  ;;              check for various special cases involving bar lines -- last one on line is placed at staff end, for example,
  ;;                if it's actually the last thing on the line (there might be a key cancellation, and so on)
  ;;              check for measure repeat signs (have to be in the middle of the bar, which requires waiting for the bar completion)
  ;;              make line-start stuff tightly packed together
  ;;  6.   check for empty staves on this line, and make them look reasonable

  (let* ((staves (staves score))
	 (staffs (make-array staves :initial-element nil))	;staves to keep track of in parallel
	 (actual-staffs (make-array staves :initial-element nil))
	 (staff-sizes (make-array staves :initial-element nil))
	 (active-staffs (make-array staves :initial-element nil))
	 (true-staffs (make-array staves :initial-element nil))
	 (staff-marks (make-array staves :initial-element nil))
	 (new-staff-data (make-array staves :initial-element nil))
	 (staff-lines (make-array staves :element-type 'fixnum :initial-element 5))
	 (not-new-style (make-array staves :initial-element nil))
	 (ys (make-array staves :initial-element nil))	;base of staves
	 (y1s (make-array staves :initial-element nil))	;top of systems (for bars, brackets, etc)
	 (stfy0s (make-array staves :initial-element nil))
	 (keys (make-array staves :initial-element nil))
	 (clefs (make-array staves :initial-element nil))
	 (brackets (make-array staves :initial-element nil))
	 (local-braces (make-array staves :initial-element nil))
	 (staff-names (make-array staves :initial-element nil))
	 (clefs-and-keys-set nil)
	 (new-page t)
	 (backpatches nil)		;these are to patch in trill wavy line end points
	 (left-bar nil)			;is there an explicit left bar at the start of the line
	 (current-page 1)
	 (current-line-number 1)	;within-page line number starting at 1
	 (cur-time-line (time-line score))
	 (staff-names-present (found-staff-name score))
	 (staff-name-room 0.0)
	 (ssize (or *cmn-score-size* (scr-size score)))
	 (staff-onset 0.0)
	 (last-staff-number 0)
	 (measure-number-staff (first (last (staves (first (systems score))))))
	 (measure-number-staff-number (and (automatic-measure-numbers score) 
					   (justification measure-number-staff)
					   (not (eq (justification measure-number-staff) :locked))
					   (1- (length (staves (first (systems score)))))))
	 (current-measure-number-staff-number nil)
	 (pending-measure-staff-number measure-number-staff-number)
	 
	 ;; 1a. basic line length -- first the page-relative size (leaving aside staff names and so on)
	 (line-length (inches-to-ps (- (page-width score) (+ (right-margin score) (left-margin score)))))
	 
	 ;; 1b. left side of overall score is over (left-margin score) from the left page edge
	 (left-x0 (inches-to-ps (left-margin score)))
	 (left-x0-scaled (/ left-x0 ssize))
	 
	 ;; 1c. similarly for the right side
	 (max-right-x1 (inches-to-ps (- (page-width score) (right-margin score))))
	 
	 ;; 1d.  right-most extent of the staff (1st guess) depends on whether this is a short little (one line) example
	 (right-x1 (if (or (> (lines score) 1)
			   (eq (full-last-line score) t))
		       max-right-x1
		     (let* ((last-tld (first (last (time-line score))))
			    (all-fx (loop for tld in (time-line score) 
				      sum (+ (tld-fx0 tld) (tld-fx1 tld))))
			    (last-compact-size (* (free-expansion-factor score)
						  (+ all-fx (tld-cx last-tld) (tld-cx1 last-tld)))))
		       (if (> (/ (* ssize last-compact-size) line-length) .667)
			   max-right-x1
			 (+ left-x0 (* ssize last-compact-size))))))
	 (right-x1-scaled (/ right-x1 ssize))
	 
	 ;; 1e. now similar calculations for the vertical placement
	 (page-h (inches-to-ps (- (page-height score) (+ (header-margin score) (footer-margin score)))))
	 (lines (time-lines score))
	 (line-hgt (size (first lines)))
	 (pages (append (list 0) (loop for ln in lines and i from 1 if (eq (line-type ln) :page) collect i)))
	 (page-fixup 0)
	 (line-h (* line-hgt ssize))
	 (line-l (- right-x1 left-x0))
	 
	 ;; 1f. top-y is where we start drawing as we work down the page (check here for little one line case)
	 (top-y (+ (if (> (pages score) 1) ;start at top and work down
		       page-h
		     (if (> (lines score) 1) ;all on one page
			 (min (* ssize (loop for ln in (time-lines score) sum (size ln)))
			      page-h)
		       (if (> staves 1)
			   line-h
			 (+ line-h
			    (* ssize 
			       (max (- (let ((val (loop for obj in (staff-data (first (staves (first (systems score)))))
						    minimize (y0 obj))))
					 (or val 0)))
				    0.0))))))
		   (inches-to-ps (footer-margin score))))
	 (top-y-fixup (* (if (> (lines score) 1)
			     (* .5 (line-separation score))
			   (line-separation score))
			 ssize))
	 (cur-y 0)
	 (cur-y-scaled 0)
	 (pending-measure-number nil)
	 (worry-about-new-style nil))
    
    (setf staff-backpatches nil)	;not actually the right thing, but with-cmn already clobbers other globals
    
    (let ((i 0))
      (loop for system in (systems score) do
	(loop for staff in (staves system) do
	  (setf (aref staffs i) (staff-data staff))
	  (setf (aref actual-staffs i) staff)
	  (setf (aref staff-sizes i) (or (staff-size staff) 1.0))
	  (setf (aref staff-marks i) (marks staff))
	  (setf (aref staff-names i) (staff-name staff))
	  (if (staff-lines staff) (setf (aref staff-lines i) (staff-lines staff)))
	  (setf (aref not-new-style i) (not (eq (justification staff) :new-style)))
	  (if (not (true-staff staff))	;this is an independent staff
	      (setf (aref true-staffs i) i)
	    (progn
	      (setf (aref true-staffs i) (position (absolutely-true-staff staff) actual-staffs))
	      (setf (staff-lines staff) 0)))
	  (incf i)))

      (when (and (= i 1) (> (box-x1 (aref actual-staffs 0)) right-x1-scaled))
	;; we actually need to have a way to tell that the user set this number so that we don't step on it
	;; and these numbers if set need to be passed throughout the score (not just in this simple case)
	(setf right-x1-scaled (box-x1 (aref actual-staffs 0)))
	(setf right-x1 (* ssize right-x1-scaled))
	(setf line-l (- right-x1 left-x0)))
      
      (setf worry-about-new-style (some #'null not-new-style))
      (setf left-bar (or (find-if-not #'null brackets) ;is there an explicit left bar
			 (find-if #'(lambda (obj) 
				      (and (bar-p obj) 
					   (eq (visible-justification obj) :left))) 
				  (aref staffs (1- i))))))
    
    (setf cur-y (- top-y line-h top-y-fixup))	;just in case no staff data at all

    (setf (box-x0 score) left-x0-scaled)
    (setf (box-x1 score) right-x1-scaled)
    (setf (box-y0 score) (footer-margin score))
    (setf (box-y1 score) (/ (- top-y top-y-fixup) ssize))

    ;; 3. go through all data -- rest of function is this loop plus a check for no data at all (just staves)
    (if (every #'null staffs)
	(layout-line line-h ssize score
		     (progn
		       (loop for i from 0 below staves do (setf (aref active-staffs i) t))
		       active-staffs)
		     ys y1s left-x0-scaled right-x1-scaled
		     brackets local-braces 0)
      (progn
	(loop while (and cur-time-line (notevery #'null staffs)) do

	  ;; 3a. add page-mark, if needed
	  (if new-page			;t on initial call (cur-y calc above is for case of no staff data)
	      (progn
		(setf cur-y (- top-y top-y-fixup))
		(setf new-page nil)
		(setf current-line-number 1)
		(if (> (pages score) 1) (setf page-fixup (line-per-page-fixup score pages current-page)))
		;; otherwise, we may not have started at the top of the page, so there's no guarantee
		;;  that we have room from top-y to footer-margin to insert any space.
		(if (> current-page 1)	;add page-mark to all staves
		    (loop for i from 0 below staves do
		      (push (page-mark) (aref new-staff-data i))))
		(incf current-page)))
	  
	  (decf cur-y (+ line-h (* ssize page-fixup)))

	  ;; 3b. get current line of time-line-data (tld) and reset x axis to start at 0
	  (let ((current-line nil))
	    ;; subtract out accumulated x-offset (so this line starts at logical 0)
	    (let ((happy t)
		  (x-offset (tld-x (first cur-time-line))))
	      (loop while happy do
		(let ((cur-tld (pop cur-time-line)))
		  (setf happy (and cur-time-line (tld-p cur-tld)))
		  (if (tld-p cur-tld)
		      (progn
			(decf (tld-x cur-tld) x-offset)
			(decf (tld-cx cur-tld) x-offset)
			(push cur-tld current-line))
		    (setf new-page (and (line-p cur-tld) (eq (line-type cur-tld) :page)))))))
	    
	    ;; 3c. now we have one "line" of the score (i.e. one complete set of systems)
	    ;;     set up the local staff positioning (if any)
	    (when current-line
	      (let ((last-note (second current-line))) ;minor fix-up for last note on the line (make it closer to the end-of-the-line)
		(when (and last-note
			   (tld-p last-note)
			   (= (tld-ex1 last-note) 3)   ;here we're trying to make sure it's a normal note
			   (= (tld-ex0 last-note) 2))
		  (setf (tld-ex1 last-note) 1)))
	      
	      (let ((staff-offset (onset (first lines))))
		(setf current-line (nreverse current-line))
		(setf staff-onset (tld-time (first current-line)))
		(if (= staff-onset staff-offset) (incf staff-offset))
		(setf last-staff-number 0)
		(loop for i from 0 below staves do
		  (setf (aref active-staffs i) (active-staff (aref actual-staffs i) staff-onset staff-offset))
		  (if (aref active-staffs i) (setf last-staff-number i))
		  (if (and (aref active-staffs i)
			   (true-staff (aref actual-staffs i)))
		      (setf (aref active-staffs (aref true-staffs i)) t))))
	      ;; now we know which staves are included in the current line.  Next set the staff y locations
	      ;;  for the current line 
	      (layout-line line-h ssize score active-staffs ys y1s left-x0-scaled right-x1-scaled brackets local-braces staff-onset)
	      (setf lines (cdr lines))
	      (when lines 
		(setf line-hgt (size (first lines)))
		(setf line-h (* line-hgt ssize)))
	      
	      (if (and current-measure-number-staff-number
		       (= pending-measure-staff-number current-measure-number-staff-number)
		       (aref active-staffs measure-number-staff-number))
		  (setf pending-measure-staff-number measure-number-staff-number))
	      (setf current-measure-number-staff-number nil)
	      (when (and measure-number-staff-number
			 (not (aref active-staffs measure-number-staff-number)))
		;; we have automatic measure numbers inserted, but the relevant staff is inactive on this line
		;;   so, find the lowest staff of the top system, and (when bars are seen later), copy the
		;;   measure-number mark into the new controlling staff.
		(let* ((first-active-staff-number (position-if #'identity active-staffs))
		       (cur-y1 (aref y1s first-active-staff-number)))
		  (setf current-measure-number-staff-number first-active-staff-number)
		  (loop for i from (1+ first-active-staff-number) below staves while (and (aref y1s i) (= cur-y1 (aref y1s i))) do
		    (if (and (aref active-staffs i)
			     (not (true-staff (aref actual-staffs i))))
			(setf current-measure-number-staff-number i)))))
	      
	      ;; 3d. get room for staff names, if any
	      (if (not staff-names-present) 
		  (progn
		    (setf staff-name-room 0.0)
		    (loop for i from 0 below staves do (setf (aref staff-names i) nil))
		    ;; if no staff names, use entire horizontal extent for musical data
		    (setf line-l (- right-x1 left-x0)))
		(progn
		  (setf staff-name-room (+ .1 (maximum-staff-name-room score staff-names brackets local-braces)))
		  ;; if just one line in score then push right-x1 over to accommodate staff names, if it's not already fully to the right
		  (if (= (lines score) 1)
		      (progn
			(setf right-x1 (min max-right-x1 (+ right-x1 (* staff-name-room ssize))))
			(setf right-x1-scaled (/ right-x1 ssize))
			(map-over-staves #'(lambda (score stf) 
					     (declare (ignore score)) 
					     (setf (box-x1 stf) right-x1-scaled)) 
					 score)))
		  
		  ;; now set the left side of the staff lines (i.e. push staff start right to leave room for the staff names)
		  (map-over-staves #'(lambda (score stf) 
				       (declare (ignore score)) 
				       (setf (box-x0 stf) (+ left-x0-scaled staff-name-room))
				       (setf (staff-name-x0 stf) (- staff-name-room)))
				   score)
		  
		  ;; and reset what we think our overall staff line length is (i.e. how much horizontal room we have for musical data)
		  (setf line-l (- right-x1 left-x0 (* staff-name-room ssize)))
		  (if (not (plusp line-l)) (cmn-warn "durned staff names are so long there's not room for the music")) 
		  (setf staff-names-present (always-show-staff-names score))))
	      
	      ;; make a guess at where the current line should end on the right -- only case that
	      ;; is affected here is  a short final line.  This needs to be done before the staves
	      ;; are set up, but after we've checked staff names and set the maximal line-l value.
	      ;; right-x1 and right-x1-scaled have their first-stab values. 
	      
	      (when (and (> (lines score) 1)  ;there are several lines
			 (not (eq (full-last-line score) t))
			 (not cur-time-line)) ;and we're at the last one
		(let ((fx-line-l 0)		;total fx space requested
		      (cx-line-l 0))	;total cx space requested
		  (loop for cur-tld in current-line do
		    (when (tld-p cur-tld)
		      (incf fx-line-l (+ (tld-fx0 cur-tld) (tld-fx1 cur-tld)))
		      (setf cx-line-l (+ (tld-cx cur-tld) (tld-cx1 cur-tld)))))
		  (let* ((current-scaled-line-length (/ line-l ssize))
			 ;; we have current-scaled-length, but how much would look nice? 
			 (last-partial-line-length (* (+ fx-line-l cx-line-l) (free-expansion-factor score))))
		    (if (eq (full-last-line score) :left)
			(setf current-scaled-line-length last-partial-line-length)
		      (if (and (full-last-line score) (numberp (full-last-line score)))
			  (setf current-scaled-line-length (* (full-last-line score) last-partial-line-length))
			(if (< (* 4 last-partial-line-length) current-scaled-line-length)
			    (setf current-scaled-line-length (* .4 current-scaled-line-length))
			  (if (< (* 2 last-partial-line-length) current-scaled-line-length)
			      (setf current-scaled-line-length (* .66 current-scaled-line-length))))))
		    (setf line-l (* current-scaled-line-length ssize))
		    (setf right-x1 (+ left-x0 (* ssize staff-name-room) line-l))
		    (setf right-x1-scaled (/ right-x1 ssize)))))
	      
	      ;; now line-l is the unscaled length of the line (i.e. in Ps units)
	      (setf cur-y-scaled (/ cur-y ssize))
	      
	      ;; 3e. check for clefs and keys that need to be carried along and repeated by cmn on each new line
	      ;;     an added complication here is that the key may have been canceled at the end of the previous line
	      ;;     once these things are added, we have to re-set all the tld x and cx values to reflect the 
	      ;;       fact that we've added junk, so we have less room for the musical data than might otherwise appear.
	      ;; also fixup the brackets and what-not that appear at the start of the staves in this line
	      (let ((clefs-and-keys-added nil)
		    (top-staff-of-line t)
		    (staff-line-coincide nil))
		(loop for i from 0 below staves do
		  (when (and (aref staffs i) (aref active-staffs i))
		    (let* ((cur-x (+ left-x0-scaled staff-name-room))
			   (stfy0 (+ cur-y-scaled (aref ys i) (if (evenp current-page) .0001 0))))
		      ;; the .0001 is to keep staff-y0 from being equal by accident across a page break when there's
		      ;; only one system per page and we have ties/slurs/connected-text/beams broken across the page boundary.
		      
		      ;; 3f. if previous data here, add a :line-break marker for beamifiers (several things can run across a line break)
		      (if (aref new-staff-data i)
			  (let ((last-audible-object (find-if #'(lambda (n) (or (audible-p n) (rest-p n))) (aref new-staff-data i))))
			    (if last-audible-object (push :line-break (store-data last-audible-object)))))
		      
		      ;; 3g. brackets, left bars, and so on set (y0 and y1, x0 etc)
		      (setf (aref stfy0s i) stfy0)
		      (fixup-brackets-and-braces score brackets local-braces 
						 ys y1s new-staff-data i cur-x 
						 stfy0 cur-y-scaled left-bar actual-staffs pending-measure-number last-staff-number)
		      ;; if left-bar then (aref new-staff-data i) first element is a bar-line
		      
		      ;; 3h. add measure number, if needed
		      (when (and pending-measure-number
				 (= i pending-measure-staff-number))
			(let ((obj (first (aref new-staff-data i))))
			  (if (and obj (marks-p obj) (not (eq (visible-justification obj) :right)))
			      (add-to-marks obj (list pending-measure-number))
			    (let ((left-line (copy bar)))
			      (setf (visible-justification left-line) :left)
			      (setf (box-x0 left-line) cur-x)
			      (setf (box-y0 left-line) stfy0)
			      (if (not left-bar) (setf (matrix left-line) (list 0 0 0 0 0 0)))
			      (setf (box-y1 left-line) (+ cur-y-scaled (or (aref y1s 0) (find-if #'numberp y1s) 0.0)))
			      (add-to-marks left-line (list pending-measure-number))
			      (push left-line (aref new-staff-data i)))))
			(setf pending-measure-number nil))
		      
		      ;; 3i. collect marks that apply to the current staff and add them to the backpatch lists
		      (if (aref staff-marks i) ;looking for measure repeats and so forth
			  (let* ((first-time (tld-time (first current-line)))
				 (last-time (tld-time (first (last current-line))))
				 (staffmarks (loop for mark in (aref staff-marks i)
					       if (and (backpatch mark)
						       (<= first-time (odb-onset mark) last-time))
					       collect mark)))
			    (if staffmarks
				(loop for mark in staffmarks do
				  (push (list (backpatch-time mark nil) mark) backpatches)))))
		      
		      (setf staff-line-coincide (new-staff score i (aref actual-staffs i) new-staff-data
							   cur-x stfy0 right-x1-scaled (- staff-name-room)
							   staff-lines staff-names staff-marks brackets local-braces current-line))
		      
		      (when (and top-staff-of-line
				 (or line-hook (line-hook score)))
			(funcall (or line-hook (line-hook score)) score (first (aref new-staff-data 0))
				 current-line-number 
				 (+ cur-y-scaled (aref y1s 0))
				 left-x0-scaled right-x1-scaled cur-x (aref staff-sizes 0))
			(incf current-line-number))
		      (setf top-staff-of-line nil)
		      
		      ;; 3k. add in staff name, if needed
		      (if (and (use-abbreviated-staff-names score)
			       (aref staff-names i))
			  (setf (aref staff-names i) (abbreviation-of-instrument-name (aref staff-names i))))
		      
		      ;; 3l. add  the current ruling clefs and key signatures to the old staff data (reversed now, so push), if needed
		      (when (and clefs-and-keys-set
				 (or (not worry-about-new-style)
				     (aref not-new-style i)
 				     staff-line-coincide)
				 (always-show-clefs score)
				 ;; user choice whether to repeat clefs on every new line
				 )
			(let ((clef-present (clef-p (first (aref staffs i))))
			      (new-clef nil))
			  (when (aref clefs i)
			    (if clef-present ;already have a clef, so just make sure clefs array is up to date
				(setf (aref clefs i) (first (aref staffs i)))
			      (progn
				;; create a new clef, but put off pushing it until after possible key decision
				;; here's a test case for the tied-to staff repeated-clef problem:
				;; (cmn (setf s1 (staff treble c4 e c4 e c4 w line-break d4 w d4 w)) 
				;;      (staff (tied-to s1) (treble (scale 0 0)) d5 q d5 w d5 w d5 w))
				(setf new-clef (house (cmn-clef (aref clefs i) 
								:x0 cur-x :center 0 :staff-y0 stfy0 
								:justification nil :dx 0 :dy 0) 
						      score))
				
					;(if (not (true-staff (aref actual-staffs i)))
					;    (setf (matrix new-clef) (list 1 0 0 1 0 0))
					;  (setf (matrix new-clef) (copy-list (matrix (aref clefs i)))))
				;; this I think was wrong --if invisible clef (or not normal size)
				;; is in use on current staff, we need to copy that on down to the next line
				(setf (matrix new-clef)
				      (scale-matrix (matrix (aref clefs i)) (automatic-clef-size score) (automatic-clef-size score)))
#|
	(cmn treble (c4 q)
	(treble (scale 0.8 0.8))
	(line-break)
	(treble in-parentheses)
	(c4 q))
;;; which has (at least) 2 bugs: parens are too small for treble clef, and line-break precedes the 2nd treble clef
|#				
				
				(setf clefs-and-keys-added t)
				(setf (odb-beat new-clef) staff-onset)
				(setf (odb-onset new-clef) staff-onset))))
			  (when (aref keys i)
			    (if (not (canceled-key-p (aref keys i)))
				(progn
				  ;; create a new key signature and push it on the data for this staff
				  (let ((new-key (house (cmn-key (aref keys i) :x0 cur-x :center 0 :staff-y0 stfy0) score)))
				    (dcopy (aref clefs i) new-key)
				    (setf clefs-and-keys-added t)
				    (setf (odb-beat new-key) staff-onset)
				    (setf (odb-onset new-key) staff-onset)
				    (if (not (true-staff (aref actual-staffs i)))
					(setf (matrix new-key) (list 1 0 0 1 0 0))
				      (setf (matrix new-key) (copy-list (matrix (aref keys i)))))
				    (push new-key (aref staffs i))))
			      (setf (aref keys i) nil)))
			  ;; now push clef (this order keeps the data as though it were (staff treble a-major...) or whatever)
			  (when new-clef (push new-clef (aref staffs i))))))))
		
		;; 4. now the fun begins -- from here on we are deciding the horizontal placement of the musical data on the current line
		;;    since we may have added clefs and keys, our first task is to recaculate where the musical center point is for
		;;     the first thing on the staff (this is not easy because we may have a mixture of explicit and implicit clefs
		;;     and there may be a variety of other things lurking at the staff start, like meters).
		(when clefs-and-keys-added
		  (let ((cur-tld (first current-line))
			(new-tld nil)
			(diff 0))
		    ;; here we mimic compactify in a small way -- since this is the start of the line, much of the
		    ;;   special stuff in compactify is not needed.
		    (loop for i from 0 below staves do
		      (when (and (aref staffs i) (aref active-staffs i))
			(let ((collected-tld (compactify-first-onset (aref staffs i) (aref staff-sizes i))))
			  ;; get necessary overall x-incr and add into all tld-cx and tld-x fields (diff of previous max-cx0 and current)
			  (when collected-tld
			    (if (null new-tld) (setf new-tld (make-tld :x 0 :cx 0 :cx0 0 :cx1 0 :fx0 0 :fx1 0 :ex0 0 :ex1 0)))
			    (setf (tld-cx0 new-tld) (max (tld-cx0 new-tld) (tld-cx0 collected-tld)))
			    (setf (tld-cx  new-tld) (max (tld-cx  new-tld) (tld-cx  collected-tld)))
			    (setf (tld-fx0 new-tld) (max (tld-fx0 new-tld) (tld-fx0 collected-tld)))
			    (setf (tld-ex0 new-tld) (max (tld-ex0 new-tld) (tld-ex0 collected-tld)))
			    (setf (tld-cx1 new-tld) (max (tld-cx1 new-tld) (tld-cx1 collected-tld)))
			    (setf (tld-fx1 new-tld) (max (tld-fx1 new-tld) (tld-fx1 collected-tld)))
			    (setf (tld-ex1 new-tld) (max (tld-ex1 new-tld) (tld-ex1 collected-tld)))))))
		    (when new-tld 
		      (setf diff (- (tld-cx0 new-tld) (tld-cx0 cur-tld)))
		      (setf (tld-cx0 cur-tld) (max (tld-cx0 cur-tld) (tld-cx0 new-tld)))
		      (setf (tld-cx  cur-tld) (max (tld-cx  cur-tld) (tld-cx  new-tld)))
		      (setf (tld-fx0 cur-tld) (max (tld-fx0 cur-tld) (tld-fx0 new-tld)))
		      (setf (tld-ex0 cur-tld) (max (tld-ex0 cur-tld) (tld-ex0 new-tld)))
		      (setf (tld-cx1 cur-tld) (max (tld-cx1 cur-tld) (tld-cx1 new-tld)))
		      (setf (tld-fx1 cur-tld) (max (tld-fx1 cur-tld) (tld-fx1 new-tld)))
		      (setf (tld-ex1 cur-tld) (max (tld-ex1 cur-tld) (tld-ex1 new-tld)))
		      
		      (when (plusp diff)
			;; if there's a need for more room than we had planned on before we knew there would be a
			;;   line break here, add that difference into the current time-line-data list.
			(loop for td in (cdr current-line) do
			  (incf (tld-cx td) diff)
			  (incf (tld-x td) diff)))))))
	      
	      ;; 4a. The current line is ready to go -- we have all the various justification data in the time-line-data list
	      ;;     and can use that to decide where to put things.  The basic idea is to make sure the cx fields ("walls") are handled,
	      ;;     then if possible the fx fields ("fences"), and if there's anything left over, distribute all of it out to the
	      ;;     ex fields ("expanders").  There are lots of special cases, most trying to keep things from colliding.
	      (let ((max-lx0 0)
		    (fx-line-l 0)		;total fx space requested
		    (cx-line-l 0)		;total cx space requested
		    (expanders 0)		;total expansion (ex) space 
		    (bar-positions nil))
		
		(setf (tld-ex0 (first current-line)) 1)
		
		;; 4b. add up these totals
		(loop for cur-tld in current-line do
		  (when (tld-p cur-tld)
		    (incf fx-line-l (+ (tld-fx0 cur-tld) (tld-fx1 cur-tld)))
		    (setf cx-line-l (+ (tld-cx cur-tld) (tld-cx1 cur-tld)))
		    (incf expanders (+ (tld-ex0 cur-tld) (tld-ex1 cur-tld)))))
		
		;; 4c. decide how to parcel the available space out
		(let* ((line-ln (/ line-l ssize))
		       (wall-factor 1.0)
		       (fx-factor (if (zerop fx-line-l) 1.0 (min 1.0 (max 0.0 (divide (- line-ln cx-line-l) fx-line-l)))))
		       (ex-factor (if (or (< fx-factor 1.0) (zerop expanders))
				      0.0
				    (/ (- line-ln cx-line-l fx-line-l) expanders))))
		  
		  (when (> ex-factor .5)
		    (if (> ex-factor 1.0)
			(progn 
			  (decf expanders .9)
			  (setf (tld-ex0 (first current-line)) .1))
		      (progn
			(decf expanders .5)
			(setf (tld-ex0 (first current-line)) .5)))
		    (setf ex-factor (/ (- line-ln cx-line-l fx-line-l) expanders)))
		  
		  ;; 4d. now reset all the current time-line-data fields to reflect the current line's justification decisions
		  (if (> cx-line-l line-ln)
		      (let ((fact (/ line-ln cx-line-l)))
			(setf wall-factor fact)
			(loop for td in current-line do
			  (setf (tld-x td) (* fact (tld-x td)))
			  (setf (tld-cx td) (* fact (tld-cx td)))))
		    (let ((x-incr 0))
		      (loop for td in current-line do
			(incf (tld-x td) x-incr)
			(incf x-incr (+ (* fx-factor (tld-fx0 td))
					(* ex-factor (tld-ex0 td))))
			(incf (tld-cx td) x-incr)
			(incf x-incr (+ (* fx-factor (tld-fx1 td))
					(* ex-factor (tld-ex1 td)))))))
		  
		  ;; 4dd. try to even out running passages.
		  (regularize-time-line score fx-factor ex-factor current-line)
		  
		  ;; 4e. check for local justification guidance (dx on line-mark for the current line)
		  (when (line-data score)
		    (let* ((cur-line (find staff-onset (line-data score) :key #'onset))
			   (cur-dx (and cur-line (vis-dx cur-line))))
		      (if (list-p cur-dx) (line-data-x current-line cur-dx))))
		  
		  (setf max-lx0 (+ left-x0-scaled staff-name-room))
		  
		  ;; 5. Justification completed.  From here on down, we're trying to pass the justified time line data 
		  ;;       down to the actual entities on this line.  This also requires that various back patches be
		  ;;       honored (i.e. ties and slurs need to know where the tied-to note is and so on).
		  
		  (loop for i from 0 below staves do
		    (let* ((cur-x max-lx0)	           ;current x position as we move along the line
			   (last-right-x cur-x)        ;last object's right edge (to avoid collisions)
			   (stfy0 (aref stfy0s i))     ;current staff's y0 (everybody needs to get this, so save some slot references)
			   (stfls (inner-line-separation (aref actual-staffs i)))
			   (not-new-style-staff (aref not-new-style i)))
		      (loop for td in current-line do  ;pass justification decisions down to all staff data objects
			(let ((cur-time (tld-time td)) ;onset time of the current entity
			      (happy t)
			      (possible-collision nil));two notes at same onset, not called a chord for some reason (counterpoint for example)
			  
			  ;; 5a. backpatch horizontal position based on patch time: tld-cx is the current musical midpoint, relative to max-lx0
			  ;;     main complication occurs when the backpatch point is not the current onset time, or passes the end of the
			  ;;     current line -- in the latter case we go to right-x1-scaled, since that's the best we can do
			  (when backpatches
			    (loop for patch in backpatches do
			      (when (first patch)
				(let ((pc nil))
				  (if (>= cur-time (first patch))
				      (progn
					(setf pc (second patch))
					(if (= cur-time (first patch))
					    (if (backpatch-start pc)
						(setf (box-x1 pc) (- (+ max-lx0 (tld-cx td)) (tld-cx0 td)))
					      (setf (box-x1 pc) (+ max-lx0 (tld-cx td))))
					  (setf (box-x1 pc)
						(find-time-line-position-of (first patch) max-lx0 right-x1-scaled current-line))))
				    (if (and (crescendo-p (second patch))
					     cur-time-line 
					     (= (first patch) (tld-time (first cur-time-line))))
					(progn
					  (setf (box-x1 (second patch)) right-x1-scaled)
					  (setf pc (second patch)))))
				  (if (and pc (crescendo-p pc))
				      (let ((x0-time (+ (odb-onset pc) (onset-offset pc))))
					(if (and (not (tag-note pc)) 
						 (/= stfy0 (staff-y0 pc)))
					    (setf (tag-note pc) (make-instance 'note :staff-y0 stfy0 :line 0)))
					(setf (box-x0 pc)
					      (if (>= x0-time (tld-time (first current-line)))
						  (find-time-line-position-of x0-time max-lx0 right-x1-scaled current-line)
						(find-time-line-position-of x0-time max-lx0 right-x1-scaled (time-line score))))))
				  (if pc (setf (first patch) nil))))))
			  
			  ;; 5b. The Main Loop -- here the x positions are actually set.
			  
			  (loop while (and (aref staffs i) (> cur-time (odb-onset (first (aref staffs i))))) do
			    (setf (aref staffs i) (cdr (aref staffs i))))
			  ;; clear out to start of possibly previously inactive staff (new-style layout)
			  
			  (if (aref active-staffs i)
			      (loop while (and happy
					       (aref staffs i)
					       (= cur-time (odb-onset (first (aref staffs i))))
					       (or (and (not (eq (tld-type td) :line))
							(not (eq (tld-type td) :page)))
						   ;; try to catch note at end of line where bar is actually expected
						   (and (not (audible-p (first (aref staffs i))))
							(not (rest-p (first (aref staffs i))))))
					       (or not-new-style-staff
						   (let* ((cur-o (first (aref staffs i)))
							  (dur (or (odb-duration cur-o) 0)))
						     (active-staff (aref actual-staffs i)
								   (if (plusp dur) cur-time (- cur-time .001))
								   (if (plusp dur) 
								       (+ cur-time dur) 
								     (+ cur-time .001))))))
				    do
				;; all the .001 foolishness is trying to catch clefs and bars that are otherwise
				;;  rejected by the active-staff function because they are right on the staff time boundary.
				(let* ((cur-obj (first (aref staffs i)))
				       (cur-scl (aref staff-sizes i))
				       (obj-space (* cur-scl (rx-space cur-obj))) ;space taken by the body of the current object
				       
				       ;; to find the true left and right spaces, we need to remember that the current 
				       ;; time line data values may have been edited			       
				       (ur-lh (+ (* fx-factor (min (tld-fx0 td) (* cur-scl (fence-left-space cur-obj))))
						 (* ex-factor (min (tld-ex0 td) (* cur-scl (expand-left-space cur-obj))))))
				       (lh (+ (wall-left-space cur-obj) ur-lh))
				       (rh (+ (wall-right-space cur-obj)
					      (* fx-factor (min (tld-fx1 td) (* cur-scl (fence-right-space cur-obj))))
					      (* ex-factor (min (tld-ex1 td) (* cur-scl (expand-right-space cur-obj)))))))
				  (if (marks cur-obj) ;check the marks list also for backpatch possibilities
				      (loop for mark in (marks cur-obj) do
					(if (backpatch mark)
					    (progn
					      (setf (box-x1 mark) right-x1-scaled)
					      (push (list (backpatch-time mark cur-obj) mark) backpatches)))))

				  ;; 5c. place a note, chord, or rest -- these are centered at tld-cx (the musical center of the current beat)
				  (if (or (note-p cur-obj)
					  (chord-p cur-obj)
					  (rest-p cur-obj))
				      ;; center at tld-cx
				      (let ((old-x0 (box-x0 cur-obj)))
					;; grace-notes cause x0 to be negative, so we need to remove this offset
					;; from obj-space (else the space gets allocated twice)
					
					(setf (box-x0 cur-obj) (- (+ max-lx0 (tld-cx td))
								  (+ (center cur-obj)
								     (if (note-p cur-obj)
									 (collision-head-backup cur-obj (note-collision cur-obj))
								       0))))
					
					;; check also for collision with whatever preceded us
					(if (and (not possible-collision) (< (box-x0 cur-obj) last-right-x))
					    (let ((diff (- last-right-x (box-x0 cur-obj))))
					      ;;(print (format nil "reset x0 from ~,3F to ~,3F at ~A" (box-x0 cur-obj) last-right-x (tld-time td)))
					      (setf (box-x0 cur-obj) last-right-x)
					      (when (stem-tie cur-obj)
						;; must fix up everyone else sharing this stem!
						(let ((all-notes (stem-tie cur-obj)))
						  (if (not (listp all-notes)) (setf all-notes (stem-tie all-notes)))
						  (loop for n in all-notes do
						    (if (not (eq n cur-obj)) (incf (vis-dx n) diff)))))))
					(setf possible-collision t)
					(setf (box-x1 cur-obj) (+ (box-x0 cur-obj) (* wall-factor obj-space) old-x0))
					(setf last-right-x (+ rh (box-x1 cur-obj))))
				    ;; 5e. barlines are extra trouble
				    (if (bar-p cur-obj)
					(let* ((end-of-line (eq (tld-type td) :line))
					       (key-lurking (and (cdr (aref staffs i))
								 (key-p (cadr (aref staffs i)))
								 (cadr (aref staffs i))))
					       (cancellation-pending (and end-of-line
									  key-lurking
									  (canceled-key-p key-lurking)))
					       (ckroom (if cancellation-pending
							   (+ (- (box-x1 key-lurking) (box-x0 key-lurking)) .25)
							 0))
					       (rx1 (if (or (eq (visible-justification cur-obj) :right)
							    end-of-line)
							(let ((measure-number (find-if #'measure-mark-p (marks cur-obj))))
							  (setf (visible-justification cur-obj) :right)
							  (when measure-number
							    (setf (marks cur-obj) (remove measure-number (marks cur-obj)))
							    (setf pending-measure-staff-number i)
							    (setf pending-measure-number measure-number))
							  right-x1-scaled)
						      (if (or (eq (visible-justification cur-obj) :left)
							      (= (odb-onset cur-obj) staff-onset))
							  (prog1
							      max-lx0
							    (setf rh 0)
							    (incf cur-x .01))
							(+ max-lx0 ur-lh (tld-bar-x td) (tld-x td))))))
					  ;; left-dx was factored into cx0 of tld-x during compactify
					  ;; bar-x tracks where the bar is relative to the various other objects
					  ;; the problem here is that clefs can occur in midline, before or after the bar.
					  (setf happy (not end-of-line))
					  
					  (let ((bar-set (and bar-positions (find (onset cur-obj) bar-positions :key #'first))))
					    (if bar-set ;previous staff already decided where this bar goes 
						(setf rx1 (second bar-set))
					      (push (list (onset cur-obj) rx1) bar-positions)))
					  
					  (setf (box-x0 cur-obj) (- rx1 ckroom))
					  (setf (box-x1 cur-obj) (- rx1 ckroom))
					  (setf last-right-x (+ rh rx1 
								(if (or (dots-right cur-obj) (dots-left cur-obj)) 
								    (+ .125
								       (if (and (dots-right cur-obj) (dots-left cur-obj)) .275 0))
								  0)))
					  (setf (box-y0 cur-obj) (- stfy0 .009))
					  (setf (box-y1 cur-obj) (+ cur-y-scaled (if (broken cur-obj)
										     (aref y1s i)
										   (or (aref y1s 0) 
										       (find-if #'numberp y1s)))))
					  
					  (when worry-about-new-style ;there is a new-style staff lurking near by
					    ;; if the top y1 staff is new-style and currently inactive, search down
					    ;; (i.e. up in the array) for one that is either active and not new-style,
					    ;; or new-style and active, and use its y0+staff-size as the current bar y1.
					    
					    ;; start at top, find first = current y1, if new-style and currently inactive,
					    ;;   scan down until active staff found, use its y0+staff-size
					    
					    (let* ((target-y1 (- (box-y1 cur-obj) cur-y-scaled))
						   (top-i (position-if #'(lambda (n) (and n (< (abs (- n target-y1)) .01))) y1s)))
					      (loop while (and (/= i top-i)
							       (or (not (aref active-staffs top-i))
								   (and (not (aref not-new-style top-i))
									(not (active-staff 
									      (aref actual-staffs top-i) 
									      (- cur-time .001) 
									      (+ cur-time .001)))))) do
									      (incf top-i))
					      (setf (box-y1 cur-obj) (+ cur-y-scaled 
									(aref ys top-i) 
									(aref staff-sizes top-i)))))
					  
					  (when (and measure-number-staff-number 
						     current-measure-number-staff-number
						     (= i current-measure-number-staff-number))
					    (let ((mnb (find-if #'(lambda (n) 
								    (and (metrical-bar-p n)
									 (= cur-time (onset n))))
								(staff-data measure-number-staff))))
					      (when mnb
						(let ((meas-mark (find-if #'measure-mark-p (marks mnb))))
						  (if meas-mark
						      (if (eq (justification cur-obj) :right)
							  (progn
							    (setf pending-measure-number meas-mark)
							    (setf pending-measure-staff-number i))
							(add-to-marks cur-obj (list meas-mark))))))))
					  
					  (when staff-backpatches 
					    (check-staff-limits cur-time (box-x0 cur-obj) (box-x0 cur-obj)))
					  
					  (when cancellation-pending
					    (setf (staff-y0 cur-obj) stfy0)
					    (push cur-obj (aref new-staff-data i))
					    (setf (aref staffs i) (cdr (aref staffs i)))
					    
					    (setf cur-obj key-lurking)
					    (setf (box-x0 cur-obj) (+ (- rx1 ckroom) .2))
					    (setf (box-x1 cur-obj) (+ (box-x0 cur-obj) ckroom))
					    (setf (aref keys i) nil)))
				      
				      ;; 5g. deleted 
				      ;; 5h. Everything Else -- main problem here involves the start and end of the line
				      ;;     and making sure the clef is on the correct side of the bar, etc
				      (progn
					(if (clef-p cur-obj)
					    (progn
					      (setf (aref clefs i) cur-obj)
					      (setf clefs-and-keys-set t))
					  (if (key-p cur-obj)
					      (progn
						(if (not (canceled-key-p cur-obj))
						    (setf (aref keys i) cur-obj)
						  (setf (aref keys i) nil))
						(setf clefs-and-keys-set t))))
					
					;; 5i. special start-of-line case
					(if (= staff-onset (odb-onset cur-obj))
					    (progn
					      (incf cur-x lh)
					      (setf (box-x0 cur-obj) cur-x)
					      (if (and (/= cur-scl 1.0)
						       (or (key-p cur-obj) (meter-p cur-obj) (clef-p cur-obj)))
						  (setf obj-space (/ obj-space cur-scl)))
					      ;; try to line these up across different sized staves
					      (setf (box-x1 cur-obj) (+ cur-x obj-space))
					      (when staff-backpatches 
						(check-staff-limits cur-time left-x0-scaled (box-x1 cur-obj)))
					      (incf cur-x (+ obj-space rh))
					      (setf last-right-x cur-x))
					  
					  (progn
					    (setf (box-x0 cur-obj) (max last-right-x (+ lh max-lx0 (tld-x td))))
					    (setf (box-x1 cur-obj) (+ (box-x0 cur-obj) obj-space))
					    (when staff-backpatches 
					      (check-staff-limits cur-time (- (+ (box-x0 cur-obj) (vis-dx cur-obj)) .25) (+ (box-x1 cur-obj) .25)))
					    (if (not (eq (visible-justification cur-obj) :none))
						(setf last-right-x (+ rh (vis-dx cur-obj) (box-x1 cur-obj)))))))))
				  
				  ;; 5j. finally set the ubiquitous staff-y0 field
				  (if (staff-relative-mixin-p cur-obj)
				      (progn
					(setf (staff-y0 cur-obj) stfy0)
					(if (chord-p cur-obj)
					    (loop for note in (chord-data cur-obj) do
					      (setf (staff-y0 note) stfy0)))))
				  (if (audible-p cur-obj) (setf (local-line-separation cur-obj) stfls))
				  ;; 5k. and place the fixed-up object on the (growing) staff data list
				  (push cur-obj (aref new-staff-data i))
				  (setf (aref staffs i) (cdr (aref staffs i)))
				  )  ;let curobj
				)    ;loop while and happy
			    )        ;if active staff
			  )          ;let cur-time
			)            ;loop for td
		      )              ;let* cur-x
		    )                ;loop for i
		  )                  ;let* line-in
		)                    ;let max-lx0
	      )                      ;let clefs and keys
	    )                        ;let current-line
	  )                          ;loop while notevery null
	)                            ;progn
      )                              ;if
    (when staff-backpatches		;somehow these got missed in the first pass
      (let ((all-times (remove-duplicates (loop for sb in staff-backpatches collect (second sb) collect (third sb))))
	    (bars (find-if #'metrical-bar-p (staff-data (first (staves (first (systems score))))))))
	(loop for sys in (systems score) while staff-backpatches do
	      (loop for stf in (staves sys) while staff-backpatches do
		    (loop for obj in (staff-data stf) while staff-backpatches do
			  (if (and (or (and (not bars)
					    (not (invisible-p obj)))
				       (bar-p obj)) 
				   (member (odb-onset obj) all-times))
			      (check-staff-limits (odb-onset obj) (box-x0 obj) (box-x0 obj))))))
	(when staff-backpatches 
	  (loop for stfpat in staff-backpatches do
		(if (second stfpat) (setf (box-x0 (first stfpat)) left-x0-scaled))
		(if (third stfpat) (setf (box-x1 (first stfpat)) right-x1-scaled)))
	  (setf staff-backpatches nil))))
    
    (if (> current-page (1+ (pages score)))
	(print (format nil "we seem to have produced ~d page~p, but we expected to produce ~d" 
		       current-page current-page (pages score))))
    
    ;; 6. Check for (completely) empty staves, and make them look reasonable
    (let ((i 0))
      (loop for system in (systems score) do
	    (loop for staff in (staves system) do
		  (when (not (aref new-staff-data i))
		    (new-staff score i staff new-staff-data 
			       (box-x0 staff) (box-y0 staff)
			       (max (box-x1 staff) (+ (box-x0 staff) .5)) nil
			       staff-lines staff-names staff-marks)
		    (if (or (aref brackets i) 
			    (aref local-braces i))
			(fixup-brackets-and-braces score brackets local-braces ys y1s new-staff-data 
						   i left-x0-scaled 
						   (box-y0 staff)
						   (/ cur-y ssize)
						   left-bar
						   actual-staffs nil (1- staves))))
		  (setf (staff-data staff) (nreverse (aref new-staff-data i)))
		  (incf i))))
    score))

#|
(cmn 
 (size 12) (automatic-measure-numbers 1)
 (layout :old-style)
 (engorge 
  (loop for i from 0 to 3 
	collect (system 
		 bracket 
		 (engorge 
		  (loop for j from 0 to 3 
			collect (staff 
				 bar 
				 (if (= j 0) treble
				   (if (= j 1) alto
				     (if (= j 2) tenor bass)))
				 (meter 2 4) 
				 (engorge 
				  (loop for k from 0 to 256 
					collect 
					(if (< k 128)
					    (if (< k (* 16 (+ 1 (* 4 (if (= i 2) 1 (if (= i 3) 0 i))) j))) (c4 q) quarter-rest)
					  (if (< (- 256 k) (* 16 (+ 1 (* 4 (if (= i 2) 1 (if (= i 3) 0 i))) j))) (c4 q) quarter-rest)))) 
				 full-double-bar)))))))

(cmn (page-width 4) staff treble e-major c4 q c4 q c4 q bar (cancel e-major) line-mark c4 q c4 q c4 q bar)
(cmn (always-show-staff-names nil) (page-width 4)
     staff (staff-name "hi there") treble c4 q c4 q c4 q bass bar line-mark c4 q c4 q c4 q line-mark c4 q c4 q c4 q)

(cmn (staff (layout :new-style) (meter 2 4 invisible) (treble unjustified (dx -.75) (onset 8)) (c4 q (onset 8)))
     (staff bass (meter 2 4) (engorge (loop for i from 0 to 15 collect (e3 q)))))

(cmn (size 40) (staff (x1 10)) (staff (x1 8)))
|#



;;; drawify finally produces the .eps output from the finished score description

(defun paged-file-names (score original-name &optional page)
  (let* ((ext (pathname-type original-name))
	 (ext-len (length ext))
	 (orig-len (length original-name))
	 (root-name (if (plusp ext-len) (subseq original-name 0 (- orig-len (1+ ext-len))) original-name))
	 (len (length root-name))
	 (name-end-1 (and (> len 2) (read-from-string (subseq root-name (- len 2) len))))
	 (pg-1 (and name-end-1 (numberp name-end-1) (minusp name-end-1) (abs name-end-1)))
	 (name-end-2 (and (> len 3) (read-from-string (subseq root-name (- len 3) len))))
	 (pg-2 (and name-end-2 (numberp name-end-2) (minusp name-end-2) (abs name-end-2)))
	 (paged-name (or pg-1 pg-2))
	 (base-name (if paged-name (subseq root-name 0 (- len (if pg-2 3 2))) root-name)))
    (setf ext (or ext "eps"))
    (if page
	(format nil "~A-~A~D.~A" 
		base-name 
		(if (and pg-2 (< (+ (1- paged-name) page) 9)) "0" "") 
		(if paged-name (+ paged-name page) page)
		ext)
      (if paged-name
	  (format nil "(~A..~A~D.~A): " root-name (if (and pg-2 (< (+ paged-name (pages score)) 10)) "0" "") (+ -1 paged-name (pages score)) ext)
	(if (= (pages score) 2)
	    (format nil "(~A, ~A-1.~A): " original-name base-name ext)
	  (format nil "(~A, ~A-1..~D.~A): " original-name base-name (1- (pages score)) ext))))))

(defun paged-file-name (score original-name page)
  (paged-file-names score original-name page))

(defun display-page-number (score page)
  (show score (cmn-text :letters (format nil "~D" page) :font-name (normal-font) :font-scaler .5)
	:matrix (translate-matrix score nil 
				  (inches-to-font-units score (* .5 (+ (- (page-width score) (right-margin score)) (left-margin score)))) 
				  (inches-to-font-units score (* .5 (footer-margin score))))))

(defparameter *verbose-eject* t)

(defun eject-page (score original-name page) 
  (if (> (pages score) 1) 
      (when *verbose-eject*
	(princ (format nil "~A~A~D~A " 
		       (if (= page 1)
			   (paged-file-names score original-name)
			 "")
		       (if (= page 1) "page " "") 
		       page 
		       (if (= page (pages score)) "." "")))
	(force-output)))
  (if (automatic-page-numbers score)
      (display-page-number score page))
  (when (< page (pages score))
    (if (not (all-output-in-one-file score))
	(let ((otype (output-type score)))
	  (finalize score :continue t)
	  (setf (output-type score) otype)
	  (setf (draw-list score) nil)
	  (initialize score :file (paged-file-name score original-name page)))
      (progn
	;; (setf (draw-list score) nil)
	(page-number score (1+ page) t)
	(g-set-line-width score 0)))))


(defun one-page (score staffs staff-sizes current-objects page original-name)
  (let ((staves (staves score)))
    (if (or page-hook (page-hook score)) (funcall (or page-hook (page-hook score)) score page))
    (loop for i from 0 below staves do
      ;; check for staffs that aren't the overall score size and scale appropriately
      (setf *cmn-staff* (aref staffs i))
      (with-color score *cmn-staff*
	(let ((*staff-line-separation* (or (inner-line-separation *cmn-staff*) *staff-line-separation*)))
	  (if (aref staff-sizes i)
	      (progn
		(with-scaling score (aref staff-sizes i) 0 0
		  (loop while (and (aref current-objects i)
				   (not (page-p (first (aref current-objects i))))) do
		    (let ((cur-obj (first (aref current-objects i))))
		      (display cur-obj nil (or (visible-section cur-obj) score)))
		    (setf (aref current-objects i) (cdr (aref current-objects i))))))
	    (progn
	      (loop while (and (aref current-objects i)
			       (not (page-p (first (aref current-objects i))))) do
                (let ((cur-obj (first (aref current-objects i))))
		  (display cur-obj nil (or (visible-section cur-obj) score)))
		(setf (aref current-objects i) (cdr (aref current-objects i)))))))))
    (if (not (embedded score)) (matrix-back score))
    (eject-page score original-name page)))


(defun drawify (score)
  (when (not no-initialization)
    (let ((original-name (output-file score))
	  (current-objects (make-array (staves score)))
	  (staff-sizes (make-array (staves score)))
	  (staffs (make-array (staves score)))
	  (i 0)
	  (staves (staves score)))
      (map-over-staves #'(lambda (score staff)
			   (declare (ignore score))
			   (setf (aref staffs i) staff)
			   (setf (aref current-objects i) (staff-data staff))
			   (if (and (staff-size staff)
				    (/= (staff-size staff) 1.0))
			       (let* ((scaler (/ 1.0 (staff-size staff))))
				 (setf (aref staff-sizes i) (staff-size staff))
				 (loop for object in (staff-data staff) do
				   (when (score-object-p object) ;might be embedded page mark
				     (setf (box-x0 object) (* (box-x0 object) scaler))
				     (if (box-x1 object) (setf (box-x1 object) (* (box-x1 object) scaler)))
				     (if (or (bar-p object)
					     (brace-p object)
					     (bracket-p object))
					 (setf (box-y1 object) (* (box-y1 object) scaler)))
				     (setf (box-y0 object) (* (box-y0 object) scaler))
				     (if (staff-relative-mixin-p object)
					 (setf (staff-y0 object) (* (staff-y0 object) scaler)))
				     (if (marks object)
					 (loop for mark in (marks object) do
					   (if (crescendo-p mark)
					       (progn
						 (setf (box-x0 mark) (* (box-x0 mark) scaler))
						 (setf (box-x1 mark) (* (box-x1 mark) scaler))))))
				     (when (or (audible-p object) (rest-p object)) ; rest-p added 8-Apr-02
				       (if (chord-p object)
					   (loop for note in (chord-data object) do
					     (setf (box-x0 note) (* (box-x0 note) scaler))
					     (setf (box-x1 note) (* (box-x1 note) scaler))					     
					     (setf (staff-y0 note) (* (staff-y0 note) scaler))))
				       (if (beams object)
					   (let* ((beams (beams object))
						  (beam-data (beam-data beams)))
					     (setf (staff-y0 beams) (* (staff-y0 beams) scaler))
					     (loop for beams in beam-data do
					       (setf (bdat-stfy0 beams) (* (bdat-stfy0 beams) scaler))
					       (setf (bdat-x0 beams) (* (bdat-x0 beams) scaler)))))))))
			     (setf (aref staff-sizes i) nil))
			   (incf i))
		       score)
      ;; take outer score's transformation matrix into account (inner scores are handled as marks)

      (loop for page from 1 to (pages score) do
	(if (not (embedded score)) (matrix-front score (matrix score)))
	(when (= page 1)
	  (if (marks score) (display-marks score score))
	  (loop for system in (systems score) do
	    (if (marks system) (display-marks system score))))
	(setf *cmn-page* page)
	(one-page score staffs staff-sizes current-objects page original-name)
	(loop for i from 0 below staves do
	  (if (aref current-objects i)
	      (setf (aref current-objects i) (cdr (aref current-objects i))))))
      ))
  score)

;;; for recursive cmn calls

(defun with-cmn (&rest args)
  (make-self-acting 
   :action #'add-to-marks 
   :argument (let ((new-score nil))
		(unwind-protect
		    (progn
		      (setf no-initialization t)
		      (setf new-score (apply #'cmn args)))
		  (setf no-initialization nil))
		(list new-score))))

(defmethod display ((score score) container outer-score &optional justifying)
  (let ((bbox nil))
    (if justifying
	(when (not (eq (justification score) :none))
	  (if (matrix score)
	      (setf bbox (transform-box (matrix score) 
					;; dx/dy deliberately left out!
					(list (box-x0 container) (box-y0 container))
					(list (box-x0 score) (box-y0 score) (box-x1 score) (box-y1 score))))
	    (setf bbox (list (+ (box-x0 score) (box-x0 container))
			     (+ (box-y0 score) (box-y0 container))
			     (+ (box-x1 score) (box-x0 container))
			     (+ (box-y1 score) (box-y0 container))))))
    (let ((old-score *cmn-score*)
	  (page-hook nil))
      (setf *cmn-score* score)
      (setf (output score) (output outer-score))
      (setf (output-type score) (output-type outer-score))
      (setf (scr-size score) (scr-size outer-score))
      (if (matrix score)
	  (with-transformation score 
			       (matrix score)
			       (* (+ (vis-dx score) (box-x0 container)) (scr-size outer-score)) 
			       (* (+ (vis-dy score) (box-y0 container)) (scr-size outer-score))
             (drawify score))
	(drawify score))
      (let ((lx0 (* (+ (vis-dx score) (box-x0 container)) (scr-size outer-score)))
	    (ly0 (* (+ (vis-dy score) (box-y0 container)) (scr-size outer-score))))
	(setf bbox
	      (transform-box (or (matrix score) (list 1 0 0 1 0 0))
			     (list lx0 ly0)
			     (list 0 0 (- (box-x1 score) lx0) (- (box-y1 score) ly0)))))
      (setf *cmn-score* old-score)))
    (when (and bbox (not (bounded outer-score)))
      (setf (box-x1 outer-score) (max (box-x1 outer-score) (third bbox)))
      (setf (box-y1 outer-score) (max (box-y1 outer-score) (fourth bbox))))))

;;; (cmn staff treble (c4 q (note-head :diamond) (with-cmn (dx -1.0) (dy 1.5) staff treble c6 q no-stem)))
;;; (i.e. with-cmn without scale msg leaves score without matrix)

;;; part extraction -- takes a staff name (string) and returns the associated staves for further processing

(defun text-of (name)
  (if (text-p name)
      (letters name)
    name))

(defun extract-part (staff-name)
  ;; staff-name can be a name (string) or a function that gets the staff-name and returns t if the staff is to be extracted
  (setf editor-hook 
	#'(lambda (objects)
	    (let ((new-objects nil))
	      (loop for object in objects do
		(if (self-acting-p object)
		    (push object new-objects)
		    (if (and (staff-p object)
			     (staff-name object)
			     (or (and (stringp staff-name)
				      (string-equal (text-of (staff-name object)) staff-name))
				 (and (functionp staff-name)
				      (funcall staff-name (text-of (staff-name object))))))
			(push object new-objects)
			(if (system-p object)
			    (loop for staff in (staves object) do
			      (if (and (staff-p staff)
				       (staff-name staff)
				       (or (and (stringp staff-name)
						(string-equal (text-of (staff-name object)) staff-name))
					   (and (functionp staff-name)
						(funcall staff-name (text-of (staff-name object))))))
				  (push staff new-objects)))))))
	      (nreverse new-objects))))
  nil)
;; this function should also include the tempo markings and rehearsal numbers
;; and automatic cues? (or extracted cues?)

(eval-when (load) (setf *cmn-score* (make-instance 'score)))

