;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-

(in-package :cmn)

;;; this file provides the wedged beams used by folks like Bartok
;;; there are two versions, a simple beam addition, and a whole new cmn object type
;;;
;;; Finale calls these "feathered" beams.


;;; the simplest version just adds the slanted beams to a normal beam:

(defun display-wedge-beams (mark note beamxy-note score start-beams end-beams &optional justifying)
  (declare (ignore mark))
  (when (not justifying)
    ;; now find the beam location and draw the other beams
    (let* ((beamxy (outer-beam beamxy-note))
	   (bptr (beams note))
	   (beam-above (eq (beam-direction bptr) :up))
	   (xb (+ (first beamxy) *half-stem-width*))
	   (xe (third beamxy))
	   (yb (second beamxy))
	   (ye (fourth beamxy))
	   (faster (< start-beams end-beams))
	   (beam-sep1 *beam-spacing*)
	   (beam-wid1 *beam-width*)
	   (beam-sep (if beam-above (- beam-sep1) beam-sep1))
	   (beam-thickness (if beam-above beam-wid1 (- beam-wid1)))
	   (dyb (if faster 0 beam-sep))
	   (dye (if faster beam-sep 0))
	   (beams (abs (- start-beams end-beams))))
      (when (eq (beam-direction bptr) :between)
	(let* ((binfo (beam-data bptr))
	       (bnotes (beam-notes bptr))
	       (need-draw nil))
	  ;; stem-x0 = true location of stem of each note and stem-end = true end.
	  ;; if beam-above and note-above agree, then we have to extend the stem to the current wedge width
	  (setf (line-width score) *stem-width*)
	  (loop for n in bnotes and desc in binfo do
	    (if (and (audible-p n)
		     (not (beams n))
		     (bdat-above desc))
		(progn
		  (setf need-draw t)
		  (moveto score (stem-x0 n) (stem-end n))
		  (rlineto score 0 (* beams
				      beam-sep1
				      (/ (- xe (stem-x0 n)) (- xe xb)))))))
	  (if need-draw (draw score))))
      (when (not beam-above)
	(progn
	  (incf yb beam-wid1)
	  (incf ye beam-wid1)))
      (if (> (min start-beams end-beams) 1)
	  (let ((base-beam-sep (* (1- (min start-beams end-beams)) beam-sep)))
	    (incf ye base-beam-sep)
	    (incf yb base-beam-sep)))

      (let ((left-out-beams (- (min start-beams end-beams) (bdat-fb (first (beam-data bptr))))))
	(when (plusp left-out-beams)
	  (let ((fixup (if (not beam-above) beam-wid1 0.0)))
	    (draw-beams score xb (- yb fixup) xe (- ye fixup) left-out-beams (beam-direction bptr)))))
	    
      #-sbcl (loop for i from 1 to beams and y0 from (+ yb dyb) by dyb and y1 from (+ ye dye) by dye do
	;; now draw the beam with a sharp point at the end with fewer beams
	(moveto score xb y0)
	(lineto score xe y1)
	(lineto score xe (+ y1 (if faster beam-thickness 0)))
	(lineto score xb (+ y0 (if faster 0 beam-thickness)))
	(lineto score xb y0)
	(fill-in score))
      #+sbcl
      (do ((i 1 (1+ i))
	   (y0 (+ yb dyb) (+ y0 dyb))
	   (y1 (+ ye dye) (+ y1 dye)))
	  ((= i beams))
	(moveto score xb y0)
	(lineto score xe y1)
	(lineto score xe (+ y1 (if faster beam-thickness 0)))
	(lineto score xb (+ y0 (if faster 0 beam-thickness)))
	(lineto score xb y0)
	(fill-in score))
      )))

(defun ur-begin-wedge-beam (which-beam start-beams end-beams &rest args)
  (engorge
   (list
    (apply which-beam
	   no-beam-break
	   (justification (list
			   (* (+ .5 (max start-beams (1- end-beams))) *beam-width*)
			   (* (+ .5 (max end-beams (1- start-beams))) *beam-width*)))
	   args)
    (mark #'(lambda (mark note score &optional justifying)
	      (if (eq :return-limits justifying)
		  (list start-beams end-beams)
		(display-wedge-beams mark note note score start-beams end-beams justifying)))
	  :wedge-beam))))

(defun begin-wedge-beam (start-beams end-beams &rest args)
  (apply #'ur-begin-wedge-beam #'begin-beam start-beams end-beams args))

(defun wedge-beam- (start-beams end-beams &rest args)
  (apply #'ur-begin-wedge-beam #'beam- start-beams end-beams args))

(defun end-wedge-beam (&optional ur-beam)
  (if (and ur-beam
	   (or (not (self-acting-p ur-beam))
	       (not (beam-p (argument ur-beam)))))
      (cmn-warn "odd argument to end-wedge-beam"))
  (end-beam ur-beam))


(defun fixup-wedge-beam (beam)
  (let ((true-order (reverse (tag-note beam))))
    (if (not (beams (first true-order)))
      ;; we crossed a staff
	(let* ((beamed-note (find-if #'beam-p (tag-note beam) :key #'beams))
	       (old-mark (find :wedge-beam (marks (first true-order)) :key #'(lambda (n) (and (sundry-p n) (sundry-name n)))))
	       (limits (funcall (sundry-mark old-mark) old-mark nil nil :return-limits))
	       (start-beams (first limits))
	       (end-beams (second limits)))
	  ;; :return-limits is a kludge to pass back to us the arguments to the lambda form in the original
	  (add-to-marks beamed-note (list
				     (mark #'(lambda (mark note score &optional justifying)
					       (display-wedge-beams 
						      mark note 
						      (find-if #'outer-beam true-order) 
						      score start-beams end-beams justifying))
					   :wedge-beam)))
	  (setf (marks (first true-order)) (remove old-mark (marks (first true-order))))))))


(defun -wedge-beam (beam)
  (make-self-acting
   :action #'(lambda (note old-beam)
	       (let ((true-beam (argument old-beam)))
		 (push note (tag-note true-beam))
		 (let ((beams (reverse (tag-note true-beam))))
		   (if (every #'onset beams)
		       (progn
			 (setf beams (sort beams #'< :key #'onset))
			 (setf (beamed (first beams))
			   #'(lambda (score staff stf-y0)
			       (annotate-beam score true-beam stf-y0 beams 1.0)
			       (fixup-wedge-beam true-beam))))
		     (setf (beamed (first beams))
		       #'(lambda (score staff stf-y0) 
			   (if (every #'onset beams)
			       (setf beams (sort beams #'< :key #'onset)))
			   (annotate-beam score true-beam stf-y0 beams 1.0)
			   (fixup-wedge-beam true-beam))))
		   nil)))
   :argument beam))


(defun -wedge-beam- (&optional ur-beam)
  (-beam- ur-beam))

#|
(cmn staff treble (c4 e (begin-wedge-beam 4 1)) (c4 e) (c4 e) (c4 e (end-wedge-beam)))
(cmn staff treble (c5 e (begin-wedge-beam 4 1)) (c5 e) (c5 e) (c5 e (end-wedge-beam)))
(cmn staff treble (c4 e (begin-wedge-beam 1 4)) (c4 e) (c4 e) (c4 e (end-wedge-beam)))
(cmn staff treble (c4 e (begin-wedge-beam 1 4)) (c4 e) (d4 e) (e4 e) (f4 e) (d4 e) (e4 e) (g4 e (end-wedge-beam)))
(cmn (staff treble 
	    (c4 e (let ((wbl (wedge-beam- 4 1 (dy -.5)))) (setf wb (first (data wbl))) wbl)) (c4 e (-wedge-beam- wb)) quarter-rest)
     (staff treble quarter-rest (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam wb))))
(cmn (size 40) 
     (staff treble 
	    (c4 e (let ((wbl (wedge-beam- 3 1 (dy -.5)))) (setf wb (first (data wbl))) wbl)) (c4 e (-wedge-beam- wb)) quarter-rest)
     (staff (dy -.5) treble quarter-rest (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam- wb)) 
	    (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam wb)) ))
(cmn (size 40) 
     (staff treble eighth-rest
	    (c4 e (let ((wbl (wedge-beam- 3 1 (dy -.5)))) (setf wb (first (data wbl))) wbl)) (c4 e (-wedge-beam- wb)) quarter-rest)
     (staff (dy -.5) treble (c4 e (-wedge-beam- wb)) quarter-rest (c4 e (-wedge-beam- wb)) 
	    (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam- wb)) (c4 e (-wedge-beam wb)) ))

;;; the nominal note duration for every note under the wedge beam should be the duration that corresponds to the
;;; minimum number of beams -- i.e. if we go from 2 to 6 beams, every note should have a duration of s (sixteenth=2 beams).
;;; (I subsequently added a check for this).

(cmn staff treble (c4 s (begin-wedge-beam 2 4)) (c4 s) (c4 s) (c4 s (end-wedge-beam)))

(cmn (size 24) (AUTOMATIC-LINE-BREAKS nil) 
  (staff treble       
    (note fs5 s (begin-wedge-beam 2 4))(note fn5 s)(note e5 s)(note fs5 s) 
    (note fn5 s) (note e5 s) (note fs5 s) (note fn5 s)
    (note e5 s) (note fs5 s) (note fn5 s) (note e5 s)
    (note fs5 s) (note fn5 s) (note e5 s) (note fs5 s)
    (note fn5 s) (note e5 s) (note fs5 s)(note fn5 s)
    (note e5 s) (note fs5 s) (note fn5 s) (note e5 s (end-wedge-beam))
    (bar)(line-mark)))
|#



;;; but that version doesn't know what the duration of the overall group is (for metering purposes and
;;; so on), nor does it automatically provide the changed spacing of the notes within the group), so
;;; the following version treats the group under the beam as a sort of chord spread out horizontally.
;;; To keep other simultaneous voices lined up right, we insert invisible spacers into the current staff
;;; and fix up their apparent x0 and x1 later.


(defvar spacer-space 0)      ;a bit of a kludge (else need pointers into current staff data)

(defclass spacer (wrest)
  ((ws :initform 0 :initarg :ws :accessor ws)))

(defmethod house ((sp spacer) score)
  (declare (ignore score))
  (setf (box-x1 sp) (ws sp)))

(defmethod display ((sp spacer) container score &optional justifying)
  (declare (ignore container score justifying))
  nil)

(defun invisible-spacer (dur white-space)
  (make-instance 'spacer :quarters dur :ws white-space))


(defclass true-space (sundry) ())

(defmethod identify ((sp true-space)) "")

(defmethod backpatch ((tsp true-space)) t) ;sets x1

(defmethod backpatch-time ((tsp true-space) obj)
  (+ (odb-onset obj) (* 4 (quarters obj))))

(defmethod backpatch-start ((tsp true-space)) t) ; sets x1 to center - left-space


(defclass bartok-bounce (chord)
  ((data :initarg :data :initform nil :accessor bartok-bounce-data)
   (spacing :initarg :spacing :initform .7 :accessor spacing)
   (start-beams :initarg :start-beams :initform 1 :accessor start-beams)
   (end-beams :initarg :end-beams :initform 1 :accessor end-beams)
   ;; for user adjusting dy0 and dy1
   (user-data :initarg :beam-user-data :initform nil :accessor beam-user-data)
   (dxs :initarg :dxs :initform nil :accessor dxs)
   (ws :initarg :ws :initform 0 :accessor ws)))

(deferred-action start-beams)
(deferred-action end-beams)
(deferred-action spacing)

(defmethod bartok-bounce-p ((obj t)) nil)
(defmethod bartok-bounce-p ((obj bartok-bounce)) t)

(defmethod maximum-line ((bb bartok-bounce)) (or (loop for note in (bartok-bounce-data bb) maximize (maximum-line note)) 0))
(defmethod minimum-line ((bb bartok-bounce)) (or (loop for note in (bartok-bounce-data bb) minimize (minimum-line note)) 0))

;; add dy0 and dy1 messages to wedge

(defmethod dsud ((b bartok-bounce) num-i)
  (if (not (beam-user-data b)) 
      (setf (beam-user-data b) (list 0 0 0 0)))
  (if (> (second num-i) 3) 
      (cmn-error "bartok-bounce can't handle d~A~D" 
		 (if (evenp (second num-i)) "x" "y")
		 (floor (second num-i) 2)))
  (setf (nth (second num-i) (beam-user-data b)) (first num-i)))

(defun bartok-bounce (&rest objects)
  (let ((notes nil)
	(bb (make-instance 'bartok-bounce)))
    (setf *cmn-owning-object* bb)
    (setf (center bb) .15)
    (loop for object in objects do
      (setf *cmn-object* object)
      (when object
	(if (self-acting-p object)
	    (funcall (action object) bb (argument object))
	  (if (or (note-p object) (chord-p object) (rest-p object))
	      (push object notes)
	    (if (rhythm-p object)
		(rcopy object bb)
	      (if (score-object-p object)
		  (if (or (sundry-p object) (pause-p object) (dynamics-p object))
		      (add-to-marks bb (list (if (write-protected object) (copy object) object)))
		    (copy object bb))
		(if (score-object-list-p object)
		    (loop for note in (data object) do
		      (push (if (write-protected note) (copy note) note) notes))
		  (if (text-p object)
		      (add-to-marks bb (list object))
		    (cmn-warn "odd argument to bartok-bounce: ~A" object)))))))))
    (setf (bartok-bounce-data bb) (nreverse notes))
    (loop for note in (bartok-bounce-data bb) do
      (setf (beat note) 0)
      (setf (flags note) 1)
      (setf (quarters note) 1/2))
    (cmn-tick-pipe bb)
    (setf (quarters bb) (/ (quarters bb) 4))
    
    (let* ((notes (length (bartok-bounce-data bb)))
	   (spaces (1- notes))
	   (note-linear-dxs (loop for note in (bartok-bounce-data bb)
			     collect (+ .5 (if (note-p note)
					       (if (note-sign note) (+ (width (note-sign note)) .05) 0)
					     (if (chord-p note) 
						 (if (find-if #'sign (chord-data note)) .3 0)
					       0)))))
	   (linear-x-space (+ .1 (apply #'+ note-linear-dxs)))
	   (faster (> (end-beams bb) (start-beams bb)))
	   (note-dxs (append (list 0)
			     (let* ((space (* .5 linear-x-space))
				    (spacex (loop for n from 1 to spaces 
					     collect (* space (spacing bb))
					     do (decf space (* space (spacing bb))))))
			       (if (not faster) (setf spacex (nreverse spacex)))
			       (loop for x in (cdr note-linear-dxs) and dx in spacex 
				collect (+ x dx)))))
	   (x-space (apply #'+ note-dxs)))
      (setf (dxs bb) note-dxs)
      (let ((qb (quarters bb))
	    (qs (* .25 x-space)))
	(setf (ws bb) qs)
	(add-to-marks bb (list (make-instance 'true-space 
				   :name :true-space
				   :mark #'(lambda (mark note score &optional justifying)
					     (declare (ignore mark note score justifying))
					     nil))))
	(engorge (list
		  bb 
		  (invisible-spacer qb qs)
		  (invisible-spacer qb qs)
		  (invisible-spacer qb qs)))))))

(defmethod display ((bb bartok-bounce) container score &optional justifying)
  (declare (ignore container))
  ;; set position of each note, beam the bb, etc
  ;; if this is a housing pass, just figure out how much room we'll need at a minimum
  (if justifying
      (let ((x0 (box-x0 bb))
	    (dxtotal (apply #'+ (dxs bb)))
	    (dxcur 0.0)
	    (len (length (dxs bb))))
	(moveto score (+ (box-x0 bb) (* (if (< len 4) 4 (if (< len 8) 2.75 (if (< len 12) 1.5 1))) (ws bb))) (box-y0 bb))
	(loop for note in (bartok-bounce-data bb) and dx in (dxs bb) do
	  (if (not (rest-p note))
	      (if (or (note-p note)
		    (not (find-if #'sign (chord-data note))))
		  (setf (center note) .143)
		(let ((old-output (output score)))
		  ;; chords with accidentals can be all over the map, so we have to draw them to figure out where the stem is
		  (clear-score score)
		  (setf (center note) (display note nil score justifying))
		  (setf (output score) old-output))))
	  (setf (box-x0 note) (+ x0 dx))
	  (setf (odb-duration note) 0)
	  (setf (onset note) (+ (onset bb) (* 4 (duration bb) (/ dxcur dxtotal))))
	  (incf dxcur dx)
	  (incf x0 dx))
	0)
    (let* ((x0 (+ (box-x0 bb) (vis-dx bb)))
	   (stfy0 (staff-y0 bb))
	   (bx1 (box-x1 (find :true-space (marks bb) :key #'sundry-name)))
	   (extra-space (max 0 (- bx1 (+ (box-x0 bb) 1 (* 4 (ws bb))))))
	   ;(faster (> (end-beams bb) (start-beams bb)))
	   (note-dxs (dxs bb))
	   (pad (/ extra-space (1+ (length note-dxs))))
	   (left-y 0.0)
	   (right-y 0.0)
	   )

      (loop for note in (bartok-bounce-data bb) and dx in note-dxs do
	(setf (box-y0 note) (* (minimum-line note) *staff-line-separation*))
	(setf (box-x0 note) (+ x0 dx))
	(if (chord-p note) (loop for n in (chord-data note) do (setf (staff-y0 n) stfy0)))
	(setf (staff-y0 note) stfy0)
	(incf x0 (+ dx pad)))

      (annotate-beam score nil stfy0 (bartok-bounce-data bb) 1.0)
      (when (beam-user-data bb)
	(incf left-y (second (beam-user-data bb)))
	(incf right-y (fourth (beam-user-data bb))))
      (setf (visible-justification (beams (first (bartok-bounce-data bb)))) 
	    ;; add various dy[*]-messages to wedge:
	(list (+  left-y (vis-dy bb)
		  (* (+ .5 (max (start-beams bb) (1- (end-beams bb)))) *beam-width*))
	      (+  right-y
		  (vis-dy bb) (* (+ .5 (max (end-beams bb) (1- (start-beams bb)))) *beam-width*))))
      (add-to-marks (first (bartok-bounce-data bb))
		    (list (mark #'(lambda (mark note score &optional justifying)
				    (display-wedge-beams mark note note score 
					   (start-beams bb) 
					   (end-beams bb)
					   justifying))
				:wedge-beam)))
      (loop for note in (bartok-bounce-data bb) do
	(loop for mark in (marks note) do
	  (if (backpatch mark)
	      (if (tag-note mark)
		  (setf (box-x1 mark) (box-x0 (tag-note mark)))))))
      (loop for note in (bartok-bounce-data bb) do
        ;; a horrendous kludge
	(if (and (chord-p note)
		 (numberp (stem-x0 note))
		 (find-if #'sign (chord-data note)))
	    (if (eq (stem-direction note) :up)
		(decf (box-x0 note) (- (stem-x0 note) .286))
	      (decf (box-x0 note) (stem-x0 note))))
	(display note nil score)))))

;;; now set up the wedged beam stuff as before
;;; (cmn staff treble (bartok-bounce q (notes c4 d4 e4 f4) (start-beams 1) (end-beams 3)))
#|
(cmn (staff treble (c4 q) (bartok-bounce q (notes cs4 d4 ef4 f4 e4 d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e))
(cmn (staff treble (c4 q) (bartok-bounce q (notes (chord c4 g4) d4 eighth-rest f4 e4 d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e))
(cmn (staff treble (c4 q) 
       (bartok-bounce q (notes (chord cs4 g4) d4 eighth-rest f4 (chord e4 fs4) d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e))

;;; the arguments to bartok-bounce are the duration (and onset time if desired)
;;;    the list of notes under the wedged beams (in the "notes" statement) -- each of these is a full fledged note
;;;    start-beams and end-beams (as above)
;;;    spacing = how to space the notes -- between 0 and 1, default is .7 -- clumpier as you approach 1

 (cmn (staff treble (meter 2 4) 
        (c4 q) 
        (bartok-bounce q (notes (c4 staccato) (d4 marcato) 
                                (e4 upside-down-fermata) (f4 (bartok-pizzicato (dy .5))) 
                                (e4 (fingering 7 9)) (d4 ppp) c4) 
          (spacing .5) (start-beams 1) (end-beams 3)) 
        (c4 q)) 
      (staff bass (meter 2 4) c3 e c3 e c3 e c3 e c3 e c3 e))

(cmn (staff treble (c4 q) 
       (bartok-bounce q (notes (chord cs4 ds4 g4) (chord d4 a4) eighth-rest fs4 (chord e4 fs4) (chord d4 e4) c4) 
		      (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)) 
     (staff bass c3 e c3 e c3 e c3 e c3 e c3 e))

(cmn (staff treble (c4 q) (bartok-bounce q (notes cs4 (d4 begin-crescendo) ef4 f4 (e4 end-crescendo) d4 c4) (spacing .7) (start-beams 1) (end-beams 3)) (c4 q)))

(cmn (size 40)
     (staff treble (bartok-bounce q (notes (chord cs4 g4) ef4 c4 e4 (chord ef4 gn4) (chord f4 bf4))
				  (start-beams 1) (end-beams 3)))
     (staff bass (bartok-bounce q (notes (chord g3 cs4) ef4 c4 e4 (chord ef4 gn3) (chord f4 bf3))
				(start-beams 3) (end-beams 1))))
|#
