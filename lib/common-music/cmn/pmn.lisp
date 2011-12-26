;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; proportional music notation via spacing-hook.
;;; set the variable *pmn-line-thickness*  to 0.0 to turn off the trailing line that shows the note duration.
;;;
;;; this messes up slightly as line-breaks are added because cmn recalculates the spacing after
;;; inserting the clefs and so forth.  If this is a real problem, let me know.

(in-package :cmn)

(defvar *pmn-line-thickness* 0.1)
(defvar *pmn-stem-choice* :none)

(defun pmn (&rest args)
  (apply #'cmn
	 (automatic-ties nil)
	 (automatic-rests nil)
	 (spacing-hook
	  #'(lambda (score)
	      (let ((beat-scl 0.0))
		(let ((cur-fx 0.0))
		  (loop for td in (time-line score) do
		    (incf cur-fx (tld-fx0 td))
		    (setf (tld-acc-x td) (+ (tld-cx td) cur-fx))
		    (incf cur-fx (tld-fx1 td))))
		(let* ((td0 (first (time-line score)))
		       (t0 (tld-time td0))
		       (x0 (tld-acc-x td0))
		       (cur-min-t 100.0)
		       (cur-max-x 0.0))
		  (loop for td1 in (cdr (time-line score)) do
		    (when (> (tld-time td1) t0)
		      (let ((dt (- (tld-time td1) t0))
			    (dx (- (tld-acc-x td1) x0)))
			(when (<= dt cur-min-t)
			  (setf cur-min-t dt)
			  (when (> dx cur-max-x)
			    (setf cur-max-x dx)))
			(setf td0 td1)
			(setf t0 (tld-time td0))
			(setf x0 (tld-acc-x td0)))))
		  (setf beat-scl (* (/ cur-max-x cur-min-t))))
		(loop for td in (time-line score) do
		  (let ((dx0 (- (tld-cx td) (tld-x td))))
		    (setf (tld-cx td) (* beat-scl (tld-time td)))
		    (setf (tld-x td) (- (tld-cx td) dx0))))

		;; now get rid of stems and beams and add the optional line showing the duration
		(let ((line-thickness *pmn-line-thickness*))
		  (loop for sys in (systems score) do
		    (loop for stf in (staves sys) do
		      (loop for n in (staff-data stf) do
			(when (or (note-p n) (chord-p n))
			  (if *pmn-stem-choice* (setf (stem-direction n) *pmn-stem-choice*))
			  (setf (beamed n) #'(lambda (&rest rest) (declare (ignore rest)) 0))
			  (when (> line-thickness 0.0)
			    ;; the line is drawn from just after the note-head to the end of the note using beat-scl
			    (add-to-marks n 
			      (list (make-instance 'sundry 
				     :name :useless-line
				     :mark #'(lambda (mark note score &optional justifying)
					       (declare (ignore mark justifying))
					       (let* ((old-width (line-width score))
						      (x0 (+ (x0 note) (center note) .2))
						      (dx (min (- (* beat-scl (free-expansion-factor score) (duration note)) .5)
							       (- (x1 (first (staves (first (systems score))))) x0))))
						 ;; this tries not to run over the end of the line, but it ignores the
						 ;; possibility of a collision with the start of the next note on this staff.
						 (if (note-p note)
						     (progn
						       (moveto score 
							       x0
							       (+ (staff-y0 note)
								  (* *staff-line-separation* (+ (note-line note) .125))))
						       (setf (line-width score) line-thickness)
						       (rlineto score dx 0)
						       (draw score)
						       (setf (line-width score) old-width))
						   ;; chord case -- draw a bunch of lines
						   (progn
						     (setf (line-width score) line-thickness)
						     (loop for n in (chord-data note) do
						       (moveto score 
							       x0
							       (+ (staff-y0 n)
								  (* *staff-line-separation* (+ (note-line n) .125))))
						       (rlineto score dx 0))
						     (draw score)
						     (setf (line-width score) old-width)))))))))))))))))
	 args))

;;; (pmn staff treble c4 q e4 h bf4 q bar)
;;; (pmn staff treble (chord (notes c4 e4 g4) q) e4 h bf4 q bar)
;;; (pmn (size 16) (free-expansion-factor 1.5) (staff treble c4 q c4 e c4 q c4 s c4 s c4 q bar) (staff bass c4 e c4 e c4 q c4 q c4 e bar))
;;; (pmn staff treble (c4 h (onset 0)) (e4 h (onset .5)) (chord h (notes c4 g4) (onset 4.0)))
