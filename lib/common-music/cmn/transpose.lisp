;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; score/part transposition

(in-package :cmn)

#|
           transpose (data &key (from-key c-major) (to-key c-major) (octave 0))


Transposition is assumed to go to the closest match upwards -- use "octave" argument to transpose down.

  (cmn (transpose (staff treble g4 q gs4 q gf4 q gn4 q 
		       (g4 q double-sharp) (g4 q double-flat) (g4 q (natural-sharp)) (g4 q (natural-flat))) 
		:to-key df-major))

To transpose one staff in a score:
 
  (cmn (staff treble c4 q) (transpose (staff bass c3 q) :from-key bf-major))

To transpose an entire score, wrap up everything into one staff, one system, or one score object

  (cmn (transpose (score (staff treble c4 q) (system (staff treble c4 q) (staff bass c3 q))) :from-key bf-major))

To go from a "score in C" part to the instrumentalist's part (say we're playing a d-flat trumpet),
  set up the part-extractor to extract the part you want and wrap it in a transpose call:

  (cmn (transpose (staff treble df5 q ef5 q) :from-key df-major :octave -1))

The transposition is normally upwards, so the "octave -1" gives us an end result of transposition down a half-step
  (that is, if the instrument is in key foo, we transpose our score-in-C part from key foo to c-major.  If the
  name "c-major" causes heartburn, use the key "no-key":

  (cmn (transpose (staff treble df5 q ef5 q) :from-key df-major :to-key no-key :octave -1))


Calls on with-cmn are not transposed by the outer transposition call -- you'll have
  to wrap up the with-cmn system data in an explicit call on transpose:

  (cmn (size 100) 
    (transpose 
      (staff treble 
        (c4 q (note-head :diamond) 
          (with-cmn (scale .75 .75) (dx -1.0) (dy 1.5) 
            (transpose (staff treble c4 q no-stem) :to-key d-major)))) 
      :to-key d-major)))


Currently trill "other-note" data is not transposed.

|#


(defun map-signature (frm)
  (let* ((arr (make-array 7 :element-type 'fixnum :initial-element 0))
	 (froms (signature frm))
	 (shrp (and froms (eq (first froms) sharp)))
	 (accs (if froms (second froms) 0)))
    (when froms 
      (if shrp
	  (loop for i in '(3 0 4 1 5 2 6) and j from 0 below accs do
	    (setf (aref arr i) 1))
	(loop for i in '(6 2 5 1 4 0 3) and j from 0 below accs do
	  (setf (aref arr i) -1))))
    arr))

(defun unmap-signature (sig)
  (let* ((accs-present (find-if-not #'zerop sig))
	 (accs (and accs-present (count accs-present sig))))
    (if accs-present
	(list (if (= accs-present 1) sharp flat) accs))))

(defun key-mode (ky) 
  (if (member ky (list cf-major c-major cs-major df-major d-major ef-major e-major f-major
		       fs-major gf-major g-major af-major a-major bf-major b-major no-key))
      :major
    :minor))
;; the "key" no-key is assumed to be a major mode key because it is used only to transpose
;;   instrumental parts, and there's no need to declare them to be in a minor mode.

(defun key-base (ky)
  (nth (position ky (list cf-major af-minor c-major a-minor cs-major as-minor 
			  df-major bf-minor d-major b-minor ef-major c-minor 
			  e-major cs-minor f-major d-minor
			  fs-major ds-minor gf-major ef-minor g-major e-minor
			  af-major f-minor a-major fs-minor 
			  bf-major g-minor b-major gs-minor no-key))
       '(0 5 0 5 0 5 1 6 1 6 2 0 2 0 3 1 3 1 4 2 4 2 5 3 5 3 6 4 6 4 0)))

(defun key-pitch (ky)
  (nth (position ky (list cf-major af-minor c-major a-minor cs-major as-minor 
			  df-major bf-minor d-major b-minor ef-major c-minor 
			  e-major cs-minor f-major d-minor
			  fs-major ds-minor gf-major ef-minor g-major e-minor
			  af-major f-minor a-major fs-minor 
			  bf-major g-minor b-major gs-minor no-key))
       '(-1 8 0 9 1 10 1 10 2 11 3 0 4 1 5 2 6 3 6 3 7 4 8 5 9 6 10 7 11 8 0)))

(defun update-key (obj from-sig to-sig)
  (let ((obj-sig (map-signature obj)))
    (if (equalp obj-sig from-sig)
	(setf (signature obj) (unmap-signature to-sig))
      ;;  figure out relative new "from" modulation, and reset both from and to
      ;;  new from = obj in all cases?
      (let* ((obj-sig-level (+ 7 (let ((val (loop for i from 0 to 6 sum (aref obj-sig i)))) val)))
	     (from-sig-level (+ 7 (let ((val (loop for i from 0 to 6 sum (aref from-sig i)))) val)))
	     (level-change (- obj-sig-level from-sig-level))
	     (to-sig-level (+ 7 (let ((val (loop for i from 0 to 6 sum (aref to-sig i)))) val)))
	     (new-acc-level (+ to-sig-level level-change)))
	(if (> new-acc-level 7) (decf new-acc-level 14)
	  (if (< new-acc-level -7) (incf new-acc-level 14)))
	(let ((new-acc (if (plusp new-acc-level) sharp (if (minusp new-acc-level) flat)))
	      (new-accs (abs new-acc-level)))
	  (setf (signature obj) (and new-acc (list new-acc new-accs)))
	  (let ((new-sig (map-signature obj)))
	    (loop for i from 0 to 6 do
	      (setf (aref from-sig i) (aref obj-sig i))
	      (setf (aref to-sig i) (aref new-sig i)))))))))

(defun update-note-1 (obj cc pc fsig tsig)
  (let* ((cur-pc (pitch obj))
	 (cur-cc (cclass obj))
	 (cur-oct (octave obj))
	 (new-pc (+ cur-pc pc))
	 (new-cc (+ cur-cc cc))

	 (cur-acc (if (note-sign obj) 
		      (let ((acc (note-sign obj)))
			(if (or (eq acc sharp) (eq acc small-sharp)) 1
			  (if (or (eq acc flat) (eq acc small-flat)) -1
			    (if (or (eq acc natural) (eq acc small-natural)) 0
			      (if (eq acc double-flat) -2
				(if (eq acc double-sharp) 2
				  (let ((nat-sign (and (marks acc)
						       (find-if #'(lambda (n) 
								    (and (sundry-p n) 
									 (member (sundry-name n) 
										 (list :natural-sharp :natural-flat))))
								(marks acc)))))
				    (if nat-sign
					(if (eq (sundry-name nat-sign) :natural-sharp) 1 -1)
				      (progn (warn "unrecognized accidental...") 0)))))))))
		    (aref fsig cur-cc))) ;i.e. no sign = take key sig (or whatever is currently in force)

	 (old-fsig (aref fsig cur-cc))
	 (fsig-change (- cur-acc old-fsig)))
    (if (and (> new-pc 11) (or (/= new-cc 6) (/= new-pc 12))) ;catch b-sharp special case
	(let ((octs (floor new-pc 12)))
	  (incf cur-oct octs)
	  (decf new-pc (* 12 octs))
	  (decf new-cc (* 7 octs)))
      (if (and (minusp new-pc) (or (/= new-cc 0) (/= new-pc -1))) ; catch c-flat special case
	  (let ((octs (+ 1 (floor (abs new-pc) 12))))
	    (decf cur-oct octs)
	    (incf new-pc (* 12 octs))
	    (incf new-cc (* 7 octs)))))
    (setf (octave obj) cur-oct)
    (setf (pitch obj) new-pc)
    (setf (cclass obj) new-cc)
    (if (or (not (zerop fsig-change))
	    (and (sign obj)
		 (/= old-fsig (aref tsig new-cc))))
	(let ((cur-tsig (+ (aref tsig new-cc) fsig-change)))
	  (setf (note-sign obj)
	    ;; currently no attempt is made to keep small-<sign>s and special case accidentals intact
	    ;; the actual pitch notated should be correct, and the caller can always fix it in cmn-store.
	    (if (= cur-tsig 0) natural
	      (if (= cur-tsig 1) sharp
		(if (= cur-tsig -1) flat
		  (if (= cur-tsig -2) double-flat
		    (if (= cur-tsig 2) double-sharp
		      (progn		;trouble here if b double sharp and so on
			(warn (format nil "~A-~A encountered -- hope for best" 
				      (if (= (abs cur-tsig) 3) "triple"
					(if (= (abs cur-tsig) 4) "quadruple"
					  (format nil "~D-semitone" (abs cur-tsig))))
				      (if (plusp cur-tsig) "sharp" "flat")))
			(incf (cclass obj) (* (floor (abs cur-tsig) 2) (signum cur-tsig)))
			;; i.e. round toward 0, not toward negative infinity
			(if (not (evenp cur-tsig)) (if (plusp cur-tsig) sharp flat)))))))))))))
      
(defun update-note (obj cc pc fsig tsig)
  (update-note-1 obj cc pc fsig tsig)
  (if (marks obj)
      (loop for m in (marks obj) do
	(if (grace-note-p m)
	    (loop for gobj in (grace-data m) do
	      (if (note-p gobj)
		  (update-note-1 gobj cc pc fsig tsig)
		(loop for gcobj in (chord-data gobj) do
		  (update-note-1 gcobj cc pc fsig tsig))))
	  (if (and (sundry-p m) (eq (sundry-name m) :auxiliary-note))
	      (let ((n (funcall (sundry-mark m) m nil nil :note)))
		(update-note-1 n cc pc fsig tsig)))))))


(defun transpose (data &key (from-key c-major) (to-key c-major) (octave 0))
  (let* ((from-sig (map-signature from-key))
	 (to-sig (map-signature to-key))
	 (cclass-inc-1 (- (key-base to-key) (key-base from-key)))
	 (cclass-inc (+ (if (minusp cclass-inc-1) (+ cclass-inc-1 7) cclass-inc-1) (* octave 7)))
	 (pclass-inc-1 (- (key-pitch to-key) (key-pitch from-key)))
	 (pclass-inc (+ (if (minusp pclass-inc-1) (+ pclass-inc-1 12) pclass-inc-1) (* octave 12)))
	 (saved-from-sig (map-signature from-key))
	 (saved-to-sig (map-signature to-key)))

    (flet ((transpose-obj (obj)
	     (if (audible-p obj)
		 (if (note-p obj) 
		     (update-note obj cclass-inc pclass-inc from-sig to-sig)
		   (loop for nobj in (chord-data obj) do
		     (update-note nobj cclass-inc pclass-inc from-sig to-sig)))
	       (if (key-p obj)
		   (update-key obj from-sig to-sig))))
	   (reset-original-sigs ()
	     (loop for i from 0 below 6 do
	       (setf (aref from-sig i) (aref saved-from-sig i))
	       (setf (aref to-sig i) (aref saved-to-sig i)))))

      (if (not (eq (key-mode from-key) (key-mode to-key)))
	  (warn "changing modes is not fair -- results are unlikely to be convincing..."))
      (if (score-p data)
	  (loop for sys in (systems data) do
	    (loop for stf in (staves sys) do
	      (reset-original-sigs)
	      (loop for obj in (staff-data stf) do
		(transpose-obj obj))))
	(if (system-p data)
	    (loop for stf in (staves data) do
	      (reset-original-sigs)
	      (loop for obj in (staff-data stf) do
		(transpose-obj obj)))
	  (if (staff-p data)
	      (loop for obj in (staff-data data) do
		(transpose-obj obj))
	    (loop for obj in data do
	      (transpose-obj obj)))))

      data)))



;;; also trill notes{ornament-sign, other-note}

;;; part-extraction could include the transpose call in the extract-part form (taking the (nreverse new-objects) as "data" arg)
;;; (cmn (transpose (staff treble bf3 q c4 q ef4 q f4 q) :from-key bf-major :to-key c-major)) -- how to get rid of naturals?


