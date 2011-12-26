;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; continuation of cmn2.lisp

(in-package :cmn)

;;;
;;; ----------------    tags
;;;
;;; there are several similar things that need to know where the bounding notes are after
;;; justification has taken place -- ties, slurs, beams are handled separately.
;;; Others are crescendos, connected text, tremolos, gliss and portamento, etc.

(defclass tag-mixin (staff-relative-mixin visible)
  ((name :initarg :name :initform nil :accessor tag-name)
   (type :initarg :type :initform nil :accessor tag-type)
   (note :initarg :note :initform nil :accessor tag-note)))

(defun find-tag (tag objects group data)
  (find-if #'(lambda (object)
	       (and (funcall group object)
		    (find (tag-name tag) (funcall data object) :key #'tag-name)))
	   objects))

(defmethod tag-p ((obj t)) nil)
(defmethod tag-p ((obj tag-mixin)) t)

(defmethod descry ((tag tag-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A"
	  (if (not controller) "(tag" "")
	  (if (tag-name tag) (format nil " :name ~A" (tag-name tag)) "")
	  (if (tag-type tag) (format nil " :type :~(~A~)" (tag-type tag)) "")
	  (if (tag-note tag) 
	      (format nil " :note ~A" 
		      (if (write-protected (tag-note tag))
			  (note-name (tag-note tag))
			(brief-identify (tag-note tag))))
	    "")
	  (if (next-method-p) (call-next-method tag stream (or controller tag)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((tag tag-mixin) &optional object)
  (let ((new-tag (if (not object) (make-instance 'tag-mixin)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (tag-name new-tag) (tag-name tag))
    (setf (tag-type new-tag) (tag-type tag))
    (if (next-method-p) (call-next-method tag new-tag))
    new-tag))

(defmethod identify ((tag tag-mixin))
  (format nil "~(~A~)" (tag-type tag)))




;;; 
;;; ----------------    irregular beat subdisions
;;;
;;; these should be "beat-" and so on, not the long-winded "beat-subdivision-"
;;; also should take sundries and text in place of the subdivision number (for "4:3" and so on)


(defclass beat-subdivision-mark (tag-mixin sundry font-mixin)
  ((subdivision :initarg :subdivision :initform nil :accessor subdivision)
   (beats :initarg :beats :initform nil :accessor beats)
   (bracketed :initarg :bracketed :initform t :accessor bracketed) ; can be :up or :down
   (bracket-user-data :initarg :bracket-user-data :initform nil :accessor bracket-user-data)
   (type :initarg :type :initform nil :accessor bracket-type)
   (font-name :initform (normal-font))	;Ross says this should be Times-BoldItalic
   (font-scaler :initform 0.5)))

(defmethod dsud ((bsm beat-subdivision-mark) num-i)
  (if (not (bracket-user-data bsm)) 
      (setf (bracket-user-data bsm) (list 0 0 0 0)))
  (if (> (second num-i) 3) 
      (cmn-error "beat subdivision can't handle d~A~D" 
	     (if (evenp (second num-i)) "x" "y")
	     (floor (second num-i) 2)))
  (setf (nth (second num-i) (bracket-user-data bsm)) (first num-i)))

(defmethod descry ((beat beat-subdivision-mark) &optional stream controller)
  (format stream "(beat-subdivision~A~A~A~A~A~A)"
	  (if (subdivision beat) (format nil " :subdivision ~A" (subdivision beat)) "")
	  (if (beats beat) (format nil " :beats ~A" (beats beat)) "")
	  (if (bracketed beat) (format nil " :bracketed t") "")
	  (if (bracket-user-data beat) (format nil " :bracket-user-data '~A" (bracket-user-data beat)) "")
	  (if (bracket-type beat) (format nil " :type :~(~A~)" (bracket-type beat)) "")
	  (if (next-method-p) (call-next-method beat stream (or controller beat)) "")))

(defmethod copy ((beat beat-subdivision-mark) &optional object)
  (let ((new-beat (if (not object) (make-instance 'beat-subdivision-mark)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (subdivision new-beat) (subdivision beat))
    (setf (beats new-beat) (beats beat))
    (setf (bracketed new-beat) (bracketed beat))
    (setf (bracket-user-data new-beat) (bracket-user-data beat))
    (setf (bracket-type new-beat) (bracket-type beat))
    (if (next-method-p) (call-next-method beat new-beat))
    new-beat))

(deferred-action subdivision)
(deferred-action beats)
(deferred-action bracketed)
(deferred-action bracket-type)
;; see under slur for dx0 and friends

;;; the "ratified" field is used solely for the auto-beat subdivision stuff here --
;;; it has no meaning outside this set of methods, so changes of any kind here are safe.

(defun no-beat-subdivision () (make-self-acting 
			       :action #'(lambda (note &rest rest)
					   (declare (ignore rest))
					   (setf (ratified note) :none))
			       :argument nil))

(defvar no-beat-subdivision (no-beat-subdivision))

(defun beat-subdivision- (&rest args)
  (let ((new-beat-subdivision (make-instance 'beat-subdivision-mark)))
    (loop for act in args do
      (if (self-acting-p act)
	  (funcall (action act) new-beat-subdivision (argument act))))
    (make-self-acting 
     :action #'(lambda (note beat)
		 (let ((sub (subdivision new-beat-subdivision)))
		   (push note (tag-note beat))
		   (setf (ratified note) sub)
		   (if (numberp sub) 
		       (setf (beats new-beat-subdivision) sub)
		     (let* ((lets (if (text-p sub) (letters sub) (if (stringp sub) sub "")))
			    (cpos (search ":" lets)))
		       (if cpos 
			   (setf (beats new-beat-subdivision) (read-from-string (subseq lets 0 cpos)))
			 (setf (beats new-beat-subdivision) (read-from-string lets))))))
		 nil)
     :argument new-beat-subdivision)))

(defun -beat-subdivision- (beat)
  (make-self-acting
   :action #'(lambda (note old-beat)
	       (let ((true-beat (argument old-beat)))
		 (setf (ratified note) :any)
		 (push note (tag-note true-beat))
		 nil))
   :argument beat))

(defun -beat-subdivision (beat)
  (make-self-acting
   :action #'(lambda (note old-beat)
	       (let* ((true-beat (argument old-beat))
		      (nbeats (append (nreverse (tag-note true-beat)) (list note))))
		 (setf (tag-note true-beat) nbeats)
		 (setf (ratified note) :any)
		 (add-to-marks note
			       (list (make-instance 'sundry 
				      :name :beat-subdivision
				      :mark #'(lambda (mark lnote score &optional justifying)
						(declare (ignore mark))
						(if (not justifying)
						    (display-beat-subdivision-number true-beat lnote score))))))
		 nil))
   :argument beat))

#|
(cmn treble (c4 (rq 1/6) (setf hi (beat-subdivision- (subdivision 3)))) (c4 (rq 2/6) (-beat-subdivision- hi)) (c4 (rq 3/6) (-beat-subdivision hi)))
|#

(defun integer-multiple (a b)		;aliquot would be a more scholarly name
  (if (= a b) 1
    (multiple-value-bind (int frac) (if (> a b) (floor a b) (floor b a))
      (and (zerop frac) int))))

(defun funny-case (n)
  (and (numberp (dots n))
       (<= (dots n) 1)
       (and (member (quarters n) '(1 1/2 1/4))
	    (not (member (denominator (beat n)) '(1 2 4 8 16 32 64 128 256))))
       (setf (dots n) 1)
       n))

(defun check-for-irregular-beat-subdivisions (score staff)
  ;; if irregular beat subdivision found, mark and all subsequent in that group
  ;; if entire group beamed, all done, else fixup outer-beam to guide bracket decision
  ;; decide whether bracket needed as well as number 
  ;; in any case number will go at midpoint of outer-beam endpoints
  (declare (ignore score))
  (loop for i from 0 and note in (staff-data staff) do
    (if (and (or (audible-p note) (rest-p note))
	     (ratiop (quarters note))
	     (not (member (denominator (quarters note)) '(1 2 4 8 16 32 64 128 256)))
	     (not (ratified note)))
	;; here we have some irregular beat subdivision that hasn't been noticed already and
	;; the caller wants us to put in the subdivision number ourselves (see markify)
	;; There are two basic cases -- a number in the middle of the relevant
	;; portion of the beam (if beamed, of course), and a bracket that follows
	;; the beam slope, if beamed, and can be on either side of the group of note
	;; and a bracketed number following one side or the other of the group of notes/rests.
	(let ((subdivision (denominator (quarters note)))
	      (bracketed t))
	  ;; this can be 6 = 3 sixteenths or problem cases like 4 e's in time of 3 where caller wants the "4".
	  ;; the first can be "6" if there are 6, or "3" if beat is e not q (sigh...)
	  ;; the display is handled as a mark on the marks list, all subsequent notes are ratified
	  ;;get group
	  (let* ((group nil)
		 (group-beats 0)
		 (staff-data-length (length (staff-data staff)))
		 (group-direction (or (stem-direction note) :up)) ;first guess
		 (happy t))
	    (push note group)
	    (setf group-beats (numerator (quarters note)))
	    (loop for j from (1+ i) below staff-data-length while happy do
	      (let ((object (nth j (staff-data staff))))
		(if (or (audible-p object) (rest-p object))
		    (if (ratiop (quarters object))
			(let* ((den (denominator (quarters object)))
			       (mlt (integer-multiple subdivision den)))
			  (if (and mlt (or (member mlt '(1 2 4)) (and (member mlt '(3 6)) (funny-case object))))
			      (progn
				(if (/= mlt 1)
				    (if (> den subdivision)
					(progn
					  (setf group-beats (* group-beats mlt))
					  (setf subdivision den)
					  (incf group-beats (numerator (quarters object))))
				      (incf group-beats (* mlt (numerator (quarters object)))))
				  (incf group-beats (numerator (quarters object))))
				(push object group)
				(if (and (not (rest-p object))
					 (not (stem-eq group-direction (stem-direction object))))
				    (setf group-direction :up))
				(if (zerop (mod group-beats subdivision)) (setf happy nil)))
			    (setf happy nil)))
		      (setf happy nil)))))
	    (when (> (length group) 1)
	      (if (and (beams note)	;applies to both notes and rests
		       (or (rest-p note) (eq group-direction (stem-direction note)))
		       (= (length group) (length (beam-notes (beams note)))))
		  (setf bracketed nil))
	      ;;mark group
	      (loop for gn in group do (setf (ratified gn) subdivision))
	      (let* ((last-note (first group))
		     (local-gn (reverse group))
		     (new-beat (make-instance 'beat-subdivision-mark
				:subdivision (let ((len (length group))) 
					       (if (or (and (> group-beats 1)
							    (oddp group-beats))
						       (= len group-beats))
						   group-beats
						 (if (< group-beats (* 2 (1- len))) 
						     group-beats 
						   (if (and (zerop (mod group-beats 2))
							    (> group-beats 2)
							    (< group-beats (* 4 (1- len)))) 
						       (floor group-beats 2) 
						     (if (and (zerop (mod group-beats 4))
							      (> group-beats 4)
							      (< group-beats (* 8 (1- len))))
							 (floor group-beats 4)
						       subdivision)))))
				;; I'm not sure about this -- perhaps we should use group-beats here, not subdivision?
				:bracketed (if bracketed (if (member group-direction '(:up :up?)) :up :down))
				:note local-gn)))
		(add-to-marks last-note 
			      (list (make-instance 'sundry 
				     :name :beat-subdivision
				     :mark #'(lambda (mark lnote score &optional justifying)
					       (declare (ignore mark justifying))
					       (display-beat-subdivision-number new-beat lnote score))))))))))))


;;; we can either go ahead and display the subdivision number (in the simple case)
;;; or put off the display until after the stem ends are calculated.  In the latter
;;; case, we make another sundry with the name :beat and let display-note-slurs-and-brackets 
;;; handle it later.  Attachment to a rest means do it now because stem ends can be believed.
;;; So, only delayed case is bracketed, not a rest as last note, not stem-down last note and group up.

;;; according to Read, the number is supposed to be centered on the note that is "central in time"
;;; but two note groups put it in between the two.  And in his examples, as opposed to the text,
;;; he centers even numbers between the "center" notes!  And he's forgetting other special cases 
;;; (see the test cases below).

(defun display-beat-subdivision-number (mark note score &optional justifying)
  ;; called only above in check-for-irregular-beat-subdivisions
  (declare (ignore justifying))
  (if (and (not (rest-p note))
	   (bracketed mark)
	   (or (and (eq (bracketed mark) :up)
		    (stem-is-up? note))
	       (and (eq (bracketed mark) :down)
		    (stem-is-down? note))))
      (add-to-marks note (list (make-instance 'sundry
				:name :beat
				:mark #'(lambda (not-mark note score &optional justifying)
					  (declare (ignore not-mark justifying))
					  (display-subdivision-bracket mark note score)))))
    ;; here we have all the information we need to make a decent bracket
    ;; outer-beam is believable if the entire group is beamed
    (if (not (bracketed mark))		;rewritten 16-Nov-94 to get the number centered properly
	(let* ((subdivision (or (subdivision mark) (denominator (quarters note))))
	       (group (tag-note mark))
	       (gr0 (first group))
	       (beamxy (outer-beam gr0))
	       (mid-onset (+ (odb-onset gr0) (* .5 (loop for gr in group sum (odb-duration gr)))))
	       (special-case (= (length group) 2))
	       (grl (if (not special-case)
			(loop for gr in group do 
			  (if (or (= mid-onset (odb-onset gr)) 
				  (> (+ (odb-onset gr) (odb-duration gr)) mid-onset)) 
			      (return gr)))))
	       (x (if (and (not special-case) (integerp subdivision))
		      (if (and (evenp subdivision) ;try to center more between the notes than on the second
			       (= mid-onset (odb-onset grl)))
			       ;; (cmn staff treble c4 ts c4 ts c4 te c4 ts c4 ts c4 ts c4 ts c4 ts c4 ts c4 te) 
			  (- (box-x0 grl) (divide (- (box-x0 grl) (box-x1 gr0)) (* 2 subdivision)))
			(+ (box-x0 grl) (center grl)))
		    (* .5 (+ (box-x1 gr0) (box-x0 (second group))))))
	       (y (if beamxy (+ (second beamxy) 
				(* (- (fourth beamxy) (second beamxy)) 
				   (divide (- x (first beamxy)) (- (third beamxy) (first beamxy)))))
		    (if (not special-case)
			(if (eq (bracketed mark) :down)
			    (box-y0 grl)
			  (box-y1 grl))
		      (if (eq (bracketed mark) :down)
			  (* .5 (+ (box-y0 gr0) (box-y0 (second group))))
			(* .5 (+ (box-y1 gr0) (box-y1 (second group))))))))
	       (matr (translate-matrix score mark 
				       (- (+ (vis-dx mark) x) .1)
				       (+ (dy mark) y
					  (if (stem-is-up? note)
					      .25
					    -.5)))))
	  (if (not (text-p subdivision))
	      (show score (cmn-text :letters (if (numberp subdivision) 
					       (format nil "~D" subdivision)
					     subdivision)
				  :font-name (font-name mark)
				  :font-scaler (font-scaler mark))
		    :matrix matr)
	    (show score subdivision :matrix matr)))
      (display-subdivision-bracket mark note score))))

(defun display-bracketed-number (mark note score rx0 ry0 rx1 ry1 above &optional (dist .5))
  (declare (ignore note))
  ;; called only below in display-subdivision-bracket
  (let* ((user-data (bracket-user-data mark))
	 (x0 (+ (vis-dx mark) rx0 (if user-data (first user-data) 0)))
	 (x1 (+ (vis-dx mark) rx1 (if user-data (third user-data) 0)))
	 (y0 (+ (vis-dy mark) ry0 (if user-data (second user-data) 0)))
	 (y1 (+ (vis-dy mark) ry1 (if user-data (fourth user-data) 0)))
	 (fixed-dist (min .75 (max .25 dist)))
	 (mid-x (- (+ x0 (* fixed-dist (- x1 x0))) .1))
	 (mid-y (+ y0 (* fixed-dist (- y1 y0))))
	 (subdivision (subdivision mark))
	 (matr (translate-matrix score mark mid-x (+ mid-y (if above .25 -.6))))) ; (- font-scaler+a little and -.25)
    (if (not (text-p subdivision))
	(show score (cmn-text :letters (if (numberp subdivision) 
					 (format nil "~D" subdivision)
				       subdivision)
			    :font-name (font-name mark)
			    :font-scaler (font-scaler mark))
	      :matrix matr)
      (show score subdivision :matrix matr))
    (let* ((beam-sep (if above .25 -.25))
	   (len (if above .15 -.15))
	   (len0 (if (and above (eq (bracket-type mark) :up-down)) .3
		   (if (and (not above) (eq (bracket-type mark) :down-up)) -.3 0)))
	   (len1 (if (and above (eq (bracket-type mark) :down-up)) .3
		   (if (and (not above) (eq (bracket-type mark) :up-down)) -.3 0)))
	   (num-sep .125)
	   (num-wid (* (font-scaler mark)
		       (if (numberp subdivision)
			   (if (< subdivision 10) .4 .8)
			 (if (text-p subdivision)
			     (* (font-scaler subdivision) (length (letters subdivision)))
			   (* .4 (length subdivision))))))
	   (num-x0 (- mid-x num-sep))
	   (num-x1 (+ mid-x num-sep num-wid))
	   (yscl (divide (- y1 y0) (- x1 x0)))
	   (num-y0 (+ y0 (* yscl (- num-x0 x0))))
	   (num-y1 (+ y0 (* yscl (- num-x1 x0)))))
      (moveto score x0 (+ y0 beam-sep len0))
      (lineto score x0 (+ y0 beam-sep len))
      (lineto score num-x0 (+ num-y0 beam-sep len))
      (moveto score num-x1 (+ num-y1 beam-sep len))
      (lineto score x1 (+ y1 beam-sep len))
      (lineto score x1 (+ y1 beam-sep len1))
      (draw score))))

(defun display-subdivision-bracket (mark note score)
  ;; here we definitely have a bracket, even if alongside the beam
  ;; 16-Nov-94 changed to try to center the number correctly (as per Read anyway)
  (let* ((subdivision (or (subdivision mark) (denominator (quarters note))))
	 (bracketed (bracketed mark))
	 (group (tag-note mark))
	 (gr0 (first group))
	 (gr1 note)
	 (beamxy (outer-beam gr0))
	 (beam-info (and beamxy (beams gr0)))
	 (beamed-notes (and beam-info (length (beam-notes beam-info))))
	 (x0 (- (if beamxy (min (first beamxy) (box-x0 gr0)) (box-x0 gr0)) .05))
	 ;; (cmn staff treble (cs4 (rq 2/5)) (cs4 (rq 2/5)) (cs4 (rq 2/5)) (cs4 (rq 2/5)) (cs4 (rq 2/5)))
	 ;; i.e. need to include accidental in bracket
	 (x1 (+ (if (and beamxy
			 (numberp subdivision)
			 (= beamed-notes subdivision)
			 (= (onset (car (last (beam-notes beam-info)))) (onset note)))
		    ;; added onset check 15-Apr-02 for cases like:
		    ;; (cmn 
		    ;;   (c4 (rq 1/9) (setf hi1 (beat-subdivision- (subdivision 3))) (setf hi2 (beat-subdivision- (subdivision 3) (dy .5))))
		    ;;   (c4 (rq 1/9) (-beat-subdivision- hi1) (-beat-subdivision- hi2))
		    ;;   (c4 (rq 1/9) (-beat-subdivision- hi2) (-beat-subdivision hi1))
		    ;;   (c4 (rq 2/3) (-beat-subdivision hi2)))
		    (third beamxy)
		  (box-x1 gr1))
		.05))
	 (above (or (eq bracketed :up)
		    (and (not (eq bracketed :down))
			 (notevery #'(lambda (n)
				       (or (rest-p n) (stem-is-down? n)))
				   group))))
	 (ly0 (or (if beamxy 
		      (second beamxy) 
		    (if above
			(if (audible-p gr0)  
			    (if (stem-is-up? gr0) 
				(stem-end gr0) 
			      (box-y0 gr0)) 
			  (+ (box-y0 gr0)
			     (if (rest-p gr0) (box-y1 gr0) 0.0)))
		      (if (or (rest-p gr0) (stem-is-up? gr0))
			  (+ (box-y0 gr0)
			     (if (rest-p gr0) (box-y1 gr0) 0.0)) ; was y0? (and below) 29-Mar-02
			(stem-end gr0))))
		  (box-y0 gr0)
		  0.0))
	 ;; ideally we'd search the notes under the bracket for other brackets and move one of them upwards
	 (ly1 (or (if beamxy 
		      (if (and (numberp subdivision) (= beamed-notes subdivision))
			  (fourth beamxy)
			(+ (second beamxy) 
			   (* (- (fourth beamxy) (second beamxy)) 
			      (divide (- x1 x0) 
				 (- (third beamxy) (first beamxy))))))
		    (if above
			(if (audible-p gr1)  
			    (if (stem-is-up? gr1) 
				(stem-end gr1) 
			      (box-y0 gr1)) 
			  (+ (box-y0 gr1)
			     (if (rest-p gr1) (box-y1 gr1) 0.0)))
		      (if (or (rest-p gr1) (stem-is-up? gr1))
			  (+ (box-y0 gr1)
			     (if (rest-p gr1) (box-y1 gr1) 0.0))
			(stem-end gr1))))
		  (box-y0 gr1)
		  0.0))
	 (dist (if (= (length group) 2) .5
		 (let* ((mid-onset (+ (odb-onset gr0) (* .5 (loop for gr in group sum (odb-duration gr)))))
			(grl (loop for gr in group do 
			       (if (or (= mid-onset (odb-onset gr)) 
				       (> (+ (odb-onset gr) (odb-duration gr)) mid-onset)) 
				   (return gr)))))
		   (if (and (numberp subdivision) 
			    (evenp subdivision)
			    (= mid-onset (odb-onset grl)))
		       (- (divide (- (+ (box-x0 grl) (center grl)) x0) (- x1 x0)) (divide 1.0 (* 2 subdivision)))
		     (divide (- (+ (box-x0 grl) (center grl)) x0) (- x1 x0)))))))
    (setf (subdivision mark) subdivision)
    (if (and bracketed beamxy
	     (or (eq bracketed t)
		 (stem-eq bracketed (stem-direction note))))
	(display-bracketed-number mark note score x0 ly0 x1 ly1 above dist)
      ;; now for the non-beam, bracketed case -- can follow either the note heads, stem ends, or a mixture
      ;; basic case is to try follow stem-end, but go above if anything doesn't fit
      ;; bracket tilt is not more than maximum-subdivision-bracket-tilt
      (if above
	    (let* ((max-y (if (> (length group) 2)
			      (or
			       (loop for gn in (cdr (butlast group))
			        maximize (if (audible-p gn)
					     (if (stem-is-up? gn)
						 (or (stem-end gn) (box-y0 gn))
					       (box-y0 gn))
					   (+ (box-y0 gn) (box-y1 gn))))
			       0)))
		   (y0 (if max-y (max ly0 max-y) ly0))
		   (y1 (if max-y (max ly1 max-y) ly1))
		   (diff (abs (- y1 y0))))
	      (if (> diff *maximum-subdivision-bracket-tilt*)
		  (if (< y0 y1)
		      (setf y0 (- y1 *maximum-subdivision-bracket-tilt*))
		    (setf y1 (- y0 *maximum-subdivision-bracket-tilt*))))
	      (display-bracketed-number mark note score x0 y0 x1 y1 above dist))
	  (let* ((min-y (if (> (length group) 2)
			    (or
			     (loop for gn in group 
			      minimize (if (or (rest-p gn) (stem-is-up? gn))
					   (box-y0 gn)
					 (or (stem-end gn) (box-y0 gn))))
			     0)))
		 (y0 (if min-y (min ly0 min-y) ly0))
		 (y1 (if min-y (min ly1 min-y) ly1))
		 (diff (abs (- y1 y0))))
	    (if (> diff *maximum-subdivision-bracket-tilt*)
		(if (> y0 y1)
		    (setf y0 (+ y1 *maximum-subdivision-bracket-tilt*))
		  (setf y1 (+ y0 *maximum-subdivision-bracket-tilt*))))
	    (display-bracketed-number mark note score x0 y0 x1 y1 above dist))))))


#|
;;; tests of subdivisions

(cmn staff treble (meter 2 4) (c4 te) (g4 te) (a4 te) (e5 te) (f5 te) (g5 te) 
     (c4 tq) (g4 te) (c4 te) (g4 tq) (e5 tq) (d5 te) (d5 te) (e5 tq)
     (c4 te) (c5 tq) (c5 tq) (c4 te))
(cmn staff treble (meter 2 4) (c4 (rq 1/6)) (d4 (rq 1/6)) (e4 (rq 1/6)) (f4 (rq 1/6)) (g4 (rq 1/6)) (a4 (rq 1/6)))
(cmn staff treble (meter 4 4) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)))
(cmn staff treble (meter 4 4) (c4 th) (c4 th) (c4 th))
(cmn staff treble d4 ts f4 ts a4 ts b4 s d5 s   
                  d4 ts f4 ts a4 ts b4 s a4 s
		  d4 s d4 s d4 ts d4 ts d4 ts
		  d4 s f4 s a4 ts b4 ts d5 ts
		  d4 s d4 ts d4 ts d4 ts d4 s)
(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 3) (dy .5)))) 
     (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi)))
(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 3) (bracket-type :down-up) (dy .5)))) 
     (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi)))
(cmn staff treble (c4 te no-beam) (rest (rq 1/3) no-beam) (e4 te no-beam))
(cmn staff treble c4 ts c4 ts c4 ts d4 e e4 e f4 e g4 e g4 ts g4 ts g4 ts)
(cmn staff treble (meter 4 4) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 4/5)) (c4 (rq 2/5)) (c4 (rq 2/5)))
(cmn staff treble c4 ts d4 ts e4 ts f4 q e4 ts d4 ts c4 ts)
(cmn (automatic-beams nil) staff treble c4 ts c4 ts c4 te c4 ts c4 ts c4 ts c4 ts c4 ts c4 ts c4 te) 
(cmn staff treble (g4 (rq 2/3)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) 
   (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 1/12)) (g4 (rq 2/3)))
(cmn staff treble c4 ts c4 ts c4 ts c4 e c4 ts c4 ts c4 ts c4 te.)
(cmn staff treble c4 ts c4 te. c4 te)
(cmn staff treble c4 (rq 2/9) c4 (rq 3/9) c4 (rq 4/9)) -- doesn't work
(cmn staff treble c4 (rq 1/9) c4 (rq 3/9) c4 (rq 3/9) c4 (rq 2/9))
(cmn (staff treble (B3 (RQ 2/5)) (B3 (RQ 4/5)) (B3 (RQ 4/5)))) 

|#



;;;
;;; ----------------    text (lyric, poco a poco, and so on -- any text connected by some pattern)
;;;
;;; text- -text -text- are the three functions (text without any "-" is just simple text)
;;; these take both the text (and font info and so on), and an optional text-counter 
;;;   the counter helps us keep track of multiple-lines of lyrics under a single line of music.

;;; context|connecting-pattern-level added by Anders Vinjar 21-June-00

(defclass connected-text (text tag-mixin thick)
  ((connecting-pattern :initarg :connecting-pattern :initform nil :accessor context-pattern)
   (connecting-pattern-level  :initarg :connecting-pattern-level :initform .3 :accessor context-pattern-level)
   (end-pattern :initarg :end-pattern :initform nil :accessor end-pattern)
   (data :initarg :data :initform nil :accessor context-data)
   (letters :accessor context-letters)	;from text class
   (x :accessor context-x)
   (y :accessor context-y)
   (font-name :initform (normal-font) :accessor context-font-name) ;from font-mixin class
   (font-scaler :initform .5 :accessor context-font-scaler)
   (font-size :accessor context-font-size)
   (type :accessor context-type)	;from tag-mixin class
   (note :accessor context-note)))

;;; type is one of :-text :-text- :text- so we know where the dashed lines are.
;;; name is the counter disambiguating this text sequence from others simultaneous with it
;;; mark is our display function (occurs on mark list of first note)
;;; letters is the actual text -- also has font-name font-size font-scaler, and so on

(defmethod connected-text-p ((obj t)) nil)
(defmethod connected-text-p ((obj connected-text)) t)

(deferred-action end-pattern)

(defmethod descry ((txt connected-text) &optional stream controller)
  (format stream "~A~A~A~A~A~A"
	  (if (not controller) (format nil "(~(~A~)" (tag-type txt)) "")
	  (if (context-data txt) (format nil " :data ~A" (context-data txt)) "")
	  (if (context-pattern txt) (format nil " :connecting-pattern ~A" (context-pattern txt)) "")
          (if (context-pattern-level txt) (format nil " :connecting-pattern-level ~A" (context-pattern-level txt)) "")
	  (if (next-method-p) (call-next-method txt stream (or controller txt)) "")
	  (if (not controller) ")" "")))

(defun identify-box (box)
  (format nil "~A~A"
	  (if (not (zerop (x0 box))) (format nil " (x0 ~A)" (not-rational (x0 box))) "")
	  (if (not (zerop (y0 box))) (format nil " (y0 ~A)" (not-rational (y0 box))) "")))

(defmethod identify ((txt connected-text))
  ;; even if the original didn't use a tag variable, we will.  No real complication here except the x and y fields.
  ;; If lisp ever gives us access to the interpreted function description, we could do something reasonable with x/y.
  (if (eq (context-type txt) :text-)
      (let ((text-tag-name (new-cmn-store-tag "text-")))
	;; tie through the list in (context-data txt) (a list of connected-text instances, each pointing to context-note)
	;; only the first has the-usual-suspects.  The rest just tie into text-tag-name.
	(loop for tag in (context-data txt) do
	  (add-to-cmn-store-tags (context-note tag)
				 (format nil "(~(~A~) ~S ~A~A)" 
					 (context-type tag) 
					 (context-letters tag) 
					 text-tag-name
					 (identify-box tag))))
	(format nil "(setf ~A (text- ~S~A~A~A~A~A~A))" 
		text-tag-name 
		(context-letters txt) 
		(if (context-font-name txt) 
		    (format nil " (font-name ~S)" 
			    (context-font-name txt))
		  "")
		(if (context-font-size txt) 
		    (format nil " (font-size ~D)" (context-font-size txt))
		  (if (context-font-scaler txt) 
		      (format nil " (font-scaler ~1,3F)" (context-font-scaler txt)) 
		    ""))
		(if (context-pattern txt)
		    (if (listp (context-pattern txt))
                        (format nil " (connecting-pattern '(~D ~D))" (first (context-pattern txt)) (second (context-pattern txt)))
                      (format nil " (connecting-pattern ~A)" (context-pattern txt))
                      )
		  "")
		(if (context-pattern-level txt)
 		    (format nil " (connecting-pattern-level ~1,3F)"  (context-pattern-level txt))
 		  "")
		(identify-box txt)
		(the-usual-suspects txt)))
    ""))

(defmethod copy ((txt connected-text) &optional object)
  (let ((new-txt (if (not object) (make-instance 'connected-text)
		   (if (write-protected object) (copy object)
		     object))))
    (if (context-data txt)
	(setf (context-data new-txt) (loop for datum in (context-data txt) collect (copy datum))))
    (setf (context-pattern new-txt) (context-pattern txt))
    (setf (context-pattern-level new-txt) (context-pattern-level txt))
    (setf (context-note new-txt) (context-note txt))
    (if (next-method-p) (call-next-method txt new-txt))
    new-txt))

(defgeneric connected-text-x (note txt) )
(defmethod connected-text-x (note txt) (+ (box-x0 note) (vis-dx txt) (center note)))
(defmethod connected-text-x ((staff staff-mixin) txt) (+ (vis-dx txt) (box-x1 txt)))
(defmethod connected-text-x ((system system-mixin) txt) (+ (vis-dx txt) (box-x1 txt)))
(defmethod connected-text-x ((bar bar-mixin) txt) (+ (vis-dx txt) (box-x0 bar)))

(defgeneric connected-text-y (note mark score) )
(defmethod connected-text-y (note mark score) (declare (ignore score)) (+ (box-y0 mark) (vis-dy mark) (* (minimum-line note) *staff-line-separation*)))
(defmethod connected-text-y ((staff staff-mixin) mark score) (declare (ignore score)) (+ (vis-dy mark) (box-y0 mark)))
(defmethod connected-text-y ((system system-mixin) mark score) (declare (ignore score)) (+ (vis-dy mark) (box-y0 mark)))
(defmethod connected-text-y ((bar bar-mixin) mark score) (declare (ignore score)) (+ (vis-dy mark) (staff-y0 bar)))

(defun connecting-pattern (pattern)
  (make-self-acting 
   :action #'(lambda (text pattern) (setf (context-pattern text) pattern)) 
   :argument pattern))

(defun connecting-pattern-level (pattern-level)
  (make-self-acting 
   :action #'(lambda (text pattern-level) (setf (context-pattern-level text) pattern-level)) 
   :argument pattern-level))
 
(defun ctx (owner txt score)
  (if (text-x txt) 
      (funcall (text-x txt) txt owner score)
    (connected-text-x owner txt)))

(defun cty (owner txt score)
  (if (text-y txt) 
      (funcall (text-y txt) txt owner score)
    (connected-text-y owner txt score)))

(defmethod display ((mark connected-text) note score &optional justifying)
  (if (not justifying)
      (when (eq (context-type mark) :text-)
	(let ((all-texts (append (list mark) (context-data mark))))
	  (if (apply #'= (loop for text in all-texts collect (staff-y0 (context-note text))))
	      (display-connected-text all-texts mark note score justifying)

	    ;; otherwise, we have to continue across at least one line break.
	    ;; Since, there may be any number of lines between any two connecting notes,
	    ;; we have to find which staff we are on, and add fixups for end-of-line if
	    ;; the connection is not the last note, and start-of-line if not first note,
	    ;; and both for each line floating in between somewhere.  So we collect
	    ;; the staff-y0's, find the associated staff, look for missing staff-y0's
	    ;; and mark those, get end-of-first staff, start and end points for intermediate
	    ;; staves, and start-of-staff for the last (onset times), then finally
	    ;; run through these lists grouping the old and new text node and calling
	    ;; display-connected-text.

	    (let* ((first-text (first all-texts))
		   (first-note (context-note first-text))
		   (first-onset (odb-onset first-note))
		   (our-staff *cmn-staff*)
		   ;; *cmn-staff* is a debugging convenience -- it's bad form to use it this way.
		   ;; onset and note itself are the distinguishing things, not staff-y0 (which
		   ;; can be tied-equal, and can be equal across page breaks).
		   (staff-x0 (box-x0 our-staff))
		   (staff-x1 (box-x1 our-staff))
		   (default-pattern (context-pattern first-text))
		   (default-pattern-level (context-pattern-level first-text))
		   (default-x (text-x first-text))
		   (default-y (text-y first-text))
		   (default-dx (vis-dx first-text))
		   (default-dy (vis-dy first-text))
		   (all-data (staff-data our-staff)))
	      (loop while (and all-data (not (eq (first all-data) first-note))) do (pop all-data))
	      ;; now all-data should point at the first note
	      (if (null all-data) (cmn-error "aw good grief")
		(let* ((text-group nil)
		       (cur-text (first all-texts))
		       (cur-staff-y0 (staff-y0 first-note))
		       (left-text cur-text))
		  (loop while cur-text do
		    ;; push start if needed, get all for this line, push end if needed, send to display
		    ;; any new dy (et al) msg overrides for entire line
		    (if (/= (odb-onset (first all-data)) first-onset)
			(progn
			  (setf (context-note left-text) (short-note (context-note cur-text) :x0 (+ staff-x0 .5)))
			  (setf (staff-y0 (context-note left-text)) cur-staff-y0)
			  (if (not (context-pattern left-text)) (setf (context-pattern left-text) default-pattern))
			  (if (not (context-pattern-level left-text)) (setf (context-pattern-level left-text) default-pattern-level))
			  (if (not (text-x left-text)) (setf (text-x left-text) default-x))
			  (if (not (text-y left-text)) (setf (text-y left-text) default-y))
			  (if (zerop (vis-dx left-text)) (setf (vis-dx left-text) default-dx))
			  (if (zerop (vis-dy left-text)) (setf (vis-dy left-text) default-dy))
			  (setf (context-letters left-text) "")
			  (push left-text text-group)))
		    (loop while (and cur-text 
				     (= cur-staff-y0 (staff-y0 (context-note cur-text)))) do
		      (push cur-text text-group)
		      (loop while (not (eq (context-note cur-text) (first all-data))) do (pop all-data))
		      (setf left-text cur-text) ;in case line break and initial note(s) are included in previous text pattern
		      (setf cur-text (pop all-texts))
		      (when cur-text
			(if (context-pattern cur-text) (setf default-pattern (context-pattern cur-text)))
			(if (context-pattern-level cur-text) (setf default-pattern-level (context-pattern-level cur-text)))
			(if (text-x cur-text) (setf default-x (text-x cur-text)))
			(if (text-y cur-text) (setf default-y (text-y cur-text)))
			(if (not (zerop (vis-dx cur-text))) 
			    (setf default-dx (vis-dx cur-text))
			  (setf (vis-dx cur-text) default-dx))
			(if (not (zerop (vis-dy cur-text))) 
			    (setf default-dy (vis-dy cur-text))
			  (setf (vis-dy cur-text) default-dy))))
		    (if cur-text
			(let ((right-text (copy (first text-group))))
			  (setf (context-note right-text) (short-note (context-note (first text-group)) :x0 (- staff-x1 .25)))
			  (setf (context-letters right-text) "")
			  (push right-text text-group)
			  (loop while (and all-data (not (staff-p (first all-data)))) do (pop all-data))
			  (loop while (and all-data (not (audible-p (first all-data)))) do (pop all-data))
			  (setf first-onset (odb-onset (context-note cur-text)))))
		    (setf text-group (nreverse text-group))
		    (display-connected-text text-group (first text-group) note score justifying)
		    (loop while (and all-data (not (audible-p (first all-data)))) do (pop all-data))
		    (setf text-group nil)
		    (setf cur-staff-y0 (staff-y0 (first all-data))))))))))
    (if (not (eq (visible-justification mark) :none))
	(justify-connected-text mark note score))))


#|
(cmn staff treble (c4 q (text- "hi")) c4 q c4 q c4 q c4 q c4 q (line-mark) c4 q c4 q c4 q (c4 q (-text "ha")))
text across page break bug: (cmn staff treble (c4 q (text- "hi")) c4 q c4 q c4 q c4 q c4 q (page-mark) c4 q c4 q c4 q (c4 q (-text "ha")))
(cmn staff treble (c4 q (text- "hi")) c4 q c4 q c4 q c4 q c4 q (line-mark) (c4 q (-text- "ho")) c4 q c4 q (c4 q (-text "ha")))
(cmn (size 24) (line-separation 4.0) (automatic-line-breaks nil) (automatic-bars nil)     
  (system bracket (staff bar d4 h b3 q bf3 q bar quarter-rest (e4 q begin-slur
            (setf txt1 (text- "accel. al doppio movimento" (y #'(lambda (mark note score &rest rest) (+ (staff-y0 note) 1.5)))
               (connecting-pattern '(7 20)) (font-name (bold-italic-font))(font-scaler .5))) (begin-crescendo mp))
      (e4 h (grace-note f4)) (ds4 q) (dn4 q) (bar) (e4 h) (b3 q) (bf3 h) (g3 q end-crescendo end-slur)
      (bar) (e4 q p begin-slur) (e4 q (grace-note f4)) (ds4 q) (dn4 q) (e4 h) (bar) (LINE-MARK)
      (b3 q) (bf3 q) (e4 q) (g3 q) (fs3 q end-slur) (e4 q begin-tie begin-slur) bar 
      (e4 q end-tie) (ds4 q) (dn4 q) (e4 q) (b3 q) (bf3 q) (e4 q ) (g3 q)
      (fs3 q end-slur (mm 120 q (dx -1.5) unjustified in-parentheses) (-text txt1 "")) (bar )	(LINE-MARK))))
(cmn staff treble (c4 q (text- "hi" (dy 1.0))) d4 q line-break c4 q (c4 q (-text "ho")))
(cmn staff treble (c4 q (text- "hi" (dy 1.0))) d4 q line-break c4 q (c4 q (-text "ho" (dy 1.0))))
|#

(defun justify-connected-text (mark note score)
  (show score mark :matrix (translate-matrix score mark 
					     (+ (ctx note mark score) (vis-dx mark) -.125)
					     (+ (cty note mark score) (vis-dy mark)
						(if (not (y mark))
						    (- (staff-y0 note) .625)
						  0)))))

(defmethod display-connected-text-text ((text connected-text) score x0 y0)
  (let ((xx0 0))
    (matrix-front score (translate-matrix score text x0 y0))
    (when (plusp (length (letters text)))	;might be just connecting dashes
      (show score text)
      (incf xx0 (* .15 (length (letters text)))))
    (if (member (end-pattern text) '(:bracket-down :bracket-up))
	(progn
	  (with-thickness score text (or (thickness text) (text-connecting-thickness score))
            (moveto score xx0 (* (context-pattern-level text) (context-font-scaler text)))
	    (rlineto score .3 0)
	    (rlineto score 0 (if (eq (end-pattern text) :bracket-up) .375 -.375))
	    (draw score)))
      (if (end-pattern text)
	  (funcall (end-pattern text) text score x0 y0)))
    (matrix-back score)))


;;; this version accommodates the connecting-pattern :dash (or 'dash)
;;; which places a centered dash (or further dashes if the distance is
;;; large) between text-marks, and lets the various #'-text- and
;;; #'-text marks have individual x-messages to handle justification of
;;; lyrics etc. (AV 3-Jul-00, 14-Dec-00, 3-Jan-01).

(defun text-allotment (letters)
  (loop for i from 0 below (length letters)
        sum (case (aref letters i)
              ((#\W) 2.25)
              ((#\M) 2.15)
              ((#\%) 2.0)
              ((#\m) 1.9)
              ((#\A #\D #\G #\H #\K #\N #\O #\Q #\U #\V #\X) 1.75)
              ((#\w #\Y) 1.7)
              ((#\B #\C #\R) 1.6)
              ((#\E #\L #\T #\Z) 1.5)
              ((#\F #\P #\S #\= #\- #\~) 1.35)
              ((#\b #\d #\g #\h #\k #\n #\o #\p #\q #\u #\v #\x #\y) 1.2)
              ((#\a #\c #\e #\z #\?) 1.05)
              ((#\s #\J) 0.95)
              ((#\f #\r #\I  #\! #\(  #\) #\' #\`) 0.8)
              ((#\i #\l #\j #\t #\/ #\\ #\; #\:) 0.7)
              ((#\, #\.) 0.6)
              ((#\|) 0.5)
              (t 1.2))))


(defun display-connected-text (all-texts mark note score &optional justifying)
  (declare (ignore note))
  (let* ((all-notes (loop for text in all-texts collect (context-note text)))
	 (all-centers (loop for note in all-notes and tmark in all-texts
                        collect 
			(if (text-x tmark) 
			    (funcall (text-x tmark) tmark note score)
			  (connected-text-x note tmark))))
	 (all-dists (loop for x0 in all-centers and x1 in (cdr all-centers)
                      collect (- x1 x0)))
	 (dist all-dists)
	 (last-text-is-not-null (or (plusp (length (letters (first (last all-texts)))))
				    (find-if #'dynamics-p (marks (context-note (first (last all-texts)))))))
	 (dx-mark (vis-dx mark))
	 (min-y0 (+ (vis-dy mark)
		    ;; this idiotic let needed by the new(!) clisp 
		    (let ((val
			   (loop for note in all-notes and tmark in all-texts
			     minimize
			     (if (text-y mark) 
				 (funcall (text-y mark) tmark note score)
			       (connected-text-y note tmark score)))))
		      (or val 0)))))
    (if (marks mark) (display-marks mark score justifying))
    (loop for text in all-texts and center in all-centers and note in all-notes do 
      (let* ((letdist (* (context-font-scaler text) .4 (text-allotment (letters text))))
	     (xx0 (+ center dx-mark -.125) )
	     (yy0 (if (not (y mark)) 
		      (+ min-y0 (- (staff-y0 note) .625))
		    min-y0)))
        (display-connected-text-text text score xx0 yy0)
	(let ((next-center-dist (pop dist)))
          (when (and next-center-dist (> next-center-dist (+ .75 letdist)))
 	    (moveto score (+ xx0 .05 letdist) (+ yy0 (* (context-pattern-level text) (context-font-scaler text))))
	    (let ((dst (if (or dist
			       last-text-is-not-null)
			   (- next-center-dist (+ .5 letdist))
			 (- next-center-dist letdist))))
              (with-thickness
               score text (or (thickness text) (text-connecting-thickness score))
               (cond
                ((equal (context-pattern text) '(0 0)) (rlineto score dst 0) (draw score))
                ((member (context-pattern text) '(:dash 'dash))
                 (if (< dst 3.0)
                     ;; put in one dash centered between syllables
                     (progn (rmoveto score   (+ 0.225 (-  (/ dst 2.0) 0.05)) 0)
                            (rlineto score 0.1 0)
                            (draw score))
                   ;; else put in some dashes 
                   (progn (loop for curr-x from 0. to (- dst 2.0) by 1.5
                            do
                            (rmoveto score 1.5 0)
                            (rlineto score 0.1 0))
                          (draw score))))
                ((member (context-pattern text) '(:arrow 'arrow))
                 (rlineto score dst 0)
                 (draw score)
                 (moveto score
                         (+ xx0 .15 letdist dst)
                         (+ yy0 (* (context-pattern-level text) (context-font-scaler text))))
                 (rlineto score -0.15 (max 0.05 (* (or (thickness text) (text-connecting-thickness score)) 1.5)))
                 (rlineto score 0.045 (min -0.05 (- (* (or (thickness text) (text-connecting-thickness score)) 1.5))))
                 (rlineto score -0.045 (min -0.05 (- (* (or (thickness text) (text-connecting-thickness score)) 1.5))))
                 (rlineto score 0.15 (max 0.05 (* (or (thickness text) (text-connecting-thickness score)) 1.5)))
                 (fill-in score))
                (t (rlineto score dst 0 :pattern (let* ((pat (or (context-pattern text) (text-connecting-pattern score)))
                                                        (scl (/ (scr-size score) 40)))
                                                   (map 'list #'(lambda (n) (* n scl)) pat)))
                   (draw score)))))))))))

(defun connected-ur-text (new-text &rest objects)
  (let ((old-text nil))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (let* ((args (argument act))
		   (first-arg (and args (if (listp args) (first args) args))))
	      (if (and first-arg
		       (connected-text-p first-arg))
		  (setf old-text first-arg)
		(funcall (action act) new-text args)))
	  (if (text-p act)
	      (copy act new-text)
	    (if (stringp act)
		(setf (letters new-text) act)
	      (cmn-warn "odd argument to ~(~A~): ~A" (context-type new-text) act))))))
    (values new-text old-text)))

(defun text- (&rest objects)
  ;; text placed at current position (can be note, staff, etc) with pattern connecting to the right
  (let ((new-text (apply #'connected-ur-text (make-instance 'connected-text :type :text-) objects)))
    (push new-text text-stack)
    (make-self-acting 
     :action #'(lambda (new-note ct)
		 (setf (context-note ct) new-note)
		 (add-to-marks new-note (list ct)))
     :argument new-text)))

(defun -text- (&rest objects)
  (multiple-value-bind
      (new-text old-text)
      (apply #'connected-ur-text (make-instance 'connected-text :type :-text-) objects)
    (if (not old-text)
	(if text-stack
	    (setf old-text (first text-stack))
	  (cmn-warn "-text- called, but no text- to connect to.")))
    (make-self-acting
     :action #'(lambda (new-note lct)
		 (let ((ct (first lct))
		       (ot (second lct)))
		   (setf (context-note ct) new-note)
		   (add-to-marks new-note (list ct))
		   (push ct (context-data ot))
		   nil))		;make sure notify doesn't leave us lying around on someone's marks list
     :argument (list new-text old-text))))

(defun -text (&rest objects)
  (multiple-value-bind
      (new-text old-text)
      (apply #'connected-ur-text (make-instance 'connected-text :type :-text) objects)
    (if (not old-text)
	(if text-stack
	    (setf old-text (first text-stack))
	  (cmn-warn "-text called, but no text- or -text- to connect to.")))
    (make-self-acting
     :action #'(lambda (new-note lct)
		 (let ((ct (first lct))
		       (ot (second lct)))
		   (setf (context-note ct) new-note)
		   (push ct (context-data ot))
		   (add-to-marks new-note (list ct))
		   (setf (context-data ot) (nreverse (context-data ot)))
		   (pop text-stack)
		   nil))		;make sure notify doesn't leave us lying around on someone's marks list
     :argument (list new-text old-text))))

(defmethod backpatch ((txt connected-text))
  (or (staff-p (context-note txt))
      (system-p (context-note txt))))

(defmethod backpatch-time ((txt connected-text) obj)
  (declare (ignore obj))
  (odb-onset txt))

(defun text_ (&rest args) (apply #'text- (connecting-pattern-level .0) args))
(defun _text_ (&rest args) (apply #'-text- (connecting-pattern-level .0) args))
(defun _text (&rest args) (apply #'-text (connecting-pattern-level .0) args))
(defun text-> (&rest args) (apply #'text- (connecting-pattern :arrow) args))
 
#|
(cmn staff treble (c4 h (text- "hi")) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!")))
(cmn staff treble (c4 h (text- "hi" (thickness .1))) (d4 q) (d4 q (-text- "ho")) (e4 q) (e4 h (-text "away!")))

(cmn staff treble 
     (c4 h (setf a1 (text- "hi")) 
	 (setf a2 (text- "yow" (y0 2.0))))
     (d4 q (-text- "za" a2 (y0 2.0))) 
     (d4 q (-text- "ho" a1)) 
     (e4 q (-text "!" (y0 2.0))) 
     (e4 h (-text "away!" a1)))

(cmn staff treble (c4 h (text- "hi")) (d4 e) (e4 h (-text "ho")))
(cmn staff treble (c4 h (text_ "hi" )) (d4 e) (e4 h (_text)))
|#


;;; from AV 3-Jul-00
;;; >From Kurt Stone, p. 299:
;;; 
;;; "A fine point of text placement - a tradition now frequently
;;; ignored - is to make a distinction between single-note words or
;;; syllables and those sung on more than one note:
;;; 
;;;   - single-note words and syllables are centered below the note;
;;;   - multinote words and syllables are aligned flush left with the
;;;     initial note
;;; 
;;; If a monosyllabic word or the last syllable of a multisyllabic
;;; word is to be sung on more than one note, an extension line or
;;; extender is drawn at period level from the end of the word (or
;;; from its punctuation mark, if any) to the last note.  Punctuation
;;; must appear in its normal position, i.e., not at the end of the
;;; extender."

#|
(defmacro lyric-verse (n)
  `(y ,#'(lambda (mark note score)
	   (declare (ignore score))
           (- (staff-y0 note)
              (+ .75 (* n (font-scaler mark)))))))

(defmacro lyric-center-justified ()
  `(x ,#'(lambda (mark note score)
	   (declare (ignore score))
           (- (+ (x0 note) (center note))
              (* 0.4
                 (* 0.175 (length (letters mark))))))))

(defmacro lyric-left-justified ()
  `(x ,#'(lambda (mark note score)
	   (declare (ignore mark score))
           (+ (x0 note) (center note)))))
|#
(defun lyric-verse (n)
  (y (lambda (mark note score)
       (declare (ignore score))
       (- (staff-y0 note)
	  (+ .75 (* n (font-scaler mark)))))))

(defun lyric-center-justified ()
  (x (lambda (mark note score)
       (declare (ignore score))
       (- (+ (x0 note) (center note))
	  (* 0.4
	     (* 0.175 (length (letters mark))))))))

(defun lyric-left-justified ()
  (x (lambda (mark note score)
       (declare (ignore mark score))
       (+ (x0 note) (center note)))))

;;; the functions in this interface are:
;;;
;;;     one-note syllables: (center-justified)
;;;
;;;      #'lyric         ; one word sung on one note, centered beneath note:
;;;      #'lyric-        ; first syllable in word, center-justified
;;;      #'-lyric-       ; single-note continuation, center-justified
;;;      #'-lyric        ; last syllable, sung on a single note, center-justified:
;;;
;;;     multi-note syllables: (left-justified)
;;;
;;;      #'lyric--       ; start of syllable spread over several notes, left-justified
;;;      #'-lyric--      ; multi-note continuation, left-justified
;;;      #'lyric_        ; mono-syllable melisma, left-justified, extended by extension-line
;;;      #'-lyric_       ; last-syllable melisma, left-justified, extended by extension-line 
;;;      #'_lyric        ; end extension-line
;;;
;;;      #'lyric-verse   ; can be used to automatically adjust to correct y-level

(defun lyric (&rest args)               
  (apply #'text- (connecting-pattern :dash) (lyric-center-justified) (lyric-verse 1) args))

(defun lyric- (&rest args)              
  (apply #'text- (connecting-pattern :dash) (lyric-center-justified) (lyric-verse 1) args))

(defun -lyric- (&rest args) 
  (apply #'-text- (connecting-pattern :dash) (lyric-center-justified) args))

(defun -lyric (&rest args)
  (apply #'-text   (connecting-pattern :dash) (lyric-center-justified)args))

(defun lyric-- (&rest args)              
  (apply #'text- (connecting-pattern :dash) (lyric-left-justified) (lyric-verse 1) args))

(defun -lyric-- (&rest args)              
  (apply #'-text- (connecting-pattern :dash) (lyric-left-justified)  args))

(defun lyric_ (&rest args)              
  (apply #'text-  (connecting-pattern-level .0) (connecting-pattern '(0 0)) (lyric-left-justified) (lyric-verse 1) args))

(defun -lyric_ (&rest args)             
  (apply #'-text- (connecting-pattern-level .0) (connecting-pattern '(0 0)) (lyric-left-justified)  args))

(defun _lyric (&rest args)
  (apply #'-text  (connecting-pattern-level .0) (connecting-pattern '(0 0)) args))

#|
(cmn (output-file "text.eps")
     (free-expansion-factor 3.6)
     (size 20)
     (automatic-line-breaks nil)
     (automatic-ties t)
     staff treble
     (c4 h (setf a1 (lyric "News"  (lyric-verse 1)  )))
     (d4 e (-lyric-- a1 "pap" ) (begin-slur) )
     (line-mark)
     (f4 e (end-slur) (tie-))
     (f4 e (-tie) (begin-slur))
     (c5 q (end-slur) (-lyric_ a1 "er") (begin-slur))
     (e5 e (end-slur) (_lyric a1) )
     (eighth-rest)
     (e4 q (tie-) (lyric_ "Down!"))
     (e4 e (-tie) (begin-slur))
     (d4 e (end-slur) (_lyric)))
|#




;;;
;;; ----------------    glissando
;;;
;;; (c4 h (glissando-to d4)) (c4 h begin-glissando) (g4 h end-glissando)
;;; ^ is like a trill -- needs to be fit into available space
;;;                           ^ is like a tie or slur
;;; (chord (notes c4 g4) begin-glissando) (chord (notes c4 g4) end-glissando) -- should this have lines connecting all notes?
;;; (c4 h (begin-glissando (text "hi mom" (font-name (normal-font)) (font-scaler .4))))


(defvar glissando-counter 0)

(defclass glissando (tag-mixin sundry)
  ((wavy-line :initarg :wavy-line :initform nil :accessor wavy-line)
   (minimum-length :initarg :minimum-length :initform .01 :accessor minimum-length)))

(deferred-action minimum-length)

(defmethod descry ((gliss glissando) &optional stream controller)
  (format stream "(glissando~A~A~A)"
	  (if (wavy-line gliss) (format nil " :wavy-line ~(~A~)" (wavy-line gliss)) "")
	  (if (minimum-length gliss) (format nil " :minimum-length ~A" (minimum-length gliss)) "")
	  (if (next-method-p) (call-next-method gliss stream (or controller gliss)) "")))

(defmethod identify ((gliss glissando))
  ;; gliss or port, gliss-to or begin/end-gliss, possible text in type field
  ;; for begin/end case, name is ctr number until fixed up by the end function
  ;; upshot is we can trust sunry-name and sundry-note
  (if (member (sundry-name gliss) '(:glissando-to :portamento-to))
      (format nil "(~(~A~) ~A~A~A~A~A)"
	      (sundry-name gliss)
	      (note-name (tag-note gliss))
	      (if (wavy-line gliss) " (wavy-line t)" "")
	      (if (minimum-length gliss) (format nil " (minimum-length ~A)" (minimum-length gliss)) "")
              (if (thickness gliss) (format nil " (thickness ~1,3F)" (thickness gliss)) "")
	      (the-usual-suspects gliss))
    (let ((gliss-tag-name (new-cmn-store-tag "gliss-")))
      (add-to-cmn-store-tags (tag-note gliss)
			     (format nil "(end-~(~A~) ~A)" (sundry-name gliss) gliss-tag-name))
      (format nil "(setf ~A (begin-~(~A~)~A~A~A~A))"
	      gliss-tag-name
	      (sundry-name gliss) 
	      (if (wavy-line gliss) " (wavy-line t)" "")
	      (if (minimum-length gliss) (format nil " (minimum-length ~A)" (minimum-length gliss)) "")
              (if (thickness gliss) (format nil " (thickness ~1,3F)" (thickness gliss)) "")
	      (the-usual-suspects gliss)))))

(defmethod copy ((gliss glissando) &optional object)
  (let ((new-gliss (if (not object) (make-instance 'glissando)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (wavy-line new-gliss) (wavy-line gliss))
    (setf (minimum-length new-gliss) (minimum-length gliss))
    (if (next-method-p) (call-next-method gliss new-gliss))
    new-gliss))

(defmethod backpatch ((gliss glissando))
  (eq (sundry-name gliss) :glissando-to))

(defmethod backpatch-time ((gliss glissando) obj)
  (+ (odb-onset obj) (odb-duration obj)))

(defun glissando-to (&rest objects)
  (let ((new-gliss (make-instance 'glissando :name :glissando-to :mark #'display-glissando-to)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-gliss (argument act))
	(if (text-p act)
	    (setf (tag-type new-gliss) act)
	  (if (note-p act)
	      (setf (tag-note new-gliss) act)))))
    new-gliss))

(defvar *cmn-glissando-extension* .125)

(defun display-glissando-to (mark note score &optional (desc "gliss"))
  (let* ((other-note (or (tag-note mark) note))
	 (end-line (or (note-line other-note) 
		       (place-of-note-given-note note other-note)))
	 ;; fillify, where the note-line field is set, happens before boxify (housing pass)
	 ;;   or markify, so the note-line field should be correct -- we want to use it if
	 ;;   possible because there may have been a clef change between the two notes.
 	 (diff (cond ((> end-line (line note)) *cmn-glissando-extension*)
                      ((< end-line (line note)) (- *cmn-glissando-extension*))
                      (t 0)))
	 (x0 (+ (box-x0 note) (vis-dx mark) (center note) .3))
  	 (x1 (+ (vis-dx mark)
		(vis-dx other-note)
		(if (and (not (zerop (box-x1 mark)))
  			 (> (box-x1 mark) (box-x0 note)))
		    (- (box-x1 mark) .25)
		  (if (and (= (staff-y0 note) (staff-y0 other-note))
			   (not (zerop (box-x0 other-note))))
		      (- (box-x0 other-note) .125)
		    (- (box-x1 score) .25)))))
	 (dist (max (- x1 x0) (or (minimum-length mark) 0)))
	 (y0 (+ (box-y0 note) (vis-dy mark) diff))
 	 (y1 (- (+ (staff-y0 note) (vis-dy mark) (* *staff-line-separation* end-line)) (* .5 diff)))
	 (thickness (or (thickness mark) *glissando-thickness*))
	 )

    (if (minimum-length mark) (setf x1 (max x1 (+ x0 (minimum-length mark)))))
    (if (not (wavy-line mark))
	(progn
	  ;; matrix and thickness added 27-June-00 Anders Vinjar
 	  (matrix-front score  (translate-matrix score mark x0 y0))
	  (setf (line-width score) thickness)
	  (moveto score 0 0)
	  (rlineto score (- x1 x0) (- y1 y0))
  	  (draw score)
 	  (setf (line-width score) 0)
	  (matrix-back score))
      (let ((angle (* (/ 360 (* 2 pi)) (atan (- y1 y0) dist)))
	    (count (round dist .4)))
	(matrix-front score (rotate-matrix (translate-matrix score mark x0 (- y0 .1)) angle))
	(with-color score mark
	  (draw-trill-sections score count))
	(matrix-back score)))
    (when (and (> dist 1.0)
	       (or (and (always-show-gliss-name score)
			desc)
		   (text-p (tag-type mark)))
	       (not (eq desc t)))
      (let ((angle (* (/ 360 (* 2 pi)) (atan (- y1 y0) dist))))
	(show score (if (text-p (tag-type mark))
			(tag-type mark)
		      (if (text-p desc) 
			  desc
			(text desc
			      (font-name (normal-font)) 
			      (font-scaler .4))))
	      :matrix (rotate-matrix (translate-matrix score mark (+ x0 .1) (+ y0 .1 (if (wavy-line mark) .1 0))) angle))))))

;;; (cmn staff treble (e4 h (glissando-to a3)) quarter-rest (c4 h  (dx 1.0) begin-glissando) (g4 h (dx 1.0) end-glissando))

(defun portamento-to (&rest objects)
  (let ((new-port (apply #'glissando-to objects)))
    (setf (sundry-name new-port) :portamento-to)
    (setf (sundry-mark new-port)
      #'(lambda (mark note score &optional justifying)
	  (declare (ignore justifying))
	  (display-glissando-to mark note score "port")))
    new-port))

(defun push-glissando (glissando) (push glissando glissando-stack) glissando)
(defun pop-glissando () (pop glissando-stack))

(defun begin-glissando (&rest objects)
  (let ((new-gliss (apply #'glissando-to objects)))
    (setf (sundry-name new-gliss) (incf glissando-counter))
    (push-glissando new-gliss)))

(defun begin-portamento (&rest objects) (apply #'begin-glissando objects))

(defun end-glissando-1 (&optional glissando port) 
  (let* ((t-glissando (or glissando (pop-glissando) (cmn-warn "no glissando to end")))
	 (sfa (make-self-acting 
	       :action #'(lambda (new-note &rest rest) 
			   (declare (ignore rest))
			   (setf (tag-note t-glissando) new-note))
	       :argument nil)))
    (if glissando
	(setf glissando-stack (remove glissando glissando-stack)))
    (setf (sundry-name t-glissando) (if port :portamento :glissando))
    (if port 
	(setf (sundry-mark t-glissando)
	  #'(lambda (mark note score &optional justifying)
	      (declare (ignore justifying))
	      (display-glissando-to mark note score port))))
    sfa))

(defun end-glissando (&optional gliss) (end-glissando-1 gliss "gliss"))
(defun end-portamento (&optional port) (end-glissando-1 port "port"))

(defun add-glissando (note &rest ns) 
  (declare (ignore ns))
  (let ((g (begin-glissando)))
    (push g (marks note))))

(defun glissando- (&rest objects) (apply #'begin-glissando objects))
(defun -glissando (&rest objects) (apply #'end-glissando objects))
(defun portamento- (&rest objects) (apply #'begin-portamento objects))
(defun -portamento (&rest objects) (apply #'end-portamento objects))

(defvar begin-glissando (make-self-acting :action #'add-glissando :argument nil))
(defvar end-glissando (make-self-acting 
		       :action #'(lambda (new-note &rest rest)
				   (declare (ignore rest))
				   (let ((t-g (or (pop-glissando) (cmn-warn "no glissando to end"))))
				     (setf (sundry-name t-g) :glissando)
				     (setf (tag-note t-g) new-note)))
		       :argument nil))

(defvar begin-portamento (make-self-acting :action #'add-glissando :argument nil))
(defvar end-portamento (make-self-acting 
			:action #'(lambda (new-note &rest rest)
				    (declare (ignore rest))
				    (let ((t-g (or (pop-glissando) (cmn-warn "no portamento to end"))))
				      (setf (sundry-name t-g) :glissando)
				      (setf (sundry-mark t-g)
					#'(lambda (mark note score &optional justifying)
					    (declare (ignore justifying))
					    (display-glissando-to mark note score "port")))
				      (setf (tag-note t-g) new-note)))
			:argument nil))




;;;
;;; ----------------    crescendo and diminuendo (the wedges, not the text)
;;;
;;; (c4 h (crescendo (duration 2.0)) (c4 h begin-crescendo) (g4 h end-crescendo)

(defvar crescendo-counter 0)

(defclass crescendo (tag-mixin sundry)
  ((width :initarg :width :initform nil :accessor width)
   (onset-offset :initarg :onset-offset :initform 0 :accessor onset-offset)
   (begin-dynamic :initarg :begin-dynamic :initform nil :accessor begin-dynamic :accessor crescendo-begin-dynamic)
   (end-dynamic :initarg :end-dynamic :initform nil :accessor end-dynamic :accessor crescendo-end-dynamic)
   (crescendo-user-data :initarg :crescendo-user-data :initform nil :accessor crescendo-user-data)
   ;;(justification :accessor justification) -- this is inherited from visible-mixin via score-object via sundry
   ))


(defmethod crescendo-p ((obj t)) nil)
(defmethod crescendo-p ((obj crescendo)) t)

(deferred-action width)
(deferred-action onset-offset)

(defmethod begin-dynamic (val) (make-self-acting :action #'(lambda (obj arg) (setf (begin-dynamic obj) (copy arg))) :argument val))
(defmethod end-dynamic (val) (make-self-acting :action #'(lambda (obj arg) (setf (end-dynamic obj) (copy arg))) :argument val))

(defmethod dsud ((csm crescendo) num-i)
  (if (not (crescendo-user-data csm)) 
      (setf (crescendo-user-data csm) (list 0 0 0 0)))
  (if (> (second num-i) 3) 
      (cmn-error "~(~A~) can't handle d~A~D" 
		 (tag-type csm)
		 (if (evenp (second num-i)) "x" "y")
		 (floor (second num-i) 2)))
  (setf (nth (second num-i) (crescendo-user-data csm)) (first num-i)))

(defmethod descry ((cresc crescendo) &optional stream controller)
  (format stream "(~(~A~)~A~A~A~A~A~A~A"
	  (tag-type cresc)
	  (if (width cresc) (format nil " :width ~A" (width cresc)) "")
	  (if (not (zerop (onset-offset cresc))) (format nil " :onset-offset ~1,3F" (onset-offset cresc)) "")
	  (if (crescendo-begin-dynamic cresc) (format nil " :begin-dynamic ~A" (descry (crescendo-begin-dynamic cresc))) "")
	  (if (crescendo-end-dynamic cresc) (format nil " :end-dynamic ~A" (descry (crescendo-end-dynamic cresc))) "")
	  (if (crescendo-user-data cresc) (format nil " :crescendo-user-data '~A" (crescendo-user-data cresc)) "")
	  (if (next-method-p) (call-next-method cresc stream (or controller cresc)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((cresc crescendo))
  ;; crescendo/diminuendo with/out duration/onset-offset, and begin/end same, also width
  ;; here we can run into line-breaks
  (if (or (not (onset-offset cresc)) 
	  (/= (onset-offset cresc) -.001))
      ;; otherwise we have a cresc created by split-crescendo which only marks a temporary line-crossing
      (format nil "(~(~A~)~A~A~A~A~A~A~A)"
	      (tag-type cresc)
	      (if (not (zerop (onset-offset cresc))) (format nil " (onset-offset ~1,3F)" (onset-offset cresc)) "")
	      (if (duration cresc) (format nil " (duration ~1,3F)" (duration cresc)) "")
	      (if (crescendo-begin-dynamic cresc) (format nil " (begin-dynamic ~A)" (identify (crescendo-begin-dynamic cresc))) "")
	      (if (crescendo-end-dynamic cresc) (format nil " (end-dynamic ~A)" (identify (crescendo-end-dynamic cresc))) "")
	      (if (crescendo-user-data cresc) (identify-user-data (crescendo-user-data cresc) nil) "")
	      (if (thickness cresc) (format nil " (thickness ~1,3F)" (thickness cresc)) "")
	      (the-usual-suspects cresc))
    ""))

(defmethod copy ((cresc crescendo) &optional object)
  (let ((new-cresc (if (not object) (make-instance 'crescendo)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (width new-cresc) (width cresc))
    (setf (onset-offset new-cresc) (onset-offset cresc))
    (setf (tag-note new-cresc) (tag-note cresc))
    (if (crescendo-begin-dynamic cresc) (setf (crescendo-begin-dynamic new-cresc) (copy (crescendo-begin-dynamic cresc))))
    (if (crescendo-end-dynamic cresc) (setf (crescendo-end-dynamic new-cresc) (copy (crescendo-end-dynamic cresc))))
    (setf (crescendo-user-data new-cresc) (crescendo-user-data cresc))
    (if (next-method-p) (call-next-method cresc new-cresc))
    new-cresc))

(defmethod backpatch ((cresc crescendo))
  t)

(defmethod backpatch-time ((cresc crescendo) obj)
  (if (and (odb-onset cresc) (odb-duration cresc))
      (+ (odb-onset cresc) (odb-duration cresc))
    (if (tag-note cresc)
	(progn
	  (setf (duration cresc) (- (odb-onset (tag-note cresc)) (or (odb-onset cresc) (odb-onset obj))))
	  (odb-onset (tag-note cresc)))
      (progn
	(setf (duration cresc) (odb-duration obj))
	(+ (odb-onset obj) (odb-duration obj))))))

(defun crescendo (&rest objects)
  (let ((new-cresc (make-instance 'crescendo :type :crescendo :mark #'display-crescendo)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-cresc (argument act))
	(if (dynamics-p act)
	    (if (crescendo-begin-dynamic new-cresc)
		(setf (crescendo-end-dynamic new-cresc) (copy act))
	      (setf (crescendo-begin-dynamic new-cresc) (copy act))))))
    new-cresc))

(defun diminuendo (&rest objects)
  (let ((cresc (apply #'crescendo objects)))
    (setf (tag-type cresc) :diminuendo)
    cresc))

(defun display-crescendo (mark note score &optional justifying)
  (when (not justifying)
    (let* ((dsize *dynamics-size*)
           (cresc (eq (tag-type mark) :crescendo))
           (begin-dynamics-size (* (if (crescendo-begin-dynamic mark)
                                       (let ((w (width (crescendo-begin-dynamic mark))))
                                         (if (zerop w)
                                             (* 0.35 (length (dynamics-mark (crescendo-begin-dynamic mark)))) w))
                                     0.0)
                                   dsize))
           (end-dynamics-size (* (if (crescendo-end-dynamic mark)
                                     (let ((w (width (crescendo-end-dynamic mark))))
                                       (if (zerop w)
                                           (* 0.35 (length (dynamics-mark (crescendo-end-dynamic mark)))) w))
                                   0.0)
                                 dsize))
           (x0-mark (+ (vis-dx mark) 
                       (if (or (> (box-x0 mark) (box-x0 note))
                               (/= (onset-offset mark) 0.0))
                           (box-x0 mark)
                         (+ (box-x0 note) .125))
                       (* .5 begin-dynamics-size)
                       (if (crescendo-user-data mark) (first (crescendo-user-data mark)) 0)))
           (x1-mark (+ (max (- (box-x1 mark) .125) (+ x0-mark .5))
                       (- (* .5 end-dynamics-size))
                       (if (crescendo-user-data mark) (third (crescendo-user-data mark)) 0)))
           (x0 x0-mark)
	   (ct (* (or (thickness mark) .25) (- x1-mark x0-mark))) 
           (x0-off (+ x0 ct))
           (x1 x1-mark)
           (width (or (width mark) .25))
           (half-width (* .5 width))
           (y0 (+ (staff-y0 note)
                  (vis-dy mark)
                  (box-y0 mark)
                  (if (member (visible-justification mark) '(:up :above))
		      (+ (max *dynamics-minimum-vertical-separation*
 			      (* (max 10 (+ (maximum-line note)
					    (if (member (stem-direction note) '(:up)) 8 4)))
				 *staff-line-separation*)
			      (+ (y1 note) 0.2))
			 (* 0.5 width))
		    (+ (min 0
 			    (- *dynamics-minimum-vertical-separation*)
			    (* (min -6 (- (minimum-line note)
					  (if (member (stem-direction note) '(:down)) 9 4)))
			       *staff-line-separation*)
			    (if (tag-note mark) (* (- (minimum-line (tag-note mark)) 2) *staff-line-separation*) 0))))))
           (y1 (+ y0 half-width))
           (y2 (- y0 half-width)))
      
      (when (crescendo-user-data mark)
        (incf y0 (if cresc (second (crescendo-user-data mark)) (fourth (crescendo-user-data mark))))
        (incf y1 (if (not cresc) (second (crescendo-user-data mark)) (fourth (crescendo-user-data mark))))
        (incf y2 (if (not cresc) (second (crescendo-user-data mark)) (fourth (crescendo-user-data mark)))))

      (when (crescendo-begin-dynamic mark)
	;; the "x0" above is the wedge point -- can be either left or rightmost x
	(show score (crescendo-begin-dynamic mark)
	      :matrix (scale-matrix (translate-matrix score mark 
						      (+ (dx (crescendo-begin-dynamic mark)) (- x0 begin-dynamics-size .2))
						      (+ (dy (crescendo-begin-dynamic mark)) (- y0 .125)))
				    dsize dsize)
	      :data (dynamics-mark (crescendo-begin-dynamic mark))))
      
      (when (crescendo-end-dynamic mark)
	(show score (crescendo-end-dynamic mark)
	      :matrix (scale-matrix (translate-matrix score mark
                                                      (+ x1 (dx (crescendo-end-dynamic mark)) .2)
                                                      (+ (dy (crescendo-end-dynamic mark)) (- y0 .125)))
				    dsize dsize)
	      :data (dynamics-mark (crescendo-end-dynamic mark))))

      (matrix-front score (if cresc
                              (translate-matrix score mark x0 y0)
                            (mirror-matrix
                             (translate-matrix score mark x1 y0))))      
      
      (with-color score mark
                  (moveto score 0 0)
                  (lineto score (- x1 x0) (- y1 y0))
                  (lineto score (- x0-off x0) 0)
                  (lineto score (- x1 x0) (- y2 y0))
                  (lineto score 0 0)
                  (fill-in score))
      (matrix-back score))))

;;; (cmn (size 100) treble c4 q (crescendo mf))
;;; (cmn (left-margin 0.1) (right-margin 0.1) (system (staff c5 q begin-diminuendo d5 q e5 q g5 q a5 q end-diminuendo)))

(defun push-crescendo (crescendo) (push crescendo crescendo-stack) crescendo)
(defun pop-crescendo () (pop crescendo-stack))
(defun push-diminuendo (diminuendo) (push diminuendo diminuendo-stack) diminuendo)
(defun pop-diminuendo () (pop diminuendo-stack))

(defun begin-crescendo (&rest objects)
  (let ((new-cresc (apply #'crescendo objects)))
    (setf (sundry-name new-cresc) (incf crescendo-counter))
    (push-crescendo new-cresc)))

(defun begin-diminuendo (&rest objects)
  (let ((new-cresc (apply #'crescendo objects)))
    (setf (sundry-name new-cresc) (incf crescendo-counter))
    (setf (tag-type new-cresc) :diminuendo)
    (push-diminuendo new-cresc)))

(defun end-crescendo (&optional crescendo)
  (let* ((t-crescendo (or crescendo (pop-crescendo) (cmn-warn "no crescendo end point")))
	 (sfa (make-self-acting 
	       :action #'(lambda (new-note &rest rest) 
			   (declare (ignore rest))
			   (if t-crescendo
			       (setf (tag-note t-crescendo) new-note)))
	       :argument nil)))
    (if crescendo
	(setf crescendo-stack (remove crescendo crescendo-stack)))
    sfa))

(defun end-diminuendo (&optional diminuendo)
  (let* ((t-diminuendo (or diminuendo (pop-diminuendo) (cmn-warn "no diminuendo end point")))
	 (sfa (make-self-acting 
	       :action #'(lambda (new-note &rest rest) 
			   (declare (ignore rest))
			   (if t-diminuendo
			       (setf (tag-note t-diminuendo) new-note)))
	       :argument nil)))
    (if diminuendo
	(setf diminuendo-stack (remove diminuendo diminuendo-stack)))
    sfa))

(defun crescendo- (&rest objects) (apply #'begin-crescendo objects))
(defun -crescendo (&rest objects) (apply #'end-crescendo objects))
(defun diminuendo- (&rest objects) (apply #'begin-diminuendo objects))
(defun -diminuendo (&rest objects) (apply #'end-diminuendo objects))

(defvar begin-crescendo (make-self-acting 
			 :action #'(lambda (note &rest ns)
				     (declare (ignore ns))
				     (push (begin-crescendo) (marks note)))
			 :argument nil))

(defvar end-crescendo (make-self-acting 
		       :action #'(lambda (new-note &rest rest)
				   (declare (ignore rest))
				   (let* ((t-crescendo (or (pop-crescendo) (cmn-warn "no crescendo end point"))))
				     (if t-crescendo
					 (setf (tag-note t-crescendo) new-note))))
		       :argument nil))

(defvar begin-diminuendo (make-self-acting 
			  :action #'(lambda (note &rest ns)
				      (declare (ignore ns))
				      (push (begin-diminuendo) (marks note)))
			  :argument nil))

(defvar end-diminuendo (make-self-acting 
			:action #'(lambda (new-note &rest rest)
				    (declare (ignore rest))
				    (let* ((t-diminuendo (or (pop-diminuendo) (cmn-warn "no diminuendo end point"))))
				      (if t-diminuendo
					  (setf (tag-note t-diminuendo) new-note))))
			:argument nil))

(defvar crescendo (make-self-acting 
		   :action #'(lambda (note &rest ns)
			       (declare (ignore ns))
			       (push (crescendo) (marks note)))
		   :argument nil))

(defvar diminuendo (make-self-acting 
		    :action #'(lambda (note &rest ns)
				(declare (ignore ns))
				(push (diminuendo) (marks note)))
		    :argument nil))

(defun split-crescendo (mark note staff score)
  ;; here we have a crescendo crossing either a line or page break. (called in cmn4)
  (let* ((right-note (tag-note mark))
	 (right-staff staff)		;(cmn staff treble (c4 h begin-diminuendo) c4 h line-mark (eighth-rest end-diminuendo))
	 ;; this can be a faked-up note created within justify with staff-y0 = to the line's top staff y0!
	 (tagged-note (let ((on-the-same-page nil))
			(loop for object in (staff-data staff) do
			  (if on-the-same-page
			      (if (staff-p object) 
				  (setf right-staff object)
				(if (and (or (audible-p object) (rest-p object))
					 (/= (staff-y0 object) (staff-y0 note)))
				    ;; if right-note used, <= staff-y0 object right-note should work
				    ;; all we're trying to do here is find the first rest or note on the next line (sigh)
				    (return object)))
			    (if (eq note object) 
				(setf on-the-same-page t)))))))

    (if (not tagged-note) 
	(cmn-error "attempt to split a ~(~A~) across a line-break failed: ~1,4F and ~1,4F from ~A to ~A" 
	       (tag-type mark) (staff-y0 right-note) (staff-y0 note) 
	       (brief-identify note) (brief-identify right-note)))

    (let* ((left-cresc mark)
	   (right-cresc (make-instance 'crescendo :x1 (find-time-line-position-of (+ .01 (odb-onset mark) (odb-duration mark)) 
									(box-x0 staff) (box-x1 right-staff)
									(time-line score))
					;; the .01 is to keep us from hitting the exact end of the line by accident
					:dy (vis-dy mark) :dx (vis-dx mark) 
					:mark #'display-crescendo
					:width (width mark) :type (tag-type mark))))

      (setf (box-x1 left-cresc) (box-x1 staff))
      (setf (box-x0 right-cresc) (+ (box-x0 staff) .5))
      (setf (onset-offset right-cresc) -.001) ;a kludge to force display-crescendo to believe x0
      
      (when (crescendo-end-dynamic left-cresc)
	(setf (crescendo-end-dynamic right-cresc) (crescendo-end-dynamic left-cresc))
	(setf (crescendo-end-dynamic left-cresc) nil))
      (setf (tag-note left-cresc) nil)
      (add-to-marks tagged-note (list right-cresc)))))

(defvar always-dyn-up ;; for vocal staffs etc. (AV 3-Jul-00)
  (make-self-acting  
   :action #'(lambda (staff &rest rest) 
               (loop for obj in (staff-data staff) do 
                 (cond ((note-p obj)
                        (loop for mrk in (marks obj)
                          do
                          (when (or (crescendo-p mrk) (dynamics-p mrk))
                            (and (member (visible-justification mrk) '(nil :none))
                                 (setf (visible-justification mrk) :above))))))))
   :argument nil))

#|
(cmn staff treble
   (bartok-bounce
      h
      (notes (e5 (lyric_ "det")
                 (begin-crescendo (justification :above)
                                  (end-dynamic f))
                 (begin-diminuendo  (dx 0.0)
                                    (justification :above)))
             (D5)
             (C5)
             (B4 (_lyric) (end-crescendo))
             (A4 (lyric_ "var"))
             (G4)
             (F4 (end-diminuendo) (_lyric)) )
      (spacing -0.14) (start-beams 3) (end-beams 1))
)
|#

;;;
;;; ----------------    first-ending and second-ending
;;;
;;; (bar begin-first-ending) ... (bar end-first-ending begin-second-ending) etc
;;;
;;; this does not currently handle multi-line endings well (the middle lines have no ending indicator, etc)
;;; we probably need to attach these lines as marks to the staves -- see connected text display method

(defclass ending (tag-mixin sundry font-mixin)
  ((max-line :initarg :max-line :initform nil :accessor max-line)
   (terminal :initarg terminal :initform nil :accessor terminal)
   (font-name :initform (normal-font))
   (font-scaler :initform 0.4)))

(defmethod ending-p ((obj t)) nil)
(defmethod ending-p ((obj ending)) t)

(defmethod descry ((end ending) &optional stream controller)
  (format stream "(~(~A~)-ending~A~A~A)"
	  (tag-type end)
	  (if (max-line end) (format nil " :max-line ~D" (max-line end)) "")
	  (if (terminal end) " :terminal t" "")
	  (if (next-method-p) (call-next-method end stream (or controller end)) "")))

(defmethod identify ((end ending))
  (add-to-cmn-store-tags (tag-note end) (format nil "(end-~(~A~)-ending)" (tag-type end)))
  (format nil "(begin-~(~A~)-ending~A~A)"
	  (tag-type end)
	  (if (terminal end) " (terminal t)" "")
	  (the-usual-suspects end)))

(defmethod copy ((end ending) &optional object)
  (let ((new-end (if (not object) (make-instance 'ending)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (terminal new-end) (terminal end))
    (setf (max-line new-end) (max-line end))
    (if (next-method-p) (call-next-method end new-end))
    new-end))

(defvar pending-first-ending nil)
(defvar pending-second-ending nil)

(defmethod backpatch ((end ending)) nil)

(defun display-ending (mark bar score &optional justifying)
  (let* ((barp (bar-p bar))
	 (bartag (tag-note mark))
	 (bartagp (bar-p bartag))
	 (stfy0-bartag (staff-y0 bartag))
	 (end-bar (and barp (eq (visible-justification bar) :right)))
	 (broken-ending (and (/= (staff-y0 bar) stfy0-bartag)
			     (not end-bar))))
    (when (not justifying)
      (if (not bartag) (cmn-warn "ending lost its end point"))
      (setf (terminal mark) (and bartagp (terminal bartag)))
      (if (zerop (box-x1 mark)) 
	  (if (not broken-ending)
	      (setf (box-x1 mark) (+ (box-x0 bartag)
				     (if (and bartagp (dots-left bartag))
					 (if (eq (visible-justification bartag) :right)
					     .025
					   .25)
				       0)))
	    (setf (box-x1 mark) (+ (box-x1 score) .125)))))
    (let* ((first (eq (tag-type mark) :first))
	   (dyf (font-scaler mark))
	   (y0 (+ (vis-dy mark) 
		  (max (box-y1 (if end-bar bartag bar))
		       (if (not barp)
			   (+ (staff-y0 bar) 1.0)
			 0.0)
		       (if (and end-bar (not bartagp))
			   (+ stfy0-bartag 1.0) 
			 0.0)
		       (+ (box-y0 (if end-bar bartag bar))
			  (* (or (max-line mark) 0) 
			     *staff-line-separation*)))
		  (* dyf 1.25)))
	   (x0 (+ (if end-bar
		      (+ (box-x0 score) 1.0)
		    (+ (box-x0 bar) (vis-dx bar)
		       (if (and barp (dots-left bar)) .25 0) 
		       (if (and barp (dots-right bar)) -.175 0)))
		  (vis-dx mark) 
		  (if first 0 .025)))
	   (x1 (- (box-x1 mark) 
		  (vis-dx mark) 
		  (if (and bartagp (dots-right bartag)) .175 0) 
		  (if first .025 0))))
      
      (moveto score x0 (- y0 dyf))
      (lineto score x0 y0)
      (lineto score x1 y0)
      (if (and (not broken-ending)
	       (or first (terminal mark)))
	  (lineto score x1 (- y0 dyf)))
      (draw score)
      (show score (cmn-text :letters
                            (format nil "~D."
                                    (if first 1 ; Andreas Menke 12-Mar-89
                                      (if (numberp (tag-type mark))
                                          (tag-type mark)
                                        2)))
			    :font-name (font-name mark)
			    :font-scaler dyf)
	    :matrix (translate-matrix score mark (+ x0 .1) (+ (- y0 dyf) .05)))
      (when broken-ending
	(let* ((x0 (- (box-x0 *cmn-staff*) .5)) ;(+ (box-x0 score) .5)) is not right, but neither is *cmn-staff*!
					; A.M. suggests (inches-to-font-units score (left-margin score))
	       (x1 (- (+ (box-x0 bartag)
			 (if (and bartagp (dots-left bartag)) .25 0))
		      (if first .05 0)))
	       (y0 (+ (vis-dy mark) 
		      (max (box-y1 bartag)
			   (if (not bartagp) (+ stfy0-bartag 1.0) 0.0)
			   (+ (box-y0 bartag)
			      (* (or (max-line mark) 0) 
				 *staff-line-separation*)))
		      (* dyf 1.25))))
	  (moveto score x0 y0)
	  (lineto score x1 y0)
	  (if (or first (terminal mark))
	      (lineto score x1 (- y0 dyf)))
	  (draw score))))))
	

#|
;;; some tests
(cmn staff treble c4 q (bass begin-first-ending) c4 q (bar end-first-ending))
(cmn staff treble (meter 2 4) (c4 q) (c4 q) (bar begin-first-ending) (c4 q) (c4 q) 
     (end-repeat-bar end-first-ending begin-second-ending) (c4 q) (c4 q end-second-ending) 
     bar c4 q (c4 q begin-first-ending) bar (c4 q) (c4 q) 
     (begin-and-end-repeat-bar end-first-ending begin-second-ending) c4 q (quarter-rest end-second-ending) 
     bar c4 q c4 q (bass begin-first-ending) bar (c4 q) (c4 q) (bar end-first-ending))
(cmn (size 60) staff treble (meter 2 4) (c4 q) (c4 q) (bar begin-first-ending) (c4 q) (c4 q) 
     (end-repeat-bar end-first-ending begin-second-ending) (c4 q) line-mark (c4 q end-second-ending) 
     bar c4 q (c4 q begin-first-ending) bar (c4 q) (c4 q) (begin-and-end-repeat-bar end-first-ending begin-second-ending) 
     c4 q (quarter-rest end-second-ending) bar c4 q c4 q (bass (dx -.25) begin-first-ending) bar (c4 q) (c4 q) (bar end-first-ending))
(cmn staff treble c4 q (bass begin-first-ending) c4 q (bar end-first-ending begin-second-ending)
     c4 q (bar end-second-ending (begin-other-ending 3)) c4 q (end-other-ending))
|#


(defun first-ending (&rest objects)
  (let ((new-end (make-instance 'ending :type :first :mark #'display-ending)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-end (argument act))))
    (setf pending-first-ending new-end)
    new-end))

(defun second-ending (&rest objects)
  (let ((new-end (make-instance 'ending :type :second :mark #'display-ending)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-end (argument act))))
    (setf pending-second-ending new-end)
    new-end))

(defun begin-first-ending (&rest objects)
  (make-self-acting 
   :action #'(lambda (note &rest ns)
	       (declare (ignore ns))
	       (push (apply #'first-ending objects) (marks note)))
   :argument nil))

(defun begin-second-ending (&rest objects)
  (make-self-acting 
   :action #'(lambda (note &rest ns)
	       (declare (ignore ns))
	       (push (apply #'second-ending objects) (marks note)))
   :argument nil))

(defun end-pend (new-note which)
  (if (string-equal which "first")
      (progn
	(if (not pending-first-ending) (cmn-warn "end-first-ending without matching begin?"))
	(setf (tag-note pending-first-ending) new-note)
	(setf pending-first-ending nil))
    (progn
      (if (not pending-second-ending) (cmn-warn "end-second-ending without matching begin?"))
      (setf (tag-note pending-second-ending) new-note)
      (setf pending-second-ending nil))))

(defun end-first-ending (&optional (which "first"))
  (make-self-acting 
   :action #'(lambda (new-note &rest rest) 
	       (declare (ignore rest))
	       (end-pend new-note which))
   :argument nil))

(defun end-second-ending () 
  (end-first-ending "second"))

(defvar begin-first-ending (begin-first-ending))
(defvar begin-second-ending (begin-second-ending))
(defvar end-first-ending (end-first-ending))
(defvar end-second-ending (end-second-ending))


;;; third ending and up courtesy Andreas Menke (12-Mar-98):

(defun other-ending (number &rest objects)
  (let ((new-end (make-instance 'ending :type number :mark #'display-ending)))
    (loop for act in objects do
      (if (self-acting-p act)
          (funcall (action act) new-end (argument act))))
    (when pending-second-ending
          (cmn-error "pending second/other ending!"))
    (setf pending-second-ending new-end)
    new-end))

(defun begin-other-ending (number &rest objects)
  (make-self-acting
   :action #'(lambda (note &rest ns)
               (declare (ignore ns))
               (push (apply #'other-ending number objects) (marks note)))	
   :argument nil))

(defun end-other-ending (&optional number)
  (declare (ignore number))
  (end-first-ending "second"))


;;;
;;; ----------------    ties
;;;
;;; (this code is a mess because I changed my mind halfway through...I should have triggered on right not left).

(defvar tie-counter 0)

(defclass tie (tag-mixin breathing-space thick)
  ((name :accessor tie-name)
   (type :accessor tie-type)
   (note :accessor tie-note)
   (staff :initarg :tie-staff :initform nil :accessor tie-staff)
   (direction :initarg :direction :initform nil :accessor tie-direction :accessor cmn-tie-direction)
   (curvature :initarg :curvature :initform nil :accessor tie-curvature)
   (dashed :initarg :dashed :initform nil :accessor dashed)
   (user-data :initarg :tie-user-data :initform nil :accessor tie-user-data)
   (breathing-space :initform .04)
   (move-right :initarg :tie-move-right :initform nil :accessor tie-move-right)
   (added :initarg :tie-added :initform nil :accessor tie-added)))

(deferred-action tie-direction)
(deferred-action tie-curvature)

(defmethod (setf tie-direction) (dir (note audible)) (setf (tie-direction (first (ties note))) dir))
(defmethod (setf tie-curvature) (crv (note audible)) (setf (tie-curvature (first (ties note))) crv))
(defmethod (setf tie-curvature) (n (scr score)) (setf *tie-curvature* n) nil)

(defmethod identify ((tie tie))
  (if (eq (tie-type tie) :left)
      (format nil "(begin-tie~A~A~A~A~A~A~A~A)"
	      (if (tie-curvature tie) (format nil " (tie-curvature ~1,3F)" (tie-curvature tie)) "")
	      (if (tie-direction tie) (format nil " (tie-direction :~(~A~))" (tie-direction tie)) "")
	      (if (dashed tie) (format nil " (dashed '~A)" (dashed tie)) "")
	      (if (tie-user-data tie) (identify-user-data (tie-user-data tie) nil) "")
	      (if (/= (breathing-space tie) .04) (format nil " (breathing-space ~1,3F)" (breathing-space tie)) "")
	      (if (tie-move-right tie) "  (tie-move-right t)" "")
	      (if (tie-added tie) " (tie-added t)" "")
	      (identify-visible tie))
    (if (eq (tie-type tie) :right)
	"(end-tie)"
      "")))

(defmethod dsud ((tm tie) num-i)
  (if (not (tie-user-data tm)) 
      (setf (tie-user-data tm) (list 0 0 0 0)))
  (if (> (second num-i) 3) 
      (cmn-error "tie can't handle d~A~D" 
	     (if (evenp (second num-i)) "x" "y")
	     (floor (second num-i) 2)))
  (setf (nth (second num-i) (tie-user-data tm)) (first num-i)))

(defun sorted-ties (ties)
  ;; this ensures end-tie precedes begin-tie in cmn-store (lisp's sort is destructive)
  (append (loop for tie in ties
	   if (eq (tie-type tie) :right)
	   collect (identify tie))
	  (loop for tie in ties
	   if (eq (tie-type tie) :left)
	   collect (identify tie))))

(defun new-tie (&rest objects)
  (let ((new-tie (make-instance 'tie :name (incf tie-counter) :type :left)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-tie (argument act))
	(cmn-warn "odd argument to tie: ~A" act)))
    new-tie))

(defun user-add-to-ties (note object) 
  (when object
    (setf (tie-note object) note) 
    (push object (ties note))))

(defun user-add-to-left-ties (note possible-tie)
  (user-add-to-ties note (push-tie (let ((nt (or possible-tie (new-tie))))
				     (setf (tie-move-right nt) t)
				     nt))))

;;; (cmn (system (staff (as5 (rq 5/6) begin-tie) (bar) (as5 q end-tie)))) 

(defun user-add-to-right-ties (note possible-tie)
  (if possible-tie
      (let ((actual-tie (argument possible-tie)))
	(user-add-to-ties note (tie-tie actual-tie))
	(setf tie-stack (remove actual-tie tie-stack)))
    (user-add-to-ties note (tie-tie (pop-tie)))))

(defun push-tie (tie) (push tie tie-stack) tie)
(defun pop-tie () (pop tie-stack))

(defun copy-ties (new-chord note)
  (loop for tie in (ties note) 
   collect (let ((new-tie (copy tie)))
	     (setf (tie-note new-tie) new-chord)
	     (if (not (equal (tie-note tie) note))
		 (cmn-error "back patch screwed up: tie ~A note ~A (at ~1,3F) is not equivalent to ~A" 
			(descry tie) note (odb-onset note) (tie-note tie)))
	     new-tie)))

(defun tie-tie (&optional tie) 
  (let ((t-tie (or tie (pop-tie) (cmn-warn "no tie to end"))))
    (if t-tie (make-instance 'tie :name (tie-name t-tie) :type :right :curvature (tie-curvature t-tie)))))

(defun begin-tie (&rest objects)
  (make-self-acting :action #'user-add-to-left-ties :argument (apply #'new-tie objects)))
(defun tie- (&rest objects) (apply #'begin-tie objects))

(defun end-tie (&optional tie)		;not actually a tie instance
  (make-self-acting :action #'user-add-to-right-ties :argument tie))
(defun -tie (&optional tie) (end-tie tie))

(defvar begin-tie (make-self-acting :action #'user-add-to-left-ties :argument nil))
(defvar tie- (make-self-acting :action #'user-add-to-left-ties :argument nil))
(defvar end-tie (make-self-acting :action #'user-add-to-right-ties :argument nil))
(defvar -tie (make-self-acting :action #'user-add-to-right-ties :argument nil))

;;; the functions and variables have to be in sync here so that (c4 end-tie (begin-tie)) doesn't cause confusion


(defun find-tie (tie objects)
  (find-if #'(lambda (object)
	       (or (and (or (note-p object) (rest-p object))
			(ties object)
			(find (tie-name tie) (ties object) :key #'tie-name)
			object)
		   (and (chord-p object)
			(find-tie tie (chord-data object)))))
	   objects))

(defmethod tie-p ((obj t)) nil)
(defmethod tie-p ((obj tie)) t)

(defmethod descry ((tie tie) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) "(tie" "")
	  (if (tie-direction tie) (format nil " :direction :~(~A~)" (tie-direction tie)) "")
	  (if (tie-curvature tie) (format nil " :curvature ~A" (tie-curvature tie)) "")
	  (if (dashed tie) (format nil " :dashed '~A" (dashed tie)) "")
	  (if (tie-user-data tie) (format nil " :tie-user-data '~A" (tie-user-data tie)) "")
	  (if (/= (breathing-space tie) .04) (format nil " :breathing-space ~1,3F" (breathing-space tie)) "")
	  (if (tie-move-right tie) " :move-right t" "")
	  (if (tie-added tie) " :added t" "")
	  (if (next-method-p) (call-next-method tie stream (or controller tie)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((tie tie) &optional object)
  (let ((new-tie (if (not object) (make-instance 'tie)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (cmn-tie-direction new-tie) (cmn-tie-direction tie))
    (setf (tie-curvature new-tie) (tie-curvature tie))
    (setf (tie-staff new-tie) (tie-staff tie))
    (setf (tie-user-data new-tie) (tie-user-data tie))
    (setf (dashed new-tie) (dashed tie))
    (setf (breathing-space new-tie) (breathing-space tie))
    (setf (tie-move-right new-tie) (tie-move-right tie))
    (setf (tie-added new-tie) (tie-added tie))
    (if (next-method-p) (call-next-method tie new-tie))
    new-tie))

(defun solve-bezier (x x0 y0 y1 y2 x3 y3)
  (let* ((cy (* 3 (- y1 y0)))
	 (by (- (* 3 (- y2 y1)) cy))
	 (ay (- y3 y0 cy by))
	 (i (divide (- x x0) (- x3 x0))))
    (+ y0 (* i (+ cy (* i (+ by (* i ay))))))))

(defmethod display ((tie tie) container score &optional justifying)
  (when (and (not justifying) 
	     (member (tie-type tie) '(:left :prepared)))
    (if (and container (not (cmn-tie-direction tie))) (setf (cmn-tie-direction tie) (tie-direction container)))
    (prepare-note-tie score tie)
    (display-tie tie score justifying)))

(defun display-tie (tie score &optional justifying)
  ;; fixed by Anders Vinjar 25-Jan-99
  (let* ((user-data (tie-user-data tie))
	 (x0 (+ (vis-dx tie) (box-x0 tie) (if user-data (first user-data) 0)))
	 (x1 (- (+ (box-x1 tie) (if user-data (third user-data) 0)) (vis-dx tie)))
	 (y0 (+ (vis-dy tie) (box-y0 tie) (if user-data (second user-data) 0)))
	 (curvature (or (tie-curvature tie) *tie-curvature*))
	 (direction (cmn-tie-direction tie))
	 (thickness *tie-thickness*)
	 (dx (min .25 (* .2 (- x1 x0))))
	 (y1 (if (eq direction :up) 
		 (+ y0 curvature (if user-data (fourth user-data) 0))
	       (- y0 curvature (if user-data (fourth user-data) 0)))))
    (if (marks tie) (display-marks tie score justifying))
    (if (dashed tie)
	(let ((dash-width (second (dashed tie)))
	      (dash-step (first (dashed tie))))
	  (with-thickness score tie dash-width
	    (with-color score tie
	      (loop for x from (+ x0 dash-step) to (- x1 dash-step) by dash-step do
		(moveto score x (+ .01 (solve-bezier x x0 y0 y1 y1 x1 y0)))
		(rlineto score 0 (* -2 thickness)))
	      (draw score))))
      (with-color score tie
	(moveto score x0 y0)
	(curveto score (+ x0 dx) y1 (- x1 dx) y1 x1 y0)
	(curveto score (- x1 dx) (- y1 thickness) (+ x0 dx) (- y1 thickness) x0 y0)
	(fill-in score)))))

;;; (cmn (size 200) (footer-margin 3.0) (staff (staff-lines 0) 
;;;   (c4 w (begin-tie (tie-curvature .3) (dashed '(.15 .05)) (gray-scale .4)) (marcato (dy .125) (gray-scale .5) (dx .625))) (c4 w end-tie)))
;;; (cmn (staff treble c4 q (begin-tie (dashed '(.2 .1))) d4 q (end-tie)) (staff bass c4 s c4 s c4 s c4 s c4 q))

(defmethod house ((tie tie) score)
  (declare (ignore score))
  ;; depends on note placement when housing is well in the past
  nil)

(defun prepare-note-tie (score left-tie)
  (declare (ignore score))
  (when (eq (tie-type left-tie) :left)
    (if (not (listp (tie-note left-tie))) 
	(cmn-warn "begin-tie without matching end-tie")
      (let* ((staff (tie-staff left-tie))
	     (stf-scl (if (and staff (staff-size staff)) (/ 1.0 (staff-size staff)) 1.0))
	     (note1 (first (tie-note left-tie)))
	     (note2 (second (tie-note left-tie))))
	(if (or (not (audible-p note1)) (not (audible-p note2))) (cmn-error "tie trouble: ~A" (descry left-tie)))
	(if (not staff) (cmn-warn "unresolved tie: ~A" (descry left-tie)))
	(if (note-p note1)
	    (let* ((stem1 (if (stem-is-up? note1) :up :down))
		   (stem2 (if (stem-is-up? note2) :up :down)))
	      (if (not (cmn-tie-direction left-tie)) 
		  (setf (cmn-tie-direction left-tie) 
		    (if (eq stem1 stem2)
			(if (eq stem1 :up) :down :up)
		      (if (> (note-line note1) 4) :up :down))))
	      (if (/= (staff-y0 note1) (staff-y0 note2))
		  ;; here we have a tie across lines, so we need to fake up two left ties going off into space
		  (let ((y-offset (+ (* (note-line note1) *staff-line-separation*)
				     (if (eq (cmn-tie-direction left-tie) :up) .2 -.2)))
			(right-tie (make-instance 'tie)))
		    (setf (tie-type right-tie) :prepared)
		    (setf (cmn-tie-direction right-tie) (cmn-tie-direction left-tie))
		    (setf (box-x0 left-tie) (+ (box-x0 note1) (vis-dx note1) (center note1) (breathing-space left-tie) .1))
		    (setf (box-x0 right-tie) (min (- (box-x0 note2) 0.5) (+ (* stf-scl (box-x0 staff)) .75)))
		    (setf (box-y0 left-tie) (+ (staff-y0 note1) y-offset))
		    (setf (box-y0 right-tie) (+ (staff-y0 note2) y-offset))
		    (setf (box-x1 left-tie) (+ (* stf-scl (box-x1 staff)) .2))
		    (setf (box-x1 right-tie) (+ (box-x0 note2) (vis-dx note2) (- (center note2) (breathing-space left-tie) .1
										 (if (and (eq stem2 :down) 
											  (eq (cmn-tie-direction right-tie) :down)) 
										     .1 0))))
		    ;; might be a page break in between note1 and note2, so we have to put off the second tie's display
		    (push right-tie (ties note2)))
		(progn
		  (setf (box-x0 left-tie) (+ (box-x0 note1) 
					     (vis-dx note1) 
					     (center note1) 
					     (breathing-space left-tie)
					     .1
					     (if (or (and (eq (cmn-tie-direction left-tie) :down)
							  (stem-is-up? note1))
						     (and (eq (cmn-tie-direction left-tie) :up)
							  (stem-is-down? note1)))
						 -.0625 0)
					     (if (and (eq (cmn-tie-direction left-tie) :up)
						      (stem-is-up? note1)) 
						 .1 0)))
		  (setf (box-x1 left-tie) (+ (box-x0 note2) 
					     (vis-dx note2) 
					     (- (center note2) 
						(breathing-space left-tie) .1 
						(if (and (not (eq stem1 stem2))
							 (or (and (eq (cmn-tie-direction left-tie) :up)
								  (stem-is-up? note2))
							     (and (eq (cmn-tie-direction left-tie) :down)
								  (stem-is-down? note2))))
						    .1 0))))
		  (setf (box-y0 left-tie) (+ (staff-y0 note1)
					     (* (note-line note1) *staff-line-separation*)
					     (if (eq (cmn-tie-direction left-tie) :up) .2 -.2))))))
	  ;; chord ties
	  (progn
	    ;; once again, we have to check for line/page breaks within the tie
	    (if (= (staff-y0 note1) (staff-y0 note2))
		(progn
		  (setf (box-x0 left-tie) (+ (box-x0 note1) (vis-dx note1)))
		  (setf (box-x1 left-tie) (+ (box-x0 note2) (vis-dx note2))))
	      (let ((right-tie (make-instance 'tie)))
		(setf (tie-type right-tie) :prepared)
		(setf (box-x0 left-tie) (box-x0 note1))
		(setf (box-x0 right-tie) (min (- (box-x0 note2) 0.5) (+ (* stf-scl (box-x0 staff)) .75)))
		(setf (box-x1 left-tie) (+ (* stf-scl (box-x1 staff)) .2))
		(setf (box-x1 right-tie) (+ (box-x0 note2) (vis-dx note2)))
		(push right-tie (ties note2))))))))))


(defvar unresolved-ties nil)

(defun tieify-staff (score staff)
  (setf unresolved-ties nil)
  (loop for object in (staff-data staff) do
    (if (or (note-p object)
	    (rest-p object)
	    (chord-p object))
	(resolve-ties score staff object))
    (if (chord-p object)
	(loop for note in (chord-data object) do
	  (setf (box-x0 note) (box-x0 object))
	  (setf (box-x1 note) (box-x1 object))
	  (setf (center note) .15)
	  (resolve-ties score staff note)))))

(defun resolve-ties (score staff object)
  (declare (ignore score))
  (let* ((right-ties (and (ties object) 
			  (remove-if #'(lambda (tie) 
					 (eq (tie-type tie) :left))
				     (ties object))))
	 (left-ties (and (ties object)
			 (remove-if #'(lambda (tie)
					(eq (tie-type tie) :right))
				    (ties object)))))
    (if right-ties			;we have :right ties here
	(loop for right-tie in right-ties do
	  (let* ((name (tie-name right-tie))
		 (left-tie (find name unresolved-ties :key #'tie-name)))
	    (if (not left-tie)
		(cmn-error "can't find corresponding tie for ~A (tie ~A)~A" 
		       right-tie (tie-name right-tie)
		       (if (tie-note right-tie)
			   (if (odb-onset (tie-note right-tie))
			       (format nil " at ~1,3F in ~A" (odb-onset (tie-note right-tie)) (brief-identify (tie-note right-tie)))
			     (format nil " in [note in chord?] ~A" (brief-identify (tie-note right-tie))))))
	      (let ((moved nil))
		(setf unresolved-ties (remove left-tie unresolved-ties))
		(setf (tie-staff left-tie) staff)
		(if (and (tie-move-right left-tie)
			 (ties (tie-note left-tie)))
		    (let ((move-it nil))
		      (loop for nt in (ties (tie-note left-tie)) do
			(if (and (not (eq nt left-tie))
				 (tie-added nt))
			    (setf move-it nt)))
		      ;; auto-added tie but user has also specified begin-tie, so we need to move
		      ;;   the user tie to the end of the current (auto-tied) sequence
		      ;; (cmn treble (c4 (rq 5/2) begin-tie) (c4 q end-tie))
		      ;; (cmn treble (meter 2 4) (c4 (rq 8) begin-tie) (c4 q end-tie))
		      ;;
		      ;; but chords don't work yet:
		      ;; (cmn staff (chord (notes as5 bs5) (rq 5/6) begin-tie) (bar) (chord (notes as5 bs5) q end-tie))
		      ;;   the tie added and move-right fields appear to be set correctly, so our search above must miss them
		      (if move-it
			  (let ((new-move-it nil))
			    (setf moved t)
			    (let ((cur (cadr (tie-note move-it))))
			      (loop while cur do
				(setf new-move-it nil)
				(if (ties cur)
				    (loop for nt in (ties cur) do
				      (if (tie-added nt)
					  (setf new-move-it nt))))
				(if new-move-it
				    (progn
				      (setf move-it new-move-it)
				      (setf cur (cadr (tie-note move-it))))
				  (setf cur nil))))
			    (setf (tie-note left-tie) (list (cadr (tie-note move-it)) (tie-note right-tie)))
			    ))))
		(if (not moved) (setf (tie-note left-tie) (list (tie-note left-tie) (tie-note right-tie)))))))))
    (if left-ties
	(setf unresolved-ties (append left-ties unresolved-ties)))))


(defun display-tied-chord (score chord0 tie accidentals lh-x0 rh-x0 stem-direction lines min-line max-line)
  ;; accidentals is a list of accidental offsets, note-head displacements, line numbers, and signs
  (when (and tie
	     (member (tie-type tie) '(:left :prepared)))
    (prepare-note-tie score tie)
    (let* ((chord-mid-line (if (not (tie-direction tie))
			       (* .5 (+ max-line min-line))
			     (if (eq (tie-direction tie) :up)
				 (1- min-line)
			       (1+ max-line))))
	   ;; x1 = tie's notion of chord1's x0
	   (x1 (box-x1 tie))
	   (other-stem-direction (and (second (tie-note tie)) (stem-direction (second (tie-note tie)))))
	   (dots (dots chord0))
	   (outlying-second 0)
	   (outlying-second-line 0)
	   ;;(stems-different (or (and (member other-stem-direction '(:up :up?))
	   ;;		               (member stem-direction '(:down :down?)))
	   ;;		          (and (member other-stem-direction '(:down :down?))
	   ;;		               (member stem-direction '(:up :up?)))))
	   (both-sides-in-use (and (find :left accidentals :key #'second)
				   (find :right accidentals :key #'second))))
      
      (when (eq (tie-type tie) :prepared)
	(setf x1 (if (or both-sides-in-use (not (member stem-direction '(:down :down?)))) lh-x0 rh-x0))
	(setf other-stem-direction stem-direction))
      
      (loop for line in lines and next-line in (cdr lines) do 
	(when (and (or (= line next-line) 
		       (= (abs (- line next-line)) 1))
		   (> (abs (- line chord-mid-line)) (abs outlying-second)))
	  (setf outlying-second-line (* .5 (+ line next-line)))
	  (setf outlying-second (- line chord-mid-line))))
      (if (and (not (tie-direction tie))
	       (not (zerop outlying-second)))
	  (setf chord-mid-line outlying-second-line))

      (loop for note in accidentals and
       i from 0 and
       line in lines do
	(let ((tie-dir (if (> line chord-mid-line) :up :down))
	      (head-note1 (or (and (eq line min-line)
				   (member stem-direction '(:up :up?)))
			      (and (eq line max-line)
				   (member stem-direction '(:down :down?)))))
	      (head-note2 (or (and (eq line min-line)
				   (member other-stem-direction '(:up :up?)))
			      (and (eq line max-line)
				   (member other-stem-direction '(:down :down?))))))
	  (display-tie (make-instance 'tie :type :left :direction tie-dir
				 :tie-user-data (tie-user-data tie)
				 :x0 (if (eq (tie-type tie) :prepared)
					 (box-x0 tie)
				       (+ (if (and (eq (second note) :left)
						   (not (or (and (eq tie-dir :up)
								 (find (1+ line) lines))
							    (and (eq tie-dir :down)
								 (find (1- line) lines)))))
					      rh-x0 
					    (+ rh-x0 .25))
					  (if head-note1
					      (if (member stem-direction '(:up :up?))
						  -.0625
						0)
					    (+ .04 ;default breathing-space
					       (if (and dots (plusp dots)) (* dots .2) 0)
					       .05))))
				 :y0 (+ (staff-y0 chord0)
					(if head-note1 
					    (if (member stem-direction '(:up :up?)) 
						-.1 
					      .1) 
					  (if head-note2
					      (if (member other-stem-direction '(:up :up?))
						  -.1
						.1)
					    0))
					(* *staff-line-separation* line) 
					(if (> line chord-mid-line) .1 -.1))
				 :x1 (+ x1 (if (and both-sides-in-use
						    (or (and (eq (second note) :right) 
							     ;(not stems-different)
							     (not (or (and (eq tie-dir :up)
									   (find (1+ line) lines))
								      (and (eq tie-dir :down)
									   (find (1- line) lines)))))
							(and (eq (second note) :left)
							     ;stems-different
							     (member stem-direction '(:down down?)))))
					       .25
					     0)
					(if head-note2
					    (if (member other-stem-direction '(:up :up?))
						.0625
					      .125)
					  (- (+ .04 .05))))) ;.04=default breathing-space
		       score))))))

;;; (cmn staff treble (chord (notes c4 d4 e4) (begin-tie (tie-direction :down)) q) (chord (notes c4 d4 e4) end-tie q))

;;; tieify is like beamify -- adds in or fills out tie descriptions, but needs a label to unravel multiple ties
;;; (i.e. on the same staff can be several interleaved simultaneous ties, even multiple ties to unisons, so
;;; we need a way to unravel which notes belongs to which tie).


(defun map-over-staves (fun score)
  (loop for system in (systems score) do
    (setf *cmn-system* system)
    (loop for staff in (staves system) do
      (setf *cmn-staff* staff)
      (funcall fun score staff)))
  score)

(defun map-over-score (score fun)
  (loop for system in (systems score) do
    (loop for staff in (staves system) do
      (loop for object in (staff-data staff) do
        (funcall fun score system staff object)))))

;;; (map-over-score *cmn-score* #'(lambda (score sys stf obj) (if (key-p obj) (print (descry obj)))))
  
(defun tieify (score)
  (map-over-staves #'tieify-staff score)
  score)

#|
(cmn staff treble 
     (chord (notes c5 a4) q stem-down (begin-tie)) (chord (notes c5 a4) q stem-up (end-tie)) 
     (chord (notes c5 a4) q stem-up (begin-tie)) (chord (notes c5 a4) q stem-down (end-tie)) 
     (c4 q stem-down (begin-tie)) (c4 q stem-up (end-tie)) 
     (c4 q stem-up (begin-tie)) (c4 q stem-down (end-tie)) 
     (chord (notes cs5 a4) q stem-down (begin-tie)) (chord (notes c5 a4) q stem-up (end-tie)) 
     (chord (notes c5 af4) q stem-up (begin-tie)) (chord (notes c5 a4) q stem-down (end-tie)) 
     (cs4 q stem-down (begin-tie)) (c4 q stem-up (end-tie)) 
     (cf4 q stem-up (begin-tie)) (c4 q stem-down (end-tie)) 
     (chord (notes c5 g4 a4) q stem-down (begin-tie)) (chord (notes c5 g4 a4) q stem-up (end-tie)) 
     (chord (notes c5 b4 a4) q stem-up (begin-tie)) (chord (notes c5 a4 b4) q stem-down (end-tie))
     line-mark
     (chord (notes c5 a4 e4) q stem-down (begin-tie)) (chord (notes c5 a4 e4) q stem-down (end-tie)) 
     (chord (notes c5 a4 d4) q stem-up (begin-tie)) (chord (notes c5 a4 d4) q stem-up (end-tie)) 
     (chord (notes cs5 a4) q stem-down (begin-tie)) (chord (notes c5 a4) q stem-down (end-tie)) 
     (chord (notes c5 af4) q stem-up (begin-tie)) (chord (notes c5 a4) q stem-up (end-tie)) 
     (chord (notes c5 g4 a4) q stem-down (begin-tie)) (chord (notes c5 g4 a4) q stem-down (end-tie)) 
     (chord (notes c5 b4 a4) q stem-up (begin-tie)) (chord (notes c5 a4 b4) q stem-up (end-tie)))

(cmn staff treble (meter 2 4) 
     (engorge (loop for i from 0 to 10 
	       collect (chord (notes c4 g4) e.)
	       collect (chord (notes c5 g5) e.)
	       collect (chord (notes bf4 c5) e.)
	       collect (chord (notes c4 d4) e.)
	       collect (chord (notes cs4 ds4 gs4) e.)
	       collect (chord (notes ef6 df6 bf5) e.))))

|#




;;;
;;; ----------------    slurs
;;;
;;; the passage from the user's slur request to the actual display of a slur is relatively circuitous.
;;; First, end-slur has to be equivalent to (end-slur) so the first level just returns a self-acting
;;;  object that the owning note, or whoever, resolves (ties the begin and end together).
;;;  The result of the resolution is a sundry object on the marks list whose type is :set-up-slur.
;;;  Upon (true) display, the sundry-mark is executed, and it looks for line/page breaks, splitting
;;;  the slur if necessary.  The result of that execution is the (final) sundry of type :slur, also
;;;  on the marks list, but display-marks will not deal with a :slur mark unless forced -- this allows
;;;  us to wait until the note has actually been displayed before trying to calculate the slur's shape.
;;;  Finally, the :slur object is attached to the last note in the slur note list so that every note
;;;  under the slur is actually drawn first -- the reason for this subterfuge is that we need to know
;;;  everything about the note in true (final) coordinates before making the slur.

(defclass slur (tag-mixin score-object)
  ((name :accessor slur-name)
   (type :accessor slur-type)
   (note :accessor slur-note)
   (data :initarg :data :initform nil :accessor slur-data)
   (user-data :initarg :user-data :initform nil :accessor slur-user-data)
   (direction :initarg :direction :initform nil :accessor slur-direction :accessor cmn-slur-direction)
   (thickness :initarg :thickness :initform nil :accessor slur-thickness)
   (curvature :initarg :curvature :initform nil :accessor slur-curvature)
   (dashed :initarg :dashed :initform nil :accessor dashed)))

(defvar slur-counter 0)

(deferred-action slur-direction)
(deferred-action slur-thickness)
(deferred-action slur-curvature)

(defmethod (setf slur-thickness) (n (scr score)) (setf *slur-thickness* n) nil)
(defmethod (setf slur-curvature) (n (scr score)) (setf *slur-curvature* n) nil)
(defmethod cmn-thickness ((obj slur)) (slur-thickness obj))

(defmethod slur-p ((obj t)) nil)
(defmethod slur-p ((obj slur)) t)

(defmethod descry ((slur slur) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) "(slur" "")
	  (if (slur-data slur) (format nil " :data '(~:{(~1,3F ~1,3F ~1,3F ~1,3F ~1,3F ~1,3F) ~}))" (slur-data slur)) "")
	  (if (slur-user-data slur) (format nil " :user-data '(~{~1,3F ~})" (slur-user-data slur)) "")
	  (if (slur-direction slur) (format nil "~%~A      :direction :~(~A~)" prewhitespace (slur-direction slur)) "")
	  (if (slur-thickness slur) (format nil " :thickness ~1,3F" (slur-thickness slur)) "")
	  (if (slur-curvature slur) (format nil " :curvature ~1,3F" (slur-curvature slur)) "")
	  (if (dashed slur) (format nil " :dashed '~A" (dashed slur)) "")
	  (if (slur-note slur) (format nil "~A :note ~A" 
				       (if (and (> (length (first (slur-data slur))) 2) (not (slur-direction slur)))
					   (format nil "~%~A~A" prewhitespace prewhitespace)
					 "")
				       (brief-identify (slur-note slur))))
	  (if (next-method-p) (call-next-method slur stream (or controller slur)) "")
	  (if (not controller) ")" "")))

(defun identify-slur (slur)
  (format nil "~A~A~A~A~{~A~}~A"
	  (if (slur-thickness slur) (format nil " (slur-thickness ~1,3F)" (slur-thickness slur)) "")
	  (if (slur-curvature slur) (format nil " (slur-curvature ~1,3F)" (slur-curvature slur)) "")
	  (if (slur-direction slur) (format nil " (slur-direction :~(~A~))" (slur-direction slur)) "")
	  (if (dashed slur) (format nil " (dashed '~A)" (dashed slur)) "")
	  (if (slur-user-data slur)
	      (loop for ds in (slur-user-data slur) and i from 0
	       if (not (zerop ds))
	       collect (format nil " (~A~A ~1,3F)" (if (evenp i) "dx" "dy") (floor i 2) ds))
	    '(""))
	  (identify-visible slur)))

(defmethod dsud ((slur slur) num-i)
  (if (not (slur-user-data slur)) 
      (setf (slur-user-data slur) (list 0 0 0 0 0 0 0 0)))
  (setf (nth (second num-i) (slur-user-data slur)) (first num-i)))

(defun dx0 (num) (make-self-acting :action #'dsud :argument (list num 0)))
(defun dy0 (num) (make-self-acting :action #'dsud :argument (list num 1)))
(defun dx1 (num) (make-self-acting :action #'dsud :argument (list num 2)))
(defun dy1 (num) (make-self-acting :action #'dsud :argument (list num 3)))
(defun dx2 (num) (make-self-acting :action #'dsud :argument (list num 4)))
(defun dy2 (num) (make-self-acting :action #'dsud :argument (list num 5)))
(defun dx3 (num) (make-self-acting :action #'dsud :argument (list num 6)))
(defun dy3 (num) (make-self-acting :action #'dsud :argument (list num 7)))

(defun user-add-to-slurs (note object) 
  (when object
    (setf (slur-note object) note) 
    (push object (slurs note))))

(defun user-add-to-left-slurs (note possible-slur)
  (push (if possible-slur (list :begin-slur possible-slur) :begin-slur) (store-data note))
  (user-add-to-slurs note (push-slur (or possible-slur (new-slur)))))

(defun user-add-to-right-slurs (note possible-slur)
  (push (if possible-slur (list :end-slur possible-slur) :end-slur) (store-data note))
  (if possible-slur
      (let ((actual-slur (argument possible-slur)))
	(user-add-to-slurs note (slur-slur actual-slur))
	(setf slur-stack (remove actual-slur slur-stack)))
    (user-add-to-slurs note (slur-slur (pop-slur)))))

(defun new-slur (&rest objects)
  (let ((ns (make-instance 'slur :name (incf slur-counter) :type :left)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) ns (argument act))))
    ns))

(defun slur-slur (&optional slur) 
  (let ((t-slur (or slur (pop-slur) (cmn-warn "no slur to end"))))
    (if t-slur (make-instance 'slur :name (slur-name t-slur) :type :right))))

(defun begin-slur (&rest objects)
  (make-self-acting :action #'user-add-to-left-slurs :argument (apply #'new-slur objects)))

(defun end-slur (&optional slur)	;not actually a slur instance
  (make-self-acting :action #'user-add-to-right-slurs :argument slur))

(defvar begin-slur (make-self-acting :action #'user-add-to-left-slurs :argument nil))
(defvar end-slur (make-self-acting :action #'user-add-to-right-slurs :argument nil))

(defun push-slur (slur) (push slur slur-stack) slur)
(defun pop-slur () (pop slur-stack))

(defun find-slur (slur objects)
  (find-if #'(lambda (object)
	       (or (and (note-p object)
			(slurs object)
			(find (slur-name slur) (slurs object) :key #'slur-name)
			object)
		   (and (chord-p object)
			(find-slur slur (chord-data object)))))
	   objects))

(defmethod copy ((slur slur) &optional object)
  (let ((new-slur (if (not object) (make-instance 'slur)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (cmn-slur-direction new-slur) (cmn-slur-direction slur))
    (setf (slur-thickness new-slur) (slur-thickness slur))
    (setf (slur-curvature new-slur) (slur-curvature slur))
    (setf (slur-data new-slur) (copy-tree (slur-data slur)))
    (setf (dashed new-slur) (dashed slur))
    (setf (slur-user-data new-slur) (copy-tree (slur-user-data slur)))
    (if (next-method-p) (call-next-method slur new-slur))
    new-slur))

(defun copy-slur (slur note)
  (let ((new-slur (copy slur)))
    (setf (slur-note new-slur) note)
    new-slur))

(defun copy-slurs (note new-chord)
  (setf (slurs new-chord) (loop for slur in (slurs note) collect (copy slur)))
  (loop for old-slur in (slurs note) and new-slur in (slurs new-chord) do
    (if (eq note (slur-note old-slur)) (setf (slur-note new-slur) new-chord))))

(defmethod display ((slur slur) container score &optional justifying)
  (declare (ignore justifying))
  (if (and container (not (cmn-slur-direction slur))) (setf (cmn-slur-direction slur) (slur-direction container)))
  (when (slur-data slur)
    (let* ((sud (slur-user-data slur))
	   (dxs (vis-dx slur))
	   (dys (vis-dy slur))
	   (x0 (+ (box-x0 slur) dxs (if sud (first sud) 0)))
	   (y0 (+ (box-y0 slur) dys (if sud (second sud) 0)))
	   (cr0 (if sud 
		    (loop for s0 in (first (slur-data slur)) and s1 in (cddr sud) collect (+ s0 s1))
		  (copy-list (first (slur-data slur)))))
	   (cr1 (if sud 
		    (let ((rsud (list (nth 4 sud) (nth 5 sud)
				      (nth 2 sud) (nth 3 sud)
				      (nth 0 sud) (nth 1 sud))))
		      (loop for s0 in (second (slur-data slur)) and s1 in rsud collect (+ s0 s1)))
		  (copy-list (second (slur-data slur))))))
      (if dxs
	  (loop for xu in cr0 by #'cddr and xp in cr1 by #'cddr and i from 0 by 2 do
	    (setf (nth i cr0) (+ xu dxs))
	    (setf (nth i cr1) (+ xp dxs))))
      (if dys
	  (loop for yu in (cdr cr0) by #'cddr and yp in (cdr cr1) by #'cddr and i from 1 by 2 do
	    (setf (nth i cr0) (+ yu dys))
	    (setf (nth i cr1) (+ yp dys))))
      (if (not (dashed slur))
	  (with-color score slur
            (moveto score x0 y0)
	    (apply #'curveto score cr0)
	    (apply #'curveto score cr1)
	    (fill-in score))
	(let ((dash-width (second (dashed slur)))
	      (dash-step (first (dashed slur)))
	      (x3 (fifth cr0))
	      (y1 (second cr0))
	      (y2 (fourth cr0))
	      (y3 (sixth cr0)))
	  (with-thickness score slur dash-width
	    (with-color score slur
	      (loop for x from (+ x0 dash-step) to (- x3 dash-step) by dash-step do
		(moveto score x (+ .01 (solve-bezier x x0 y0 y1 y2 x3 y3)))
		(rlineto score 0 (* -2 *slur-thickness*)))
	      ;; this is assuming short dashes! -- a horizontal line is kinda minimal
	      (draw score))))))))

;;; (cmn treble (c4 q (begin-slur (dashed (list .4 .1)))) d4 q f4 q (e4 q end-slur))


(defmethod house ((slur slur) score)
  (declare (ignore score))
  ;; depends on note placement when housing is well in the past
  nil)


;;; untangle slurs for simultaneous voices on one staff with simultaneous slurs

;;; slur- and friends can get confused by intervening notes not included in the -slur- sequence
(defun slur- (&rest args)
  (let ((new-slur (make-instance 'slur :type :special-slur)))
    (loop for act in args do
      (if (self-acting-p act)
	  (funcall (action act) new-slur (argument act))))
    (make-self-acting 
     :action #'(lambda (note slur)
		 (push (list :slur- new-slur) (store-data note))
		 (push note (tag-note slur))
		 nil)
     :argument new-slur)))

(defvar slur- (make-self-acting
	       :action #'(lambda (note &rest rest)
			   (declare (ignore rest))
			   (let ((new-slur (make-instance 'slur :type :special-slur)))
			     (push (list :slur- new-slur) (store-data note))
			     (push note (tag-note new-slur))
			     (setf (argument slur-) new-slur)
			     nil))
	       :argument nil))

(defun -slur- (slur)
  (make-self-acting
   :action #'(lambda (note old-slur)
	       (let ((true-slur (argument old-slur)))
		 (push note (tag-note true-slur))
		 nil))
   :argument slur))

(defun -slur (slur)
  (make-self-acting
   :action #'(lambda (note old-slur)
	       (let ((true-slur (argument old-slur)))
		 (push note (tag-note true-slur))
		 (let ((slur-notes (reverse (tag-note true-slur))))
		   (add-to-marks note (list (make-instance 'sundry 
					     :name :set-up-slur 
					     :mark #'(lambda (mark note score &optional justifying)
						       (declare (ignore mark note))
						       (when (not justifying)
							 (make-and-get-ready-to-display-slur true-slur score slur-notes))))))
		   nil)))
   :argument slur))


(defun stem-heading (object)
  (if (audible-p object)
      (or (and (eq (beamed object) t)
	       (beams object)
	       (beam-direction (beams object)))
	  (and (stem-is-up? object) :up) 
	  :down)))

(defun inform-slur (slur x0 y0 x1 y1 x2 y2 x3 y3 score-slur-thickness)
  (let ((thickness (or (slur-thickness slur) score-slur-thickness)))
    (setf (box-x0 slur) x0)
    (setf (box-y0 slur) y0)
    (setf (slur-data slur) (list (list x1 y1 x2 y2 x3 y3)
				 (list x2 (- y2 thickness)
				       x1 (- y1 thickness)
				       x0 y0)))))

(defun lowest-notes-sign (chord)
  (let ((n (first (chord-data chord))))
    (loop for n0 in (cdr (chord-data chord)) do
      (if (< (note-line n0) (note-line n))
	  (setf n n0)))
    (note-sign n)))

(defun highest-notes-sign (chord)
  (let ((n (first (chord-data chord))))
    (loop for n0 in (cdr (chord-data chord)) do
      (if (> (note-line n0) (note-line n))
	  (setf n n0)))
    (note-sign n)))

;;; more cases -- if last note or first note and stem is odd, and we can avoid the stem, head for note if possible.

(defun under-slur-y (note-or-chord score ctr &optional (special-whole-note-case nil))
  (declare (ignore score))
  ;; if stem-up, look for staccato and tenuto
  ;; if stem-down, look for stem-end (beam)
  ;; base point of reference is minimum-line and line-separation -- can't use y0 or y1 here, but staff-y0 should be ok
  (+ -.125
     (if (or (stem-is-up? note-or-chord) special-whole-note-case)
	 (+ (staff-y0 note-or-chord)
	    (* (min 10 (minimum-line note-or-chord)) *staff-line-separation*)
	    ;; the min 10 business is to keep slurs from crossing leger lines -- is this correct?
	    (if (and (plusp ctr)
		     (or (and (note-p note-or-chord)
			      (member (note-sign note-or-chord) (list sharp natural)))
			 (and (chord-p note-or-chord)
			      (member (lowest-notes-sign note-or-chord) (list sharp natural)))))
		-.2
	      0)
	    (if (ties note-or-chord) -.1 0)
	    (if (marks note-or-chord)
		(* -.25 (count-if #'(lambda (n) 
				      (and (sundry-p n) 
					   (member (sundry-name n) '(:staccato :tenuto))))
				  (marks note-or-chord)))
	      0))
       (or (stem-end note-or-chord)
	   (+ (staff-y0 note-or-chord)
	      (if (< (quarters note-or-chord) 4)
		  (* (min 4 (- (minimum-line note-or-chord) 6)) *staff-line-separation*)
		(* (minimum-line note-or-chord) *staff-line-separation*)))))))

(defun over-slur-y (note-or-chord score ctr &optional (special-whole-note-case nil))
  (declare (ignore score))
  (+ .125
     (if (and (stem-is-up? note-or-chord) (not special-whole-note-case))
	 (or (stem-end note-or-chord)
	     (+ (staff-y0 note-or-chord)
		(if (< (quarters note-or-chord) 4)
		    (* (max 4 (+ (maximum-line note-or-chord) 6)) *staff-line-separation*)
		  (* (minimum-line note-or-chord) *staff-line-separation*))))
       (+ (staff-y0 note-or-chord)
	  (* (maximum-line note-or-chord) *staff-line-separation*)
	  (if (plusp ctr)
	      (if (or (and (note-p note-or-chord)
			   (eq (note-sign note-or-chord) flat))
		      (and (chord-p note-or-chord)
			   (eq (highest-notes-sign note-or-chord) flat)))
		  .3
		(if (or (and (note-p note-or-chord)
			     (member (note-sign note-or-chord) (list sharp natural)))
			(and (chord-p note-or-chord)
			     (member (highest-notes-sign note-or-chord) (list sharp natural))))
		    .1
		  0))
	    0)
	  (if (ties note-or-chord) .1 0)
	  (if (marks note-or-chord)
	      (* .25 (count-if #'(lambda (n) 
				   (and (sundry-p n) 
					(member (sundry-name n) '(:staccato :tenuto))))
			       (marks note-or-chord)))
	    0)))))


;;; there are 3 basic cases --
;;;    notes form a line (two-note-slur -- use end points)
;;;    notes in a triangle (three-note-slur -- use extreme notes)
;;;    notes coverable with a rhombus (four-note-slur -- use the four extremes)

(defun make-two-note-slur (score slur note1 note2 len &optional (below :not-given))
  (let* ((stem1 (stem-heading note1))
	 (stem2 (stem-heading note2))
	 (whole-note-case (or (whole-note-p note1) (whole-note-p note2)))
	 (special-whole-note-case (and whole-note-case (= len 2)))
	 (slur-type (or (cmn-slur-direction slur)
			(if (eq below :not-given)
			    (if (stem-eq stem1 stem2)
				(if (stem-eq stem1 :up)
				    :down
				  :up)
			      :up)
			  (if below 
			      :down
			    :up))))
	 (x0 (+ (box-x0 note1) (vis-dx note1) (center note1) .05))
	 (x3 (+ (box-x0 note2) (vis-dx note2) (center note2) -.05))
	 (y0 (if (eq slur-type :down)
		 (- (under-slur-y note1 score 0 special-whole-note-case) .1)
	       (+ (over-slur-y note1 score 0 special-whole-note-case) .1)))
	 (y3 (if (eq slur-type :down)
		 (- (under-slur-y note2 score 1 special-whole-note-case) .1)
	       (+ (over-slur-y note2 score 1 special-whole-note-case) .1))))
    
    (if special-whole-note-case
	(if (and (eq slur-type :up)
		 (not (whole-note-p note1))
		 (stem-eq stem1 :up))
	    (incf x0 .25)
	  (if (and (eq slur-type :down)
		   (not (whole-note-p note2))
		   (stem-eq stem2 :down))
	      (decf x3 .25))))
    
    (if (and (not (stem-eq stem1 stem2))
	     (not whole-note-case))
	(let ((yd (if (= len 2) .3 .2)))
	  (if (stem-eq stem1 :up)
	      (if (eq slur-type :up)
		  (progn
		    (if (and (= len 2) (<= (maximum-line note1) (minimum-line note2)))
			(setf y0 y3)
		      (decf y0 yd))
		    (incf x0 .2))
		(progn
		  (incf y3 yd)
		  (decf x3 .2)))
	    (if (eq slur-type :up)
		(progn
		  (if (and (= len 2) (<= (maximum-line note2) (minimum-line note1)))
		      (setf y3 y0)
		    (decf y3 yd))
		  (decf x3 .1))
	      (progn
		(incf y0 yd)
		(incf x0 .1))))))
    
    (let ((slur-slope (/ (- y3 y0) (max (- x3 x0) .01)))
	  (max-slope *maximum-slur-slope*))
      (if (> slur-slope max-slope)
	  (setf y3 (+ y0 (* (- x3 x0) max-slope)))
	(if (< slur-slope (- max-slope))
	    (setf y3 (- y0 (* (- x3 x0) max-slope))))))
    
    (let* ((diff (abs (- y3 y0)))
	   (x-diff (+ .3 (* -.2 (min 1.0 (max 0.0 (- diff .25))))))
	   (x1 (+ x0 (* x-diff (- x3 x0))))
	   (x2 (+ x0 (* (- 1.0 x-diff) (- x3 x0))))
	   (vy1 (+ y0 (* x-diff (- y3 y0))))
	   (vy2 (+ y0 (* (- 1.0 x-diff) (- y3 y0))))
	   (y-diff (* (if (eq slur-type :down) -1.0 1.0) 
		      (+ (or (slur-curvature slur) *slur-curvature*)
			 (* .2 (min 1.0 (max 0.0 (- diff .25)))))))
	   (y1 (+ vy1 y-diff))
	   (y2 (+ vy2 y-diff)))
      
      (inform-slur slur x0 y0 x1 y1 x2 y2 x3 y3 *slur-thickness*))))

#|
;;; some two note slur tests
(cmn (size 24) (free-expansion-factor 2) staff treble 
     (c4 q begin-slur) (e4 q end-slur) (c4 q begin-slur) (c5 q end-slur) (c5 q begin-slur) 
     (a4 q end-slur) (c5 q begin-slur) (c4 q end-slur) (a4 q begin-slur) (c5 q end-slur)
     (e6 q begin-slur) (c5 q end-slur) (e3 q begin-slur) (a4 q end-slur))
|#


(defun make-three-note-slur (score slur notes slur-below len)
  (let* ((note1 (first notes))
	 (note2 (second notes))
	 (note3 (third notes))
	 (y11 (if slur-below (under-slur-y note1 score 0) (over-slur-y note1 score 0)))
	 (y12 (if slur-below (under-slur-y note2 score 1) (over-slur-y note2 score 1)))
	 (y13 (if slur-below (under-slur-y note3 score 2) (over-slur-y note3 score 2)))
	 (above (not slur-below))
	 (mult (if above 1.0 -1.0))
	 (x0 (+ (box-x0 note1) 
		(vis-dx note1) 
		(if (and above (stem-eq (stem-heading note1) :up)) 
		    .3
		  (center note1)) 
		.05))
	 (xc (+ (box-x0 note2) (vis-dx note2) (center note2)))
	 (x3 (+ (box-x0 note3) (vis-dx note3) (center note3) -.05)))
    
    (when (= len 3)
      (let* ((stem1 (stem-heading note1))
	     (stem2 (stem-heading note2))
	     (stem3 (stem-heading note3)))
	
	(if (and (not (stem-eq stem1 stem2))
		 (< (quarters note1) 4)
		 (< (quarters note2) 4)
		 (< (quarters note3) 4))
	    (if (stem-eq stem1 :up)
		(if above
		    (progn
		      (if (<= (maximum-line note1) (minimum-line note2))
			  (setf y11 y12)
			(decf y11 .3))
		      (incf x0 .2)))))
	
	(if (not (stem-eq stem2 stem3))
	    (if (stem-eq stem2 :down)
		(if above
		    (progn
		      (if (<= (maximum-line note3) (minimum-line note2))
			  (setf y13 y12)
			(decf y13 .3))
		      (decf x3 .1)))))))
    
    (let ((slur-slope-1 (divide (- y12 y11) (- xc x0)))
	  (slur-slope-2 (divide (- y13 y12) (- x3 xc)))
	  (max-slope *maximum-slur-slope*))
      (if (> slur-slope-1 max-slope)
	  (setf y11 (- y12 .1 (* (- xc x0) max-slope)))
	(if (< slur-slope-1 (- max-slope))
	    (setf y11 (+ y12 .1 (* (- xc x0) max-slope)))))
      (if (> slur-slope-2 max-slope)
	  (setf y13 (+ y12 -.1 (* (- x3 xc) max-slope)))
	(if (< slur-slope-2 (- max-slope))
	    (setf y13 (- y12 -.1 (* (- x3 xc) max-slope))))))
    
    (let* ((diff-1 (abs (- y12 y11)))
	   (diff-2 (abs (- y13 y12)))
	   (x1-diff (+ .3 (* -.2 (min 1.0 (max 0.0 (- diff-1 .25))))))
	   (x2-diff (+ .3 (* -.2 (min 1.0 (max 0.0 (- diff-2 .25))))))
	   (x1 (+ x0 (* x1-diff (- xc x0))))
	   (x2 (- x3 (* x2-diff (- x3 xc))))
	   (slur-curve (or (slur-curvature slur) *slur-curvature*))
	   (y0 (+ y11 (* mult (max .1 (min .5 (* .3 diff-1))))))
	   (y3 (+ y13 (* mult (max .1 (min .5 (* .3 diff-2))))))
	   (y1 (funcall (if above #'max #'min) (+ (* mult slur-curve) y0) (+ y12 (* mult (max .2 (min .5 (* (if (< len 4) .5 1.0) diff-1)))))))
	   (y2 (funcall (if above #'max #'min) (+ (* mult slur-curve) y3) (+ y12 (* mult (max .2 (min .5 (* (if (< len 4) .5 1.0) diff-2))))))))
      
      (inform-slur slur x0 y0 x1 y1 x2 y2 x3 y3 *slur-thickness*))))

#|
;;; some initial 3-note slur tests
(cmn (size 24) (free-expansion-factor 2) staff treble 
     (c4 q begin-slur) (d4 q) (e4 q end-slur) 
     (c4 q begin-slur) (c4 q) (c4 q end-slur) 
     (c4 q begin-slur) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (b4 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (c4 q end-slur) 
     (a4 q begin-slur) (b4 q) (c5 q end-slur)
     (c5 q begin-slur) (c5 q) (c5 q end-slur)
     (g4 q begin-slur) (c4 q) (e4 q end-slur) 
     (g4 q begin-slur) (e4 q) (c4 q end-slur) 
     (c5 q begin-slur) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (c4 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (c5 q end-slur) 
     (a4 q begin-slur) (b4 q) (c4 q end-slur)
     (e3 q begin-slur) (a4 q) (g3 q end-slur)
     (e6 q (begin-slur (slur-direction :down))) (b4 q) (g6 q end-slur)
     )
|#

(defun make-four-note-slur (score slur notes slur-below len)
  (let* ((note1 (first notes))
	 (note2 (second notes))
	 (note3 (third notes))
	 (note4 (fourth notes))
	 (y11 (if slur-below (under-slur-y note1 score 0) (over-slur-y note1 score 0)))
	 (y12 (if slur-below (under-slur-y note2 score 1) (over-slur-y note2 score 1)))
	 (y13 (if slur-below (under-slur-y note3 score 2) (over-slur-y note3 score 2)))
	 (y14 (if slur-below (under-slur-y note4 score 3) (over-slur-y note4 score 3)))
	 (above (not slur-below))
	 (mult (if above 1.0 -1.0))
	 (s1 (stem-heading note1))
	 (x0 (+ (box-x0 note1) 
		(vis-dx note1)
		(if (and above (stem-eq s1 :up)) 
		    .3
		  (center note1)) 
		.05))
	 (x1 (+ (box-x0 note2) (vis-dx note2) (center note2)))
	 (x2 (+ (box-x0 note3) (vis-dx note3) (center note3))) 
	 (x3 (+ (box-x0 note4) (vis-dx note4) (center note4) -.05)))
    (let* ((diff-1 (abs (- y12 y11)))
	   (diff-2 (abs (- y14 y13)))
	   (slur-curve (or (slur-curvature slur) (* 2 *slur-curvature*)))
	   (y0 (+ y11 (* mult (max .1 (min .5 (* .3 diff-1))))))
	   (y3 (+ y14 (* mult (max .1 (min .5 (* .3 diff-2))))))
	   (y1 (funcall (if above #'max #'min) 
			(+ (* mult slur-curve) y0) (+ y12 (* mult (max .2 (min .5 (* (if (< len 5) 1.0 1.5) diff-1)))))))
	   (y2 (funcall (if above #'max #'min) 
			(+ (* mult slur-curve) y3) (+ y13 (* mult (max .2 (min .5 (* (if (< len 5) 1.0 1.5) diff-2))))))))
      
      (inform-slur slur x0 y0 x1 y1 x2 y2 x3 y3 *slur-thickness*))))

#|
;;; some initial 4-note slur tests
(cmn (size 24) (free-expansion-factor 2) staff treble 
     (c4 q begin-slur) (d4 q) (f4 q) (e4 q end-slur) 
     (c4 q begin-slur) (c4 q) (c4 q) (c4 q end-slur) 
     (c4 q begin-slur) (a4 q) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (b4 q) (g4 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (e4 q) (c4 q end-slur) 
     (a4 q begin-slur) (b4 q) (b4 q) (c5 q end-slur)
     (c5 q begin-slur) (c5 q) (c5 q) (c5 q end-slur)
     (g4 q begin-slur) (c4 q) (c5 q) (e4 q end-slur) 
     (g4 q begin-slur) (e4 q) (d4 q) (c4 q end-slur) 
     (c5 q begin-slur) (a4 q) (a4 q) (c5 q end-slur) 
     (c5 q begin-slur) (c4 q) (c5 q) (a4 q end-slur) 
     (c5 q begin-slur) (g4 q) (c4 q) (c5 q end-slur) 
     (a4 q begin-slur) (b4 q) (b4 q) (c4 q end-slur)
     (e3 q begin-slur) (a4 q) (a4 q) (g3 q end-slur)
     (a4 q begin-slur) g4 q f4 q (a4 q end-slur)
     (a4 q begin-slur) f4 q g4 q (a4 q end-slur)
     (c5 q begin-slur) d5 q e5 q (a4 q end-slur)
     (c5 q begin-slur) a5 q g5 q f5 q e5 q d5 q (c5 q end-slur)
     (e6 q (begin-slur (slur-direction :down))) (b4 q) (b4 q) (g6 q end-slur)
     )
|#

(defstruct nxy note x y)

(defun nxy-slope (n0 n1)
  (divide (- (nxy-y n1) (nxy-y n0)) (- (nxy-x n1) (nxy-x n0))))

(defun boil-under (notes score)
  ;; find either the apex of the implied triangle, or the corners of the trapezoid, or the ends of the line
  (let* ((left-slope 0.0)
	 (right-slope 0.0)
	 (nxys (loop for note in notes and i from 0
		collect (make-nxy :note note :x (+ (box-x0 note) (vis-dx note)) :y (under-slur-y note score i))))
	 (firstn (first nxys))
	 (lastn (first (last nxys)))
	 (right-anchor nil)
	 (left-anchor nil)
	 (cur-slope nil))
    (setf nxys (butlast (cdr nxys)))
    (setf left-slope (nxy-slope firstn lastn))
    (setf right-slope left-slope)
    (loop for nx in nxys do
      (setf cur-slope (nxy-slope firstn nx))
      (if (< cur-slope left-slope)
	  (progn
	    (setf left-slope (- cur-slope .01))
	    (setf left-anchor nx)
	    (setf right-anchor nil)
	    (setf right-slope (+ (nxy-slope nx lastn) .01)))
	(progn
	  (setf cur-slope (nxy-slope nx lastn))
	  (if (> cur-slope right-slope)
	      (progn
		(setf right-slope (+ cur-slope .01))
		(setf right-anchor nx))))))
    (if (and right-anchor left-anchor)
	(list firstn left-anchor right-anchor lastn)
      (if left-anchor
	  (list firstn left-anchor lastn)
	(list firstn lastn)))))

(defun boil-over (notes score)
  ;; find either the apex of the implied triangle, or the corners of the trapezoid, or the ends of the line
  (let* ((left-slope 0.0)
	 (right-slope 0.0)
	 (nxys (loop for note in notes and i from 0
		collect (make-nxy :note note :x (+ (box-x0 note) (vis-dx note)) :y (over-slur-y note score i))))
	 (firstn (first nxys))
	 (lastn (first (last nxys)))
	 (right-anchor nil)
	 (left-anchor nil)
	 (cur-slope nil))
    (setf nxys (butlast (cdr nxys)))
    (setf left-slope (nxy-slope firstn lastn))
    (setf right-slope left-slope)
    (loop for nx in nxys do
      (setf cur-slope (nxy-slope firstn nx))
      (if (> cur-slope left-slope)
	  (progn
	    (setf left-slope (+ cur-slope .01))
	    (setf left-anchor nx)
	    (setf right-anchor nil)
	    (setf right-slope (- (nxy-slope nx lastn) .01)))
	(progn
	  (setf cur-slope (nxy-slope nx lastn))
	  (if (< cur-slope right-slope)
	      (progn
		(setf right-slope (- cur-slope .01))
		(setf right-anchor nx))))))
    (if (and right-anchor left-anchor)
	(list firstn left-anchor right-anchor lastn)
      (if left-anchor
	  (list firstn left-anchor lastn)
	(list firstn lastn)))))

(defun set-up-slur (score this-staff slur note1 note2 i0 ci0 i1 ci1)
  (declare (ignore note2))
  (if (not note1)
      (error "no attachment note for slur in ~A starting at note ~A" 
	     (identify-staff score this-staff) (identify (nth i0 (staff-data this-staff)))))
  (let* ((obj nil)
	 (loc-ci0 ci0)
	 (loc-ci1 ci1)
	 (slur-notes nil))
    (loop for i from i0 to i1 do
      (setf obj (nth i (staff-data this-staff)))
      (if (audible-p obj)
	  (if loc-ci0
	      (let ((loc-obj (nth loc-ci0 (chord-data obj))))
		(setf loc-ci0 loc-ci1)
		(push loc-obj slur-notes))
	    (push obj slur-notes))))
    ;; this won't work for a slur threading its way through more than two chords
    (add-to-marks note1 
		  (list (make-instance 'sundry 
			 :name :set-up-slur 
			 :mark #'(lambda (mark note score &optional justifying)
				   (declare (ignore mark note))
				   (when (not justifying)
				     (make-and-get-ready-to-display-slur slur score (nreverse slur-notes) this-staff))))))))

(defun slurify-object (i ci object score staff unresolved-slurs slur-indices)
  (let* ((right-slurs (and (slurs object) 
			   (remove-if #'(lambda (slur) 
					  (eq (slur-type slur) :left))
				      (slurs object))))
	 (left-slurs (and (slurs object)
			  (remove-if #'(lambda (slur)
					 (eq (slur-type slur) :right))
				     (slurs object)))))
    (if right-slurs			;we have :right slurs here
	(loop for right-slur in right-slurs do
	  (let* ((name (slur-name right-slur))
		 (left-slur (find name unresolved-slurs :key #'slur-name))
		 (left-index (and left-slur 
				  (find name slur-indices :key #'second))))
	    (if (not left-slur)
		(cmn-warn "can't find corresponding slur for ~A" right-slur)
	      (progn
		(setf unresolved-slurs (remove left-slur unresolved-slurs))
		(set-up-slur score staff left-slur 
			     (slur-note left-slur) (slur-note right-slur) 
			     (first left-index) (third left-index)
			     i ci))))))
    (if left-slurs
	(progn
	  (loop for slur in left-slurs do
	    (push (list i (slur-name slur) ci) slur-indices))
	  (setf unresolved-slurs (append left-slurs unresolved-slurs))))
    (values unresolved-slurs slur-indices)))

(defun slurify-staff (score staff)
  (let ((unresolved-slurs nil)
	(slur-indices nil))
    (loop for object in (staff-data staff) and i from 0 do
      (when (audible-p object)
	(multiple-value-setq 
	    (unresolved-slurs slur-indices)
	  (slurify-object i nil object score staff unresolved-slurs slur-indices))
	(if (chord-p object)
	    (loop for tnote in (chord-data object) and j from 0 do
	      (if (note-p tnote)
		  (multiple-value-setq 
		      (unresolved-slurs slur-indices)
		    (slurify-object i j tnote score staff unresolved-slurs slur-indices)))))))))


(defun slurify (score)
  (map-over-staves #'slurify-staff score)
  score)

(defun make-and-get-ready-to-display-slur (slur score notes &optional staff)
  ;; by this point, all values can be trusted (i.e. we have put off the call to this function
  ;; until all relevant fields have received their final (true) values).
  (let* ((last-note (first notes))
	 (cur-notes nil)
	 (cur-stf-y0 (staff-y0 last-note)))
    (push last-note cur-notes)
    (loop for this-note in (cdr notes) do
      (let ((new-stf-y0 (staff-y0 this-note)))
	(if (/= new-stf-y0 cur-stf-y0)
	    (let* ((any-staff (or staff *cmn-staff* (first (staves (first (systems score))))))
		   (staff-x1 (/ (box-x1 any-staff) (or (staff-size any-staff) 1.0))))
	      (let ((ln (short-note last-note)))
		(setf (box-x0 ln) (+ staff-x1 .2))
		(setf (stem-end ln) nil)
		(push ln cur-notes))
	      (let ((local-slur slur)	;should this copy the slur?
		    (local-notes (nreverse cur-notes)))
		(add-to-marks last-note (list (make-instance 'sundry 
					       :name :slur
					       :mark #'(lambda (mark note score &optional justifying)
							 (declare (ignore mark note))
							 (when (not justifying)
							   (prepare-slur score local-slur local-notes)))))))
	      (setf cur-stf-y0 new-stf-y0)
	      (setf cur-notes nil)
	      (setf slur (make-instance 'slur :type :left 
				    :direction (slur-direction slur)
				    :curvature (slur-curvature slur) 
				    :thickness (slur-thickness slur)
				    :user-data (copy-list (slur-user-data slur))))

	      ;; this isn't really right -- if the slur is broken, we surely will want separate
	      ;;   controls over the various slur fields.  In any case, slur dx and dy are not
	      ;;   currently passed on to the new slur -- what is the right thing here?

	      (let ((ln (short-note this-note)))
		(setf (box-x0 ln) (- (box-x0 this-note) 1.75))
		(setf (stem-end ln) nil)
		(push ln cur-notes))))
	(push this-note cur-notes)
	(setf last-note this-note)))
    (add-to-marks last-note (list (make-instance 'sundry 
				   :name :slur
				   :mark #'(lambda (mark note score &optional justifying)
					     (declare (ignore mark note))
					     (when (not justifying)
					       (prepare-slur score slur (nreverse cur-notes)))))))))

(defun prepare-slur (score slur notes)
  ;; try to make a pretty slur connecting these two notes
  ;; this involves following more or less the contour of the in-between notes
  ;; the data under the slur is between (nth i0 (staff-data staff)) and ditto i1
  ;; The slur calculation has to be delayed until nearly every thing else about the
  ;; notes is known -- in particular the beam placement.
  
  (if (or (not notes) (= (length notes) 1))
      (cmn-error "slur has lost its notes"))
  
  (loop for n0 in notes by #'cdr and n1 in (cdr notes) by #'cdr do
    (if (= (box-x0 n0) (box-x0 n1))
	(error "trouble in slur -- these two notes are simultaneous: ~&~A and ~A" (identify n0) (identify n1))))
  
  (if (= (length notes) 2)
      (make-two-note-slur score slur (first notes) (second notes) 2)
    (let* ((dir (cmn-slur-direction slur))
	   (dirs (and (not dir)
		      (loop for note in notes collect (stem-heading note))))
	   (below (if dir
		      (eq dir :down)
		    (and (stem-eq (first dirs) :up)
			 (every #'(lambda (n) (stem-eq n :up)) dirs))))
	   (ns (if below
		   (boil-under notes score)
		 (boil-over notes score))))
      (if (= (length ns) 2)
	  (make-two-note-slur score slur (first notes) (nxy-note (second ns)) (length dirs) below)
	(if (= (length ns) 3)
	    (make-three-note-slur score 
				  slur 
				  (loop for note in ns collect (nxy-note note)) 
				  below
				  (length dirs))
	  (make-four-note-slur score 
			       slur 
			       (loop for note in ns collect (nxy-note note)) 
			       below
			       (length dirs))))))
  
  (if (marks slur) (display-marks slur score nil))
  (display slur nil score))


#|
(cmn (size 24) staff treble 
     g4 q begin-slur e4 q fs4 q end-slur
     g4 q begin-slur gf4 q ef4 q end-slur
     g4 q begin-slur ef4 q c4 q end-slur
     g4 q begin-slur (b4 q stem-up) a4 q end-slur
     b4 q begin-slur e5 q d5 q cs5 q end-slur
     b4 q begin-slur d4 q e4 q fs4 q g4 q a4 q end-slur
     g4 q begin-slur ef5 q (bf4 q stem-up) ef4 q end-slur)
|#





;;;
;;; ----------------    tremolo
;;;
;;; can be measured or unmeasured, on the stem or between stems, normally (beams or flags)+tremolo-slashes=3

(defclass tremolo-mixin (tag-mixin font-mixin thick-mixin)
  ((position :initarg :position :initform nil :reader tremolo-position) ; :on-stem :between-stems nil
   (measured :initarg :measured :initform nil :reader measured) ; t or nil
   (paranoid :initarg :paranoid :initform nil :reader paranoid) ; include the word "trem."
   (slashes :initarg :slashes :initform nil :reader tremolo-slashes) ; an integer or nil = 3 total counting flags or beams
   (beams :initarg :beams :initform nil :reader tremolo-beams) ; full beams (purely stylistic foolishness)
   (width :initarg :width :initform .3 :reader width)
   (slant :initarg :slant :initform .1 :reader slant)
   (maximum-length :initarg :maximum-length :initform .85 :reader maximum-length)
   (font-name :initform (normal-font))
   (font-scaler :initform .4)
   (thickness :initform .08)))

(defclass write-protected-tremolo (write-protect tremolo-mixin) ())

(defclass tremolo (tremolo-mixin font-mixin thick)
  ((position :accessor tremolo-position)
   (measured :accessor measured)
   (paranoid :accessor paranoid)
   (slashes :accessor tremolo-slashes)
   (beams :accessor tremolo-beams)
   (width :accessor width)
   (slant :accessor slant)
   (maximum-length :accessor maximum-length)
   (name :accessor tremolo-name)
   (type :accessor tremolo-type)
   (note :accessor tremolo-note)))	; nil or the other note we are "tremolo-ing" to

(deferred-action tremolo-position)
(deferred-action measured)
(deferred-action paranoid)
(deferred-action tremolo-slashes)
(deferred-action tremolo-beams)
(deferred-action slant)
;(deferred-action width)
(deferred-action maximum-length)

(defmethod tremolo-p ((obj t)) nil)
(defmethod tremolo-p ((obj tremolo)) t)

(defmethod descry ((tremolo tremolo-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) "(tremolo" "")
	  (if (tremolo-position tremolo) (format nil " :position :~(~A~)" (tremolo-position tremolo)) "")
	  (if (tremolo-slashes tremolo) (format nil " :slashes ~D" (tremolo-slashes tremolo)) "")
	  (if (tremolo-beams tremolo) (format nil " :beams ~D" (tremolo-beams tremolo)) "")
	  (if (measured tremolo) " :measured t" "")
	  (if (paranoid tremolo) " :paranoid t" "")
	  (format nil " :width ~1,3F" (width tremolo))
	  (format nil " :slant ~1,3F" (slant tremolo))
	  (format nil " :maximum-length ~1,3F" (maximum-length tremolo))
	  (if (tremolo-note tremolo) (format nil " :note ~A" (tremolo-note tremolo)) "")
	  (if (next-method-p) (call-next-method tremolo stream (or controller tremolo)) "")
	  (if (not controller) ")" "")))

(defvar unmatched-tremolos nil)

(defmethod identify ((tremolo tremolo-mixin))
  ;; a relatively easy case -- no threads, no line crossing -- do need to use tags here just to be safe
  ;; if type one of :left :right, we have a two-note case
  (if (not (eq (tremolo-type tremolo) :right))
      (let* ((begin (eq (tremolo-type tremolo) :left))
	     (var-time (and (not begin)
			    (eq (tremolo-position tremolo) :on-stem)
			    (not (measured tremolo))
			    (not (tremolo-slashes tremolo))
			    (not (tremolo-beams tremolo))))
	     (fun-name (format nil "~A~Atremolo"
			       (if (eq (tremolo-type tremolo) :left) "begin-" "")
			       (if var-time 
				   (if (paranoid tremolo) "paranoid-" "")
				 (if (measured tremolo) "measured-" ""))))
	     (tag-name (and begin (new-cmn-store-tag "trem-"))))
	(if begin
	    (if (tremolo-note tremolo)
		(add-to-cmn-store-tags (tremolo-note tremolo) 
				       (format nil "(end-~Atremolo ~A)"
					       (if (measured tremolo) "measured-" "")
					       tag-name))
	      (push (list tremolo tag-name) unmatched-tremolos)))
	(if var-time
	    (format nil " ~A" fun-name)
	  (format nil " ~A(~A~A~A~A~A)~A"
		  (if begin (format nil "(setf ~A " tag-name) "")
		  fun-name
		  (if (and (not (measured tremolo)) 
			   (tremolo-position tremolo)
			   (not (eq (tremolo-position tremolo) :on-stem)))
		      (format nil " (tremolo-position :~(~A~))" (tremolo-position tremolo))
		    "")
		  (if (and (measured tremolo) (paranoid tremolo))
		      " (paranoid t)" "")
		  (if (and (tremolo-slashes tremolo) (not (zerop (tremolo-slashes tremolo))))
		      (format nil " (tremolo-slashes ~D)" (tremolo-slashes tremolo))
		    "")
		  (if (or (and (measured tremolo)
			       (/= (tremolo-beams tremolo) 1))
			  (and (not (measured tremolo))
			       (tremolo-beams tremolo)
			       (not (zerop (tremolo-beams tremolo)))))
		      (format nil " (tremolo-beams ~D)" (tremolo-beams tremolo))
		    "")
		  (if begin ")" ""))))
    (if unmatched-tremolos
	(let ((trem-pair (find (tremolo-name tremolo) unmatched-tremolos :key #'(lambda (n) (tremolo-name (first n))))))
	  (if trem-pair
	      (progn
		(setf unmatched-tremolos (remove trem-pair unmatched-tremolos))
		(format nil " (end-tremolo ~A)" (second trem-pair)))
	    ""))
      "")))

(defmethod copy ((tremolo tremolo-mixin) &optional object)
  (let ((new-tremolo (if (not object) (make-instance 'tremolo)
		       (if (write-protected object) (copy object)
			 object))))
    (setf (tremolo-position new-tremolo) (tremolo-position tremolo))
    (setf (tremolo-slashes new-tremolo) (tremolo-slashes tremolo))
    (setf (tremolo-beams new-tremolo) (tremolo-beams tremolo))
    (setf (measured new-tremolo) (measured tremolo))
    (setf (paranoid new-tremolo) (paranoid tremolo))
    (setf (width new-tremolo) (width tremolo))
    (setf (slant new-tremolo) (slant tremolo))
    (setf (maximum-length new-tremolo) (maximum-length tremolo))
    (if (next-method-p) (call-next-method tremolo new-tremolo))
    new-tremolo))

(defun user-set-tremolo (note object) 
  (setf (tremolo-note object) note) 
  (setf (get-tremolo note) object))

(defun ur-tremolo (&rest objects)
  (let ((new-trem (make-instance 'tremolo)))
    (loop for object in objects do
      (if (self-acting-p object)
	  (funcall (action object) new-trem (argument object))))
    new-trem))

(defun tremolo (&rest objects)
  (make-self-acting 
   :action #'user-set-tremolo
   :argument (apply #'ur-tremolo objects)))

(defvar tremolo (make-self-acting
		 :action #'user-set-tremolo
		 :argument (make-instance 'tremolo :position :on-stem)))

(defvar unmeasured-tremolo (make-self-acting
			    :action #'user-set-tremolo
			    :argument (make-instance 'tremolo :position :on-stem)))
(defun unmeasured-tremolo (&rest objects) 
  (apply #'tremolo (tremolo-position :on-stem) objects))

(defvar paranoid-tremolo (make-self-acting
			  :action #'user-set-tremolo
			  :argument (make-instance 'tremolo :paranoid t :position :on-stem)))
(defun paranoid-tremolo (&rest objects) 
  (apply #'tremolo (tremolo-position :on-stem) (paranoid t) objects))

(defvar measured-tremolo (make-self-acting
			  :action #'user-set-tremolo
			  :argument (make-instance 'tremolo :position :on-stem :measured t :slashes 1 :beams 0)))

(defun measured-tremolo (&rest objects) 
  (let* ((new-sa (apply #'tremolo (measured t) objects))
	 (new-trem (argument new-sa)))
    (if (not (tremolo-position new-trem)) 
	(setf (tremolo-position new-trem) :on-stem))
    (if (not (tremolo-slashes new-trem))
	(setf (tremolo-slashes new-trem)
	  (if (eq (tremolo-position new-trem) :on-stem)
	      1
	    0)))
    (if (not (tremolo-beams new-trem))
	(setf (tremolo-beams new-trem)
	  (if (eq (tremolo-position new-trem) :on-stem)
	      0
	    1)))
    new-sa))


;;;   have to set duration and beats to 1/2 expected value,
;;;           make sure stems go same direction, choose up or down for whole notes too -- this in markify after beam handles its cases
;;; either full beams ("traditional notation") or partial, any number of either (for measured tremolo)
;;; also have to include slur if not measured and involves two notes

(defun halve-duration (object)
  (and (get-tremolo object)
       (member (tremolo-type (get-tremolo object)) '(:left :right))))

(defvar tremolo-counter 0)
(defun new-tremolo () (make-instance 'tremolo :name (incf tremolo-counter) :type :left))
(defun push-tremolo (trem) (push trem tremolo-stack) trem)
(defun pop-tremolo () (pop tremolo-stack))

(defun tremolo-tremolo (&optional trem)
  (let ((t-trem (or trem (pop-tremolo) (cmn-error "tremolo confused"))))
    (make-instance 'tremolo :name (tremolo-name t-trem) :type :right)))

(defun user-add-to-left-tremolos (note trem)
  (let ((new-trem (if (and trem (write-protected trem)) (copy trem) trem)))
    (if new-trem (setf (tremolo-type new-trem) :left))
    (setf (get-tremolo note) (push-tremolo (or new-trem (new-tremolo))))))

(defun user-add-to-right-tremolos (note trem)
  (if trem
      (let ((actual-trem (argument trem)))
	(setf tremolo-stack (remove actual-trem tremolo-stack))
	(setf (get-tremolo note) (tremolo-tremolo actual-trem)))
    (setf (get-tremolo note)
      (tremolo-tremolo (pop-tremolo)))))

(defvar begin-tremolo (make-self-acting :action #'user-add-to-left-tremolos :argument nil))
(defvar end-tremolo (make-self-acting :action #'user-add-to-right-tremolos :argument nil))

(defun begin-tremolo (&rest args) 
  (make-self-acting 
   :action #'user-add-to-left-tremolos 
   :argument (argument (apply #'tremolo args))))

(defun end-tremolo (&optional trem) 
  (make-self-acting :action #'user-add-to-right-tremolos :argument trem))

(defun begin-measured-tremolo (&rest args)
  (make-self-acting 
   :action #'user-add-to-left-tremolos 
   :argument (argument (apply #'tremolo (measured t) (tremolo-slashes 0) (tremolo-beams 1) args))))

(defun end-measured-tremolo (&optional trem)
  (make-self-acting :action #'user-add-to-right-tremolos :argument trem))

(defvar begin-measured-tremolo (make-self-acting 
				:action #'user-add-to-left-tremolos 
				:argument (make-instance 'write-protected-tremolo :measured t :slashes 0 :beams 1)))
(defvar end-measured-tremolo (make-self-acting :action #'user-add-to-right-tremolos :argument nil))


;;;
;;; ----------------    beams
;;;

(defclass beam (tag-mixin staff-relative-mixin)
  ((data :initarg :data :initform nil :accessor beam-data)
   (direction :initarg :direction :initform nil :accessor beam-direction)
   (notes :initarg :notes :initform nil :accessor beam-notes)
   (line-separation :initarg :line-separation :initform nil :accessor beam-line-separation)
   (user-data :initarg :beam-user-data :initform nil :accessor beam-user-data)
   (user-beams :initarg :beam-user-beams :initform nil :accessor beam-user-beams :accessor explicit-beams)
   (rhythmic-unit :initarg :beam-rhythmic-unit :initform nil :accessor beam-rhythmic-unit)))


(defmethod beam-p ((obj t)) nil)
(defmethod beam-p ((obj beam)) t)

(deferred-action explicit-beams)
(deferred-action beam-rhythmic-unit)
(deferred-action beam-direction)

(defvar no-beam-break (make-self-acting :action #'(lambda (beam-tag note) 
						    (declare (ignore note)) 
						    (setf (beam-rhythmic-unit beam-tag) 0)) 
					:argument nil))

(defmethod descry ((beam beam) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) "(beam" "")
	  (if (beam-direction beam) (format nil " :direction :~(~A~)" (beam-direction beam)) "")
	  (if (not (zerop (vis-dy beam))) (format nil " :dy ~1,3F" (vis-dy beam)) "")
	  (if (beam-rhythmic-unit beam) (format nil " :beam-rhythmic-unit ~1,3F" (beam-rhythmic-unit beam)) "")
	  (if (beam-notes beam) (format nil "~A :notes '(~{~A ~})" 
					(if (> (length (beam-notes beam)) 4) (format nil "~%~A~A" prewhitespace prewhitespace) "")
					(loop for note in (beam-notes beam) collect (brief-identify note)))
	    "")
	  (if (beam-data beam) 
	      (format nil "~%~A              :data ~A~{~%                                        ~A~}~
                           ~%~A             "
		      prewhitespace
		      (car (beam-data beam)) (cdr (beam-data beam))
		      prewhitespace)
	    "")
	  (if (beam-user-data beam) 
	      (format nil " :user-data '(~{~1,3F ~})" (beam-user-data beam))
	    "")
	  (if (beam-user-beams beam)
	      (format nil " :user-beams '(~{~1,3F ~})" (beam-user-beams beam))
	    "")
	  (if (next-method-p) (call-next-method beam stream (or controller beam)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((beam beam) &optional object)
  (let ((new-beam (if (not object) (make-instance 'beam)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (beam-data new-beam) (beam-data beam))
    (setf (beam-direction new-beam) (beam-direction beam))
    (setf (vis-dy new-beam) (vis-dy beam))
    (setf (beam-rhythmic-unit new-beam) (beam-rhythmic-unit beam))
    (setf (beam-user-data new-beam) (copy-list (beam-user-data beam)))
    (setf (beam-user-beams new-beam) (copy-list (beam-user-beams beam)))
    (if (beam-notes beam) (setf (beam-notes new-beam) (loop for note in (beam-notes beam) collect (copy note))))
    (if (next-method-p) (call-next-method beam new-beam))
    new-beam))

(defmethod house ((beam beam) score)
  (declare (ignore score))
  ;; depends on note placement when housing is well in the past
  nil)

(defstruct (bdat 
	    (:print-function
	     (lambda (d s k)
	       (declare (ignore k))
	       (format s "~1,3F ~1,3F ~1,3F ~A :fb ~A :plb ~A :prb ~A ~A ~A :full ~A :above ~A :stfy0 ~1,3F"
		       (bdat-x0 d) (bdat-y0 d) (bdat-y1 d) 
		       (bdat-typ d) (bdat-fb d) (bdat-plb d) (bdat-prb d)
		       (bdat-strem d) (bdat-btrem d) (bdat-bfull d) (bdat-above d) (bdat-stfy0 d)))))
  x0 y0 y1 typ fb plb prb strem btrem bfull above stfy0)

(defmethod dsud ((b beam) num-i)
  (if (not (beam-user-data b)) 
      (setf (beam-user-data b) (list 0 0 0 0)))
  (if (> (second num-i) 3) 
      (cmn-error "beam can't handle d~A~D" 
	     (if (evenp (second num-i)) "x" "y")
	     (floor (second num-i) 2)))
  (setf (nth (second num-i) (beam-user-data b)) (first num-i)))

(defun lots-of-leger-lines (score stems &optional (direction :up))
  (declare (ignore score))
  (let* ((stf-y0 (bdat-stfy0 (first stems)))
	 (leger-top (if (eq direction :up)
			(- stf-y0 (* 2 *staff-line-separation*))
		      (+ stf-y0 (* 10 *staff-line-separation*))))
	 (leger-notes (if (eq direction :up)
			  (loop for stem in stems sum (if (< (bdat-y0 stem) leger-top) 1 0))
			(loop for stem in stems sum (if (> (bdat-y0 stem) leger-top) 1 0))))
	 (nstems (length stems)))
    (> leger-notes (* .6 nstems))))

(defun balanced-notes (stems)
  (let* ((nstems (length stems))
	 (mid-group (floor nstems 2))
	 (mid-group-1 (ceiling nstems 2))
	 (all-y0 (loop for i from 0 below mid-group and desc in stems sum (bdat-y0 desc)))
	 (all-y1 (loop for i from mid-group-1 below nstems sum (bdat-y0 (nth i stems)))))
    (< (abs (- all-y1 all-y0)) .5)))

(defun draw-beam (score x0 y0 x1 y1 &optional (thickness 1.0))
  (let ((beam-thickness (* thickness *beam-width*)))
    (moveto score x0 y0)
    (lineto score x0 (+ y0 beam-thickness))
    (lineto score x1 (+ y1 beam-thickness))
    (lineto score x1 y1)
    (lineto score x0 y0)
    (fill-in score)))

(defun draw-beams (score x0 py0 x1 py1 &optional (beams 1) (direction :up) (thickness 1.0))
  (let ((beam-spacing (* thickness (if (member direction '(:up :up?)) *beam-spacing* (- *beam-spacing*)))))
    #-(or gcl sbcl) (loop for i from 1 to beams and
	   y0 from py0 by beam-spacing and
	   y1 from py1 by beam-spacing do
	    (draw-beam score x0 y0 x1 y1 thickness))
    #+(or gcl sbcl) (do ((i 1 (1+ i))
	       (y0 py0 (+ y0 beam-spacing))
	       (y1 py1 (+ y1 beam-spacing)))
	      ((> i beams))
	    (draw-beam score x0 y0 x1 y1 thickness))
    ))

(defmethod display ((beam beam) container score &optional justifying)
  (declare (ignore container justifying))
  ;; stems is a list of bdat stem descriptors
  ;; the stem should start at x0 y0, the bottom note head in :up case
  ;;  then should go at least to y1 -- this number chosen to avoid collisions between beams and note heads
  (let* ((slope-trigger *beam-slope-trigger*)
	 (direction (beam-direction beam))
	 (notes (beam-notes beam))
	 (line-sep (or (beam-line-separation beam) *staff-line-separation*))
	 (descs (beam-data beam)))

    (loop for desc in descs and note in notes do
      (incf (bdat-x0 desc) 
	    (if (member (note-head note) *centered-note-heads*)
		(if (bdat-above desc)
		    (- (note-head-x0-offset note t *half-stem-width*) *half-stem-width*)
		  (- (note-head-x0-offset note nil *half-stem-width*) *half-stem-width*))
	      (if (bdat-above desc)
		  (- (+ (center note) .145) *stem-width*)
		(- (+ (center note) .145) (* .29 (* (note-size note) *note-head-size*))))))
      ;; see cmntest for various cases that break the note-head-x0-offset case --
      ;;   need to figure out what to do here
      (incf (bdat-y0 desc) (bdat-stfy0 desc))
      (incf (bdat-y1 desc) (bdat-stfy0 desc)))
    
    (let* ((max-y1 (or (if (eq direction :up)
			   (loop for desc in descs maximize (bdat-y1 desc))
			 (if (eq direction :down)
			     (loop for desc in descs minimize (bdat-y1 desc))
			   (loop for desc in descs maximize (bdat-stfy0 desc))))
		       0))
	   ;; this is the lowest the top beam can be (if :up)
	   (min-y1 (or (if (eq direction :up)
			   (loop for desc in descs minimize (bdat-y1 desc))
			 (if (eq direction :down)
			     (loop for desc in descs maximize (bdat-y1 desc))
			   (loop for desc in descs minimize (bdat-stfy0 desc))))
		       0))
	   ;; this is the lowest possible stem (for max stem length checks)
	   (max-min-stem (- max-y1 min-y1))
	   ;; this is the biggest stem if we choose the smallest possible stems and a horizontal beam
	   (first-y1 (bdat-y1 (first descs)))
	   (last-y1 (bdat-y1 (first (last descs))))
	   ;; we want horizontal beams when 1. group starts and ends on nearly same note
	   ;;                               2. most the notes use leger lines
	   ;;                               3. notes are grouped in a balanced way vertically => medium = midpoint
	   ;; otherwise we want a slant that is not more than a staff space = .25
	   ;; if possible, each stem should be at least y2 long before the bottom beam
	   (end-diff (- last-y1 first-y1))
	   (stem-x0 (bdat-x0 (first descs)))
	   (stem-x1 (bdat-x0 (first (last descs))))
	   (total-x (- stem-x1 stem-x0))
	   (left-y max-y1)
	   (right-y max-y1)
	   (first-staffy0 (bdat-stfy0 (first descs)))
	   (beam-space *beam-spacing*)
	   (max-beams (or (loop for desc in descs
		           maximize (+ (or (bdat-fb desc) 0) 
				       (or (bdat-plb desc) 0) 
				       (or (bdat-prb desc) 0) 
				       (* .5 (or (bdat-strem desc) 0))
				       (or (bdat-btrem desc) 0)))
			  0))
	   (stem-len (max (- *ideal-stem-length*
			     (* (min max-beams 3) 
				line-sep))
			  (* max-beams line-sep))))
      
      (if (eq direction :between)
	  (let* ((up-stf0 (or (loop for desc in descs maximize (bdat-stfy0 desc)) 0))
		 (down-stf0 (or (loop for desc in descs minimize (bdat-stfy0 desc)) 0))
		 (beam-us (beam-notes beam))
		 (up-notes (loop for note in beam-us if (= up-stf0 (staff-y0 note)) collect note))
		 (down-notes (loop for note in beam-us if (= down-stf0 (staff-y0 note)) collect note))
		 (up-bottom (or (loop for note in up-notes minimize (minimum-line note)) 0))
		 (down-top (or (loop for note in down-notes maximize (maximum-line note)) 0))
		 (plausible-mid-point (* .5 (+ (+ down-stf0 (* *staff-line-separation* (max 8 down-top)))
					       (+ up-stf0 (* *staff-line-separation* (min 0 up-bottom)))))))
	    (setf left-y plausible-mid-point)
	    (setf right-y plausible-mid-point))
	(if (or (< (abs end-diff) slope-trigger) ;half an octave (.5) normally
		(some #'(lambda (n) (/= (bdat-stfy0 n) first-staffy0)) descs)
		(balanced-notes descs)	;repeated patterns
		(lots-of-leger-lines score descs direction))
	    (progn			;horizontal beam
	      (if (< max-min-stem 1.0)	;ambitus is within an octave
		  (setf left-y (+ max-y1 (if (eq direction :up) stem-len (- stem-len))))
		(if (< max-min-stem 2.0) ;here the "ideal" makes a stem too long
		    (setf left-y (+ max-y1 (* (if (eq direction :up) 1.0 -1.0)
					      line-sep
					      (- 2 max-min-stem))))
		  (setf left-y max-y1)))
	      (setf right-y left-y))
	  ;; here we have to tilt the beam one way or the other and once again get the best overall placement
	  (let* ((tilt-direction (if (> first-y1 last-y1) :down :up))
		 (max-tilt *maximum-beam-tilt*)
		 (tilt-amount (* max-tilt (max -1.0 (min 1.0 end-diff))))
		 (min-stems (loop for desc in descs if (= (bdat-y1 desc) max-y1) collect (bdat-x0 desc)))
		 ;; this is a list of the x coordinates of all the stems that are the minimum length
		 ;; we have to take either the first or last (depending on tilt-direction) and get the
		 ;;   equivalent y1 point on the tilting beam -- then refigure the max-min-stem value
		 ;;   and decide where to put the actual beam using basically the same decisions as before.
		 (min-stem-x0 (if (eq tilt-direction :up) 
				  (first min-stems) 
				(nth (1- (length min-stems)) min-stems)))
		 (x-frac (1- (divide (- min-stem-x0 stem-x0) total-x)))
		 (left-tilt (* tilt-amount x-frac))
		 )
	    ;;	       (tilted-fixup (max 0 (- slope-trigger .35)))) ;default beam-slope-trigger is .5
	    ;;	  (decf stem-len (+ tilted-fixup *beam-width*))
	    ;; these lines screw up on patterns like (cmn staff bass f3 s fs3 s g3 s a2 s c3 s cs3 s d3 s g3 s)
	    
	    (decf stem-len *beam-width*)
	    (if (< max-min-stem 1.0)
		(setf left-y (+ max-y1 left-tilt (if (eq direction :up) stem-len (- stem-len))))
	      (if (< max-min-stem 2.0)
		  (setf left-y (+ max-y1 left-tilt (* (if (eq direction :up) 1.0 -1.0)
						      line-sep
						      (- 2 max-min-stem))))
		(setf left-y (+ max-y1 left-tilt))))
	    (setf right-y (+ left-y tilt-amount)))))
    
      ;; now we're ready to draw the beams, stems (partial or full)
      
      (when (eq direction :up)
	(decf left-y *beam-width*)
	(decf right-y *beam-width*))
      
      (when (not (zerop (vis-dy beam)))
	(incf left-y (vis-dy beam))
	(incf right-y (vis-dy beam)))
      
      (when (beam-user-data beam)
	(incf left-y (second (beam-user-data beam)))
	(incf right-y (fourth (beam-user-data beam))))

      (when (list-p (visible-justification beam)) ;needed by wedge beam display to pass along room request
	(if (eq direction :up)
	    (progn
	      (incf left-y (first (visible-justification beam)))
	      (incf right-y (second (visible-justification beam))))
	  (if (eq direction :down)
	      (progn
		(decf left-y (first (visible-justification beam)))
		(decf right-y (second (visible-justification beam)))))))
      
      (setf (outer-beam (first notes)) (list (bdat-x0 (first descs)) left-y (bdat-x0 (first (last descs))) right-y))
      
      (let* ((current-beams 0)
	     (last-x0 stem-x0)
	     (left-over-stems (cdr descs))
	     (y-diff (- right-y left-y))
	     (interp-frac (divide y-diff total-x))
	     (need-draw nil)
	     (between-p (eq direction :between))
	     (between-above (bdat-above (first descs)))
	     (old-color (color score)))

	(setf (line-width score) *stem-width*) ;first draw all the stems
	(if (color beam) (setf (color score) (color beam)))
	(loop for desc in descs and note in notes do
	  (let* ((x0 (bdat-x0 desc))
		 (full-stem (eq (bdat-typ desc) :full))
		 (ntrem (bdat-strem desc))
		 (bx0 (+ x0 *half-stem-width*))
		 (stf-y0 (bdat-stfy0 desc))
		 (above (bdat-above desc))
		 (y1 (+ (if (zerop y-diff)
			    left-y
			  (+ left-y (* (- x0 stem-x0) interp-frac)))
			(if above *beam-width* 0)
			(if (and between-p 
				 (or (> current-beams 1)
				     (> (bdat-fb desc) 1)))
			    (if above
				(if (not between-above)
				    (* (1- (max current-beams (bdat-fb desc))) beam-space)
				  0)
			      (if between-above
				  (* -1.0 (1- (max current-beams (bdat-fb desc))) beam-space)
				0))
			  0)))
		 (y0 (if full-stem 
			 (+ (bdat-y0 desc) (note-head-y0-offset note above))
		       (if (zerop *partial-stem-length*)
			   y1
			 (+ y1 (* (+ (* (+ (max current-beams 
						(bdat-fb desc))
					   (max 0 (or (bdat-plb desc) 0))
					   (max 0 (or (bdat-prb desc) 0)))
					beam-space)
				     *partial-stem-length*)
				  (if above -1.0 1.0)))))))
	    
	    (setf current-beams (bdat-fb desc))
	    (let ((ys0 (+ y0 (if (not (rest-p note)) (vis-dy note) 0) (or (stem-dy note) 0))))
	      ;; ideally we'd use (box-y0 note) here since it is the true stem end,
	      ;; but then the beam itself can be misplaced
	      (moveto score bx0 ys0)
 	      (lineto score bx0 y1 :pattern (pattern note))
	      (if (stem-mark note) 
		  (progn
		    (draw score) 
		    (setf need-draw nil)
		    (funcall (stem-mark note) score bx0 y0 y1)
		    (setf (line-width score) *stem-width*))
		(setf need-draw t))
	    
	      (when (audible-p note)
		(if above 
		    (setf (box-y1 note) (- y1 stf-y0))
		  (if (and (stem-end note)
			   (> (+ stf-y0 (stem-end note)) (box-y0 note)))
		      (setf (box-y1 note) (+ .2 (- (max y0 (box-y0 note)) stf-y0)))))
		(setf (stem-end note) y1))
	      (when (plusp ntrem) 
		(draw score)
		(setf need-draw nil)
		(draw-tremolo score nil (get-tremolo note) ntrem bx0 ys0 y1 (if (> y1 ys0) :up :down) .1)))))
	
	(if need-draw (draw score))
	(setf (line-width score) 0)
	(setf current-beams 0)
	
	(loop for desc in descs and note in notes do
	  ;; see how many new beams are heading to the right, draw them as far as possible.
	  ;; draw partial-beams both ways
	  (let* ((beams-to-right (bdat-fb desc))
		 (first-x (+ (bdat-x0 desc) *half-stem-width*))
		 (initial-beam-offset (* current-beams beam-space))
		 (first-y (if (zerop y-diff) 
			      left-y
			    (+ left-y (* (- first-x stem-x0) interp-frac))))
		 (above (if (not between-p) (bdat-above desc) between-above)))
	    (loop for i from 0 and k from (1+ current-beams) to beams-to-right do
	      (let* ((last-stem (if (not left-over-stems)
				    (cmn-error "beam info falls off the end of the beam group")
				  (loop for next-stem in left-over-stems
				   if (< (bdat-fb next-stem) k) return next-stem)))
		     (last-x (+ (or (and last-stem (bdat-x0 last-stem))
				    stem-x1)
				*half-stem-width*))
		     (last-y (if (zerop y-diff)
				 left-y
			       (+ left-y (* (- last-x stem-x0) interp-frac))))
		     (beam-height (+ initial-beam-offset (* i beam-space))))
		(draw-beam score
			   first-x 
			   (+ first-y (* (if above -1.0 1.0) beam-height)) 
			   last-x 
			   (+ last-y (* (if above -1.0 1.0) beam-height)))))
	    
	    (if (and (bdat-plb desc) 
		     (plusp (bdat-plb desc)) ;there are partial beams heading left from first-x at the bottom of the full beams
		     (or (beamed note)
			 (/= (bdat-fb desc) 0)))
		(let* ((start-y (+ first-y
				   (* current-beams (if above -1.0 (bdat-plb desc)) beam-space)))
		       (beam-length (min *partial-beam-length*
					 (* .4 (- first-x last-x0))))
		       (end-y (- start-y
				 (* interp-frac beam-length))))
		  (draw-beams score
			      (- first-x beam-length)
			      end-y
			      first-x
			      start-y
			      (bdat-plb desc)
			      :down)))
	    (if (and (bdat-prb desc)
		     (plusp (bdat-prb desc)) ;there are partial beams heading right from first-x at the bottom of the full beams
		     (or (beamed note)	     ;special case -- "beam" is broken over some flagged note
			 (/= (bdat-fb desc) 0)))
		(let* ((start-y (+ first-y
				   (* beams-to-right (if above -1.0 (bdat-prb desc)) beam-space)))
		       (next-x (if left-over-stems
				   (bdat-x0 (first left-over-stems))
				 stem-x1))
		       (beam-length (min *partial-beam-length*
					 (* .4 (- next-x first-x))))
		       (end-y (+ start-y
				 (* interp-frac beam-length))))
		  (draw-beams score
			      first-x
			      start-y
			      (+ first-x beam-length)
			      end-y
			      (bdat-prb desc)
			      :down)))
	    
	    (if (and (bdat-btrem desc)
		     (plusp (bdat-btrem desc))) ;tremolo "beams" tracking true beams
		(let* ((start-y (+ first-y
				   (* beams-to-right (if above -1.0 1.0) beam-space)))
		       (next-x (if left-over-stems
				   (bdat-x0 (first left-over-stems))
				 stem-x1))
		       (trem-beam-space (or (bdat-bfull desc) .125))
		       (beam-start (+ first-x trem-beam-space))
		       (beam-end (- next-x trem-beam-space))
		       (end-y (+ start-y (* interp-frac (- next-x first-x trem-beam-space)))))
		  (draw-beams score
			      beam-start
			      start-y
			      beam-end
			      end-y
			      (bdat-btrem desc)
			      (if above :down :up)
			      (if (bdat-bfull desc) 1.0 .9))))
	    
	    (setf left-over-stems (cdr left-over-stems))
	    (setf last-x0 first-x)
	    (setf current-beams beams-to-right)))
	(if (color beam) (setf (color score) old-color))))))


;;; beamify runs through the object list looking for groups of objects that need beams.
;;; beams are not yet drawn, just described.

(defun flatten (L)			;borrowed from /dist/lisp/mac/Lisp-Utilities/extensions.lisp
  "Flattens list L, i.e., returns a single list containing the
   same atoms as L but with any internal lists 'dissolved'. For example,
   (flatten '(a (b c) d))  ==>  (a b c d)
   Recursively flattens components of L, according to the following rules:
    - an atom is already flattened.
    - a list whose CAR is also a list is flattened by appending the
      flattened CAR to the flattened CDR (this is what dissolves internal
      lists).
    - a list whose CAR is an atom is flattened by just flattening the CDR
      and CONSing the original CAR onto the result.
   These rules were chosen with some attention to minimizing CONSing."
  
  (cond ((null L) '())
	((atom L) L)
	((consp L)
	 (if (consp (car L))
	     (append (flatten (car L)) (flatten (cdr L)))
	   (cons (car L) (flatten (cdr L)))))
	(t L)))

(defun locked-stem-direction (objects)
  (loop for object in objects do
      (if (and (audible-p object)
	     (stem-is-locked object))
	(return-from locked-stem-direction (stem-direction object)))))

(defun beam-across-line-break (score the-beam stf-y0 beam-us beam-break &optional specific-stem-direction beam-line-sep)
  ;; break beam into n pieces (one for each line) with right partial beam at end of 1st and so on.
  (let ((cury0 stf-y0)
	(cur-beam nil) 
	(explicit-beam (find :begin-beam (store-data (first beam-us)))))
    (loop for note in beam-us do
      (if (= cury0 (staff-y0 note))
	  (push note cur-beam)
	(progn
	  (when (> (length cur-beam) 1)
	    (if explicit-beam (pushnew :end-beam (store-data (first cur-beam))))
	    (setf cur-beam (reverse cur-beam))
	    (if explicit-beam (pushnew :begin-beam (store-data (first cur-beam))))
	    (annotate-beam score the-beam cury0 cur-beam beam-break specific-stem-direction beam-line-sep))
	  (setf cury0 (staff-y0 note))
	  (setf cur-beam nil)
	  (push note cur-beam))))
    (when (and cur-beam (> (length cur-beam) 1)) 	    
      (setf cur-beam (reverse cur-beam))
      (if explicit-beam (pushnew :begin-beam (store-data (first cur-beam))))
      (annotate-beam score the-beam cury0 cur-beam beam-break specific-stem-direction beam-line-sep))))

(defun beam-between-staves (score the-beam stf-y0 beam-us beam-break &optional specific-stem-direction beam-line-sep)
  ;; here we have a beam between staves -- unless instructed otherwise (via stem-direction)
  ;;  we try to put the beam between the staves.
  (let ((last-displayed-note (first beam-us)))
    (loop for note in beam-us do 
      (setf (beamed note) t))
    (loop for note in (cdr beam-us) do
      (if (<= (staff-y0 note) (staff-y0 last-displayed-note))
	  (setf last-displayed-note note)))
    (setf (beams last-displayed-note)
      (annotate-beam-1 score the-beam stf-y0 beam-us beam-break specific-stem-direction beam-line-sep))))

(defun annotate-beam (score the-beam stf-y0 beam-us beam-break 
		      &optional specific-stem-direction beam-line-sep)
  ;; beam-us is the list of notes (chords) to share the current beam
  (if (and the-beam (beam-rhythmic-unit the-beam)) 
      (setf beam-break (and (not (zerop (beam-rhythmic-unit the-beam)))
			    (beam-rhythmic-unit the-beam))))
  (let ((stf (staff-y0 (first beam-us))))
    (if (notevery #'(lambda (n) (= (staff-y0 n) stf)) beam-us)
	;; here we may have either a beam between staves or a beam broken at a line break (or both...)
	;; since cmn syntax doesn't provide explicit dis-ambiguation, we'll do it by examining the line end/start
	;; beamify follows justify, so we can place a end-of-line marker during the line-break in justify
	(if (some #'(lambda (n) (find :line-break (store-data n))) (butlast beam-us))
	    (beam-across-line-break score the-beam stf-y0 beam-us beam-break specific-stem-direction beam-line-sep)
	  ;; (cmn staff treble c4 e c4 e c4 e c4 e (c4 e begin-beam) line-mark c4 e (c4 e end-beam))
	  (beam-between-staves score the-beam stf-y0 beam-us beam-break specific-stem-direction beam-line-sep))
      (setf (beams (first beam-us))
	(annotate-beam-1 score the-beam stf-y0 beam-us beam-break specific-stem-direction beam-line-sep)))))


(defun annotate-beam-1 (score the-beam stf-y0 beam-us beam-break 
		      &optional specific-stem-direction beam-line-sep)
  (let* ((lines (flatten (loop for note in beam-us if (not (rest-p note)) collect (line note))))
	 (stf-y0s (loop for note in beam-us collect (staff-y0 note)))
	 (max-stf-y0 (or (loop for stf in stf-y0s maximize stf) 0))
	 (min-stf-y0 (or (loop for stf in stf-y0s minimize stf) 0))
	 (max-line (or (loop for line in lines maximize line) 0))
	 (min-line (or (loop for line in lines minimize line) 0))
	 (nflags (loop for note in beam-us collect (flags note)))
	 (ntrem (loop for note in beam-us and flgs in nflags
		 collect (if (and (get-tremolo note) 
				  (not (member (tremolo-type (get-tremolo note)) '(:left :right))))
			     (or (tremolo-slashes (get-tremolo note)) (- 3 flgs))
			   0)))
	 (btrem (loop for note in beam-us and flgs in nflags 
		 collect (if (and (get-tremolo note)
				  (eq (tremolo-type (get-tremolo note)) :left))
			     (or (tremolo-beams (get-tremolo note))
				 (- 3 flgs))
			   0)))
	 (bfull nil)
	 (main-flags (or (loop for flg in (subseq nflags 1 (1- (length nflags))) minimize flg) 0))
	 (direction (or (and (every #'(lambda (n) (= n stf-y0)) stf-y0s)
			     (or specific-stem-direction
				 (locked-stem-direction beam-us)
				 (if (< max-line 4) :up
				   (if (> min-line 4) :down
				     (if (> (- max-line 4) (- 4 min-line)) :down :up)))))
			(and the-beam (beam-direction the-beam))
			(and (some #'(lambda (n) (and (= (staff-y0 n) max-stf-y0)
						      (eq (stem-direction n) :up)))
				   beam-us)
			     :up)
			(and (some #'(lambda (n) (and (= (staff-y0 n) min-stf-y0)
						      (eq (stem-direction n) :down)))
				   beam-us)
			     :down)
			(and (= (length (remove-duplicates stf-y0s)) 2)
			     :between)
			specific-stem-direction
			(locked-stem-direction beam-us)
			:down))
	 (top-stf-y0 (or (loop for y in stf-y0s maximize y) 0))
	 (aboves (loop for stfy0 in stf-y0s collect 
		   (or (eq direction :up)
		       (and (not (eq direction :down))
			    (/= stfy0 top-stf-y0))))))

      (if (find-if #'rest-p beam-us)
	  (if (or (eq direction :between)
		  (and (eq direction :up) (< max-line 8))
		  (and (eq direction :down) (> min-line 0)))
	      ;; rests should move only if they would otherwise collide with or be on the wrong side of the beam
	      (let* ((first-note (find-if #'audible-p beam-us))
		     (note-line (line first-note))
		     (last-line (if (listp note-line) (divide (apply #'+ note-line) (length note-line)) note-line)))
		(loop for note in beam-us and above in aboves do
		  (if (rest-p note)
		      (setf (rest-line note) (* 2 (if above (floor last-line 2) (ceiling last-line 2))))
		    (progn
		      (setf note-line (line note))
		      (setf last-line (if (listp note-line) (divide (apply #'+ note-line) (length note-line)) note-line))))))))
      
      (let* ((x0s (loop for note in beam-us collect (+ (box-x0 note) (vis-dx note))))
	     (y0s (loop for note in beam-us and above in aboves
		    collect
		    (let ((line-sep (or (and (audible-p note) (local-line-separation note)) *staff-line-separation*)))
		      (if above
			  (* (minimum-line note) line-sep)
			(* (maximum-line note) line-sep)))))
	     (beat0 (floor (or (loop for note in beam-us minimize (odb-beat note)) 0)))
	     (beats (loop for note in beam-us collect (odb-beat note)))
	     (break-at (if (and beam-break (> main-flags 2)) .5 nil))
	     (btrem-last nil)
	     (y1s (loop for note in beam-us and flg in nflags and trem in ntrem and y0 in y0s and bt in btrem and above in aboves
		   collect
		   (let ((line-sep (or (and (audible-p note) (local-line-separation note)) *staff-line-separation*)))
		     (+ (if above
			    (max (* (max -2 (maximum-line note)) line-sep) y0)
			  (min (* (min 9 (minimum-line note)) line-sep) y0))
			(if above line-sep (- line-sep))
			(* trem (if above .125 -.125))
			(* (max bt (or btrem-last 0)) (if above *beam-spacing* (- *beam-spacing*)))
			(* flg (if above *beam-spacing* (- *beam-spacing*)))))
		   do (setf btrem-last bt)))
	     ;; i.e. give us at least an octave's worth of stem, extend it for extra beams by the beam width and spacing
	     ;; and make sure the beams are not floating in the leger lines (hence the min 9 for example).  
	     (full-beams-right nil)
	     (partial-beams-left nil)
	     (partial-beams-right nil)
	   
	     (left-overs nil)
	     (last-beat (first (last beats)))
	     (last-full-beam 0))

	(when btrem 
	  (setf bfull (and (find-if #'(lambda (note) 
					(and (get-tremolo note) 
					     (measured (get-tremolo note))
					     .125))
				    beam-us)
			   0)))
	(loop for note in beam-us and above in aboves do
	  (if (not (rest-p note))
	      (let ((stem-dir (audible-stem-direction note)))
		(if (or (and (not above) (eq stem-dir :up))
			(and above (eq stem-dir :down)))
		    (let ((pos (position note beam-us)))
		      (cmn-warn (format nil "attempt to override explicit stem direction setting on ~A in~%~{ ~A~}~%    ~A~%        ~{ ~A~}"
					(identify note) 
					(loop for i from (max 0 (- pos 5)) below pos collect (identify (nth i beam-us)))
					(identify note)
					(loop for i from (1+ pos) below (min (length beam-us) (+ pos 5)) 
					 collect (identify (nth i beam-us))))))
		  (progn
		    (if (and (chord-p note)
			     (or (and (eq stem-dir :up?) (eq direction :down))
				 (and (eq stem-dir :down?) (eq direction :up))))
			(let* ((lines (loop for n in (chord-data note) collect (note-line n)))
			       (lines+1 (map 'list #'1+ lines))
			       (intersect (intersection lines lines+1))
			       (unison (/= (length lines) (length (remove-duplicates lines)))))
			  (if (or intersect unison)
			      (let* ((old-x0 (x0 note))
				     (old-center (center note))
				     (old-output (output score))
				     (chord-center 0)
				     (other-note-x0 0)
				     (old-server-box (list (x0 score) (y0 score) (x1 score) (y1 score))))
				(clear-score score)
				(setf (x0 note) 0)
				(setf (audible-stem-direction note) direction)
				(multiple-value-setq
				    (chord-center other-note-x0)
				  (display note nil score t))
				(setf (output score) old-output)
				(setf (x0 score) (first old-server-box))
				(setf (y0 score) (second old-server-box))
				(setf (x1 score) (third old-server-box))
				(setf (y1 score) (fourth old-server-box))
				;; now fix up chord x0 so that center is correct given new arrangement of accidentals/note-heads
					;(setf (center note) chord-center)
				(setf (x0 note) (- (+ old-x0 old-center) chord-center))))))
		    
		    (setf (audible-stem-direction note) 
		      (if (not (eq direction :between)) direction
			(if above :up :down))))))))
      
	(if (or (not the-beam)
		(not (beam-user-beams the-beam)))
	    (progn
	      (let ((breaks (and break-at (list (+ beat0 break-at) (+ beat0 (* 2 break-at)) (+ beat0 (* 3 break-at))))))
		(loop for nfl in nflags and nfr in (cdr nflags) and beat in beats do
		  ;; need to catch both broken full beams and partial beams and decide direction of latter
		  (let ((min-lr (min nfl nfr)))
		    (if (and full-beams-right break-at
			     (member beat breaks :test #'=))
			(let ((previous-beams (first full-beams-right)))
			  (setf last-full-beam 1)
			  ;; here we are breaking the beamed group back to the basic (single) beam, so we
			  ;;  have to repair the previous left and next right beam to reflect that change.
			  (setf (first full-beams-right) 1)
			  (if (second full-beams-right)
			      (incf (first left-overs) (- previous-beams (second full-beams-right))))))
		    (push min-lr full-beams-right)
		    (push (- nfl (max last-full-beam min-lr)) left-overs)
		    (setf last-full-beam min-lr))))
	    
	      (push (- (first (last nflags)) (first full-beams-right)) left-overs)
	      (push 0 full-beams-right)
	    
	      (setf left-overs (nreverse left-overs))
	      (setf full-beams-right (nreverse full-beams-right))

	      (loop for nflags in left-overs and beat in beats do
		(if (zerop nflags)
		    (progn
		      (push 0 partial-beams-left)
		      (push 0 partial-beams-right))
		  (if (not partial-beams-right)
		      (progn
			(push nflags partial-beams-right)
			(push 0 partial-beams-left))
		    (if (= beat last-beat)
			(progn
			  (push nflags partial-beams-left)
			  (push 0 partial-beams-right))
		      (if (and beam-break
			       (or (and (>= beam-break 1.0)
					(or (integerp beat) (= (denominator beat) 2)))
				   (and (= beam-break .75)
					(or (= beat .25) (= beat .5)))))
			  (progn
			    (push nflags partial-beams-right)
			    (push 0 partial-beams-left))
			(progn
			  (push nflags partial-beams-left)
			  (push 0 partial-beams-right)))))))
	    
	      (setf partial-beams-left (nreverse partial-beams-left))
	      (setf partial-beams-right (nreverse partial-beams-right)))
	
	  (progn			; else side of [if [not [beam-user-beams the beam]]]
	    (setf full-beams-right (first (beam-user-beams the-beam)))
	    (setf partial-beams-right (second (beam-user-beams the-beam)))
	    (setf partial-beams-left (third (beam-user-beams the-beam)))))

	(let ((beam-info (loop for x0 in x0s and
			  y0 in y0s and
			  y1 in y1s and
			  note in beam-us and
			  fb in full-beams-right and
			  plb in partial-beams-left and
			  prb in partial-beams-right and
			  trem in ntrem and
			  bt in btrem and
			  above in aboves and
			  stfy0 in stf-y0s
			  collect (make-bdat :x0 x0 :y0 y0 :y1 y1 :typ (if (audible-p note) :full :partial) 
					     :fb fb :plb plb :prb prb :strem trem :btrem bt :bfull bfull
					     :above above :stfy0 stfy0)))
	      (fb-in 0))
	  (loop for note in beam-us and fb in full-beams-right do 
	    (if (or (> fb-in 0) (> fb 0) (stem-tie note) (= (flags note) 0)) (setf (beamed note) t) (setf (beamed note) nil))
	    (setf fb-in fb)
	    (if (get-tremolo note) (setf (tremolo-slashes (get-tremolo note)) 0))
	    (if (chord-p note) (loop for true-note in (chord-data note) do (setf (beamed true-note) t))))
	  (make-instance 'beam 
	   :data beam-info 
	   :staff-y0 stf-y0 
	   :direction direction 
	   :line-separation beam-line-sep
	   :notes beam-us
	   :dy (or (and the-beam (vis-dy the-beam)) 0)
	   :dx (or (and the-beam (vis-dx the-beam)) 0)
	   :justification (and the-beam (visible-justification the-beam))
	   :beam-user-data (and the-beam (copy-list (beam-user-data the-beam)))))))) ; next stop L3287 cmn4.lisp

#|
;;; test case for staff-line-separation check from Anders Vinjar
(cmn (size 24)
     (free-expansion-factor 3.0)
     (staff-separation 3.0)
     (staff (g4 e  (stem-up) (setf tag (beam- (dy 1.5))))
            (half-rest  invisible))
     (staff (treble invisible)
            (eighth-rest invisible)
            (b3 e  (-beam- tag))
            (d5 e (-beam- tag))
            (quarter-rest invisible))
     (staff (staff-line-separation 0.3)
            (treble invisible)
            (dotted-quarter-rest invisible)
            (chord (notes b3 d3) e  (-beam- tag))
            (d5 e (-beam tag))))
|#

(defun stem-directions-fit (score current this)
  (or (not current)
      (not this)
      (eq current this)
      (and (member current '(:up :up?)) (member this '(:up :up?)))
      (and (member current '(:down :down?)) (member this '(:down :down?)))
      (and (liberal-automatic-beams score) (member current '(:up? :down?)))))


(defun function-p (object)
  ;; our lisp thinks (functionp t) is t and (typep t 'function) is t, but doesn't allow (funcall t)
  ;; so what damn good is functionp??  but we can't look for LAMBDA (which is not always the car
  ;; of the list if there's a lexical closure) because the compiled form is some #<function> object)
  ;; so this ridiculous function is aimed at the special case of the beamed field that had better be
  ;; either nil or t or a function of some sort, because I've given up trying to find a way to
  ;; distinguish a funcall-able thing from anything else.
  (and object
       (not (eq object t))))

;;; we can't use Franz's suggestion because it returns (function-p (function (lambda (a) a))) = NIL
;;; (fboundp object)
;;; (not (macro-function object))
;;; (not (special-operator-p object))



(defun analyze-beam (beamed-notes)	;only for automatic beams (so we can ignore cross-staff troubles)
  (when (some #'chord-p beamed-notes)
    ;; only chords with notes on both sides and a disagreement within the beam cause spacing trouble
    (let* ((direction (if (some #'(lambda (n) (eq (stem-direction n) :up)) beamed-notes)
			  :up
			(if (some #'(lambda (n) (eq (stem-direction n) :down)) beamed-notes)
			    :down
			  (let* ((lines (flatten (loop for note in beamed-notes if (not (rest-p note)) collect (line note))))
				 (max-line (or (loop for line in lines maximize line) 0))
				 (min-line (or (loop for line in lines minimize line) 0)))
			    (if (< max-line 4) :up?
			      (if (> min-line 4) :down?
				(if (> (- max-line 4) (- 4 min-line)) :down? :up?))))))))
      (loop for n in beamed-notes do
	(if (and (chord-p n) (not (audible-stem-direction n)))
	    (setf (audible-stem-direction n) direction))))))

(defun beamify-staff (score staff)
  
  ;; first run through data lists looking for successive flagged notes within a beat
  ;; if found, set (fourth (object-value)) to :filled-head (or (:filled-head n) = dotted)
  ;; get max and min pline values (for direction decision)
  ;; get x0 y0 y1 for each note + beaming descriptions
  
  ;; beaming descriptor goes on data list of first note of the group.
  ;; :beam-up or :beam-down is type, value is stems list, stf-y0 is available via display args
  
  ;; stems = list of stem descriptors '(x0 y0 y1 type full-beams-right partial-left-beams partial-right-beams stem-tremolo)
  ;; the stem should start at x0 y0 (the bottom note head in :up case)
  ;;  then should go at least to y1 (this number chosen to avoid collisions between beams and note heads)
  ;; The type field indicates what kind of stem this is (:partial :full etc)
  ;; all coordinates are user-style (pre-font-scaling)
  
  (let*	((beam-start nil)		;the object at the start of the beams (data<-beam info)
	 (beam-us nil)			;list of objects under one beam
	 (beat-length 1.0)
	 (beats 200000)
	 (bar-first-beat 0)		;start of current measure in fixed-up (actual) beat values
	 (beam-break 1.0)
	 (beam-first-beat 0)		;start of beam within bar (actual beat)
	 (beam-counter 0)		;0-based into list of beam patterns if an odd meter
	 (current-beam-break 1.0)	;length of current beam beat in beats
	 (current-stem-direction nil)
	 (current-stems-tied nil)	;are we dealing with stems across multiple staves
	 (staff-break nil)
	 (bar-break nil)
	 (meter-break nil))

    (loop for object in (staff-data staff) do

      (if (staff-p object) 
	  (setf staff-break object)

	(if (and (metrical-bar-p object)
		 (not (eq (justification object) :left)))
	    (setf bar-break object)
	  
	  (if (meter-p object)
	      (setf meter-break object)
	    
	    (if (or (audible-p object)
		    (rest-p object))
		
		(let ((nflags (flags object))
		      (beam-beat (- (odb-beat object) beam-first-beat)))
		  ;; if beaming and non-beamable found mid-beat, break it into two (data has now a note)
		  ;; if not beaming and flags found, set beam-start and so on
		  ;; if beaming and new beat, check out beam-us 

		  (if beam-start
		      (if (or staff-break bar-break meter-break
			      (zerop nflags)
			      (beamed object)
			      (and (rest-p object) (old-style-beams score))
			      (not (stem-directions-fit score current-stem-direction (and (not (rest-p object)) (stem-direction object))))
			      (>= beam-beat current-beam-break)
			      (and (not (rest-p object)) (if (stem-tie object) (not current-stems-tied) current-stems-tied)))
			  (progn
			    
			    (if (> (count-if #'audible-p beam-us) 1)
				(let ((beamed-notes (reverse beam-us)))
				  (loop for n in beamed-notes do (setf (flagged n) :none))
				  (analyze-beam beamed-notes)
				  (setf (beamed (first beamed-notes))
				    #'(lambda (score staff stf-y0)
					(annotate-beam score nil stf-y0 beamed-notes current-beam-break)))))
			    (setf beam-start nil)
			    (setf beam-us nil))))

		  (loop while (>= beam-beat current-beam-break) do
		    (incf beam-counter)
		    (incf beam-first-beat current-beam-break)
		    (decf beam-beat current-beam-break)
		    (setf current-beam-break (if (listp beam-break)
						 (or (nth beam-counter beam-break) (first beam-break))
					       beam-break)))
		  (if (and (plusp nflags) 
			   (not (beamed object))
			   (or (not (rest-p object)) (not (old-style-beams score))))
		      (progn
			(if (not beam-start)
			    (progn
			      (setf current-stem-direction (and (not (rest-p object)) (stem-direction object)))
			      (setf current-stems-tied (stem-tie object))
			      (setf beam-start object)))
			(push object beam-us)))

		  (if meter-break
		      (progn
			(setf beat-length (beat-duration meter-break))
			(setf beats (beats-per-measure meter-break))
			(setf beam-break (or (meter-beaming meter-break)
					     (if (>= beat-length 1.0) 
						 beat-length
					       (if (= beat-length .5)
						   (if (member beats '(3 6 9 12 15 18 21)) 1.5 
						     (if (member beats '(5 7 11 13 17)) 
							 (append (make-list (1- (floor beats 2)) :initial-element 1.0) (list 1.5))
						       1.0))
						 (if (= beat-length .25)
						     (if (member beats '(3 6 9 12 15 18 21)) .75 
						       (if (member beats '(5 7 11 13 17)) 
							   (append (make-list (1- (floor beats 2)) :initial-element .5) (list .75))
							 .5))
						   (min 1.0 (* beats beat-length)))))))
			(setf current-beam-break (if (listp beam-break) (first beam-break) beam-break))			  
			(setf meter-break nil)))

		  (if staff-break
		      (setf staff-break nil))

		  (if bar-break
		      (progn
			(setf beam-counter 0)
			(setf bar-first-beat (or (odb-beat bar-break) (odb-onset bar-break) bar-first-beat))
			(setf beam-first-beat bar-first-beat)
			(setf current-beam-break (if (listp beam-break) (first beam-break) beam-break))
			(setf bar-break nil))))
	      )))))

    (if (and beam-start 
	     (> (count-if #'audible-p beam-us) 1))
	(let ((beamed-notes (reverse beam-us)))
	  (loop for n in beamed-notes do (setf (flagged n) :none))
	  (analyze-beam beamed-notes)
	  (setf (beamed (first beamed-notes))
	    #'(lambda (score staff stf-y0)
		(annotate-beam score nil stf-y0 beamed-notes (or current-beam-break 1.0))))))
    score))


(defun handle-explicit-beams (score staff)
  (let* ((stf-y0 (box-y0 staff))
	 (*staff-line-separation* (or (line-separation staff) *staff-line-separation*)))
    (loop for object in (staff-data staff) do
      (if (staff-p object) 
	  (setf stf-y0 (box-y0 object))
	(if (or (audible-p object)
		(rest-p object))
	    (if (beamed object)
		(if (function-p (beamed object))
		    (funcall (beamed object) (or (visible-section object) score) staff stf-y0))))))
    score))

(defun beamify (score)
  (map-over-staves #'handle-explicit-beams score)
  score)



;;; explicit beams

(defun collect-and-annotate-beams (score staff note beam stf-y0)
  ;; note is last note of group, beam has pointer to first note
  (let* ((first-note (tag-note beam))
	 (fpos (position first-note (staff-data staff)))
	 (lpos (position note (staff-data staff)))
	 (notes nil))
    (loop for i from fpos to lpos do
      (let ((obj (nth i (staff-data staff))))
	(if (or (audible-p obj)
		(rest-p obj))
	    (push obj notes))))
    (annotate-beam score beam stf-y0 (nreverse notes) 1.0)))

(defun begin-beam (&rest args)
  (let ((new-beam (make-instance 'beam)))
    (loop for act in args do
      (if (self-acting-p act)
	  (funcall (action act) new-beam (argument act))))
    (push new-beam beam-stack)
    (make-self-acting 
     :action #'(lambda (note beam)
		 (setf (tag-note beam) note)
		 (setf (flagged note) :none)
		 (push (list :begin-beam new-beam) (store-data note))
		 nil)
     :argument new-beam)))

(defun end-beam (&optional ur-beam)
  (let* ((beam (and ur-beam (argument ur-beam))) ;28-Aug-92
	 (old-beam (or beam
		       (pop beam-stack) 
		       (cmn-warn "end-beam without matching begin-beam"))))
    (if (and beam beam-stack) (setf beam-stack (remove beam beam-stack)))
    (make-self-acting
     :action #'(lambda (note abeam)
		 (setf (beamed (tag-note abeam)) #'(lambda (score staff stf-y0) 
						     (collect-and-annotate-beams score staff note abeam stf-y0)))
		 (setf (flagged note) :none)
		 (push (list :end-beam) (store-data note))
		 nil)
     :argument old-beam)))

(defvar begin-beam (make-self-acting
		    :action #'(lambda (note &rest rest)
				(declare (ignore rest))
				(let ((new-beam (make-instance 'beam :note note)))
				  (push new-beam beam-stack)
				  (setf (flagged note) :none)
				  (push :begin-beam (store-data note))
				  nil))
		    :argument nil))

(defvar end-beam (make-self-acting
		  :action #'(lambda (note &rest rest)
			      (declare (ignore rest))
			      (let ((beam (or (pop beam-stack) (cmn-warn "end-beam without matching begin-beam"))))
				(setf (beamed (tag-note beam)) #'(lambda (score staff stf-y0) 
								   (collect-and-annotate-beams score staff note beam stf-y0)))
				(setf (flagged note) :none)
				(push :end-beam (store-data note))
				nil))
		  :argument nil))

(defun beam- (&rest args)
  (let ((new-beam (make-instance 'beam)))
    (loop for act in args do
      (if (self-acting-p act)
	  (funcall (action act) new-beam (argument act))))
    (make-self-acting 
     :action #'(lambda (note beam)
		 (push note (tag-note beam))
		 (setf (flagged note) :none)
		 (setf (beamed note) t)
		 (push (list :beam- new-beam) (store-data note))
		 nil)
     :argument new-beam)))

(defvar beam- (make-self-acting
	       :action #'(lambda (note &rest rest)
			   (declare (ignore rest))
			   (let ((new-beam (make-instance 'beam)))
			     (push note (tag-note new-beam))
			     (setf (flagged note) :none)
			     (setf (beamed note) t)
			     (setf (argument beam-) new-beam)
			     (push (list :beam- new-beam) (store-data note))
			     nil))
	       :argument nil))

(defun -beam- (beam)
  (make-self-acting
   :action #'(lambda (note old-beam)
	       (let ((true-beam (argument old-beam)))
		 (if (not (beam-p true-beam)) 
		     (warn "-beam- got ~A in ~A on ~A" (identify true-beam) (identify old-beam) (identify note)))
		 (setf (flagged note) :none)
		 (setf (beamed note) t)
		 (push note (tag-note true-beam))
		 nil))
   :argument beam))

(defun -beam (beam)
  (make-self-acting
   :action #'(lambda (note old-beam)
	       (let ((true-beam (argument old-beam)))
		 (if (not (beam-p true-beam)) 
		     (warn "-beam- got ~A in ~A on ~A" (identify true-beam) (identify old-beam) (identify note)))
		 (push note (tag-note true-beam))
		 (setf (flagged note) :none)
		 (let ((beams (reverse (tag-note true-beam))))
		   (if (every #'onset beams)
		       (progn
			 (setf beams (sort beams #'< :key #'onset))
			 (setf (beamed (first beams))
			   #'(lambda (score staff stf-y0)
			       (annotate-beam score true-beam stf-y0 beams 1.0))))
		     (setf (beamed (first beams))
		       #'(lambda (score staff stf-y0) 
			   (if (every #'onset beams)
			       (setf beams (sort beams #'< :key #'onset)))
			   (annotate-beam score true-beam stf-y0 beams 1.0))))
		   nil)))
   :argument beam))

(defvar no-beam (make-self-acting
		 :action #'(lambda (note &rest args)
			     (declare (ignore args))
			     (push :no-beam (store-data note))
			     (setf (beamed note) #'(lambda (&rest rest)
						     (declare (ignore rest))
						     0)))
		 :argument nil))

#|
;;; some tests

(cmn (size 32) staff treble sixteenth-rest c4 e c4 s sixteenth-rest c5 e c5 s c6 s 
     sixteenth-rest sixteenth-rest c4 s c4 s sixteenth-rest sixteenth-rest c6 s
     (sixteenth-rest begin-beam) c4 e (sixteenth-rest end-beam) (sixteenth-rest begin-beam) c5 e (sixteenth-rest end-beam)
     (thirty-second-rest begin-beam) c4 e. (thirty-second-rest end-beam)
     c6 s c6 s sixteenth-rest c6 s  c5 s sixteenth-rest c5 s c5 s  c4 s c4 s sixteenth-rest c4 s  
     g3 s g3 s sixteenth-rest g3 s  c6 s c5 s sixteenth-rest c4 s  c4 s c5 s sixteenth-rest c6 s
     sixteenth-rest c4 s c4 s c4 s c4 s sixteenth-rest c4 s c4 s 
     c4 s c4 s sixteenth-rest c4 s c4 s c4 s c4 s sixteenth-rest
     c4 e. c4 s c4 e.. c4 (rq 1/8) c4 s c4 s c4 e c4 s c4 e c4 s c4 s c4 e. c4 (rq 1/8) c4 e..
     c4 e c5 e c5 e c4 e c4 s e4 s g4 s c5 s c5 s a4 s f4 s c4 s
     c4 s. (c4 (rq 1/8)) (c4 (rq 1/8)) c4 s. c4 s.. (d4 (rq 1/16)) (d4 (rq 1/16)) c4 s..)

(cmn (staff treble (chord (notes c4 c5) e (setf ib0 (beam-))) (eighth-rest)) 
     (staff bass (eighth-rest) (chord (notes g3 g2) e (-beam ib0))))

(cmn (staff treble (df5 e (onset 0) (setf ib0 (beam-))) (eighth-rest) (eighth-rest) (c5 e (onset 1.5) (-beam- ib0))) 
     (staff bass (eighth-rest) (c3 e (onset .5) (-beam- ib0)) (e3 e (onset 1.0) (-beam ib0)) (eighth-rest)))

(cmn (staff treble (df5 s (onset 0) (setf ib0 (beam-))) (sixteenth-rest) (eighth-rest) (sixteenth-rest) (c5 s (onset 1.25) (-beam- ib0))) 
     (staff bass (sixteenth-rest) (c3 s (onset .25) (-beam- ib0)) (d3 e (onset .5) (-beam- ib0)) 
	    (e3 s (onset 1.0) (-beam ib0)) (sixteenth-rest)))

(cmn (staff-separation 0) 
  (staff (staff-lines 1) (start-line 2) percussion 
    (b4 s (setf hi (beam- ))) dotted-eighth-rest) 
  (staff (staff-lines 1) (start-line 2) percussion 
    sixteenth-rest (b4 s (-beam- hi)) sixteenth-rest (b4 s (-beam- hi)))
  (staff (staff-lines 1) (start-line 2) percussion 
    eighth-rest (b4 s (-beam hi)) sixteenth-rest))

(cmn (automatic-rests nil) (staff-separation 0)
     (staff (staff-lines 1) (start-line 2) percussion 
	    (sixteenth-rest (setf hi (beam-))) (eighth-rest invisible) (setf mc (b4 s)))
     (staff (staff-lines 1) (start-line 2) percussion 
	    (b4 s. (onset .25) (-beam- hi)) (b4 s (-beam- hi) (tied-to mc) (onset .75)))
     (staff (staff-lines 1) (start-line 2) percussion 
	    (b4 (rq 1/8) (onset .625) (-beam hi)) (sixteenth-rest (scale 0 0)) bar))

(cmn staff treble (chord e (notes g4 g5)) (chord e (notes a4 as4)) (chord e (notes c4 e4)) (chord e (notes d5 ds5)))

(cmn (size 32) staff treble 
     (meter 2 4) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 5 16) c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s 
     (meter 2 4) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 3 8) c4 e c4 e c4 e               c4 e c4 e c4 e 
     (meter 7 8) c4 e c4 e c4 e c4 e c4 e c4 e c4 e     c4 e c4 e c4 e c4 e c4 e c4 e c4 e 
     (meter 7 16) c4 s c4 s c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s c4 s c4 s 
     (meter 1 4) c4 e c4 e      c4 e c4 e 
     (meter 13 8) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e)

(cmn (size 24) staff treble 
     (meter 2 4 (beaming 2)) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 5 16 (beaming (list .75 .5))) c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s 
     (meter 2 4 (beaming (list .5 1.5))) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 3 8 (beaming (list 1.0 .5))) c4 e c4 e c4 e               c4 e c4 e c4 e 
     (meter 7 8 (beaming (list 1.5 2.0))) c4 e c4 e c4 e c4 e c4 e c4 e c4 e     c4 e c4 e c4 e c4 e c4 e c4 e c4 e 
     (meter 7 16 (beaming (list .75 1.0))) c4 s c4 s c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s c4 s c4 s 
     (meter 1 4 (beaming .5)) c4 e c4 e      c4 e c4 e 
     (meter 13 8 (beaming (list 4.0 2.5))) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e      
     (meter 19 8 (beaming (list .5 5.0 1.5 2.5))) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e)

(cmn (staff treble (chord e (notes g4 g5)) (chord e (notes g4 a4 c5 e4)) (chord e (notes c4 e4)) (chord e (notes d5 d5))) 
     (staff bass c3 e c3 e c3 e c3 e))
(cmn (size 60) staff treble (chord e (notes g4 g5)) (chord e (notes gs4 a4 d5)) (chord e (notes g4 g5)) (chord e (notes fs4 cs5 d5)) )
(cmn staff treble (chord s (notes d5 ds5 fs4)) (chord s (notes fs5 ds5 a4)))
(cmn staff treble (chord s (notes fs4 cs5 c5)) (chord s (notes cs5 f5 d5)) (chord s (notes ds5 fs4 c5)) (chord s (notes ds5 d5 cs5))
      (chord s (notes fs4 f5 c5)) (chord s (notes c5 cs5 f5)) (chord s (notes d5 ds5 fs4)) (chord s (notes fs5 ds5 a4)))

(cmn staff treble (meter 3 4) (c4 s begin-beam) c4 e. c4 s c4 e. c4 s (c4 e. end-beam))
(cmn staff treble (c4 e. begin-beam) c4 s c4 e.. (c4 (rq 1/8) end-beam) 
                  (c5 e. begin-beam) c5 s c5 e.. (c5 (rq 1/8) end-beam) 
                  (c4 (rq 1/8) begin-beam) c4 e.. c4 e. (c4 s end-beam) 
		  (c5 (rq 1/8) begin-beam) c5 e.. c5 s (c5 e. end-beam) )
(cmn staff treble (c4 e. begin-beam) c4 s c4 e c4 s c4 (rq 1/8) (c4 (rq 1/8) end-beam) 
                  (c5 e. begin-beam) c5 s c5 e c5 s c5 (rq 1/8) (c5 (rq 1/8) end-beam) 
                  (c4 (rq 1/8) begin-beam) c4 (rq 1/8) c4 e c4 s c4 s c4 e (c4 s end-beam) 
		  (c5 (rq 1/8) begin-beam) c5 e c5 s. c5 s c5 s. (c5 s. end-beam) )
(cmn staff treble (meter 4 4) c4 e c4 q sixteenth-rest c4 s c4 s c4 e c4 q c4 e)

|#


(defun identify-store-data (note)
  (if (store-data note)
      (format nil "~{ ~A~}" 
	      (loop for datum in (reverse (store-data note))
	       collect (if (not (listp datum))
			   (if (not (member datum '(:line-break :unmetered)))
			       (format nil "~(~A~)" datum)
			     "")
			 (if (eq (first datum) :begin-beam)
			     (format nil "(~(~A~)~A)" (first datum) (identify-visible (second datum)))
			   (if (member (first datum) '(:end-beam :end-slur))
			       (format nil "(~(~A~))" (first datum))
			     (if (eq (first datum) :begin-slur)
				 (format nil "(begin-slur~A)" (identify-slur (second datum)))
			       (if (member (first datum) '(:beam- :slur-))
				   ;; collect the current beam- with setf,
				   ;; and add tags for all the rest.
				   (let* ((tag (second datum))
					  (beam (eq (first datum) :beam-))
					  (name (if beam "beam" "slur"))
					  (notes (nreverse (tag-note tag)))
					  (tag-name (new-cmn-store-tag (if beam "beam-" "slur-"))))
				     (loop for note in (cdr (butlast notes)) do
				       (add-to-cmn-store-tags note (format nil "(-~A- ~A)" name tag-name)))
				     (add-to-cmn-store-tags (first (last notes)) (format nil "(-~A ~A)" name tag-name))
				     (format nil "(setf ~A (~A-~A))" 
					     tag-name name 
					     (if (not beam) (identify-slur tag) (identify-visible tag))))
				 (if (eq (first datum) :tied-to)
				     (format nil "(tied-to ~A)" (second datum))
				   ""))))))))

    ""))
