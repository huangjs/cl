;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; examples of various random things
;;;
;;;   add a stem-down
;;;   whole rests
;;;   the text x and y fields
;;;   drum beats
;;;   staccato above the beam
;;;   extensions for all-stems-up, or all-eighths, etc
;;;   silly beams and explicit beam positioning
;;;   alternate rhythm names
;;;   slurs going off into space (invisible notes)
;;;   pizzicato entity
;;;   gray-scale example
;;;   how to pass parameters to a user-defined graphics entity
;;;   how to apply some attribute to everything on a staff
;;;   arrows between objects (uses filter-hook)
;;;   page numbers (uses page-hook) and line separation marks (uses line-hook), copyright notice
;;;   lining up onsets across staves and setting the onset of a tied-to staff automatically
;;;   glissando across a clef change
;;;   justification abbreviations
;;;   changing margins without affecting justification decisions
;;;   multi-note tremolos
;;;   doubled unison
;;;   crescendo hairpin with text inside
;;;   clusters
;;;   beaming across multiple percussion clefs
;;;   frames (a simple example)
;;;   put a circle around a note
;;;   non-horizontal crescendo wedge
;;;   chords across many staves
;;;   beat subdivision brackets with ends in opposite directions
;;;   what's in the <mumble> font?
;;;   how to make a clef that (un)transposes
;;;   beat message example
;;;   automatic beaming choices (the meter beaming message)
;;;   note with accidental in parentheses
;;;   make unjustified the default for dynamics and text
;;;   dx/dy specializations to measure-rest number
;;;   how to make room on the first page for a title
;;;   beams on unisons (two styles)
;;;   different note-head sizes
;;;   global customizations of various sorts
;;;   figured bass
;;;   personalized cmn
;;;   guitar tablature notation ("fretboard" or fingerboard diagrams)
;;;   brace connecting two lines of lyrics
;;;   MIDI note numbers in CMN
;;;   patterns and colors
;;;   multi-colored staff lines (red-line in middle etc)
;;;   clm note list to cmn at a lower level than described in cmn.html
;;;   evenly spaced measures (simple spacing-hook example)
;;;   sharp or flat sign embedded in text

;;; ------------------------------------------------
;;; add a stem down and make the note head a half-note head (carl.cmn)
;;; stem up would be the same thing, but placed at (+ (x0 note) .2875) and going up .75, not down .75

(setf with-stem-down-too 
    (make-self-acting
	:action #'(lambda (note &rest rest)
		    (declare (ignore rest))
		    (setf (head-quarters note) 2)
		    (add-to-marks 
		     note 
		     (list (make-instance 'sundry
			       :name :with-stem-down-too
			       :mark #'(lambda (mark note score &optional justifying)
					 (declare (ignore justifying))
					 (moveto score (x0 note) (y0 note))
					 (setf (line-width score) *stem-width*)
					 (rlineto score 0 -.75)
					 (draw score)
					 (setf (line-width score) 0))))))
	:argument nil))


;;; ------------------------------------------------
;;; send out NUM whole-rests (can be scaled by 0 0 if invisible rests are needed for an attached staff) (joh.cmn)

(defun whole-rests (num)
  (engorge (loop for i from 1 to num collect whole-rest collect bar)))


;;; similarly to make a staff with empty meaures:

(defun spc (n) (rest n invisible))
(cmn treble (meter 4 4) (engorge (loop for i from 0 to 5 collect (spc w) collect bar)))



;;; ------------------------------------------------
;;; the X and Y fields in the text class (joh.cmn)

(defun staff-text- (&rest args)
  (apply #'text- (connecting-pattern '(7 20))
	 (font-name "Times-Italic") (font-scaler .4)
	 (y #'(lambda (mark note score &optional justifying)
		(+ (staff-y0 note) 1.0 (dy mark))))
	 (x #'(lambda (mark note score &optional justifying)
		(+ (x0 note) (dx mark) (if (sign note) .2 0))))
	 args))


;;; ------------------------------------------------
;;; use auxiliary-note to create a drum beat in small notes

(defun ax (&rest args) 
  (apply #'auxiliary-note no-beam args))

(defun axes (&rest args) 
  (let* ((xdif .5) 
	 (xtotal (+ .25 (* xdif (length args)))))
    (engorge 
     (loop for arg in args and i from xtotal downto 0 by xdif 
      collect (ax arg e (dx (- i)))))))

(cmn staff treble (e5 q (axes d4 d4 d4 d4)))


;;; ------------------------------------------------
;;; staccato placed above the beam (as in mir.cmn)

(defun display-beam-staccato (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y0 (+ (staff-y0 note) .25 (y1 note))))
    (cmn-dot score
	    (+ (box-x0 note) (box-x0 mark) (vis-dx mark) (center note) -.05)
	    (+ y0 (box-y0 mark) (vis-dy mark)))))

(defvar beam-staccato (make-instance 'write-protected-sundry :name :beam-staccato :mark #'display-beam-staccato))
(defun beam-staccato (&rest objects) (apply #'mark #'display-beam-staccato :beam-staccato objects))



;;; ------------------------------------------------
;;; with-stem-up

(defun with-stem-up (&rest args) 
  (engorge 
   (loop for arg in args 
    collect (if (or (note-p arg) 
		    (chord-p arg)) 
		(progn 
		  (setf (stem-direction arg) :up) 
		  arg) 
	      arg))))

;;; (cmn staff treble c5 q (with-stem-up (c5 q) (c4 q)) c5 q)


;;; or similarly:

(defun with-eighths (&rest args) 
  (engorge (loop for arg in args collect (if (note-p arg) (apply #'note arg e nil) arg))))

;;; (cmn staff treble (with-eighths c4 e4 f4 g4))



;;; ------------------------------------------------
;;; silly beams
;;;
;;; (this explicit beaming stuff can be useful if cmn screws up)

(cmn staff treble (c4 e. (begin-beam (explicit-beams (list (list 1 1 0) (list 0 1 0) (list 0 0 0))))) 
     (c4 s) (c4 s end-beam))

;;; that is, the various beam makers take the additional message explicit-beams.
;;; the argument to explicit-beams is a list of three lists, each list having a
;;; number for each note under the beam, and each number referring to (respectively)
;;;
;;;  the number of full beams going to the right from the current note
;;;  the number of partial beams heading right from the current note
;;;  the number of partial beams heading left from the current note
;;;
;;; so ((1 1 0) (0 1 0) (0 0 0)) => 1 full beam across the top, and one partial beam
;;; heading toward the right from the second note.

(cmn staff treble (c4 e. (begin-beam (dy 1) (explicit-beams (list (list 1 1 0) (list 5 1 0) (list 0 0 3))))) 
     (c4 s) (c4 s end-beam))


;;; another example:
(cmn staff treble (meter 2 4)
     (b5 (rq 1/6) (tremolo (tremolo-slashes 1)) 
	(begin-beam (explicit-beams (list (list 2 2 1 2 2 0) (list 0 0 0 0 0 0) (list 0 0 0 0 0 0)))))
     (b5 (rq 1/6) (tremolo (tremolo-slashes 1)))
     (b5 (rq 1/6) (tremolo (tremolo-slashes 1)))

     (b5 (rq 1/6) (tremolo (tremolo-slashes 1)))
     (b5 (rq 1/6) (tremolo (tremolo-slashes 1)))
     (b5 (rq 1/6) (tremolo (tremolo-slashes 1)) 
	(end-beam))
    )



;;; explicit beam positioning follows the pattern of other cmn entities -- use
;;; dx and dy to move the beam as a unit, and dx0 dx1 dy0 dy1 to move just the
;;; end-points.  Beam-direction can be :up or :down.

(cmn staff treble (c4 e (begin-beam (beam-direction :down) (dy0 -.5))) (d4 e end-beam))



;;; ------------------------------------------------
;;; other rhythm names
;;;
;;; (these really should be write-protected in the defvar case, and same-name functions should be provided)

(defvar 8th (rq 1/2))
(defvar 16th (rq 1/4))
(defvar 32nd (rq 1/8))
(defvar 64th (rq 1/16))
(defvar 128th (rq 1/32))

;;; see also rests.lisp


;;; ------------------------------------------------
;;; slurs going off into emptiness (ringing sounds and so on)
;;;  this has now been automated -- see ring.lisp
;;;
(cmn staff treble (g5 w begin-slur) (a5 w (scale 0 0) end-slur) bass (cs2 w begin-slur (scale 0 0)) (cs2 w end-slur))



;;; ------------------------------------------------
;;; pizzicato (other such names could be handled similarly)
;;;

(defun display-pizzicato (mark note score &optional justifying)
  (declare (ignore justifying))
  (show score (text "pizz." (font-name "Times-Italic") (font-scaler .5))
	:matrix (translate-matrix score mark
				  (- (+ (box-x0 note) (vis-dx note) (vis-dx mark)) .25)
				  (- (+ (staff-y0 note) (vis-dy mark)) .5))))

(defvar pizzicato (make-instance 'write-protected-sundry :name :pizzicato :mark #'display-pizzicato))
(defun pizzicato (&rest objects) (apply #'mark #'display-pizzicato :pizzicato objects))



;;; ------------------------------------------------
;;; fun with gray-scale
;;;
(cmn (free-expansion-factor 2.0) staff (treble (gray-scale .8)) 
  (g5 (note-head :square) w (gray-scale .7) (begin-slur (gray-scale .6))) (g5 (scale 0 0) end-slur w) 
  (g5 (note-head :square) w (gray-scale .5) (begin-slur (gray-scale .4))) (g5 (scale 0 0) end-slur w) 
  (g5 (note-head :square) w (gray-scale .3) (begin-slur (gray-scale .2))) (g5 (scale 0 0) end-slur w) 
  (g5 (note-head :square) w (gray-scale .1) (begin-slur)) (g5 (scale 0 0) end-slur w) 
  bass (c2 (scale 0 0) (begin-tie (dashed '(.2 .1)))) (cs2 w end-tie))



;;; ------------------------------------------------
;;; passing parameters to a user-supplied mark
;;;
;;; (cmn staff treble c4 q wind-block) 
;;; (cmn staff treble (c4 q (wind-block 4 1)))

(defun display-wind-block (mark note score &optional (width .5) (height 1.0))
  (let* ((y-off (+ (staff-y0 note) (vis-dy mark)))
	 (x-off (+ (x0 note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (moveto score 0 0)
    (lineto score 0 height)
    (lineto score width height)
    (lineto score width 0)
    (lineto score 0 0)
    (fill-in score)
    (matrix-back score)))

(defun wind-block (width height &rest objects)
  (apply #'mark #'(lambda (mark note score &optional justifying)
		  (declare (ignore justifying))
		  (display-wind-block mark note score width height))
	 'wind-block
	 objects))

(defvar wind-block (wind-block .4 1))


;;; ------------------------------------------------
;;; applying some attribute to every note on a staff
;;;
;;; (cmn staff always-stem-down treble c4 q)

(defvar always-stem-down 
    (make-self-acting  
     :action #'(lambda (staff &rest rest) 
		 (loop for obj in (staff-data staff) do 
		   (if (or (note-p obj) (chord-p obj)) 
		       (setf (stem-direction obj) :down)))) 
     :argument nil))

(setf always-tie-up
    (make-self-acting 
     :action #'(lambda (staff &rest rest)
		 (loop for obj in (staff-data staff) do 
		   (if (or (note-p obj) (rest-p obj) (chord-p obj)) 
		       (setf (tie-direction obj) :up)))) 
     :argument nil))

(setf always-slur-up
    (make-self-acting 
     :action #'(lambda (staff &rest rest)
		 (loop for obj in (staff-data staff) do 
		   (if (or (note-p obj) (chord-p obj)) 
		       (setf (slur-direction obj) :up)))) 
     :argument nil))

(defvar always-no-beam
    (make-self-acting  
     :action #'(lambda (staff &rest rest) 
		 (declare (ignore rest))
		 (loop for obj in (staff-data staff) do 
		   (if (or (note-p obj) (chord-p obj)) 
		       (progn
			 (push :no-beam (store-data obj))
			 (setf (beamed obj) #'(lambda (&rest rest)
						(declare (ignore rest))
						0))))))
     :argument nil))




;;; ------------------------------------------------
;;; arrows pointing to the current object from some other saved object
;;;
;;; uses filter-hook called in filterify, just for lafs.
;;; can set arrow color, dashed-style, arrowhead length and angle,
;;;   and the distances we fall back from the two objects.


(defvar filter-hook-progn nil)

(defun arrow-dashed (arg) (list :arrow-dashed arg))
(defun arrow-begin-space (arg) (list :arrow-begin-space arg))
(defun arrow-end-space (arg) (list :arrow-end-space arg))
(defun arrowhead-length (arg) (list :arrowhead-length arg))
(defun arrowhead-angle (arg) (list :arrowhead-angle arg))
(defun arrow-color (arg) (list :arrow-color arg))

(defun arrow-from (start-obj &rest args)
  ;; obj is the object where the arrow starts
  ;; args can be arrow-dashed, arrow-begin-space, arrow-end-space, arrow-color, arrowhead-length, arrowhead-angle
  ;; we'll handle these as simple functions, rather than define a new arrowhead class and use self-acting
  (if (not filter-hook)
      (setf filter-hook #'(lambda (score)
			    (let ((saved-progn filter-hook-progn))
			      (setf filter-hook-progn nil)
			      (loop for fun in saved-progn do
				(funcall fun score))
			      score))))
  (make-self-acting 
   :action 
   #'(lambda (end-obj &rest rest)
       (push 
	#'(lambda (score) 
	    (add-to-marks		;this extra indirection needed to make sure we end up on the right page
	     end-obj 
	     (list 
	      (make-instance 'sundry 
		  :name :arrow-from 
		  :mark #'(lambda (mark note score &rest rest)
			    (let* ((ox0 (+ (x0 start-obj) (center start-obj) (vis-dx start-obj)))
				   (oy0 (y0 start-obj))
				   (ox1 (+ (x0 end-obj) (center end-obj) (vis-dx end-obj)))
				   (oy1 (y0 end-obj))
				   (theta (if (= oy1 oy0) 0.0
					    (if (= ox1 ox0) 
						(* pi -.5) 
					      (atan (- oy1 oy0) (- ox1 ox0)))))
				   (arrow-dashed nil)
				   (arrow-fatness (/ pi 8))
				   (arrow-length .4)
				   (arrow-color nil)
				   (arrow-end-space .5)
				   (arrow-begin-space .25))
			      (loop for arg in args do
				(case (first arg)
				  (:arrow-dashed (setf arrow-dashed (second arg)))
				  (:arrow-begin-space (setf arrow-begin-space (second arg)))
				  (:arrow-end-space (setf arrow-end-space (second arg)))
				  (:arrowhead-length (setf arrow-length (second arg)))
				  (:arrowhead-angle (setf arrow-fatness (second arg)))
				  (:arrow-color (setf arrow-color (second arg)))))
			      (let* ((top-theta (+ theta arrow-fatness))
				     (bottom-theta (- theta arrow-fatness))
				     (x0 (coerce (+ ox0 (* arrow-begin-space (cos theta))) 'single-float))
				     (x1 (coerce (- ox1 (* arrow-end-space (cos theta))) 'single-float))
				     (top-x (coerce (- x1 (* arrow-length (cos top-theta))) 'single-float))
				     (bottom-x (coerce (- x1 (* arrow-length (cos bottom-theta))) 'single-float))
				     (y0 (coerce (+ oy0 (* arrow-begin-space (sin theta))) 'single-float))
				     (y1 (coerce (- oy1 (* arrow-end-space (sin theta))) 'single-float))
				     (top-y (coerce (- y1 (* arrow-length (sin top-theta))) 'single-float))
				     (bottom-y (coerce (- y1 (* arrow-length (sin bottom-theta))) 'single-float))
				     (back-x (coerce (- x1 (* .5 arrow-length (cos theta))) 'single-float))
				     (back-y (coerce (- y1 (* .5 arrow-length (sin theta))) 'single-float))
				     (old-color (color score)))
				(comment score "arrow")
				(if arrow-color (setf (color score) arrow-color))
				(moveto score x0 y0)
				;(lineto score x1 y1 :pattern (if arrow-dashed (if (not (listp arrow-dashed)) (list 5 10) arrow-dashed)))
				(lineto score back-x back-y :pattern (if arrow-dashed (if (not (listp arrow-dashed)) (list 5 10) arrow-dashed)))
				;; keep arrow shaft from going too far if arrow is thick (AV)
				(draw score)
				(moveto score x1 y1)
				(lineto score top-x top-y)
				(lineto score back-x back-y)
				(lineto score bottom-x bottom-y)
				(lineto score x1 y1)
				(fill-in score)
				(if arrow-color (setf (color score) old-color))
				)))))))
	filter-hook-progn))
   :argument nil))

#|
(cmn (staff treble c4 q (setf bigc (c4 w)) (c4 q) line-break d4 e d4 e d4 q (arrow-from bigc (arrow-dashed '(10 16)))))
(cmn (staff treble c4 q (setf bigc (c4 w)) (c4 q) line-break d4 e d4 e d4 q 
       (arrow-from bigc (arrowhead-angle (/ pi 4))(arrowhead-length 1.0) (arrow-color '(1 0 0)) (arrow-dashed '(10 13)))))
(cmn (staff treble c4 q (setf bigc (c4 w)) (c5 q stem-up) b4 e a4 e g4 q 
      (arrow-from bigc (arrow-dashed '(2 4)) (arrowhead-length .3) (arrow-begin-space .4) 
      (arrow-end-space .25) (arrowhead-angle (/ pi 12)))))
|#


;;; ------------------------------------------------
;;; Page numbers 
;;;
;;; There's a variable called page-hook that is funcall'd
;;; on each page.  It's intended for page-numbers, borders,
;;; and so on.  Here's an example:

(defun page-number (score page)
  ;; called automatically if placed as value of page-hook
  ;; dimensions of current page are in score variables page-width, page-height,
  ;;   left/right/header/footer-margin (in inches), "page" above is the current page number
  (show score (text (format nil "-~D-" page)  (font-scaler .5))
	:matrix (list 1 0 0 1
		      (* (scr-size score) (in-inches (+ (right-margin score) (* .5 (page-width score)) -.5)))
		      (* (scr-size score) (in-inches (* .75 (footer-margin score)))))))


;;; To use this, 
;;;
;;; (setf page-hook #'page-number)
;;;
;;; then call up cmn as usual.
;;;
;;; (setf page-hook nil) 
;;;
;;; to cancel it.
;;;
;;; Now say we want page numbers on all but the first page where we want a copyright notice
;;; instead.  The circled-C symbol is at #o240 in the Times-Roman font, at #o251 in the ISO
;;; encoded version of it:

(defun page-number-with-copyright-notice (score page)
  (if (> page 1)
      (show score (text (format nil "-~D-" page)  (font-scaler .5))
	    :matrix (list 1 0 0 1
			  (* (scr-size score) (in-inches (+ (right-margin score) (* .5 (page-width score)) -.5)))
			  (* (scr-size score) (in-inches (* .75 (footer-margin score))))))
    (let ((left-side (in-inches (+ (right-margin score) (* .5 (page-width score)) -2)))
	  (top-side (in-inches (* .875 (footer-margin score)))))
      (show score (text "\\240 1234 by Roger Bacon"  (font-scaler .5))
	    :matrix (list 1 0 0 1 (* (scr-size score) left-side) (* (scr-size score) top-side)))
      ;; the double backslash is the easiest way to embed arbitrary characters in the text string.
      ;; the three digit number following the slashes is the octal code of the special character we want.
      (show score (text "ALL RIGHTS RESERVED" (font-scaler .5))
	    :matrix (list 1 0 0 1 (* (scr-size score) left-side) (* (scr-size score) (- top-side .625)))))))


;;; or say we want the Author's name in the upper left corner, and the date in the
;;; upper right corner on every page:

(defvar author nil)
(defvar attribution nil)

(defun page-with-author-and-whatnot (score page)
  (let ((left-side (in-inches (+ (left-margin score) .5)))
	(top-side (in-inches (- (page-height score) .5)))
	;; or use the staff-relative calculation (which works better for partial pages)
	;; (top-side (+ 2.0 (box-y0 (find-if #'staff-p (staff-data (first (staves (first (systems score)))))))))
	(right-side (in-inches (- (page-width score) (right-margin score)))))
    (show score (text author  (font-scaler .5))
	  :matrix (list 1 0 0 1 (* (scr-size score) left-side) (* (scr-size score) top-side)))
    (show score (text attribution (font-scaler .5))
	  :matrix (list 1 0 0 1 (* (scr-size score) (- right-side (* .4 (length attribution))))	(* (scr-size score) top-side)))))

(defun cmna (auth attr &rest args)
  (setf author auth)
  (setf attribution attr)
  (apply #'cmn (page-hook #'page-with-author-and-whatnot) args))

;;; now cmna replaces cmn: (cmna "Roger Bacon" "1234?" staff treble c4 q)




;;; ------------------------------------------------
;;; line marks
;;;
;;; Similar to page-hook is the variable line-hook -- its value (a function) is funcall'd
;;; on each line.  The arguments passed to that function are:
;;;
;;;   (score top-staff current-line-number y1 x0 x1 data-x0 staff-size)
;;;
;;; where the actual musical data goes from data-x0 to x1, starting vertically at y1.
;;; Current-line-number is the number of this line within the current page (starting at 1).
;;; 
;;; Here, we'll cause each line after the first on the page to add the // marks that are
;;; sometimes used to emphasize the separation between successive lines of a multi-system score.

(defun separate-lines (score staff line sy1 sx0 sx1 sdx0 ssize)
  (when (> line 1)			;i.e. no need for these marks at the top of the page
    (add-to-marks 
     staff 
     (list (make-instance 'sundry 
	     :name 'line-separation
	     :mark #'(lambda (mark note score &rest rest)
		       (let* ((fx (/ 1.0 ssize))
			      (fatness (* fx .125))
			      (length (* fx .5))
			      (slant (* fx .25))
			      (ysep (+ fatness (* fx .075)))
			      (x0 (* fx sdx0))
			      (y1 (* fx sy1))
			      (y0 (+ y1 (* fx .5 (line-separation score)))))
			 (moveto score x0 y0)
			 (lineto score (+ x0 length) (+ y0 slant))
			 (lineto score (+ x0 length) (+ y0 slant fatness))
			 (lineto score x0 (+ y0 fatness))
			 (lineto score x0 y0)
			 (fill-in score)
			 (moveto score x0 (- y0 ysep))
			 (lineto score (+ x0 length) (+ y0 slant (- ysep)))
			 (lineto score (+ x0 length) (+ y0 slant (- ysep) fatness))
			 (lineto score x0 (+ y0 (- ysep) fatness))
			 (lineto score x0 (- y0 ysep))
			 (fill-in score))))))))


;;; if you only want this function applied to one score, use the score slot line-hook, 
;;; rather than the global variable:
;;;
;;;   (cmn (line-hook #'separate-lines) treble c4 q c4 q c4 q line-mark c4 q c4 q c4 q)
;;;
;;; Page-hook is also available as a score slot.

;;; Here's an example of adding some text to the first rest on the bottom staff
;;; of each line (where we know in advance there will be such a rest):

(defvar linectr 1)
(defun lh (score staff line sy1 sx0 sx1 sdx0 ssize)
  (let* ((current-line-onset (if (= linectr 1) 0 (onset (nth (- linectr 2) (time-lines score)))))
	 (bottom-staff (first (last (staves (first (last (systems score)))))))
	 (first-rest-on-bottom-staff 
	  (find-if #'(lambda (obj) 
		       (and (rest-p obj) 
			    (= (onset obj) current-line-onset))) 
		   (staff-data bottom-staff))))
    (incf linectr)
    (add-to-marks first-rest-on-bottom-staff (list (text "hiho" (dy -1.0))))))

;;; Then in the cmn call
;;;
;;;    (cmn (progn (setf linectr 1) nil) ...)
;;;
;;; We don't use the "line" argument here because it is page-relative.


;;; ------------------------------------------------
;;; lining up notes across staves
;;;
;;; it can be a pain to figure out what a note's onset should be
;;; at the start of a "tied-to" staff.  To make this a little simpler,
;;; the "onset" message can take as its argument a function of no
;;; arguments that returns the onset when evaluated.  Then we save
;;; a pointer to the note or object that marks the start of the objects
;;; on the tied-to staff and return that object's onset as the first
;;; onset of the tied-to staff data:

(cmn (setf s1 (staff treble (meter 6 8) 
		     a4 e a4 e (setf ho (a4 e)) a4 e a4 e a4 e)) 
     (staff (tied-to s1) 
	    (d4 q. (onset #'(lambda ()
			      (onset ho))) 
		stem-down) 
	    (d4 e stem-down)))

;;; You don't need to prepend invisible rests to the tied staff's data --
;;; if cmn finds a tied staff starting at some arbitrary time, it automatically
;;; acts as though the necessary rests were there.

;;; The onset function should return a number.  If this construct is used
;;; in the midst of a staff data list, the function is evaluated, and the onset
;;; is set accordingly, just as if it had been set explicitly to that value.
;;; (I need a function passed in as the onset value so I can defer evaluation
;;; until the somewhat arbitrary moment when I'm ready to deal with the onset).



;;; ------------------------------------------------
;;; begin/end-glissando and glissando-to
;;;
;;; If there's a clef change between the begin-glissando and the end-glissando,
;;;   cmn still tries to match up the actual notes.  There are cases where this
;;;   looks strange, but these cases can use glissando-to:

(cmn staff treble (c4 q begin-glissando) bass (g3 q end-glissando))
(cmn staff treble (c4 q (glissando-to g3)) bass (g3 q))



;;; ------------------------------------------------
;;; justification abbreviations
;;;
(defvar below (justification :below))
(defvar above (justification :above))
(defvar up (justification :up))
(defvar down (justification :down))



;;; ------------------------------------------------
;;;   changing margins without affecting justification decisions
;;;

(setf new-margin 2.0)

(cmn (size (round (* 24 (/ (- 8.5 (* 2 new-margin)) 7.5))))
     ;;              ^ old size    ^ fix-up for changed margin width
     (left-margin new-margin)
     (right-margin new-margin)
     ...)


;;; ------------------------------------------------
;;; multi-note tremolos
;;;

(cmn staff treble (c4 s (note-head :tremolo)) (d4 s (note-head :tremolo)) (e4 s (note-head :tremolo)))


;;; ------------------------------------------------
;;; double-unison

(cmn (chord (notes g4 g4) q))


;;; ------------------------------------------------
;;;   crescendo hairpin with text inside

(cmn (free-expansion-factor 3) (size 48) 
  (g4 h (crescendo (width .5) (end-dynamic (text "poco" (dx -.75) (dy .05) (font-scaler .3) (font-name "Times-Italic"))))) 
  g4 h bar)


;;; ------------------------------------------------
;;; clusters
;;;
;;; There are a zillion ways to draw clusters.  Here's one:


(defun display-cluster (mark chord score &optional justifying)
  ;; can't be implemented as a stem-mark because the cluster might not have a stem.
  (when (and (or (not justifying)
		 (not (eq (visible-justification chord) :none)))
	     (not (invisible-p chord)))
    (let* ((x0 (+ (x0 chord) (vis-dx mark) (center chord)))
	   (low-line (minimum-line chord))
	   (high-line (maximum-line chord))
	   (low-y0 (+ (staff-y0 chord) (* *staff-line-separation* low-line)))
	   (high-y0 (+ (staff-y0 chord) (* *staff-line-separation* high-line)))
	   (filled (< (quarters chord) 2))
	   (stemmed (< (quarters chord) 4))
	   (stemright (and stemmed (member (stem-direction chord) '(:up :up?)))))
      (if filled
	  (progn
	    (moveto score (+ x0 (if stemright .15 -.15)) low-y0)
	    (lineto score (+ x0 (if stemright -.1 .1)) low-y0)
	    (lineto score (+ x0 (if stemright -.1 .1)) high-y0)
	    (lineto score (+ x0 (if stemright .15 -.15)) high-y0)
	    (lineto score (+ x0 (if stemright .15 -.15)) low-y0)
	    (fill-in score))
	(progn
	  (setf (line-width score) .04)
	  (moveto score (+ x0 .125) (+ low-y0 .05))
	  (rlineto score 0 (- high-y0 low-y0 .15))
	  (rmoveto score -.25 0)
	  (rlineto score 0 (- (- high-y0 low-y0 .15)))
	  (draw score))))))

(defvar cluster (make-instance 'write-protected-sundry :name :cluster :mark #'display-cluster))
(defun cluster (&rest objects) (apply #'mark #'display-cluster :cluster objects))

;;; (cmn (size 100) (chord (notes c4 c5) q (cluster)) (chord (notes c4 c5) w (cluster)) (chord (notes g4 g5) q (cluster)))



;;; ------------------------------------------------
;;;   beaming across multiple percussion clefs
;;;

(cmn (automatic-rests nil) (staff-separation 0)
  (staff (staff-lines 1) (start-line 2) percussion 
    (sixteenth-rest (setf hi (beam-))) (eighth-rest invisible) (setf mc (b4 s)))
  (staff (staff-lines 1) (start-line 2) percussion 
    (b4 s. (onset .25) (-beam- hi)) (b4 s (-beam- hi) (tied-to mc) (onset .75)))
  (staff (staff-lines 1) (start-line 2) percussion 
    (b4 (rq 1/8) (onset .625) (-beam hi)) (sixteenth-rest (scale 0 0)) bar))


;;; ------------------------------------------------
;;; frames
;;;
;;; this could be extended to add arrows and so on.


(defclass frame (tag-mixin sundry)
  ((width :initform .025 :initarg :width :accessor width)))

(defun frame- (&rest args)
  (let ((nf (make-instance 'frame)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) nf (argument arg))))
    (make-self-acting 
     :action #'(lambda (note newf) 
		 (setf (tag-note newf) note) 
		 nil)
     :argument nf)))

(defun -frame (oldf &rest args)
  (let ((newf (make-instance 'frame)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) newf (argument arg))))
    (make-self-acting
     :action #'(lambda (note old-frame)
		 (add-to-marks note
			       (list (make-sundry
				      :name :frame
				      :mark #'(lambda (mark lnote score &optional justifying)
						(when (not justifying)
						  (let* ((first-note (tag-note old-frame))
							 (x0 (+ (min (box-x0 first-note) (box-x0 lnote)) (vis-dx old-frame) -.1))
							 (x1 (+ (max (box-x0 first-note) (box-x0 lnote)) (vis-dx newf) .4))
							 (y0 (+ (min (box-y0 first-note) (box-y0 lnote)) (vis-dy newf) -.2))
							 (y1 (+ (max (box-y0 first-note) (box-y0 lnote)) (vis-dy old-frame) .4)))
						    (setf (line-width score) (width old-frame))
						    (moveto score x0 y0)
						    (lineto score x0 y1)
						    (lineto score x1 y1)
						    (lineto score x1 y0)
						    (lineto score (- x0 (* .5 (width old-frame))) y0)
						    (draw score)
						    (setf (line-width score) 0))))))))
     :argument (argument oldf))))

;;; (cmn staff treble (c4 q (setf fr (frame- (width .1) (dy .5)))) d4 q e4 q (f4 q (-frame fr (dx .1) (dy -.25))))


;;; ------------------------------------------------
;;; put a circle around a note
;;;
;;;   (see ccarh93.cmn)

(defun circled (&rest args)
  (apply #'mark #'(lambda (mark note score &optional justifying)
		    (when (not justifying)
		      (with-thickness score mark .025
		        (circle score 
			      (+ (x0 note) (dx mark) (dx note) (center note) (if (sign note) -.05 0))
			      (+ (staff-y0 note) (dy mark) (* *staff-line-separation* (head-line note)))
			      (if (not (sign note)) .5 .6))
		        (draw score))))
	 :circled args))

(defvar circled (circled))



;;; ------------------------------------------------
;;; non-horizontal crescendo
;;;
;;; the easiest way to get this mark is to use the rotate message:

(cmn staff treble (c4 q (diminuendo (rotate 25) (dy .4) (dx1 .25))) d4 q)


;;; ------------------------------------------------
;;; chords across many staves
;;;
;;; here we use the tied-to message as it applies to notes and chords.
;;;  to leave out one staff, put a dummy note there with :none as its notehead.

(cmn (staff treble (setf mc (c4 e stem-up)) (setf md (c4 e stem-up))) 
     (staff bass c3 e (tied-to mc) (d3 e (note-head :none) (tied-to md)))
     (staff bass cs3 e (tied-to mc) ef3 e (tied-to md)) 
     (staff bass c3 e (tied-to mc) (chord (notes c3 gf3) e (tied-to md))))


;;; ------------------------------------------------
;;; beat subdivision brackets with ends in opposite directions
;;;
;;; (use :down-up or :up-down as the bracket-type):

(cmn staff treble (c4 te (setf hi (beat-subdivision- (subdivision 3) (bracket-type :down-up) (dy .5)))) 
     (c4 te (-beat-subdivision- hi)) (c4 te (-beat-subdivision hi)))



;;; ------------------------------------------------
;;; how to display a map of some font (what's in the <mumble> font?)
;;;
;;; This uses the glyph function to display an arbitrary member of a font.
;;;  (ISO encoding of Times Roman for an example):

(cmn (size 10)    
 (prolog #'(lambda (score) (make-ISO-encoded-version score "Times-Roman" "ISO-Times-Roman")))
 (staff (staff-lines 0)
	(c4 invisible 
	    (make-sundry 
	     :mark #'(lambda (mark note score &rest rest)
		       (let ((font-size 1))
			 (moveto score 0 10)
			 (rlineto score 0 66)
			 (rlineto score 54 0)
			 (rlineto score 0 -66)
			 (rlineto score -54 0)
			 (draw score)
			 (loop for i from 0 to 8 do
			   (moveto score (+ 6 (* i 6)) 10)
			   (rlineto score 0 66))
			 (loop for j from 0 to 32 do
			   (moveto score 0 (+ 10 (* j 2)))
			   (rlineto score 54 0))
			 (draw score)
			 (loop for i from 0 to 7 do
			   (show score (text (format nil "~D" i)  (font-scaler 2.0))
				 :matrix (translate-matrix score mark (+ 2.5 (* (1+ i) 6)) 74.4)))
			 (loop for j from 0 to 31 and k from 31 downto 0 do
			   (show score (text (format nil "#o~Ox" k)  (font-scaler 2.0))
				 :matrix (translate-matrix score mark .5 (+ (* j 2) 10.4))))
			 (loop for j from 0 to 31 and k from 31 downto 0 do
			   (loop for i from 0 to 7 do
			     (show score (text (glyph (+ (* k 8) i))
					       (font-name "ISO-Times-Roman") (font-scaler 2.0))
				   :matrix (translate-matrix score mark (+ 2.5 (* (1+ i) 6)) (+ (* j 2) 10.4)))))
			 ))))))



;;; ------------------------------------------------
;;;  how to make a clef that (un)transposes
;;;

(setf transposed-double-bass 
  (let ((new-clef (copy double-bass))) 
    (setf (clef-base-pitch new-clef) (clef-base-pitch bass)) 
    new-clef))

(cmn staff transposed-double-bass c3 q)


;;; ------------------------------------------------
;;; using the beat message
;;;
;;; beat is 1-based, not 0-based, so the first beat in a measure is beat 1.
;;; If no meter is supplied, and a beat comes along that is less than the previous beat,
;;;   cmn tries to assume that the measure length is 4 -- the caller really should
;;;   include the meter (with the invisible message if needed) or explicit barlines
;;;   rather than expect cmn to conjure something up.

(cmn staff treble (meter 2 4) (c4 q (beat 2)) (cs4 e (beat 1)) (d4 e (beat 2.5)))



;;; ------------------------------------------------
;;; automatic beaming choices (the meter beaming message)
;;;
;;;   Normally, if automatic-beaming is on (the default), cmn beams according to
;;;   the following table:
;;;     
;;;   Any meter with a beat (denominator) 
;;;     >= 4 -- beam by the beat
;;;      = 8 -- beam by quarters if numerator is even
;;;          -- beam by triplets if numerator is divisible by 3
;;;          -- beam such that the final group in a bar is a triplet otherwise
;;;      = 16 - same as 8 case, but now beam by eighths
;;;   otherwise beam by quarters or full measures, whichever is less.
;;;
;;; An example is:

(cmn staff treble 
     (meter 2 4) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 5 16) c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s 
     (meter 2 4) c4 e c4 e c4 e c4 e          c4 e c4 e c4 e c4 e 
     (meter 3 8) c4 e c4 e c4 e               c4 e c4 e c4 e 
     (meter 7 8) c4 e c4 e c4 e c4 e c4 e c4 e c4 e     c4 e c4 e c4 e c4 e c4 e c4 e c4 e 
     (meter 7 16) c4 s c4 s c4 s c4 s c4 s c4 s c4 s    c4 s c4 s c4 s c4 s c4 s c4 s c4 s 
     (meter 1 4) c4 e c4 e      c4 e c4 e 
     (meter 13 8) c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e c4 e)

;;;
;;; however, these choices are often not the right thing, especially for the
;;; "unusual" meters like 7/8, and it is a major pain to put in zillions of
;;; explicit beams.  So you can ask for any arbitrary style of automatic beaming
;;; through the "beaming" message to the meter object (which can be invisible
;;; of course, so you can easily and invisibly control these decisions at any
;;; point in the score).  The argument to beaming is either a number or a list.
;;; If a number, it gives the length of the beat from the beamer's point of view.
;;; That is (beaming 2) says "beam by half notes", and (beaming .125) says
;;; "beam by 32nd notes".  If you want the beams within a bar to follow different
;;; sized beats, pass a list of them (in order of occurrence) to the beaming
;;; message:  (beaming '(2 1)) says "in each bar, beam the first half note 
;;; together and then the quarter" (assuming 3/4 time here).  Similarly, 
;;; (beaming '(.25 1.75)) says "beam the first sixteenth apart from the rest".
;;; Here are some examples:

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



;;; ------------------------------------------------
;;; note with accidental in parentheses
;;;

(cmn staff treble (c4 (sharp in-parentheses)))

;;; sometimes the placement is not great, but you can use the dx message:

(cmn staff treble (c4 (sharp in-parentheses (dx -.15))))




;;; ------------------------------------------------
;;; make unjustified the default for some class
;;;
;;;   there are a variety of ways to handle this.  Since the "constant"
;;;   versions of built-in entities are created at
;;;   cmn load/compile time, we can't depend on initialize-instance.
;;;   Here's a way to make sure the justification is :none (i.e.
;;;   unjustified):

(defmethod display :before ((dynamic dynamics-mixin) container score &optional justifying)
  (if (not (justification dynamic)) (setf (justification dynamic) :none)))

(defmethod display :before ((text text) container score &optional justifying)
  (if (not (justification text)) (setf (justification text) :none)))

;;; if pcl had generic-flet, we could use that instead.  (Dynamics
;;; are unjustified by default at present).



;;; ------------------------------------------------
;;;   dx/dy specializations to measure-rest number
;;;

(defun number (num &rest args) 
  (apply #'text 
	 (format nil "~D" num)
	 (font-name "Times-Bold")
	 (font-scaler 1.0)
	 (y #'(lambda (mark note score &optional justifying)
		(+ (staff-y0 note) (dy mark) 1.45)))
	 (x #'(lambda (mark note score &optional justifying)
		(+ (* .5 (+ (x0 note) (x1 note)))
		   (dx mark)
		   (if (> num 100) -.3 (if (> num 10) -.15 0)))))
	 args))

;;; (cmn staff treble (meter 2 4) (c4 h) (measure-rest (number 31 (dy .10))) c4 h)



;;; ------------------------------------------------
;;; how to make room on the first page for a title
;;;
;;;  use the dy message to an explicit initial line-mark:

(cmn (title "hi") (staff treble (line-mark (dy -1.0)) c4 q))

;;; this looks odd, but it's telling cmn to make extra space available for the title
;;;  at the top of the first page. 
;;;
;;; other title-moving choices:

(cmn (title (text "hi" (dy 1.4))) treble c4 q)
(cmn (title "hi") (title-separation 2.4) treble c4 q)



;;; ------------------------------------------------
;;;   beams on unisons (two styles)
;;;
;;;   this example shows both the one-head and two-head style

(cmn (setf stf1 (staff treble g4 e g4 e g4 e g4 e)) 
     (staff (tied-to stf1) (g4 e stem-down) (g4 e stem-down) (g4 e (dx .3) stem-down) (g4 e (dx .3) stem-down)))



;;; ------------------------------------------------
;;;  different note-head sizes
;;;
;;;  taken from a Chopin Etude:

(cmn (automatic-line-breaks nil)
     (size 30)
     (regularize 1)
     (staff bar treble fs-minor common-time 
	    (cycle (list c4 c5 g4 b4 a4 f4 d4 d5 c4 c5 g4 b4 a4 g4 f4 f5 f4 f5 as4 c5 b4 g4 es4 es5 an4 a5 bs4 d5 c5 bn4 g4 g5)
		   stem-up 
		   (rq 1/8)
		   (list (note-head-size 1.25) (note-head-size .75) (note-head-size .75) (note-head-size .75) 
			 (note-head-size .75) (note-head-size .75) (note-head-size 1.25) (note-head-size .75)))))



;;; ------------------------------------------------
;;;   global customizations
;;;
;;;   Say we want to change the beat-subdivision marks to use the Times-Italic font:

(defmethod initialize-instance :after ((bt beat-subdivision-mark) &rest initargs) 
  (setf (font-name bt) "Times-Italic"))



;;; ------------------------------------------------
;;;   figured bass -- use "fingering" for now (I'll automate this someday)

(cmn staff bass (e2 q (fingering 7 "5#" 3)))



;;; ------------------------------------------------
;;;   personalized cmn
;;;
;;;  say we like fatter staff lines and thinner stems:

(defun my-cmn (&rest args)
  (apply #'cmn (staff-line-width .025) (stem-width .01) args))

;;; (my-cmn (size 100) staff treble c4 q)



;;; ------------------------------------------------
;;;   guitar tablature
;;;
;;; (cmn (size 100) staff treble (c4 q (fingerboard-diagram "C?" 0 1 nil 3 4)))

(defun display-fingerboard-diagram (score mark x0 y0 strings chord-name)
  ;; at (x0=left y0=bottom), draw the fingerboard. Then place a dot on the nth space
  ;; (0'th = "0" above the string, nil = no dot or "0").  If text (or a string)
  ;; is included it is placed above the diagram.
  (let* ((fret-spacing .2)
	 (fret-width .025)
	 (string-width .01)
	 (string-spacing fret-spacing)
	 (top-width .05)
	 (dot-size .07)
	 (y1 (+ y0 top-width (* 5 fret-spacing)))
	 (strs (length strings))
	 (dx (* (1- strs) string-spacing)))
    (with-thickness score mark top-width
      (moveto score x0 y1)
      (rlineto score dx 0)
      (draw score))
    (with-thickness score mark string-width
      (loop for x from x0 by string-spacing and i from 1 to strs do
        (moveto score x y0)
        (lineto score x y1))
      (draw score))
    (with-thickness score mark fret-width
      (loop for y from (+ y0 fret-spacing) by fret-spacing and i from 1 to 5 do
        (moveto score x0 y)
        (rlineto score dx 0))
      (draw score))
    (loop for s in strings and x from x0 by string-spacing do
      (when s
	(if (= s 0)
	    (show score (text "0"  (font-scaler .3)) :matrix (translate-matrix score mark (- x .1) (+ y1 .1)))
	  (circle score x (- y1 (* fret-spacing (- s .25))) dot-size 0 360 t))))
    (when chord-name
      (show score (text chord-name  (font-scaler .5)) :matrix (translate-matrix score mark (+ x0 (* 1.5 string-spacing)) (+ y1 .4))))))

(defun fingerboard-diagram (&rest args)
  (let ((chord-name nil)
	(strings nil)
	(nm (make-sundry :name :fingerboard-diagram)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) nm (argument arg))
	(if (stringp arg)
	    (setf chord-name arg)
	  (push arg strings))))
    (setf (sundry-mark nm) #'(lambda (mark note score &optional justifying)
			       (when (not justifying)
				 (let ((x0 (+ (x0 note) -.25 (vis-dx mark)))
				       (y0 (+ (staff-y0 note) 1.5))
				       (ss (nreverse strings)))
				   (display-fingerboard-diagram score mark x0 y0 ss chord-name)))))
    nm))


;;; ------------------------------------------------
;;;   brace connecting two lines of lyrics
;;;
;;;   The open brace symbol is #o173 in the Times-Roman font, so one way to do this is:

(cmn staff treble (c4 q (text "\\173" (dx -.35) (dy -.8)) 
		        (text "Oh-" (font-scaler .35) (dx -.05) (dy -.5)) 
			(text "Ah-" (font-scaler .35) (dx -.05) (dy -.875))))



;;; ------------------------------------------------
;;;   MIDI note numbers in CMN
;;;

(defun make-midi-note (midinum &optional use-sharp)
  (let* ((cmn-num (- midinum 12))
	 (pit (mod cmn-num 12))
	 (oct (floor cmn-num 12))
	 (clas (case pit 
		 (0 0) (2 1) (4 2) (5 3) 
		 (7 4) (9 5) (11 6)
		 (1 (if use-sharp 0 1))
		 (3 (if use-sharp 1 2))
		 (6 (if use-sharp 3 4))
		 (8 (if use-sharp 4 5))
		 (10 (if use-sharp 5 6)))))
    (make-note :cclass clas :pitch pit :octave oct 
	       :sign (if (member pit (list 1 3 6 8 10)) 
			 (if use-sharp sharp flat)))))

;;; (cmn staff treble (note (make-midi-note 60)))
;;; (cmn staff treble (note (make-midi-note 61 nil)))


;;; ------------------------------------------------
;;;    Patterns and Colors
;;;
;;; a general pattern is a function called with the score as its only argument.
;;; a recurring pattern can be defined in the header with the prolog function
;;;
;;; this example only works in a level 2 Postscript (we haven't implemented the X and Quickdraw
;;; equivalents in the case statement below)

(defun pat1 (score)
  ;; this example taken from the "Postscript Language Reference Manual" p206
  (when (output score)
    (let ((str "<<
  /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 40 40] /XStep 40 /YStep 40
  /star	{
    gsave 0 8 moveto 4 {144 rotate 0 8 lineto} repeat
    closepath fill grestore
  }
  /PaintProc {
    begin
    0.333 setgray 0 0 40 40 rectfill
    1.0 0.0 0.0 setrgbcolor 10 10 translate star
    0.0 1.0 0.0 setrgbcolor 0 20 translate star
    0.0 0.0 1.0 setrgbcolor 20 -20 translate star
    1.0 1.0 0.0 setrgbcolor 0 20 translate star
    end
  }
  >>
  matrix makepattern setpattern
"))
      (g-send score str))))

(defun pat2 (score)
  (when (output score)
    (let ((str "<<
  /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] /XStep 10	/YStep 10
  /PaintProc      {
    begin
    0 0 0 setrgbcolor 0 0 5 5 rectfill
    1 0 0 setrgbcolor 5 5 5 5 rectfill
    0 0 1 setrgbcolor 5 0 5 5 rectfill
    1 1 1 setrgbcolor 0 5 5 5 rectfill
    end
  }
  >>
  matrix makepattern setpattern
"))
      (g-send score str))))

(defun pat3 (score)
  (when (output score)
    (let ((str "<<
  /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] /XStep 10	/YStep 10
  /PaintProc      {
    begin
    1 1 1 setrgbcolor 0 0 10 10 rectfill
    1 0 0 setrgbcolor newpath 3 3 moveto 3 3 3 0 360 arc fill
    0 1 0 setrgbcolor newpath 7 7 moveto 7 7 3 0 360 arc fill
    end
  }
  >>
  matrix makepattern setpattern
"))
      (g-send score str))))
  
(cmn (color '(1 0 0)) (size 100)
     (treble (pattern #'pat1)) c4 q
     (bass (pattern #'pat2)) (c4 q (color '(0 1 0)))
       c4 q (segno (dy -1) (pattern #'pat3)))


;;; dashed slurs:

(defun pat2 (score)
  (when (output score)
    (let ((str "<<
  /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] /XStep 10	/YStep 10
  /PaintProc      {
    begin
    0 0 0 setrgbcolor 0 0 5 10 rectfill
    1 1 1 setrgbcolor 5 0 10 10 rectfill
    end
  }
  >>
  matrix makepattern setpattern
"))
      (g-send score str))))

(cmn (size 100) treble c4 q (begin-slur (pattern #'pat2)) d4 q e4 q (end-slur))


;;; ------------------------------------------------
;;; multi-colored staff lines (red-line in middle etc)
;;;
;;; A staff is a score-object, so it has a draw-func slot that can be any function you like.
;;; If it is non-nil, it is funcalled as follows: (funcall (draw-func staff) score staff x0 y0 x1)
;;; where the default action is to draw (staff-lines staff) lines from x0 y0 to x1 y0.

(deferred-action draw-func)

(cmn (staff (draw-func #'(lambda (score stf x0 y0 x1)
			   (setf (line-width score) *staff-line-width*)
			   (loop for i from 0 below (staff-lines stf) do
			     (moveto score x0 (+ y0 (* i 2 *staff-line-separation*)))
			     (if (= i 2) (setf (color score) '(1 0 0)) (setf (color score) '(0 0 0)))
			     (lineto score x1 (+ y0 (* i 2 *staff-line-separation*)))
			     (draw score))))
	    treble c4 q))


;;; ------------------------------------------------
;;;   clm note list to cmn at a lower level than described in cmn.html
;;;
;;; say we have a clm note list calling the fm-violin instrument
;;; and we want to turn it into a cmn score.  Here's one way:
;;; redefine the fm-violin function to be a cmn function that
;;; calls add-note-to-staff:

(defvar scr nil)
(defvar stf nil)
(defvar qlen 1.0)

(defun qify (n)
  (let ((nn (round (* 4 (/ n qlen)))))
    (/ nn 4)))

(defun fm-violin (beg dur frq &rest args)
  (declare (ignore args))
  (add-note-to-staff scr stf (qify beg) (qify dur) frq))

(defun lify (n)
  (setf scr (init-clm-input))
  (setf stf (add-staff scr "vln" nil))
  (load n)
  (finish-clm-input scr nil nil))

;;; The qlen variable holds the length (in seconds) of a quarter note.
;;; (lify "p.clm") loads in p.clm and produces a score of it.
;;; An obvious extension would be to set up separate staves --
;;; this version produces a "piano" score.


;;; ------------------------------------------------
;;; evenly spaced measures via spacing-hook

(cmn (size 24) (output-file "keysigs.eps")
     (spacing-hook #'(lambda (score)
		       ;; divide space on the line into n evenly spaced measures
		       (let* ((bars (count-if #'(lambda (n) (eq (tld-type n) :bar)) (time-line score)))
			      (line-width (/ (inches-to-ps (- (page-width score) (right-margin score))) (size score)))
			      (measure-width (/ line-width bars))
			      (current-pos 0.0)
			      (original-pos 0.0))
			 (loop for td in (time-line score) do
			   (if (eq (tld-type td) :bar)
			       (progn
				 (incf current-pos measure-width)
				 (setf original-pos (tld-x td))
				 (setf (tld-cx td) (+ current-pos (- (tld-cx td) (tld-x td))))
				 (setf (tld-x td) current-pos))
			     (progn
			       (setf (tld-cx td) (+ current-pos (- (tld-cx td) original-pos)))
			       (setf (tld-x td) (+ current-pos (- (tld-x td) original-pos)))))))))
     (full-last-line t)
     (staff 
            treble b-major (quarter-rest (scale 0 0)) bar
            bass fs-minor (quarter-rest (scale 0 0)) bar  
            treble af-major (quarter-rest (scale 0 0)) bar
            bass  df-major  (quarter-rest  (scale 0 0)) bar))


;;; ------------------------------------------------
;;; sharp or flat sign embedded in text

(defun musical-text (&rest args)
  (make-instance 'sundry 
   :name :musical-text
   :mark #'(lambda (mark obj score &optional justifying)
	     (declare (ignore justifying))
	     (let* ((txt (apply #'text args))
		    (letters (letters txt))
		    (len (length letters))
		    (show-flat (search "-flat" letters))
		    (show-sharp (search "-sharp" letters))
		    (fname (or (font-name txt) "Times-Roman"))
		    (dx-mark (or (vis-dx txt) (vis-dx mark)))
		    (dy-mark (or (vis-dy txt) (vis-dy mark)))
		    (fsize (or (font-size txt)
			       (and (font-scaler txt)
				    (floor (* (font-scaler txt) (size score))))
			       (size score)))
		    (fscale (/ fsize (size score))))
	       (if (and (not show-flat) (not show-sharp))
		   (let ((matr (translate-matrix score mark 
						 (+ (box-x0 obj) dx-mark)
						 (+ (box-y0 obj) dy-mark))))
		     (show score txt :matrix matr))
		 (let* ((pos (or show-flat show-sharp))
			(l0 (subseq letters 0 pos))
			(l1 (subseq letters (+ pos (if show-flat 5 6))))
			(x0 (+ (box-x0 obj) dx-mark))
			(y0 (+ (box-y0 obj) dy-mark)))
		   (show score (cmn-text :letters l0 :font-name fname :font-size fsize)
			 :matrix (translate-matrix score mark x0 y0))
		   (show score (if show-flat flat sharp) 
			 :matrix (scale-matrix 
				  (translate-matrix score flat 
						    (+ x0 (* (+ (if show-flat .25 .25) (* pos .4)) fscale))  
						    (+ y0 (* (if show-flat .2 .3) fscale)))
				  fscale fscale))
		   (show score (cmn-text :letters l1 :font-name fname :font-size fsize)
			 :matrix (translate-matrix score mark (+ x0 (* (+ (* pos .4) (if show-flat .4 .4)) fscale)) y0))))))))

;;; (cmn staff treble c4 q (musical-text "change to c-sharp Major" (dy -.5) (font-scaler .5)))


;;; ------------------------------------------------
;;; landscape mode (from Anders Vinjar):

;    RG> Hi, I'm printing large size music, and I'd like to be
;    RG> able to set CMN up to rotate the PostScript output for
;    RG> landscape printing?  
;
;Try setting the 'matrix-slot in the overall score-attributes.

    (cmn (matrix '(0 1 -1  0 650 32))
	 (page-width 29.7)
	 (page-height 21.0)
	 treble c4 q)

;should get you somewhere near (the last two numbers are just
;offsetting the score x- and y-wise). 
;-anders
