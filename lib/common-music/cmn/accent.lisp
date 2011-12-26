;;; -*- Syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; various articulation marks -- an extension of the stuff in cmn1.lisp
;;;
;;; included here are:
;;;   
;;;   sul G, sul tasto, col legno and so on (connected text with bracket at end)
;;;   fingernail articulation mark (harp)
;;;   double-tongue and triple-tongue (slur+two or three dots)
;;;   nail-pizzicato (circle+dot)
;;;   martellato (wedge+dot)
;;;   heavy-accent (big wedge)
;;;   hauptstimme and nebenstimme
;;;   no-accent (an underlined "u")
;;;   doink, rip, and smear (Finale appears to use the name "doit" for these marks)
;;;   sprechstimme (i.e. x'd-stem) and circled-stem
;;;   organ-heel and toe
;;;   vibrato
;;;   inverted-turn


(in-package :cmn)



;;; ------------------------------------------------
;;; sul G and other such indications
;;;
;;;   default is to go above the staff with a bracket down at the end

(defun sul- (&rest args) 
  (apply #'text- 
	 (connecting-pattern '(10 10))
	 (font-name "Times-Italic")
	 (y #'(lambda (mark note score)
		(declare (ignore score))
		(+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		   (max (if (and (< 1 (head-line note) 5)
				 (not (whole-note-p note)))
			    (* (+ 8 (head-line note)) *staff-line-separation*)
			  0.0)
			(* (max 10 (+ 3 (head-line note))) *staff-line-separation*)))))
	 args))

(defun -sul (&rest args) (apply #'-text (end-pattern :bracket-down) args))


(defun sul-tasto- (&rest args) (apply #'sul- "sul tasto" args))
(defun -sul-tasto (&rest args) (apply #'-sul args))

;;; and so on for sul-pont, sul G, col legno, non vib and the rest of that clan
;;; unfortunately, the defvar versions of the same entities are slightly more complex...

(defvar sul-tasto- (make-self-acting 
		    :action #'(lambda (new-note &rest rest)
				(declare (ignore rest))
				(let ((st (sul-tasto-)))
				  (funcall (action st) new-note (argument st))))
		    :argument nil))

(defvar -sul-tasto (make-self-acting
		    :action #'(lambda (new-note &rest rest)
				(declare (ignore rest))
				(let ((st (-sul-tasto)))
				  (funcall (action st) new-note (argument st))))
		    :argument nil))


;;; ------------------------------------------------
;;; fingernail articulation mark
;;;
;;; this mark is apparently used in harp music

(defun display-fingernail (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		   (max (if (and (< 1 (head-line note) 5)
				 (not (whole-note-p note)))
			    (* (+ 8 (head-line note)) *staff-line-separation*)
			  0.0)
			(* (max 10 (+ 3 (head-line note))) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) -.05 (vis-dx mark) (center note) (box-x0 mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "fingernail")
    (with-thickness score mark .025
      (circle score 0 0 .3 10 170 nil)
      (circle score 0 -.2 .4 45 135 nil))
    (matrix-back score)))

(define-accent fingernail #'display-fingernail nil '(0 -.2 .4 .3))



;;; ------------------------------------------------
;;; double and triple tongue marks
;;;

(defun display-tongue (mark note score dots)
  (let ((y0 (+ (max (+ 1.25 (staff-y0 note))
		    (* (+ 2 (head-line note)) *staff-line-separation*))
	       (vis-dy mark)))
	(x0 (+ (box-x0 note) (vis-dx mark))))
    (comment score "double tongue")
    (display-tie (make-instance 'tie :x0 (- x0 .125) 
			   :x1 (+ x0 .375) 
			   :y0 y0
			   :direction :up)
		 score)
  (if (= dots 2)
      (progn
	(cmn-dot score (+ x0 .05) (- y0 .1))
	(cmn-dot score (+ x0 .2) (- y0 .1)))
    (progn
      (cmn-dot score x0 (- y0 .1))
      (cmn-dot score (+ x0 .125) (- y0 .1))
      (cmn-dot score (+ x0 .25) (- y0 .1))))))


(defvar double-tongue (make-instance 'write-protected-sundry 
			  :name :double-tongue 
			  :mark #'(lambda (mark note score &optional justifying)
				    (declare (ignore justifying))
				    (display-tongue mark note score 2))))
(defun double-tongue (&rest objects) 
  (apply #'mark #'(lambda (mark note score &optional justifying)
		    (declare (ignore justifying))
		    (display-tongue mark note score 2))
	 :double-tongue
	 objects))

(defvar triple-tongue (make-instance 'write-protected-sundry 
			  :name :triple-tongue 
			  :mark #'(lambda (mark note score &optional justifying)
				    (declare (ignore justifying))
				    (display-tongue mark note score 3))))
(defun triple-tongue (&rest objects) 
  (apply #'mark #'(lambda (mark note score &optional justifying)
		    (declare (ignore justifying))
		    (display-tongue mark note score 3))
	 :triple-tongue
	 objects))


;;; ------------------------------------------------
;;; nail pizzicato (Bartok style)
;;;

(defun display-nail-pizzicato (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		   (max (if (and (< 1 (head-line note) 5)
				 (not (whole-note-p note)))
			    (* (+ 8 (head-line note)) *staff-line-separation*)
			  0.0)
			(* (max 10 (+ 3 (head-line note))) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) .15 (vis-dx mark) (center note) (box-x0 mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "nail pizzicato")
    (with-thickness score mark .025
      (circle score 0 0 .15 0 360 nil)
      (circle score 0 0 .025 0 360 t))
    (matrix-back score)))

(define-accent nail-pizzicato #'display-nail-pizzicato nil '(-.15 -.15 .15 .15))



;;; ------------------------------------------------
;;; martellato
;;;

(defun display-martellato (mark note score &optional no-dot)
  (let* ((upper (not (member (visible-justification mark) '(:down :below))))
	 (y0 (+ (vis-dy mark) (staff-y0 note) 
		(if upper
		    (max (if (and (< 1 (head-line note) 5)
				  (not (whole-note-p note)))
			     (* (+ 8 (head-line note)) *staff-line-separation*)
			   0.0)
			 (* (max 10 (+ 3 (head-line note))) *staff-line-separation*))
		  (- (min 0.0 (box-y0 note))
		     1.0))))
	 (x0 (+ (box-x0 note) (vis-dx mark) -.05 (center note) (box-x0 mark))))
    (matrix-front score (translate-matrix score mark x0 y0)) 
    (comment score "martellato")
    (if upper
	(progn
	  (moveto score 0 0)
	  (lineto score .023 0)
	  (lineto score .097 .258)
	  (lineto score .186 0)
	  (lineto score .254 0)
	  (lineto score .112 .4)
	  (lineto score 0 0))
      (progn
	(moveto score .07 0)
	(lineto score .112 .4)
	(lineto score .088 .4)
	(lineto score .014 .142)
	(lineto score -.074 .4)
	(lineto score -.142 .4)
	(lineto score .07 0)))
    (fill-in score)
    (when (not no-dot)
      (if upper 
	  (circle score .104 .03 .03 0 360 t)
	(circle score (- .254 .104 .07) (- .4 .03) .03 0 360 t))) ;.07 for initial offset (centering)
    (matrix-back score)))

(define-accent martellato #'display-martellato nil '(-.25 0 .25 .3))



;;; ------------------------------------------------
;;; heavy-accent

(defvar heavy-accent (make-instance 'write-protected-sundry 
		     :name :heavy-accent 
		     :mark #'(lambda (mark note score &optional justifying)
			       (declare (ignore justifying))
			       (display-martellato mark note score t))))

(defun heavy-accent (&rest objects) (apply #'mark #'(lambda (mark note score &optional justifying)
						    (declare (ignore justifying))
						    (display-martellato mark note score t))
					 :heavy-accent objects))


;;; ------------------------------------------------
;;; Hauptstimme

(defun display-hauptstimme (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (box-y0 note)))
	 (x-off (+ (box-x0 note) -.125 (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "hauptstimme")
    (with-thickness score mark .025
      (moveto score 0 0)
      (rlineto score 0 .5)
      (rlineto score .2 0)
      (rmoveto score -.2 -.25)
      (rlineto score -.325 0)
      (rmoveto score 0 -.25)
      (rlineto score 0 .5)
      (draw score))
    (matrix-back score)))

(define-accent hauptstimme #'display-hauptstimme nil '(-.325 0 .2 .5))


;;; ------------------------------------------------
;;; Nebenstimme

(defun display-nebenstimme (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (box-y0 note)))
	 (x-off (+ (box-x0 note) -.125 (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "nebenstimme")
    (with-thickness score mark .025
      (moveto score 0 0)
      (rlineto score 0 .5)
      (rlineto score .2 0)
      (rmoveto score -.2 -.5)
      (rlineto score -.3 .5)
      (rlineto score 0 -.5)
      (draw score))
    (matrix-back score)))

(define-accent nebenstimme #'display-nebenstimme nil '(-.3 0 .2 .5))


;;; ------------------------------------------------
;;; no-accent

(defun display-no-accent (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		   (max (if (and (< 1 (head-line note) 5)
				 (not (whole-note-p note)))
			    (* (+ 8 (head-line note)) *staff-line-separation*)
			  0.0)
			(* (max 10 (+ 3 (head-line note))) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) -.05 (vis-dx mark) (center note) (box-x0 mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "no accent")
    (with-thickness score mark .025
      (circle score 0  .2 .15 180 360 nil)
      (moveto score -.15 -.025)
      (rlineto score .325 0)
      (moveto score -.15 .2)
      (rlineto score 0 .1)
      (moveto score .15 .2)
      (rlineto score 0 .1)
      (draw score))
    (matrix-back score)))

(define-accent no-accent #'display-no-accent nil '(0 0 .5 .3))


;;; ------------------------------------------------
;;; doink

(defun display-doink (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y0 (+ (vis-dy mark) (box-y0 note))) 
	 (x0 (+ (box-x0 note) .5 (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x0 y0))
    (comment score "doink")
    (moveto score 0 0)
    (curveto score .01 0 .04 .2 .5 .5)
    (curveto score .04 .15 .01 -.05 0 0)
    (fill-in score)
    (matrix-back score)))

(define-accent doink #'display-doink nil '(0 0 .5 .5))


;;; ------------------------------------------------
;;; rip

(defun display-rip (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y0 (+ (vis-dy mark) (box-y0 note))) 
	 (x0 (+ (box-x0 note) -.125 (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x0 y0))
    (comment score "rip")
    (moveto score 0 0)
    (curveto score -.01 0 -.04 -.2 -.5 -.5)
    (curveto score -.04 -.15 -.01 .05 0 0)
    (fill-in score)
    (matrix-back score)))

(define-accent rip #'display-rip nil '(-.5 -.5 0 0))


;;; ------------------------------------------------
;;; smear

(defun display-smear (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
	 (y0 (+ (staff-y0 note) (* hl *staff-line-separation*) .25 (if (and (evenp hl) (< 1 hl 9)) .125 0)))
	 (x0 (+ (box-x0 note) -.05 (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x0 y0))
    (comment score "smear")
    (moveto score 0 0)
    (curveto score .2 .3 .3 -.3 .45 .05)
    (curveto score .3 -.2 .2 .2 0 0)
    (fill-in score)
    (matrix-back score)))

(define-accent smear #'display-smear nil '(0.000 -0.062 0.448 0.086))


;;; ------------------------------------------------
;;; sprechstimme
;;;
;;;   any note or chord's stem can have any arbitrary mark placed on its stem.
;;;   The stem-mark field is a function funcall'd with the args score x0 y0 y1 (stem-wise)

(defun sprechstimme (&rest objects)
  (let ((nm (apply #'mark nil nil objects)))
    (stem-mark #'(lambda (score x0 y0 y1)
		   (comment score "sprechstimme")
		   (with-thickness score nm .01
		     (moveto score (- (+ x0 (vis-dx nm)) .125 .01) (- (+ (vis-dy nm) (* .5 (+ y0 y1))) .125))
		     (rlineto score .25 .25)
		     (rmoveto score 0 -.25)
		     (rlineto score -.25 .25)
		     (draw score))))))

(defvar sprechstimme (sprechstimme))


;;; ------------------------------------------------
;;; circled-stem

(defun circled-stem (&rest objects)
  (let ((nm (apply #'mark nil nil objects)))
    (stem-mark #'(lambda (score x0 y0 y1)
		   (comment score "circled stem")
		   (with-thickness score nm .01
		     (circle score (+ x0 (vis-dx nm)) (+ (vis-dy nm) (* .5 (+ y0 y1))) .125 0 360 nil))))))

(defvar circled-stem (circled-stem))


;;; ------------------------------------------------
;;; organ-heel
;;;
;;; my organ music does not use the same marking as recommended by Read, "Music Notation"
;;; This function implements the latter.

(defun display-organ-heel (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note) -.5))
	 (x-off (+ (box-x0 note) (center note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "organ heel")
    (with-thickness score mark .0125
      (circle score 0 0 .15 0 360 nil))
    (matrix-back score)))

(define-accent organ-heel #'display-organ-heel nil '(-.15 -.15 .15 .15))


;;; ------------------------------------------------
;;; organ-toe

(defun display-organ-toe (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (vis-dy mark) (staff-y0 note) -.5))
	 (x-off (+ (box-x0 note) (center note) (vis-dx mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "organ toe")
    (with-thickness score mark .0125
      (moveto score -.1 -.3)
      (rlineto score .1 .4)
      (rlineto score .1 -.4)
      (draw score))
    (matrix-back score)))

(define-accent organ-toe #'display-organ-toe nil '(-.1 -.3 .1 .3))


;;; ------------------------------------------------
;;; vibrato
;;;
;;; (this uses the wavy line of the trill ornament)

(defclass vib (trill)
  ((wavy-line :initform t)))

(defun display-vibrato (vib note score &optional justifying)
  (declare (ignore justifying))
  (let ((x0 (+ (box-x0 note) (vis-dx vib) .5))
	(y0 (+ (box-y0 note) (vis-dy vib) -.0625 
	       (if (and (evenp (head-line note)) 
			(< (head-line note) 9)) .125 0))))
    (matrix-front score (translate-matrix score vib x0 y0))
    (let* ((wid (- (third trill-section-bounds) (first trill-section-bounds)))
	   (ct (round (- (box-x1 vib) x0 .75) wid)))
      (draw-trill-sections score ct)
      (matrix-back score))))

(defun vibrato (&rest objects)
  (let ((new-vib (make-instance 'vib :mark #'display-vibrato)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-vib (argument act)))))
    new-vib))

(defvar vibrato (make-instance 'vib :name :vibrato :mark #'display-vibrato))


;;; ------------------------------------------------
;;; inverted-turn
;;;

(defun display-inverted-turn (mark note score &optional justifying)
  (declare (ignore justifying))
  (incf (box-x0 mark) (if (stem-is-up? note) .1 .15))
  (setf (matrix mark) (mirror-matrix (matrix mark)))
  (display-ornament mark #'draw-turn turn-bounds note score))

(defvar inverted-turn (make-instance 'write-protected-ornament :name :inverted-turn :mark #'display-inverted-turn))
(defun inverted-turn (&rest objects) (apply #'ornament #'display-inverted-turn :inverted-turn objects))
