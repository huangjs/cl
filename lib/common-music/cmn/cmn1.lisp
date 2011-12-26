;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; continuation of cmn0.lisp

(in-package :cmn)

;;;
;;; ----------------    bar-lines, double-bars etc
;;;

(defclass bar-mixin (staff-relative-mixin score-object-mixin thick-mixin breathing-space-mixin)
  ((double :initarg :double :initform nil :reader double)
   (terminal :initarg :terminal :initform nil :reader terminal)
   (within-measure :initarg :within-measure :initform nil :reader within-measure)
   (dashed :initarg :dashed :initform nil :reader dashed)
   (dots-left :initarg :dots-left :initform nil :reader dots-left)
   (dots-right :initarg :dots-right :initform nil :reader dots-right)
   (inserted :initarg :inserted :initform nil :reader inserted)
   (broken :initarg :broken :initform t :reader broken)
   (thick-lines :initarg :thick-lines :initform nil :reader thick-lines)
   (marks :reader bar-marks)
   (thickness :initform nil)
   (breathing-space :initform .1)
   (height :initform nil)
   (flanges :initarg :flanges :initform nil :reader flanges)))

;;; bar can be single line, double line, either can be light or heavy or dashed, either side may have repeat dots
;;; marks can be dal-segno fine ds-al-coda dc dc-al-fine etc
;;; repeat bar can occur within a measure and has no metric significance
;;; double-repeat bar can be of three (or more?) forms: [thin-line thick line], [thin thick thick thin], [thick thick]


(defclass bar (bar-mixin staff-relative-mixin score-object thick breathing-space)
  ((double :accessor double)
   (terminal :accessor terminal)
   (within-measure :accessor within-measure)
   (dashed :accessor dashed)
   (dots-left :accessor dots-left)
   (dots-right :accessor dots-right)
   (inserted :accessor inserted)
   (broken :accessor broken)
   (thick-lines :accessor thick-lines)
   (marks :accessor bar-marks)
   (height :accessor height)
   (flanges :accessor flanges)))

(defclass write-protected-bar (write-protect bar-mixin)
  ())

(deferred-action double)
(deferred-action terminal)
(deferred-action justification)
(deferred-action dashed)
(deferred-action within-measure)
(deferred-action dots-left)
(deferred-action dots-right)
(deferred-action broken)
(deferred-action thick-lines)
(deferred-action height)
(deferred-action flanges)

(defvar bar (make-instance 'write-protected-bar))
(defvar double-bar (make-instance 'write-protected-bar :double t :terminal t))
(defvar terminal-bar (make-instance 'write-protected-bar :terminal t))
(defvar interior-double-bar (make-instance 'write-protected-bar :double t))
(defvar dashed-bar (make-instance 'write-protected-bar :dashed t))
(defvar begin-repeat-bar (make-instance 'write-protected-bar :double t :dots-right t))
(defvar end-repeat-bar (make-instance 'write-protected-bar :double t :dots-left t))
(defvar final-repeat-bar (make-instance 'write-protected-bar :double t :dots-left t :terminal t))
(defvar begin-and-end-repeat-bar (make-instance 'write-protected-bar :double t :dots-right t :dots-left t))
(defvar full-bar (make-instance 'write-protected-bar :broken nil))
(defvar full-double-bar (make-instance 'write-protected-bar :double t :terminal t :broken nil))
(defvar full-interior-double-bar (make-instance 'write-protected-bar :double t :broken nil))
(defvar begin-and-end-repeat-bar-without-thin-lines (make-instance 'write-protected-bar :double t :dots-right t :dots-left t :thick-lines 2))
(defvar begin-and-end-repeat-bar-with-one-thick-line (make-instance 'write-protected-bar :double t :dots-right t :dots-left t :thick-lines 1))
(defvar begin-flanges-repeat-bar (make-instance 'write-protected-bar :double t :dots-right t :flanges t))
(defvar end-flanges-repeat-bar (make-instance 'write-protected-bar :double t :dots-left t :flanges t))

(defmethod bar-p ((obj t)) nil)
(defmethod bar-p ((obj bar-mixin)) t)
(defmethod metrical-bar-p ((obj t)) nil)
(defmethod metrical-bar-p ((obj bar-mixin)) (not (within-measure obj)))

(defun bar-print-name (bar)
  (if (double bar) 
      (if (terminal bar) 
	  (if (dots-left bar)
	      "final-repeat-bar"
	    (if (broken bar)
		"double-bar"
	      "full-double-bar"))
	(if (dots-left bar)
	    (if (flanges bar)
		"end-flanges-repeat-bar"
	      (if (dots-right bar)
		  (if (not (thick-lines bar))
		      "begin-and-end-repeat-bar"
		    (if (= (thick-lines bar) 1)
			"begin-and-end-repeat-bar-with-one-thick-line"
		      "begin-and-end-repeat-bar-without-thin-lines"))
		"end-repeat-bar"))
 	  (if (dots-right bar)
  	      "begin-repeat-bar"
 	    (if (flanges bar)
		"begin-flanges-repeat-bar"
               (if (broken bar)
                   "interior-double-bar"
                 "full-interior-double-bar")))))
    (if (terminal bar)
	"terminal-bar"
      (if (dashed bar)
	  "dashed-bar"
	(if (broken bar)
	    "bar"
	  "full-bar")))))

(defmethod descry ((bar bar-mixin) &optional stream controller)
  (format stream "(~A~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) (format nil "~A" (bar-print-name bar)) "")
	  (if (within-measure bar) (format nil " :within-measure ~A" (within-measure bar)) "")
	  (if (dashed bar) (format nil " :dashed ~A" (dashed bar)) "")
	  (if (dots-left bar) (format nil " :dots-left ~A" (dots-left bar)) "")
	  (if (dots-right bar) (format nil " :dots-right ~A" (dots-right bar)) "")
          (if (flanges bar) (format nil " :flanges ~A" (flanges bar)) "")
	  (if (inserted bar) " (created by cmn)" "")
	  (if (thick-lines bar) (format nil " :thick-lines ~D" (thick-lines bar)) "")
	  (if (thickness bar) (format nil " :thickness ~A" (thickness bar)) "")
	  (if (next-method-p) (call-next-method bar stream (or controller bar)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((bar bar-mixin) &optional object)
  (let ((new-bar (if (not object) (make-instance 'bar)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (double new-bar) (double bar))
    (setf (terminal new-bar) (terminal bar))
    (setf (broken new-bar) (broken bar))
    (setf (within-measure new-bar) (within-measure bar))
    (setf (dots-left new-bar) (dots-left bar))
    (setf (dots-right new-bar) (dots-right bar))
    (setf (flanges new-bar) (flanges bar))
    (setf (dashed new-bar) (dashed bar))
    (setf (inserted new-bar) (inserted bar))
    (setf (thick-lines new-bar) (thick-lines bar))
    (setf (thickness new-bar) (thickness bar))
    (if (next-method-p) (call-next-method bar new-bar))
    new-bar))

(defmethod identify ((bar bar-mixin))
  (format nil "(~A~A~A~A)" 
	  (bar-print-name bar)
	  (check-for-cmn-store-tag bar)
	  (identify-marks bar)
	  (identify-visible bar)))

(defun bar (&rest objects)
  (apply #'ur-bar (make-instance 'bar) objects))

(defun cmn-bar (&key onset beat duration hidden)
  (let ((new-bar (make-instance 'bar)))
    (setf (odb-onset new-bar) onset)
    (setf (odb-beat new-bar) beat)
    (setf (odb-duration new-bar) duration)
    (setf (inserted new-bar) t)
    (if hidden (setf (matrix new-bar) (list 0 0 0 0 0 0)))
    new-bar))

(defun ur-bar (new-bar &rest objects)
  (loop for act in objects do
    (when act
      (if (self-acting-p act)
	  (funcall (action act) new-bar (argument act))
	(if (bar-p act)
	    (copy act new-bar)
	  (if (visible-p act)
	      (push (copy act) (bar-marks new-bar))
	    (cmn-warn "odd argument to bar: ~A" act))))))
  new-bar)

(defmethod minimum-line ((bar bar-mixin)) 0)

(defun terminal-bar (&rest args) (apply #'bar (terminal t) args))
(defun double-bar (&rest args) (apply #'bar (double t) (terminal t) args))
(defun interior-double-bar (&rest args) (apply #'bar (double t) args))
(defun dashed-bar (&rest args) (apply #'bar (dashed t) args))
(defun begin-repeat-bar (&rest args) (apply #'bar (double t) (dots-right t) args))
(defun end-repeat-bar (&rest args) (apply #'bar (double t) (dots-left t) args))
(defun final-repeat-bar (&rest args) (apply #'bar (double t) (dots-left t) (terminal t) args))
(defun begin-and-end-repeat-bar (&rest args) (apply #'bar (double t) (dots-right t) (dots-left t) args))
(defun begin-and-end-repeat-bar-without-thin-lines (&rest args) (apply #'bar (double t) (dots-right t) (dots-left t) (thick-lines 2) args))
(defun begin-and-end-repeat-bar-with-one-thick-line (&rest args) (apply #'bar (double t) (dots-right t) (dots-left t) (thick-lines 1) args))

(defun full-double-bar (&rest args) (apply #'bar (double t) (terminal t) (broken nil) args))
(defun full-interior-double-bar (&rest args) (apply #'bar (double t) (broken nil) args))
(defun full-bar (&rest args) (apply #'bar (broken nil) args))

(defun begin-flanges-repeat-bar (&rest args) (apply #'bar (double t) (dots-right t) (flanges t) args))
(defun end-flanges-repeat-bar (&rest args) (apply #'bar (double t) (dots-left t) (flanges t) args))
 
(defmethod notify ((bar bar) &optional objects)
  (apply #'ur-bar bar objects))

(defun repeat-dots (score x0 y0 &optional (radius .06))
  ;; x0 is left side, circle takes center
  (circle score (+ x0 radius) (+ y0 .375) radius 0 360 t)
  (circle score (+ x0 radius) (+ y0 .375 .25) radius 0 360 t))

(defmethod display ((bar bar-mixin) container score &optional justifying)
  (declare (ignore container))
  (let* ((x0 (+ (box-x0 bar) (vis-dx bar)))
	 (y0 (+ (box-y0 bar) (vis-dy bar)))
	 (y1 (if (height bar) (+ y0 (height bar)) (+ (box-y1 bar) (vis-dy bar))))
	 (double-bar-space (breathing-space bar))
	 (double-bar-thickness (or (thickness bar) *double-barline-thickness*))
	 (bar-thickness (or (thickness bar) *barline-thickness*))
	 (size (scr-size score))
	 (dot-radius .06)
	 (dot-to-line-spacing (if (and (thick-lines bar) (= (thick-lines bar) 1)) dot-radius double-bar-space))
	 (dot-back (+ dot-to-line-spacing (* 2 dot-radius)))
	 (px0 x0))
    (when (double bar) (decf px0 (if (or (terminal bar) (dots-left bar))
				     (* 2 double-bar-thickness)
				   double-bar-thickness)))
					;make room for either interior-double-bar (not terminal) or double bar
    (when (dots-left bar) (decf px0 dot-back))
    (when (and (double bar) (thick-lines bar) (/= (thick-lines bar) 2)) (decf px0 double-bar-space))
    (if (bar-marks bar) (display-marks bar score justifying))
    (when (not (invisible-p bar))
      ;; now the zillion possible bars -- dashed single, single, double, dashed double
      ;;   terminal, double both, left and right dots, probably more I've forgotten.
      (moveto score px0 y0)
      (when (dots-left bar)
	(repeat-dots score px0 y0)
	(incf px0 dot-back)
	(moveto score px0 y0))
      (when (and (or (not (dots-right bar))
		     (dots-left bar))
		 (or (not (thick-lines bar))
		     (/= (thick-lines bar) 2)))
	(setf (line-width score) bar-thickness)
	(if (dashed bar)
	    (lineto score px0 y1 :pattern (list (floor (* 5 (/ size 40))) (floor (* 7 (/ size 40)))))
	  (lineto score px0 y1))
	(draw score)
	(incf px0 double-bar-space))
      (when (flanges bar)
	(if (and (not (member (flanges bar) '(:left left)))
		 (or (member (flanges bar) '(:right right)) (dots-right bar)))
	    (let ((px01 (if (double bar) px0
			  (- px0 double-bar-space))))
	      (progn (moveto score px01 y0)
		     (curveto score (+ px01 0.2) (- y0 0.04) (+ px01 0.26) (- y0 0.14) (+ px01 0.29) (- y0 0.2))
		     (curveto score (+ px01 .26) (- y0 0.1) (+ px01 0.2) (- y0 0.025) (+ px01 double-bar-thickness) y0)
		     (fill-in score)
		     (moveto score px01 y1)
		     (curveto score (+ px01 0.2) (+ y1 0.04) (+ px01 0.26) (+ y1 0.14) (+ px01 0.29) (+ y1 0.2))
		     (curveto score (+ px01 .26) (+ y1 0.1) (+ px01 0.2) (+ y1 0.025) (+ px01 double-bar-thickness) y1)
		     (fill-in score)))
	  (let ((px01 (if (double bar)
			  (if (or (dots-left bar) (dots-right bar) (terminal bar))
			      (+ px0 double-bar-thickness)
			    px0)
			(- px0 double-bar-space))))
	    (progn (moveto score px01 y0)
		   (curveto score (- px01 0.2) (- y0 0.04) (- px01 0.26) (- y0 0.14) (- px01 0.29) (- y0 0.2))
		   (curveto score (- px01 .26) (- y0 0.1) (- px01 0.2) (- y0 0.025) (- px01 double-bar-thickness) y0)
		   (fill-in score)
		   (moveto score px01 y1)
		   (curveto score (- px01 0.2) (+ y1 0.04) (- px01 0.26) (+ y1 0.14) (- px01 0.29) (+ y1 0.2))
		   (curveto score (- px01 .26) (+ y1 0.1) (- px01 0.2) (+ y1 0.025) (- px01 double-bar-thickness) y1)
		   (fill-in score)))))
      (when (double bar)
	(when (or (terminal bar) (dots-left bar) (dots-right bar))
	  (moveto score px0 y0)
	  (lineto score (+ px0 double-bar-thickness) y0)
	  (lineto score (+ px0 double-bar-thickness) y1)
	  (lineto score px0 y1)
	  (lineto score px0 y0)
	  (fill-in score)
	  (incf px0 (+ double-bar-thickness double-bar-space))
	  (if (and (thick-lines bar) (= (thick-lines bar) 2)) (decf px0 .04))
	  (when (and (dots-left bar) 
		     (dots-right bar)
		     (or (not (thick-lines bar))
			 (/= (thick-lines bar) 1)))
	    (moveto score px0 y0)
	    (lineto score (+ px0 double-bar-thickness) y0)
	    (lineto score (+ px0 double-bar-thickness) y1)
	    (lineto score px0 y1)
	    (lineto score px0 y0)
	    (fill-in score)
	    (incf px0 (+ double-bar-thickness double-bar-space))))
	(when (and (or (dots-right bar)
		       (not (or (dots-left bar) (dots-right bar) (terminal bar))))
		   (or (not (thick-lines bar))
		       (/= (thick-lines bar) 2)))
 	  (setf (line-width score) bar-thickness) ; AV 10-Oct-00 / WS 14-Oct-06
	  (moveto score px0 y0)
	  (lineto score px0 y1)
	  (draw score)
	  (incf px0 dot-to-line-spacing))
	(when (dots-right bar)
	  (moveto score px0 y0)
	  (repeat-dots score px0 y0))
	))))


(defvar bar-walls '(.1 .1))	;was .1 0 but that presses against right side too tightly
;(defvar bar-fences '(.05 .025))
;(defvar bar-expanders '(1 1))
(defvar bar-fences '(.1 .05))
(defvar bar-expanders '(0 1))

(defmethod house ((bar bar-mixin) score)
  (declare (ignore score))
  (let ((dot-space .25))
    (setf (box-x1 bar) (+ (if (dots-left bar) dot-space 0)
			  (if (dots-right bar) dot-space 0)
			  (if (double bar) (breathing-space bar) 0)
			  (if (terminal bar) (or (thickness bar) *double-barline-thickness*) 0)
			  (if (and (dots-right bar) (dots-left bar)) 
			      (+ (breathing-space bar) (or (thickness bar) 
							   (if (double bar)
							       *double-barline-thickness*
							       *barline-thickness*)))
			    0)))
    (setf (box-y1 bar) 1.0)
    (setf (center bar) (* .5 (box-x1 bar)))
    (if (not (walls bar)) (setf (walls bar) bar-walls))
    (if (not (fences bar)) (setf (fences bar) bar-fences))
    (if (not (expanders bar)) (setf (expanders bar) bar-expanders))))

;;; (cmn staff treble c4 w begin-and-end-repeat-bar c4 w begin-and-end-repeat-bar-without-thin-lines 
;;;    c4 w begin-and-end-repeat-bar-with-one-thick-line c4 w end-repeat-bar c4 begin-repeat-bar c4 dashed-bar c4 double-bar)

#|
(cmn (automatic-ties nil) treble (c4 q invisible)
     (begin-repeat-bar  (flanges t) (text "repeat until sign" (dy 1.8) (font-scaler 0.5)))
     (b4 s (begin-slur)) (c5 s) (bf4 h (end-slur))
     (end-repeat-bar (flanges t)))
|#

;;;
;;; ----------------    braces, brackets
;;;

(defclass bracket-mixin (score-object-mixin)
  ())

(defclass bracket (bracket-mixin score-object) ())

(defclass write-protected-bracket (write-protect bracket-mixin) ())

(defvar bracket (make-instance 'write-protected-bracket))

(defmethod bracket-p ((obj t)) nil)
(defmethod bracket-p ((obj bracket-mixin)) t)

(defmethod descry ((bracket bracket-mixin) &optional stream controller)
  (format stream "~A~A~A"
	  (if (not controller) "(bracket" "")
	  (if (next-method-p) (call-next-method bracket stream (or controller bracket)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((bracket bracket-mixin))
  (format nil "(bracket~A~A)" (identify-visible bracket) (identify-marks bracket)))

(defmethod copy ((bracket bracket-mixin) &optional object)
  (let ((new-bracket (if (not object) (make-instance 'bracket)
		       (if (write-protected object) (copy object)
			 object))))
    (if (next-method-p) (call-next-method bracket new-bracket))
    new-bracket))

(defun bracket (&rest objects)
  (let ((new-bracket (make-instance 'bracket)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-bracket (argument act))
	  (if (visible-p act)
	      (push act (marks new-bracket))
	    (cmn-warn "odd argument to bracket: ~A" act)))))
    new-bracket))

(defmethod notify ((bracket bracket) &optional objects)
  (apply #'bracket bracket objects))

(defmethod display ((bracket bracket-mixin) container score &optional justifying)
  (declare (ignore container))
  (let* ((x0 (+ (box-x0 bracket) (vis-dx bracket)))
	 (y0 (+ (box-y0 bracket) (vis-dy bracket)))
	 (y1 (+ (box-y1 bracket) (vis-dy bracket))) ;y1=(+ y0 1 (* 3 (1- staves)))
	 (bx (/ (floor (* (- (or x0 0.0) .25) (scr-size score))) (scr-size score)))
	 (by y0))
    (if (marks bracket) (display-marks bracket score justifying))
    (comment score "bracket")
    (matrix-front score (scale-matrix (translate-matrix score bracket (- bx .2) (+ by .5)) 2 2))
    (draw-lower-bracket score)
    (matrix-back score)
    (moveto score bx y0)
    (lineto score bx y1)
    (lineto score (+ bx .15) y1)
    (lineto score (+ bx .15) y0)
    (lineto score bx y0)
    (fill-in score)
    (matrix-front score (scale-matrix (translate-matrix score bracket (- bx .2) (- y1 .5)) 2 2))
    (draw-upper-bracket score)
    (matrix-back score)))

(defvar bracket-walls '(.05 .05))

(defmethod house ((bracket bracket-mixin) score)
  (declare (ignore score))
  (setf (box-y1 bracket) 1.0)
  (if (not (walls bracket)) (setf (walls bracket) bracket-walls)))



(defclass brace-mixin (score-object-mixin)
  ((staves :initarg :staves :initform 2 :reader brace-staves)))

(defclass brace (brace-mixin score-object)
  ((staves :accessor brace-staves)))

(defclass write-protected-brace (write-protect brace-mixin) ())

(defvar brace (make-instance 'write-protected-brace))

(defmethod brace-p ((obj t)) nil)
(defmethod brace-p ((obj brace-mixin)) t)

(defmethod descry ((brace brace-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(brace" "")
	  (if (/= (brace-staves brace) 2) (format nil " :staves ~D" (brace-staves brace)) "")
	  (if (next-method-p) (call-next-method brace stream (or controller brace)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((brace brace-mixin))
  (format nil "(brace~A~A~A)" 
	  (if (/= (brace-staves brace) 2) (format nil " (brace-staves ~D)" (brace-staves brace)) "")
	  (identify-visible brace) 
	  (identify-marks brace)))

(defmethod copy ((brace brace-mixin) &optional object)
  (let ((new-brace (if (not object) (make-instance 'brace)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (brace-staves new-brace) (brace-staves brace))
    (if (next-method-p) (call-next-method brace new-brace))
    new-brace))

(deferred-action brace-staves)

(defun brace (&rest objects)
  (apply #'ur-brace (make-instance 'brace) objects))

(defun ur-brace (new-brace &rest objects)
  (loop for act in objects do
    (when act
      (if (self-acting-p act)
	  (funcall (action act) new-brace (argument act))
	(if (visible-p act)
	    (push act (marks new-brace))
	  (cmn-warn "odd argument to brace: ~A" act)))))
  new-brace)

(defmethod notify ((brace brace) &optional objects)
  (apply #'ur-brace brace objects))

(defmethod display ((brace brace-mixin) container score &optional justifying)
  (declare (ignore container))
  (let* ((tx0 (+ (box-x0 brace) (vis-dx brace)))
	 (y0 (+ (box-y0 brace) (vis-dy brace)))
	 (y1 (+ (box-y1 brace) (vis-dy brace)))
	 (midy (* .5 (+ y0 y1)))
	 (xfactor (/ (- y1 y0) 3.14))
	 (yfactor (/ (- y1 y0) 3.14))
	 (x0 (- tx0 (+ (* xfactor .15) *brace-space*))))
    (when (marks brace) (with-position brace x0 y0 (display-marks brace score justifying)))
    (matrix-front score (scale-matrix (translate-matrix score brace x0 midy) xfactor yfactor))
    ;; this pair of curves needs to be centered on y=0
    (moveto score -0.035 0.000)
    (curveto score 0.550 0.528 -0.172 0.800 0.237 1.572)
    (curveto score -0.295 0.750 0.415 0.360 -0.035 0.000)
    (fill-in score)
    (moveto score -0.035 0.000)
    (curveto score 0.550 -.528 -0.172 -0.800 0.237 -1.572)
    (curveto score -0.295 -.750 0.415 -0.360 -0.035 0.000)
    (fill-in score)
    (matrix-back score)))

(defvar brace-walls '(.1 .05))

(defmethod house ((brace brace-mixin) score)
  (declare (ignore score))
  (setf (box-y1 brace) 1.0)
  (if (not (walls brace)) (setf (walls brace) brace-walls)))

;;; (cmn (system (brace (brace-staves 3)) (staff (staff-size .7) treble c4 q) (staff treble c4 q) (staff bass c3 q)))

(defun the-usual-suspects (object)
  (format nil "~A~A~A~A~A" 
	  (identify-marks object) 
	  (check-for-cmn-store-tag object)
	  (identify-matrix object)
	  (identify-color object)
	  (identify-visible object)))


;;;
;;; ----------------    clefs
;;;

;;; clefs are french-violin treble soprano mezzo-soprano alto tenor baritone baritone-C baritone-F bass sub-bass percussion

(defclass clef-mixin (staff-relative-mixin score-object-mixin breathing-space-mixin) 
  ((position :initarg :position :initform nil :reader clef-position)
   (base-pitch :initarg :base-pitch :initform nil :reader clef-base-pitch)
   (base-line-note :initarg :base-line-note :initform nil :reader clef-base-line-note)
   (base-space-note :initarg :base-space-note :initform nil :reader clef-base-space-note)
   (top-space-note :initarg :top-space-note :initform nil :reader clef-top-space-note)
   (sharp-offset :initarg :sharp-offset :initform nil :reader clef-sharp-offset)
   (flat-offset :initarg :flat-offset :initform nil :reader clef-flat-offset)
   (letter :initarg :letter :initform nil :reader clef-letter)
   (name :initarg :name :initform nil :reader clef-name)
   (breathing-space :initform .04)))

(defclass clef (clef-mixin score-object breathing-space) 
  ((position :accessor clef-position)
   (base-pitch :accessor clef-base-pitch)
   (base-line-note :accessor clef-base-line-note)
   (base-space-note :accessor clef-base-space-note)
   (top-space-note :accessor clef-top-space-note)
   (sharp-offset :accessor clef-sharp-offset)
   (flat-offset :accessor clef-flat-offset)
   (letter :accessor clef-letter)
   (name :accessor clef-name)
   (breathing-space :initform .04)))

(defclass write-protected-clef (write-protect clef-mixin) ())

(defmethod clef-p ((obj t)) nil)
(defmethod clef-p ((obj clef-mixin)) (not (key-p obj)))

(defmethod descry ((clef clef-mixin) &optional stream controller)
  (format stream "~A :position ~D :base-pitch ~D :base-line-note ~D :base-space-note ~D :top-space-note ~D :sharp-offset ~D~%~A~
                  :flat-offset ~D :letter :~(~A~) :name :~(~A~) ~A~A"
	  (if (not controller) "(clef" "")
	  (clef-position clef)
	  (clef-base-pitch clef)
	  (clef-base-line-note clef)
	  (clef-base-space-note clef)
	  (clef-top-space-note clef)
	  (clef-sharp-offset clef)
	  prewhitespace
	  (clef-flat-offset clef)
	  (clef-letter clef)
	  (clef-name clef)
	  (if (next-method-p) (call-next-method clef stream (or controller clef)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((clef clef-mixin) &optional object)
  (let ((new-clef (if (not object) (make-instance 'clef)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (clef-position new-clef) (clef-position clef))
    (setf (clef-base-pitch new-clef) (clef-base-pitch clef))
    (setf (clef-base-line-note new-clef) (clef-base-line-note clef))
    (setf (clef-base-space-note new-clef) (clef-base-space-note clef))
    (setf (clef-top-space-note new-clef) (clef-top-space-note clef))
    (setf (clef-sharp-offset new-clef) (clef-sharp-offset clef))
    (setf (clef-flat-offset new-clef) (clef-flat-offset clef))
    (setf (clef-letter new-clef) (clef-letter clef))
    (setf (clef-name new-clef) (clef-name clef))
    (if (next-method-p) (call-next-method clef new-clef))
    (if (marks new-clef) (setf (marks new-clef) nil))
    new-clef))

(defmethod dcopy ((clef clef-mixin) key)
  (setf (clef-position key) (clef-position clef))
  (setf (clef-base-pitch key) (clef-base-pitch clef))
  (setf (clef-base-line-note key) (clef-base-line-note clef))
  (setf (clef-base-space-note key) (clef-base-space-note clef))
  (setf (clef-top-space-note key) (clef-top-space-note clef))
  (setf (clef-sharp-offset key) (clef-sharp-offset clef))
  (setf (clef-flat-offset key) (clef-flat-offset clef))
  (setf (clef-letter key) (clef-letter clef))
  (setf (clef-name key) (clef-name clef))
  key)

(defun clef (&rest objects)
  (apply #'ur-clef (make-instance 'clef) objects))

(defun ur-clef (new-clef &rest objects)
  (loop for object in objects do
    (when object
      (if (clef-p object)
	  (copy object new-clef)
	(if (self-acting-p object)
	    (funcall (action object) new-clef (argument object))
	  (if (visible-p object)
	      (push object (marks new-clef))
	    (cmn-warn "odd argument to clef: ~A" object))))))
  new-clef)

(defun cmn-clef (old-clef &key x0 center staff-y0 justification dx dy)
  (let ((new-clef (make-instance 'clef)))
    (copy old-clef new-clef)
    (setf (box-x0 new-clef) x0)
    (setf (box-x1 new-clef) (+ x0 (- (box-x1 old-clef) (box-x0 old-clef))))
    (setf (center new-clef) center)
    (setf (staff-y0 new-clef) staff-y0)
    (setf (visible-justification new-clef) justification)
    (setf (vis-dx new-clef) dx)
    (setf (vis-dy new-clef) dy)
    new-clef))

(defmethod notify ((clef clef) &optional objects)
  (apply #'ur-clef clef objects))

(defmacro define-clef (name base-pitch base-line-note position base-space-note top-space-note sharp-offset flat-offset letter cname drawf bbox)
  `(progn
     (defvar ,name (make-instance 'write-protected-clef 
		       :position ,position
		       :base-pitch ,base-pitch
		       :base-line-note ,base-line-note
		       :base-space-note ,base-space-note
		       :top-space-note ,top-space-note
		       :sharp-offset ,sharp-offset
		       :flat-offset ,flat-offset
		       :letter ,letter
		       :name ,cname
		       :draw-func ,drawf
		       :x0 (first ,bbox)
		       :x1 (third ,bbox)
		       :y0 (second ,bbox)
		       :y1 (fourth ,bbox)
		       :width (- (third ,bbox) (first ,bbox))))
     (defun ,name (&rest args) (apply #'ur-clef (copy ,name) args))))

(define-clef sub-bass 28 2 .25 1 4 8 4 :F-clef :sub-bass #'draw-bass-clef bass-clef-bounds)
(define-clef bass 31 4 0 3 6 6 2 :F-clef :bass #'draw-bass-clef bass-clef-bounds)
(define-clef double-bass 19 4 0 3 6 6 2 :F-clef :double-bass #'draw-bass-clef bass-clef-bounds)
(define-clef quad-bass 7 4 0 3 6 6 2 :F-clef :quad-bass #'draw-bass-clef bass-clef-bounds)
(define-clef baritone-F 35 6 -.25 5 1 4 8 :F-clef :baritone-F #'draw-bass-clef bass-clef-bounds)
(define-clef baritone-C 35 6 .5 5 1 4 7 :C-clef :baritone #'draw-c-clef c-clef-bounds)
(define-clef baritone 35 6 .5 5 1 4 7 :C-clef :baritone #'draw-c-clef c-clef-bounds)
(define-clef tenor 38 1 .25 0 3 2 5 :C-clef :tenor #'draw-c-clef c-clef-bounds)
(define-clef alto 41 3 0 2 5 7 3 :C-clef :alto #'draw-c-clef c-clef-bounds)
(define-clef mezzo-soprano 45 5 -.25 4 0 5 1 :C-clef :mezzo-soprano #'draw-c-clef c-clef-bounds)
(define-clef soprano 48 0 -.5 6 2 3 6 :C-clef :soprano #'draw-c-clef c-clef-bounds)
(define-clef treble 52 2 0 1 4 8 4 :G-clef :treble #'draw-treble-clef treble-clef-bounds)
(define-clef tenor-treble 40 2 0 1 4 8 4 :G-clef :tenor-treble #'draw-treble-clef treble-clef-bounds)
(define-clef double-treble 64 2 0 1 4 8 4 :G-clef :double-treble #'draw-treble-clef treble-clef-bounds)
(define-clef quad-treble 72 2 0 1 4 8 4 :G-clef :quad-treble #'draw-treble-clef treble-clef-bounds)
(define-clef french-violin 55 4 -.25 3 6 2 6 :G-clef :french-violin #'draw-treble-clef treble-clef-bounds)
(define-clef percussion 52 2 .25 1 4 8 4 :no-clef :percussion #'draw-percussion-clef percussion-clef-bounds)

(defmethod display ((clef clef-mixin) container score &optional justifying)
  (declare (ignore container))
  (let ((y0 (+ (staff-y0 clef)
	       (vis-dy clef)
	       (if (eq (clef-letter clef) :G-clef) .25
		 (if (eq (clef-letter clef) :F-clef) .75
		   (if (eq (clef-letter clef) :C-clef) .5
		     0)))))
	(x0 (+ (box-x0 clef) (vis-dx clef))))
    (when (and (member (clef-name clef) '(:tenor-treble :double-bass :double-treble :quad-bass :quad-treble))
	       (not (invisible-p clef)))
      (let ((xscl (if (not (matrix clef)) 1.0 (first (matrix clef))))
	    (yscl (if (not (matrix clef)) 1.0 (fourth (matrix clef))))
	    (xoff (if (eq (clef-name clef) :tenor-treble)
		      .25
		    (if (eq (clef-name clef) :double-bass)
			.125
		      (if (eq (clef-name clef) :quad-bass)
			  0.0
			(if (eq (clef-name clef) :quad-treble)
			    .25
			  .35)))))
	    (yoff (if (eq (clef-name clef) :tenor-treble)
		      -1.0
		    (if (member (clef-name clef) '(:double-bass :quad-bass))
			-.85
		      1.2))))
	(show score (text (if (member (clef-name clef) '(:quad-treble :quad-bass))
			      (if *use-italian-octave-signs*
				  "15ma"
				"15")
			    (if *use-italian-octave-signs*
				"8va"
			      "8"))
			  (font-name (normal-font)) (font-scaler (* xscl .4)))
	      :matrix (translate-matrix score clef (+ x0 (breathing-space clef) (* xscl xoff)) (+ y0 (clef-position clef) (* yscl yoff))))))
    (let ((xx0 (+ x0 (if (not (eq (clef-letter clef) :G-clef)) (breathing-space clef) 0)))
	  (yy0 (+ y0 (clef-position clef))))
      (when (marks clef) (with-position clef xx0 yy0 (display-marks clef score justifying)))
      (show score clef :matrix (translate-matrix score clef xx0 yy0)))))

;;; (cmn (staff double-treble c5 q c6 q) (staff quad-treble d6 q e7 q) (staff double-bass c3 q c2 q) (staff quad-bass c2 q c1 q))

(defvar clef-walls '(.05 .05))
(defvar clef-fences '(.05 .05))

(defmethod house ((clef clef-mixin) score)
  (declare (ignore score))
  (setf (center clef) 0)
  (if (not (invisible-p clef))
      (progn
	(if (not (walls clef)) (setf (walls clef) clef-walls))
	(if (not (fences clef)) (setf (fences clef) clef-fences)))
    (progn
      (setf (box-x0 clef) 0)
      (setf (box-x1 clef) 0)
      (setf (width clef) 0)))
  clef)

;;; (cmn staff (treble (scale 0 0)) (ef-major (scale 0 0)) (meter 3 4) c4 q c4 h c4 h c4 q unmetered c4 w c4 w c4 w)

(defmethod identify ((clef clef-mixin))
  (format nil "(~(~A~)~A)" 
	  (clef-name clef) 
	  (the-usual-suspects clef)))


;;;
;;; ----------------    accidentals
;;;

(defclass accidental-mixin (score-object-mixin) 
  ((sign-name :initarg :sign-name :reader sign-name)))

(defclass accidental (accidental-mixin score-object) 
  ((sign-name :accessor sign-name)))

(defclass write-protected-accidental (write-protect accidental-mixin) ())

(defmethod accidental-p ((obj t)) nil)
(defmethod accidental-p ((obj accidental-mixin)) t)

(defmethod descry ((accidental accidental-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(accidental" "")
	  (if (not controller) (format nil " :sign-name '~A" (sign-name accidental)) "")
	  (if (next-method-p) (call-next-method accidental stream (or controller accidental)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((accidental accidental-mixin) &optional object)
  (let ((new-accidental (if (not object) (make-instance 'accidental)
			  (if (write-protected object) (copy object)
			    object))))
    (if (next-method-p) (call-next-method accidental new-accidental))
    new-accidental))

(defun accidental (&rest objects)
  (apply #'ur-accidental (make-instance 'accidental) objects))

(defun ur-accidental (new-accidental &rest objects)
  (loop for object in objects do
    (when object
      (if (accidental-p object)
	  (setf (sign-name new-accidental) (sign-name object))
	(if (self-acting-p object)
	    (funcall (action object) new-accidental (argument object))
	  (if (visible-p object)
	      (push object (marks new-accidental))
	    (cmn-warn "odd argument to accidental: ~A" object))))))
  new-accidental)

(defmethod notify ((accidental accidental) &optional objects)
  (apply #'ur-accidental accidental objects))

(defmacro define-accidental (name drawf bbox)
  `(progn
     (defvar ,name nil)
     (setf ,name
	 (make-instance 'write-protected-accidental
			:sign-name ',name
			:draw-func #'(lambda (score &optional style)
				       (if (= *accidental-size* 1.0)
					   (funcall ,drawf score style)
					 (progn
					   (matrix-front score (list *accidental-size* 0 0 *accidental-size* 0 0))
					   (funcall ,drawf score style)
					   (matrix-back score))))
			:x0 (first ,bbox)
			:x1 (third ,bbox)
			:y0 (second ,bbox)
			:y1 (fourth ,bbox)
			:width (- (third ,bbox) (first ,bbox))))
     (defun ,name (&rest objects) (apply #'ur-accidental (copy ,name) objects))))

;;; (let ((*accidental-size* 1.4)) (cmn (size 100) cs4 q))
;;; (let ((*accidental-size* 2.0)) (cmn treble (c4 (sharp (scale .5 .5)) q) (cf4 q) (c4 (sharp (scale 1 .2)))))

(define-accidental sharp #'draw-sharp sharp-bounds)
(define-accidental flat #'draw-flat flat-bounds)
(define-accidental natural #'draw-natural natural-bounds)
(define-accidental double-sharp #'draw-double-sharp double-sharp-bounds)
(define-accidental double-flat #'draw-double-flat double-flat-bounds)

(defun draw-half-size (score drawf &optional style)
  (matrix-front score (list .5 0 0 .5 0 0))
  (funcall drawf score style)
  (matrix-back score))
  
(define-accidental small-sharp
                     #'(lambda (score &optional style)
			 (draw-half-size score #'draw-sharp style))
		     (mapcar #'(lambda (n) (* n .5)) sharp-bounds))

(define-accidental small-flat
                     #'(lambda (score &optional style)
			 (draw-half-size score #'draw-flat style))
		     (mapcar #'(lambda (n) (* n .5)) flat-bounds))

(define-accidental small-natural 
                     #'(lambda (score &optional style)
			 (draw-half-size score #'draw-natural style))
		     (mapcar #'(lambda (n) (* n .5)) natural-bounds))

(define-accidental natural-sharp
                     #'(lambda (score &optional style)
			 (draw-natural score style)
			 (matrix-front score (list 1 0 0 1 (* (scr-size score) (+ .05 (width natural))) 0))
			 (draw-sharp score style)
			 (matrix-back score))
		     (let ((bb (map 'list #'(lambda (a b) (+ a b)) natural-bounds sharp-bounds)))
		       (incf (third bb) .05)
		       bb))

(define-accidental natural-flat
                     #'(lambda (score &optional style)
			 (draw-natural score style)
			 (matrix-front score (list 1 0 0 1 (* (scr-size score) (+ .05 (width natural))) 0))
			 (draw-flat score style)
			 (matrix-back score))
		     (let ((bb (map 'list #'(lambda (a b) (+ a b)) natural-bounds flat-bounds)))
		       (incf (third bb) .05)
		       bb))

;;; is natural-natural actually ever used?  what about natural-natural-flat? natural-double-flat?
;;; similar code could implement triple sharp (used by Alkan) = sharp double-sharp

#|
(defun sign-name (sign)
  (if sign
      (if (eq sign sharp) 'sharp
	(if (eq sign flat) 'flat
	  (if (eq sign natural) 'natural
	    (if (eq sign double-sharp) 'double-sharp
	      (if (eq sign double-flat) 'double-flat
		(if (eq sign small-sharp) 'small-sharp
		  (if (eq sign small-flat) 'small-flat
		    (if (eq sign small-natural) 'small-natural
		      sign))))))))))
|#

(defmethod copy ((acc accidental-mixin) &optional object)
  (let ((new-accidental (if (not object) (make-instance 'accidental)
			  (if (write-protected object) (copy object)
			    object))))
    (setf (sign-name new-accidental) (sign-name acc))
    (if (next-method-p) (call-next-method acc new-accidental))
    new-accidental))

(defmethod identify ((accidental accidental-mixin))
  (format nil "(sign ~(~A~)~A)" (sign-name accidental) (the-usual-suspects accidental)))

(defvar accidental-walls '(.025 .025))
(defvar accidental-fences nil)

(defmethod house ((accidental accidental) score)
  (declare (ignore score))
  (setf (center accidental) (* .5 (box-x1 accidental)))
  (if (not (walls accidental)) (setf (walls accidental) accidental-walls))
  (if (not (fences accidental)) (setf (fences accidental) accidental-fences)))


(defvar flats (list flat))
(defvar sharps (list sharp))
(defvar naturals (list natural))
(defvar double-sharps (list double-sharp))
(defvar double-flats (list double-flat natural-flat natural-sharp))


;;;
;;; ----------------    keys
;;;

(defclass key-mixin (clef-mixin score-object-mixin) 
  ((signature :initarg :signature :initform nil :reader signature)))

(defclass key (key-mixin clef score-object) 
  ((signature :accessor signature)))

(defclass write-protected-key (write-protect key-mixin) ())

(defmethod key-p ((obj t)) nil)
(defmethod key-p ((obj key-mixin)) t)

(deferred-action signature)

(defmethod canceled-key-p ((obj t)) nil)
(defmethod canceled-key-p ((obj key-mixin))
  (and (signature obj)
       (third (signature obj))
       (eq (third (signature obj)) :cancel)))

(defmethod descry ((key key-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(key" "")
	  (if (signature key) (format nil " :signature (list ~(~A~) ~D~A)" 
				      (sign-name (first (signature key))) 
				      (second (signature key))
				      (if (third (signature key))
					  (format nil " :~(~A~)" (third (signature key)))
					""))
	    "")
	  (if (next-method-p) (call-next-method key stream (or controller key)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((key key-mixin))
  (format nil "~A(key (signature~A)~A)~A"
	  (if (and (signature key) (eq (third (signature key)) :cancel)) "(cancel " "")
	  (if (signature key) (format nil " (list ~(~A~) ~D)"
				      (sign-name (first (signature key))) 
				      (second (signature key)))
	    " nil")
	  (the-usual-suspects key)
	  (if (and (signature key) (eq (third (signature key)) :cancel)) ")" "")))

(defmethod copy ((key key-mixin) &optional object)
  (let ((new-key (if (not object) (make-instance 'key)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (signature new-key) (signature key))
    (if (next-method-p) (call-next-method key new-key))
    new-key))

(defun key (&rest objects)
  (apply #'ur-key (make-instance 'key) objects))

(defun ur-key (new-key &rest objects)
  (loop for object in objects do
    (when object
      (if (key-p object)
	  (setf (signature new-key) (signature object))
	(if (listp object)		;might be '(sharp 5) explicitly
	    (setf (signature new-key) object)
	  (if (self-acting-p object)
	      (funcall (action object) new-key (argument object))
	    (if (visible-p object)
		(push object (marks new-key))
	      (cmn-warn "odd argument to key: ~A" object)))))))
  new-key)

(defun cmn-key (old-key &key x0 center staff-y0)
  (let ((new-key (make-instance 'key)))
    (copy old-key new-key)
    (setf (box-x0 new-key) x0)
    (setf (box-x1 new-key) (+ x0 (- (box-x1 old-key) (box-x0 old-key))))
    (setf (center new-key) center)
    (setf (staff-y0 new-key) staff-y0)
    new-key))

(defmethod notify ((key key) &optional objects)
  (apply #'ur-key key objects))

(defmacro define-key (name data)
  `(progn
     (defvar ,name (make-instance 'write-protected-key :signature ,data))
     (defun ,name (&rest objects) 
       (apply #'ur-key (make-instance 'key :signature ,data) objects))))

(define-key no-key nil)
(define-key c-major  nil)
(define-key a-minor  nil)
(define-key cs-major (list sharp 7))
(define-key as-minor (list sharp 7))
(define-key df-major (list flat 5))
(define-key bf-minor (list flat 5))
(define-key d-major  (list sharp 2))
(define-key b-minor  (list sharp 2))
(define-key ef-major (list flat 3))
(define-key c-minor  (list flat 3))
(define-key e-major  (list sharp 4))
(define-key cs-minor (list sharp 4))
(define-key f-major  (list flat 1))
(define-key d-minor  (list flat 1))
(define-key fs-major (list sharp 6))
(define-key ds-minor (list sharp 6))
(define-key gf-major (list flat 6))
(define-key ef-minor (list flat 6))
(define-key g-major  (list sharp 1))
(define-key e-minor  (list sharp 1))
(define-key af-major (list flat 4))
(define-key f-minor  (list flat 4))
(define-key a-major  (list sharp 3))
(define-key fs-minor (list sharp 3))
(define-key bf-major (list flat 2))
(define-key g-minor  (list flat 2))
(define-key b-major  (list sharp 5))
(define-key gs-minor (list sharp 5))
(define-key cf-major (list flat 7))
(define-key af-minor (list flat 7))

(defmacro def-key (name &rest accs)
  `(define-key ,name (list :special (list ,@accs))))
;;; (def-key foo fs4 cs4 bf4) -> special case key signature
;;; accidentals are displayed in the order listed. 


(defun cancel (key &rest args)
  (let ((new-key (if (write-protected key)
		     (copy key)
		   key)))
    (setf (signature new-key) (append (signature key) (list :cancel)))
    (apply #'ur-key new-key args)))

(defmethod display ((key key-mixin) container score &optional justifying)
  (declare (ignore container))
  (when (and (signature key)
	     (not (invisible-p key)))
    (let* ((key-sig (signature key))
	   (cancel (and (third key-sig) (eq (third key-sig) :cancel)))
	   (fancy (eq (first key-sig) :special))
	   (x0 (+ (box-x0 key) (vis-dx key)))
	   (y0 (+ (staff-y0 key) (vis-dy key)))
	   (num (if fancy (length (second key-sig)) (second key-sig)))
	   (sharps (eq (first key-sig) sharp))
	   (glf (if cancel 
		    natural 
		  (if (not fancy)
		      (if sharps 
			  sharp 
			flat)
		    (loop for n0 in (second key-sig) collect (note-sign n0)))))
	   (dx (if cancel .2 (if fancy .22 (width glf))))
	   (sls *staff-line-separation*)
	   (y0-offset (if sharps 
			  (clef-sharp-offset key) 
			(clef-flat-offset key)))
	   (jumps (if (not fancy)
		      (if sharps	;the pattern is different if it won't fit on the staff (tenor with sharps for example)
			  (if (> y0-offset 3)
			      '(-3 4 -3 -3 4 -3 0) 
			    '(4 -3 4 -3 4 -3 0))
			(if (< y0-offset 6)
			    '(3 -4 3 -4 3 -4 0)
			  '(-4 3 -4 3 -4 3 0)))
		    (let* ((lines (loop for n0 in (second key-sig) 
				   collect (place-of-note-on-clef n0 treble)))
			   (diffs (loop for n0 in lines by #'cdr and n1 in (cdr lines) by #'cdr 
				   collect (- n1 n0))))
		      (setf y0-offset (- (first lines) (- (clef-sharp-offset treble) (clef-sharp-offset key))))
		      (append diffs (list 0))))))
      ;; the "3" and "6" may be wrong
      (when (marks key) (with-position key x0 y0 (display-marks key score justifying)))
      #-(or gcl sbcl) (loop for i from 0 below num and x from x0 by dx do
	      (let ((y (+ y0 (* sls y0-offset))))
		(incf y0-offset (nth i jumps))
		(if cancel
		    (if (and (> i 0) (plusp (nth i jumps)))
			(incf x .05) 
		      (decf x .1)))
		(show score (if (listp glf) (pop glf) glf) :matrix (translate-matrix score key x y))))
      #+(or gcl sbcl) (do ((i 0 (1+ i))
		 (x x0 (+ x dx)))
		((>= i num))
	      (let ((y (+ y0 (* sls y0-offset))))
		(incf y0-offset (nth i jumps))
		(if cancel
		    (if (and (> i 0) (plusp (nth i jumps)))
			(incf x .05) 
		      (decf x .1)))
		(show score (if (listp glf) (pop glf) glf) :matrix (translate-matrix score key x y))))
      )))

(defvar key-walls '(.05 .05))
(defvar key-fences '(.05 .05))

(defmethod house ((key key-mixin) score)
  (declare (ignore score))
  (let* ((sig (and (not (invisible-p key)) (signature key)))
	 (cancel (and (third sig) (eq (third sig) :cancel)))
	 (special (eq (first sig) :special))
	 (dx (if sig (* (if cancel
			    .2 
			  (if special
			      .22
			    (width (if (eq (first sig) sharp) sharp flat))) )
			(if (not special)
			    (second sig)
			  (length (second sig))))
	       0)))
    (setf (box-x1 key) (+ (box-x0 key) dx))
    (setf (center key) 0)
    (if (not (walls key)) (setf (walls key) (if sig key-walls)))
    (if (not (fences key)) (setf (fences key) (if sig key-fences))))
  key)




;;;
;;; ----------------    meter
;;;

(defclass meter-mixin (staff-relative-mixin score-object-mixin) 
  ((num :initarg :num :initform nil :reader num)
   (den :initarg :den :initform nil :reader den)
   (name :initarg :name :initform nil :reader meter-name)
   (size :initarg :size :initform nil :reader meter-size)
   (style :initarg :style :initform nil :reader meter-style)
   (beaming :initarg :beaming :initform nil :reader meter-beaming)))

(defclass meter (meter-mixin score-object) 
  ((num :accessor num)
   (den :accessor den)
   (name :accessor meter-name)
   (size :accessor meter-size)
   (style :accessor meter-style)
   (beaming :accessor beaming)))

(defclass write-protected-meter (write-protect meter-mixin) ())

(defmethod meter-p ((obj t)) nil)
(defmethod meter-p ((obj meter-mixin)) t)

(deferred-action num)
(deferred-action den)
(deferred-action meter-size)
(deferred-action meter-style)
(deferred-action beaming)

(defmethod descry ((meter meter-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A"
	  (if (not controller) "(meter" "")
	  (if (or (num meter) (den meter)) (format nil " :num ~A :den ~A" (num meter) (den meter)) "")
	  (if (meter-name meter) (format nil " :name ~A" (meter-name meter)) "")
	  (if (meter-size meter) (format nil " :size ~1,3F" (meter-size meter)) "")
	  (if (meter-style meter) (format nil " :style ~(~A~)" (meter-style meter)) "")
	  (if (meter-beaming meter) (format nil " :beaming ~A" (meter-beaming meter)) "")
	  (if (next-method-p) (call-next-method meter stream (or controller meter)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((meter meter-mixin))
  (format nil "(~A~A~A~A~A)"
	  (if (meter-name meter) 
	      (format nil "~(~A~)" (meter-name meter))
	    (format nil "meter ~A ~A" (num meter) (den meter)))
	  (if (meter-size meter) (format nil " (meter-size ~1,3F)" (meter-size meter)) "")
	  (if (meter-style meter) (format nil " (meter-style :~(~A~))" (meter-style meter)) "")
	  (if (meter-beaming meter) 
	      (if (listp (meter-beaming meter)) 
		  (format nil " (beaming '~A)" (meter-beaming meter))
		(format nil " (beaming ~A)" (meter-beaming meter)))
	    "")
	  (the-usual-suspects meter)))

(defmethod copy ((meter meter-mixin) &optional object)
  (let ((new-meter (if (not object) (make-instance 'meter)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (num new-meter) (num meter))
    (setf (den new-meter) (den meter))
    (setf (meter-name new-meter) (meter-name meter))
    (setf (meter-size new-meter) (meter-size meter))
    (setf (meter-style new-meter) (meter-style meter))
    (setf (beaming new-meter) (if (listp (meter-beaming meter)) (copy-list (meter-beaming meter)) (meter-beaming meter)))
    (if (next-method-p) (call-next-method meter new-meter))
    new-meter))

(defun meter (&rest objects)
  (apply #'ur-meter (make-instance 'meter) objects))

(defun ur-meter (new-meter &rest objects)
  (loop for object in objects do
    (when object
      (if (meter-p object)
	  (copy object new-meter)
	(if (self-acting-p object)
	    (funcall (action object) new-meter (argument object))
	  (if (visible-p object)
	      (add-to-marks new-meter (list (if (write-protected object) (copy object) object)))
	    (if (listp object)
		(progn
		  (setf (num new-meter) (first object))
		  (setf (den new-meter) (second object)))
	      (if (not (num new-meter))
		  (setf (num new-meter) object)
		(if (not (den new-meter))
		    (setf (den new-meter) object)
		  (cmn-warn "odd argument to meter: ~A" object)))))))))
  new-meter)

(defmethod notify ((meter meter) &optional objects)
  (apply #'ur-meter meter objects))

(defmacro define-meter (name num den meter-name drawf bbox)
  `(progn
     (defvar ,name (make-instance 'write-protected-meter 
		     :num ,num 
		     :den ,den 
		     :name ,meter-name
		     :draw-func ,drawf
		     :x0 (first ,bbox)
		     :x1 (third ,bbox)
		     :y0 (second ,bbox)
		     :y1 (fourth ,bbox)
		     :width (- (third ,bbox) (first ,bbox))))
     (defun ,name (&rest objects) (apply #'ur-meter (copy ,name) objects))))

(define-meter alla-breve 2 2 :alla-breve #'draw-cut-time cut-time-bounds)
(define-meter common-time 4 4 :common-time #'draw-common-time common-time-bounds)
(define-meter cut-time 2 2 :cut-time #'draw-cut-time cut-time-bounds)

(defvar unmetered (make-instance 'write-protected-meter :num 0 :den 4 :name :unmetered :matrix (list 0 0 0 0 0 0) :justification :none))
(defun unmetered () (make-instance 'meter :num 0 :den 4 :name :unmetered :matrix (list 0 0 0 0 0 0) :justification :none))
(defvar suppressed-denominator
    (make-self-acting :action #'(lambda (nm arg) 
				  (declare (ignore arg)) 
				  (setf (meter-style nm) :suppressed)) 
		      :argument nil))

;;; (cmn staff treble (meter 3 4 (meter-size 2) suppressed-denominator) c4 q)
;;; (cmn staff treble (meter 3 4 note-head-denominator) c4 q)
;;; (cmn (size 100) treble (meter 7 8) bar (meter 1 4) bar (meter 3 6) bar (meter 4 9) bar (meter 2 5))

(defvar note-head-denominator
    (make-self-acting :action #'(lambda (nm arg) 
				  (declare (ignore arg)) 
				  (setf (meter-style nm) :note-head))
		      :argument nil))

(defun text-to-numeral (m)
  (cond ((char= m #\0) (values #'draw-zero   (- (third zero-bounds) (first zero-bounds))))
	((char= m #\1) (values #'draw-one    (- (third one-bounds) (first one-bounds))))
	((char= m #\2) (values #'draw-two    (- (third two-bounds) (first two-bounds))))
	((char= m #\3) (values #'draw-three  (- (third three-bounds) (first three-bounds))))
	((char= m #\4) (values #'draw-four   (- (third four-bounds) (first four-bounds))))
	((char= m #\5) (values #'draw-five   (- (third five-bounds) (first five-bounds))))
	((char= m #\6) (values #'draw-six    (- (third six-bounds) (first six-bounds))))
	((char= m #\7) (values #'draw-seven  (- (third seven-bounds) (first seven-bounds))))
	((char= m #\8) (values #'draw-eight  (- (third eight-bounds) (first eight-bounds))))
	((char= m #\9) (values #'draw-nine   (- (third nine-bounds) (first nine-bounds))))
	((char= m #\+) (values #'draw-plus   (- (third plus-bounds) (first plus-bounds))))
	((char= m #\/) (values #'draw-mslash (- (third mslash-bounds) (first mslash-bounds))))
	(t (warn "can't handle meter text: ~C" m))))
			
(defmethod show ((score score) (obj meter) &key matrix data)
  (let ((len (length data))
	(xwid 0.0))
    (matrix-front score matrix)
    (if (zerop len)
	(with-color score obj
	  (funcall (draw-func obj) score (and (eq (pattern-type obj) :outlined) (pattern-data obj))))
      (loop for i from 0 below len do
	(let* ((m (elt data i))
	       (inner-matrix nil))
	  (if (> xwid 0.0) 
	      (matrix-front score (setf inner-matrix (list 1 0 0 1 (* xwid (scr-size score)) 0)))
	    (setf inner-matrix nil))
	  (multiple-value-bind 
	      (drawf width)
	      (text-to-numeral m)
	    (with-color score obj
	      (funcall drawf score (and (eq (pattern-type obj) :outlined) (pattern-data obj))))
	    (when inner-matrix (matrix-back score))
	    (incf xwid width)))))
    (matrix-back score)))

(defmethod display ((meter meter-mixin) container score &optional justifying)
  ;; meter numerals are weird -- need special (non-text) handling
  (when container
    (setf (box-x0 meter) (box-x0 container))
    (setf (staff-y0 meter) (staff-y0 container)))
  (when (and (or (not justifying)
		 (not (eq (visible-justification meter) :none)))
	     (not (invisible-p meter)))
    (let* ((parens (find-if #'(lambda (n) (and (sundry-p n) (eq :in-parentheses (sundry-name n)))) (marks meter)))
	   (x0 (+ (box-x0 meter) (vis-dx meter) (if parens .1 0)))
	   (y0 (+ (staff-y0 meter) (vis-dy meter)))
	   (size (or (meter-size meter) 1.0)))
      (when (marks meter) (with-position meter x0 y0 (display-marks meter score justifying)))
      (when (or (not (meter-size meter))
		(not (zerop (meter-size meter))))
	(if (meter-name meter)
	    (show score meter :matrix (scale-matrix (translate-matrix score meter x0 (+ y0 (- (* *staff-dy* .5) .025))) size size))
	  (if (eq (meter-style meter) :suppressed)
	      (show score meter 
		    :data (format nil "~A" (num meter)) 
		    :matrix (scale-matrix (translate-matrix score meter x0 (+ y0 (- (* *staff-dy* .5) .025))) size size))
	    (let* ((num-text (format nil "~A" (num meter)))
		   (den-text (format nil "~S" (den meter)))
		   (num-len (length num-text))
		   (den-len (length den-text))
		   (num-offset (* (if (>= num-len den-len) 
				      (if (numberp (num meter))
					  (if (= (num meter) 6)
					      -.0125
					    (if (= (num meter) 1)
						.05
					      0))
					0)
				    (* .15 (- den-len num-len)))
				  size))
		   (den-offset (* (if (>= den-len num-len)
				      (if (numberp (den meter))
					  (if (= (den meter) 1)
					      .05
					    (if (= (den meter) 8)
						.0125 
					      0))
					0)
				    (* .15 (- num-len den-len)))
				  size)))
	      (show score meter
		    :matrix (scale-matrix (translate-matrix score meter (+ x0 num-offset) (+ y0 .51 (* size .25))) size size)
		    :data num-text)
	      (if (not (eq (meter-style meter) :note-head))
		  (show score meter
			:matrix (scale-matrix (translate-matrix score meter (+ x0 den-offset) (- (+ y0 .47) (* size .215))) size size)
			:data den-text)
		(floating-note score (/ 4 (den meter)) nil (translate-matrix score meter x0 (+ y0 .25)))))))))))

#-(or clisp cmu cltl2 excl sbcl openmcl) (defmacro nth-value (n form) `(nth ,n (multiple-value-list ,form)))

(defun text-dx (txt)
  (loop for i from 0 below (length txt)
   sum (nth-value 1 (text-to-numeral (elt txt i)))))

(defvar meter-walls '(.05 .05))
(defvar meter-fences '(.1 .1))
(defvar meter-expanders '(0 2))

(defmethod house ((meter meter) score)
  (declare (ignore score))
  (if (not (invisible-p meter))
    ;; [jack] "~s" doesn't handle string num
      (let* ((num-text (format nil "~A" (num meter)))
	     (den-text (format nil "~S" (den meter)))
	     (dx-num (text-dx num-text))
	     (dx-den (text-dx den-text))
	     (parens (find-if #'(lambda (n) (and (sundry-p n) (eq :in-parentheses (sundry-name n)))) (marks meter))))
	(setf (box-x1 meter) (+ (* (max dx-num dx-den) (or (meter-size meter) 1.0)) (if parens .1 0)))
	(setf (center meter) 0)
	(if (not (walls meter)) (setf (walls meter) meter-walls))
	(if (not (fences meter)) (setf (fences meter) meter-fences))
	(if (not (expanders meter)) (setf (expanders meter) meter-expanders)))
    (progn
      (setf (box-x1 meter) 0)
      (setf (center meter) 0))))

;;; changed 26-May-97 for double meters

(defun algol+ (sym dym)
  (let ((lastden 1.0)
	(beats 0))
    ;; [jack] (format nil "~s" sym) doesn't handle string num
    (with-input-from-string (str (nsubstitute #\  #\+ (format nil "~a" sym)))
      (with-input-from-string (dtr (nsubstitute #\  #\+ (format nil "~a" dym)))
	(loop for num = (read str nil nil nil) and den = (read dtr nil nil nil) while num do
	  (if den (setf lastden den))
	  (incf beats (* 4 (/ num lastden))))))
	  ;(push (* 4 (/ num lastden)) beams))) ;aufo beaming message someday
    beats))
	  
(defun beats-per-measure (meter)
  (if (numberp (num meter))
      (num meter)
    (algol+ (num meter) (den meter))))

(defun beat-duration (meter)
  (/ 4.0 (if (numberp (den meter)) (den meter) 1)))




;;; ----------------    text (class definition in cmn-objects.lisp)

(defmethod show ((score score) (text text) &key matrix data)
  (when (not (invisible-p text))
    (let* ((lfont (font-name text))
	   (ssize (or *cmn-score-size* (scr-size score)))
	   (lsize (or (font-size text) (* ssize (or (font-scaler text) 1.0)))))
      (when (output score)
	(if (< lsize 1.0) (warn "(text ~S: font-size = ~,3F which will be invisible)" (letters text) lsize))
	(matrix-front score matrix)
	(with-color score text
	  (moveto score 0 0)		; need "current point" for PS
	  (g-set-font score lfont lsize)
	  (g-text score (letters text) (eq (pattern-type text) :outlined))
	  (when (eq data :underlined)
	    (g-rmoveto score 0 -2)
	    (g-lineto score 0 -2)
	    (g-draw score)))
	(matrix-back score))
      (when (not (bounded score))
	(setf (box-x1 score) (max (box-x1 score) (* (length (letters text)) .4 (or (font-scaler text) 1.0))))))))

(defmethod display ((text text) container score &optional justifying)
  (if (and (or (not justifying)
	       (not (member (visible-justification text) '(:none :none-left :none-right :none-center))))
	   (not (invisible-p text)))
      (let* ((just (visible-justification text))
	     (letter-size (if (font-size text) 
			      (/ (font-size text) (if justifying
						      *old-cmn-score-size*
						    *cmn-score-size*))
			    (font-scaler text)))
	     (letter-length (* .4 letter-size (length (letters text))))
	     (x-off (if (text-x text)
			(funcall (text-x text) text container score justifying)
		      (+ (box-x0 text) (vis-dx text) 
			 (if container
			     (if (or (not just) (member just '(:left :none :none-left)))
				 (box-x0 container) 
			       (if (member just '(:right :none-right))
				   (- (box-x0 container) letter-length)
				 (if (member just '(:center :none-center))
				     (- (box-x0 container) (* .5 letter-length))
				   (cmn-error "unknown justification: ~A" just))))
			   0))))
	     (y-off (if (text-y text)
			(funcall (text-y text) text container score justifying)
		      (+ (box-y0 text) (vis-dy text) (if container (box-y0 container) 0)))))
	(when (marks text) (with-position text x-off y-off (display-marks text score justifying)))
	(when (not (font-name text)) (setf (font-name text) (normal-font)))
	(if (and container justifying)
	    (moveto score (+ x-off 
			     (if (or (not just) (member just '(:left :none :none-left)))
				 letter-length
			       (if (member just '(:right :none-right))
				   0.0
				 (if (member just '(:center :none-center))
				     (* .5 letter-length)))))
		    y-off)
	  (show score text :matrix (translate-matrix score text x-off y-off) :data (text-style text))))))

;;;(deferred-action font-name)
;;;(deferred-action font-size)
(deferred-action x)
(deferred-action y)

(defun text (&rest objects)
  (apply #'ur-text (make-instance 'text) objects))

(defun cmn-text (&key letters font-name font-size font-scaler)
  (let ((new-text (make-instance 'text)))
    (if letters (setf (letters new-text) letters))
    (if font-name (setf (font-name new-text) font-name))
    (if font-size (setf (font-size new-text) font-size))
    (if font-scaler (setf (font-scaler new-text) font-scaler))
    new-text))

(defmethod staff-name ((val t) &rest args)
  (make-self-acting :action #'(lambda (obj arg)
				(setf (staff-name obj) arg))
		    :argument (apply #'ur-text (cmn-text :letters val) args)))

(defun ur-text (new-text &rest objects)
  (loop for act in objects do
    (when act
      (if (self-acting-p act)
	  (funcall (action act) new-text (argument act))
	(if (stringp act)
	    (setf (letters new-text) act)
	  (if (visible-p act)
	      (push act (marks new-text))
	    (cmn-warn "odd argument to text: ~A" act))))))
  new-text)

(defvar text-walls '(.05 .05))
(defvar text-fences '(.2 .2))
(defvar text-expanders '(5 5))

(defmethod identify ((text text))
  (format nil "(text ~S~A~A~A~A)"
	  (letters text)
	  (if (font-name text) (format nil " (font-name ~S)" (font-name text)) "")
	  (if (font-size text) (format nil " (font-size ~1,3F)" (font-size text)) "")
	  (if (/= (font-scaler text) 1.0) (format nil " (font-scaler ~1,3F)" (font-scaler text)) "")
	  (the-usual-suspects text)))

(defvar unjustified (make-self-acting 
		     :action #'(lambda (obj &rest rest)
				 (declare (ignore rest))
				 (setf (visible-justification obj) :none)
				 nil)
		     :argument nil))



;;; ----------------    pauses -- fermata general-pause (gp, g.p., and grand-pause) caesura breath-mark

;;; someday: text, possibly slurs on pauses

(defclass pause-mixin (score-object-mixin)
  ((name :initform nil :initarg :name :reader pause-name)))

(defclass pause (pause-mixin score-object)
  ((name :accessor pause-name)))

(defclass write-protected-pause (write-protect pause-mixin) ())

(defvar pause-font (bold-font))

(defun pause-1 (&rest objects)
  (apply #'ur-pause (make-instance 'pause) objects))

(defun ur-pause (new-pause &rest objects)
  (loop for object in objects do
    (when object
      (if (pause-p object)
	  (copy object new-pause)
	(if (self-acting-p object)
	    (funcall (action object) new-pause (argument object))
	  (if (visible-p object)
	      (push object (marks new-pause))
	    (cmn-warn "odd argument to pause: ~A" object))))))
  new-pause)

(defmacro define-pause (pname drawf bbox)
  `(progn
     (defvar ,pname (make-instance 'write-protected-pause 
		      :name ',pname 
		      :draw-func ,drawf
		      :x0 (first ,bbox)
		      :x1 (third ,bbox)
		      :y0 (second ,bbox)
		      :y1 (fourth ,bbox)
		      :width (- (third ,bbox) (first ,bbox))))
     (defun ,pname (&rest objects) 
       (apply #'ur-pause (copy ,pname) objects))))

(defun draw-GP (score &optional style)
  (declare (ignore style))
  (show score (text "G.P." (font-name pause-font) (font-scaler 0.5))))

(defvar GP-bounds '(0 0 .3 .1))

(define-pause breath-mark #'draw-breath-mark breath-mark-bounds)
(define-pause general-pause #'draw-GP GP-bounds)
(define-pause g.p. #'draw-GP GP-bounds)
(define-pause grand-pause #'draw-GP GP-bounds)
(define-pause fermata #'draw-fermata fermata-bounds)
(define-pause upside-down-fermata #'draw-upside-down-fermata upside-down-fermata-bounds)
(define-pause hold #'draw-fermata fermata-bounds)
(define-pause caesura #'draw-caesura caesura-bounds)
(define-pause pause  #'draw-caesura caesura-bounds)

(defmethod pause-p ((obj t)) nil)
(defmethod pause-p ((obj pause-mixin)) t)

(defmethod descry ((pause pause-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(pause" "")
	  (if (pause-name pause) (format nil " :name '~(~A~)" (pause-name pause)) "")
	  (if (next-method-p) (call-next-method pause stream (or controller pause)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((pause pause-mixin))
  (format nil "(~(~A~)~A)" (pause-name pause) (the-usual-suspects pause)))

(defmethod copy ((pause pause-mixin) &optional object)
  (let ((new-pause (if (not object) (make-instance 'pause)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (pause-name new-pause) (pause-name pause))
    (if (next-method-p) (call-next-method pause new-pause))
    new-pause))

(defmethod display ((pause pause-mixin) subject score &optional justifying)
  ;; subject can be note chord bar or rest
  ;; pause can be any of those given above
  ;; here we sort out where the damn thing goes relative to the subject
  (when (and (or (not justifying)
		 (not (eq (visible-justification pause) :none)))
	     (not (invisible-p pause)))

    ;; testing
    (cond ((or (rest-p subject) (bar-p subject))
	   (incf (vis-dx pause) (vis-dx subject)) (incf (vis-dy pause) (vis-dy subject)))
	  ((chord-p subject)
	   ;; The following kludge usually works!!
	   (incf (vis-dx pause) (* 0.5 (vis-dx subject)))))
    ;;
    
    (let* ((wid (width pause))
	   (sx0 (+ (box-x0 subject) 
		   (if (not (bar-p subject)) 
		       (if (and (rest-p subject)
				(eq (rest-mark subject) :rest1)				
				(not (member :unmetered (store-data subject))))
			   (* .5 (- (box-x1 subject) (box-x0 subject)))
			 (center subject))
		     0)))
	   (sy0 (box-y0 subject))
	   (sx1 (max sx0 (box-x1 subject)))
	   (x0 (if (member (pause-name pause) '(fermata upside-down-fermata))
		   (- sx0 (* .5 wid))
		 (if (bar-p subject)			 
		     (- sx0 wid)
		   sx1)))
	   (y0 (if (eq (pause-name pause) 'upside-down-fermata)
		   (if (bar-p subject) 
		       (- (box-y0 subject) .3)
		     (+ (staff-y0 subject) (* (min -2 (- (minimum-line subject) 3)) *staff-line-separation*)))
		 (+ (max sy0 (if (bar-p subject) 
				 (box-y1 subject) 
			       (+ (staff-y0 subject) 
				  (* (max 8 
					  (+ 2 (if (and (or (note-p subject) (chord-p subject))
							(stem-is-up subject))
						   3
						 0)
					     (maximum-line subject))) 
				     *staff-line-separation*))))
		    (if (eq (pause-name pause) 'fermata) .25
		      (if (eq (pause-name pause) 'caesura) -.25
			(if (eq (pause-name pause) 'pause) .4
			  .25)))))))
      (let ((xx0 (+ x0 (vis-dx pause)))
	    (yy0 (+ y0 (vis-dy pause))))
	(when (marks pause) (with-position pause xx0 yy0 (display-marks pause score justifying)))
	(show score pause :matrix (translate-matrix score pause xx0 yy0))))))

;;; (cmn (staff (mm 60 (gray-scale .5) (scale 2 1)) treble (c4 q (fermata (gray-scale .5) (scale 2 1)))))

(defvar pause-walls '(.1 .1))
(defvar pause-fences '(.25 .25))
(defvar pause-expanders '(8 8))

(defmethod house ((pause pause) score)
  (declare (ignore score))
  (if (not (invisible-p pause))
      (progn
	(setf (center pause) (* .5 (box-x1 pause)))
	(if (not (walls pause)) (setf (walls pause) pause-walls))
	(if (not (fences pause)) (setf (fences pause) pause-fences))
	(if (not (expanders pause)) (setf (expanders pause) pause-expanders)))
    (setf (box-x1 pause) 0)))




;;;
;;; ----------------    dynamics
;;;

(defclass dynamics-mixin (score-object-mixin)
  ((name :initform nil :initarg :name :reader dynamics-name)
   (mark :initform nil :initarg :mark :reader dynamics-mark)
   (dynamics-spacing :initform nil :initarg :dynamics-spacing :reader dynamics-spacing)
   (justification :initform :none)))

(defclass dynamics (dynamics-mixin score-object)
  ((name :accessor dynamics-name)
   (mark :accessor dynamics-mark)
   (dynamics-spacing :accessor dynamics-spacing)))

(defclass write-protected-dynamics (write-protect dynamics-mixin) ())

(defun dynamics (&rest objects)
  (apply #'ur-dynamics (make-instance 'dynamics) objects))

(defun ur-dynamics (new-dynamics &rest objects)
  (loop for act in objects do
    (when act
      (if (dynamics-p act)
	  (copy act new-dynamics)
	(if (self-acting-p act)
	    (funcall (action act) new-dynamics (argument act))
	  (if (visible-p act)
	      (push act (marks new-dynamics))
	    (cmn-warn "odd argument to dynamics: ~A" act))))))
  new-dynamics)


(defmacro define-dynamics (name true-name mark spacing bbox)
  `(progn
     (defvar ,name (make-instance 'write-protected-dynamics
				  :name ,true-name
				  :mark ,mark
				  :dynamics-spacing ,spacing
				  :x0 (first ,bbox)
				  :x1 (third ,bbox)
				  :y0 (second ,bbox)
				  :y1 (fourth ,bbox)
				  :width (- (third ,bbox) (first ,bbox))))
     (defun ,name (&rest objects) 
       (apply #'ur-dynamics (copy ,name) objects))))

(define-dynamics p :p "p"                 '(0 0)                 '(0.000 -0.148 0.4 0.292))
(define-dynamics piano :p "p"             '(0 0)                 '(0.000 -0.148 0.4 0.292))
(define-dynamics pp :pp "pp"              '(.1 0 0 0)            '(0.000 -0.148 0.7 0.292))
(define-dynamics pianissimo :pp "pp"      '(.1 0 0 0)            '(0.000 -0.148 0.7 0.292))
(define-dynamics ppp :ppp "ppp"           '(.1 0 .1 0 0 0)       '(0.000 -0.148 1.0 0.292))
(define-dynamics pianississimo :ppp "ppp" '(.1 0 .1 0 0 0)       '(0.000 -0.148 1.0 0.292))
(define-dynamics pppp :pppp "pppp"        '(.1 0 .1 0 .1 0 0 0)  '(0.000 -0.148 1.3 0.292))

(define-dynamics f :f "f"                 '(0 0)                 '(0.000 -0.176 0.514 0.418))
(define-dynamics forte :f "f"             '(0 0)                 '(0.000 -0.176 0.514 0.418))
(define-dynamics ff :ff "ff"              '(.2 0 0 0)            '(0.000 -0.176 0.83 0.418))
(define-dynamics fortissimo :ff "ff"      '(.2 0 0 0)            '(0.000 -0.176 0.83 0.418))
(define-dynamics fff :fff "fff"           '(.2 0 .2 0 0 0)       '(0.000 -0.176 1.13 0.418))
(define-dynamics fortississimo :fff "fff" '(.2 0 .2 0 0 0)       '(0.000 -0.176 1.13 0.418))
(define-dynamics ffff :ffff "ffff"        '(.2 0 .2 0 .2 0 0 0)  '(0.000 -0.176 1.44 0.418))

(define-dynamics mezzopiano :mp "mp"      '(.05 0 0 0)           '(0.000 -0.148 0.781 0.292))
(define-dynamics mp :mp "mp"              '(.05 0 0 0)           '(0.000 -0.148 0.781 0.292))
(define-dynamics mezzoforte :mf "mf"      '(.1 0 0 0)            '(0.000 -0.176 0.886 0.418))
(define-dynamics mf :mf "mf"              '(.1 0 0 0)            '(0.000 -0.176 0.886 0.418))
(define-dynamics sf :sf "sf"              '(.15 0 0 0)           '(0.000 -0.176 0.6 0.418))
(define-dynamics sforzato :sf "sf"        '(.15 0 0 0)           '(0.000 -0.176 0.6 0.418))
(define-dynamics sforzando :sf "sf"       '(.15 0 0 0)           '(0.000 -0.176 0.6 0.418))
(define-dynamics sff :sff "sff"           '(.15 0 .2 0 0 0)      '(0.000 -0.176 0.913 0.418))
(define-dynamics sp :sp "sp"              '(.1 0 0 0)            '(0.000 -0.148 0.54 0.293))
(define-dynamics spp :spp "spp"           '(.1 0 .1 0 0 0)       '(0.000 -0.148 0.844 0.293))
 
(define-dynamics fp :fp "fP"              '(.25 -.05 0 0)        '(0.000 -0.176 .668 0.418))
(define-dynamics ffp :ffp "ffP"           '(.2 0 .25 -.05 0 0)   '(0.000 -0.176 .99 0.418))
(define-dynamics forzando :fz "fz"        '(.15 -.05 0 0)        '(0.000 -0.176 .663 0.418))
(define-dynamics fz :fz "fz"              '(.15 -.05 0 0)        '(0.000 -0.176 .663 0.418))
(define-dynamics rfz :rfz "rfz"           '(.15 .03 .15 -.05 0 0) '(0.000 -0.176 0.99 0.418))
(define-dynamics rinforzando :rfz "rfz"   '(.15 .03 .15 -.05 0 0) '(0.000 -0.176 0.99 0.418))
(define-dynamics sfp :sfp "sfP"           '(.15 0 .25 -.05 0 0)  '(0.000 -0.176 0.754 0.418))
(define-dynamics sfz :sfz "sfz"           '(.15 0 .15 -.05 0 0)  '(0.000 -0.176 0.749 0.418))

(define-dynamics niente :niente "n"       '(0 0)                 '(0.000 0 0.15 0.175))
;(define-dynamics n :n "n"                '(0 0)                 '(0.000 0 0.15 0.175))
(define-dynamics subito :I "I"            '(0 0)                 '(0.000 0 0.1 0.175))
(define-dynamics I :I "I"                 '(0 0)                 '(0.000 0 0.1 0.175))

(deferred-action dynamics-mark)

#|
(cmn (free-expansion-factor 3.8)
     (size 24)
     (c4 h (begin-crescendo (begin-dynamic (dynamic "fIp")) (end-dynamic  (dynamic "Ipp" ))))
     (c5 h (stem-up) (end-crescendo))
     (c4 h (begin-crescendo mf (dynamic "Ip")))
     (c5 h (stem-up) (end-crescendo)))
|#
#|
(defun dynamic (n &optional spacing) 
  (if (dynamics-p n)
      n
    (if (stringp n)
	(make-instance 'dynamics :mark n :dynamics-spacing spacing)
      (if (symbolp n)
	  (make-instance 'dynamics :mark (symbol-name n) :dynamics-spacing spacing)))))
|#
;;; this improved version thanks to Anders Vinjar 21-Apr-00
(defun dynamic (n &rest objects) 
  (if (dynamics-p n)
      n
    (let* ((space-list (and (listp (car objects)) (pop objects))))
      (if (stringp n)
          (apply #'ur-dynamics (make-instance 'dynamics :mark n :dynamics-spacing space-list) objects)
        (if (symbolp n)
            (apply #'ur-dynamics (make-instance 'dynamics :mark (symbol-name n) :dynamics-spacing space-list) objects))))))

;;;(cmn treble c4 q (dynamic "fffspz" '(-0.1 0.1)  (dy 2.0)))


(defmethod dynamics-p ((obj t)) nil)
(defmethod dynamics-p ((obj dynamics-mixin)) t)

(defmethod descry ((dynamics dynamics-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A"
	  (if (not controller) "(dynamics" "")
	  (if (dynamics-name dynamics) (format nil " :name ~A" (dynamics-name dynamics)) "")
	  (if (dynamics-mark dynamics) (format nil " :mark ~A" (dynamics-mark dynamics)) "")
	  (if (dynamics-spacing dynamics) (format nil " :spacing ~A" (dynamics-spacing dynamics)) "")
	  (if (next-method-p) (call-next-method dynamics stream (or controller dynamics)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((dynamics dynamics-mixin))
  (format nil "(~(~A~)~A)" 
	  (or (dynamics-name dynamics) (format nil "dynamic ~S" (dynamics-mark dynamics)))
	  (the-usual-suspects dynamics)))

(defmethod copy ((dynamics dynamics-mixin) &optional object)
  (let ((new-dynamics (if (not object) (make-instance 'dynamics)
			(if (write-protected object) (copy object)
			  object))))
    (setf (dynamics-name new-dynamics) (dynamics-name dynamics))
    (setf (dynamics-mark new-dynamics) (dynamics-mark dynamics))
    (setf (dynamics-spacing new-dynamics) (dynamics-spacing dynamics))
    (if (next-method-p) (call-next-method dynamics new-dynamics))
    new-dynamics))

(defvar dynamics-walls '(.05 .05))
(defvar dynamics-fences '(.1 .1))
(defvar dynamics-expanders '(3 3))

(defun text-to-dynamic (m)
  (cond ((char= m #\f) (values #'draw-f (third f-bounds)))
	((char= m #\p) (values #'draw-p (third p-bounds)))
	((char= m #\P) (values #'draw-lig-p (third lig-p-bounds))) ;ligatured version of p
	((char= m #\m) (values #'draw-m (third m-bounds)))
	((char= m #\s) (values #'draw-s (third s-bounds)))
	((char= m #\r) (values #'draw-r (third r-bounds)))
	((char= m #\z) (values #'draw-z (third z-bounds)))
        ((char= m #\n) (values #'draw-niente (third niente-bounds)))
        ((char= m #\I) (values #'draw-subito (third subito-bounds)))
	(t (warn "dynamics: ~C?" m))))

	
(defun text-to-dynamic-spacing (m)
  (cond ((char= m #\f) (list .2 0))
	((char= m #\p) (list .1 0))
	((char= m #\P) (list .25 -.05))
	((char= m #\m) (list .05 0))
	((char= m #\s) (list .1 0))
	((char= m #\r) (list .1 .03))
	((char= m #\z) (list .1 0))
        ((char= m #\n) (list .1 0))
        ((char= m #\I) (list 0.2 0.))
	(t (warn "dynamic: ~C?" m))))
	
(defmethod show ((score score) (obj dynamics) &key matrix data)
  (let ((len (length data))
	(xoff 0.0)
	(space-data (dynamics-spacing obj)))
    (when (not (invisible-p obj))
      (matrix-front score matrix)
      (let ((yoff 0.0)
	    (inner-matrix nil))
	(loop for i from 0 below len do
	  (let* ((dat (elt data i))
		 (wdx (if space-data (first space-data) (first (text-to-dynamic-spacing dat))))
		 (wdy (if space-data (second space-data) (second (text-to-dynamic-spacing dat)))))
	    (if space-data (setf space-data (cddr space-data)))
	    (multiple-value-bind (drawf wid) (text-to-dynamic dat)
	      (if (or (/= xoff 0.0) (/= yoff 0.0))
		  (progn
		    (setf inner-matrix (list 1 0 0 1 (* xoff (scr-size score)) (* yoff (scr-size score))))
		    (matrix-front score inner-matrix))
		(setf inner-matrix nil))
	      (with-color score obj
		(funcall drawf score (and (eq (pattern-type obj) :outlined) (pattern-data obj))))
	      (when inner-matrix (matrix-back score))
	      (incf xoff (- wid wdx))
	      (setf yoff wdy)))))
      (matrix-back score))))

(defmethod house ((dynamics dynamics) score)
  (declare (ignore score))
  (if (not (invisible-p dynamics))
      (progn
	(setf (center dynamics) (* .5 (box-x1 dynamics)))
	(if (not (walls dynamics)) (setf (walls dynamics) dynamics-walls))
	(if (not (fences dynamics)) (setf (fences dynamics) dynamics-fences))
	(if (not (expanders dynamics)) (setf (expanders dynamics) dynamics-expanders)))
    (setf (box-x1 dynamics) 0)))

(defmethod display ((dynamics dynamics) container score &optional justifying)
  ;; there is a specialization on the second arg (as note) in cmn2.lisp
  (when (and (or (not justifying)
		 (not (eq (visible-justification dynamics) :none)))
	     (not (invisible-p dynamics)))
    (let* ((x0 (+ (if container (box-x0 container) 0) (vis-dx dynamics)))
	   (y0 (+ (if container (box-y0 container) 0)
		  (vis-dy dynamics)
		  (if (and container (eq (visible-justification dynamics) :above))
		      (+ (staff-y0 container) (* 10 *staff-line-separation*))
		    0.0)))
	   (dsa *dynamics-size*))
      (when (marks dynamics) (with-position dynamics x0 y0 (display-marks dynamics score justifying)))
      (show score dynamics 
	    :matrix (scale-matrix (translate-matrix score dynamics x0 y0) dsa dsa)
	    :data (dynamics-mark dynamics)))))

;;; (cmn (dynamics-size .5) (staff treble (mf (dy 3.0) (gray-scale .5) (scale 3 1)) (c4 q (ppp (gray-scale .5) (scale 2 1)))))
;;; (cmn staff treble (meter 3 4) (c4 q breath-mark) c4 h p c4 h c4 q unmetered (c4 w (breath-mark (scale 0 0))) (c4 w (p (scale 0 0))) c4 w)
;;; (cmn treble c4 q (dynamic "fffspz" (list .2 0 .2 0 .15 0 .05 0 0 0 0 0)))
;;; spacing here is a list of dx dy values moving the current point after each character by -dx dy

(defun title-space (title)
  (if title 
      (+ 1.0 (if (text-p title) (+ (font-scaler title) (vis-dy title)) 1.0))
    0.0))

(defun make-title (title)
  (make-instance 'sundry 
   :name :title
   :source (format nil "(title ~S)" title)
   :mark #'(lambda (mark ignored score &optional justifying)
	     (declare (ignore ignored))
	     (when (not justifying)
	       (let* ((txt (if (text-p title) title
			     (make-instance 'text
			      :letters title 
			      :font-name (bold-font)
			      :font-scaler 1.0)))
		      (x0 (box-x0 score))
		      (x1 (box-x1 score))
		      (y0 (let ((first-staff (or (find-if #'staff-p (staff-data (first (staves (first (systems score))))))
						 (first (staves (first (systems score)))))))
			    (+ (or (staff-size first-staff) 1.0)
			       (or (title-separation score) 1.0)
			       (box-y0 first-staff))))
		      (cx (* .5 (+ x1 x0)))
		      (ltx (max 0.0 (- cx (* .2 (length (letters txt)))))))
		 (let ((xx0 (+ ltx (vis-dx txt)))
		       (yy0 (+ y0 (vis-dy txt))))
		   (show score txt :matrix (translate-matrix score mark xx0 yy0))
		   (when (marks txt) (with-position txt xx0 yy0 (display-marks txt score justifying)))))))))

(defun make-staff-name ()
  (make-instance 'sundry 
   :name :staff-name 
   :mark #'(lambda (mark staff score &optional justifying)
	     (declare (ignore justifying))
	     (when (staff-name staff)
	       (let* ((nam (staff-name staff))
		      (txt (text-p nam)))
		 (when txt
		   (if (not (font-name nam)) (setf (font-name nam) (staff-name-font score)))
		   (if (and (= (font-scaler nam) 1.0)
			    (not (font-size nam)))
		       (setf (font-scaler nam) (staff-name-font-scaler score))))
		 (let* ((letters (if txt (letters nam) nam))
			(len (length letters))
			(show-flat (and (> len 7) (string-equal "-flat" (subseq letters 1 6))))
			(show-sharp (and (> len 8) (string-equal "-sharp" (subseq letters 1 7))))
			(fname (or (and txt (font-name nam)) (staff-name-font score)))
			(dx-mark (if txt (vis-dx nam) (vis-dx mark)))
			(dy-mark (if txt (vis-dy nam) (vis-dy mark)))
			(fsize (or (and txt (or (font-size nam)
						(and (font-scaler nam)
						     (floor (* (font-scaler nam) (scr-size score))))))
				   (max (staff-name-font-minimum-size score) 
					(floor (* (size score) (staff-name-font-scaler score))))))
			(fscale (/ fsize (scr-size score))))
		   (if (not (staff-name-x0 staff))
		       (setf (staff-name-x0 staff) -1.0))
		   (if (and (not show-flat) (not show-sharp))
		       (let* ((y-up (* -.4 fscale))
			      (matr (translate-matrix score mark 
						      (+ (box-x0 staff) (staff-name-x0 staff) dx-mark)
						      (+ (box-y0 staff) dy-mark .5 y-up))))
			 (if txt
			     (show score nam :matrix matr)
			   (show score (cmn-text :letters letters :font-name fname :font-size fsize) :matrix matr)))
		     (let* ((l0 (subseq letters 0 1))
			    (l1 (subseq letters (if show-flat 6 7)))
			    (y-up (* -.4 fscale))
			    (x0 (+ (box-x0 staff) (staff-name-x0 staff) dx-mark))
			    (y0 (+ (box-y0 staff) dy-mark .5 y-up)))
		       (show score (cmn-text :letters l0 :font-name fname :font-size fsize)
			     :matrix (translate-matrix score mark x0 y0))
		       (show score (if show-flat flat sharp) 
			     :matrix (scale-matrix 
				      (translate-matrix score flat 
							(+ x0 (* (if show-flat .5 .7) fscale))  
							(+ y0 (* (if show-flat .125 .4) fscale))) 
				      fscale fscale))
		       (show score (cmn-text :letters l1 :font-name fname :font-size fsize)
			     :matrix (translate-matrix score mark (+ x0 (* (if show-flat .8 .9) fscale))  y0))))))))))


(defmacro define-accent (name dpyf drawf bbox)
  `(progn
     (defvar ,name (make-instance 'write-protected-sundry
		     :name ',name 
		     :mark ,dpyf
		     :draw-func ,drawf
		     :x0 (first ,bbox)
		     :x1 (third ,bbox)
		     :y0 (second ,bbox)
		     :y1 (fourth ,bbox)
		     :width (- (third ,bbox) (first ,bbox))))
     (defun ,name (&rest objects) (apply #'cmn-mark ,name objects))))

(defun cmn-dot (score x0 y0 &optional (radius .04) openfig)
  (circle score x0 y0 radius 0 360 (not openfig)))

(defun draw-staccato (score &optional style)
  (circle score 0 0 .05 0 360 (not style)))

(defun display-staccato (mark note score &optional justifying)
  (declare (ignore justifying))		;Ross says this should be over the stem if not over the notehead -- contradicted by actual scores
  (let* ((hl (head-line note))
	 (dir (direction-from-note mark note))
	 (y-off (+ .25 (if (and (evenp hl) 
				(or (< 1 hl 9)
				    (and (> hl 9) (eq dir :down))
				    (and (< hl 1) (eq dir :up))))
			   .125 0)))
	 (y0 (+ (staff-y0 note) (* hl *staff-line-separation*))))
    (show score mark :matrix (translate-matrix score mark 
					       (+ (box-x0 note) (box-x0 mark) (vis-dx mark) (center note))
					       (+ y0 (box-y0 mark) (vis-dy mark) (if (eq dir :up) y-off (- y-off)))))))

(define-accent staccato #'display-staccato #'draw-staccato '(0 0 .1 .1))
(defun staccato-p (object) (and object (sundry-p object) (eq (sundry-name object) 'staccato)))


(defun display-accent (mark note score &optional justifying)
  (declare (ignore justifying))
  (show score mark
	:matrix (translate-matrix
		 score mark
		 (+ (box-x0 note) (vis-dx mark) (center note) -.2 (x0 mark))
		 (+ (box-y0 mark) (vis-dy mark) 
		    (if (member (visible-justification mark) '(:down :below))
			(+ (staff-y0 note) (min (* (- (head-line note) 4) *staff-line-separation*) -.5))
		      (max (+ (or (stem-end note) 0) .125)
			   (+ (staff-y0 note)
			      (* (max 10 (+ 3 (head-line note))) 
				 *staff-line-separation*))))))))

(define-accent accent #'display-accent #'draw-accent accent-bounds)


(defun display-little-swell (mark note score &optional justifying)
  (let* ((dyn (and (marks mark) (find-if #'dynamics-p (marks mark))))
	 (y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		   (if (member (visible-justification mark) '(:down :below))
		       (min (* (- (head-line note) 2) *staff-line-separation*) -.5)
		     (* (max 10 (+ 3 (head-line note))) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) (vis-dx mark) (center note) -.1 (if dyn -.3 0) (x0 mark)))
	 (xx0 .5))
    (matrix-front score (translate-matrix score mark (- x-off .3) y-off))
    (with-color score mark (draw-tnecca score))
    (when dyn
      (with-position mark .4 0 (display-marks mark score justifying))
      (setf xx0 (+ 0.4 (width dyn)))
      ;; why is this being drawn twice?!? We need to leave it on the marks list for (unlikely) subsequent identify etc
      (if (not justifying)
	  (setf (matrix dyn) (list 0 0 0 0 0 0))))
    (matrix-front score (list 1 0 0 1 (* xx0 (scr-size score)) 0))
    (draw-accent score)
    (matrix-back score)
    (matrix-back score)))

(define-accent little-swell #'display-little-swell nil (map 'list #'(lambda (a b) (+ a b)) accent-bounds tnecca-bounds))


(defun display-wedge (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
	 (dir (direction-from-note mark note))
	 (y-off (+ (box-y0 mark) (vis-dy mark) 
		   (staff-y0 note) 
		   (* (+ (if (eq dir :up) 3 -3) 
			 hl 
			 (if (= hl 3) 1 (if (= hl 5) -1 0)))
		      *staff-line-separation*)))
	 (x-off (+ (box-x0 note) (vis-dx mark) .1
		   (center note) (box-x0 mark)))
	 (matr (translate-matrix score mark x-off y-off)))
    (show score mark :matrix (if (eq dir :up) matr (flip-matrix matr)))))

(define-accent wedge #'display-wedge #'draw-wedge wedge-bounds)


(defun display-tenuto (mark note score &optional justifying)
  (declare (ignore justifying))
  ;; tenuto gives way only to staccato
  (let* ((hl (head-line note))
	 (staccato (find-if #'staccato-p (marks note)))
	 (mark-length (+ .3 (if (whole-note-p note) .1 0)))
	 (y-off (+ .25 (if staccato .2 0) (if (and (evenp hl) (< 1 hl 8)) .125 0)))
	 (y0 (+ (staff-y0 note) (vis-dy mark) (* hl *staff-line-separation*))))
    (matrix-front score (translate-matrix score mark 
					  (+ (box-x0 note) (center note) (* -.5 mark-length) (vis-dx mark) (x0 mark))
					  (+ y0 (box-y0 mark) (if (eq (direction-from-note mark note) :up) y-off (- y-off)))))
    (comment score "tenuto")
    (with-thickness score mark .05
      (moveto score 0 0)
      (rlineto score mark-length 0)
      (draw score))
    (matrix-back score)))

(define-accent tenuto #'display-tenuto nil '(0 0 .3 .05))
(defun tenuto-p (object) (and object (sundry-p object) (eq (sundry-name object) 'tenuto)))


(defun display-down-bow (mark note score &optional justifying)
  (when (not justifying)
    (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) -.15
		     (max (if (and (< 1 (head-line note) 5)
				   (not (whole-note-p note)))
			      (* (+ 9 (head-line note)) *staff-line-separation*)
			    0.0)
			  (* (max 10 (+ 3 (head-line note))) *staff-line-separation*))))
	   (x-off (+ (box-x0 note) (vis-dx mark) (if (not (whole-note-p note)) -.15 -.2) (center note) (box-x0 mark))))
      (show score mark :matrix (translate-matrix score mark x-off y-off)))))

(define-accent down-bow #'display-down-bow #'draw-down-bow down-bow-bounds)


(defun display-up-bow (mark note score &optional justifying)
  (when (not justifying)
    (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		     (max (if (and (< 1 (head-line note) 5)
				   (not (whole-note-p note)))
			      (* (+ 8 (head-line note)) *staff-line-separation*)
			    0.0)
			  (* (max 9 (+ 3 (head-line note))) *staff-line-separation*))))
	   (x-off (+ .075 (box-x0 note) (vis-dx mark) (if (not (whole-note-p note)) -.05 -.1) (center note) (box-x0 mark))))
      (show score mark :matrix (translate-matrix score mark x-off y-off)))))

(define-accent up-bow #'display-up-bow #'draw-up-bow up-bow-bounds)


(defun display-detache (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
	 (mark-length (+ .3 (if (whole-note-p note) .1 0)))
	 (y-off (+ .25 (if (and (evenp hl) (< 1 hl 8)) .125 0)))
	 (y0 (+ (staff-y0 note) (* hl *staff-line-separation*))))
    (matrix-front score	(translate-matrix score mark 
					  (+ (box-x0 note) (vis-dx mark) (center note) (* -.5 mark-length) (box-x0 mark))
					  (+ y0 (box-y0 mark) (vis-dy mark) (if (eq (direction-from-note mark note) :up) y-off (- y-off)))))
    (comment score "detache")
    (with-thickness score mark .01
      (moveto score 0 0)
      (lineto score mark-length 0)
      (draw score))
    (matrix-back score)))

(define-accent detache #'display-detache nil '(0 0 .3 .01))


(defun display-martele (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((dir (direction-from-note mark note))
	 (y-off (+ (box-y0 mark) (vis-dy mark) 
		   (staff-y0 note) 
		   (if (eq dir :up) 
		       (* (max 11 (+ 3 (head-line note))) *staff-line-separation*)
		     (* (min -3 (- (minimum-line note) 3)) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) (vis-dx mark) (center note) (box-x0 mark)))
	 (dy (if (eq dir :up) .2 -.2)))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "martele")
    (with-thickness score mark .01
      (moveto score -.1 dy)
      (rlineto score .1 (- dy))
      (rlineto score .1 dy)
      (draw score))
    (matrix-back score)))

(define-accent martele #'display-martele nil '(-.1 0 .1 .2))


(defun display-marcato (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((dir (direction-from-note mark note))
	 (y-off (+ (box-y0 mark) (vis-dy mark) 
		   (staff-y0 note) 
		   (if (eq dir :up) 
		       (* (max 11 (+ 3 (head-line note))) *staff-line-separation*)
		     (* (min -3 (- (minimum-line note) 3)) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) (vis-dx mark) .1 (center note) (box-x0 mark)))
	 (dy (if (eq dir :up) -.2 .2)))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "marcato")
    (with-thickness score mark .025
      (moveto score -.1 dy)
      (rlineto score .1 (- dy))
      (rlineto score .1 dy)
      (draw score))
    (matrix-back score)))

(define-accent marcato #'display-marcato nil '(-.1 0 .1 .2))


(defun display-bartok-pizzicato (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) 
		   (max (if (and (< 1 (head-line note) 5)
				 (not (whole-note-p note)))
			    (* (+ 8 (head-line note)) *staff-line-separation*)
			  0.0)
			(* (max 10 (+ 4 (head-line note))) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) (vis-dx mark) .1 (center note) (box-x0 mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "bartok pizz")
    (with-thickness score mark .025
      (circle score 0 0 .15 0 360 nil)
      (moveto score 0 0)
      (rlineto score 0 .25)
      (draw score))
    (matrix-back score)))

(define-accent bartok-pizzicato #'display-bartok-pizzicato nil '(-.075 0 .075 .25))
(define-accent snap-pizzicato #'display-bartok-pizzicato nil '(-.075 0 .075 .25))


(defun display-thumb (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) .1
		   (max (if (and (< 1 (head-line note) 5)
				 (not (whole-note-p note)))
			    (* (+ 8 (head-line note)) *staff-line-separation*)
			  0.0)
			(* (max 10 (+ 4 (head-line note))) *staff-line-separation*))))
	 (x-off (+ (box-x0 note) (vis-dx mark) .1 (center note) (box-x0 mark))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (comment score "thumb")
    (with-thickness score mark .025
      (circle score 0 0 .15 0 360 nil)
      (moveto score 0 0)
      (rlineto score 0 -.25)
      (draw score))
    (matrix-back score)))

(define-accent thumb  #'display-thumb nil '(-.075 0 .075 .25))


(defgeneric display-in-parentheses (mark note score &optional justifying) )

(defun draw-both-parens (score mark x0 y0 x1 scl)
  (matrix-front score (scale-matrix (translate-matrix score mark x0 y0) scl scl))
  (comment score "parens")
  (draw-left-paren score)
  (matrix-back score)
  (matrix-front score (scale-matrix (translate-matrix score mark x1 y0) scl scl))
  (draw-right-paren score)
  (matrix-back score))

(defmethod display-in-parentheses ((mark sundry-mixin) (note accidental-mixin) score &optional justifying)
  (declare (ignore justifying))
  (draw-both-parens score mark (- (box-x0 note) .2) (- (box-y0 note) .2) (+ (box-x0 note) .2) 1))

#|
(defmethod display-in-parentheses ((mark sundry-mixin) (note meter-mixin) score &optional justifying)
  (declare (ignore justifying))
  (let* ((x0 (- (+ (box-x0 note) (vis-dx note)) (+ (vis-dx mark) .26)))
	 (x1 (+ (box-x1 note) (vis-dx mark) (vis-dx note) -.05))
	 (y (+ (vis-dy mark) (staff-y0 note))))
    (draw-both-parens score mark x0 y x1 2)))
|#

;; from AV 14-Dec-00
;;(meter 3 4  (dx 1.0) (dy 1.0) in-parentheses)

(defmethod display-in-parentheses ((mark sundry-mixin) (note meter-mixin) score &optional justifying)
  (declare (ignore justifying))
  (let* ((x0 (- (box-x0 note) .26))
	 (x1 (+ (box-x1 note)  (vis-dx note) -.05))
	 (y (+ (vis-dy note) (staff-y0 note))))
    (draw-both-parens score mark x0 y x1 2)))

;;; see cmn2.lisp for audible-mixin version of this method

(defmethod display-in-parentheses ((mark sundry-mixin) (note t) score &optional justifying)
  (declare (ignore justifying))
  (let* ((x0 (- (+ (box-x0 note) (vis-dx mark)) .25))
	 (x1 (+ (box-x1 note) .1 (vis-dx mark)))
	 (y (+ (vis-dy mark) (if (plusp (box-y0 note)) (box-y0 note) (staff-y0 note)))))
    (draw-both-parens score mark x0 y x1 1)))

(defmethod display-in-parentheses ((mark sundry-mixin) (dyn dynamics-mixin) score &optional justifying)
  (declare (ignore justifying))
  (let* ((x0 (- (+ (box-x0 dyn) (vis-dx mark)) .15))
	 (x1 (+ x0 (width dyn)))
	 (y (+ -.125 (vis-dy mark) (box-y0 dyn))))
    (draw-both-parens score mark x0 y x1 *dynamics-size*)))

#|
(cmn staff treble (c4 q (p in-parentheses)) (c4 q (pp in-parentheses)) (c4 q (sp in-parentheses)) 
  (c4 q (spp in-parentheses)) (c4 q (f in-parentheses)) (c4 q (ff in-parentheses)) 
  (c4 q (fp in-parentheses)) (c4 q (rfz in-parentheses)) (c4 q (sff in-parentheses))) 
|#

(defun in-parentheses (&rest objects)
  (let ((new-paren (make-instance 'sundry :name :in-parentheses :mark #'display-in-parentheses)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-paren (argument act))))
    new-paren))

(defvar in-parentheses (make-instance 'write-protected-sundry :name :in-parentheses :mark #'display-in-parentheses))


(defun display-left-hand-pizzicato (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
	 (dir (direction-from-note mark note))
	 (y0 (+ (box-y0 mark) (vis-dy mark) 
		(staff-y0 note) 
		(if (eq dir :up) 
		    (* (max 10 (+ 4 hl)) *staff-line-separation*)
		  (* (min -2 (- (minimum-line note) 3)) *staff-line-separation*))))
	 (x0 (+ (box-x0 note) (vis-dx mark) (box-x0 mark) (center note) -.15)))
    (matrix-front score (translate-matrix score mark x0 y0))
    (comment score "left hand pizz")
    (with-thickness score mark .04
      (moveto score 0 0)
      (lineto score 0.3 0.0)
      (moveto score .16 -.14)
      (rlineto score 0.0 .28)
      (draw score))
    (matrix-back score)))

(define-accent left-hand-pizzicato #'display-left-hand-pizzicato nil '(0 -.14 .3 .14))
(define-accent stopped-note #'display-left-hand-pizzicato nil '(0 -.14 .3 .14))

(defun display-natural-harmonic (mark note score &optional justifying)
  (declare (ignore justifying))
  (let* ((hl (head-line note))
	 (y-off (+ .3 
		   (if (find-if #'tenuto-p (marks note)) .15 0)
		   (if (and (evenp hl) (< 1 hl 9)) .075 0))) ;was .125
	 (y0 (+ (staff-y0 note) (* hl *staff-line-separation*))))
    (matrix-front score (translate-matrix score mark 
					  (+ (box-x0 note) (box-x0 mark) .05 (vis-dx mark) (center note))
					  (+ y0 (box-y0 mark) (vis-dy mark) (if (eq (direction-from-note mark note) :up) y-off (- y-off)))))
    (comment score "natural harmonic")
    (circle score 0 0 .08 0 360 nil)
    (matrix-back score)))

(define-accent natural-harmonic #'display-natural-harmonic nil '(-.05 -.05 .05 .05))
(define-accent open-note #'display-natural-harmonic nil '(-.05 -.05 .05 .05))
  
#|
;;; test cases
(cmn staff treble 
  a3 q staccato d4 h staccato b4 w staccato e5 q staccato d6 q staccato as3 q staccato df4 q staccato fs5 w staccato
  a3 q accent d4 h accent  b4 w accent e5 q accent  d6 q accent as3 q accent df4 q accent fs5 w accent
  a3 q little-swell d4 h little-swell  b4 w little-swell e5 q little-swell  d6 q little-swell 
    as3 q little-swell df4 q little-swell fs5 w little-swell
  a3 q wedge d4 h wedge  b4 w wedge e5 q wedge  d6 q wedge as3 q wedge df4 q wedge fs5 w wedge
  a3 q tenuto d4 h tenuto  b4 w tenuto e5 q tenuto  d6 q tenuto as3 q tenuto df4 q tenuto fs5 w tenuto
  a3 q marcato d4 h marcato  b4 w marcato e5 q marcato  d6 q marcato as3 q marcato df4 q marcato fs5 w marcato
  a3 q down-bow d4 h down-bow  b4 w down-bow e5 q down-bow  d6 q down-bow as3 q down-bow df4 q down-bow fs5 w down-bow
  a3 q up-bow d4 h up-bow  b4 w up-bow e5 q up-bow  d6 q up-bow as3 q up-bow df4 q up-bow fs5 w up-bow
  a3 q detache d4 h detache  b4 w detache e5 q detache  d6 q detache as3 q detache df4 q detache fs5 w detache
  a3 q martele d4 h martele  b4 w martele e5 q martele  d6 q martele as3 q martele df4 q martele fs5 w martele
  a3 q thumb d4 h thumb  b4 w thumb e5 q thumb  d6 q thumb as3 q thumb df4 q thumb fs5 w thumb
  a3 q natural-harmonic d4 h natural-harmonic  b4 w natural-harmonic e5 q natural-harmonic  d6 q natural-harmonic 
    as3 q natural-harmonic df4 q natural-harmonic fs5 w natural-harmonic
  a3 q bartok-pizzicato d4 h bartok-pizzicato  b4 w bartok-pizzicato e5 q bartok-pizzicato  d6 q bartok-pizzicato 
    as3 q bartok-pizzicato df4 q bartok-pizzicato fs5 w bartok-pizzicato
  a3 q stopped-note d4 h stopped-note  b4 w stopped-note e5 q stopped-note  d6 q stopped-note 
    as3 q stopped-note df4 q stopped-note fs5 w stopped-note
  a3 q open-note d4 h open-note  b4 w open-note e5 q open-note  d6 q open-note as3 q open-note df4 q open-note fs5 w open-note
  a3 q left-hand-pizzicato d4 h left-hand-pizzicato  b4 w left-hand-pizzicato e5 q left-hand-pizzicato  d6 q left-hand-pizzicato 
    as3 q left-hand-pizzicato df4 q left-hand-pizzicato fs5 w left-hand-pizzicato
  )
|#

;; handled via define-accent for simplicity

(defun display-pedal (mark note score &optional justifying)
  (declare (ignore justifying))
  (let ((y-off (+ (min (- (staff-y0 note) .75) 
		       (- (box-y0 note) (if (stem-is-down? note) 1.5 .75)))
		  (vis-dy mark)))
	(x-off (+ (box-x0 note) -.25 (vis-dx mark) (box-x0 mark))))
    (show score mark :matrix (translate-matrix score mark x-off y-off))))

(define-accent pedal #'display-pedal #'draw-ped ped-bounds)


(defun display-pedal-off (mark note score &optional justifying)
  (declare (ignore justifying))
  (let ((y-off (+ (min (- (staff-y0 note) .75) 
		       (- (box-y0 note) (if (stem-is-down? note) 1.5 .75)))
		  (vis-dy mark)))
	(x-off (+ (box-x0 note) -.25 (vis-dx mark) (box-x0 mark))))
    (show score mark :matrix (translate-matrix score mark x-off y-off))))

(define-accent pedal-off #'display-pedal-off #'draw-pedal-off pedal-off-bounds)

;;; (cmn (size 40) treble c4 q pedal c4 q c4 q c4 q pedal-off)

(defun display-segno (mark bar score &optional justifying)
  (declare (ignore justifying))
  (let ((y-off (+ (box-y0 bar) (vis-dy mark) (* 12 *staff-line-separation*)))
	(x-off (+ (box-x0 bar) (vis-dx mark) (center bar) -.25 (box-x0 mark))))
    (show score mark :matrix (translate-matrix score mark x-off y-off))))

(define-accent segno #'display-segno #'draw-segno segno-bounds)


(defun display-coda (mark bar score &optional justifying)
  (declare (ignore justifying))
  (let ((y-off (+ (box-y0 bar) (vis-dy mark) (* 12 *staff-line-separation*))) 
	(x-off (+ (box-x0 bar) (vis-dx mark) (center bar) -.25 (box-x0 mark))))
    (show score mark :matrix (translate-matrix score mark x-off y-off))))

(define-accent coda #'display-coda #'draw-coda coda-bounds)


(defun change-beat (old-beat new-beat &rest objects)
  (let* ((oq (quarters old-beat))
	 (nq (quarters new-beat))
	 (new-cb (make-instance 'sundry 
	      :name :change-beat
	      :source (format nil "(change-beat ~A ~A)" old-beat new-beat)
	      :mark #'(lambda (mark note score &optional justifying)
			(let ((x0 (+ (box-x0 note) (vis-dx mark) (center note) -.5 
				     (if (< oq 1.0) -.25 0)))
			      (y0 (+ (box-y0 note) (vis-dy mark) (* 10 *staff-line-separation*))))
			  (when (and (marks mark) (not justifying))
			    (setf (box-x0 mark) x0)
			    (setf (box-x1 mark) (+ x0 .5))
			    (setf (box-y0 mark) y0))
			  (floating-note score oq t (translate-matrix score mark x0 y0 .75))
			  (matrix-front score (translate-matrix score mark (+ x0 .4) y0))
			  (show score (text " = " (font-name (normal-font)) (font-scaler .6)))
			  (matrix-back score)
			  (floating-note score nq t (translate-matrix score nil (+ x0 .75) y0 .75)))))))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-cb (argument act))
	  (if (visible-p act)
	      (push act (marks new-cb))))))
    new-cb))


(defun mm (num &rest objects)
  (let ((new-mm (make-instance 'sundry :name :mm :mark nil :source (format nil "(mm ~D)" num)))
	(beat nil))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mm (argument act))
	  (if (rhythm-p act)
	      (setf beat act)
	    (if (visible-p act)
		(push act (marks new-mm)))))))
    (let ((hq (or (and beat (quarters beat)) 1)))
      (setf (sundry-mark new-mm)
	#'(lambda (mark note score &optional justifying)
	    (let* ((x0 (+ (vis-dx mark) (if note (box-x0 note) (box-x0 mark))))
		   (y0 (+ (if note (box-y0 note) (staff-y0 mark))
			  (vis-dy mark)
			  (* 12 *staff-line-separation*))))
	      (when (and (marks mark) 
			 (not justifying))
		(setf (box-x0 mark) x0)
		(setf (box-x1 mark) (+ x0 .5 (* .2 (1+ (length (format nil "= ~D" num))))))
		(setf (box-y0 new-mm) y0))
	      (floating-note score hq t (translate-matrix score new-mm x0 y0 .75))
	      (matrix-front score (translate-matrix score new-mm (+ x0 (* .4 (if (matrix new-mm) (first (matrix new-mm)) 1.0))) y0))
	      (show score (text (format nil "= ~D" num) (font-name (normal-font)) (font-scaler .5)))
	      (matrix-back score)))))
    new-mm))

;;; (cmn (size 18) staff treble c4 q (mm 120) c4 q (mm 120 (scale 2 2)))


;;;________________________________________________________________________________
;;; (AV 3-Jul-00)
;;; displays mouth-position marks for vocal parts.  'Mouthpos is
;;; expressed as either:
;;;   - integer steps (between 0 and 5)
;;;   - float factor (between 0.0 1.0)
;;;   - one of the symbols :CLOSED :SLIGHT :OPEN :WIDE :PURSED or :NASAL
;;;   or a list of one of the above indications together with a float describing
;;;   the 'width of the mouth-position (normal is 0.5, pursed is 0.3)

#|
(cmn (free-expansion-factor 3.0)
     (c4 h begin-slur (mouth-position :pursed) (text-> "" (dy 1.25) (dx 0.1)))
     (c4 h end-slur (mouth-position :nasal) (-text "" (dy 1.25) (dx -0.6))))
|#

(defvar mouth-wide-open-height 0.7)
(defvar mouth-wide-open-width 0.6)

(defun display-mouth-position (mark note score &optional (mouthpos 3))
  (let* ((y-off (+ (y0 mark) (dy mark) (staff-y0 note)
                   (* (max 12 (+ 4 (head-line note))) *staff-line-separation*)))
         (x-off (+ (x0 note) -.25 (dx mark) (center note) (x0 mark)))
         (wide-open-height mouth-wide-open-height)
         (mp-height (if (listp mouthpos) (car mouthpos) mouthpos))
         (height
          (cond ((floatp mp-height) (* mp-height wide-open-height))
                ((integerp mp-height) (* (case mp-height
                                           (0 0.2)
                                           (1 0.4)
                                           (2 0.7)
                                           (3 1.0))
                                         wide-open-height))
                (t (* wide-open-height
                      (case mp-height
                        (:closed 0.2)
                        (:slight 0.4)
                        (:open 0.7)
                        (:wide 1.0)
                        (:pursed 0.7)
                        (:nasal 0.9)
                        (t (error "wrong mouth-position")))))))
         (width (cond
                 ((eql mp-height :pursed) 0.3)
                 ((eql mp-height :nasal) 0.35)
                 ((and (listp mouthpos) (cadr mouthpos)) (cadr mouthpos))
                 (t mouth-wide-open-width))))
    (matrix-front score (translate-matrix score mark x-off y-off))
    (if (eql mp-height :nasal)
	(progn
	  (setf (line-width score) 0.05)
	  (moveto score (/ width 2.0) 0)
	  (lineto score width 0)
	  (lineto score (/ width 2.0) height)
	  (lineto score 0 0)
	  (lineto score (/ width 2.0) 0)
	  (draw score))
      (progn
	(setf (line-width score) 0.17)
	(moveto score 0 0)
	(rlineto score width 0)
	(moveto score 0 height)
	(rlineto score width 0)
	(draw score)
	(setf (line-width score) 0.02)
	(moveto score 0.01 0)
	(rlineto score 0 height)
	(moveto score (- width 0.01) 0)
	(rlineto score 0 height)
	(draw score)
	(setf (line-width score) 0.0)
	))
    (matrix-back score)))

(defvar mouth-position
  (make-instance 'write-protected-sundry
                 :name :mouth-position
                 :mark #'(lambda (mark note score &optional justifying)
                           (declare (ignore justifying))
                           (display-mouth-position mark note score))))

(defun mouth-position (siz &rest objects)
  (apply #'mark #'(lambda (mark note score &optional justifying)
                    (declare (ignore justifying))
                    (display-mouth-position mark note score siz))
         :mouth-position objects))


(defun display-swirl (mark note score &optional size)
  (declare (ignore size))
  (let* ((note-y-off (+ (y0 mark) (dy mark) (staff-y0 note)
			(* (min 11 (- (head-line note) 4)) *staff-line-separation*)
			-0.2
			))
	 (note-x-off (+ (x0 note) -.2 (dx mark) (center note) (center mark)))
	 (skl 1.6))
    (with-thickness score mark .03
		    (loop for x-off from note-x-off by (* skl 0.24)
		      repeat 4 do
		      (moveto score x-off note-y-off)
		      (curveto score
			       (+ x-off (* skl 0.2)) (+ note-y-off (* skl 0.02))
			       (+ x-off (* skl 0.2)) (+ note-y-off (* skl 0.2))
			       (+ x-off (* skl 0.1)) (+ note-y-off (* skl 0.2)))
		      (curveto score
			       (+ x-off (* skl 0.0)) (+ note-y-off (* skl 0.2))
			       (+ x-off (* skl 0.0)) (+  note-y-off (* skl 0.0))
			       (+ x-off (* skl 0.24)) note-y-off))
		    (draw score)
		    )))

(defvar swirl
  (make-instance 'write-protected-sundry 
		 :name :swirl :mark #'display-swirl))

(defun swirl (&rest objects) 
  (apply #'mark #'display-swirl :swirl  objects))

;;; ----------------------------------------------------------------


;;;
;;; ----------------    ornamentation

(defclass ornament-mixin (sundry-mixin)
  ((sign :initarg :sign :initform nil :reader ornament-sign))) ;sign is sharp flat and so on

(defclass write-protected-ornament (write-protect ornament-mixin) () )

(defclass ornament (ornament-mixin sundry)
  ((sign :accessor ornament-sign)))

(deferred-action ornament-sign)

(defmethod descry ((ornament ornament-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) (format nil "(~(~A~)" (sundry-name ornament)) "")
	  (if (ornament-sign ornament) (format nil " :sign ~A" (descry (ornament-sign ornament))) "")
	  (if (next-method-p) (call-next-method ornament stream (or controller ornament)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((ornament ornament-mixin))
  (format nil "(~(~A~)~A~A)" 
	  (sundry-name ornament) 
	  (if (ornament-sign ornament) (format nil " (ornament-sign ~A)" (sign-name (ornament-sign ornament))) "")
	  (the-usual-suspects ornament)))

(defmethod copy ((ornament ornament-mixin) &optional object)
  (let ((new-ornament (if (not object) (make-instance 'ornament)
			(if (write-protected object) (copy object)
			  object))))
    (setf (sundry-name new-ornament) (sundry-name ornament))
    (setf (sundry-mark new-ornament) (sundry-mark ornament))
    (if (next-method-p) (call-next-method ornament new-ornament))
    new-ornament))

(defun display-ornament (mark drawf bounds note score &optional (count 1) other-drawf other-bounds)
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) (* (max 10 (+ 3 (head-line note))) *staff-line-separation*)))
	 (x-off (+ (box-x0 note) (vis-dx mark) (center note) (box-x0 mark)))
	 (dy (fourth bounds))
	 (xd x-off)
	 (yd y-off)
	 (inner-matrix nil)
	 (matr (translate-matrix score mark x-off y-off)))
    (matrix-front score matr)
    (loop for i from 0 below count do
      (when inner-matrix (matrix-front score inner-matrix))
      (funcall drawf score)
      (when inner-matrix (matrix-back score))
      (when (> count 1) (setf inner-matrix (list 1 0 0 1 (* i (third bounds) (scr-size score)) 0))))
    (when other-drawf
      (matrix-front score (list 1 0 0 1 0 (+ dy (- (second other-bounds)) .05)))
      (incf dy (+ (- (fourth other-bounds) (second other-bounds)) .05))
      (funcall other-drawf score)
      (matrix-back score))
    (matrix-back score)
    (when (ornament-sign mark)
      (show score (ornament-sign mark) :matrix (translate-matrix score mark (+ x-off .2) (+ y-off dy .05 (- (box-y0 (ornament-sign mark))))))
      (setf xd (+ x-off .2))
      (setf yd (+ y-off dy .05 (- (box-y0 (ornament-sign mark))))))
    (list xd yd)))

(defun ornament (func name &rest objects)
  (let ((new-ornament (make-instance 'ornament :name name :mark func)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-ornament (argument act))
	  (if (visible-p act)
	      (push act (marks new-ornament))))))
    new-ornament))


(defun display-mordent (mark note score &optional justifying)
  (declare (ignore justifying))
  (decf (box-x0 mark) .125)
  (let ((pos (display-ornament mark #'draw-mordent mordent-bounds note score)))
    (moveto score (+ (first pos) .3) (+ (second pos) .3))
    (setf (line-width score) .03)
    (rlineto score 0 -.4)
    (draw score)
    (setf (line-width score) 0)))

;;; mordent with the slash -- the Harvard dictionary calls this a mordent, and the symbol
;;; without a slash is called "inverted-mordent" -- Gardner Read says these names are used
;;; in either way by various authors.

(defvar mordent (make-instance 'write-protected-ornament :name :mordent :mark #'display-mordent))
(defun mordent (&rest objects) (apply #'ornament #'display-mordent :mordent objects))

(defun display-inverted-mordent (mark note score &optional justifying)
  (declare (ignore justifying))
  (decf (box-x0 mark) .125)
  (display-ornament mark #'draw-mordent mordent-bounds note score))

(defvar inverted-mordent (make-instance 'write-protected-ornament :name :inverted-mordent :mark #'display-inverted-mordent))
(defun inverted-mordent (&rest objects) (apply #'ornament #'display-inverted-mordent :inverted-mordent objects))

(defun display-double-mordent (mark note score &optional justifying)
  (declare (ignore justifying))
  (decf (box-x0 mark) .125)
  (display-ornament mark #'draw-double-mordent double-mordent-bounds note score))

(defvar double-mordent (make-instance 'write-protected-ornament :name :double-mordent :mark #'display-double-mordent))
(defun double-mordent (&rest objects) (apply #'ornament #'display-double-mordent :double-mordent objects))

(defun display-turn (mark note score &optional justifying)
  (declare (ignore justifying))
  (decf (box-x0 mark) (if (stem-is-up? note) .2 .15))
  (display-ornament mark #'draw-turn turn-bounds note score))

(defvar turn (make-instance 'write-protected-ornament :name :turn :mark #'display-turn))
(defun turn (&rest objects) (apply #'ornament #'display-turn :turn objects))

(defun display-short-trill (mark note score &optional justifying)
  (declare (ignore justifying))
  (decf (box-x0 mark) (if (stem-is-up? note) .2 .15))
  (display-ornament mark #'draw-trill-section trill-section-bounds note score 2))

(defvar short-trill (make-instance 'write-protected-ornament :name :short-trill :mark #'display-short-trill))
(defun short-trill (&rest objects) (apply #'ornament #'display-short-trill :short-trill objects))

(defun display-tr (mark note score &optional justifying)
  (when (not justifying)
    (decf (box-x0 mark) (if (stem-is-up? note) .2 .15))
    (display-ornament mark #'draw-tr tr-bounds note score 2)))

(defvar tr (make-instance 'write-protected-ornament :name :tr :mark #'display-tr))
(defun tr (&rest objects) (apply #'ornament #'display-tr :tr objects))

(defun display-trilled-turn (mark note score &optional justifying)
  (when (not justifying)
    (decf (box-x0 mark) (if (stem-is-up? note) .2 .15))
    (display-ornament mark #'draw-trill-section trill-section-bounds note score 2 #'draw-turn turn-bounds)))

(defvar trilled-turn (make-instance 'write-protected-ornament :name :trilled-turn :mark #'display-trilled-turn))
(defun trilled-turn (&rest objects) (apply #'ornament #'display-trilled-turn :trilled-turn objects))


;;; trills are relatively complicated -- there can be all kinds of additional information attached to the trill

(defclass trill-mixin (ornament-mixin)
  ((sign-position :initarg :sign-position :initform nil :reader sign-position)
   (other-note :initarg :other-note :initform nil :reader other-note)
   (wavy-line :initarg :wavy-line :initform nil :reader wavy-line)
   (wavy-time :initarg :wavy-time :initform nil :reader wavy-time)))

(defclass write-protected-trill (write-protect trill-mixin)
  ())

(defclass trill (trill-mixin ornament)
  ((sign-position :accessor sign-position)
   (other-note :accessor other-note)
   (wavy-line :accessor wavy-line)
   (wavy-time :accessor wavy-time)))

(defmethod trill-p ((obj t)) nil)
(defmethod trill-p ((obj trill-mixin)) t)

(defmethod descry ((trill trill-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A"
	  (if (not controller) "(trill" "")
	  (if (sign-position trill) (format nil " :sign-position :~(~A~)" (sign-position trill)) "")
	  (if (other-note trill) (format nil " :other-note ~A" (other-note trill)) "")
	  (if (wavy-line trill) (format nil " :wavy-line ~A~A" 
					(if (listp (wavy-line trill)) "'" "")
					(wavy-line trill)) 
	    "")
	  (if (wavy-time trill) (format nil " :wavy-time ~A" (wavy-time trill)) "")
	  (if (next-method-p) (call-next-method trill stream (or controller trill)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((trill trill-mixin))
  (format nil "(trill~A~A~A~A~A)"
	  (if (ornament-sign trill) (format nil " (ornament-sign ~A)" (sign-name (ornament-sign trill))) "")
	  (if (sign-position trill) (format nil " (sign-position :~(~A~))" (sign-position trill)) "")
	  (if (wavy-line trill) 
	      (format nil " (wavy-line ~A~A)" 
		      (if (listp (wavy-line trill)) "'" "")
		      (wavy-line trill)) 
	    "")
	  (if (wavy-time trill) (format nil " (wavy-time ~1,3F)" (wavy-time trill)) "")
	  (the-usual-suspects trill)))

(defmethod copy ((trill trill-mixin) &optional object)
  (let ((new-trill (if (not object) (make-instance 'trill)
		     (if (write-protected object) (copy object)
		       object))))
    (if (other-note trill)
	(setf (other-note new-trill)
	      (if (listp (other-note trill))
		  (other-note trill)
		(copy (other-note trill)))))
    (setf (sign-position new-trill) (sign-position trill))
    (setf (wavy-line new-trill) (wavy-line trill))
    (setf (wavy-time new-trill) (wavy-time trill))
    (if (next-method-p) (call-next-method trill new-trill))
    new-trill))

(defmethod backpatch ((trill trill-mixin)) 
  (wavy-line trill))

(defmethod backpatch-time ((trill trill-mixin) obj)
  (declare (ignore obj))
  (wavy-time trill))

(defmethod backpatch-start ((trill trill-mixin)) t)

(deferred-action sign-position)
(deferred-action wavy-line)
(deferred-action wavy-time)
(deferred-action other-note)

(defun one-other-note (tr)
  (let ((other (other-note tr)))
    (if (listp other)
	(car other)
      other)))

(defun display-trill (trill note score &optional justifying)
  (let* ((y-off (+ (box-y0 trill) (vis-dy trill) (staff-y0 note) (* (max 9 (+ 2 (head-line note))) *staff-line-separation*)))
	 (x-off (+ (box-x0 note) (center note) (vis-dx trill) (box-x0 trill) -.2))
	 (use-parens (eq (sign-position trill) :in-parentheses))
	 (oy-off nil)
	 (xx0 x-off)
	 (yy0 y-off))
    (matrix-front score (translate-matrix score trill x-off y-off))
    (with-color score trill
      (draw-tr score (and (eq (pattern-type trill) :outlined) (pattern-data trill))))
    (matrix-back score)
    ;; decided to do each portion in its own matrix to keep things simple -- otherwise
    ;; we have inner matrices mimicking rmoves and the thing gets very hard to understand.
    (if (or (ornament-sign trill) (other-note trill))
	(if (and (ornament-sign trill)
		 (or (not (sign-position trill))
		     (eq (sign-position trill) :right)))
	    (show score (ornament-sign trill) :matrix (translate-matrix score trill (+ x-off .1 .385) (+ y-off .15)))
	  (if (and (ornament-sign trill)
		   (eq (sign-position trill) :up))
	      (show score (ornament-sign trill) :matrix (translate-matrix score trill (+ x-off .1) (+ y-off .75)))
	    (if (or (and (other-note trill) 
			 (not (sign-position trill)))
		    use-parens)
		(let ((other-note-line (and (other-note trill)
					    (place-of-note-given-note note (one-other-note trill)))))
		  (setf xx0 (+ x-off (- (third tr-bounds) (first tr-bounds))))
		  (setf oy-off (if (other-note trill)
				   (+ (staff-y0 note) (* *staff-line-separation* other-note-line))
				 (+ (box-y0 note) *staff-line-separation*)))
		  (setf yy0 (- oy-off .15))
		  ;; ^ brute force to line up parens?
		  (if (and (numberp (dots note)) (plusp (dots note))) (incf xx0 .15) (incf xx0 .05))
		  (when use-parens
		    (matrix-front score (translate-matrix score trill xx0 yy0))
		    (draw-left-paren score)
		    (matrix-back score))
		  (incf yy0 .15)
		  (when (or (sign (one-other-note trill))
			    (ornament-sign trill))
		    (incf xx0 (if use-parens .15 .05)))
		  (if (ornament-sign trill)
		      (show score (ornament-sign trill) :matrix (translate-matrix score trill xx0 yy0)))
		  (if (other-note trill)
		      (let ((others (if (listp (other-note trill)) (other-note trill) (list (other-note trill)))))
			(loop for other in others do
			  (let* ((line (place-of-note-given-note note other))
				 (sls *staff-line-separation*)
				 (oy0 (+ (staff-y0 note) (* *staff-line-separation* line))))
			    (if (sign other)
				(show score (sign other) :matrix (scale-matrix (translate-matrix score trill xx0 oy0) .6 .6)))
			    (incf xx0 .1)
			    (if (or (< line -1) (> line 9))
				(let* ((yline (if (oddp line) (if (minusp line) sls (- sls)) 0))
				       (factor (if (minusp line) 2 -2))
				       (lines (if (oddp line) 
						  (if (plusp line) 
						      (floor (- line 9) 2)
						    (floor (- (abs line) 1) 2))
						(if (plusp line)
						    (floor (- line 8) 2)
						  (floor (abs line) 2)))))
				  (incf xx0 .05)
				  (matrix-front score (scale-matrix (translate-matrix score trill xx0 oy0) .6 .6))
				  (draw-quarter-note score)
				  (moveto score .05 yline)
				  (do ((i 0 (1+ i)))
				      ((>= i lines))
				    (rlineto score .25 0)
				    (rmoveto score -.25 (* factor sls)))
				  (draw score)
				  (matrix-back score))
			      (progn
				(matrix-front score (scale-matrix (translate-matrix score trill xx0 oy0) .6 .6))
				(draw-quarter-note score)
				(matrix-back score)))
			    (incf xx0 .2)
			    ))))
		  (when use-parens
		    (matrix-front score (translate-matrix score trill xx0 (- oy-off .15)))
		    (draw-right-paren score)
		    (matrix-back score)))))))
    (if (wavy-line trill)
	(let* ((sign-dx (if (and (or (other-note trill) use-parens)
				 (or (not oy-off) (>= oy-off y-off)))
			    .8
			  (if (and (ornament-sign trill)
				   (or (not (sign-position trill))
				       (member (sign-position trill) '(:right :in-parentheses))))
			      .4
			    0)))
	       (wavy-dx (and (not justifying)
			     (listp (wavy-line trill))))
	       (trill-x0 (+ x-off .5 sign-dx (if wavy-dx (first (wavy-line trill)) 0)))
	       (trill-length (- (+ (box-x1 trill) 
				   (if wavy-dx (third (wavy-line trill)) 0))
				trill-x0 .25))
	       (trill-width (- (third trill-section-bounds) (first trill-section-bounds)))
	       (count (round trill-length trill-width)))
	  (when (plusp count)
	    (matrix-front score (translate-matrix score trill trill-x0 y-off))
	    (draw-trill-sections score count)
	    (matrix-back score))))))

#|
(cmn (staff treble 
       (note gf5 w. (trill (wavy-line t) (ornament-sign small-natural) (sign-position :in-parentheses) (other-note f5)))) 
     (staff bass c3 q c3 q c3 q c3 q c3 q c3 q ))

(cmn (size 100) (staff treble bn4 (trill (wavy-line t)) w) (staff bass c4 q c4 q c4 q c4 q))
(cmn gf5 w. (trill (other-note f5)))

(cmn c5 w. (trill (other-note (list d5 e5 f5))))
|#


(defvar trill (make-instance 'write-protected-trill :name :trill :mark #'display-trill))

(defun trill (&rest objects)
  (let ((new-trill (make-instance 'trill :mark #'display-trill)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-trill (argument act)))))
    new-trill))



(defclass arpeggio-mark-mixin (sundry-mixin)
  ((arrow-direction :initarg :arrow-direction :initform nil :reader arrow-direction)))

(defclass write-protected-arpeggio-mark (write-protect arpeggio-mark-mixin) ())

(defclass arpeggio-mark (arpeggio-mark-mixin sundry)
  ((arrow-direction :accessor arrow-direction)))

(defmethod arpeggio-p ((obj t)) nil)
(defmethod arpeggio-p ((obj arpeggio-mark-mixin)) t)

(deferred-action arrow-direction)

(defmethod descry ((arp arpeggio-mark-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) (format nil "(~(~A~)" (sundry-name arp)) "")
	  (if (arrow-direction arp) (format nil " :arrow-direction :~(~A~)" (arrow-direction arp)) "")
	  (if (next-method-p) (call-next-method arp stream (or controller arp)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((arpeggio arpeggio-mark-mixin))
  (format nil "(~(~A~)~A~A)"
	  (sundry-name arpeggio)
	  (if (and (eq (sundry-name arpeggio) :arpeggio)
		   (arrow-direction arpeggio))
	      (if (eq (arrow-direction arpeggio) :up)
		  " arrow-up" " arrow-down")
	    "")
	  (the-usual-suspects arpeggio)))

(defmethod copy ((arp arpeggio-mark-mixin) &optional object)
  (let ((new-arp (if (not object) (make-instance 'arpeggio-mark)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (sundry-name new-arp) (sundry-name arp))
    (setf (sundry-mark new-arp) (sundry-mark arp))
    (setf (arrow-direction new-arp) (arrow-direction arp))
    (if (next-method-p) (call-next-method arp new-arp))
    new-arp))

(defun display-arpeggio (mark chord score &optional just-arrow)
  (let* ((arrow-info (arrow-direction mark))
	 (maxl (maximum-line chord))
	 (minl (minimum-line chord))
	 (dist (- maxl minl))
	 (bottom (+ (staff-y0 chord) -.125 (* minl *staff-line-separation*) (vis-dy mark) (box-y0 mark)))
	 (y-loc 0)
	 (x-loc (+ (box-x0 chord) (vis-dx mark) (box-x0 mark) (if (or (< minl -1) (> maxl 9)) -.05 0))))
    (if just-arrow
	(progn
	  (moveto score x-loc bottom)
	  (rlineto score 0 (setf y-loc (* dist *staff-line-separation*)))
	  (draw score))
      (progn
	(matrix-front score (translate-matrix score mark x-loc bottom))
	(draw-arpeggios score (round dist 4)) ;dist is in terms of line numbers (not .52 = length of one arpeggio section)
	(matrix-back score)))
    (when arrow-info
      (let* ((above (eq arrow-info :up))
	     (dy (if above -.2 .2))
	     (dy1 (* dy 1.5))
	     (x1-loc (+ x-loc (if (not just-arrow) (* .5 (- (third arpeggio-bounds) (first arpeggio-bounds))) 0)))
	     (y1-loc (if above (+ y-loc bottom
				  (if above .05 0)
				  (if (not just-arrow)
				      (* .52 (round dist 4))
				    0))
		       bottom)))
	(moveto score (- x1-loc .01) y1-loc)
	(lineto score (- x1-loc .01) (- y1-loc dy))
	(lineto score (- x1-loc .1) y1-loc)
	(lineto score x1-loc (- y1-loc dy1))
	(lineto score (+ x1-loc .1) y1-loc)
	(lineto score (+ x1-loc .01) (- y1-loc dy))
	(lineto score (+ x1-loc .01) y1-loc)
	(lineto score (- x1-loc .01) y1-loc)
	(fill-in score)))))

;;; (cmn (size 100) treble (chord (notes c4 c5) h (arpeggio arrow-down)) (chord (notes c4 c5) h (arpeggio arrow-up)) )

(defvar arpeggio (make-instance 'write-protected-arpeggio-mark :name :arpeggio :mark #'display-arpeggio))

(defun arpeggio (&rest objects)
  (let ((new-arp (make-instance 'arpeggio-mark :name :arpeggio :mark #'display-arpeggio)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-arp (argument act))
	  (if (arrow-p act)
	      (if (eq (sundry-name act) :arrow-up)
		  (setf (arrow-direction new-arp) :up)
		(if (eq (sundry-name act) :arrow-down)
		    (setf (arrow-direction new-arp) :down)))
	    (if (visible-p act)
		(push act (marks new-arp)))))))
    new-arp))

(defun display-no-arpeggio (mark chord score &optional justifying)
  (declare (ignore justifying))
  (let* ((maxl (maximum-line chord))
	 (minl (minimum-line chord))
	 (dist (- maxl minl))
	 (bottom (+ (staff-y0 chord) (* minl *staff-line-separation*) (y0 mark) (vis-dy mark)))
	 (x-loc (+ (box-x0 chord) (vis-dx mark) (if (or (< minl -1) (> maxl 9)) -.15 -.05))))
    (matrix-front score  (translate-matrix score mark x-loc bottom))
    (moveto score 0 0)
    (rlineto score -.1 0)
    (rlineto score 0 (+ (- (y0 mark)) (y1 mark) (* dist *staff-line-separation*)))
    (rlineto score .1 0)
    (draw score)
    (matrix-back score)))

(defvar no-arpeggio (make-instance 'write-protected-sundry :name :no-arpeggio :mark #'display-no-arpeggio))
(defun no-arpeggio (&rest objects) (apply #'mark #'display-no-arpeggio :no-arpeggio objects))

(defmacro chord-range (range &optional dur)
  `(chord ,range (or ,dur q) no-stem (no-arpeggio (y0 -0.2) (y1 0.2))))

;(cmn staff treble (chord-range (notes f4 e5)))


(defun arrow-p (object)
  (and object (arpeggio-p object) (member (sundry-name object) '(:arrow-up :arrow-down))))

(defun display-arrow (mark chord score &optional justifying)
  (declare (ignore justifying))
  (display-arpeggio mark chord score t))

(defvar arrow-up (make-instance 'write-protected-arpeggio-mark :name :arrow-up :mark #'display-arrow :arrow-direction :up))

(defun arrow-up (&rest objects) 
  (let ((new-arrow (apply #'arpeggio objects)))
    (setf (arrow-direction new-arrow) :up)
    (setf (sundry-name new-arrow) :arrow-up)
    (setf (sundry-mark new-arrow) #'display-arrow)
    new-arrow))

(defvar arrow-down (make-instance 'write-protected-arpeggio-mark :name :arrow-down :mark #'display-arrow :arrow-direction :down))

(defun arrow-down (&rest objects) 
  (let ((new-arrow (apply #'arpeggio objects)))
    (setf (arrow-direction new-arrow) :down)
    (setf (sundry-name new-arrow) :arrow-down)
    (setf (sundry-mark new-arrow) #'display-arrow)
    new-arrow))

(defun arrow (&rest objects)
  (let ((new-arrow (make-instance 'arpeggio-mark :name :arrow-up :mark #'display-arrow :arrow-direction :up)))
    (loop for act in objects do
      (if (self-acting-p act)
	  (funcall (action act) new-arrow (argument act))
	(if (eq act :down)
	    (progn
	      (setf (sundry-name new-arrow) :arrow-down)
	      (setf (arrow-direction new-arrow) :down))
	  (if (visible-p act)
	      (push act (marks new-arrow))))))
    new-arrow))



;;; arrowheads (Anders Vinjar)

(defun draw-arrowhead-up (mark note  score &optional justification)
  (declare (ignore justification))
  (let ((stem-up (if (audible-stem-direction note)
                     (member (audible-stem-direction note) '(:up :up?))
                   (< (note-line note) 4))))
    (comment score "arrowhead-up")
    (incf (vis-dy note)  (if stem-up 0.125 -0.125))
    (with-scaling score
      (* (note-size note) *note-head-size*)
      (if stem-up (* (size score) 0.28) 0)
      (if stem-up (* (size score) -0.25) 0)
      (with-thickness score mark .03
                      (moveto score 0.0 0.0)
                      (rlineto score .14 0)
                      (rlineto score -.14 .25)
                      (rlineto score -.14 -.25)
                      (rlineto score .14 .0)
                      (if (>= (quarters note) 2)
                          (draw score t)
                        (fill-in score))))
    (if (marks note) (dolist (m (marks note)) (incf (dx m) (if stem-up 0.125 -0.125))))
    (if (note-sign note) (setf (slot-value (note-sign note) 'dx) (if stem-up 0.125 -0.125)))      
    (if stem-up
        (setf (width mark) (+ 0.28 *half-stem-width*)))))

(defun arrowhead-up (&rest objects)
  (let ((new-head (make-instance 'sundry :name :arrowhead-up :mark #'draw-arrowhead-up)))
    (loop for act in objects do
      (if (self-acting-p act)
          (funcall (action act) new-head (argument act))))
    new-head))

(defvar arrowhead-up (make-instance 'write-protected-sundry :name :arrowhead-up :mark #'draw-arrowhead-up))

(defun draw-arrowhead-down (mark note  score &optional justification)
  (declare (ignore justification))
  (let ((stem-up (if (audible-stem-direction note)
                     (member (audible-stem-direction note) '(:up :up?))
                   (< (note-line note) 4))))
    (comment score "arrowhead-down")
    (incf (vis-dy note)  (if stem-up 0.125 -0.115))
    (with-scaling score
      (* (note-size note) *note-head-size*)
      (if stem-up (* (size score) 0.28) 0)
      (if stem-up (* (size score) -0.25) 0)
      (with-thickness score mark .03
                      (moveto score 0.0 .23)
                      (rlineto score .14 0)
                      (rlineto score -.14 -.25)
                      (rlineto score -.14 .25)
                      (rlineto score .14 .0)
                      (if (>= (quarters note) 2)
                          (draw score t)
                        (fill-in score))))
    (if (marks note) (dolist (m (marks note)) (incf (dx m) (if stem-up 0.125 -0.125))))
    (if (note-sign note) (setf (slot-value (note-sign note) 'dx) (if stem-up 0.125 -0.125)))      
    (if stem-up
        (setf (width mark) (+ 0.28 *half-stem-width*))
      (setf (width mark) (- 0.28 *half-stem-width*)))))

(defun arrowhead-down (&rest objects)
  (let ((new-head (make-instance 'sundry :name :arrowhead-down :mark #'draw-arrowhead-down)))
    (loop for act in objects do
      (if (self-acting-p act)
          (funcall (action act) new-head (argument act))))
    new-head))

(defvar arrowhead-down (make-instance 'write-protected-sundry :name :arrowhead-down :mark #'draw-arrowhead-down))


#|

(cmn (size 24)
     (free-expansion-factor 1.5)
     (staff treble
            (d4 h (note-head :diamond-1) (stem-up)
                (lyrics "king - ") (p (justification :up)))
            (d4 q (note-head :diamond-1) (lyrics "dom"))
            (whole-rest invisible)
            (g5 q (dy .3) (note-head arrowhead-up) (f (justification :up) (dy .2)) (lyrics "sov  -"))
            (g5 e (dy .3) (dx 0.1) (note-head arrowhead-up)
                (dynamics (justification :up) (dx -0.8)
                          (text "sub."(font-scaler .4) (font-name "Times-Italic")))
                (stem-dy .25)
                (lyrics "er -"))
            (g5 q (dy .3) (note-head arrowhead-up) (lyrics "eign!"))))

|#


;;;
;;; ----------------    rehearsal numbers and letters
;;;
;;; these can be boxed or circled or left alone, normally attached to a bar line
;;; auto-incrementing is needed so we don't have to count them by hand
;;; letters increment past Z as AA BB CC and so on.

(defun next-rehearsal-letter (current-letter)
  (if (not current-letter)
      "A"
    (let ((count (length current-letter))
	  (base-letter (char current-letter 0)))
      (if (char= base-letter #\Z) 
	  (progn
	    (setf base-letter #\A)
	    (incf count))
	(if (char= base-letter #\H)	;skip "I" -- looks too much like "1" and "J"
	    (setf base-letter #\J)
	  (setf base-letter (code-char (1+ (char-code base-letter))))))
      (make-string count :initial-element base-letter))))

(defun next-rehearsal-number (current-number)
  (1+ current-number))


(defclass rehearsal-mark (sundry font-mixin)
  ((remark :initarg :remark :initform nil :accessor remark)
   (frame :initarg :frame :initform nil :accessor frame)
   (frame-width :initarg :rehearsal-frame-width :initform .075 :accessor rehearsal-frame-width)
   (frame-white-space :initarg :rehearsal-frame-white-space :initform .05 :accessor rehearsal-frame-white-space)
   (font-name :initform (bold-font))
   (font-scaler :initform 1.0)))

(deferred-action frame)
(deferred-action rehearsal-frame-width)
(deferred-action rehearsal-frame-white-space)


(defmethod copy ((rn rehearsal-mark) &optional object)
  (let ((new-rn (if (not object) (make-instance 'rehearsal-mark)
		  (if (write-protected object) (copy object)
		    object))))
    (setf (remark new-rn) (remark rn))
    (setf (frame new-rn) (frame rn))
    (setf (rehearsal-frame-width new-rn) (rehearsal-frame-width rn))
    (setf (rehearsal-frame-white-space new-rn) (rehearsal-frame-white-space rn))
    (if (next-method-p) (call-next-method rn new-rn))
    new-rn))

(defmethod rehearsal-mark-p ((obj t)) nil)
(defmethod rehearsal-mark-p ((obj rehearsal-mark)) t)

(defmethod descry ((rm rehearsal-mark) &optional stream controller)
  (format stream "~A :remark ~A :frame ~A~A~A~A~A"
	  (if (not controller) 
	      (if (numberp (remark rm)) 
		  "(rehearsal-number" 
		"(rehearsal-letter") 
	    "")
	  (remark rm)
	  (if (frame rm) (format nil ":~(~A~)" (frame rm)))
	  (format nil " :width ~1,3F" (rehearsal-frame-width rm))
	  (format nil " :white-space ~1,3F" (rehearsal-frame-white-space rm))
	  (if (next-method-p) (call-next-method rm stream (or controller rm)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((rm rehearsal-mark))
  (format nil "(~A~A~A~A~A~A)"
	  (if (numberp (remark rm)) "rehearsal-number" "rehearsal-letter")
	  (if (numberp (remark rm)) (format nil " ~D" (remark rm)) (format nil " ~S" (remark rm)))
	  (if (frame rm) (format nil "(frame :~(~A~))" (frame rm)) "")
	  (if (/= (rehearsal-frame-width rm) .075) (format nil " (rehearsal-frame-width ~1,3F)" (rehearsal-frame-width rm)) "")
	  (if (/= (rehearsal-frame-white-space rm) .05) (format nil " (rehearsal-frame-white-space ~1,3F)" (rehearsal-frame-white-space rm)) "")
	  (the-usual-suspects rm)))

(defvar rehearsal-letter (make-self-acting 
			  :action #'(lambda (bar &rest rest)
				      (declare (ignore rest))
				      (add-to-marks bar (list (ur-rehearsal-mark nil))))
			  :argument nil))

(defvar rehearsal-number (make-self-acting 
			  :action #'(lambda (bar &rest rest)
				      (declare (ignore rest))
				      (add-to-marks bar (list (ur-rehearsal-mark t))))
			  :argument nil))

(defun display-rehearsal-mark (mark bar score &optional justifying)
  (declare (ignore justifying))
  (let ((x0 (+ (box-x0 bar) (vis-dx mark)))
	(y0 (+ (box-y1 bar) 0.5 (vis-dy mark)))
	(txt (cmn-text :letters (if (numberp (remark mark)) (format nil "~D" (remark mark)) (remark mark))
		     :font-name (font-name mark)
		     :font-scaler (font-scaler mark))))
    (show score txt :matrix (translate-matrix score mark x0 y0))
    (when (and (frame mark) (not (eq (frame mark) :none)))
      (let* ((width (rehearsal-frame-width mark))
	     (ws (rehearsal-frame-white-space mark))
	     (fx0 (- x0 width ws))
	     (fy0 (- y0 width ws))
	     (fx1 (+ x0 ws width (* (length (letters txt)) (font-scaler mark) .7)))
	     ;; the "right" thing would be to go out, find the appropriate .afm file,
	     ;; read in the glyph widths, and use them here rather than .7.
	     (fy1 (+ y0 width ws (* .7 (font-scaler mark)))))
	(setf (line-width score) width)
	(case (frame mark)
	  (:box 
	   (moveto score fx0 fy0)
	   (lineto score fx1 fy0)
	   (lineto score fx1 fy1)
	   (lineto score fx0 fy1)
	   (lineto score fx0 (- fy0 (* .5 width)))
	   (draw score))
	  (:circle 
	   (let ((rx (* .5 (+ fx0 fx1)))
		 (ry (* .5 (+ fy0 fy1)))
		 (r (+ width (* .5 (max (- fx1 fx0) (- fy1 fy0))))))
	     (circle score rx ry r 0 360 nil)))
	  (otherwise (cmn-error "unknown rehearsal mark frame: ~A" (frame mark))))
	(setf (line-width score) 0)))))

(defun ur-rehearsal-mark (num &rest args)
  (let ((new-rehearsal-mark (make-instance 'rehearsal-mark :name :rehearsal-mark :mark #'display-rehearsal-mark)))
    (loop for act in args do
      (when act
	(if (and num (numberp act))
	    (setf (remark new-rehearsal-mark) act)
	  (if (and (not num) (stringp act))
	      (setf (remark new-rehearsal-mark) act)
	    (if (and (not num) (characterp act))
		(setf (remark new-rehearsal-mark) (make-string 1 :initial-element act))
	      (if (self-acting-p act)
		  (funcall (action act) new-rehearsal-mark (argument act))
		(if (visible-p act)
		    (push act (marks new-rehearsal-mark)))))))))
    (if (not (remark new-rehearsal-mark))
	(setf (remark new-rehearsal-mark) 
	  (if num 
	      (if rehearsal-stack 
		  (next-rehearsal-number (remark rehearsal-stack))
		1)
	    (if rehearsal-stack
		(next-rehearsal-letter (remark rehearsal-stack))
	      "A"))))
    (if (and rehearsal-stack
	     (not (frame new-rehearsal-mark))
	     (frame rehearsal-stack))
	(setf (frame new-rehearsal-mark) (frame rehearsal-stack)))
    (setf rehearsal-stack new-rehearsal-mark)
    new-rehearsal-mark))

(defun rehearsal-number (&rest args) (apply #'ur-rehearsal-mark t args))
(defun rehearsal-letter (&rest args) (apply #'ur-rehearsal-mark nil args))




;;;
;;; ----------------    measure numbers
;;;

(defclass measure-mark (sundry font-mixin)
  ((remark :initarg :remark :initform nil :accessor remark)
   (font-name :initform (italic-font))	;Ross says this should be the standard music font numerals (as used in meters)
   (font-scaler :initform .5)))

(defmethod copy ((rn measure-mark) &optional object)
  (let ((new-rn (if (not object) (make-instance 'measure-mark)
		  (if (write-protected object) (copy object)
		    object))))
    (setf (remark new-rn) (remark rn))
    (if (next-method-p) (call-next-method rn new-rn))
    new-rn))

(defmethod measure-mark-p ((obj t)) nil)
(defmethod measure-mark-p ((obj measure-mark)) t)

(defmethod descry ((rm measure-mark) &optional stream controller)
  (format stream "(measure-number :remark ~A~A~A"
	  (remark rm)
	  (if (next-method-p) (call-next-method rm stream (or controller rm)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((rm measure-mark))
  (format nil "(measure-number ~A~A)" (remark rm) (the-usual-suspects rm)))

(defun display-measure-mark (mark bar score &optional justifying)
  (declare (ignore justifying))
  (when (not (invisible-p mark))
    (let ((x0 (+ (box-x0 bar) (vis-dx mark)))
	  (y0 (+ (box-y1 bar) 0.5 (vis-dy mark)))
	  (txt (cmn-text :letters (if (numberp (remark mark)) (format nil "~D" (remark mark)) (remark mark))
		       :font-name (font-name mark)
		       :font-scaler (font-scaler mark))))
      (show score txt :matrix (translate-matrix score mark x0 y0)))))

(defun measure-number (&rest args)
  (let ((new-measure-mark (make-instance 'measure-mark :name :measure-mark :mark #'display-measure-mark)))
    (loop for act in args do
      (when act
	(if (or (numberp act) (stringp act))
	    (setf (remark new-measure-mark) act)
	  (if (self-acting-p act)
	      (funcall (action act) new-measure-mark (argument act))
	    (if (visible-p act)
		(push act (marks new-measure-mark)))))))
    new-measure-mark))




;;;
;;; ----------------    fingering
;;;

(defclass finger-mark (sundry font-mixin)
  ((fingers :initarg :fingers :initform nil :accessor fingers)
   (size :initarg :fingering-size :initform 1.0 :accessor fingering-size)
   (font-name :initform (normal-font))
   (font-scaler :initform .33)))

(defmethod finger-mark-p ((obj t)) nil)
(defmethod finger-mark-p ((obj finger-mark)) t)

(deferred-action fingering-size)
(deferred-action fingers)

(defmethod copy ((fing finger-mark) &optional object)
  (let ((new-fing (if (not object) (make-instance 'finger-mark)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (fingers new-fing) (fingers fing))
    (if (next-method-p) (call-next-method fing new-fing))
    new-fing))

(defmethod descry ((fing finger-mark) &optional stream controller)
  (format stream "~A :fingers '~A~A~A"
	  (if (not controller) "(fingering" "")
	  (fingers fing)
	  (if (next-method-p) (call-next-method fing stream (or controller fing)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((fng finger-mark))
  (format nil "(fingering~{ ~D~}~A)" (fingers fng) (the-usual-suspects fng)))

(defun display-fingering (mark note score &optional justifying)
  (let* ((dir (direction-from-note mark note))
	 (ffs (* (font-scaler mark) (fingering-size mark)))
	 (nums (fingers mark))
	 (len (length nums))
	 (dy (* (1- len) ffs))
	 (y-off (+ (box-y0 mark) (vis-dy mark)  
		   (staff-y0 note) 
		   (if (eq dir :up) 
		       (+ dy
			  (* (max 9 (+ 2 (head-line note))) *staff-line-separation*))
		     (min (- (+ ffs .125)) 
			  (* (- (minimum-line note) 3) *staff-line-separation*)))))
	 (x-off (+ (box-x0 note) (vis-dx mark) -.1 (center note) (box-x0 mark))))
#-(or gcl sbcl) (loop for num in nums and new-y from y-off by (- ffs) do
	(if (not (text-p num))
	    (show score (cmn-text :letters (if (numberp num) (format nil "~D" num) num)
				  :font-name (font-name mark) :font-scaler ffs)
		  :matrix (translate-matrix score mark x-off new-y))
	  (progn
	    (setf (box-x0 num) x-off)
	    (setf (box-y0 num) new-y)
	    (display num mark score justifying))))
#+(or gcl sbcl) (let ((new-y y-off))
	(loop for num in nums do
	  (if (not (text-p num))
	      (show score (cmn-text :letters (if (numberp num) (format nil "~D" num) num)
				    :font-name (font-name mark) :font-scaler ffs)
		    :matrix (translate-matrix score mark x-off new-y))
	    (progn
	      (setf (box-x0 num) x-off)
	      (setf (box-y0 num) new-y)
	      (display num mark score justifying)))
	  (decf new-y ffs)))
   ))

(defun fingering (&rest objects)
  (let ((new-mark (make-instance 'finger-mark :name :fingering :mark #'display-fingering))
	(fingers nil))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mark (argument act))
	  (if (visible-p act)
	      (push act (marks new-mark))
	    (push act fingers)))))
    (if (not (fingers new-mark)) (setf (fingers new-mark) (reverse fingers)))
    new-mark))




;;;
;;; ----------------    octave signs
;;;

(defclass octave-sign (sundry font-mixin)
  ((octave :initarg :octave :initform nil :accessor octave)
   (note0 :initarg :note0 :initform nil :accessor note0)
   (note1 :initarg :note1 :initform nil :accessor note1)
   (max-line :initarg :max-line :initform nil :accessor max-line)
   (connecting-pattern :initarg :connecting-pattern :initform '(10 20) :accessor octave-sign-pattern)
   (vertical-separation :initarg :vertical-separation :initform 3 :accessor vertical-separation)
   (font-name :initform (normal-font))
   (font-scaler :initform .5)))

(deferred-action vertical-separation)
(deferred-action octave-sign-pattern)

(defmethod descry ((octave octave-sign) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A"
	  (if (not controller) (format nil "(octave-sign") "")
	  (if (octave octave) (format nil " :octave ~D" (octave octave)) "")
	  (if (note0 octave) (format nil " :note0 ~A" (note0 octave)) "")
	  (if (note1 octave) (format nil " :note1 ~A" (note1 octave)) "")
	  (if (max-line octave) (format nil " :max-line ~D" (max-line octave)) "")
	  (format nil " :vertical-separation ~A" (vertical-separation octave))
	  (format nil " :connecting-pattern '~A" (octave-sign-pattern octave))
	  (if (next-method-p) (call-next-method octave stream (or controller octave)) "")
	  (if (not controller) ")" "")))

(defvar cmn-store-tags nil)		;a list of (obj str) pairs

(defmethod identify ((octave octave-sign))
  ;; like beam or slur -- have to mark end note (note1) and deal with possible tags / line breaks
  ;; first problem is handled with add-to-cmn-store-tags for the ending note
  ;; there are also the special cases of single note octaves and no octaves
  ;; if zerop dy and dx, probably safe to use the variable form
  ;; in general, no need for explicit tags here
  (let* ((var-time (and (zerop (vis-dx octave)) (zerop (vis-dy octave))))
	 (oct (octave octave))
	 (fun-name (concatenate 'string 
		     (if (= oct 0) "no-"
		       (if (= (abs oct) 2) "two-"
			 ""))
		     "octave"
		     (if (= oct 0) "-sign"
		       (if (= (abs oct) 2) "s"
			 ""))
		     (if (= oct 0) ""
		       (if (plusp oct) "-up"
			 "-down"))))
	 (begin-and-end (not (eq (note0 octave) (note1 octave)))))
    (if begin-and-end
	(if (not (pitch (note0 octave)))
	    (let ((pair (find :octave cmn-store-tags :key #'third :test #'eq)))
	      (if pair
		  (progn
		    (setf cmn-store-tags (remove pair cmn-store-tags))
		    (format nil "~A" (second pair)))))
	  (progn
	    (push (list (note1 octave)
			(format nil "~Aend-~A~A"
				(if var-time "" "(")
				fun-name
				(if var-time "" ")"))
			:octave)
		  cmn-store-tags)
	    (format nil "~Abegin-~A~A~A"
		    (if var-time "" "(")
		    fun-name
		    (if var-time "" (the-usual-suspects octave))
		    (if var-time "" ")"))))
      (if var-time
	  fun-name
	(format nil "(~A~A)" fun-name (the-usual-suspects octave))))))

(defmethod copy ((octave octave-sign) &optional object)
  (let ((new-octave (if (not object) (make-instance 'octave-sign)
		      (if (write-protected object) (copy object)
			object))))
    (setf (octave new-octave) (octave octave))
    (setf (max-line new-octave) (max-line octave))
    (setf (vertical-separation new-octave) (vertical-separation octave))
    (setf (octave-sign-pattern new-octave) (copy-list (octave-sign-pattern octave)))
    (if (next-method-p) (call-next-method octave new-octave))
    new-octave))

(defun start-octave (num &rest args)
  (make-self-acting
   :action #'(lambda (note &rest rest)
	       (declare (ignore rest))
	       (let ((new-8 (make-instance 'octave-sign
			     :note0 note
			     :octave num
			     :mark #'display-octave
			     :name :octave)))
		 (setf octave-stack new-8)
		 (if args (loop for arg in args do
			    (if (self-acting-p arg)
				(funcall (action arg) new-8 (argument arg)))))
		 nil))
   :argument nil))

(defun end-octave (num &rest args)
  (declare (ignore args))
  (make-self-acting 
   :action #'(lambda (note &rest rest)
	       (declare (ignore rest))
	       (when (not (zerop num))
		 (if (not octave-stack) 
		     (funcall (action (start-octave num)) note nil)))
	       (add-to-marks (note0 octave-stack) (list octave-stack))
	       (setf (note1 octave-stack) note)
	       (setf octave-stack nil))
   :argument nil))

(defgeneric (setf octaved) (val obj))
(defgeneric (setf store-data) (val obj))

(defun start-and-end-octave (num &rest args)
  (make-self-acting
   :action #'(lambda (note &rest rest)
	       (declare (ignore rest))
	       (setf (octaved note) t)
	       (add-to-marks note (list (let ((new-8 (make-instance 'octave-sign
						      :note0 note
						      :note1 note
						      :mark #'display-octave
						      :octave num
						      :name :octave)))
					  (if args (loop for arg in args do
						     (if (self-acting-p arg)
							 (funcall (action arg) new-8 (argument arg)))))
					  new-8))))
   :argument nil))

(defvar begin-octave-up         (start-octave 1))
(defvar octave-up               (start-and-end-octave 1))
(defvar end-octave-up           (end-octave 1))
(defvar begin-octave-down       (start-octave -1))
(defvar octave-down             (start-and-end-octave -1))
(defvar end-octave-down         (end-octave -1))
(defvar two-octaves-up          (start-and-end-octave 2))
(defvar two-octaves-down        (start-and-end-octave -2))
(defvar begin-two-octaves-up    (start-octave 2))
(defvar begin-two-octaves-down  (start-octave -2))
(defvar end-two-octaves-up      (end-octave 2))
(defvar end-two-octaves-down    (end-octave -2))
(defvar no-octave-sign          (start-and-end-octave 0))
(defvar begin-no-octave-sign    (start-octave 0))
(defvar end-no-octave-sign      (end-octave 0))

(defun begin-octave-up (&rest args)      (apply #'start-octave 1 args))
(defun octave-up (&rest args)            (apply #'start-and-end-octave 1 args))
(defun end-octave-up (&rest args)        (apply #'end-octave 1 args))
(defun begin-octave-down (&rest args)    (apply #'start-octave -1 args))
(defun octave-down (&rest args)          (apply #'start-and-end-octave -1 args))
(defun end-octave-down (&rest args)      (apply #'end-octave -1 args))
(defun two-octaves-up (&rest args)       (apply #'start-and-end-octave 2 args))
(defun two-octaves-down (&rest args)     (apply #'start-and-end-octave -2 args))
(defun begin-two-octaves-up (&rest args) (apply #'start-octave 2 args))
(defun begin-two-octaves-down (&rest args) (apply #'start-octave -2 args))
(defun end-two-octaves-up (&rest args)   (apply #'end-octave 2 args))
(defun end-two-octaves-down (&rest args) (apply #'end-octave -2 args))
(defun no-octave-sign (&rest args)       (apply #'start-and-end-octave 0 args))
(defun begin-no-octave-sign (&rest args) (apply #'start-octave 0 args))
(defun end-no-octave-sign (&rest args)   (apply #'end-octave 0 args))

(defun prepare-octave-signs (octize oct) ;used only by automatic octave signs
  (list
   (make-instance 'octave-sign 
    :name :octave
    :mark #'display-octave
    :note0 (first octize) 
    :note1 (first (last octize)) 
    :octave oct 
    :max-line (or (if (plusp oct)
		      (loop for note in octize maximize (maximum-line note))
		    (loop for note in octize minimize (minimum-line note)))
		  0))))

(defun display-octave (sundry note score &optional justifying)
  ;; direction is whether we are above of below (:up or :down)
  ;; octaves is how many octaves (8 or 15 is the number used)
  ;; use-italian (if t) means use either "8va" or "15ma" rather than "8" or "15" (i much prefer the latter)
  ;; add-bassa (if t) means "add the word bassa to the 8 or 8va or whatever.
  ;; I suppose a prettier version would put the "va" or "ma" in a smaller font than the "8"
  (if (and (not justifying) (not (zerop (octave sundry))))
      (let ((no-start (not (note0 sundry)))
	    (no-end (not (note1 sundry))))
	(if (and (note0 sundry)
		 (note1 sundry)
		 (/= (staff-y0 (note0 sundry)) (staff-y0 (note1 sundry))))
	    (let* ((all-data (staff-data *cmn-staff*))
		   (n0 (note0 sundry))
		   (n1 (note1 sundry))
		   (s0 (staff-y0 n0))
		   (s1 (staff-y0 n1))
		   (curs s0))
	      ;; loop through the staves in the data list looking for the end staff and inserting no-start-or-end
	      ;;  markers for any in-between -- first find overall staff (*cmn-staff* used here -- bad form)
	      (loop while (not (eq n0 (first all-data))) do (pop all-data))
	      (let ((first-notes nil))
		(loop while (not (eq n1 (first all-data))) do
		  (let* ((curn (pop all-data))
			 (curns (if (audible-p curn) (staff-y0 curn) curs)))
		    (if (and (/= s1 curns) (/= curs curns))
			(progn
			  (push curn first-notes)
			  (setf curs curns)))))
		(when first-notes
		  (loop for firno in first-notes do
		    (add-to-marks firno
				  (list (make-instance 'octave-sign
					 :name :octave
					 :mark #'display-octave
					 :note0 nil
					 :dx (vis-dx sundry)
					 :dy (vis-dy sundry)
					 :octave (octave sundry)
					 :max-line nil))))))
	      (add-to-marks (note1 sundry) 
			    (list (make-instance 'octave-sign
				   :name :octave
				   :mark #'display-octave
				   :note0 nil
				   :dx (vis-dx sundry)
				   :dy (vis-dy sundry)
				   :note1 (note1 sundry)
				   :octave (octave sundry)
				   :max-line nil)))
	      (setf no-end t)
	      (setf (note1 sundry) nil)))
	
	(if  no-start
	    (setf (note0 sundry) (make-instance 'note 
				  :line 0 
				  :x0 (if *cmn-staff* (box-x0 *cmn-staff*) (box-x0 score)))))
	(if no-end
	    (setf (note1 sundry) (make-instance 'note 
				  :line 0 
				  :x1 (if *cmn-staff* (box-x1 *cmn-staff*) (box-x1 score)))))
	
	(let* ((use-italian *use-italian-octave-signs*)
	       (direction (if (plusp (octave sundry)) :up :down))
	       (octaves (octave sundry))
	       (add-bassa *add-bassa-to-octave-signs*)
	       (x0 (- (x0 (note0 sundry)) (if no-start 0 .2)))
	       (x1 (x1 (note1 sundry)))
	       (y0 (+ (staff-y0 note) 
		      (if (eq direction :up)
			  (* (max 12
				  (+ (vertical-separation sundry)
				     (or (max-line sundry) 0)))
			     *staff-line-separation*)
			(* (min -4 
				(- (or (max-line sundry) (minimum-line note) 0)
				   (vertical-separation sundry)))
			   *staff-line-separation*))))
	       (octmsg (if (= octaves 1)
			   (if use-italian
			       "8va"	;why not ottava
			     "8")
			 (if (= octaves 2)
			     (if use-italian
				 "15ma"	;why not quindicesima
			       "15")
			   (if (= octaves -1)
			       (if use-italian
				   (if add-bassa
				       "8va bassa"
				     "8va")
				 "8")
			     (if (= octaves -2)
				 (if use-italian
				     (if add-bassa
					 "15ma bassa"
				       "15ma")
				   "15")
			       "")))))
	       (siz (scr-size score))
	       (mid-y0 (+ y0 (if (eq direction :up) .2 -.2)))
	       (px1 (* (+ x0 (* .3 (length octmsg))))))
	  
	  (incf x0 (vis-dx sundry))
	  (incf y0 (vis-dy sundry))
	  (incf mid-y0 (vis-dy sundry))
	  (incf px1 (vis-dx sundry))
	  (if (not no-start)
	      (show score (cmn-text :font-name (font-name sundry)
				  :font-size (floor (* siz (font-scaler sundry)))
				  :letters octmsg)
		    :matrix (translate-matrix score sundry x0 (- y0 (if (eq direction :up) 0 .4)))))
	  (when (< (+ px1 .5) x1)	; i.e. when there's room for a dashed line given our 15ma bassa silliness
	    (moveto score (+ px1 .2) mid-y0)
	    (lineto score (- x1 .25) mid-y0 :pattern (map 'list #'(lambda (pt) 
								    (* pt (/ (scr-size score) 40))) 
							  (octave-sign-pattern sundry)))
	    (draw score))
	  (when (not no-end)
	    (moveto score (- x1 .2) mid-y0)
	    (rlineto score .2 0)
	    (lineto score x1 y0)
	    (draw score))))))

;;; (cmn staff treble (c4 q begin-octave-up) c4 q line-break c4 q c4 q line-break c4 q (c4 q end-octave-up))



;;;
;;; ----------------    graphics
;;;
;;; graphics function (i.e. (c4 q (graphics (file "hi.eps") (rotate 90))) places hi.eps at the middle c rotated 90 degress)

(defun read-4-numbers (gbb)
  (let ((gbb-as-list nil))
    (loop for i from 0 to 3 do
      (multiple-value-bind 
	  (int len)
	  (read-from-string gbb)
	(push int gbb-as-list)
	(if (< len (length gbb)) (setf gbb (subseq gbb len)))))
    (nreverse gbb-as-list)))

(defun insert-eps-file (score file)
  ;; ideally we would leave the PostScript comments in the inserted file alone, but
  ;; that confused WriteNow which believes the first %%BoundingBox it sees and it 
  ;; quits upon encountering %%Trailer.  So, we'll strip out those portions...
  (let ((gbb nil))
    (with-open-file (s file)
      (loop do
	(let ((text (read-line s nil :at-end)))
	  (if (not (eq text :at-end))
	      (progn
		(if (and (> (length text) 15) 
			 (string= "%%BoundingBox" (subseq text 0 13)))
		    (setf gbb (subseq text 14))
		  (if (or (< (length text) 9) 
			  (not (string= "%%Trailer" (subseq text 0 9))))
		      (g-send score text))))
	    (return-from insert-eps-file gbb)))))))

(defun file (name) (make-instance 'sundry :name :file :mark name))

(defun display-graphics (mark note score file)
  (let* ((y-off (+ (box-y0 mark) (vis-dy mark) (staff-y0 note) (* (line note) *staff-line-separation*)))
	 (x-off (+ (box-x0 note) (vis-dx mark) (box-x0 mark)))
	 (matr (translate-matrix score mark x-off y-off)))
    (matrix-front score matr)
    ;; PS ref man p717 ff
    (g-send score (format nil "/b4_state save def userdict begin~%~
                                   0 setgray 0 setlinecap 1 setlinewidth 0 setlinejoin 10 setmiterlimit [] 0 setdash~%~
                                   newpath /showpage {} def"))
    (let ((gbb (read-4-numbers (insert-eps-file score file))))
      (g-send score "end b4_state restore")
      (matrix-back score))))

(defun graphics (&rest objects)
  (let ((new-graph (make-instance 'sundry :name :graphics))
	(file nil))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-graph (argument act))
	  (if (sundry-p act)
	      (if (eq (sundry-name act) :file)
		  (setf file (sundry-mark act))
		(push act (marks new-graph)))))))
    (setf (sundry-mark new-graph)
	  #'(lambda (mark note score &optional justifying)
	      (if (not justifying)
		  ;; at housing time server size is 1.0 but output bounds were true, not scaled,
		  ;; and we have no clean way to see the scaling, so we'll put off the bounds check.
		  (display-graphics mark note score file))))
    new-graph))

