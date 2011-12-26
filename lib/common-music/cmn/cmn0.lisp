;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-

(in-package :cmn)

(defvar *cmn-score* nil)		;top of current score structure (needed globally for debugging and error messages)
(defvar *cmn-units* :inches)		;can also be :cm

;;;
;;; ----------------    score (top node of entire data structure)
;;;

(defvar *cmn-score-size* nil)
(defvar *old-cmn-score-size* nil)
(defvar *cmn-output-type* :postscript) ; no other choices anymore
(defvar *cmn-output-stream* nil)

(defclass score (visible)
  ((x :initarg :x :initform nil :accessor x :accessor scr-x) ;current drawing position
   (y :initarg :y :initform nil :accessor y :accessor scr-y)
   (size :initarg :size :initarg size :accessor size :accessor scr-size :initform 40)
   (scaling-matrix :initarg :scaling-matrix :accessor scaling-matrix :initform nil)
   (output :initform nil :initarg :output :accessor output) ;output command stream
   (output-file :initform nil :initarg :output-file :initarg output-file :accessor output-file)
   (line-width :initform 0 :initarg :line-width :accessor line-width)
   (prologue :initarg :prologue :initarg prologue :initform nil :accessor prologue)
   (bounded :initform nil :initarg :bounded :accessor bounded) ;is server supposed to track the excursion religiously.
   (output-type :initform *cmn-output-type* :initarg :output-type :accessor output-type)

   (systems :initarg :systems :accessor systems :initform nil)
   (title :initarg :title :accessor title :initform nil)
   (time-line :initarg :time-line :initform nil :accessor time-line)
   (time-lines :initarg :time-lines :initform nil :accessor time-lines)
   (line-data :initarg :line-data :initform nil :accessor line-data)
   (lines :initarg :lines :initform nil :accessor lines)
   (pages :initarg :pages :initform nil :accessor pages)
   (staves :initarg :staves :initform nil :accessor staves)

   ;; now user-settable slots
   (page-height :initarg :page-height :accessor page-height :initform 11.0)
   (page-width :initarg :page-width :accessor page-width :initform 8.5)
   (left-margin :initarg :left-margin :accessor left-margin :initform 0.5)
   (right-margin :initarg :right-margin :accessor right-margin :initform 0.5)
   (header-margin :initarg :header-margin :accessor header-margin :initform 1.0)
   (footer-margin :initarg :footer-margin :accessor footer-margin :initform 1.0)
   (free-expansion-factor :initarg :free-expansion-factor :accessor free-expansion-factor :initform 1.25)
   (line-separation :initarg :line-separation :accessor line-separation :initform 2.0)
   (staff-separation :initarg :staff-separation :accessor staff-separation :initform 1.5)
   (system-separation :initarg :system-separation :accessor system-separation :initform 1.5)
   (regularize :initarg :regularize :accessor regularize :initform nil)
   (beats-per-quarter-note :initarg :beats-per-quarter-note :accessor beats-per-quarter-note :initform 1)
   (initial-octave :initarg :initial-octave :accessor initial-octave :initform 4)
   (metronome :initarg :metronome :accessor metronome :initform 60)
   (old-style-beams :initarg :old-style-beams :initform nil :accessor old-style-beams)
   (automatic-page-numbers :initarg :automatic-page-numbers :initform nil :accessor automatic-page-numbers)
   (automatic-measure-numbers :initarg :automatic-measure-numbers :initform nil :accessor automatic-measure-numbers)
   (first-measure-number :initarg :first-measure-number :initform nil :accessor first-measure-number)
   (always-show-gliss-name :initarg :always-show-gliss-name :initform t :accessor always-show-gliss-name)
   (always-show-staff-names :initarg :always-show-staff-names :initform t :accessor always-show-staff-names)
   (always-show-clefs :initarg :always-show-clefs :initform t :accessor always-show-clefs)
   (automatic-clef-size :initarg :automatic-clef-size :initform 1.0 :accessor automatic-clef-size)
   (all-output-in-one-file :initarg :all-output-in-one-file :initform nil :accessor all-output-in-one-file)
   (automatic-line-breaks :initarg :automatic-line-breaks :initform t :accessor automatic-line-breaks)
   (automatic-octave-signs :initarg :automatic-octave-signs :initform t :accessor automatic-octave-signs)
   (automatic-beams :initarg :automatic-beams :initform t :accessor automatic-beams)
   (liberal-automatic-beams :initarg :liberal-automatic-beams :initform t :accessor liberal-automatic-beams)
   (automatic-ties :initarg :automatic-ties :initform t :accessor automatic-ties)
   (automatic-bars :initarg :automatic-bars :initform t :accessor automatic-bars)
   (automatic-rests :initarg :automatic-rests :initform t :accessor automatic-rests)
   (automatic-naturals :initarg :automatic-naturals :initform nil :accessor automatic-naturals)
   (implicit-accidental-style :initarg :implicit-accidental-style :initform :new-style :accessor implicit-accidental-style)
   (implicit-accidental-duration :initarg :implicit-accidental-duration :initform nil :accessor implicit-accidental-duration)
   (redundant-accidentals :initarg :redundant-accidentals :initform t :accessor redundant-accidentals)
   (automatic-beat-subdivision-numbers :initarg :automatic-beat-subdivision-numbers :initform t :accessor automatic-beat-subdivision-numbers)
   (show-rulers :initarg :show-rulers :initform nil :accessor show-rulers)
   (initial-onset :initarg :initial-onset :initform 0 :accessor initial-onset)
   (section-onset :initarg :section-onset :initform 0 :accessor section-onset)
   (full-last-line :initarg :full-last-line :initform nil :accessor full-last-line)
   (line-hook :initarg :line-hook :initform nil :accessor line-hook)
   (page-hook :initarg :page-hook :initform nil :accessor page-hook)
   (spacing-hook :initarg :spacing-hook :initform nil :accessor spacing-hook)
   ;; we use these "hooks", analogous to lisp's eval-hook because we want to make local
   ;;  modifications to a particular evaluation of the cmn function, and the equivalent mechanism within CLOS
   ;;  (using generic-flet) is not implemented in pcl.
   (use-abbreviated-staff-names :initarg :use-abbreviated-staff-names :initform t :accessor use-abbreviated-staff-names)
   (text-connecting-pattern :initarg :text-connecting-pattern :initform '(10 0) :accessor text-connecting-pattern)
   (text-connecting-thickness :initarg :text-connecting-thickness :initform 0.02 :accessor text-connecting-thickness)
   ;; these affect line layout and are most easily handled here (could be moved)
   (staff-name-font :initarg :staff-name-font :initform (normal-font) :accessor staff-name-font)
   (staff-name-font-scaler :initarg :staff-name-font-scaler :initform .4 :accessor staff-name-font-scaler)
   (staff-name-font-minimum-size :initarg :staff-name-font-minimum-size :initform 8 :accessor staff-name-font-minimum-size)
   (embedded :initarg :embedded :initform nil :accessor embedded)
   (draw-list :initform nil :accessor draw-list) ; postscript output stream optimization
   (saved-vars :initform nil :accessor saved-vars)
   (music-font :initform nil :initarg :music-font :accessor music-font)
   (page-color :initform nil :initarg :page-color :accessor page-color)
   (one-line-per-page :initform nil :initarg :one-line-per-page :accessor one-line-per-page)
   (title-separation :initform nil :initarg :title-separation :accessor title-separation)
   ))

#|
(cmn (page-color '(0.3 1. 0.0)) (color '(0.0 0.0 1.0))  (c5 q (walls '(1 3)) (begin-glissando (rotate 12)) (text-> "+"))
       (g5 q (note-head :arrow-up) (dy 0.3) (end-glissando) in-parentheses (-text "o")))
|#

(deferred-action title)
(deferred-action page-height)
(deferred-action page-width)
(deferred-action left-margin)
(deferred-action right-margin)
(deferred-action header-margin)
(deferred-action footer-margin)
(deferred-action free-expansion-factor)
(deferred-action staff-separation)
(deferred-action system-separation)
(deferred-action regularize)
(deferred-action music-font)
(deferred-action one-line-per-page)
(deferred-action title-separation)
(deferred-action page-color)

;;;  here we need to know as fast as possible what the score size is

(defmethod size ((val number)) 
  (if (not *cmn-score-size*) (setf *cmn-score-size* val))
  (make-self-acting :action #'(lambda (obj arg) (setf (size obj) arg)) :argument val))


(defun prolog (func) (make-self-acting :action #'setf-prolog :argument func))
(defun setf-prolog (obj val) (push val (prologue obj)))


;;;  in very large scores, cmn can take a long time before trying to open the output file,
;;;  which is annoying if it dies in the attempt -- output-file therefore makes an
;;;  immediate attempt to open the file so that cmn dies right away if it's going to anyway.

(defmethod output-file (name)
  (let ((full-name name)
	(fil nil))
    (if (not (pathname-type full-name)) 
	(setf full-name (concatenate 'string full-name ".eps")))
    (setf fil (open full-name :direction :output :if-exists :supersede))
    (close fil)
    #+(and mcl (not openmcl))
    (progn (ccl:set-mac-file-type full-name :EPSF)
	   (ccl:set-mac-file-creator full-name :|gsVR|))
    (make-self-acting :action #'(lambda (obj arg) (setf (output-file obj) arg)) :argument name)))

;;; embedded cmn calls (in with-cmn for example) need to get the outer cmn's output-type so we need it set immediately

(defvar *current-output-type* nil)

(defmethod output-type (type)
  (setf *current-output-type* type)
  (make-self-acting :action #'(lambda (obj arg) (setf (output-type obj) arg)) :argument type))

(deferred-action beats-per-quarter-note)
(deferred-action initial-octave)
(deferred-action metronome)
(deferred-action old-style-beams)
(deferred-action automatic-page-numbers)
(deferred-action automatic-measure-numbers)
(deferred-action first-measure-number)
(deferred-action staff-name-font)
(deferred-action staff-name-font-scaler)
(deferred-action staff-name-font-minimum-size)
(deferred-action use-abbreviated-staff-names)
(deferred-action text-connecting-pattern)
(deferred-action text-connecting-thickness)
(deferred-action always-show-gliss-name)
(deferred-action always-show-staff-names)
(deferred-action always-show-clefs)
(deferred-action automatic-clef-size)
(deferred-action all-output-in-one-file)
(deferred-action automatic-line-breaks)
(deferred-action automatic-octave-signs)
(deferred-action automatic-beams)
(deferred-action liberal-automatic-beams)
(deferred-action automatic-ties)
(deferred-action automatic-bars)
(deferred-action automatic-rests)
(deferred-action automatic-naturals)
(deferred-action implicit-accidental-style)
(deferred-action implicit-accidental-duration)
(deferred-action redundant-accidentals)
(deferred-action automatic-beat-subdivision-numbers)
(deferred-action show-rulers)
(deferred-action initial-onset)
(deferred-action section-onset)
(deferred-action full-last-line)
(deferred-action line-hook)
(deferred-action page-hook)
(deferred-action spacing-hook)

(defmethod copy ((score score) &optional object)
  (if (not object)
      (make-instance 'score 
	  :systems (loop for system in (systems score) collect (copy system))
	  :title (title score) 
	  :time-line (copy-tree (time-line score))
	  :time-lines (copy-tree (time-lines score))
	  :line-data (copy-tree (line-data score))
	  :lines (lines score)
	  :pages (pages score)
	  :staves (staves score)
	  :size (size score) 
	  :page-height (page-height score)
	  :page-width (page-width score)
	  :left-margin (left-margin score)
	  :right-margin (right-margin score)
	  :header-margin (header-margin score)
	  :footer-margin (footer-margin score)
	  :free-expansion-factor (free-expansion-factor score)
	  :line-separation (line-separation score)
	  :staff-separation (staff-separation score)
	  :system-separation (system-separation score)
	  :size (size score)
	  :regularize (regularize score)
	  :music-font (music-font score)
	  :one-line-per-page (one-line-per-page score)
	  :title-separation (title-separation score)
	  :beats-per-quarter-note (beats-per-quarter-note score)
	  :initial-octave (initial-octave score)
	  :metronome (metronome score)
	  :old-style-beams (old-style-beams score)
	  :automatic-page-numbers (automatic-page-numbers score)
	  :automatic-measure-numbers (automatic-measure-numbers score)
	  :first-measure-number (first-measure-number score)
	  :staff-name-font (staff-name-font score)
	  :staff-name-font-scaler (staff-name-font-scaler score)
	  :staff-name-font-minimum-size (staff-name-font-minimum-size score)
	  :use-abbreviated-staff-names (use-abbreviated-staff-names score)
	  :text-connecting-pattern (text-connecting-pattern score)
 	  :text-connecting-thickness (text-connecting-thickness score)
	  :always-show-gliss-name (always-show-gliss-name score)
	  :always-show-staff-names (always-show-staff-names score)
 	  :always-show-clefs (always-show-clefs score)
 	  :automatic-clef-size (automatic-clef-size score)
	  :all-output-in-one-file (all-output-in-one-file score)
	  :automatic-line-breaks (automatic-line-breaks score)
	  :automatic-octave-signs (automatic-octave-signs score)
	  :automatic-beams (automatic-beams score)
	  :liberal-automatic-beams (liberal-automatic-beams score)
	  :automatic-ties (automatic-ties score)
	  :automatic-bars (automatic-bars score)
	  :automatic-rests (automatic-rests score)
	  :automatic-naturals (automatic-naturals score)
	  :implicit-accidental-style (implicit-accidental-style score)
	  :implicit-accidental-duration (implicit-accidental-duration score)
	  :redundant-accidentals (redundant-accidentals score)
	  :automatic-beat-subdivision-numbers (automatic-beat-subdivision-numbers score)
	  :show-rulers (show-rulers score)
	  :initial-onset (initial-onset score)
	  :section-onset (section-onset score)
	  :full-last-line (full-last-line score))
    (error "can't copy embedded scores yet")))


(defun print-if-changed (field field-name init-value &optional store-form keyword)
  (let ((init-char (if store-form "(" ":"))
	(end-char (if store-form ")" "")))
    (or (and (not init-value)
	     field
	     (format nil "~%  ~A~A ~A~A" init-char field-name (if keyword (format nil ":~(~A~)" field) field) end-char))
	(and init-value
	     (or (and (eq init-value t)
		      (not field)
		      (format nil "~%  ~A~A nil~A" init-char field-name end-char))
		 (and (numberp init-value)
		      (/= init-value field)
		      (format nil "~%  ~A~A ~A~A" init-char field-name field end-char))
		 (and (listp init-value)
		      (not (equal init-value field))
		      (format nil "~%  ~A~A '~A~A" init-char field-name field end-char))
		 (and (stringp init-value)
		      (not (string-equal init-value field))
		      (format nil "~%  ~A~A ~S~A" init-char field-name field end-char))
		 ""))
	"")))

(defun style-list (score &optional (all t) (sf nil))
  (remove-if #'(lambda (n) (or (null n) (= (length n) 0))) 
  (list 
   (if all (print-if-changed (lines score) "lines" nil sf) "")
   (if all (print-if-changed (pages score) "pages" nil sf) "")
   (if all (print-if-changed (staves score) "staves" nil sf) "")
   (print-if-changed (page-height score) "page-height" 11.0 sf)
   (print-if-changed (page-width score) "page-width" 8.5 sf)
   (print-if-changed (left-margin score) "left-margin" 0.5 sf)
   (print-if-changed (right-margin score) "right-margin" 0.5 sf)
   (print-if-changed (header-margin score) "header-margin" 1.0 sf)
   (print-if-changed (footer-margin score) "footer-margin" 1.0 sf)
   (print-if-changed (free-expansion-factor score) "free-expansion-factor" 1.25 sf)
   (print-if-changed (line-separation score) "line-separation" 2.0 sf)
   (print-if-changed (staff-separation score) "staff-separation" 1.5 sf)
   (print-if-changed (system-separation score) "system-separation" 1.5 sf)
   (print-if-changed (size score) "size" 40 sf)
   (print-if-changed (regularize score) "regularize" nil sf)
   (print-if-changed (music-font score) "music-font" nil sf)
   (print-if-changed (one-line-per-page score) "one-line-per-page" nil sf)
   (print-if-changed (title-separation score) "title-separation" nil sf)
   (print-if-changed (beats-per-quarter-note score) "beats-per-quarter-note" 1 sf)
   (print-if-changed (initial-octave score) "initial-octave" 4 sf)
   (print-if-changed (metronome score) "metronome" 60 sf)
   (print-if-changed (old-style-beams score) "old-style-beams" nil sf)
   (print-if-changed (automatic-page-numbers score) "automatic-page-numbers" nil sf)
   (print-if-changed (automatic-measure-numbers score) "automatic-measure-numbers" nil sf 
		     (and (automatic-measure-numbers score) (member (automatic-measure-numbers score) '(:by-line :by-page))))
   (print-if-changed (first-measure-number score) "first-measure-number" nil sf)
   (print-if-changed (staff-name-font score) "staff-name-font" (normal-font) sf)
   (print-if-changed (staff-name-font-scaler score) "staff-name-font-scaler" .4 sf)
   (print-if-changed (staff-name-font-minimum-size score) "staff-name-font-minimum-size" 8 sf)
   (print-if-changed (use-abbreviated-staff-names score) "use-abbreviated-staff-names" t sf)
   (print-if-changed (text-connecting-pattern score) "text-connecting-pattern" '(10 0) sf)
   (print-if-changed (text-connecting-thickness score) "text-connecting-thickness" 0.02 sf)
   (print-if-changed (always-show-gliss-name score) "always-show-gliss-name" t sf)
   (print-if-changed (always-show-staff-names score) "always-show-staff-names" t sf)
   (print-if-changed (always-show-clefs score) "always-show-clefs" t sf)
   (print-if-changed (automatic-clef-size score) "automatic-clef-size" t sf)
   (print-if-changed (all-output-in-one-file score) "all-output-in-one-file" nil sf)
   (print-if-changed (automatic-line-breaks score) "automatic-line-breaks" t sf)
   (print-if-changed (automatic-octave-signs score) "automatic-octave-signs" t sf)
   (print-if-changed (automatic-beams score) "automatic-beams" t sf)
   (print-if-changed (liberal-automatic-beams score) "liberal-automatic-beams" t sf)
   (print-if-changed (automatic-ties score) "automatic-ties" t sf)
   (print-if-changed (automatic-bars score) "automatic-bars" t sf)
   (print-if-changed (automatic-rests score) "automatic-rests" t sf)
   (print-if-changed (automatic-naturals score) "automatic-naturals" nil sf)
   (print-if-changed (implicit-accidental-style score) "implicit-accidental-style" :new-style sf)
   (print-if-changed (implicit-accidental-duration score) "implicit-accidental-duration" nil sf)
   (print-if-changed (redundant-accidentals score) "redundant-accidentals" t sf)
   (print-if-changed (automatic-beat-subdivision-numbers score) "automatic-beat-subdivision-numbers" t sf)
   (print-if-changed (show-rulers score) "show-rulers" nil sf)
   (print-if-changed (initial-onset score) "initial-onset" 0 sf)
   (print-if-changed (section-onset score) "section-onset" 0 sf)
   (print-if-changed (full-last-line score) "full-last-line" nil sf))))

(defmethod descry ((score score) &optional stream controller)
  (format stream "(score ~A~A~A~A~A~A~A~A~A~A~A~A~A~%  ~A)"
	  (if (title score) (format nil " :title ~A" (title score)) "")
	  (if (next-method-p) (call-next-method score stream (or controller score)) "")
	  (if (time-line score)
	      (format nil "~%   :time-line (list ~{~A~})" (time-line score))
	    "")
	  (if (time-lines score)
	      (format nil "~%   :time-lines (list ~{~%          ~A~})" (loop for ln in (time-lines score) collect (descry ln)))
	    "")
	  (if (line-data score)
	      (format nil "~%   :line-data (list ~{~%          ~A~})" (loop for ln in (line-data score) collect (descry ln)))
	    "")
	  (style-list score)

	  (if (x score) (format nil " :x ~A" (x score)) "")
	  (if (y score) (format nil " :y ~A" (y score)) "")
	  (if (size score) (format nil " :size ~A" (size score)) "")
	  (if (output score) (format nil " :output-file ~A" (output-file score)) "")
	  (format nil " (~(:~A~))" (output-type score))
	  (if (not (zerop (line-width score))) (format nil " :line-width ~A" (line-width score)) "")
	  (if (bounded score) (format nil " :bounded ~A" (bounded score)) "")
	  
	  (if (systems score) (format nil " :systems (list ~{~%     ~A~})" 
				      (loop for system in (systems score) collect (descry system stream score)))
	    "")))

(defmethod score-p ((obj t)) nil)
(defmethod score-p ((obj score)) t)

(defmethod identify ((score score))
  (with-output-to-string (s)
    (format s "(with-cmn ")
    (format s "~A~A" 
	  (identify-matrix score)
	  (identify-visible score))
    (identify-score score s nil)
    (format s ")")))

(defvar *staff-line-separation* .126)
(defvar *staff-dy* 1.018)
(defvar *staff-line-width* .02)
(defvar *barline-thickness* .02)
(defvar *double-barline-thickness* .1)
(defvar *brace-space* .125)
(defvar *dynamics-size* .8)
(defvar *dynamics-minimum-vertical-separation* 0.5)
(defvar *add-bassa-to-octave-signs* nil)
(defvar *use-italian-octave-signs* nil)
(defvar *accidental-to-note-head-space* .0625)
(defvar *first-dot-spacing* .2)
(defvar *dot-spacing* .15) ;was .2, AV uses .1, but that looks too close to me: (cmn (size 100) c4 q (dots 3))
(defvar *dot-vertical-spacing* 0.0)
(defvar *flag-vertical-spacing* .2)
(defvar *note-head-size* 1.0)
(defvar *stem-width* .0325)
(defvar *half-stem-width* .0125)
(defvar *ideal-stem-length* .5)
(defvar *curvy-flags* t)
(defvar *grace-note-size* .5)
(defvar *slur-thickness* .03)
(defvar *slur-curvature* .25)
(defvar *beam-spacing* (* 2.125 .126))
(defvar *maximum-subdivision-bracket-tilt* 0)
(defvar *tie-thickness* .04)
(defvar *tie-curvature* .125)
(defvar *maximum-slur-slope* .75)
(defvar *maximum-beam-tilt* .4)
(defvar *beam-width* (* 1.125 .126))
(defvar *partial-beam-length* .4)
(defvar *partial-stem-length* .1)
(defvar *beam-slope-trigger* .5)
(defvar *glissando-thickness* .01)
(defvar *accidental-size* 1.0)

;;; staff-line-separation moved to staff class below
(defun staff-dy (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *staff-dy* arg))))
(defun staff-line-width (n)
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *staff-line-width* arg))))
(defun barline-thickness (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *barline-thickness* arg))))
(defun double-barline-thickness (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *double-barline-thickness* arg))))
(defun brace-space (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *brace-space* arg))))
(defun dynamics-size (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *dynamics-size* arg))))
(defun dynamics-minimum-vertical-separation (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *dynamics-minimum-vertical-separation* arg))))
(defun add-bassa-to-octave-signs (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *add-bassa-to-octave-signs* arg))))
(defun use-italian-octave-signs (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *use-italian-octave-signs* arg))))
(defun stem-width (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *stem-width* arg) (setf *half-stem-width* (* .5 arg)))))
(defun ideal-stem-length (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *ideal-stem-length* arg))))
(defun curvy-flags (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *curvy-flags* arg))))
(defun grace-note-size (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *grace-note-size* arg))))
(defun maximum-subdivision-bracket-tilt (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *maximum-subdivision-bracket-tilt* arg))))
(defun tie-thickness (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *tie-thickness* arg))))
(defun maximum-slur-slope (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *maximum-slur-slope* arg))))
(defun maximum-beam-tilt (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *maximum-beam-tilt* arg))))
(defun beam-width (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *beam-width* arg))))
(defun partial-beam-length (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *partial-beam-length* arg))))
(defun partial-stem-length (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *partial-stem-length* arg))))
(defun beam-spacing (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *beam-spacing* arg))))
(defun beam-slope-trigger (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *beam-slope-trigger* arg))))
(defun default-font (n)
  (setf *cmn-default-font* n)
  nil)
(defun glissando-thickness (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *glissando-thickness* arg))))
(defun accidental-size (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *accidental-size* arg))))

(defun accidental-to-note-head-space (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *accidental-to-note-head-space* arg))))
(defun first-dot-spacing (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *first-dot-spacing* arg))))
(defun dot-spacing (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *dot-spacing* arg))))
(defun dot-vertical-spacing (n) 
  (make-self-acting :argument n :action #'(lambda (obj arg) (declare (ignore obj)) (setf *dot-vertical-spacing* arg))))

(defvar *use-circle-as-niente* nil)
(defun use-circle-as-niente (n) 
  (make-self-acting :argument n
                    :action #'(lambda (obj arg)
                                (declare (ignore obj))
                                (setf *use-circle-as-niente* arg))))

(defun save-cmn-vars ()
  (make-array 36 :initial-contents 
	      (list *staff-line-separation* *staff-dy* *staff-line-width* *barline-thickness* *double-barline-thickness* *brace-space* *dynamics-size*
		    *add-bassa-to-octave-signs* *use-italian-octave-signs* *accidental-to-note-head-space* *first-dot-spacing*
                    *dot-spacing* *flag-vertical-spacing* *note-head-size* *stem-width* *half-stem-width* *ideal-stem-length*
                    *curvy-flags* *grace-note-size* *slur-thickness* *slur-curvature* *beam-spacing* *dot-vertical-spacing*
                    *maximum-subdivision-bracket-tilt* *tie-thickness* *tie-curvature* *maximum-slur-slope* *maximum-beam-tilt*
                    *beam-width* *partial-beam-length* *partial-stem-length* *beam-slope-trigger* *cmn-default-font*
                    *use-circle-as-niente* *glissando-thickness* *accidental-size*)))

(defun restore-cmn-vars (saved-vars)
  (let ((i 0))
    (setf *staff-line-separation* (aref saved-vars i)) (incf i)
    (setf *staff-dy* (aref saved-vars i)) (incf i)
    (setf *staff-line-width* (aref saved-vars i)) (incf i)
    (setf *barline-thickness* (aref saved-vars i)) (incf i)
    (setf *double-barline-thickness* (aref saved-vars i)) (incf i)
    (setf *brace-space* (aref saved-vars i)) (incf i)
    (setf *dynamics-size* (aref saved-vars i)) (incf i)
    (setf *add-bassa-to-octave-signs* (aref saved-vars i)) (incf i)
    (setf *use-italian-octave-signs* (aref saved-vars i)) (incf i)
    (setf *accidental-to-note-head-space* (aref saved-vars i)) (incf i)
    (setf *first-dot-spacing* (aref saved-vars i)) (incf i)
    (setf *dot-spacing* (aref saved-vars i)) (incf i)
    (setf *flag-vertical-spacing* (aref saved-vars i)) (incf i)
    (setf *note-head-size* (aref saved-vars i)) (incf i)
    (setf *stem-width* (aref saved-vars i)) (incf i)
    (setf *half-stem-width* (aref saved-vars i)) (incf i)
    (setf *ideal-stem-length* (aref saved-vars i)) (incf i)
    (setf *curvy-flags* (aref saved-vars i)) (incf i)
    (setf *grace-note-size* (aref saved-vars i)) (incf i)
    (setf *slur-thickness* (aref saved-vars i)) (incf i)
    (setf *slur-curvature* (aref saved-vars i)) (incf i)
    (setf *beam-spacing* (aref saved-vars i)) (incf i)
    (setf *dot-vertical-spacing* (aref saved-vars i)) (incf i)
    (setf *maximum-subdivision-bracket-tilt* (aref saved-vars i)) (incf i)
    (setf *tie-thickness* (aref saved-vars i)) (incf i)
    (setf *tie-curvature* (aref saved-vars i)) (incf i)
    (setf *maximum-slur-slope* (aref saved-vars i)) (incf i)
    (setf *maximum-beam-tilt* (aref saved-vars i)) (incf i)
    (setf *beam-width* (aref saved-vars i)) (incf i)
    (setf *partial-beam-length* (aref saved-vars i)) (incf i)
    (setf *partial-stem-length* (aref saved-vars i)) (incf i)
    (setf *beam-slope-trigger* (aref saved-vars i)) (incf i)
    (setf *cmn-default-font* (aref saved-vars i)) (incf i)
    (setf *use-circle-as-niente* (aref saved-vars i)) (incf i)
    (setf *glissando-thickness* (aref saved-vars i)) (incf i)
    (setf *accidental-size* (aref saved-vars i)) (incf i)))


;;; ---------------- BASIC GRAPHICS FUNCTIONS ----------------

(defun clear-bounds (sc)
  (setf (box-x0 sc) 0.0)
  (setf (box-y0 sc) 0.0)
  (setf (box-x1 sc) 0.0)
  (setf (box-y1 sc) 0.0))

(defun bounds (score)
  (list (box-x0 score) (box-y0 score) (box-x1 score) (box-y1 score)))

(defun lineto (score x y &key pattern)
  (let* ((size (or *cmn-score-size* (scr-size score)))
	 (xs (float (* x size)))
	 (ys (float (* y size))))
    (if (output score)	
	(if pattern
	    (g-dashed-line score xs ys pattern nil)
	  (g-lineto score xs ys)))
    (setf (scr-x score) xs)
    (setf (scr-y score) ys)
    (when (not (bounded score))
      (setf (box-x0 score) (min (box-x0 score) x))
      (setf (box-y0 score) (min (box-y0 score) y))
      (setf (box-x1 score) (max (box-x1 score) x))
      (setf (box-y1 score) (max (box-y1 score) y)))))

(defun rlineto (score dx dy &key pattern)
  (let* ((size (or *cmn-score-size* (scr-size score)))
	 (dxs (float (* dx size)))
	 (dys (float (* dy size))))
    (if (output score)	
	(if pattern
	    (g-dashed-line score dxs dys pattern t)
	  (g-rlineto score dxs dys)))
    (incf (scr-x score) dxs)
    (incf (scr-y score) dys)
    (when (not (bounded score))
      (setf (box-x0 score) (min (box-x0 score) (/ (scr-x score) size)))
      (setf (box-y0 score) (min (box-y0 score) (/ (scr-y score) size)))
      (setf (box-x1 score) (max (box-x1 score) (/ (scr-x score) size)))
      (setf (box-y1 score) (max (box-y1 score) (/ (scr-y score) size))))))

(defun moveto (score x y)
  ;; most of the run time is spent here -- all in Lisp's format function
  (let* ((size (or *cmn-score-size* (scr-size score)))
	 (xs (float (* x size)))
	 (ys (float (* y size))))
    (if (output score)	
	(g-moveto score xs ys))
    (setf (scr-x score) xs)
    (setf (scr-y score) ys)
    (when (not (bounded score))
      (setf (box-x0 score) (min (box-x0 score) x))
      (setf (box-y0 score) (min (box-y0 score) y))
      (setf (box-x1 score) (max (box-x1 score) x))
      (setf (box-y1 score) (max (box-y1 score) y)))))

(defun rmoveto (score dx dy)
  (let* ((size (or *cmn-score-size* (scr-size score)))
	 (dxs (float (* dx size)))
	 (dys (float (* dy size))))
    (if (output score) 
	(g-rmoveto score dxs dys))
    (incf (scr-x score) dxs)
    (incf (scr-y score) dys)
    (when (not (bounded score))
      (setf (box-x0 score) (min (box-x0 score) (/ (scr-x score) size)))
      (setf (box-y0 score) (min (box-y0 score) (/ (scr-y score) size)))
      (setf (box-x1 score) (max (box-x1 score) (/ (scr-x score) size)))
      (setf (box-y1 score) (max (box-y1 score) (/ (scr-y score) size))))))

(defun comment (score x)
  (if (output score) 
      (g-comment score x)))

(defmethod (setf line-width) :after (x (score score))
  (if (output score) 
      (g-set-line-width score (* x (or *cmn-score-size* (scr-size score))))))

(defmethod (setf color) :after (xx (score score))
  (if (output score)
      (g-set-color score (or xx (list 0 0 0)))))

(defun draw (score &optional width)      ;width can be t = outlined not filled
  (if (output score)
      (if (and width (numberp width))
	  (g-draw score (round (* (scr-size score) width)))
	(g-draw score))))
		     
(defun matrix-front (score matrix)
  (if (output score)
      (g-push score (or matrix (list 1 0 0 1 0 0)))))
	  
(defun matrix-back (score)
  (if (output score)
      (g-pop score)))

(defmacro with-thickness (score obj default &body body)
  ;; color included here since these are usually in-place draw routines
  `(let ((old_thickness (line-width ,score))
	 (old_color (color ,score)))
     (setf (line-width ,score) (or (cmn-thickness ,obj) ,default))
     (if (color ,obj) (setf (color ,score) (color ,obj)))
     ,@body
     (if (color ,obj) (setf (color ,score) old_color))
     (setf (line-width ,score) old_thickness)))

(defmacro with-color (score obj &body body)
  `(let ((old_color (color ,score))
	 (old_pattern_type (pattern-type ,score)))
     (if (color ,obj) (setf (color ,score) (color ,obj)))
     (if (and (pattern-type ,obj) (not (eq (pattern-type ,obj) :outlined)))
	 (g-set-pattern score (pattern-type ,obj)))
     ,@body
     (if (and (pattern-type ,obj) (not (eq (pattern-type ,obj) :outlined)))
	 (g-set-pattern score old_pattern_type))
     (if (color ,obj) (setf (color ,score) old_color))))

(defvar func-ctr 0)

(defmethod show ((score score) (obj score-object-mixin) &key matrix data)
  (declare (ignore data))
  (let* ((outlined (and (eq (pattern-type obj) :outlined) (pattern-data obj)))
	 (func-name (and (not outlined)
			 (draw-list score)
			 (find (draw-func obj) (draw-list score) :key #'first))))
    (when (and (output score)
	       (not (music-font score))
	       (not outlined)
	       (or *cmn-output-stream* (not func-name)))
      ;; send out the definition, add name and func to draw-list
      (let ((ps-func-name (format nil "draw~D" (incf func-ctr))))
	(g-send score (format nil "/~A { " ps-func-name))
	(funcall (draw-func obj) score nil)
	(g-send score "} def ")
	(push (list (draw-func obj) ps-func-name) (draw-list score))
	(setf func-name (list (draw-func obj) ps-func-name))))
    (if matrix
	(when (not (invisible-matrix matrix))
	  (when (output score)
	    (matrix-front score matrix)
	    (with-color score obj
	      (if func-name 
		  (g-send score (second func-name))
		(funcall (draw-func obj) score outlined)))
	    (matrix-back score)))
      (progn
	(when (output score)
          (with-color score obj
	    (if func-name
		(g-send score (second func-name))
	      (funcall (draw-func obj) score outlined))))))))
			      
(defmacro with-scaling (score size x0 y0 &body body)
  `(let ((current-box (bounds ,score))
	 (current-scaling-matrix (scaling-matrix ,score))
	 (new-matrix (list ,size 0 0 ,size ,x0 ,y0)))
     ;; bounding-box for duration of this scaling is scaled both ways by size and translated by x0 y0
     (clear-bounds ,score)
     (setf (scaling-matrix ,score) (truncated-matrix-multiply (or current-scaling-matrix (list 1 0 0 1 0 0)) new-matrix))
     (matrix-front ,score new-matrix)
     ,@body
     ;; I originally had ,.body here, but in ACL that sometimes gets confused and repeats the last statement over and over
     (when (not (bounded ,score))
       ;; this division by (scr-size score) actually reflects a bug in the original way I did with-scaling,
       ;; but now code/cmn input files depend on the incorrect definition, so we kludge around it...
       (setf (box-x0 ,score) (min (first current-box) (+ (* ,size (box-x0 ,score)) (/ ,x0 (scr-size ,score)))))
       (setf (box-y0 ,score) (min (second current-box) (+ (* ,size (box-y0 ,score)) (/ ,y0 (scr-size ,score)))))
       (setf (box-x1 ,score) (max (third current-box) (+ (* ,size (box-x1 ,score)) (/ ,x0 (scr-size ,score)))))
       (setf (box-y1 ,score) (max (fourth current-box) (+ (* ,size (box-y1 ,score)) (/ ,y0 (scr-size ,score))))))
     (setf (scaling-matrix ,score) current-scaling-matrix)
     (matrix-back ,score)))

(defmacro with-transformation (score matrix x0 y0 &body body)
  `(let ((matr (copy-list (if (matrix-p ,matrix) (matrix ,matrix) ,matrix)))
	 (current-box (bounds ,score))
	 (current-scaling-matrix (scaling-matrix ,score)))
     ;; here the problem is similar to the with-scaling case, but we need to transform the
     ;; resultant box by the transforming matrix before accumulating the edges into the box
     (clear-bounds ,score)
     (setf (fifth matr) ,x0)
     (setf (sixth matr) ,y0)
     (setf (scaling-matrix ,score) 
       (truncated-matrix-multiply (or current-scaling-matrix 
				       (list 1 0 0 1 0 0))
				   matr))
     (matrix-front ,score matr)
     ,@body
     (let ((new-box (transform-box matr (list ,x0 ,y0) (bounds ,score))))
       (when (not (bounded ,score))
	 (setf (box-x0 ,score) (min (first current-box) (/ (first new-box) (scr-size ,score))))
	 (setf (box-y0 ,score) (min (second current-box) (/ (second new-box) (scr-size ,score))))
	 (setf (box-x1 ,score) (max (third current-box) (/ (third new-box) (scr-size ,score))))
	 (setf (box-y1 ,score) (max (fourth current-box) (/ (fourth new-box) (scr-size ,score))))))
     (setf (scaling-matrix ,score) current-scaling-matrix)
     (matrix-back ,score)))

(defun curveto (score x0 y0 x1 y1 x2 y2)
  (let* ((size (or *cmn-score-size* (scr-size score)))
	 (size-x0 (float (* size x0)))
	 (size-y0 (float (* size y0)))
	 (size-x1 (float (* size x1)))
	 (size-y1 (float (* size y1)))
	 (size-x2 (float (* size x2)))
	 (size-y2 (float (* size y2))))
    (if (output score)
	(g-curveto score size-x0 size-y0 size-x1 size-y1 size-x2 size-y2))
    (setf (scr-x score) size-x2)
    (setf (scr-y score) size-y2)
    (when (not (bounded score))
      (setf (box-x0 score) (min (box-x0 score) x0 x2))
      (setf (box-y0 score) (min (box-y0 score) y0 y2))
      (setf (box-x1 score) (max (box-x1 score) x0 x2))
      (setf (box-y1 score) (max (box-y1 score) y0 y2)))))

(defun fill-in (score &key even-odd)
  ;; cmn default is "winding rule", not "even-odd"
  (if (output score)
      (g-fill score (if even-odd " eofill" " fill"))))

(defun circle (score x y r &optional (ang1 0) (ang2 360) fill)
  (let* ((size (scr-size score)))
    (if (output score)
	(g-arc score (float (* size x)) (float (* size y)) (float (* size r)) ang1 ang2 fill))
    (when (not (bounded score))
      (setf (box-x0 score) (min (box-x0 score) (- x r)))
      (setf (box-y0 score) (min (box-y0 score) (- y r)))
      (setf (box-x1 score) (max (box-x1 score) (+ x r)))
      (setf (box-y1 score) (max (box-y1 score) (+ y r))))))

(defun header (score &key name)
  (g-header score 
	    name 
	    #+(or excl GCL) (first (last (pathname-directory (user-homedir-pathname))))
	    #-(or excl GCL) nil))


;;; one inch = 2.54 cm
(defun inches-to-ps (n) (if (eq *cmn-units* :inches) (* 72 n) (/ (* 72 n) 2.54)))
(defun inches-to-font-units (score n) (/ (inches-to-ps n) (scr-size score)))

;;; in-inches called only from user-view -- cannot depend on *cmn-score* here
(defun in-inches (n) (/ (* 72 n) (or *cmn-score-size* (and *cmn-score* (scr-size *cmn-score*)) 40)))
(defun in-cm (n) (/ (* 72 n) (* 2.54 (or *cmn-score-size* (and *cmn-score* (scr-size *cmn-score*)) 40))))


(defun footer (score)
  (flet ((font-inches (n) (/ (* 72 n) (scr-size score))))
    (g-footer score)
    (if (not (bounded score))
	(progn
	  ;; should we pad x1 out to the page-width here? what about header-margin?
	  (setf (box-y1 score) (max (box-y1 score)
				    (min (+ (box-y1 score) (font-inches (header-margin *cmn-score*)))
					 (font-inches (page-height *cmn-score*)))))
	  (if (and (lines score) (> (lines score) 1))
	      (setf (box-x1 score) (max (box-x1 score)
					(font-inches (page-width *cmn-score*))))
	    (if (< (box-x1 score) (font-inches (- (page-width *cmn-score*) (right-margin *cmn-score*))))
		(incf (box-x1 score) (font-inches (right-margin *cmn-score*)))))
	  (g-bbox score)))))


(defvar *cmn-output-pathname* "aaa.eps")
(defvar *cmn-initialized* nil)


;;; removed args font size title 10-Feb-94 -- never used and causing confusion

(defun page-number (score page &optional eject)
  ;; purely for Postscript's benefit (multi-page output comment)
  (when (output score) 
    (g-page-number score page eject)))

(defun initialize (score &key file bounded)
  (if (not *cmn-initialized*)
      (progn
	#+(and mcl (not openmcl)) (mcl-initialize)
	(if (not (output-file score)) (setf (output-file score) (or file *cmn-output-pathname*)))
	(if (not (pathname-type (output-file score))) 
	    (setf (output-file score) (concatenate 'string (output-file score) ".eps")))
	(setf (output score) (or *cmn-output-stream*
				 (open (output-file score)
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)))
	(setf (bounded score) bounded)
	(if (not (scr-size score)) (setf (scr-size score) 40))
	(if (not *cmn-score-size*) (setf *cmn-score-size* (scr-size score)))
	(let ((ltitle (title *cmn-score*)))
	  (if (and ltitle (text-p ltitle))
	      (header score :name (letters ltitle))
	    (header score :name ltitle)))
	(g-set-line-width score 0)
	(setf *cmn-initialized* score)
	score)
    (progn
      (setf (size score) (size *cmn-initialized*))
      (setf (scaling-matrix score) (scaling-matrix *cmn-initialized*))
      (setf (output score) (output *cmn-initialized*))
      (setf (output-file score) (output-file *cmn-initialized*))
      (setf (bounded score) (bounded *cmn-initialized*))
      score)))

(defun clear-score (score)
  (if (not *cmn-output-stream*) (setf (output score) nil))
  (clear-bounds score)
  (setf (scr-x score) 0)
  (setf (scr-y score) 0))

(defvar *cmn-preview* nil)

(defun finalize (score &key continue)
  (footer score)
  (setf *cmn-initialized* nil)
  (if (not *cmn-output-stream*)
      (close (output score)))
  (setf (output-file score) nil)
  (clear-score score)
  #+(and mcl (not openmcl)) (mcl-finalize)
  (when (not continue)
    (if (saved-vars score) (restore-cmn-vars (saved-vars score)))
    (setf *cmn-score-size* nil)))

;;; (cmn (beam-width .4) treble c4 e c4 e page-break c4 e c4 e)
;;; that is, don't restore-cmn-vars after a simple page-break!


(defvar *cmn-system* nil)		;for better error reporting
(defvar *cmn-staff* nil)
(defvar *cmn-staff-data* nil)
(defvar *cmn-object* nil)
(defvar *cmn-owning-object* nil)
(defvar *cmn-page* nil)
(defvar cmn-pipe-0 nil)
(defvar cmn-pipe-1 nil)
(defvar cmn-pipe-2 nil)


;;; method display and house score are at very end -- same for systems?

(defvar no-initialization nil)

(defun scorify (objects)
  (let ((sections nil)
	(actions nil)
	(marks nil)
	(systems nil))
    (if (and (= (length objects) 1) (score-p (first objects)))
	(setf *cmn-score* (first objects))
      (let ((new-score (make-instance 'score)))
	(setf *cmn-score* new-score)
	(setf (saved-vars *cmn-score*) (save-cmn-vars))
	(when (eq *cmn-units* :cm)
	  (setf (page-width new-score) 21.0)           ;A4 European paper format
	  (setf (page-height new-score) 29.7)
	  (setf (left-margin new-score) 1.0)
	  (setf (right-margin new-score) 1.0)
	  (setf (header-margin new-score) 2.0)
	  (setf (footer-margin new-score) 2.0))
	(multiple-value-setq
	    (sections systems actions marks)
	  (notify new-score objects))
	(setf (systems new-score) systems)
	(setf (marks new-score) marks)
	(loop for act in actions do
	  (funcall (action act) new-score (argument act)))))
    (if (title *cmn-score*) 
	(add-to-marks *cmn-score* (list (make-title (title *cmn-score*)))))
    (if (and no-initialization *current-output-type*)
	(setf (output-type *cmn-score*) *current-output-type*))
    (if (not no-initialization) 
	(initialize *cmn-score*)
      (setf (embedded *cmn-score*) t))
    (if sections
	(let ((new-score *cmn-score*)
	      (scores (loop for sct in sections collect (scorify (append actions marks (data sct))))))
	  (if systems
	      (push new-score scores)
	    (progn
	      (setf *cmn-score* (first scores))
	      scores)))
      *cmn-score*)))

(defmethod notify ((score score) &optional objects)
  ;; make sure everything in the objects list is parcelled out to the correct portion of the score
  (let ((sections nil)
	(systems nil)
	(actions nil)
	(marks nil)
	(object-list objects)
	(current-default-system nil)
	(current-default-system-data nil))
    (loop while object-list do
      (let ((object (pop object-list)))
	(when object
	  (if (score-object-list-p object)
	      (setf object-list (append (disgorge object) object-list))
	    (if (section-p object)
		(push object sections)
	      (if (system-p object)
		  (progn
		    (if current-default-system
			(push (apply #'system (nreverse current-default-system-data)) systems))
		    (setf current-default-system-data nil)
		    (if (write-protected object)
			(setf current-default-system object)
		      (progn
			(setf current-default-system nil)
			(if (member object systems) (cmn-error "attempt to load the same system twice"))
			(push object systems))))
		(if (not current-default-system)
		    (if (self-acting-p object)
			(push object actions)
		      (if (or (text-p object) (sundry-p object))
			  (push object marks)
			(progn
			  (setf current-default-system (make-instance 'write-protected-system))
			  (push object current-default-system-data))))
		  (progn
		    (if (not current-default-system)
			(setf current-default-system (make-instance 'write-protected-system)))
		    (push object current-default-system-data)))))))))
    (if current-default-system-data (push (apply #'system (nreverse current-default-system-data)) systems))
    (values (nreverse sections) (nreverse systems) (nreverse actions) marks)))


;;;
;;; --------------    overall organization of cmn:
;;;

(defvar tie-stack nil)
(defvar slur-stack nil)
(defvar glissando-stack nil)
(defvar tremolo-stack nil)
(defvar crescendo-stack nil)
(defvar diminuendo-stack nil)
(defvar text-stack nil)
(defvar octave-stack nil)
(defvar rehearsal-stack nil)
(defvar beam-stack nil)
(defvar *cmn-hidden-sections* nil)

(defun cmn (&rest objects)
  (when objects				; if no args, cmn is a no-op
    (unwind-protect
	(catch 'cannot-open-file	; open file error aborts entire process
	  (cmn-finalize			; cleanup, return complete score structure (for subsequent editing)
	   (drawify			; drawify draws the result (normally creates a separate .eps file for each page)
	    (filterify			; filterify filters out "uninteresting" portions of the final score (for page extraction)
	     (slurify			; slurify adds slurs
	      (markify			; markify adds most marks like fingerings, staccato, sF, etc
	       (tieify			; tieify adds ties
		(beamify		; beamify adds beams
		 (justify		; justify adds in white space to make each line and page look pretty
		  (pagify		; pagify breaks the linified form into pages
		   (linify		; linify breaks the compacted form into lines
		    (compactify		; compactify aligns boxes across all systems
		     (boxify		; boxify finds bounding boxes and center points
		      (fillify		; fillify does the first level of filling in missing data (bar lines, rests etc)
		       (scorify		; scorify (via notify) regularizes the input stream of objects
			(edify		; edify filters out all but "interesting" data (i.e. for part extraction, transposition, etc)
			 (cmn-initialize objects)))))))))))))))))
      (progn
	(if (not no-initialization)
	    (setf tie-stack nil
		  slur-stack nil
		  glissando-stack nil
		  crescendo-stack nil
 		  diminuendo-stack nil
		  tremolo-stack nil
		  text-stack nil
		  rehearsal-stack nil
		  beam-stack nil
		  octave-stack nil
		  cmn-pipe-0 nil
		  cmn-pipe-1 nil
		  cmn-pipe-2 nil
		  *cmn-hidden-sections* nil
		  *cmn-score-size* nil
		  *old-cmn-score-size* nil
		  *cmn-system* nil
		  *cmn-staff* nil
		  *cmn-staff-data* nil
		  *cmn-page* nil
		  *cmn-object* nil
		  *cmn-owning-object* nil))))))

(defun check-for-left-overs ()
  (if tie-stack 
      (format t "~%begin-tie without matching end-tie:~{~%    ~A~}"
	      (loop for tie in (reverse tie-stack) collect (descry tie))))
  (if slur-stack 
      (format t "~%begin-slur without matching end-slur:~{~%    ~A~}" 
	      (loop for slur in (reverse slur-stack) collect (descry slur))))
  (if glissando-stack 
      (format t "~%begin-glissando without matching end-glissando:~{~%    ~A~}" 
	      (loop for gliss in (reverse glissando-stack) collect (descry gliss))))
  (if tremolo-stack 
      (format t "~%begin-tremolo without matching end-tremolo:~{~%    ~A~}" 
	      (loop for trem in (reverse tremolo-stack) collect (descry trem))))
  (if crescendo-stack 
      (format t "~%begin-crescendo or diminuendo without matching end:~{~%    ~A~}"
	      (loop for cresc in (reverse crescendo-stack) collect (descry cresc))))
  (if octave-stack 
      (if (listp octave-stack)
	  (format t "~%begin-octave without matching end-octave:~{~%    ~A~}" 
		  (loop for oct in (reverse octave-stack) collect (descry oct)))
	(format t "~%begin-octave, but no end-octave")))
  (if beam-stack 
      (format t "~%begin-beam without matching end-beam:~{~%    ~A~}" 
	      (loop for beam in (reverse beam-stack) collect (descry beam)))))

(defvar editor-hook nil)
(defvar filter-hook nil)
(defvar page-hook nil)
(defvar line-hook nil)

(defmethod cmn-finalize ((score score)) 
  (if (not no-initialization)		;might want to save/restore "hooks" across with-cmn
      (progn
	(check-for-left-overs)
	(setf editor-hook nil)
	(setf filter-hook nil)
	(finalize score)))
  score)

(defmethod cmn-initialize (objects) 
  objects)

;;; these two are methods so that callers can customize set-up and shut-down behavior.


;;; this is no longer needed -- cmn-initialize is now a method, so edify is just a sort of :after method

;;; edify is the bottom level editor
;;; it is passed the score (a list of lists of objects) and does what it wants

(defun edify (objects)
  (if editor-hook			;if an editor is present, let it munge the un-regularized data list
      (funcall editor-hook objects)
    objects))


;;; filterify should be done by making drawify a method

;;; filterify takes the finished score description (ready for the printer) and
;;; applies one last filtering pass

(defun filterify (score)
  (if filter-hook
      (funcall filter-hook score)
    score))


;;;
;;; ----------------    system
;;;
;;; a system is a group of staves with auxiliary annotations (measure numbers, overall tempo indications, etc)
;;;   It has been pointed out to me that most others use "system" to mean the entire group of staves that make
;;;   up what I call a "line", and that the word "section" is used where I am using "system".  I was thinking
;;;   of "section" as "portion of a piece" (see above), not "portion of an ensemble".  Hopefully this use of
;;;   the wrong word won't cause too much confusion.


(defclass system-mixin (score-object-mixin)
  ((staves :initarg :staves :initform nil :reader staves)
   (bracketing :initarg :bracketing :initform nil :reader bracketing)))

(defclass system (system-mixin score-object)
  ((staves :accessor staves)
   (bracketing :accessor bracketing)))

(defclass write-protected-system (write-protect system-mixin)
  ())

(defmethod system-p ((obj t)) nil)
(defmethod system-p ((obj system-mixin)) t)

(defvar system (make-instance 'write-protected-system))

(defmethod descry ((system system-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A"
	  "(system"
	  (if (bracketing system) 
	      (format nil " ~A:bracketing ~A"
 		      (if controller (format nil "~%       ") "")
		      (bracketing system))
	    "")
	  (if (staves system)
	      (format nil " ~A:staves (list ~{~%~A~})" 
		      (if controller (format nil "~%       ") "")
		      (loop for staff in (staves system) collect (descry staff stream (or controller system))))
	    "")
	  (if (next-method-p) (call-next-method system stream (or controller system)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((system system-mixin) &optional object)
  (let ((new-system (if (not object) (make-instance 'system)
		      (if (write-protected object) (copy object)
			object))))
    (setf (bracketing new-system) (bracketing system))
    (setf (staves new-system) (loop for staff in (staves system) collect (copy staff)))
    (if (next-method-p) (call-next-method system new-system))
    new-system))

(defun system (&rest objects)
  (let ((new-system (make-instance 'system)))
    (setf *cmn-system* new-system)
    (multiple-value-bind
	(staves actions marks)
	(notify new-system objects)
      (setf (staves new-system) staves)
      (setf (marks new-system) marks)
      (loop for act in actions do
	(funcall (action act) new-system (argument act))))
    (setf *cmn-system* nil)
    new-system))

(defmethod notify ((system system) &optional objects)
  (let ((default-staff nil)
	(staves nil)
	(actions nil)
	(marks nil)
	(object-list objects)
	(default-staff-data nil))
    (loop while object-list do
      (let ((object (pop object-list)))
	(when object
	  (if (score-object-list-p object)
	      (setf object-list (append (disgorge object) object-list))
	    (if (and (not default-staff-data)
		     (or (bracket-p object) (brace-p object)))
		(setf (bracketing system) object)
	      (if (staff-p object)
		  (progn
		    (if default-staff
			(push (apply #'staff (nreverse default-staff-data)) staves))
		    (setf default-staff-data nil)
		    (if (write-protected object)
			(setf default-staff object)
		      (progn
			(setf default-staff nil)
			(if (member object staves) (cmn-error "attempt to load the same staff twice"))
			(push object staves))))
		(if (not default-staff)
		    (if (self-acting-p object)
			(push object actions)
		      (if (or (text-p object) (sundry-p object))
			  (push object marks)
			(progn
			  (setf default-staff (make-instance 'write-protected-staff))
			  (push object default-staff-data))))
		  (progn
		    (if (not default-staff)
			(setf default-staff (make-instance 'write-protected-staff)))
		    (push object default-staff-data)))))))))
    (if default-staff (push (apply #'staff (nreverse default-staff-data)) staves))
    (values (nreverse staves) (nreverse actions) marks)))



;;;
;;; ----------------    staff 
;;;

(defclass staff-mixin (score-object-mixin)
  ((name :initarg :name :initform nil :reader inner-staff-name)
   (data :initarg :data :initform nil :reader staff-data)
   (size :initarg :size :initform nil :reader staff-size)
   (lines :initarg :lines :initform nil :reader staff-lines)
   (times :initarg :times :initform nil :reader times)
   (start-line :initarg :start-line :initform 0 :reader staff-start-line)
   (local-brace :initarg :local-brace :initform nil :reader staff-local-brace)
   (staff-name-x0 :initarg :staff-name-x0 :initform nil :reader staff-name-x0)
   (true-staff :initarg :true-staff :initform nil :reader true-staff)
   (back-staff :initarg :back-staff :initform nil :reader back-staff)
   (staff-user-data :initarg :staff-user-data :initform nil :reader staff-user-data)
   (staff-hooks :initarg :staff-hooks :initform nil :reader staff-hooks)
   (line-separation :initarg :line-separation :initform nil :reader line-separation :reader inner-line-separation)))

(defclass write-protected-staff (write-protect staff-mixin)
  ())

(defclass staff (staff-mixin score-object)
  ((name :accessor inner-staff-name)
   (data :accessor staff-data)
   (size :accessor staff-size)
   (lines :accessor staff-lines)
   (times :accessor times)
   (start-line :accessor start-line)
   (local-brace :accessor staff-local-brace)
   (staff-name-x0 :accessor staff-name-x0)
   (true-staff :accessor true-staff)
   (back-staff :accessor back-staff)
   (staff-user-data :accessor staff-user-data)
   (staff-hooks :accessor staff-hooks)
   (line-separation :accessor line-separation :reader inner-line-separation)))

(deferred-action staff-lines)
(deferred-action staff-size)
(deferred-action staff-name-x0)
(deferred-action start-line)
(deferred-action line-separation)

(defmethod staff-y0 ((staff staff-mixin)) (box-y0 staff))
(defmethod (setf staff-y0) (val (staff staff-mixin)) (setf (box-y0 staff) val))

(defmethod staff-name ((val text) &rest args)
  (declare (ignore args))
  (make-self-acting :action #'(lambda (obj arg) (setf (inner-staff-name obj) arg)) :argument val))

(defmethod staff-name ((inst staff-mixin) &rest args)
  (declare (ignore args))
  (inner-staff-name inst))

(defmethod (setf staff-name) (val (inst staff-mixin))
  (setf (inner-staff-name inst) val))

(defmethod dsud ((stf staff-mixin) num-i)
  (if (not (staff-user-data stf)) 
      (setf (staff-user-data stf) (list 0 0 0 0)))
  (if (> (second num-i) 3) 
      (cmn-error "staff can't handle d~A~D" 
		 (if (evenp (second num-i)) "x" "y")
		 (floor (second num-i) 2)))
  (setf (nth (second num-i) (staff-user-data stf)) (first num-i)))

(defun identify-user-data (data stream)
  (if data
      (format stream "~{~A~}"
	  (loop for dat in data and i from 0 collect
	    (if (not (zerop dat))
		(format nil " (d~A~D ~,3F)" (if (evenp i) "x" "y") (floor i 2) dat)
	      "")))
    ""))	


(defvar staff (make-instance 'write-protected-staff))

(defun identify-staff (score staff)
  (loop for system in (systems score) and sys-num from 1 do
    (loop for stf in (staves system) and staff-num from 1 do
      (if (eq stf staff)
	  (return-from identify-staff 
	    (format nil "~A in system ~D (staff ~D)" 
		    (or (staff-name staff) "un-named staff")
		    sys-num staff-num))))))

(defmethod descry ((staff staff-mixin) &optional stream controller)
  (let ((prespace (if controller  "        " "")))
    (format stream "~A(staff~A~A~A~A~A~A~A~A~A~A~A~A)"
	    prespace
	    (if (staff-name staff) (format nil " :name ~S" (staff-name staff)) "")
	    (if (staff-name-x0 staff) (format nil " :staff-name-x0 ~,3F" (staff-name-x0 staff)) "")
	    (if (staff-size staff) (format nil " :size ~A" (staff-size staff)) "")
	    (if (staff-lines staff) (format nil " :lines ~A" (staff-lines staff)) "")
	    (if (inner-line-separation staff) (format nil " :line-separation ~A" (inner-line-separation staff)) "")
	    (if (times staff) (format nil " :times '~A" (times staff)) "")
	    (if (and (numberp (staff-start-line staff))
		     (> (staff-start-line staff) 0))
		(format nil " :start-line ~D" (staff-start-line staff)) "")
	    (if (staff-local-brace staff) (format nil " :local-brace ~A" (staff-local-brace staff)) "")
	    (if (true-staff staff) (format nil " :true-staff ~A" (true-staff staff)) "")
	    (if (staff-user-data staff) (format nil " :staff-user-data '~A" (staff-user-data staff)) "")
	    (if (next-method-p) (call-next-method staff stream (or controller staff)) "")
	    (if (staff-data staff) 
		(format nil "~%~A  :data (list~{~%            ~A~})" 
			prespace
			(loop for object in (staff-data staff) 
			 collect (descry object)))
	      ""))))

(defmethod copy ((staff staff-mixin) &optional object)
  (let ((new-staff (if (not object) (make-instance 'staff)
		     (if (write-protected object) (copy object)
		       object))))
    (setf (staff-data new-staff) (loop for obj in (staff-data staff) collect (copy obj)))
    (setf (staff-name new-staff) (staff-name staff))
    (setf (staff-name-x0 new-staff) (staff-name-x0 staff))
    (setf (staff-size new-staff) (staff-size staff))
    (setf (staff-lines new-staff) (staff-lines staff))
    (setf (line-separation new-staff) (inner-line-separation staff))
    (setf (times new-staff) (times staff))
    (setf (start-line new-staff) (staff-start-line staff))
    (setf (true-staff new-staff) (true-staff staff))
    (setf (back-staff new-staff) (back-staff staff))
    (setf (staff-user-data new-staff) (staff-user-data staff))
    (setf (staff-hooks new-staff) (staff-hooks staff))
    (if (staff-local-brace staff) (setf (staff-local-brace new-staff) (copy (staff-local-brace staff))))
    (if (next-method-p) (call-next-method staff new-staff))
    new-staff))

(defmethod staff-p ((obj t)) nil)
(defmethod staff-p ((obj staff-mixin)) t)

(defun staff-line-separation (n) 
  (make-self-acting 
   :argument n 
   :action #'(lambda (obj arg) 
	       (if (not (staff-p obj)) 
		   (setf *staff-line-separation* arg)
		 (setf (line-separation obj) n)))))

(defun staff (&rest objects)
  (let ((new-staff (make-instance 'staff)))
    (multiple-value-bind
	(data actions local-brace)
	(notify new-staff objects)
      (setf *cmn-staff* new-staff)
      (setf (staff-data new-staff) data)
      (setf *cmn-staff-data* nil)
      (cmn-clear-pipe)
      (setf (staff-local-brace new-staff) local-brace)
      (loop for act in actions do
	(funcall (action act) new-staff (argument act)))
      (if (staff-name new-staff)
	  (add-to-marks new-staff (list (make-staff-name))))
      (setf *cmn-staff* nil)
      new-staff)))

(defvar all-instrument-names 
    '("piccolo" "flute" "alto flute" "bass flute" 
      "oboe" "oboe d'amore" "english horn"
      "e-flat clarinet" "b-flat clarinet" "a clarinet" "bass clarinet" "e-flat contrabass clarinet" "b-flat contrabass clarinet"
      "bassoon" "contrabassoon"
      "soprano saxophone" "alto saxophone" "tenor saxophone" "baritone saxophone" "bass saxophone"
      "french horn" "b-flat piccolo trumpet" "e-flat trumpet" "d trumpet" "c trumpet" "b-flat trumpet"
      "cornet" "fluegelhorn" "bass trumpet"
      "alto trombone" "tenor trombone" "bass trombone" 
      "baritone horn" "euphonium" "tuba"
      "timpani" "vibraphone" "xylophone" "marimba" "glockenspiel" "chimes"
      "celesta" "piano"
      "violin" "viola" "cello" "contrabass"))

(defvar all-abbreviated-instrument-names 
    '("picc." "fl." "alt. fl." "bs. fl." 
      "ob." "ob. d'am." "e.h."
      "e-flat cl." "b-flat cl." "a cl." "bs. cl" "e-flat cbs. cl." "b-flat cbs. cl."
      "bsn." "cbsn."
      "sop. sax." "alt. sax." "ten. sax." "bar. sax." "bs. sax."
      "hn." "b-flat picc. tpt." "e-flat tpt." "d tpt." "c tpt." "b-flat tpt." "cor." "flhn." "bs. tpt."
      "alt. tbn." "ten. tbn." "bs. tbn."
      "bar." "euph." "tba." 
      "timp." "vibe." "xyl." "mar." "glock." "chm."
      "cel." "pno."
      "vn." "vla." "vc." "cb."))

(defmethod abbreviation-of-instrument-name (name)
  (let ((pos (position name all-instrument-names :test #'string-equal)))
    (or (and pos
	     (nth pos all-abbreviated-instrument-names))
	name)))

(defmethod abbreviation-of-instrument-name ((text text))
  (let ((new-name (abbreviation-of-instrument-name (letters text)))
	(new-text (copy text))) ; keep font info even when abbreviated (21-Dec-98)
    (setf (letters new-text) new-name)
    new-text))

(defmethod tied-to ((staff staff-mixin))
  (make-self-acting
   :action #'(lambda (new-staff old-staff)
	       (setf (true-staff new-staff) old-staff)
	       (setf (staff-size new-staff) (staff-size old-staff))
	       nil)
   :argument staff))

(defun notifiable (obj1 obj2)
  (or (and (audible-p obj1)
	   (or (rhythm-p obj2)
	       (sundry-p obj2)
	       (pause-p obj2)
	       (text-p obj2)
	       (self-acting-p obj2)
	       (tag-p obj2)
	       (accidental-p obj2)
	       (dynamics-p obj2)))
      (and (score-object-p obj1)
	   (sundry-p obj2))))

(defmethod notify ((staff staff-mixin) &optional objects)
  ;; just make sure all unattached (write-protected) objects are repackaged -- for the
  ;; initial cases, place clef key meter name and size onto the current staff object (these can change)
  (let ((last-wp-object nil)
	(last-wp-data nil)
	(actions nil)
	(local-brace nil)
	(object-list objects))
    (setf *cmn-staff-data* nil)
    (setf *cmn-staff* nil)
    (cmn-clear-pipe)
    (loop while object-list do
      (let* ((pobj (pop object-list)))
	(when pobj
	  (let* ((cons-obj (write-protected pobj))
		 (ok-obj (if cons-obj (copy pobj) pobj)))
	    (if (score-object-list-p ok-obj)
		(setf object-list (append (disgorge ok-obj) object-list))
	      (if (or (bracket-p ok-obj) (brace-p ok-obj))
		  (setf local-brace ok-obj)
		(let ((object nil))
		  (if last-wp-object
		      (if (not (notifiable last-wp-object ok-obj))
			  (progn
			    (push (funcall #'notify last-wp-object (nreverse last-wp-data)) *cmn-staff-data*)
			    (setf last-wp-data nil)
			    (setf last-wp-object nil)
			    (if cons-obj 
				(setf last-wp-object ok-obj)
			      (setf object ok-obj)))
			(push ok-obj last-wp-data))
		    (if cons-obj
			(setf last-wp-object ok-obj)
		      (setf object ok-obj)))
		  (when object
		    (if (self-acting-p object)
			(push object actions)
		      (if (or (text-p object) (sundry-p object))
			  (push (make-self-acting :action #'add-to-marks :argument (list object)) actions)
			(push object *cmn-staff-data*)))))))))))
    (if last-wp-object (push (funcall #'notify last-wp-object (nreverse last-wp-data)) *cmn-staff-data*))
    (if (or (line-p (first *cmn-staff-data*)) (page-p (first *cmn-staff-data*))) (pop *cmn-staff-data*))
    (values (nreverse *cmn-staff-data*) (nreverse actions) local-brace)))

(defmethod display ((staff staff-mixin) container score &optional justifying)
  (declare (ignore container))
  (let ((px0 (+ (box-x0 staff) (vis-dx staff) (if (staff-user-data staff) (first (staff-user-data staff)) 0)))
	(py0 (+ (box-y0 staff) (vis-dy staff)))
	(x1 (+ (box-x1 staff) (if (staff-user-data staff) (third (staff-user-data staff)) 0))))
    (when (or px0 py0 x1)
      ;; we are at x y and want the staff to go to x1.
      (if (marks staff) (display-marks staff score justifying))
      (let* ((x-back 0.0)
	     (y-down 0.0)
	     (line-width *staff-line-width*)
	     (lines (or (staff-lines staff) 5))
	     (sls (* 2 *staff-line-separation*))
	     (x0 (+ px0 x-back))
	     (y0 (+ py0 y-down (* sls (staff-start-line staff)))))
	(comment score "staff lines")
	(if (draw-func staff)
	    (funcall (draw-func staff) score staff x0 y0 x1)
	  (progn
	    (setf (line-width score) line-width)
	    (loop for i from 0 below lines do
	      (moveto score x0 y0)
	      (lineto score x1 y0)
	      (incf y0 sls))
	    (draw score)
	    (setf (line-width score) 0)))
	(let* ((ssize (or *cmn-score-size* (scr-size score)))
	       (x1s (* ssize x1))
	       (y1s (* ssize (- y0 sls))))
	  (setf (scr-x score) x1s)
	  (setf (scr-y score) y1s))))))

(defmethod house ((staff staff-mixin) score)
  (declare (ignore score))
  (setf (box-y1 staff) 1.0)
  (setf (center staff) 0.0))



(defclass score-object-list ()
  ((data :initarg :data :initform nil :accessor data)))

(defmethod score-object-list-p ((obj t)) nil)
(defmethod score-object-list-p ((obj score-object-list)) t)

(defmethod disgorge ((objlist score-object-list))
  (prog1
      (data objlist)
    (setf (data objlist) nil)))

(defmethod engorge (objects)
  (make-instance 'score-object-list :data objects))

(defmethod descry ((sob score-object-list) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(score-object-list" 
	    (if (data sob) (format nil "~%~A" prewhitespace)
	      ""))
	  (if (data sob)
	      (format nil " :data (list ~{ ~A~})" (loop for datum in (data sob) collect (descry datum)))
	    "")
	  (if (next-method-p) (call-next-method sob stream (or controller sob)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((sob score-object-list) &optional object)
  (let ((new-sob (if (not object) (make-instance 'score-object-list)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (data new-sob) (loop for datum in (data sob) collect (copy datum)))
    (if (next-method-p) (call-next-method sob new-sob))
    new-sob))

(defmethod display ((score-obj score-object-list) container score &optional justifying)
  (when (not (invisible-p score-obj))
    (let ((sobs (data score-obj)))
      (when (output score)
	(matrix-front score (matrix score-obj))
	(loop for sob in sobs do (display sob container score justifying))
	(matrix-back score)))))

;;; some help in error handing

(defun cmn-tick-pipe (obj)
  (setf cmn-pipe-2 cmn-pipe-1)
  (setf cmn-pipe-1 cmn-pipe-0)
  (setf cmn-pipe-0 obj))

(defun cmn-clear-pipe ()
  (setf cmn-pipe-0 nil)
  (setf cmn-pipe-1 nil)
  (setf cmn-pipe-2 nil))

(defun cmn-pipe-context ()
  (if cmn-pipe-0
      (let ((cmn0 (identify cmn-pipe-0)))
	(format nil " recently evaluated note~P:~A~A~A~A~A~A~%~VT"
		(if cmn-pipe-1 2 1)
		(if cmn-pipe-2 (format nil "~%        ") " ")
		(if cmn-pipe-2 (identify cmn-pipe-2) "")
		(if cmn-pipe-1 (format nil "~%        ") " ")
		(if cmn-pipe-1 (identify cmn-pipe-1) "")
		(if cmn-pipe-1 (format nil "~%        ") " ")
		cmn0
		(length cmn0)))
    ""))

(defun system-context (sys)
  (if sys
      (if (and (systems *cmn-score*) 
	       (> (length (systems *cmn-score*)) 1))
	  (let ((pos (position sys (systems *cmn-score*))))
	    (if pos (format nil "in system ~D " pos)
	      "in an unattached system "))
	"")
    ""))

(defun staff-context (stf)
  (if stf
      (let ((pos (and *cmn-system* 
		      (staves *cmn-system*) 
		      (> (length (staves *cmn-system*)) 1)
		      (position stf (staves *cmn-system*)))))
	(format nil "~A(staff~A~A~A "
		(if pos (format nil "in staff ~D " pos) "")
		(if (staff-size stf) (format nil " (staff-size ~,3F)" (staff-size stf)) "")
		(if (staff-lines stf) (format nil " (staff-lines ~D)" (staff-lines stf)) "")
		(if (staff-name stf) (format nil " (staff-name ~S)" (staff-name stf)) "")))
    ""))

(defun staff-data-start (stf obj owner)
  (let ((pos (and stf obj (staff-data stf)
		  (or (and owner (position owner (staff-data stf)))
		      (position obj (staff-data stf))))))
    (if (and stf (staff-data stf))
	(format nil "~{~A ~}~A"
		(loop for i from 0 below (min (or pos 5) 5) and note in (staff-data stf)  
		 if (and (not (staff-p note)) (not (bar-p note)) (not (eq obj note)) (not (eq owner note)))
		 collect (identify note))
		(if (or (and pos (> pos 8)) 
			(and (not pos) (> (length (staff-data stf)) 8)))
		    " ... " ""))
      "")))

(defun staff-data-continuation (stf obj owner sys stf0)
  (let ((pos (and stf obj (staff-data stf)
		  (or (and owner (position owner (staff-data stf)))
		      (position obj (staff-data stf))
		      (length (staff-data stf))))))
    (if (and pos (> pos 4))
	(let ((stf1 (format nil "~{~A ~}" (loop for n from (max 5 (- pos 3)) below pos
					   collect (identify (nth n (staff-data stf)))))))
	  (format nil "~A~%~VT"		;~V on p582 CLtL
		  stf1 
		  (+ 2 (length sys) (length stf0) (length stf1)))) 
					;used to include (length stf) which doesn't make any sense
					;but neither does the rest of this calculation -- has something fallen out somewhere?
      (cmn-pipe-context))))

(defun owner-context (obj owner)
  (format nil "~A~A~A"
	  (if obj (identify obj))
	  (if owner " in " "")
	  (or owner "")))

(defun cmn-error-1 (format-string &rest args)
  (let* ((sys (system-context *cmn-system*))
	 (stf (staff-context *cmn-staff*))
	 (stf0 (staff-data-start (or *cmn-staff*
				     (and *cmn-staff-data*
					  (make-instance 'staff :data (reverse *cmn-staff-data*))))
				 *cmn-object* *cmn-owning-object*))
	 (stf1 (staff-data-continuation (or *cmn-staff* 
					    (and *cmn-staff-data*
						 (make-instance 'staff :data (reverse *cmn-staff-data*))))
					*cmn-object* *cmn-owning-object*
					sys stf0))
	 (obj (identify *cmn-object*))
	 (owner (if *cmn-owning-object* (identify *cmn-owning-object*) "")))
    (format nil "~A:~%  ~A~A~A~A~A"
	   (if args
	       (apply #'format nil format-string args)
	     format-string)
	   sys stf stf0 stf1 (owner-context obj owner))))

(defun cmn-error (format-string &rest args)
  (error (apply #'cmn-error-1 format-string args)))

(defun cmn-warn (format-string &rest args)
  (warn (apply #'cmn-error-1 format-string args)))

(defmethod notify :before (any-object &optional objects)
  (declare (ignore objects))
  (setf *cmn-object* any-object))

(defmethod display :before (any-object container score &optional justifying)
  (declare (ignore score justifying))
  (setf *cmn-object* any-object)
  (setf *cmn-owning-object* container))

(defmethod copy :before (any-object &optional other-object)
  (setf *cmn-object* any-object)
  (setf *cmn-owning-object* other-object))

(defmethod house :before (any-object score)
  (declare (ignore score))
  (setf *cmn-object* any-object))
