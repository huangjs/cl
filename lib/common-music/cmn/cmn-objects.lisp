;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;

(in-package :cmn)

(defvar *cmn-default-font* (list "Times-Roman" "Times-Italic" "Times-Bold" "Times-BoldItalic"))
;;; this sets all the fallback font names
(defun normal-font () (first *cmn-default-font*))
(defun italic-font () (second *cmn-default-font*))
(defun bold-font () (third *cmn-default-font*))
(defun bold-italic-font () (fourth *cmn-default-font*))
(defvar *cmn-default-font-size* 1.0)


;;; as a convenience for callers of clm, we like to package up all the information about
;;; an entity within the parentheses of that entity -- that is:
;;;    (c4 q staccato (onset 1.5))
;;; which requires that the "onset" function pass its argument on to the "c4" function.
;;; In normal CLOS syntax this would be (setf (onset (c4 q staccato)) 1.5) or something,
;;; and the real entity rapidly gets swallowed up in onion-like layers of setfs with the
;;; associated setf-d data further and further from the field it is setting.  The
;;; functions that act like "onset" above return a "self-acting" instance and the
;;; "self-action" macro wraps up the rest.

(defparameter prewhitespace "                    ")


(defclass self-acting ()
  ((action :initarg :action :initform nil :accessor action)
   (argument :initarg :argument :initform nil :accessor argument)))
  
(defun make-self-acting (&key action argument) (make-instance 'self-acting :action action :argument argument))
(defmethod self-acting-p ((obj self-acting)) t)
(defmethod self-acting-p ((obj t)) nil)

(eval-when (compile load eval)
  (defmacro deferred-action (name)
    `(defmethod ,name ((val t)) (make-self-acting :action #'(lambda (obj val1) (setf (,name obj) val1)) :argument val))))


;;;
;;; ----------------    main methods
;;;

(defgeneric copy (read-object &optional write-object))
(defgeneric descry (object &optional stream controller))
(defgeneric display (object container score &optional justifying))
(defgeneric house (object score))
(defgeneric notify (controller &optional objects))
(defgeneric identify (object))

;;; these are the primary generic functions that every score object responds to
;;; copy returns a copy (unwrite-protected) of its argument (at top level)
;;; descry is our describe method intended for debugging (would be nice if lisp's describe were specializable)
;;; display displays an object
;;; house puts the three layers of white space boxes around an object (during justification)
;;; notify is the syntax checker -- it reads all the arguments collected into an object and regularizes everything
;;; identify is used by the error reporting mechanism to try to give an understandable reference to the current object

(defmethod descry (anything &optional stream controller)
  (declare (ignore controller))
  (format stream " ~A" anything))

(defmethod identify (anything)
  anything)



;;;
;;; ----------------    read-only structures
;;;
;;; our basic score syntax includes such things as (cmn staff treble c4 q) and we have
;;; to make sure we never write the fields of the "staff" or "treble" (etc) variables
;;; so all such special variables are "write-protected".
;;;
;;; This is actually a kludge to get around a PCL bug that (defconstant hi (make-instance...))
;;;   does not return a true constant (i.e. (constantp hi) is NIL) -- in ACL 4.1, and I assume
;;;   any "real" CLOS lisp, the write-protected class is unneeded, but at this point a rewrite
;;;   would be a big undertaking.

(defclass write-protect ()
  ())

;;; using typep as in:
;;;(defun write-protected (object) (typep object 'write-protect))
;;; is about 10 times slower than methods such as:

(defmethod write-protected ((obj t)) nil)
(defmethod write-protected ((obj write-protect)) t)



;;;
;;; ----------------    bounds (the "bounding-box")
;;;
;;; x0 y0 is lower left corner, normally -- some objects slop over one way or the other

(defclass box-mixin ()
  ((x0 :initarg :x0 :initform 0 :reader x0 :reader box-x0 :type number)
   (y0 :initarg :y0 :initform 0 :reader y0 :reader box-y0 :type number)
   (x1 :initarg :x1 :initform 0 :reader x1 :reader box-x1 :type number)
   (y1 :initarg :y1 :initform 0 :reader y1 :reader box-y1 :type number)))

(defclass box (box-mixin)
  ((x0 :accessor x0 :accessor box-x0 :type number)
   (y0 :accessor y0 :accessor box-y0 :type number)
   (x1 :accessor x1 :accessor box-x1 :type number)
   (y1 :accessor y1 :accessor box-y1 :type number)))

(defmethod box-p ((obj t)) nil)
(defmethod box-p ((obj box-mixin)) t)

(defun copy-bounding-box (obj1 obj2)
  (setf (box-x0 obj2) (box-x0 obj1))
  (setf (box-x1 obj2) (box-x1 obj1))
  (setf (box-y0 obj2) (box-y0 obj1))
  (setf (box-y1 obj2) (box-y1 obj1)))

(defmethod copy ((box box-mixin) &optional object)
  (let ((new-box (if (not object) (make-instance 'box)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (box-x0 new-box) (box-x0 box))
    (setf (box-y0 new-box) (box-y0 box))
    (setf (box-x1 new-box) (box-x1 box))
    (setf (box-y1 new-box) (box-y1 box))
    (if (next-method-p) (call-next-method box new-box))
    new-box))

(defmethod descry ((box box-mixin) &optional stream controller)
  (format stream "~A :x0 ~A :y0 ~A :x1 ~A :y1 ~A~A~A"
	  (if (not controller) "(box" "")
	  (not-rational (x0 box))
	  (not-rational (y0 box))
	  (not-rational (x1 box))
	  (not-rational (y1 box))
	  (if (next-method-p) (call-next-method box stream (or controller box)) "")
	  (if (not controller) ")" "")))

(defmacro with-position (obj x0 y0 &body body)
  `(let ((old_x0 (box-x0 ,obj))
	 (old_y0 (box-y0 ,obj)))
     (if (not (numberp ,x0)) (warn "weird argument to with-position: x0: ~A of ~A " ,x0 ,obj))
     (if (write-protected ,obj) (warn "write-protected ~A passed to with-position" ,obj))
     (setf (box-x0 ,obj) ,x0)
     (setf (box-y0 ,obj) ,y0)
     ,@body
     (setf (box-x0 ,obj) old_x0)
     (setf (box-y0 ,obj) old_y0)))


(defmethod width ((box box-mixin))
  (- (box-x1 box) (box-x0 box)))



;;; 
;;; ---------------     patterns and coordinate transformations
;;;

(defclass matrix-mixin ()
  ((matrix :initarg :matrix 
	   :initform nil
	   :reader matrix)))

(defclass matrix (matrix-mixin)
  ((matrix :accessor matrix)))

(defmethod matrix-p ((obj t)) nil)
(defmethod matrix-p ((obj matrix-mixin)) t)
(defun make-matrix () (make-instance 'matrix)) ;backwards compatibility

(defmethod copy ((matrix matrix-mixin) &optional object)
  (let ((new-matrix (if (not object) (make-instance 'matrix)
		      (if (write-protected object) (copy object)
			object))))
    (if (matrix matrix) (setf (matrix new-matrix) (copy-list (matrix matrix))))
    (if (next-method-p) (call-next-method matrix new-matrix))
    new-matrix))

(defun identity-matrix-p (object)
  (let ((mat (matrix object)))
    (or (not mat)
	(and (= (first mat) 1)
	     (= (second mat) 0)
	     (= (third mat) 0)
	     (= (fourth mat) 1)))))

(defun invisible-p (object)
  (let ((mat (matrix object)))
    (and mat
	 (= (first mat) 0)
	 (= (second mat) 0)
	 (= (third mat) 0)
	 (= (fourth mat) 0))))

(defun invisible-matrix (mat)
  (and mat
       (= (first mat) 0)
       (= (second mat) 0)
       (= (third mat) 0)
       (= (fourth mat) 0)))

(defun translate-matrix (score object x0 y0 &optional (scale 1.0))
  (let ((base (if object (copy-list (matrix object)) (list 1 0 0 1 0 0))))
    (if base 
	(progn
	  (setf (first base) (* scale (first base)))
	  (setf (fourth base) (* scale (fourth base)))
	  (setf (fifth base) (* (scr-size score) x0))
	  (setf (sixth base) (* (scr-size score) y0))
	  base)
      (list scale 0 0 scale (* (scr-size score) x0) (* (scr-size score) y0)))))

(defmethod descry ((matrix matrix-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(matrix" 
	    (if (not (identity-matrix-p matrix))
		(format nil "~%~A" prewhitespace)
	      ""))
	  (if (not (identity-matrix-p matrix))
	      (format nil " :matrix '~A"
		      (map 'list #'not-rational (matrix matrix)))
	    "")
	  (if (next-method-p) (call-next-method matrix stream (or controller matrix)) "")
	  (if (not controller) ")" "")))

(defun identify-matrix (object)
  (if (not (identity-matrix-p object))
      (if (invisible-p object)
	  " (scale 0 0)"
	(if (equal (matrix object) '(-1 0 0 1 0 0))
	    " mirrored"
	  (if (let ((mobj (matrix object)))
		(and (zerop (second mobj)) (zerop (third mobj)) (zerop (fifth mobj)) (zerop (sixth mobj))))
	      (format nil " (scale ~,3F ~,3F)" (first (matrix object)) (fourth (matrix object)))
	    (format nil " (matrix '~A)" (map 'list #'not-rational (matrix object))))))
    ""))


(defun rotate (angle)
  (make-self-acting :action #'(lambda (obj angle)
				(setf (matrix obj) (rotate-matrix (or (matrix obj) (list 1 0 0 1 0 0)) angle))
				obj)
		    :argument angle))

(defun scale (xscl yscl)
  (make-self-acting :action #'(lambda (obj scls)
				(setf (matrix obj) (scale-matrix (or (matrix obj) (list 1 0 0 1 0 0)) (first scls) (second scls)))
				obj)
		    :argument (list xscl yscl)))
				
(defun mirror ()
  (make-self-acting :action #'(lambda (obj ignored)
				(declare (ignore ignored))
				(setf (matrix obj) (mirror-matrix (or (matrix obj) (list 1 0 0 1 0 0))))
				obj)
		    :argument nil))

(defvar mirrored 
  (make-self-acting :action #'(lambda (obj ignored)
				(declare (ignore ignored))
				(setf (matrix obj) (mirror-matrix (or (matrix obj) (list 1 0 0 1 0 0))))
				obj)
		    :argument nil))
				   

(defun transform (matr)
  (make-self-acting :action #'(lambda (obj matr)
				(setf (matrix obj) (truncated-matrix-multiply (or (matrix obj) (list 1 0 0 1 0 0)) matr))
				obj)
		    :argument matr))

(defun invisible () 
  (make-self-acting :action #'(lambda (obj ignored) 
				(declare (ignore ignored)) 
				(setf (matrix obj) (list 0 0 0 0 0 0)) obj) 
		    :argument nil))

(defvar invisible 
    (make-self-acting :action #'(lambda (obj ignored) 
				  (declare (ignore ignored))
				  (setf (matrix obj) (list 0 0 0 0 0 0)) obj) 
		      :argument nil))



(defclass pattern-mixin ()
  ((pattern-type :initarg :pattern-type :initform nil :reader pattern-type)
   (pattern-data :initarg :pattern-data :initform nil :reader pattern-data)
   (color :initarg :color :initform nil :reader color)))

(defclass pattern (pattern-mixin)
  ((pattern-type :accessor pattern :accessor pattern-type)
   (pattern-data :accessor pattern-data)
   (color :accessor color)))

(defmethod copy ((pattern pattern-mixin) &optional object)
  (let ((new-pattern (if (not object) (make-instance 'pattern)
		       (if (write-protected object) (copy object)
			 object))))
    (setf (pattern-type new-pattern) (pattern-type pattern))
    (setf (pattern-data new-pattern) (pattern-data pattern))
    (setf (color new-pattern) (color pattern))
    (if (next-method-p) (call-next-method pattern new-pattern))
    new-pattern))

(defmethod descry ((pattern pattern-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A"
	  (if (not controller) "(pattern" 
	    (if (pattern-type pattern) (format nil "~%~A" prewhitespace)
	      ""))
	  (if (pattern-type pattern)
	      (format nil " :type ~A" (pattern-type pattern))
	    "")
	  (if (pattern-data pattern)
	      (format nil " :data ~A" (pattern-data pattern))
	    "")
	  (if (color pattern)
	      (format nil " :color ~A" (color pattern))
	    "")
	  (if (next-method-p) (call-next-method pattern stream (or controller pattern)) "")
	  (if (not controller) ")" "")))

(defun identify-color (obj &optional file)
  (if (color obj)
      (format file " (color '~A)" (color obj))
    ""))
      


(defclass write-protected-marks-mixin ()
  ((marks :initarg :marks :initform nil :reader marks)))

(defclass marks-mixin (write-protected-marks-mixin)
  ((marks :accessor marks)))

(defmethod marks-p ((obj t)) nil)
(defmethod marks-p ((obj marks-mixin)) t)

(defmethod descry ((vis write-protected-marks-mixin) &optional stream controller)
  (format stream "~A~A"
	  (if (marks vis) 
	      (format stream "~%~A :marks (list~{ ~A~})"
		      prewhitespace
		      (loop for mark in (marks vis) collect (descry mark stream nil)))
	    "")
	  (if (next-method-p) (call-next-method vis stream (or controller vis)) "")))

(defmethod copy ((vis write-protected-marks-mixin) &optional object)
  (let ((new-vis (if (not object) (make-instance 'marks-mixin)
		   (if (write-protected object) (copy object)
		     object))))
    (if (marks vis) (setf (marks new-vis) (loop for mark in (marks vis) collect (copy mark))))
    (if (next-method-p) (call-next-method vis new-vis))
    new-vis))

(defun display-marks (object score &optional justifying)
  (if (marks object) 
      (loop for mark in (marks object) do 
	(if (or (not (sundry-p mark))
		(not (eq (sundry-name mark) :slur)))
	    (display mark object score justifying)))))

(defmethod add-to-marks ((anything write-protected-marks-mixin) objects) 
  (setf (marks anything) (append (marks anything) objects)))

(defun edit-mark (anything mark-identifier mark-editor)
  (loop for m in (marks anything) do
    (if (funcall mark-identifier m anything)
	(funcall mark-editor m anything))))

(defun identify-marks (object &optional file)
  (if (marks object)
      (format file "~{ ~A~}" 
	      (loop for mark in (marks object)
	       collect (identify mark)))
    ""))




(defclass visible-mixin (write-protected-marks-mixin box-mixin pattern-mixin matrix-mixin)
  ((justification :initform nil :initarg :justification :reader visible-justification)
   (visible-section :initform nil :initarg :visible-section :reader visible-section)
   (dx :initarg :dx :initform 0 :reader dx :reader vis-dx)
   (dy :initarg :dy :initform 0 :reader dy :reader vis-dy)
   (center :initarg :center :initform 0 :reader center)
   (walls :initarg :walls :initform nil :reader walls)
   (fences :initarg :fences :initform nil :reader fences)
   (expanders :initform nil :initarg :expanders :reader expanders)))

(defclass visible (visible-mixin marks-mixin box pattern matrix)
  ((justification :accessor visible-justification :accessor justification)
   (visible-section :accessor visible-section)
   (dx :accessor dx :accessor vis-dx)
   (dy :accessor dy :accessor vis-dy)
   (center :accessor center)
   (walls :accessor walls)
   (fences :accessor fences)
   (expanders :accessor expanders)))

(defmethod visible-p ((obj t)) nil)
(defmethod visible-p ((obj visible-mixin)) t)

(defmethod descry ((vis visible-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A"
	  (if (visible-justification vis) 
	      (format stream " :justification :~(~A~)" (visible-justification vis))
	    "")
 	  (if (visible-section vis) 
 	      (format stream " :section ~A" (visible-section vis))
 	    "")
	  (if (not (zerop (dx vis))) (format nil " :dx ~,3F" (dx vis)) "")
	  (if (dy vis)
	      (if (listp (dy vis))
		  (format nil " :dy '~A " (dy vis))
		(if (not (zerop (dy vis))) 
		    (format nil " :dy ~,3F" (dy vis)) 
		  ""))
	    "")
	  (if (and (center vis) (not (zerop (center vis)))) (format nil " :center ~A" (center vis)) "")
	  (if (walls vis) (format nil " :walls '~A" (walls vis)) "")
	  (if (fences vis) (format nil " :fences '~A" (fences vis)) "")
	  (if (expanders vis) (format nil " :expanders '~A" (expanders vis)) "")
	  (if (next-method-p) (call-next-method vis stream (or controller vis)) "")))

(defmethod copy ((vis visible-mixin) &optional object)
  (let ((new-vis (if (not object) 
		     (cmn-error "attempt to copy a bare instance of the visible-mixin class")
		   (if (write-protected object) (copy object)
		     object))))
    (setf (visible-justification new-vis) (visible-justification vis) )
    (setf (visible-section new-vis) (visible-section vis))
    (setf (vis-dx new-vis) (vis-dx vis))
    (setf (vis-dy new-vis) (vis-dy vis))
    (setf (center new-vis) (center vis))
    (setf (walls new-vis) (copy-list (walls vis)))
    (setf (fences new-vis) (copy-list (fences vis)))
    (setf (expanders new-vis) (copy-list (expanders vis)))
    (if (next-method-p) (call-next-method vis new-vis))
    new-vis))

(defun identify-visible (object &optional file)
  (if (or (not (zerop (dx object)))
	  (not (zerop (dy object))))
      (format file "~A~A" 
	      (if (not (zerop (dx object))) (format nil " (dx ~,3F)" (dx object)) "")
	      (if (not (zerop (dy object))) (format nil " (dy ~,3F)" (dy object)) ""))
    ""))

(defun layout (val)
  (make-self-acting :argument val :action #'(lambda (obj arg) (setf (visible-justification obj) arg))))


(defclass font-mixin ()
  ((font-size :initarg :font-size :initform nil :accessor font-size)
   (font-name :initarg :font-name :initform nil :accessor font-name)
   (font-scaler :initarg :font-scaler :initform 1.0 :accessor font-scaler)))

(defmethod descry ((fnt font-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A"
	  (if (not controller) "(font" "")
	  (if (font-name fnt) (format nil " :font-name ~A" (font-name fnt)) "")
	  (if (font-size fnt) (format nil " :font-size ~A" (font-size fnt)) "")
	  (if (/= (font-scaler fnt) 1.0) (format nil " :font-scaler ~A" (font-scaler fnt)) "")
	  (if (next-method-p) (call-next-method fnt stream (or controller fnt)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((fnt font-mixin) &optional object)
  (let ((new-fnt (if (not object) (make-instance 'font-mixin)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (font-size new-fnt) (font-size fnt))
    (setf (font-name new-fnt) (font-name fnt))
    (setf (font-scaler new-fnt) (font-scaler fnt))
    (if (next-method-p) (call-next-method fnt new-fnt))
    new-fnt))



(defclass text (font-mixin visible)
  ((letters :initarg :letters :initform nil :accessor letters)
   (y :initarg :y :initform nil :accessor y :accessor text-y)
   (x :initarg :x :initform nil :accessor x :accessor text-x)
   (style :initarg :text-style :initform nil :accessor text-style)
   (font-name :initform (normal-font))
   (justification :initform :none)))

(defmethod text-p ((obj t)) nil)
(defmethod text-p ((obj text)) t)

(defmethod descry ((text text) &optional stream controller)
  (format stream "~A~A~A~A~A"
	  (if (not controller) "(text" "")
	  (if (letters text) (format nil " :letters ~A" (letters text)) "")
	  (if (text-style text) (format nil " :text-style ~A" (text-style text)) "")
	  (if (next-method-p) (call-next-method text stream (or controller text)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((text text) &optional object)
  (let ((new-text (if (not object) (make-instance 'text)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (letters new-text) (letters text))
    (setf (text-style new-text) (text-style text))
    (setf (text-x new-text) (text-x text))
    (setf (text-y new-text) (text-y text))
    (if (next-method-p) (call-next-method text new-text))
    new-text))

(defun glyph (num)
  (make-self-acting :action #'(lambda (obj num)
				(setf (letters obj) (format nil "\\~O" num))
				obj)
		    :argument num))


(defun make-ISO-encoded-version (score font-name new-font-name)
  (g-send score (format nil
		     " /~A findfont
 dup length dict begin
   {1 index /FID ne {def} {pop pop} ifelse} forall
   /Encoding ISOLatin1Encoding def
   currentdict
 end
 /~A exch definefont pop
" 
		     font-name new-font-name)))



(deferred-action center)
(deferred-action fences)
(deferred-action walls)
(deferred-action expanders)
(deferred-action x0)
(deferred-action y0)
(deferred-action x1)
(deferred-action y1)
(deferred-action dx)
(deferred-action dy)
(deferred-action matrix)
(deferred-action pattern)
(deferred-action color)
(deferred-action font-name)
(deferred-action font-size)
(deferred-action font-scaler)

(defun underlined ()
  (make-self-acting
   :action #'(lambda (obj arg)
	       (declare (ignore arg))
	       (setf (text-style obj) :underlined))
   :argument nil))

(defvar underlined (underlined))

(defun gray-scale (&optional (n 0))
  (make-self-acting
   :action #'(lambda (obj arg)
	       (setf (color obj) (list arg arg arg)))
   :argument n))

(defun setf-gray-scale (obj n)
  (setf (color obj) (list n n n)))

(defsetf gray-scale setf-gray-scale)


(defun outlined (&optional (n 0))
  (make-self-acting
   :action #'(lambda (obj n)
	       (setf (pattern-type obj) :outlined)
	       (setf (pattern-data obj) n))
   :argument n))
  

;;;
;;; --------------    basic musical time fields (onset/duration/beat=>odb)
;;; 

(defclass odb-mixin ()
  ((onset :initarg :onset :initform nil :reader onset :reader odb-onset)
   (duration :initarg :duration :initform nil :reader duration :reader odb-duration)
   (beat :initarg :beat :initform nil :reader beat :reader odb-beat)))

(defclass odb (odb-mixin)
  ((onset :accessor onset :accessor odb-onset)
   (duration :accessor duration :accessor odb-duration)
   (beat :accessor beat :accessor odb-beat)))

(defmethod odb-p ((obj t)) nil)
(defmethod odb-p ((obj odb-mixin)) t)

(defmethod descry ((odb odb-mixin) &optional stream controller)
  (format stream "~A~A~A~A~A~A"
	  (if (not controller) "(odb" 
	    (if (or (onset odb) (duration odb) (beat odb))
		(format nil "~%~A" prewhitespace)
	      ""))
	  (if (onset odb) (format nil " :onset ~A" (onset odb)) "")
	  (if (duration odb) (format nil " :duration ~A" (duration odb)) "")
	  (if (beat odb) (format nil " :beat ~A" (beat odb)) "")
	  (if (next-method-p) (call-next-method odb stream (or controller odb)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((odb odb-mixin) &optional object)
  (let ((new-object (if (not object) (make-instance 'odb) 
		      (if (write-protected object) (copy object)
			object))))
    (setf (odb-onset new-object) (odb-onset odb))
    (setf (odb-duration new-object) (odb-duration odb))
    (setf (odb-beat new-object) (odb-beat odb))
    (if (next-method-p) (call-next-method odb new-object))
    new-object))


(defmethod onset (val)
  (make-self-acting
   :argument val
   :action #'(lambda (obj arg)
	       (if (numberp arg)
		   (setf (onset obj) (fratify arg))
		 (setf (onset obj) arg)))))

(defmethod duration ((val number))
  (make-self-acting
   :argument val
   :action #'(lambda (obj arg)
	       (setf (duration obj) (fratify arg)))))

(defmethod beat ((val number))
  (make-self-acting
   :argument val
   :action #'(lambda (obj arg)
	       (setf (beat obj) (fratify arg)))))


(defclass staff-relative-mixin ()
  ((staff-y0 :initarg :staff-y0 :initform 0 :accessor staff-y0)))

(defmethod staff-relative-mixin-p ((obj t)) nil)
(defmethod staff-relative-mixin-p ((obj staff-relative-mixin)) t)

(defmethod descry ((stf-y0 staff-relative-mixin) &optional stream controller)
  (format stream "~A~A~A~A"
	  (if (not controller) "(staff-relative-mixin" "")
	  (format nil " :staff-y0 ~A" (staff-y0 stf-y0))
	  (if (next-method-p) (call-next-method stf-y0 stream (or controller stf-y0)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((stf-y0 staff-relative-mixin) &optional object)
  (let ((new-stf (if (not object) (make-instance 'staff-relative-mixin)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (staff-y0 new-stf) (staff-y0 stf-y0))
    (if (next-method-p) (call-next-method stf-y0 new-stf))
    new-stf))

(defmethod staff-y0 ((box box-mixin)) (box-y0 box))

(deferred-action staff-y0)


(defclass thick-mixin ()
  ((thickness :initarg :thickness :initform nil :reader thickness :reader cmn-thickness)))

(defclass thick (thick-mixin)
  ((thickness :accessor thickness)))

(defmethod descry ((thk thick-mixin) &optional stream controller)
  (format stream "~A~A"
	  (if (thickness thk) (format nil " :thickness ~A" (thickness thk)) "")
	  (if (next-method-p) (call-next-method thk stream (or controller thk)) "")))

(defmethod copy ((thk thick-mixin) &optional object)
  (setf (thickness object) (thickness thk))
  (if (next-method-p) (call-next-method thk object))
  object)

(deferred-action thickness)
    


(defclass breathing-space-mixin ()
  ((breathing-space :initarg :breathing-space :initform 0 :reader breathing-space)))

(defclass breathing-space ()
  ((breathing-space :initarg :breathing-space :initform 0 :accessor breathing-space)))

(defmethod descry ((thk breathing-space-mixin) &optional stream controller)
  (format stream "~A~A"
	  (if (not (zerop (breathing-space thk))) (format nil " :breathing-space ~1,3F" (breathing-space thk)) "")
	  (if (next-method-p) (call-next-method thk stream (or controller thk)) "")))

(defmethod copy ((thk breathing-space-mixin) &optional object)
  (setf (breathing-space object) (breathing-space thk))
  (if (next-method-p) (call-next-method thk object))
  object)

(deferred-action breathing-space)
    


(defclass score-object-mixin (odb-mixin visible-mixin)
  ((draw-func :initarg :draw-func :accessor draw-func :initform nil)
   (width :initarg :width :accessor width :accessor cmn-width :initform 0)))

(defmethod score-object-p ((obj t)) nil)
(defmethod score-object-p ((obj score-object-mixin)) t)

(defclass score-object (score-object-mixin odb visible)
  ())

;;; anything that the score needs to keep track of (during alignment for example) is a score-object
;;; each such object has a bounding box and a draw function

#-excl
(defmethod print-object ((object score-object-mixin) stream)
  (format stream "#<~A~A>" 
	  (class-name (class-of object))
	  (if (odb-onset object) (format nil ": (onset ~A)" (odb-onset object)) "")))

(defmethod copy ((obj score-object-mixin) &optional object)
  (let ((new-obj (if (not object) (make-instance 'score-object)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (draw-func new-obj) (draw-func obj))
    (setf (cmn-width new-obj) (cmn-width obj))
    (if (next-method-p) (call-next-method obj new-obj))
    new-obj))



(defclass sundry-mixin (score-object-mixin thick-mixin)
  ((name :initform nil :initarg :name :reader sundry-name)
   (mark :initform nil :initarg :mark :reader sundry-mark)
   (source :initform nil :initarg :source :reader sundry-source)))

(defclass sundry (sundry-mixin score-object thick)
  ((name :accessor sundry-name)
   (mark :accessor sundry-mark)
   (source :accessor sundry-source)))

(defclass write-protected-sundry (write-protect sundry-mixin) ())

(defmethod sundry-p ((obj t)) nil)
(defmethod sundry-p ((obj sundry-mixin)) t)
(defun make-sundry (&key name mark source) (make-instance 'sundry :name name :mark mark :source source))  ;backwards compatibility

(defmethod display ((sundry sundry-mixin) note score &optional justifying)
  (when (or (not justifying)
	    (not (eq (visible-justification sundry) :none)))
    (funcall (sundry-mark sundry) sundry note score justifying)
    (if (marks sundry) (display-marks sundry score justifying))))

(defmethod descry ((sundry sundry-mixin) &optional stream controller)
  (format stream "~A~A~A"
	  (if (tag-p sundry) "" (format stream "(~(~A~)"(sundry-name sundry)))
	  (if (next-method-p) (call-next-method sundry stream (or controller sundry)) "")
	  (if (tag-p sundry) "" ")")))

(defmethod identify ((sundry sundry-mixin))
  (or (sundry-source sundry)
      (and (sundry-name sundry)
	   (not (member (sundry-name sundry) 
			'(:slur :set-up-slur :beat :beat-subdivision :staff-name :beam-between-staves :stem-tie)))
	   (format nil "(~(~A~))" (sundry-name sundry)))
      ""))

(defmethod copy ((sundry sundry-mixin) &optional object)
  (let ((new-sundry (if (not object) (make-instance 'sundry)
		      (if (write-protected object) (copy object)
			object))))
    (setf (sundry-name new-sundry) (sundry-name sundry))
    (setf (sundry-mark new-sundry) (sundry-mark sundry))
    (setf (sundry-source new-sundry) (sundry-source sundry))
    (if (next-method-p) (call-next-method sundry new-sundry))
    new-sundry))


(defgeneric backpatch (mark) )
(defgeneric backpatch-time (mark obj) )
(defgeneric backpatch-start (mark) )

(defmethod backpatch (anything) (declare (ignore anything)) nil)
(defmethod backpatch ((sundry sundry-mixin)) nil)
(defmethod backpatch-time ((sundry sundry-mixin) obj) (declare (ignore obj)) nil)
(defmethod backpatch-start (anything) (declare (ignore anything)) nil)


(defun direction-from-note (mark our-note) 
  (if (member (visible-justification mark) '(:up :above :down :below))
      (if (member (visible-justification mark) '(:up :above)) :up :down)
    (if (stem-is-up? our-note) :down :up)))

(defun cmn-mark (obj &rest objects)
  (let ((new-mark (copy obj)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mark (argument act))
	  (if (visible-p act)
	      (if (write-protected act)
		  (push (copy act) (marks new-mark))
		(push act (marks new-mark)))))))
    new-mark))

(defun mark (func name &rest objects)
  (let ((new-mark (make-instance 'sundry :name name :mark func)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mark (argument act))
	  (if (visible-p act)
	      (if (write-protected act)
		  (push (copy act) (marks new-mark))
		(push act (marks new-mark)))))))
    new-mark))


(defmethod notify ((sundry sundry-mixin) &optional objects)
  (let ((new-mark (copy sundry)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mark (argument act))
	  (if (visible-p act)
	      (push act (marks new-mark))))))
    new-mark))



;;; 
;;; ----------------    line and page marks
;;;
;;; I originally used the slot names "space" and "type", but both are apparently illegal in cltl2!!

(defclass page-mixin (odb-mixin box-mixin staff-relative-mixin) 
  ((page-space :reader page-space :initarg :space :initform nil)))

(defclass write-protected-page (write-protect page-mixin) ())

(defclass page (page-mixin odb box)
  ((page-space :accessor page-space)))

(defun page-mark () (make-instance 'page))
(defvar page-mark (make-instance 'write-protected-page))

(defmethod page-p ((obj t)) nil)
(defmethod page-p ((obj page-mixin)) t)

(defmethod copy ((page page-mixin) &optional object)
  (let ((new-page (if (not object) (make-instance 'page)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (page-space new-page) (page-space page))
    (if (next-method-p) (call-next-method page new-page))
    new-page))

(defmethod descry ((page page-mixin) &optional stream controller)
  (format stream "(page-mark~A~A)" 
	  (if (page-space page) (format nil " :space ~1,3F" (page-space page)) "")
	  (if (next-method-p) (call-next-method page stream (or controller page)) "")))

(defmethod notify ((page page-mixin) &optional objects)
  (declare (ignore objects))
  (if (write-protected page) 
      (make-instance 'page) 
    page))

(defmethod identify ((page page-mixin))
  "(page-mark)")



(defclass line-mixin (odb-mixin visible-mixin box-mixin staff-relative-mixin)
  ((line-space :reader line-space :initarg :space :initform nil)
   (size :reader size :initarg :size :initform nil)
   (line-type :reader line-type :initarg :type :initform nil)
   (line-staff :reader line-staff :initarg :line-staff :initform 0)))

(defclass write-protected-line (write-protect line-mixin) ())

(defclass line (line-mixin odb visible box)
  ((line-space :accessor line-space)
   (size :accessor size)
   (line-type :accessor line-type)
   (line-staff :accessor line-staff)))

(defun line-mark (&rest args) 
  (let ((new-line (make-instance 'line)))
    (loop for arg in args do
      (if (self-acting-p arg)
	  (funcall (action arg) new-line (argument arg))))
    new-line))

(defvar line-mark (make-instance 'write-protected-line))

(defmethod line-p ((obj t)) nil)
(defmethod line-p ((obj line-mixin)) t)

(defmethod copy ((line line-mixin) &optional object)
  (let ((new-line (if (not object) (make-instance 'line)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (size new-line) (size line))
    (setf (line-space new-line) (line-space line))
    (setf (line-type new-line) (line-type line))
    (setf (line-staff new-line) (line-staff line))
    (if (next-method-p) (call-next-method line new-line))
    new-line))

(defmethod descry ((line line-mixin) &optional stream controller)
  (format stream "(line-mark~A~A~A~A~A~A~A)"
	  (if (listp (vis-dx line)) (format nil " :dx ~A" (dx line)) "")
	  (if (or (listp (vis-dy line)) (not (zerop (vis-dy line)))) (format nil " :dy ~A" (vis-dy line)) "")
	  (if (size line) (format nil " :size ~1,3F" (size line)) "")
	  (if (line-space line) (format nil " :space ~1,3F" (line-space line)) "")
	  (if (line-type line) (format nil " :type :~(~A~)" (line-type line)) "")
	  (if (line-staff line) (format nil " :staff-number ~D" (line-staff line)) "")
	  (if (next-method-p) (call-next-method line stream (or controller line)) "")))

(defmethod notify ((line line-mixin) &optional objects)
  (declare (ignore objects))
  (if (write-protected line) 
      (make-instance 'line) 
    line))

(defmethod identify ((line line-mixin))
  "(line-mark)")			;not actually called (line-marks are implicit in staff data structure)

;;; if (dy num) => set dy to (line-number :dy '((staff-number num))) else (dy (staff-number num)) => same listified
;;; else (dy '((staff num) (staff num))).
;;; (dx '((beat num) ...))


(defun line-break (&rest args) (apply #'line-mark args))
(defvar line-break (make-instance 'write-protected-line))
(defun page-break () (make-instance 'page))
(defvar page-break (make-instance 'write-protected-page))

;;; (cmn treble c4 q c4 q c4 q (line-break (dy -1)) c4 q c4 q c4 q (line-mark (dy -2)) c4 q c4 q c4 q)


;;;
;;; ----------------    section
;;;
;;; a section is a group of systems (staves) with auxiliary local score actions

(defclass section () ((data :accessor data :initform nil :initarg :data)))
(defmethod section-p ((obj t)) nil)
(defmethod section-p ((obj section)) t)

(defun section (&rest args)
  (make-instance 'section :data args))


#|
(cmn (section (redundant-accidentals nil) (staff treble (meter 4 4) cs4 q cs4 q cs4 q cs4 q)) 
     (section (staff treble cs4 q cs4 q cs4 q cs4 q)) )
|#
