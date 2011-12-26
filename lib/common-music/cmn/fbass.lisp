;;; by Christophe Rhodes

(in-package :cmn)

;;; copied from cmn1.lisp (fingering), with a couple of modifications

;;; the interface is (fbass &rest objs), which takes any number of
;;; strings, digits and text objects. The strings are assumed to be of
;;; length one or two, and, when shown, will have #\# or #\s
;;; characters replaced by a sharp-glyph, #\n by a natural and #\f by
;;; flat. The resulting objects are displayed in a column under the note.

;;; this code is very very brittle, and is not even remotely a
;;; complete solution; it will break if you don't use the defaults. It
;;; also breaks (as does fingering) if the stems and beams are too
;;; low, though I've corrected the fingering bug that seems to cause
;;; the vertical space to be too small if the note is below the staff.

(defclass figured-bass-mark (sundry font-mixin)
  ((figures :initarg :figures :initform nil :accessor figures)
   (size :initarg :figures-size :initform 1.0 :accessor figures-size)
   (font-name :initform (normal-font))
   (font-scaler :initform .37)))

(defmethod figured-bass-mark-p ((obj t)) nil)
(defmethod figured-bass-mark-p ((obj figured-bass-mark)) t)

(deferred-action figures-size)
(deferred-action figures)

(defmethod copy ((fb figured-bass-mark) &optional object)
  (let ((new-fb (if (not object) (make-instance 'figured-bass-mark)
		  (if (write-protected object) (copy object)
		    object))))
    (setf (figures new-fb) (figures fb))
    (if (next-method-p) (call-next-method fb new-fb))
    new-fb))

(defmethod descry ((fb figured-bass-mark) &optional stream controller)
  (format stream "~A :figures '~A~A~A"
	  (if (not controller) "(fbass" "")
	  (figures fb)
	  (if (next-method-p) (call-next-method fb stream (or controller fb)) "")
	  (if (not controller) ")" "")))

(defmethod identify ((fb figured-bass-mark))
  (format nil "(fbass~{ ~D~}~A)" (figures fb) (the-usual-suspects fb)))

(defun display-figured-bass (mark note score &optional justifying)
  (let* ((ffs (* (font-scaler mark) (figures-size mark)))
	 (figs (figures mark))
	 (len (length figs))
	 (y-off (+ (box-y0 mark) (vis-dy mark)  
		   (staff-y0 note) 
		   (min (- (+ ffs .125))
			;; this is wrong -- it's not taking account of the font size
			(- (* (- (minimum-line note) 3) *staff-line-separation*) 0.125))))
	 (x-off (+ (box-x0 note) (vis-dx mark) -.1 (center note) (box-x0 mark))))
#-(or gcl sbcl) (loop for num in figs and new-y from y-off by (- ffs) do
	    (if (numberp num) ;	    (not (text-p num))
		(show score (cmn-text :letters (format nil "~D" num)
				      :font-name (font-name mark) :font-scaler ffs)
		      :matrix (translate-matrix score mark (- x-off 0.02) new-y))
	      (if (not (text-p num))
		  ;; I'm going to assume that we have at most two
		  ;; characters, because it's late and I can't get my
		  ;; head round this.
		  (progn
		    (assert (and (string num) (< (length num) 3)))
		    (let ((len (length num)))
		      (loop for x across num and i upfrom 0
			    do (case x
				 ((#\# #\s)
				  (show score sharp
					:matrix (scale-matrix
						 (translate-matrix score sharp
								   (+ x-off
								      (if (= len 1)
									  0.05
									(if (= i 0) 0 0.17)))
								   (+ new-y 0.12))
						 ffs ffs)))
				 ((#\n)
				  (show score natural
					:matrix (scale-matrix
						 (translate-matrix score natural
								   (+ x-off
								      (if (= len 1)
									  0.08
									(if (= i 0) 0 0.17)))
								   (+ new-y 0.12))
						 ffs ffs)))
				 ((#\f)
				  (show score flat
					:matrix (scale-matrix
						 (translate-matrix score natural
								   (+ x-off
								      (if (= len 1)
									  0.08
									(if (= i 0) 0 0.17)))
								   (+ new-y 0.066))
						 ffs ffs)))
				 (t (show score (cmn-text
						 :letters (format nil "~c" x)
						 :font-name (font-name mark)
						 :font-scaler ffs)
					  :matrix (translate-matrix score mark (+ x-off (* (if (= len 1) -0.02 (if (= i 0) -0.15 0.32)) ffs)) new-y)))))))
		(progn
		  (setf (box-x0 num) x-off)
		  (setf (box-y0 num) new-y)
		  (display num mark score justifying)))))
#+(or gcl sbcl) (error "I'm not writing that code twice just to suit some braindead lisp")
   ))

(defun fbass (&rest objects)
  (let ((new-mark (make-instance 'figured-bass-mark :name :fbass :mark #'display-figured-bass))
	(figures nil))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mark (argument act))
	  (if (visible-p act)
	      (push act (marks new-mark))
	    (push act figures)))))
    (if (not (figures new-mark)) (setf (figures new-mark) (reverse figures)))
    new-mark))

;;; (cmn staff treble c4 (fbass "7" "s") e4 (fbass "6" "4" "f"))
