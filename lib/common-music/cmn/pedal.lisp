;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-

(in-package :cmn)

;;; cmn syntax already uses pedal = a single pedal mark.  So, here
;;; we use pedal- -pedal- -pedal for Ped___, ___/\___, and ___|
;;; respectively.  If the pedal symbol has already appeared on the current
;;; line, it is not repeated at the next pedal-.  The other two pedals
;;; are sost.ped-, -sost.ped-, and -sost.ped (the bare word Sost.Ped. is just
;;; text, as is u.c.ped); and u.c.ped-, -u.c.ped-, -u.c.ped.
;;;
;;; The position of the /\ or | is at the left of the current object -- to 
;;; pedal to the end of a note, attach the -pedal to the object on the next
;;; beat, or use the dx message.
;;;
;;; the damper-pedal also takes the optional message half-pedal.
;;; If the variable use-slanted-pedal is true, you get the slanted style of
;;;   pedal marking, rather than the text+line style.
;;;
;;; some examples:
#|
 (cmn (free-expansion-factor 3) staff treble (c4 q pedal-) (d4 q -pedal-) (e4 q -pedal))
 (cmn (free-expansion-factor 3) staff treble (c4 q pedal-) (d4 q -pedal-) (e4 q (-pedal (dx 2.0))))
 (cmn (free-expansion-factor 3) (staff treble (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (g4 q -u.c.ped) (e4 q -pedal -sost.ped)))
 (cmn staff treble c4 q sost.ped)
 (cmn (free-expansion-factor 3) (staff (dy 2) treble 
    (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (line-break (dy -2)) (g4 q -u.c.ped) (e4 q -pedal -sost.ped)))
 (cmn (free-expansion-factor 3) (staff (dy 2) treble 
    (c4 q pedal- u.c.ped-) (d4 q -pedal- sost.ped-) (g4 q -pedal) (a4 q pedal-) 
    (line-break (dy -2)) (g4 q -u.c.ped) (e4 q -pedal -sost.ped)))
 (cmn (free-expansion-factor 3) staff treble (c4 q pedal-) (d4 q -pedal-) line-break 
    (e4 q -pedal-) (d4 q -pedal-) (c4 q -pedal-) line-break  (e4 q) (d4 q -pedal-) (c4 q -pedal))
 (cmn treble (c4 q pedal-) (d4 q -pedal-) (page-break) (d4 q) (e4 q -pedal))
 (cmn treble (c4 q pedal-) (d4 q -pedal-) (page-break) (e4 q -pedal))
|#


(defvar sustain-pedal-text "Sost.Ped")
(defvar sustain-pedal-text-abbreviation "(S)")
(defvar una-corda-pedal-text "U.C.")
(defvar una-corda-pedal-text-abbreviation "(U)")
(defvar pedal-font-name "Times-Italic")
(defvar pedal-font-scaler .5)
(defvar pedal-spacing .1)

(defvar use-slanted-pedal nil)
(defvar use-pedal-off-symbol nil)


(defvar damper-on nil)
(defvar sustain-on nil)
(defvar current-sustain nil)
(defvar current-una-corda nil)


(defclass any-pedal (connected-text)
  ((continued :initform nil :initarg :continued :accessor continued)
   (initial :initform nil :accessor initial :initarg :initial)))

(defclass damper-pedal (any-pedal)
  ((stack :initform nil :initarg :stack :accessor stack :allocation :class)
   (half-pedal :initform nil :initarg :half-pedal :accessor half-pedal)
   (type :initform :pedal)))

(defmethod damper-pedal-p ((obj t)) nil)
(defmethod damper-pedal-p ((obj damper-pedal)) t)

(defvar half-pedal (make-self-acting :action #'(lambda (ped &rest rest)
						 (declare (ignore rest))
						 (if (not (damper-pedal-p ped)) (error "half-pedal what?!?"))
						 (setf (half-pedal ped) t))
				     :argument nil))


(defclass sustain-pedal (any-pedal)
  ((stack :initform nil :initarg :stack :accessor stack :allocation :class)
   (damper :initform nil :initarg :damper :accessor damper)
   (type :initform :sost.ped)))

(defmethod sustain-pedal-p ((obj t)) nil)
(defmethod sustain-pedal-p ((obj sustain-pedal)) t)


(defclass una-corda-pedal (any-pedal)
  ((stack :initform nil :initarg :stack :accessor stack :allocation :class)
   (damper :initform nil :initarg :damper :accessor damper)
   (sustain :initform nil :initarg :sustain :accessor sustain)
   (type :initform :u.c.ped)))

(defmethod una-corda-pedal-p ((obj t)) nil)
(defmethod una-corda-pedal-p ((obj una-corda-pedal)) t)



;;; fill in the usual copy (and  maybe descry) methods

(defmethod copy ((ped any-pedal) &optional new-ped)
  (setf (initial new-ped) (initial ped))
  (setf (continued new-ped) (continued ped))
  (if (next-method-p) (call-next-method ped new-ped))
  new-ped)

(defmethod copy ((ped damper-pedal) &optional object)
  (let ((new-ped (if (not object) (make-instance 'damper-pedal)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (half-pedal new-ped) (half-pedal ped))
    (if (next-method-p) (call-next-method ped new-ped))
    new-ped))

(defmethod copy ((ped sustain-pedal) &optional object)
  (let ((new-ped (if (not object) (make-instance 'sustain-pedal)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (damper new-ped) (damper ped))
    (if (next-method-p) (call-next-method ped new-ped))
    new-ped))

(defmethod copy ((ped una-corda-pedal) &optional object)
  (let ((new-ped (if (not object) (make-instance 'una-corda-pedal)
		   (if (write-protected object) (copy object)
		     object))))
    (setf (damper new-ped) (damper ped))
    (setf (sustain new-ped) (sustain ped))
    (if (next-method-p) (call-next-method ped new-ped))
    new-ped))


(defmethod descry ((ped any-pedal) &optional stream controller)
  (format stream "~A~A~A~A~A"
	  (if (not controller) (format nil "(~(~A~)" (tag-type ped)) "")
	  (if (continued ped) (format nil " :continued ~(~A~)" (continued ped)) "")
	  (if (initial ped) (format nil " :initial ~(~A~)" (initial ped)) "")
	  (if (next-method-p) (call-next-method ped stream (or controller ped)) "")
	  (if (not controller) ")" "")))

(defmethod descry ((ped damper-pedal) &optional stream controller)
  (format stream "(~(~A~)~A~A)"
	  (tag-type ped)
	  (if (half-pedal ped) " :half-pedal t" "")
	  (if (next-method-p) (call-next-method ped stream (or controller ped)) "")))

(defmethod descry ((ped sustain-pedal) &optional stream controller)
  (format stream "(~(~A~)~A~A)"
	  (tag-type ped)
	  (if (damper ped) " :damper t" "")
	  (if (next-method-p) (call-next-method ped stream (or controller ped)) "")))

(defmethod descry ((ped una-corda-pedal) &optional stream controller)
  (format stream "(~(~A~)~A~A~A)"
	  (tag-type ped)
	  (if (damper ped) " :damper t" "")
	  (if (sustain ped) " :sustain t" "")
	  (if (next-method-p) (call-next-method ped stream (or controller ped)) "")))



;;; for cmn-store's benefit, we need to supply some plausible identify method

(defmethod identify ((ped any-pedal))
  (if (initial ped)
      (let ((pedal-tag-name (new-cmn-store-tag "pedal-"))
	    (more-to-come (cdr (context-data ped))))
	(loop for tag in (context-data ped) do
	  (pop more-to-come)
	  (add-to-cmn-store-tags (context-note tag)
				 (format nil "(-~(~A~)~A ~A~A)" 
					 (context-type tag)
					 (if (or more-to-come (continued tag)) "-" "")
					 pedal-tag-name
					 (identify-box tag))))
	(format nil "(setf ~A (~(~A~)- ~A~A))" 
		pedal-tag-name 
		(context-type ped) 
		(identify-box ped)
		(the-usual-suspects ped)))
    ""))



(defun basic-pedal (new-tag &rest objects)
  (let ((old-tag nil))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (let* ((args (argument act))
		   (first-arg (and args (if (listp args) (first args) args))))
	      (if (and first-arg
		       (connected-text-p first-arg))
		  (setf old-tag first-arg)
		(funcall (action act) new-tag args))))))
    old-tag))

(defun basic-pedal- (new-tag &rest objects)
  (apply #'basic-pedal new-tag objects)
  (push new-tag (stack new-tag))
  (make-self-acting 
   :action #'(lambda (new-note ct)
	       (setf (context-note ct) new-note)
	       (add-to-marks new-note (list ct)))
   :argument new-tag))

(defun -basic-pedal- (new-tag &rest objects)
  (let ((old-tag (or (apply #'basic-pedal new-tag objects)
		     (if (stack new-tag)
			 (first (stack new-tag))
		       (cmn-warn (format nil "-~(~A~)- called, but no corresponding ~(~A~)- to connect to."
					 (tag-type new-tag) (tag-type new-tag)))))))
    (make-self-acting
     :action #'(lambda (new-note lct)
		 (let ((ct (first lct))
		       (ot (second lct)))
		   (setf (context-note ct) new-note)
		   (add-to-marks new-note (list ct))
		   (push ct (context-data ot))
		   nil))
     :argument (list new-tag old-tag))))

(defun -basic-pedal (new-tag &rest objects)
  (let ((old-tag (or (apply #'basic-pedal new-tag objects)
		     (if (stack new-tag)
			 (first (stack new-tag))
		       (cmn-warn (format nil "-~(~A~) called, but no corresponding ~(~A~)- to connect to."
					 (tag-type new-tag) (tag-type new-tag)))))))
    (make-self-acting
     :action #'(lambda (new-note lct)
		 (let ((ct (first lct))
		       (ot (second lct)))
		   (setf (context-note ct) new-note)
		   (push ct (context-data ot))
		   (add-to-marks new-note (list ct))
		   (setf (context-data ot) (nreverse (context-data ot)))
		   (pop (stack new-tag))
		   nil))
     :argument (list new-tag old-tag))))

(defmethod damper-pedal-on ((ped any-pedal) peds) (declare (ignore peds)) nil)
(defmethod sustain-pedal-on ((ped any-pedal) peds) (declare (ignore peds)) nil)



;;; -------- damper pedal

(defun ur-pedal- (&rest objects)
  (setf damper-on t)
  (if current-sustain (setf (damper current-sustain) t))
  (if current-una-corda (setf (damper current-una-corda) t))
  (apply #'basic-pedal- (make-instance 'damper-pedal :initial t) objects))

(defun -ur-pedal- (&rest objects)
  (apply #'-basic-pedal- (make-instance 'damper-pedal) objects))

(defun -ur-pedal (&rest objects)
  (setf damper-on nil)
  (apply #'-basic-pedal (make-instance 'damper-pedal) objects))


(defun pedal- (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'ur-pedal- args)))
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar pedal- (pedal-))

(defun -pedal- (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'-ur-pedal- args)))
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar -pedal- (-pedal-))

(defun -pedal (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'-ur-pedal args)))
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar -pedal (-pedal))

(defmethod justify-pedal ((ped damper-pedal) note score) (moveto score (x0 note) (- (+ (staff-y0 note) (dy ped)) 1.0)))



;;; -------- sustain-pedal

(defun ur-sost.ped- (&rest objects)
  (setf sustain-on t)
  (if current-una-corda (setf (sustain current-una-corda) t))
  (apply #'basic-pedal- (setf current-sustain (make-instance 'sustain-pedal :initial t :damper damper-on)) objects))

(defun -ur-sost.ped- (&rest objects)
  (apply #'-basic-pedal- (make-instance 'sustain-pedal :damper damper-on) objects))

(defun -ur-sost.ped (&rest objects)
  (setf sustain-on nil)
  (setf current-sustain nil)
  (apply #'-basic-pedal (make-instance 'sustain-pedal :damper damper-on) objects))


(defun sost.ped- (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'ur-sost.ped- args))) 
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar sost.ped- (sost.ped-))

(defun -sost.ped- (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'-ur-sost.ped- args))) 
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar -sost.ped- (-sost.ped-))

(defun -sost.ped (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'-ur-sost.ped args))) 
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar -sost.ped (-sost.ped))

(defmethod justify-pedal ((ped sustain-pedal) note score) (moveto score (x0 note) (- (+ (staff-y0 note) (dy ped)) 1.8)))
(defmethod damper-pedal-on ((ped sustain-pedal) peds) (find-if #'damper peds))


;;; and support for the stand-alone word Sost.Ped.

(defun display-sost.ped (mark note score &optional justifying)
  (display (text sustain-pedal-text (font-name pedal-font-name) (font-scaler pedal-font-scaler)
			 (x0 (- (+ (box-x0 note) (vis-dx note) (vis-dx mark)) 1.0))
			 (y0 (- (+ (staff-y0 note) (vis-dy mark)) 1.0)))
	 nil score justifying))

(defvar sost.ped (make-instance 'write-protected-sundry :name :sost.ped :mark #'display-sost.ped))
(defun sost.ped (&rest objects) (apply #'mark #'display-sost.ped :sost.ped objects))



;;; -------- una corda pedal

(defun ur-u.c.ped- (&rest objects)
  (apply #'basic-pedal- (setf current-una-corda (make-instance 'una-corda-pedal :initial t :damper damper-on :sustain sustain-on)) objects))

(defun -ur-u.c.ped- (&rest objects)
  ;; this function is kinda useless, but ...
  (apply #'-basic-pedal- (make-instance 'una-corda-pedal :damper damper-on :sustain sustain-on) objects))

(defun -ur-u.c.ped (&rest objects)
  (setf current-una-corda nil)
  (apply #'-basic-pedal (make-instance 'una-corda-pedal :damper damper-on :sustain sustain-on) objects))


(defun u.c.ped- (&rest args) 
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'ur-u.c.ped- args))) 
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar u.c.ped- (u.c.ped-))

(defun -u.c.ped- (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'-ur-u.c.ped- args))) 
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar -u.c.ped- (-u.c.ped-))

(defun -u.c.ped (&rest args)
  (make-self-acting :action #'(lambda (new-note ped) 
				(declare (ignore ped))
				(let ((sa (apply #'-ur-u.c.ped args))) 
				  (funcall (action sa) new-note (argument sa)))) 
		    :argument nil))

(defvar -u.c.ped (-u.c.ped))

(defmethod justify-pedal ((ped una-corda-pedal) note score) 
  (moveto score 
	  (x0 note) 
	  (- (+ (staff-y0 note) (dy ped)) (+ 1.0 (* 2 (+ pedal-spacing pedal-font-scaler))))))
(defmethod damper-pedal-on ((ped una-corda-pedal) peds) (find-if #'damper peds))
(defmethod sustain-pedal-on ((ped una-corda-pedal) peds) (find-if #'sustain peds))


;;; and support for the stand-alone word u.c.ped

(defun display-u.c.ped (mark note score &optional justifying)
  (display (text "una corda" (font-name pedal-font-name) (font-scaler pedal-font-scaler)
			 (x0 (- (+ (box-x0 note) (vis-dx note) (vis-dx mark)) 1.0))
			 (y0 (- (+ (staff-y0 note) (vis-dy mark)) 1.0)))
	 nil score justifying))

(defvar u.c.ped (make-instance 'write-protected-sundry :name :u.c.ped :mark #'display-u.c.ped))
(defun u.c.ped (&rest objects) (apply #'mark #'display-u.c.ped :u.c.ped objects))



;;; display methods for all pedals

(defvar last-damper -1.0)
(defvar last-sustain -1.0)
(defvar last-una-corda -1.0)

(defmethod cmn-initialize :before (objects) 
  (declare (ignore objects))
  (setf last-damper -1.0)
  (setf last-sustain -1.0)
  (setf last-una-corda -1.0))

(defmethod remember-pedal ((ped damper-pedal) y0) (setf last-damper y0))
(defmethod remember-pedal ((ped sustain-pedal) y0) (setf last-sustain y0))
(defmethod remember-pedal ((ped una-corda-pedal) y0) (setf last-una-corda y0))



(defun display-one-half (score x-off y-off)
  (show score (text "1" (font-name "Times-Roman") (font-scaler .25)) :matrix (translate-matrix score nil x-off y-off))
  (show score (text "2" (font-name "Times-Roman") (font-scaler .25)) :matrix (translate-matrix score nil x-off (- y-off .275)))
  (moveto score (- x-off .05) (- y-off .05))
  (rlineto score .25 0)
  (draw score))

(defun pedal-restart (score)
  (if use-slanted-pedal (rmoveto score .125 0))
  (rlineto score 0 .375)
  (rmoveto score 0 -.375))

(defmethod display-pedal-sign ((ped damper-pedal) score all-lefts dx-mark min-y0 y0)
  ;; according to Kurt Stone in "Music Notation in the Twentieth Century", the "P" of "Ped." is the starting point
  (when (not (continued ped))		; left side of continuation line -> no ped sign at start
    (if (and (not use-pedal-off-symbol)
	     (or (= y0 last-damper)
		 use-slanted-pedal))
	(pedal-restart score)
      (progn
	(draw score)
	(matrix-front score (list 1 0 0 1 (scr-x score) (scr-y score)))
	(draw-ped score)
	(matrix-back score)
	(moveto score (+ (first all-lefts) dx-mark .875) min-y0)))))

(defmethod display-pedal-sign ((ped sustain-pedal) score all-lefts dx-mark min-y0 y0)
  (let ((sym (if (continued ped) sustain-pedal-text-abbreviation sustain-pedal-text))
	(dist (if (continued ped) .675 1.875)))
    (if (= y0 last-sustain)
	(pedal-restart score)
      (progn
	(draw score)
	(show score (text sym (font-name pedal-font-name) (font-scaler pedal-font-scaler))
	      :matrix (list 1 0 0 1 (scr-x score) (scr-y score)))
	(moveto score (+ (first all-lefts) dx-mark dist) min-y0)))))

(defmethod display-pedal-sign ((ped una-corda-pedal) score all-lefts dx-mark min-y0 y0)
  (let ((sym (if (continued ped) una-corda-pedal-text-abbreviation una-corda-pedal-text))
	(dist (if (continued ped) 1 .875)))
    (if (= y0 last-una-corda)
	(pedal-restart score)
      (progn
	(draw score)
	(show score (text sym (font-name pedal-font-name) (font-scaler pedal-font-scaler))
	      :matrix (list 1 0 0 1 (scr-x score) (scr-y score)))
	(moveto score (+ (first all-lefts) dx-mark dist) min-y0)))))


(defun display-any-pedal (ped all-peds all-notes note score &optional justifying)
  (let* ((len (length all-notes))
	 (all-lefts (loop for note in all-notes collect (x0 note)))
	 (dx-mark (vis-dx ped))
	 (min-y0 (+ (vis-dy ped) (staff-y0 note) 
		    -1.0
		    (if (damper-pedal-on ped all-peds) (- (+ pedal-spacing pedal-font-scaler)) 0.0)
		    (if (sustain-pedal-on ped all-peds) (- (+ pedal-spacing pedal-font-scaler)) 0.0))))
    (comment score "pedal mark")
    (if (marks ped) (display-marks ped score justifying))
    (if (eq (initial ped) :quit-across-page)
	(progn
	  (moveto score (+ (first all-lefts) dx-mark -.5) min-y0)
	  (push (x0 note) all-lefts)
	  (push ped all-peds))
	(progn
	  (moveto score (+ (first all-lefts) dx-mark -.125) min-y0)
	  (when (eq (continued ped) :sorta)
	    ;; should this pedal off-on wedge go at the start of the line or the end of the previous?
	    (rmoveto score -.5 0)
	    (rlineto score .5 0)
	    (rlineto score .125 .375)
	    (rlineto score .125 -.375))
	  (display-pedal-sign ped score all-lefts dx-mark min-y0 (staff-y0 note))
	  (remember-pedal ped (staff-y0 note))))
    
    (loop for ur-x0 in (cdr all-lefts) and lped in (cdr all-peds) and i from 2 do
      (let ((x0 (+ ur-x0 (vis-dx lped))))
	(if (and (damper-pedal-p ped)
		 use-slanted-pedal)
	    (lineto score (- x0 .125) (+ min-y0 .375))
	  (lineto score (- x0 .125) min-y0))
	(when (not (continued lped))
	  (if (= i len)
	      (if (damper-pedal-p ped)
		  (if use-slanted-pedal
		      (rlineto score 0 -.375)
		    (if use-pedal-off-symbol
			(progn
			  (draw score)
			  (matrix-front score (list 1 0 0 1 (scr-x score) (scr-y score)))
			  (draw-pedal-off score)
			  (matrix-back score)
			  (moveto score (scr-x score) (scr-y score)))
		      (rlineto score 0 .375)))
		(rlineto score 0 .375))
	    (if (and (damper-pedal-p ped) (half-pedal lped)) 
		(progn
		  (lineto score (- x0 .06) min-y0)
		  (lineto score x0 (+ min-y0 .2))
		  (lineto score (+ x0 .06) min-y0)
		  (draw score)
		  (display-one-half score (- x0 .25) (+ min-y0 .4))
		  (moveto score (+ x0 .06) min-y0))
	      (progn
		(lineto score x0 (+ min-y0 .375))
		(lineto score (+ x0 .125) min-y0)))))))
    (draw score)))

(defmethod display ((ped any-pedal) note score &optional justifying)
  (when (initial ped)
    (if (not justifying)
	(let* ((all-peds (append (list ped) (context-data ped)))
	       (all-notes (loop for pedal in all-peds collect (context-note pedal))))
	  (if (apply #'= (loop for note in all-notes collect (staff-y0 note)))
	      (display-any-pedal ped all-peds all-notes note score justifying)
	    (let* ((first-note (first all-notes))
		   (first-onset (onset first-note))
		   (our-staff *cmn-staff*)
		   (staff-x0 (x0 our-staff))
		   (staff-x1 (x1 our-staff))
		   (all-data (staff-data our-staff)))
	      (loop while (and all-data (not (eq (first all-data) first-note))) do (pop all-data))
	      (if (null all-data) (cmn-error "aw good grief")
		(let* ((ped-group nil)
		       (cur-ped ped)
		       (across-page nil)
		       (cur-staff-y0 (staff-y0 first-note)))
		  (pop all-peds)

		  (loop while (and cur-ped (not across-page)) do
		    (if (/= (onset (first all-data)) first-onset)
			(let ((left-ped (copy ped)))
			  (setf (continued left-ped) t)
			  (setf (context-note left-ped) (short-note (context-note cur-ped) :x0 (+ staff-x0 .5)))
			  (setf (staff-y0 (context-note left-ped)) cur-staff-y0)
			  (push left-ped ped-group))
		      (if (/= first-onset (onset first-note))
			  (setf (continued cur-ped) :sorta)))
		    (loop while (and cur-ped (= cur-staff-y0 (staff-y0 (context-note cur-ped)))) do
		      (push cur-ped ped-group)
		      (loop while (not (eq (context-note cur-ped) (first all-data))) do (pop all-data))
		      (setf cur-ped (pop all-peds)))
		    (if cur-ped
			(let ((right-ped (copy ped)))
			  (setf (context-note right-ped) (short-note (context-note (first ped-group)) :x0 (- staff-x1 .25)))
			  (push right-ped ped-group)
			  (loop while (and all-data (not (staff-p (first all-data)))) do
			    (if (page-p (first all-data)) (setf across-page t))
			    (pop all-data))
			  (loop while (and all-data (not (audible-p (first all-data)))) do (pop all-data))
			  (setf first-onset (onset (context-note cur-ped)))
			  (if (and (not all-peds)
				   (= (onset (first all-data)) first-onset))
			      (progn
				(setf (continued right-ped) nil)
				(setf cur-ped nil))
			    (setf (continued right-ped) t))))
		    (setf ped-group (nreverse ped-group))
		    (display-any-pedal 
			   (first ped-group) 
			   ped-group 
			   (loop for ped in ped-group collect (context-note ped)) 
			   (context-note (first ped-group))
			   score justifying)
		    (loop while (and all-data (not (audible-p (first all-data)))) do (pop all-data))
		    (setf ped-group nil)
		    (if across-page
			(progn
			  (if cur-ped
			      (progn
				(push cur-ped all-peds)
				(if (/= (onset (first all-data)) (onset (context-note (first all-peds))))
				    (let ((left-ped (copy (first all-peds))))
				      (setf (continued left-ped) t)
				      (setf (initial left-ped) :across-page)
				      (setf (context-note left-ped) (first all-data))
				      (add-to-marks (first all-data) (list left-ped))
				      (setf (context-data left-ped) (push left-ped all-peds)))
				  (setf (initial (first all-peds)) :across-page)))
			    (setf (initial (first (last (context-data ped)))) :quit-across-page))))

		    (setf cur-staff-y0 (staff-y0 (first all-data)))))))))
      (if (not (eq (visible-justification ped) :none))
	  (justify-pedal ped note score)))))

