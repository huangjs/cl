;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; ringing notes
(in-package :cmn)

(defclass ring-mixin (sundry-mixin) 
  ((direction :reader ring-direction :initform nil :initarg :direction)
   (length :reader ring-length :initform .5 :initarg :length)
   (thicknesss :reader ring-thickness :initform .04 :initarg :thickness)
   (curvature :reader ring-curvature :initform .125 :initarg :curvature)))

(defclass ring (ring-mixin sundry) 
  ((direction :accessor ring-direction)
   (length :accessor ring-length)
   (thicknesss :accessor ring-thickness)
   (curvature :accessor ring-curvature)))

(defclass write-protected-ring (write-protect ring-mixin) ())

(defmethod ring-p ((obj t)) nil)
(defmethod ring-p ((obj ring-mixin)) t)
(defmethod sundry-p ((obj ring-mixin)) t)

(deferred-action ring-direction)
(deferred-action ring-thickness)
(deferred-action ring-length)

(defmethod descry ((r ring-mixin) &optional stream controller)
  (format stream "(~(~A~) ~A~A~A~A~A)"
	  (sundry-name r)
	  (if (ring-direction r) (format nil " :direction :~(~A~)" (ring-direction r)) "")
	  (if (/= (ring-length r) .5) (format nil " :length ~1,3F" (ring-length r)) "")
	  (if (/= (ring-thickness r) .04) (format nil " :thickness ~1,3F" (ring-thickness r)) "")
	  (if (/= (ring-curvature r) .125) (format nil " :curvature ~1,3F" (ring-curvature r)) "")
	  (if (next-method-p) (call-next-method r stream (or controller r)) "")))

(defmethod identify ((r ring-mixin))
  (format nil "(~(~A~) ~A~A~A~A~A)"
	  (sundry-name r)
	  (if (ring-direction r) (format nil " (ring-direction :~(~A~))" (ring-direction r)) "")
	  (if (/= (ring-length r) .5) (format nil " (ring-length ~1,3F)" (ring-length r)) "")
	  (if (/= (ring-thickness r) .04) (format nil " (ring-thickness ~1,3F)" (ring-thickness r)) "")
	  (if (/= (ring-curvature r) .125) (format nil " (ring-curvature ~1,3F)" (ring-curvature r)) "")
	  (the-usual-suspects r)))

(defmethod copy ((r ring-mixin) &optional object)
  (let ((new-r (if (not object) (make-instance 'ring)
		 (if (write-protected object) (copy object)
		   object))))
    (setf (ring-direction new-r) (ring-direction r))
    (setf (ring-length new-r) (ring-length r))
    (setf (ring-thickness new-r) (ring-thickness r))
    (if (next-method-p) (call-next-method r new-r))
    new-r))



(defun filled-bezier (score x0 y0 x1 y1 dx thickness)
  (moveto score x0 y0)
  (curveto score (+ x0 dx) y1 (- x1 dx) y1 x1 y0)
  (curveto score (- x1 dx) (- y1 thickness) (+ x0 dx) (- y1 thickness) x0 y0)
  (fill-in score))



;;; make these methods on note/chord/t

;;; take into account stem-direction, presence of dots, etc (see ties in cmn3.lisp)

(defun dotted-p (note) (and (dots note) (plusp (dots note))))

(defun display-ring- (mark note score &optional justifying)
  (when (and (or (not justifying)
		 (not (eq (visible-justification mark) :none)))
	     (not (invisible-p mark)))
    (if (chord-p note)
	(loop for nt in (chord-data note) do
	  (setf (box-y0 nt) (+ (staff-y0 note) (* *staff-line-separation* (head-line nt))))
	  (if (not (find-if #'ring-p (marks nt)))
	      (display-ring- mark nt score justifying)))
      (let* ((x0 (+ (x0 note) (center note) (vis-dx mark) .2 (if (dotted-p note) .05 0.0)))
	     (x1 (+ x0 (ring-length mark)))
	     (up (or (eq (ring-direction mark) :up)
		     (and (not (ring-direction mark))
			  (stem-is-down note))))
	     (y0 (+ (y0 note) (vis-dy mark) (if up .125 -.125)))
	     (y1 (if up 
		     (+ y0 (ring-curvature mark))
		   (- y0 (ring-curvature mark)))))
	(filled-bezier score x0 y0 x1 y1 (* .3 (ring-length mark)) (ring-thickness mark))))))

(defun display--ring (mark note score &optional justifying)
  (when (and (or (not justifying)
		 (not (eq (visible-justification mark) :none)))
	     (not (invisible-p mark)))
    (if (chord-p note)
	(loop for nt in (chord-data note) do
	  (setf (box-y0 nt) (+ (staff-y0 note) (* *staff-line-separation* (head-line nt))))
	  (if (not (find-if #'ring-p (marks nt)))
	      (display--ring mark nt score justifying)))
      (let* ((x0 (- (x0 note) (vis-dx mark) (ring-length mark) .05 (if (sign note) .2 0.0)))
	     (x1 (+ x0 (ring-length mark)))
	     (up (or (eq (ring-direction mark) :up)
		     (and (not (ring-direction mark))
			  (stem-is-down note))))
	     (y0 (+ (y0 note) (vis-dy mark) (if up .125 -.125)))
	     (y1 (if up 
		     (+ y0 (ring-curvature mark))
		   (- y0 (ring-curvature mark)))))
	(filled-bezier score x0 y0 x1 y1 (* .3 (ring-length mark)) (ring-thickness mark))))))
  
(defun display--ring- (mark note score &optional justifying)
  (if (chord-p note)
      (loop for nt in (chord-data note) do
	(display--ring- mark nt score justifying))
    (progn
      (display-ring- mark note score justifying)
      (display--ring mark note score justifying))))


(defun ur-ring (func name &rest objects)
  (let ((new-mark (make-instance 'ring :name name :mark func)))
    (loop for act in objects do
      (when act
	(if (self-acting-p act)
	    (funcall (action act) new-mark (argument act))
	  (if (or (sundry-p act) (text-p act) (dynamics-p act))
	      (if (write-protected act)
		  (push (copy act) (marks new-mark))
		(push act (marks new-mark)))))))
    new-mark))


(defun ring- (&rest objects) (apply #'ur-ring #'display-ring- :ring- objects))
(defun -ring- (&rest objects) (apply #'ur-ring #'display--ring- :-ring- objects))
(defun -ring (&rest objects) (apply #'ur-ring #'display--ring :-ring objects))

(defvar ring- (make-instance 'write-protected-ring :name :ring- :mark #'display-ring-))
(defvar -ring- (make-instance 'write-protected-ring :name :-ring- :mark #'display--ring-))
(defvar -ring (make-instance 'write-protected-ring :name :-ring :mark #'display--ring))
