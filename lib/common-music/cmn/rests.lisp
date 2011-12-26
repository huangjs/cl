;;; -*- syntax: common-lisp; package: cmn; base: 10; mode: lisp -*-
;;;
;;; various special-case rests: multi-measure rests, slanted measure rests, abbreviated rest names

(in-package :cmn)


;;; ------------------------------------------------
;;;   multi-measure rest symbols occasionally used in orchestral parts
;;;
;;; (wrest used below is cmn's internal name for the rest class -- the name "rest" caused
;;; no end of strange lisp problems)


(defclass multirest (wrest)
  ((measures :initarg :measures :initform 0 :accessor measures)))

(defclass write-protected-multirest (write-protect multirest) () )

(defmacro define-multirest (name ms)
  `(progn
     (defvar ,name (make-instance 'write-protected-multirest :mark :measurerest :dots 0 :flags 0 :quarters nil :duration nil :measures ,ms))
     (defun ,name (&rest objects) 
       (apply #'ur-rest (make-instance 'multirest :mark :measurerest :dots 0 :flags 0 :quarters nil :duration nil :measures ,ms) objects))))

(define-multirest two-measure-rest 2)
(define-multirest three-measure-rest 3)
(define-multirest four-measure-rest 4)
(define-multirest five-measure-rest 5)
(define-multirest six-measure-rest 6)
(define-multirest seven-measure-rest 7)
(define-multirest eight-measure-rest 8)

(defmethod display ((rest multirest) container score &optional justifying)
  (declare (ignore container))
  (let* ((x0 (- (+ (* .5 (+ (box-x0 rest) (box-x1 rest))) (vis-dx rest)) (center rest)))
	 (y0 (+ (staff-y0 rest) (vis-dy rest) (* 6 *staff-line-separation*)))
	 (ms (measures rest)))
    (setf (box-y0 rest) (+ (staff-y0 rest) (* 6 *staff-line-separation*)))
    (if (and (beams rest) 
	     (not justifying))
	(display (beams rest) rest score))
    (if (marks rest) (display-marks rest score justifying))
    (when (not (invisible-p rest))
      (moveto score (+ x0 .25) y0)
      (if (and (/= ms 2) (/= ms 4))
	  (rmoveto score -.25 0))
      (setf (line-width score) .4)
      (if (or (= ms 2) (= ms 3))
	  (rlineto score 0 -.25)
	(rlineto score 0 -.5))
      (if (/= ms 7) 
	  (if (= ms 3)
	      (rmoveto score 1 .25)
	    (rmoveto score 1 .5))
	(rmoveto score .75 .5))
      (if (or (= ms 3) (= ms 5))
	  (rlineto score 0 -.125)
	(if (or (= ms 6) (= ms 7))
	    (rlineto score 0 -.25)
	  (if (= ms 8)
	      (rlineto score 0 -.5))))
      (if (= ms 7)
	  (progn
	    (rmoveto score .75 .25)
	    (rlineto score 0 -.125)))
      (draw score)
      (setf (line-width score) 0))))
	    
(defmethod house ((rest multirest) score)
  (declare (ignore score))
  (setf (box-x1 rest) (+ .5 (if (> (measures rest) 2) 1.5 0) (if (= (measures rest) 7) .5 0)))
  ;; for justification -- will be set later to reflect the current measure boundaries
  (setf (center rest) (* .5 (box-x1 rest)))
  (setf (walls rest) rest-walls)
  (setf (fences rest) rest-fences)
  (if (not (expanders rest))
      (setf (expanders rest) rest-expanders)))

(defmethod copy ((old-rest multirest) &optional object)
  (let ((new-rest (if (not object) (make-instance 'multirest)
		    (if (write-protected object) (copy object)
		      object))))
    (setf (measures new-rest) (measures old-rest))
    (if (next-method-p) (call-next-method old-rest new-rest))
    new-rest))

(defmethod notify ((rest multirest) &optional objects)
  (apply #'ur-rest rest objects))

#|
(cmn staff treble c4 q bar two-measure-rest bar three-measure-rest bar 
     four-measure-rest bar five-measure-rest bar six-measure-rest bar seven-measure-rest bar eight-measure-rest)
|#

;;; if we were going to use these extensively, we'd want descry and identify methods as well


;;; ------------------------------------------------
;;; slanted measure rest
;;;

(defclass slanted-wrest (wrest) () )
(defclass write-protected-slanted-wrest (write-protect slanted-wrest) () )

(defvar slanted-measure-rest (make-instance 'write-protected-slanted-wrest :mark :measurerest :dots 0 :flags 0 :quarters nil :duration nil))
(defun slanted-measure-rest (&rest objects) 
  (apply #'ur-rest (make-instance 'slanted-wrest :mark :measurerest :dots 0 :flags 0 :quarters nil :duration nil) objects))

(defmethod display ((rest slanted-wrest) container score &optional justifying)
  (declare (ignore container))
  (let* ((x0 (- (+ (* .5 (+ (box-x0 rest) (box-x1 rest))) (vis-dx rest)) (center rest)))
	 (line 4)
	 (y0 (+ (staff-y0 rest) (vis-dy rest) (* line *staff-line-separation*))))
    (setf  (box-y0 rest) (+ (staff-y0 rest) (* line *staff-line-separation*)))
    (if (and (beams rest) 
	     (not justifying))
	(display (beams rest) rest score))
    (if (marks rest) (display-marks rest score justifying))
    (when (not (invisible-p rest))
      (let* ((msx (- (box-x1 rest) (box-x0 rest)))
	     (mdx (max 0.0 (min .5 (/ (- msx 1.0) 4))))
	     (rx0 (- x0 mdx))
	     (rx1 (+ x0 .5 mdx)))
	(moveto score rx0 y0)
	(rlineto score 0 -.5)
	(moveto score rx1 (+ y0 .5))
	(rlineto score 0 -.5)
	(draw score)
	(moveto score rx0 (- y0 .125))
	(lineto score rx1 (+ y0 .375))
	(lineto score rx1 (+ y0 .125))
	(lineto score rx0 (- y0 .375))
	(lineto score rx0 (- y0 .125))
	(fill-in score))

      (let ((mn (and (store-data rest) 
		     (find-if #'(lambda (n) (and (listp n) (eq (first n) :measure-number))) (store-data rest)))))
	(when mn
	  (let ((num (second mn)))
	    (show score (make-instance 'meter)
		  :data (format nil "~D" num)
		  :matrix (translate-matrix score rest 
					    (- x0 (if (> num 10) (if (> num 100) .3 .15) 0)) 
					    (+ (staff-y0 rest) (vis-dy rest) 1.45)))))))))

(defmethod copy ((old-rest slanted-wrest) &optional object)
  (let ((new-rest (if (not object) (make-instance 'slanted-wrest)
		    (if (write-protected object) (copy object)
		      object))))
    (if (next-method-p) (call-next-method old-rest new-rest))
    new-rest))

(defmethod notify ((rest slanted-wrest) &optional objects)
  (apply #'ur-rest rest objects))



;;; ------------------------------------------------
;;; abbreviated rest names

;;;               mark           dots flags quarters
(define-rest r1   :rest1     0 0 nil   #'draw-whole-rest whole-rest-bounds)
(define-rest r2   :rest2    0 0 2      #'draw-half-rest half-rest-bounds)
(define-rest r2.  :rest2    1 0 3      #'draw-half-rest half-rest-bounds)
(define-rest r4   :rest4    0 0 1      #'draw-quarter-rest quarter-rest-bounds)
(define-rest r4.  :rest4    1 0 3/2    #'draw-quarter-rest quarter-rest-bounds)
(define-rest r8   :rest8    0 1 1/2    #'draw-8th-rest rest8-bounds)
(define-rest r8.  :rest8    1 1 3/4    #'draw-8th-rest rest8-bounds)
(define-rest r16  :rest16   0 2 1/4    #'draw-16th-rest rest16-bounds)
(define-rest r16. :rest16   1 2 3/8    #'draw-16th-rest rest16-bounds)
(define-rest r32  :rest32   0 3 1/8    #'draw-32nd-rest rest32-bounds)
(define-rest r64  :rest64   0 4 1/16   #'draw-64th-rest rest64-bounds)
(define-rest r128 :rest128  0 5 1/32   #'draw-128th-rest rest128-bounds)

(define-rest rt2  :rest2    0 0 4/3    #'draw-half-rest half-rest-bounds)
(define-rest rt4  :rest4    0 0 2/3    #'draw-quarter-rest quarter-rest-bounds)
(define-rest rt8  :rest8    0 1 1/3    #'draw-8th-rest rest8-bounds)
(define-rest rt16 :rest16   0 2 1/6    #'draw-16th-rest rest16-bounds)
  