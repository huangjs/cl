(in-package :cmn)

;;;
;;; ----------------    duration bracket
;;;

(defclass duration-bracket (sundry font-mixin)
  ((duration :initarg :duration :initform nil :accessor duration)
   (justification :initarg :justification :initform :up :accessor justification)
   (note0 :initarg :note0 :initform nil :accessor note0)
   (note1 :initarg :note1 :initform nil :accessor note1)
   (max-line :initarg :max-line :initform nil :accessor max-line)
   (duration-bracket-ender :initarg :duration-bracket-ender :initform t :accessor duration-bracket-ender)
   (vertical-separation :initarg :vertical-separation :initform 3 :accessor vertical-separation)
   (font-name :initform "Times-Bold")
   (font-scaler :initform .5)))

(deferred-action duration-bracket-ender)

(defmethod descry ((duration duration-bracket) &optional stream controller)
  (format stream "~A~A~A~A~A~A~A~A~A~A"
	  (if (not controller) (format nil "(duration-bracket") "")
	  (if (duration duration) (format nil " :duration ~D" (duration duration)) "")
	  (if (note0 duration) (format nil " :note0 ~A" (note0 duration)) "")
	  (if (note1 duration) (format nil " :note1 ~A" (note1 duration)) "")
	  (if (max-line duration) (format nil " :max-line ~D" (max-line duration)) "")
	  (format nil " :vertical-separation ~A" (vertical-separation duration))
          (format nil " :ender '~A" (duration-bracket-ender duration))
	  (if (next-method-p) (call-next-method duration stream (or controller duration)) "")
	  (if (not controller) ")" "")))

(defmethod copy ((duration duration-bracket) &optional object)
  (let ((new-duration (if (not object) (make-instance 'duration-bracket)
		      (if (write-protected object) (copy object)
			object))))
    (setf (duration new-duration) (duration duration))
    (setf (max-line new-duration) (max-line duration))
    (setf (vertical-separation new-duration) (vertical-separation duration))
    (setf (duration-bracket-ender new-duration)  (duration-bracket-ender duration))
    (if (next-method-p) (call-next-method duration new-duration))
    new-duration))

(defun start-duration-bracket (dur &rest args)
  (make-self-acting
   :action #'(lambda (note &rest rest)
	       (declare (ignore rest))
	       (let ((new-8 (make-instance 'duration-bracket
			     :note0 note
			     :duration dur
			     :mark #'display-duration
			     :name :duration)))
		 (setf duration-stack new-8)
		 (if args (loop for arg in args do
			    (if (self-acting-p arg)
				(funcall (action arg) new-8 (argument arg)))))
		 nil))
   :argument nil))

(defun end-duration-bracket (&rest args)
  ;;(declare (ignore args))
  (make-self-acting 
   :action #'(lambda (note &rest rest)
	       (declare (ignore rest))
	       (if (not duration-stack) 
		     (funcall (action (start-duration-bracket dur)) note nil))
	       (add-to-marks (note0 duration-stack) (list duration-stack))
	       (setf (note1 duration-stack) note)
	       (setf duration-stack nil))
   :argument nil))

(defgeneric (setf durationd) (val obj))
;;(defgeneric (setf store-data) (val obj))

(defun start-and-end-duration-bracket (dur &rest args)
  (make-self-acting
   :action #'(lambda (note &rest rest)
	       (declare (ignore rest))
	       ;;(setf (durationd note) t)
	       (add-to-marks note (list (let ((new-8 (make-instance 'duration-bracket
                                                                    :note0 note
                                                                    :note1 note
                                                                    :mark #'display-duration
                                                                    :duration dur
                                                                    :name :duration)))
					  (if args (loop for arg in args do
						     (if (self-acting-p arg)
							 (funcall (action arg) new-8 (argument arg)))))
					  new-8))))
   :argument nil))

(setf begin-duration-bracket (start-duration-bracket 4))
(setf duration-bracket (start-and-end-duration-bracket 4))
(setf end-duration-bracket-bracket (end-duration-bracket))

(defun begin-duration-bracket (dur &rest args) (apply #'start-duration-bracket dur args))
(defun duration-bracket (dur &rest args) (apply #'start-and-end-duration-bracket dur args))
(defun end-duration-bracket-bracket (&rest args) (apply #'end-duration-bracket args))

(defun floating-dur-note (score quarters head-type stem-up matrix)
  (let ((nh (get-note-head quarters head-type))
	(nflags (how-many-flags quarters))
	(ndots (how-many-dots quarters))
	(stem-length (if stem-up .7 -.7)))
    (matrix-front score matrix)
    (moveto score 0 0)
    (funcall (draw-func nh) score);;(if (sundry-p nh)  (display nh t score justifying))
    (when (< quarters 4)
      (let ((x0 (if stem-up (- (width nh) *half-stem-width*) 0)))
	(setf (line-width score) *stem-width*)
	(moveto score x0 0)
	(rlineto score 0 stem-length)
	(draw score)
	(setf (line-width score) 0)
	(when (plusp nflags) 
          (draw-flags score nh nflags stem-up stem-length -0.01))))
    (when (plusp ndots)
      (loop for i from 1 to ndots and xd from .5 by *dot-spacing* do
        (cmn-dot score xd 0.0)))
    (matrix-back score)))


(defun display-duration (sundry note score &optional justifying)
  (if (and (not justifying) )
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
                                  (list (make-instance 'duration-bracket
                                                       :name :duration
                                                       :mark #'display-duration
                                                       :note0 nil
                                                       :dx (vis-dx sundry)
                                                       :dy (vis-dy sundry)
                                                       :duration (duration sundry)
                                                       :max-line nil))))))
              (add-to-marks (note1 sundry) 
                            (list (make-instance 'duration-bracket
                                                 :name :duration
                                                 :mark #'display-duration
                                                 :note0 nil
                                                 :dx (vis-dx sundry)
                                                 :dy (vis-dy sundry)
                                                 :note1 (note1 sundry)
                                                 :duration (duration sundry)
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
	
        (let* ((direction (justification sundry))
               (durations (duration sundry))
               (x0 (- (x0 (note0 sundry)) (if no-start 0 .2)))
               ;(x1 (+ (x1 (note1 sundry)) (vis-dx (note1 sundry)) (x1 sundry)))
	       (x1 (+ (x1 (note1 sundry))
		      (if (eq (stem-direction (note1 sundry)) :up) 0.0 (width (note1 sundry)))
		      (vis-dx (note1 sundry)) (x1 sundry)))
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
               (durmsg (if (text-p durations) durations (format nil "~A" durations)))
               (siz (scr-size score))
               (mid-y0 (+ y0 (if (eq direction :up) .2 -.2)))
               ;;(px1 (* (+ x0 0.7)))
               (px1 (min (* (+ x0 1.0)) (+ x0 (- (/ (- x1 x0) 2.0) 0.15))))
               )
          (incf x0 (vis-dx sundry))
          (incf y0 (vis-dy sundry))
          (incf mid-y0 (vis-dy sundry))
          (incf px1 (vis-dx sundry))
          (when (not no-start)
            (moveto score x0 y0)
            (lineto score x0 mid-y0)
            (lineto score (+ x0 0.2) mid-y0)
            (draw score))
          (when (not no-start)
            (moveto score (+ x0 0.2) mid-y0)
            (lineto score px1 mid-y0
                    ;;:pattern (map 'list #'(lambda (pt) (* pt (/ (scr-size score) 40))) (pattern sundry))
                    ))
          (when (not no-start)
            (if (note-p durations)
                (floating-dur-note score
                                   (quarters durations)
                                   (note-head durations)
                                   (eq direction :up)
                                   (scale-matrix
                                    (translate-matrix score sundry
                                                      (+ px1 0.1 (dx durations))
                                                      (+ (dy durations)
                                                         (- mid-y0 
                                                            (if (eq direction :up) 0.1 -0.1))))
                                    0.8 0.8))
              (if (sundry-p durations)
                  (progn (matrix-front score
                                       (translate-matrix
                                        score sundry 0.7 (if (eq direction :up) 0.3 -2.2)))
                         (funcall (sundry-mark durations) durations note score)
                         (matrix-back score))
                (show score (cmn-text :font-name (font-name sundry)
                                      :font-scaler (font-scaler sundry)
                                      :letters durmsg)
                      :matrix (translate-matrix score sundry (+ px1 0.1) (- y0 (if (eq direction :up) 0 .4)))))))
          (when (< (+ px1 .5) x1)
            (moveto score (+ px1 (if (note-p durations) 0.5
                                   (if (sundry-p durations) 1.6
                                     (* 0.3  (length durmsg) )))) mid-y0)
            (lineto score (- x1 0.1) mid-y0 :pattern (map 'list #'(lambda (pt) 
                                                                    (* pt (/ (scr-size score) 40))) 
                                                          (pattern sundry)))
            (draw score))
          (cond ((member (duration-bracket-ender sundry) '(:arrow arrow))
                 (progn (moveto score (+ x1 0.1) mid-y0)
                        (lineto score (- x1 0.2) (+ mid-y0 0.1))
                        (lineto score (- x1 0.05) mid-y0)
                        (lineto score (- x1 0.2) (- mid-y0 0.1))
                        (lineto score (+ x1 0.1) mid-y0)
                        (fill-in score)))
                ((or no-end
                     (member (duration-bracket-ender sundry) '(:none none))
                     (not (duration-bracket-ender sundry)))
                 t)
                (t (progn
                     (moveto score (- x1 0.1) mid-y0)
                     (lineto score x1 mid-y0)
                     (lineto score x1 y0)
                     (draw score))))))))


#|

(cmn (size 20) staff treble
     (c5 q (begin-duration-bracket ""))
     g4 q (c4 q (end-duration-bracket))
     (c4 q (begin-duration-bracket ""
            (justification :up) (duration-bracket-ender :arrow)
            (x1 -0.3)))
     c4 q (c4 q (end-duration-bracket))
     (c4 q (begin-duration-bracket (c4 q)))
     c4 q (c4 q (end-duration-bracket))
     (c4 q (begin-duration-bracket "3 s." ))
     c4 q (c4 q (end-duration-bracket)))

|#
