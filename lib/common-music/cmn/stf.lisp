(in-package :cmn)

;;; fancier staff-lines handling (Anders Vinjar)

;;; an object that sits in place on a staff and gets justified, but is otherwise a no-op
(defclass staff-marker (odb visible box staff-relative-mixin) ())
(defmethod staff-marker-p ((obj t)) nil)
(defmethod staff-marker-p ((obj staff-marker)) t)
(defmethod house ((fun staff-marker) score)
  (declare (ignore score))
  (setf (walls fun) (list 0.0 0.0))
  (setf (fences fun) (list 0.0 0.0))
  (setf (expanders fun) (list 0 0)))

(defmethod display ((fun staff-marker) container score &optional justifying)
  (declare (ignore container score justifying))
  t)

(defun staff1-lines (n &optional (start-line 0) (line-separation 1))
  (make-instance 'sundry
    :name :staff1-lines
    :mark #'(lambda (mark ignored score &optional justifying)
              (declare (ignore mark ignored score justifying))
              (list n start-line line-separation))
    :source (format nil "(staff1-lines ~D ~D ~F)" n start-line line-separation)))

(defun staff1-mark (&rest line-specs)
  (make-instance 'staff-marker :marks (list (apply #'staff1-lines line-specs))))

(defvar staff1-first-line t)

(deferred-action draw-func) ; set in the source?

(defun find-staff1-marks (stf data score)
  (let ((point-list nil)
        (nl (staff-lines stf))
        (sl (staff-start-line stf))
        (sep 1)
	(in-staff nil))
    (loop for obj in data do
      (when (staff-p obj)
	(if in-staff
	    (progn
              (push (list :eol nl sl sep) point-list)
	      (return-from find-staff1-marks (values (reverse point-list))))
	  (setf in-staff (equal obj stf))))
      (when (and (or (bar-p obj) (note-p obj) (staff-marker-p obj)) (marks obj))
        (loop for m in (marks obj) do
          (when (and m (sundry-p m) (eq (sundry-name m) :staff1-lines))
            (let* ((nx1 (+ (vis-dx obj) (x0 obj) (if (note-p obj) (- (car (walls obj))) 0.0)))
                   (mark-data (funcall (sundry-mark m) m t score))
                   (nlines (first mark-data))
                   (start-line (second mark-data))
                   (line-sep (third mark-data)))
              (setq nl nlines)
              (setq sl start-line)
              (setq sep line-sep)
              (and (bar-p obj) (not (height obj)) (setf (height obj) (* 8 *staff-line-separation*)))
              (if in-staff (push (list nx1 nlines start-line line-sep) point-list))))))
      finally (push (list :eol nl sl sep) point-list))
    (values (reverse point-list))))

(defun draw-odd-staff (score stf x0 y0 x1)
  (declare (ignore x1))
  (let ((points (find-staff1-marks stf (staff-data (back-staff stf)) score))
        (first-mark-on-line t)
        point-n-lines point-start-line point-line-sep)
    (if staff1-first-line
        (progn (setq point-n-lines (staff-lines stf))
               (setq point-start-line (staff-start-line stf))
               (setq point-line-sep 1)))
    (setf (line-width score) *staff-line-width*)
    (loop for point in points
      do
      (if (and (not staff1-first-line) first-mark-on-line)
          (progn (setq point-n-lines (second point))
                 (setq point-start-line (third point))
                 (setq point-line-sep (fourth point))
                 (setq first-mark-on-line nil)))
      (loop for i from 0 below point-n-lines
        for py0 = (+ y0 (* 2 point-start-line *staff-line-separation*))
        then (+ py0 (* 2 point-line-sep *staff-line-separation*))
        do
        (moveto score x0 py0)
        (if (eq (first point) :eol)
            (progn
              (lineto score (x1 stf) py0))
          (lineto score (first point) py0)))
      (if (eq (first point) :eol)
          (setq x0 (x0 stf))
        (setq x0 (first point)))
      (setq point-n-lines (second point))
      (setq point-start-line (third point))
      (setq point-line-sep (fourth point)))
    (setq staff1-first-line nil)
    (draw score)
    (setf (line-width score) 0.0)))

#|

(cmn
 (staff (draw-func #'draw-odd-staff)
        (treble )
        (d4 w (staff1-lines 2 0))
        (bar  (staff1-lines 1 1))
        (e4 w )
        (bar (staff1-lines 2 0 4))
        (f4 w  )
        (line-mark)
        (g4 w (staff1-lines 2 0 4) )
        (bar (height 1) (staff1-lines 3 1))
        (a4 q )
        (bar  (height 1) (staff1-lines 1 2))
        (b4 w)
        (bar (height 1) (staff1-lines 1 2))
        (line-mark )
        (cs5 w)
        (d4 w (staff1-lines 3 0 2))
        (bar (staff1-lines 3 1))
        (e4 w)
        (bar (staff1-lines 2 0 4))
        (f4 w)
        (line-mark)
        (g4 w  )
        (a4 w (staff1-lines 3 1))
        (bar (height 1) (staff1-lines 1 2))
        (b4 w)
        (line-mark)
        (make-instance 'staff-marker :marks (list (staff1-lines 3 0 2)))
        (c5 w)
        (bar (height 1))))

(excl::shell "ghostview -watch aaa-1.eps&")

|#
