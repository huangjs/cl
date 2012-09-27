;; Copyright (c) 2008 Accelerated Data Works, Ryan Davis

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation files
;; (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :adw-charting)

(defclass pie-chart (vchart)
  ((total :accessor total :initarg :total :initform nil))
  (:default-initargs :width 400))

(defmethod total ((chart pie-chart))
  "computes the pie-chart total based on the data, if no value is explicitly set"
  (if-let (total (slot-value chart 'total))
	  total
	  (setf (total chart)
		(loop for item in 
		      (chart-elements chart)
		      summing (value item)))))

(defmethod radius ((chart pie-chart))
  (truncate (/ (- (height chart) 10)
	       2)))

(defmethod translate-to-next-label ((chart pie-chart) w h)
  (declare (ignore chart w))
  (translate 0 (- h)))

(defmethod legend-start-coords ((chart pie-chart) box-size label-spacing)
  (list (* 2 (+ (radius chart) (margin chart)))
	(- (height chart) box-size box-size label-spacing)))

(defmethod has-data-p ((chart pie-chart))
  (chart-elements chart))

(defmethod draw-chart ((chart pie-chart))
  (let* ((radius (radius chart))
	 (width (width chart))
	 (height (height chart))
	 (cy (- height (+ 5 radius)))
	 (cx (+ 5 radius))
	 (slices (chart-elements chart)))
    ;;draw the background circle
    (set-rgb-stroke 0 0 0)
    (set-rgb-fill 0 0 0)
    (centered-circle-path cx cy (1+ radius))
    (fill-and-stroke)
    


    (if (has-data-p chart)	  
	(if (= 1 (length slices)) ;;only one slice, draw all over the damn thing and call it good.
	    (progn
	      (set-fill (first slices))
	      (rectangle 0 0 width height)
	      (fill-path))
	    ;; more than one slice, go for it
	    (let ((angle1 0)
		  (val-to-radians (/ (* 2 pi) (total chart))))
	      (dolist (item slices)
		(let ((angle2 (+ angle1 (* val-to-radians (value item)))))
		  (set-fill (color item))
		  ;;start in the center
		  (move-to cx cy)
		  (arc cx cy radius angle1 angle2)
		  (fill-and-stroke)
		  (setf angle1 angle2))))) 
	(setf (draw-legend-p chart) nil) ;;no data, supress the legend
	)))

(defmacro with-pie-chart ((width height &key (background ''(1 1 1))) &body body)
  `(let ((*current-chart* (make-instance 'pie-chart
					 :width ,width
					 :height ,height
					 :background ,background)))
    ,@body))

