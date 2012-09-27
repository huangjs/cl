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

(defparameter +default-colors+ '((0.7 0.21960784 0.29803923)
				 (.4 0.5 0.9)
				 (0.8 0.8 0.4745098)
				 (0.52156866 0.7 0.30588236)
				 (0.17254902 0.101960786 0.3372549)
				 (0.3372549 0.5294118 0.65882355))
  "rgb 0-1.0 triples")

(defvar *color-stack* +default-colors+)
(defvar *current-chart* nil
  "The currently active chart. Bound for the
      duration of WITH-CHART.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-color-stack (() &body body)
    "resets *color-stack* to the initial list"
    `(let ((*color-stack* (copy-list +default-colors+)))
       ,@body)))

(defclass area ()
  ((width :accessor width
	  :initarg :width
	  :type integer
	  :initform nil)
   (height :accessor height
	   :initarg :height
	   :type integer
	   :initform nil)))

(defclass point ()
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)))

(defmethod x ((lst list))
  (first lst))
(defmethod y ((lst list))
  (second lst))

(defmethod clone ((p point))
  (make-instance 'point 
		 :x (x p)
		 :y (y p)))

(defun make-point (x y)
  (make-instance 'point :x x :y y))

(defclass chart (area)
  ((label-size :accessor label-size
	       :initarg :label-size
	       :initform 12)
   (margin :accessor margin
	   :initarg :margin
	   :initform 10)
   (draw-legend-p :accessor draw-legend-p
		  :initarg :draw-legend-p
		  :initform T)
   (background :accessor background
	       :initarg :background
	       :initform '(1 1 1))
   (chart-elements :accessor chart-elements
		   :initarg :chart-elements
		   :initform nil))
  (:default-initargs :width 200 :height 200))


(defgeneric draw-chart (chart)
  (:documentation "draws the chart, assuming a vecto canvas is open"))

(defclass chart-element ()
  ((color :accessor color :initarg :color :initform nil)
   (label :accessor label :initarg :label :initform "none"))
  (:documentation "this is a super-class for various chart elements"))

(defmethod color ((item chart-element))
  (if-let (color (slot-value item 'color))
	  color
	  (let ((c (pop *color-stack*)))
	    (setf *color-stack* (nconc *color-stack*				       
				       (list (mapcar #'(lambda (x)
						   (/ (+ (if (eq 1 x)
							     .7
							     1) x) 2))
					       c))))
	    (setf (color item) c))))


(defgeneric save-chart-to-file (filename chart)
  (:documentation "saves the chart to the given file"))

(defun save-file (filename)
  "saves the *current-chart* to the given file."
  (save-chart-to-file filename *current-chart*))

(defgeneric save-chart-to-stream (stream chart)
  (:documentation "saves the chart to the given stream"))

(defun save-stream (stream)
  "saves the *current-chart* to the given stream."
  (save-chart-to-stream stream *current-chart*))



(defclass slice (chart-element)  
  ((value :accessor value :initarg :value))
  (:documentation "this is a slice of a pie chart"))

(defun add-slice (label value &key color)
  "add a slice to the pie"
  (push (make-instance 'slice :color color :label label :value value)
	(chart-elements *current-chart*)))

(defclass series (chart-element)
  ((data :accessor data
	 :initarg :data
	 :documentation "a list of (x y) pairs (as lists, not cons cells)")
   (mode :accessor mode
	 :initarg :mode
	 :initform 'default
	 :documentation "a flag for how to render this series"))  
  (:documentation "represents a line on a line chart"))

(defun default-label-formatter (value)
  (typecase value
    ((or float ratio) (format nil "~,1F" value))
    (integer (princ-to-string value))
    (t (progn
	 (break "don't know how to format ~a~%" (type-of value))
	 (princ-to-string value)))
    )
  )

(defclass axis ()
  ((label :accessor label
	  :initarg :label
	  :initform nil
	  :documentation "description of this axis, usually the unit
of measurement ($, s, km, etc)")   
   (label-formatter :accessor label-formatter
		    :initarg :label-formatter
		    :initform #'default-label-formatter
		    :documentation "a function to format data points, for
printing periodic values along the axis")
   (draw-gridlines-p :accessor draw-gridlines-p
		     :initarg :draw-gridlines-p
		     :initform T
		     :documentation "determines if grid-lines are drawn
across the chart")
   (data-interval :accessor data-interval
		  :initarg :data-interval
		  :initform nil)
   (draw-zero-p :accessor draw-zero-p
		:initarg :draw-zero-p
		:documentation "Should we draw a line along the 0 of this axis?")
   (mode :accessor mode
	 :initarg :mode)
   (angle :accessor angle
	  :initarg :angle)
   (scalefn :accessor scalefn
	  :initarg :scalefn
	  :documentation "Values will be passed through this function for scaling prior to display"))
  (:documentation "represents an axis on a line chart"))

(defmethod axis-label ((axis axis) data)
  (funcall (label-formatter axis) data))


(defun add-series (label data &key color (mode 'default))
  "adds a series to the *current-chart*."
  (push (make-instance 'series :label label :data data :color color :mode mode)
	(chart-elements *current-chart*)))

(defun set-axis (axis title &key draw-gridlines-p
		 (label-formatter #'default-label-formatter)
		 (mode :value)
		 data-interval
		 scalefn
		 draw-zero-p
		 angle)
  "set the axis on the *current-chart*.  axis is either :x or :y.
label-formatter is either a format-compatible control string or
a function of 1 argument to control label formatting"
  (let ((ax (make-instance 'axis
			   :label title
			   :draw-gridlines-p draw-gridlines-p
			   :mode mode
			   :scalefn scalefn
			   :angle angle
			   :draw-zero-p draw-zero-p
			   :data-interval data-interval
			   :label-formatter (etypecase label-formatter
					      (string #'(lambda (v)
							  (format nil label-formatter v)))
					      (function label-formatter)))))
    (ccase axis
      (:x (setf (x-axis *current-chart*) ax))
      (:y (setf (y-axis *current-chart*) ax)))))

(defun find-chart-extremes (chart)
  (let ((data-minmax (find-extremes
		      (mapcan #'(lambda (series)
				  (find-extremes (data series)))
			      (chart-elements chart))))
	(x-zero (ignore-errors
		  (draw-zero-p (x-axis chart))))
	(y-zero (ignore-errors
		  (draw-zero-p (y-axis chart)))))
    (if (or x-zero y-zero)
	(destructuring-bind ((min-x min-y) (max-x max-y)) data-minmax
	  (declare (ignore max-x max-y))
	  (when x-zero (push (list 0 min-y) data-minmax))
	  (when y-zero (push (list min-x 0) data-minmax))
	  (find-extremes data-minmax))
	data-minmax)))

(defun find-extremes (data)
  "takes a list of (x y) pairs, and returns the ((x-min y-min) (x-max y-max))"
  (loop for (x y) in data
	maximizing x into x-max
	minimizing x into x-min
	maximizing y into y-max
	minimizing y into y-min
	finally (return (list (list x-min y-min)
			      (list x-max y-max)))))
