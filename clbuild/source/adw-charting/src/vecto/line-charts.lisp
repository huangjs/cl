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



(defclass line-chart (vchart)
  ((x-axis :accessor x-axis
	   :initarg :x-axis
	   :initform nil
	   :documentation "an axis object to determine formatting for
the X axis")
   (y-axis :accessor y-axis
	   :initarg :y-axis
	   :initform nil
	   :documentation "and axis object to determine formatting for
the Y axis")))

(defclass graph-region (point area)
  ((chart :accessor chart
	  :initarg :chart)
   (data-min :accessor data-min 
	     :initform nil)
   (data-max :accessor data-max 
	     :initform nil)))

(defmethod offset-y ((gr graph-region) offset)
  (let ((offset (floor offset)))
    (incf (y gr) offset)
    (decf (height gr) offset)))

(defmethod offset-x ((gr graph-region) offset)
  (let ((offset (floor offset)))
    (incf (x gr) offset)
    (decf (width gr) offset)))

(defmethod data-scale ((gr graph-region))
  (with-accessors ((w width)
		   (h height)
		   (min data-min)
		   (max data-max)) gr
    (make-point (/ w (max 1 (- (x max) (x min))))
		(/ h (max 1 (- (y max) (y min)))))))

(defmethod data-origin ((graph graph-region))
  (with-accessors ((x x)
		   (y y)
		   (d-s data-scale)
		   (min data-min)
		   (max data-max)) graph
    (let ((d-o (make-point x y)))
      ;;if we have a negative min y, move the y 0 point up
      (when (minusp (y min))
	(incf (y d-o) (abs (* (y d-s) (y min)))))

      ;;if we have a positive min y, move the y 0 down
      (when (plusp (y min))
	(decf (y d-o) (abs (* (y d-s) (y min)))))
    
      ;;if we have a negative min x, move the x 0 point right
      (when (minusp (x min))
	(incf (x d-o) (abs (* (x d-s) (x min)))))

      ;;if we have a positive min x, move the x 0 point left
      (when (plusp (x min))
	(decf (x d-o) (* (x d-s) (x min))))
      d-o)))

(defmethod has-data-p ((chart line-chart))
  (and (chart-elements chart)
       (some #'data (chart-elements chart))))

(defun draw-axes (graph y-axis-labels-x text-height x-axis-labels-y)
  "draws the axes"
  (let (gridlines)
    (macrolet ((draw-gridline ((axis) &body gridline)
		 `(when (draw-gridlines-p ,axis)
		    (push #'(lambda ()
			    (with-graphics-state
				(set-line-width 1)
			      (set-stroke '(0 0 0))
			      (set-dash-pattern #(10 2) 0)
			      ,@gridline
			      (stroke)))
			  gridlines))))

      (when-let (axis (y-axis (chart graph)))
	(iter (for (txt x y) in (calculate-y-axes graph text-height y-axis-labels-x))
	      (maximizing (font-width (chart graph) txt) into label-width)
	      (for half-text = (/ text-height 2))
	      (draw-string x (- y half-text)
			   (if (stringp txt) txt (princ-to-string txt)))
	      (let ((y y))
		(draw-gridline (axis)
			       (move-to (x graph) y)
			       (line-to (+ (x graph) 
					   (width graph)) 
					y)))
	      (finally (incf *current-x* label-width)))

	(offset-x graph *current-x*)
      
	(when (draw-zero-p axis)
	  (push (lambda ()
		  (with-graphics-state
		    (set-line-width 1)
		    (set-rgb-stroke 0 0 0)
		    (move (dp->gp graph (x (data-min graph)) 0))
		    (line (dp->gp graph (x (data-max graph)) 0))
		    (stroke)))
		gridlines)))

      (when-let (axis (x-axis (chart graph)))
	(loop for (txt x) in (calculate-x-axes graph)
	      do (progn
		   (draw-centered-string x  
					 x-axis-labels-y
					 (if (stringp txt) txt (princ-to-string txt)))
		   (let ((x x))
		     (draw-gridline (axis)
				    (move-to x (y graph))
				    (line-to x
					     (+ (y graph) (height graph)))))))
	(when (draw-zero-p axis)
	  (push (lambda ()
		  (with-graphics-state
		    (set-line-width 2)
		    (set-rgb-stroke 0 0 0)
		    (move (dp->gp graph 0 (y (data-min graph))))
		    (line (dp->gp graph 0 (y (data-max graph))))
		    (stroke)))
		gridlines))))
    gridlines)) 

(defun calculate-x-axes (graph)
  (let ((axis (x-axis (chart graph))))  
    (ccase (mode axis)
      (:value (calculate-value-x graph))
      (:category (break "should draw in order"))
      )))

(defun order-of-magnitude (n)
  (if (zerop n)
      1
      (expt 10 (floor (log n 10)))))

(defun data-distance (axis-fn graph)
  (let ((min-val (funcall axis-fn (data-min graph)))
	(max-val (funcall axis-fn (data-max graph))))
    (abs (- min-val max-val))))

(defun calculate-value-x (graph)  
  (let* ((min-x (x (data-min graph)))
	 (max-x (x (data-max graph)))
	 (diff (data-distance #'x graph))
	 (axis (x-axis (chart graph)))
	 (data-interval (or (data-interval axis)
			    (/ (order-of-magnitude diff) 8)))
	 (current-x (x graph))
	 lst)
    ;;start drawing at 0, see how much we have
    (loop for x = min-x then (+ x data-interval)
	  for gx = (round (x (dp->gp graph x 0)))
	  until (> x max-x)
	  do (when (<= current-x gx)
	       ;;record + increment current-x
	       (let* ((txt (axis-label axis x))
		      (width (font-width (chart graph) txt)))
		 
		 (push (list txt gx) lst)
		 (setf current-x (+ gx width
				    (margin (chart graph)))))))
    lst))

(defun calculate-y-axes (graph text-height y-axis-labels-x)
  (let* ((min-y (y (data-min graph)))
	 (axis (y-axis (chart graph)))
	 (diff (data-distance #'y graph))
	 (data-interval (or (data-interval axis)
			    (/ (order-of-magnitude diff) 8)))
	 (desired-text-space (* 2 text-height)))

    ;;be sure the interval has plenty of room in it for our text-height
    (iter (with scalar = (y (data-scale graph)))
	  (summing data-interval into new-interval)
	  (until (< desired-text-space
		    (* new-interval scalar)))
	  (finally
	   (setf data-interval new-interval)))
    
    (let* ((interval-magnitude (order-of-magnitude data-interval))
	   (initial-y (* interval-magnitude
			 (truncate (/ min-y interval-magnitude)))))

      (loop for y = initial-y then (+ y data-interval)
	    for gy = (y (dp->gp graph 0 y))
	    until (> gy (+ (height graph) (y graph)))
	    when (> gy (y graph))
	      collect (list (axis-label axis y)
			  y-axis-labels-x
			  gy)))))

(defun draw-graph-area (graph)
  "draws the graph area"
  (with-graphics-state
    ;;set the chart background as the avg
    ;;between the background color and 1
    (set-rgb-stroke 0 0 0)    
    (rectangle (1- (x graph)) (1- (y graph))
	       (1+ (width graph)) (1+ (height graph)))
        
    (set-fill (mapcar #'(lambda (c)
			  (/ (+ (if (eq 1 c) .9 1)
				c)
			     2))
		      (background (chart graph))))
    (fill-and-stroke)))

(defun draw-graph-outline (graph)
  (with-graphics-state
    (set-rgb-stroke 0 0 0)
    (rectangle (1- (x graph)) (1- (y graph))
	       (1+ (width graph)) (1+ (height graph)))
    (stroke)))


(defmethod dp->gp ((graph graph-region) x y)
  "convert a point from data space to graph space"
  (with-accessors ((d-o data-origin)
		   (d-s data-scale)
		   (gy y)
		   (height height)) graph
    (make-point (+ (x d-o) 
		   (* (x d-s) x))
		(max (y graph)
		     (+ (y d-o) 
			(* (y d-s) y))))))

(defmethod gp->dp ((graph graph-region) x y)
  "convert a point from graph space to data space"
  (with-accessors ((d-o data-origin)
		   (d-s data-scale)) graph
    (make-point (/ (- x (x d-o)) 
		   (x d-s))
		(/ (- y (y d-o)) 
		   (y d-s)))))

(defmethod calculate-graph-bounds ((chart line-chart) graph)
  (destructuring-bind ((min-x min-y) (max-x max-y))
	    (find-chart-extremes chart)
	  
	  (setf (data-min graph)
		(make-point min-x
			    (* 0.99
			       (if (draw-zero-p (y-axis chart))
				   (min 0 min-y)
				   min-y)))
		(data-max graph)
		(make-point max-x (* 1.01 max-y)))
	  
	  

	  ;;TODO: make this a property of the series
)
  )

(defvar *current-x* nil "keeps track of the current x coordinate for layout")

(defmethod draw-chart ((chart line-chart))
  (with-font ()
    (let* ((width (width chart))
	   (height (height chart))
	   (graph-margin (margin chart))
	   (text-height (font-height chart))
	   (legend-space (* 4 text-height))
	   (graph (make-instance 'graph-region 
				 :x graph-margin
				 :y (floor (+ legend-space graph-margin)) 
				 :width (- width graph-margin graph-margin graph-margin)
				 :height (- height graph-margin graph-margin 
					    legend-space)
				 :chart chart))
	   (x-axis-labels-y nil)
	   (*current-x* graph-margin))

      ;;if we're going to be drawing any axes, set the font and color
      (when (or (y-axis chart) (x-axis chart))      
	(set-font *font* (label-size chart))
	(set-rgb-fill 0 0 0)

	;;move the graph region about
	(when-let (axis (x-axis chart))
	  (offset-y graph (* text-height
			     (if (label axis)
				 3
				 2)))
	    
	  (setf x-axis-labels-y (- (y graph) graph-margin text-height))
	  ;;draw the x-label
	  (when-let (label (label axis))
	    (draw-centered-string (+ (x graph) (/ (width graph) 2))
				  (+ (/ graph-margin 2) legend-space)
				  label)))

	;;draw the y-label
	(when-let (label (and (y-axis chart) 
			      (label (y-axis chart))))
	  (with-graphics-state
	    ;;move to the site of the y axis label
	    (translate (+ graph-margin text-height)
		       (+ (y graph) (/ (height graph) 2)))
	   
	    ;;rotate the canvas so we're sideways	
	    (rotate (/ pi 2))
	    (draw-centered-string 0 0 label))
	  (incf *current-x* (+ text-height graph-margin))))

      (when (has-data-p chart)
	;;figure out the right scaling factors so we fill the graph    
					;find the min/max x/y across all series

	(calculate-graph-bounds chart graph)
	(let ((gridline-fns
	       (when (or (y-axis chart) (x-axis chart))
		 ;;set the drawing for grid-lines 
		 (draw-axes graph *current-x*
			    text-height
			    x-axis-labels-y))))	  
	  (draw-graph-area graph)
	  (mapcar #'funcall gridline-fns))
	(draw-series chart graph)
	(draw-graph-outline graph)))))

(defgeneric draw-series (chart graph))

(defmethod draw-series ((chart line-chart) graph) 
  (dolist (series (chart-elements chart))
    (draw-line-series series graph)))

(defun draw-line-series (series graph)
  (with-graphics-state
    (set-line-width 2)
    (set-stroke series)
    (loop for (x y) in (data series)
	  for firstp = T then nil
	  do (funcall (if firstp #'move #'line)
		      (dp->gp graph x y)))
    (stroke)))

(defmethod translate-to-next-label ((chart line-chart) w h)
  "moves the cursor right to the next legend position"
  (declare (ignore chart h))
  (translate w 0))

(defmethod legend-start-coords ((chart line-chart) box-size label-spacing)
  "starts the legends on the bottom row"
  (declare (ignore box-size label-spacing))
  (list (margin chart) (margin chart)))

(defmacro with-line-chart ((width height &key (background ''(1 1 1))) &body body)
  "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."
  `(let ((*current-chart*  (make-instance 'line-chart
					  :width ,width
					  :height ,height
					  :background ,background)))
    ,@body))