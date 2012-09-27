(in-package :adw-charting)

(defvar *default-font-file*
  (merge-pathnames
   "FreeSans.ttf"
   (asdf:component-pathname
    (asdf:find-system :adw-charting))))

(defvar *current-font* nil "a font object")
(defvar *font* nil "a font object")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-font ((&optional font-file) &body body)
    "ensures *font* is a valid font loader."
    `(let ((*font* (or *font* (get-font (or ,font-file *default-font-file*)))))
      ,@body)))

(defclass vchart (chart)
  ())

(defmethod move ((p point))
  (move-to (x p) (y p)))

(defmethod line ((p point))
  (line-to (x p) (y p)))

(defmethod font-bounding-box ((chart chart) text)
  "gets the bounding box for the given text on the given chart."  
  (with-font ()
    (string-bounding-box (if (stringp text) text
			     (princ-to-string text))
			 (label-size chart)
			 *font*)))

(defmethod font-height ((chart chart))
  "gets the pixel height of the default font, at
the size specified in the chart's label-size"
  (aref (font-bounding-box chart "A") 3))

(defmethod font-width ((chart chart) text)
  "gets the pixel width of the default font, as the size
specified in the chart's label-size"
  (aref (font-bounding-box chart text) 2))

(defgeneric set-fill (obj)
  (:documentation "shortcuts for setting the vecto fill color"))

(defmethod set-fill ((lst cons))
  (apply #'set-rgb-fill lst))

(defmethod set-fill ((chart chart))
  (when-let (bg (background chart))
    (set-fill bg)))

(defmethod set-fill ((elem chart-element))
  (set-fill (color elem)))

(defgeneric set-stroke (obj)
  (:documentation "shortcuts for setting the vecto stroke color"))

(defmethod set-stroke ((lst cons))
  (apply #'set-rgb-stroke lst))

(defmethod set-stroke ((elem chart-element))
  (set-stroke (color elem)))

(defun %render-chart (&optional (chart *current-chart*))
  (set-fill chart) 
  (clear-canvas);;fills in the background
  
  ;;ensure we have colors to auto-assign
  (with-color-stack ()
    (draw-chart chart)
    (when (draw-legend-p chart)
      (draw-legend chart))))



(defgeneric draw-legend (elem)
  (:documentation "handles drawing legends for the given chart")
  (:method ((chart chart))
	   (draw-legend-labels chart)))

(defgeneric legend-start-coords (chart box-size label-spacing)
  (:documentation "specifies where legends should start drawing"))

(defgeneric translate-to-next-label (chart w h)
  (:documentation "translates the active vecto canvas to the next
place a label should go")
  (:method ((chart chart) w h)
	   (declare (ignore chart w h))))

(defun draw-legend-labels (chart)
  "handles drawing legend labels"
  (with-graphics-state
    (with-font ()
      (let* ((elems (chart-elements chart))
	     (text-height (font-height chart))
	     (box-size (* 2 text-height))
	     (label-spacing (/ text-height 2)))
	(set-font *font* (label-size chart)) ;set the font
	(set-rgb-fill 0 0 0)		;text should be black
	(apply #'translate (legend-start-coords chart box-size label-spacing))
	(dolist (elem elems)
	  ;;translate the origin to the next label
	  (with-graphics-state
	    (set-fill (color elem))
	    (rounded-rectangle 0 label-spacing box-size box-size label-spacing text-height)
	    (fill-and-stroke))
	  (draw-string (+ box-size label-spacing)
		       text-height
		       (label elem))
	  (translate-to-next-label chart
				   (+ box-size label-spacing label-spacing
				      (font-width chart (label elem)))
				   (+ box-size label-spacing)))))))


(defmethod save-chart-to-file (filename (chart chart))
  "saves the chart to the file"
  (with-canvas (:width (width chart)
		       :height (height chart))
    (setf (chart-elements chart)
	  (reverse (chart-elements chart)) )
    (%render-chart)
    (save-png filename)))

(defmethod save-chart-to-stream (stream (chart chart))
  (with-canvas (:width (width chart) :height (height chart))
    (%render-chart)
    (save-png-stream stream)))

(defparameter +chart-types+ '((:line line-chart)
			      (:bar bar-chart)
			      (:pie pie-chart)
			      (:star-rating star-rating-chart)))


(defmacro with-chart ((type width height &key (background ''(1 1 1))) &body body)
  "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."
  `(let ((*current-chart*  (make-instance (find-class (cadr (assoc ,type +chart-types+)))
					  :width ,width
					  :height ,height
					  :background ,background)))
    ,@body))