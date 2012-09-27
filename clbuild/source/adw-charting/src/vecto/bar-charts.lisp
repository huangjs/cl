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

(defclass bar-chart (line-chart) ())

(defmethod calculate-graph-bounds :after ((chart bar-chart) graph)
  ;;make some room for the bars   
  (incf (x (data-max graph))
	(/ (data-distance #'x graph)
	   (number-of-bars chart))))

(defvar *bar-width* 1)

(defun calculate-bar-width (chart graph)
  (setf *bar-width*
	(max 1 (truncate
		(/ (* 0.5 (width graph))
		   (number-of-bars chart)))))
  )

(defun number-of-bars (chart)
  (loop for series in (chart-elements chart)
	sum (length (data series))))

(defmethod draw-series ((chart bar-chart) graph)
  (let ((bars-drawn (make-hash-table))
	(*bar-width*
	 (max 1
	      (truncate (/ (* 0.5 (width graph))
			   (number-of-bars chart))))))
    (dolist (series (chart-elements chart))
      (if (eq (mode series) 'default)
	  (draw-bar-series series graph bars-drawn)
	  (draw-line-series series graph)))))

(defun draw-bar-series (series graph bars-drawn)
;;  (decf (width graph) (* 2 *bar-width*))
  (with-graphics-state
    (set-line-width 2)
    (set-fill series)
    (iter (with ry = (y (dp->gp graph 0 0)))
	  (for (x y) in (data series))
	  (for num-bars-drawn = (gethash x bars-drawn 0))
	  (for gp = (dp->gp graph x y))
	  (for rx = (+ (x gp)
		       (* num-bars-drawn
			  *bar-width*)))
	  (for rh = (- (y gp) ry))
	  (rectangle rx ry *bar-width* rh)
	  (fill-path)  
	  (incf (gethash x bars-drawn 0))))
  ;;(incf (width graph) (* 2 *bar-width*))
  )

(defmacro with-bar-chart ((width height &key (background ''(1 1 1))) &body body)
  "Evaluates body with a chart established with the specified
dimensions as the target for chart commands, with the specified background."
  `(let ((*current-chart*  (make-instance 'bar-chart
					  :width ,width
					  :height ,height
					  :background ,background)))
     ,@body))