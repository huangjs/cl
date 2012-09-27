(in-package :adw-charting)

(defclass star-rating-chart (chart chart-element)
  ((number-of-stars :initform 5 :initarg :number-of-stars :accessor number-of-stars)
   (rating :initform 0 :initarg :rating :accessor rating))
  (:default-initargs
      :draw-legend-p nil
      )
  )

(defun set-rating (rating)
  "Sets the rating for this star-rating chart"
  (setf (rating *current-chart*) rating))

(defun set-color (color)
  "Sets the color for this star-rating chart"
  (setf (color *current-chart*) color))

(defvar *star-width* 0)

(defun %draw-star-path (percent-of-width)
  (let ((size (* percent-of-width *star-width*))
	(angle 0)
	(step (* 2 (/ (* pi 2) 5))))
    (translate (/ *star-width* 2)
	       (/ *star-width* 2))
    (move-to 0 size)
    (dotimes (i 5)
      (setf angle (+ angle step))
      (line-to (* (sin angle) size)
	       (* (cos angle) size)))))

(defun draw-star (filled-percent)
  "draws one star, filling it a given percent"


  (with-graphics-state
    (set-fill (color *current-chart*))
    
    ;;draw a fully-filled in star
    (with-graphics-state
      (%draw-star-path .5)
      (fill-path))

    (when (< filled-percent 1)
      ;;draw a slightly smaller star to wipe out the
      ;;interior of the full star, leaving a border
      (with-graphics-state
	(set-fill (background *current-chart*))
	(%draw-star-path .4)
	(fill-path))

      ;;re-fill in the full star, clipped to how full we're supposed to be
      ;;wiping out part of the blanked interior
      (when (plusp filled-percent)
	(rectangle 0 0 (* filled-percent *star-width*) (height *current-chart*))
	(clip-path)
	(end-path-no-op)
	(%draw-star-path .5)
	(fill-path)))))

(defmethod draw-chart ((chart star-rating-chart))
  ;;figure out some basic measurements
  (with-accessors ((width width)
		   (height height)
		   (number-of-stars number-of-stars)) chart
    
    (let* ((*star-width* height) ;;assume stars need to be square
	   ;; calculate the spacing between stars, assuming num stars + 1 spacers
	   (star-spacing (max 1
				(truncate
				 (/ (- width (* number-of-stars *star-width*))
				    (1+ number-of-stars))))))
      ;;draw our stars
      (with-graphics-state
	(translate star-spacing 0)
	(loop for i from 1 to (number-of-stars chart)
	      for fullness = (rating chart) then (1- fullness)
	      do
	   (draw-star (max 0 (min 1 fullness)))
	   (translate (+ *star-width* star-spacing) 0))))))