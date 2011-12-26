;; This file is part of GRT, a Common Lisp raytracing system
;; Copyright (C) 2002 Nikodemus Siivola
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA  02111-1307  USA


;;;; Here we implement the user-level rendering interface:
;;;; how to produce an image when given a scene, a camera, plus
;;;; a smattering of parameters.


(in-package grt)
(export '(render))

;; This is a bit of a hack: togather with two asd definitions (grt and
;; grt-bare) it enables us to deal with missing CL-SDL. There has to be
;; a better way, though.
(defun window-loaded-p ()
  (if (member "SDL" *modules* :test #'string=)
      t
    nil))

(defun render (&key file screen (camera (camera)) (scene *scene*)
		    (width 400)
		    (height 300)
		    (refresh 1)
		    (keep-open 3)
		    (depth 1))
  (declare (type fixnum width height depth))
  "Render a scene to file / on screen."
  (if (not (or file screen))
      (error "Keyword argument :file or :screen required."))
  (when (and screen (not (window-loaded-p)))
    (warn "No window-component available, :screen ignored.")
    (setq screen nil))
  (if file (ensure-directories-exist file))
  (let* ((img (if file (open file :direction :output) nil))
	 (win (if screen (open-window width height)))
	 (y 0)
	 (last (get-internal-real-time))
	 (delay (truncate (/ internal-time-units-per-second
			     (grt-float refresh)))))
    (declare (type (integer 0) y))
    (labels ((update-fn ()
	(if (< y height)
	    (dotimes (x width)
	      (let* ((p (render-ray camera scene x y width height depth)))
		(multiple-value-bind (r g b) (rgb-8-byte-components p)
		      (if img (format img "~D ~D ~D~%" r g b))
		      (if screen
			  (let ((now (get-internal-real-time)))
			    (draw-pixel win x y r g b)
			    (when (< delay (- now last))
			      (setq last now)
			      (refresh-window win))))))))
	(when (> y height) 
	  (if img (close img))
	  (when screen
	    (refresh-window win)
	    (cond ((numberp keep-open) (sleep keep-open))
		  ((eq t keep-open) (wait-for-close win)))
	    (close-window win))
	  (return-from render nil))
	(incf y)))
      (if img (format img "P3~%~D ~D~%255~%" width height))
      (unwind-protect
	  (if screen
	      (event-loop #'update-fn)
	    (loop (funcall #'update-fn)))
	(if img (close img))
	(if screen (close-window win))))))


(declaim 
 (ftype
  (function
   (camera scene (unsigned-byte 32)(unsigned-byte 32)
	   (unsigned-byte 32) (unsigned-byte 32) (unsigned-byte 8))
   color)
   render-ray))
(defun render-ray (camera scene x y width height depth)
  "Casts and traces a camera ray with the given image-plane coordinates."
  (trace-ray (camera-ray camera
			 (- (/ (* 2.0 x) (float width)) 1.0)
			 (- (/ (* 2.0 y) (float height)) 1.0)
			 depth)
	     scene))

