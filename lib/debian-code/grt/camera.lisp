;;
;;  This file is part of GRT, a Common Lisp raytracing system
;;  Copyright (C) 2002 Nikodemus Siivola
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;;  MA  02111-1307  USA

(in-package grt)
(export '(camera))

(defstruct (camera
	    (:constructor
	     make-camera (&key direction location right up)))
  (location  +O+ :type grt-vector)
  (direction +O+ :type grt-vector)
  (right     +O+ :type grt-vector)
  (up        +O+ :type grt-vector))

(defun camera (&key (look-at +Z+) (focal-length 3.0) (location +O+) (sky +Y+) (up 1.0) (right (/ 4.0 3.0)))
  "Constructor for camera-structures. :LOOK-AT is the spot the camera
will be pointed towards, :LOCATION is the camera location, :SKY defines
the horizatal orientation (set it of +Y+ to tilt camera), :FOCAL-LENGTH
can be increased to \"zoom-in\", and :UP and :RIGHT define the image
proportions."
  (let ((look-at (as-grt-vector look-at))
	(focal-length (grt-float focal-length))	
	(location (as-grt-vector location))
	(sky (as-grt-vector sky))
	(up (grt-float up))
	(right (grt-float right)))
    (let* ((direction-v (normalize (vector-sub look-at location)))
	   (sky-v (normalize (if (vector-alignp sky direction-v) +Z+ sky)))
	   (right-v (normalize (cross-product sky-v direction-v)))
	   (up-v (normalize (cross-product right-v direction-v))))
      (make-camera :location location
		   :direction (vector-mul direction-v focal-length)
		   :right (vector-mul right-v right)
		   :up (vector-mul up-v up)))))

(declaim (type ray *camera-ray*))
(defvar *camera-ray* (make-ray))

(declaim
 (ftype (function (camera grt-float grt-float (unsigned-byte 8)) ray)
	camera-ray))
(defun camera-ray (cam rel-x rel-y depth)
  "Shoots a camera ray with the given parameters. All calls return
   the same ray structure with new slot values."
  (grt-vector-bind (((Dx Dy Dz) (camera-direction cam))
		    ((Rx Ry Rz) (camera-right cam))
		    ((Ux Uy Uz) (camera-up cam)))
	   (setf (ray-origin *camera-ray*) (camera-location cam))
	   (setf (ray-direction *camera-ray*)
		 (nnormalize
		  (grt-vector (+ Dx (* Rx rel-x) (* Ux rel-y))
			      (+ Dy (* Ry rel-x) (* Uy rel-y))
			      (+ Dz (* Rz rel-x) (* Uz rel-y)))))
	   (setf (ray-depth *camera-ray*) depth)  
	   ; Clean up old hits.
	   (setf (ray-weight *camera-ray*)  1.0)   
	   (setf (ray-ior *camera-ray*) 1.0)
	   (setf (ray-hit-object *camera-ray*) nil)
	   *camera-ray*))
