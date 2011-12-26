;
;  This file is part of GRT, a Common Lisp raytracing system
;  Copyright (C) 2002 Nikodemus Siivola
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program; if not, write to the Free Software
;  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;  MA  02111-1307  USA
;

(in-package grt)

(defstruct (ray (:constructor make-ray (&key
					(origin +O+)
					(direction +O+)
					(extent most-positive-grt-float)
					(depth 3)
					(weight 1.0)
					(ior 1.0))))
  "Ray structure. Use MAKE-RAY to create.
   Notice that you must normalize direction by hand."
  (origin    +O+ :type grt-vector)
  (direction +O+ :type grt-vector)
  (depth     0   :type (unsigned-byte 8))
  (extent    most-positive-grt-float :type (grt-float 0.0))
  (weight    0.0 :type grt-float)
  (ior       0.0 :type grt-float)
  (hit-object   nil)
  (hit-ior      0.0     :type grt-float)
  (hit-color    +black+ :type color)
  (hit-normal   +O+     :type grt-vector)
  (hit-point    +O+     :type grt-vector)
  (refraction-c1  0.0 :type grt-float)
  (refraction-cs2 0.0 :type grt-float))

(declaim (inline ray-rel-ior))
(defun ray-rel-ior (ray)
  (/ (ray-ior ray) (ray-hit-ior ray)))

(declaim (inline ray-point))
(defun ray-point (ray dist)
  (declare (type grt-float dist))
  (vector-add (ray-origin ray) (vector-mul (ray-direction ray) dist)))

(declaim (inline transform-ray))
(defun transform-ray (ray matrix)
  (declare (type ray ray)
	   (type matrix-4 matrix))
  (make-ray
   :origin (transform-vector (ray-origin ray) matrix)
   :direction (transform-direction-vector (ray-direction ray) matrix)))
