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


;;;; This file contains the in-scene light transport lgic.


(in-package grt)


(declaim 
 (ftype (function (scene ray) (values (or null object) (grt-float 0.0)))
	first-intersection))
(defun first-intersection (scene ray)
  (labels ((rec (obs m-obj m-dist)
		(declare (type grt-float m-dist))
		(if (null obs)
		    (values m-obj m-dist)
		  (let ((dist (intersect-first (car obs) ray)))
		    (declare (type grt-float dist))
		    (if (and (> dist 0.0)
			     (or (< dist m-dist) (not m-obj)))
			(rec (cdr obs) (car obs) dist)
    		        (rec (cdr obs) m-obj m-dist))))))
    (rec (scene-objects scene) nil 0.0)))

(defun set-intersection (ray obj dist)
  "Stores intersection data in ray. Returns NIL."
  (declare (type ray ray)
	   (type object obj)
	   (type grt-float dist))
  (let* ((point (ray-point ray dist))
	 (N (normal obj point))
	 ;; The normal that gets stored is always on the side of the ray.
	 (normal (if (> 0.0 (dot-product N (ray-direction ray)))
		     N
		   (vector-mul N -1.0)))
	 (c1 (- (dot-product (ray-direction ray) normal))))
    (setf (ray-hit-object ray) obj)
    (setf (ray-hit-ior ray) (object-ior obj))
    (setf (ray-hit-point ray) point)
    (setf (ray-hit-normal ray) normal)
    (setf (ray-hit-color ray) (pigment obj point))
    (setf (ray-refraction-c1 ray) c1)
    (setf (ray-refraction-cs2 ray)
	  (- 1.0 (* (expt (ray-rel-ior ray) 2) (- 1.0 (expt c1 2)))))
    nil))

(declaim (inline total-internal-reflection-p))
(defun total-internal-reflection-p (ray)
  (> 0.0 (ray-refraction-cs2 ray)))

(defun shadow-ray (ray light)
  (let* ((origin (ray-hit-point ray))
	 (l-o (vector-sub (light-location light) origin))
	 (len (vector-length l-o))
	 (L (vector-div l-o len)))
    (values (make-ray :origin (vector-adjust origin
					     (ray-hit-normal ray)
					     +grt-epsilon+)
		      :direction L)
	    (- len +grt-epsilon+)
	    L)))

(declaim (inline filter-light))
(defun filter-light (light-color object point)
  (color-add (color-product light-color (filter-color object point))
	     (color-mul light-color (transmit object point))))

(defun shadow-color (light-color light-dist ray scene)
  (declare (type grt-float light-dist)
	   (type ray ray)
	   (type scene scene))
  (let ((shadow light-color)
	(origin (ray-origin ray)))
    (dolist (obj (scene-objects scene))
      (labels ((walk (from)
	         (setf (ray-origin ray) from)
		 (let ((d (intersect-first obj ray)))
		   (declare (type grt-float d))
		   (when (< 0.0 d light-dist)
		     (setq shadow
			   (filter-light shadow obj (ray-point ray d)))
		     (walk (vector-adjust (ray-point ray d)
					  (ray-direction ray)
					  +grt-epsilon+))))))
	(walk origin)))
    shadow))

(defun collect-light (ray scene)
  (let ((lite +black+))
    (dolist (light (scene-lights scene) lite)
      (multiple-value-bind (sr ld L) (shadow-ray ray light)
	(if (< 0.0 (dot-product L (ray-hit-normal ray)))
	    (setq lite
		  (color-add
		   lite
		   (color-mul (shadow-color (light-color light) ld sr scene)
			      (* (light-intensity light)
				 (dot-product L (ray-hit-normal ray)))))))))))

(defun local-model (ray scene)
  (let ((obj (ray-hit-object ray))
	(point (ray-hit-point ray)))
    (declare (type object obj)
	     (type grt-vector point))
    (let ((diffuse (color-mul (collect-light ray scene)
			      (object-diffuse obj)))
	  (ambient (color-mul (scene-ambient-light scene)
			      (object-ambient obj)))
	  (raw-color (color-mul (ray-hit-color ray)
				(- 1.0
				   (transparency obj point)
				   (reflection obj point)))))
      (declare (type color diffuse ambient raw-color))
      (color-product raw-color (color-add ambient diffuse)))))

(defun trace-reflected-ray (w ray scene)
  (declare (type ray ray)
	   (type scene scene)
	   (type grt-float w))  
  (let ((N (ray-hit-normal ray))	
	(D (ray-direction ray)))
    (declare (type grt-vector N D))
    (trace-ray
     (make-ray
      :origin (vector-adjust (ray-hit-point ray) N +grt-epsilon+)
      :direction (vector-sub D (vector-mul N (* 2.0 (dot-product N D))))
      :weight (* w (ray-weight ray))
      :depth (ray-depth ray)
      :ior (ray-ior ray))
     scene)))

(defun trace-refracted-ray (w ray scene) 
  (declare (type ray ray)
	   (type scene scene)
	   (type grt-float w))
  (let ((cs2 (the (grt-float 0.0) (ray-refraction-cs2 ray))))
    (trace-ray
     (make-ray
      :origin (vector-adjust
	       (ray-hit-point ray) (ray-direction ray) +grt-epsilon+)
      :direction (vector-add
		  (vector-mul (ray-direction ray)
			      (ray-rel-ior ray))
		  (vector-mul (ray-hit-normal ray)
			      (- (* (ray-rel-ior ray)
				    (ray-refraction-c1 ray))
				 (sqrt cs2))))
      :weight (* w (ray-weight ray))
      :depth (ray-depth ray)
      :ior (ray-hit-ior ray))
     scene)))
  
(defun trace-ray (ray scene)
  (if (or (= 0 (ray-depth ray)) 
	  (> (scene-adaptive-threshold scene) (ray-weight ray)))
      +black+
      (multiple-value-bind (hit dist) (first-intersection scene ray)
	(decf (ray-depth ray))
	(cond (hit
	       (set-intersection ray hit dist)
	       (let ((point (ray-hit-point ray)))
		 (if (total-internal-reflection-p ray)
		     (color-add (local-model ray scene)
				(trace-reflected-ray 1.0 ray scene))
		     (let* ((obj (ray-hit-object ray))
			    (co-r (reflection obj point))
			    (co-t (transparency obj point)))
		       (color-add
			(local-model ray scene)
			(color-mul
			 (trace-refracted-ray co-t ray scene) co-t)
			(color-mul
			 (trace-reflected-ray co-r ray scene) co-r))))))
	      (t (scene-background-color scene))))))

