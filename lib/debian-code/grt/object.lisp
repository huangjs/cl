;; This file is part of GRT, a Common Lisp raytracing system
;; Copyright (C) 2002-2003 Nikodemus Siivola
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

(in-package grt)
(export '(box csg-difference csg-intersection plane sphere))

;;;;
;;;; GENERIC SCENE OBJECT
;;;;
;;;; Scene objects are implemented as structures containing closures:
;;;; object constructor functions such as SPHERE are responsible
;;;; for passing the correct parameters to MAKE-OBJECT and translating
;;;; user input into specific types.
;;;;

;;
;; Todo:
;;
;; Store intersection-matrix and texture-matrix separately: pigment
;; matrix should not contain the transformation inferred from object
;; parameters. We don't really need the original matrices at at all, do
;; we: just the inverses?
;;
;; Implement intersect-p and bounding volumes (esp. for CSG objects).
;;
;; Pigment & other such functions should be called with the ray, not just
;; point: that way it becomes trivial to implement textures that change
;; color depending on the angle.
;;
;; Object struct should probably look like:
;;
;; (defstruct object
;;      shape
;;      texture
;;      shape-matrix
;;      texture-matrix)
;;
;; (defstruct shape
;;      intersect-first
;;      intersect-all
;;      normal
;;      proximity)
;;
;; and ditto for texture. This way eg. all spheres could share the same
;; shape-struct, which would be very handy for implementing particle systems,
;; etc with *lots* of objects.
;;
;; This way we also get convenient bundling of texture-stuff for CSG.
;; Is this a 0.2 or a 0.3 issue?
;;

(defstruct (object
	    (:constructor make-object
			  (&key
			   intersect-first intersect-all
			   normal proximity
			   (transform (identity-matrix))
			   pigment reflection filter transmit
			   diffuse ambient ior
			   &aux
			   (matrix transform)
			   (inverse-matrix (inverse-matrix transform)))))
  "Generic scene object structure."
  (bounding        (lambda (r) t) :type (function (ray) boolean))
  (intersect-first (lambda (r) 0.0) :type (function (ray) grt-float))
  (intersect-all   (lambda (r) #()) :type (function (ray) (vector grt-float)))
  (normal          (lambda (v) +O+) :type (function (grt-vector) grt-vector))
  (proximity       (lambda (v) 1.0) :type (function (grt-vector) grt-float))
  (matrix          (identity-matrix) :type matrix-4)
  (inverse-matrix  (identity-matrix) :type matrix-4)
  (pigment         (make-pigment :solid +white+) :type function) 
  (reflection      (make-solid 0.0) :type function) 
  (filter          (make-solid 0.0) :type function) 
  (transmit        (make-solid 0.0) :type function) 
  (diffuse         1.0 :type grt-float)
  (ambient         0.0 :type grt-float)
  (ior             1.0 :type grt-float))

(defun object-adjunct-matrix (object)
  (transpose-matrix (object-inverse-matrix object)))

(defun may-intersect-p (object ray)
  ;; Note that we do not transform the ray, since bounding objects are
  ;; generally defined in ray space.
  (funcall (object-bounding object) ray))

(defun intersect-first (object ray)
  (if (may-intersect-p object ray)
      (funcall (object-intersect-first object)
	       (transform-ray ray (object-inverse-matrix object)))
    0.0))

(defun intersect-all (object ray)
  (if (may-intersect-p object ray)
      (funcall (object-intersect-all object)
	       (transform-ray ray (object-inverse-matrix object)))
    #()))

(defun normal (object point)
  (nnormalize
   (transform-vector
    (funcall (object-normal object)
	     (transform-vector point (object-inverse-matrix object)))
    (object-adjunct-matrix object))))


(defun proximity (object point)
  (funcall (object-proximity object)
	   (transform-vector point (object-inverse-matrix object))))

(defun inside-p (object point)
  (> +grt-epsilon+ (proximity object point)))

(defun pigment (object point)
  (funcall (object-pigment object)
	   (transform-vector point (object-inverse-matrix object))))

(declaim (ftype (function (object grt-vector) (grt-float 0.0 1.0))
		filter))
(defun filter (object point)
  (funcall (object-filter object) 
	   (transform-vector point (object-inverse-matrix object))))

(defun filter-color (object point)
  (color-mul (pigment object point)
	     (* (filter object point)
		(- 1.0 (transmit object point)))))

(declaim
 (ftype (function (object grt-vector) (grt-float 0.0 1.0)) reflection))
(defun reflection (object point)
  (funcall (object-reflection object)
	   (transform-vector point (object-inverse-matrix object))))


(declaim (ftype (function (object grt-vector) (grt-float 0.0 1.0))
		transmit))
(defun transmit (object point)
  (declare (type object object)
	   (type grt-vector point))
  (funcall (object-transmit object)
	   (transform-vector point (object-inverse-matrix object))))


(declaim
 (ftype (function (object grt-vector) (grt-float 0.0 1.0)) transparency))
(defun transparency (object point)
  (let ((transmit (transmit object point)))
    (+ (* (filter object point) (- 1.0 transmit))
       transmit)))

(defun pre-transform (matrix arglist)
  "Replaces :TRANSFORM keyword argument in ARGLIST by MATRIX with the
original transformation applied afterwards. If :TRANSFORM keyword is
not present, then MATRIX is added as one."
  (let ((found nil))
    (labels ((parse (args)
		    (cond ((endp args) nil)
			  ((eq :transform (car args))
			   (setf (cadr args) (combine matrix (cadr args))
				 found t)
			   args)
			  (t (cons (car args) (parse (cdr args)))))))
      (let ((args (parse arglist)))
	(if found
	    args
	  (cons :transform (cons matrix args)))))))

(defun remove-keyword (arglist keyword)
  "Removes the KEYWORD and it's argument from ARGLIST, returns KEYWORD's
argument as first, and the modified ARGLIST as second value."
  (if (member keyword arglist)
      (let ((arg (getf arglist keyword)))
	(remf arglist keyword)
	(values arglist arg))
    (values arglist nil)))

(defun split-args (arglist)
  (let ((i (position-if #'keywordp arglist)))
    (if i
	(values (subseq arglist 0 i) (subseq arglist i nil))
      (values arglist nil))))
  
;;;;
;;;; SPHERE
;;;;

(defun sphere-function-values (ray)
  (values (dot-product (ray-direction ray) (ray-direction ray))
	  (* 2.0 (+ (dot-product (ray-direction ray) (ray-origin ray))))
	  (- (dot-product (ray-origin ray) (ray-origin ray)) 1.0)))

(defun sphere-intersect-first (ray)
  (multiple-value-bind (a b c) (sphere-function-values ray)
    (min-pos-root a b c)))

(defun sphere-intersect-all (ray)
  (multiple-value-bind (a b c) (sphere-function-values ray)
    (pos-roots a b c)))

(defun sphere-normal (point)
  point)

(defun sphere-proximity (point)
  (- (vector-length point) 1.0))

(defun sphere (radius location &rest args)
  (multiple-value-bind (x y z) (vector-values location)
    (apply #'make-object
	   :intersect-first #'sphere-intersect-first
	   :intersect-all #'sphere-intersect-all
	   :normal #'sphere-normal
	   :proximity #'sphere-proximity
	   (pre-transform
	    (combine  (scale radius radius radius)
		      (translate x y z))
	    args))))
  
;;;;
;;;; PLANE
;;;;

(defun plane-intersect-first (ray)
  (declare (type ray ray))
  (let ((d (y (ray-direction ray)))
	(y (-(y (ray-origin ray)))))
    (if (= d 0.0)
	0.0 ; Ray parallel to plane
      (max (/ y d) 0.0))))

(defun plane-intersect-all (ray)
  (let ((d (plane-intersect-first ray)))
    (if (zerop d)
	#()
      (vector d))))

(defun plane-normal (point)
  (declare (ignore point))
  +y+)

(defun plane-proximity (point)
  (y point))

(defun plane (normal location &rest args)
  (apply #'make-object
	 :intersect-first #'plane-intersect-first
	 :intersect-all #'plane-intersect-all
	 :normal #'plane-normal
	 :proximity #'plane-proximity
	 (pre-transform
	  (combine (reorient +y+ normal)
		   (translate location))
	  args)))

;;;;
;;;; BOX
;;;;


(defun box-intersect-first (ray)
  (labels ((sides (oc dc)
		  (if (not (= dc 0.0))
		      (let ((t1 (/ (- -1.0 oc) dc))
			    (t2 (/ (-  1.0 oc) dc)))
			(if (> t1 t2)
			    (values t2 t1)
			    (values t1 t2)))
		    (values 0.0
			    most-positive-grt-float))))
    (grt-vector-bind (((Ox Oy Oz) (ray-origin ray))
		      ((Dx Dy Dz) (ray-direction ray)))
	(let-values (((x1 x2) (sides Ox Dx))
		     ((y1 y2) (sides Oy Dy))
		     ((z1 z2) (sides Oz Dz)))
		    (declare (type grt-float x1 x2))
		    (declare (type grt-float y1 y2))
		    (declare (type grt-float z1 z2))
		    (let ((t1 (max x1 y1 z1))
			  (t2 (min x2 y2 z2)))
		      (if (or (> t1 t2)
			      (< t1 +grt-epsilon+))
			  0.0
			t1))))))

(defun box-intersect-all (ray)
  (labels ((sides (oc dc)
		  (if (not (= dc 0.0))
		      (let ((t1 (/ (- -1.0 oc) dc))
			    (t2 (/ (-  1.0 oc) dc)))
			(if (> t1 t2)
			    (values t2 t1)
			    (values t1 t2)))
		    (values 0.0
			    most-positive-grt-float))))
      (grt-vector-bind (((Ox Oy Oz) (ray-origin ray))
			((Dx Dy Dz) (ray-direction ray)))
	(let-values (((x1 x2) (sides Ox Dx))
		     ((y1 y2) (sides Oy Dy))
		     ((z1 z2) (sides Oz Dz)))
		    (declare (type grt-float x1 x2))
		    (declare (type grt-float y1 y2))
		    (declare (type grt-float z1 z2))
		    (let ((t1 (max x1 y1 z1))
			  (t2 (min x2 y2 z2)))
		      (if (or (> t1 t2)
			      (< t1 +grt-epsilon+))
			  (vector)
			(vector t1 t2)))))))

(defun box-proximity (point)
    (grt-vector-bind (((x y z) point))
	     (cond ((and (< -1.0 x 1.0)
			 (< -1.0 y 1.0)
			 (< -1.0 z 1.0)) -1.0)
		     ((or (< x -1.0)
			  (> x  1.0)
			  (< y -1.0)
			  (> y  1.0)
			  (< z -1.0)
			  (> z  1.0)) 1.0)
		     (t 0.0))))

(defun box-normal (pos)
  (grt-vector-bind (((x y z) pos))
      (cond ((=~ x -1.0) (grt-vector -1.0 0.0 0.0))
	    ((=~ x  1.0) (grt-vector 1.0 0.0 0.0))
	    ((=~ y -1.0) (grt-vector 0.0 -1.0 0.0))
	    ((=~ y  1.0) (grt-vector 0.0 1.0 0.0))
	    ((=~ z -1.0) (grt-vector 0.0 0.0 -1.0))
	    (t           (grt-vector 0.0 0.0 1.0)))))

(defun box (co1 co2 &rest args)
  (let-values (((x1 y1 z1) (vector-values co1))
	       ((x2 y2 z2) (vector-values co2)))
      (let ((co-min (grt-vector (min x1 x2) (min y1 y2) (min z1 z2)))
	    (co-max (grt-vector (max x1 x2) (max y1 y2) (max z1 z2))))
	(apply #'make-object
	       :intersect-first #'box-intersect-first
	       :intersect-all #'box-intersect-all
	       :normal #'box-normal
	       :proximity #'box-proximity
	       (pre-transform
		(combine (scale (vector-div (vector-sub co-max co-min) 2.0))
			 (translate (vector-div (vector-add co-min co-max) 2.0)))
		args)))))

;;;;
;;;; CSG Intersection
;;;;

(defun csg-intersection-intersect-first (obj-a obj-b)
  (lambda (ray)
    (flet ((inside-p (obj) (lambda (d) (inside-p obj (ray-point ray d)))))
      (let ((a (find-if (inside-p obj-b) (intersect-all obj-a ray)))
	    (b (find-if (inside-p obj-a) (intersect-all obj-b ray))))
	(declare (type (or null grt-float) a b))
	(if (and a b)
	    (min a b)
	  (or a b 0.0))))))

(defun csg-intersection-intersect-all (obj-a obj-b)
  (lambda (ray)
    (flet ((inside-p (obj) (lambda (d) (inside-p obj (ray-point ray d)))))
      (merge '(vector grt-float)
	     (remove-if-not (inside-p obj-b) (intersect-all obj-a ray))
	     (remove-if-not (inside-p obj-a) (intersect-all obj-b ray))
	     #'<))))

(defun csg-intersection-normal (obj-a obj-b)
  (lambda (point)
    (if (=~ 0.0 (proximity obj-a point))
	(normal obj-a point)
      (normal obj-b point))))
      
(defun csg-intersection-proximity (obj-a obj-b)
  (lambda (point)
    (max (proximity obj-a point)
	 (proximity obj-b point))))

(defun csg-intersection-pigment (pigment obj-a obj-b)
  (or pigment
      (lambda (point)
	(if (=~ 0.0 (proximity obj-a point))
	    (pigment obj-a point)
	  (pigment obj-b point)))))

(defun csg-intersection (&rest args)
  (let-values (((objects args) (split-args args))
	       ((args pigment) (remove-keyword args :pigment))
	       ((args bounding) (remove-keyword args :bounded-by)))
      (let ((csg (reduce (lambda (obj-a obj-b)		
			   (apply #'make-object
				  :intersect-first
				  (csg-intersection-intersect-first obj-a obj-b)
				  :intersect-all
				  (csg-intersection-intersect-all obj-a obj-b)
				  :normal
				  (csg-intersection-normal obj-a obj-b)	               
				  :proximity
				  (csg-intersection-proximity obj-a obj-b)
				  :pigment
				  (csg-intersection-pigment pigment obj-a obj-b)
				  args))
			 objects)))
	(bound csg bounding)
	csg)))

;;;;
;;;; CSG Difference
;;;;

(defun csg-difference-intersect-first (obj-a obj-b)
  (lambda (ray)
    (flet ((inside-p (obj) (lambda (d) (inside-p obj (ray-point ray d)))))
      (let ((a (find-if-not (inside-p obj-b) (intersect-all obj-a ray)))
	    (b (find-if (inside-p obj-a) (intersect-all obj-b ray))))
	(declare (type (or null grt-float) a b))
	(if (and a b)
	    (min a b)
	  (or a b 0.0))))))

(defun csg-difference-intersect-all (obj-a obj-b)
  (lambda (ray)
    (flet ((inside-p (obj) (lambda (d) (inside-p obj (ray-point ray d)))))
      (merge
       '(vector grt-float)
       (remove-if (inside-p obj-b) (intersect-all obj-a ray))
       (remove-if-not (inside-p obj-a) (intersect-all obj-b ray))
       #'<))))

(defun csg-difference-normal (obj-a obj-b)
  (lambda (point)
    (if (=~ 0.0 (proximity obj-a point))
	(normal obj-a point) ; No need to reverse.
      (normal obj-b point))))

(defun csg-difference-proximity (obj-a obj-b)
  (lambda (point)
    (let ((a (proximity obj-a point))
	  (b (proximity obj-b point)))
      (if (> +grt-epsilon+ b)
	  (max a (- b) +grt-epsilon+)
	a))))

(defun csg-difference-pigment (pigment obj-a obj-b)
  (or pigment
      (lambda (point)
	(if (=~ 0.0 (proximity obj-a point))
	    (pigment obj-a point)
	  (pigment obj-b point)))))
	    
(defun csg-difference (&rest args)
  (let-values (((objects args) (split-args args))
	       ((args pigment) (remove-keyword args :pigment))
	       ((args bounding) (remove-keyword args :bounded-by)))
      (let ((csg (reduce (lambda (obj-a obj-b)
			   (apply #'make-object
				  :intersect-first
				  (csg-difference-intersect-first obj-a obj-b)
				  :intersect-all
				  (csg-difference-intersect-all obj-a obj-b)
				  :normal
				  (csg-difference-normal obj-a obj-b)
				  :proximity
				  (csg-difference-proximity obj-a obj-b)
				  :pigment
				  (csg-difference-pigment pigment obj-a obj-b)
				  args))
			 objects)))
	(bound csg bounding)
	csg)))

;;;;
;;;; Axis-aligned bounding-box
;;;;

;;
;; The API here (mainly function names) needs a bit more thought.
;;

(defun bound (object desc &optional (transform (identity-matrix)))
  (setf (object-bounding object)
	(ecase (car desc)
	  ('box (let ((min (as-grt-vector (second desc)))
			 (max (as-grt-vector (third desc))))
		     (make-aab min max transform)))
	  ('nil (lambda (r) t))))
  object)

(declaim (ftype
	  (function
	   (grt-float grt-float grt-float grt-float grt-float grt-float)
	   (function (ray) boolean))
	  aab-intersect-p))
(defun aab-intersect-p (x0 x1 y0 y1 z0 z1)
  (lambda (ray)
    (labels ((get-distance (co c)
			   (/ (- c
				 (funcall co (ray-origin ray)))
			      (funcall co (ray-direction ray))))
	     (tighten (min max co c0 c1)
		      (declare (type grt-float min max))
		      (let ((t1 (get-distance co c0))
			    (t2 (get-distance co c1)))
			(declare (type grt-float t1 t2))
			(if (< t1 t2)
			    (values (max min t1) (min max t2))
			  (values (max min t2) (min max t1))))))
      (multiple-value-bind (min max) (tighten 0.0 most-positive-grt-float #'x x0 x1)
	(if (< min max)
	    (multiple-value-bind (min max) (tighten min max #'y y0 y1)
	      (if (< min max)
		  (multiple-value-bind (min max) (tighten min max #'z z0 z1)
		    (if (< min max)
			t
		      nil))
		nil))
	  nil)))))
     
(defun make-aab (co1 co2 transform)
  (let (corners)
    (flet ((trans (v) (push (transform-vector v transform) corners)))
      (let-values (((x1 y1 z1) (vector-values co1))
		   ((x2 y2 z2) (vector-values co2)))
        (let ((min (grt-vector (min x1 x2) (min y1 y2) (min z1 z2)))
	      (max (grt-vector (max x1 x2) (max y1 y2) (max z1 z2))))
	  (trans min)
	  (trans max)
	  (trans (grt-vector (x max) (y min) (z min)))	  
	  (trans (grt-vector (x min) (y max) (z min)))
	  (trans (grt-vector (x min) (y min) (z max)))
	  (trans (grt-vector (x max) (y max) (z min)))
	  (trans (grt-vector (x min) (y max) (z max)))
	  (trans (grt-vector (x max) (y min) (z max))))))
    (aab-intersect-p
     (reduce #'min corners :key #'x)
     (reduce #'max corners :key #'x)
     (reduce #'min corners :key #'y)
     (reduce #'max corners :key #'y)
     (reduce #'min corners :key #'z)
     (reduce #'max corners :key #'z))))

