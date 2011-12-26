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
(export '(combine translate reorient rotate-around rotate-z rotate-x rotate-y rotate scale))

(defun combine (&rest transforms)
  (reduce #'matrix-multiply transforms))

(defun translate (&rest args)
  (multiple-value-bind (x y z) (vector-values (if (= 1 (length args))
						  (car args)
						args))
    (make-matrix
     '(4 4)
     :initial-contents `((1.0 0.0 0.0 ,x)
			 (0.0 1.0 0.0 ,y)
			 (0.0 0.0 1.0 ,z)
			 (0.0 0.0 0.0 1.0)))))
  
(defun scale (&rest args)
  (multiple-value-bind (x y z) (vector-values (if (= 1 (length args))
						  (car args)
						args))
    (make-matrix
     '(4 4)
     :initial-contents `(( ,x 0.0 0.0 0.0)
			 (0.0  ,y 0.0 0.0)
			 (0.0 0.0  ,z 0.0)
			 (0.0 0.0 0.0 1.0)))))
  
(defun reorient (vector-a vector-b)
  "Constructs a transformation matrix required to orient VECTOR-A with VECTOR-B."
  (let ((vector-a (normalize (as-grt-vector vector-a)))
	(vector-b (normalize (as-grt-vector vector-b))))
    (rotate-around (cross-product vector-a vector-b)
		   (rad-to-deg (acos (dot-product vector-a vector-b))))))

(defun rotate-around (axis degrees)
  "Retuns a rotation matrix corresponding to DEGREES of rotation
around AXIS (which must be a unit-length vector)."
  (multiple-value-bind (x y z) (vector-values axis)
    (let* ((theta (deg-to-rad degrees))
	   (c (fcos theta))
	   (s (fsin theta))
	   (t. (- 1 (fcos theta))))
      (make-matrix
       '(4 4)
       :initial-contents
       `((,(+ (* t. x x) c)       ,(- (* t. x y) (* s z)) ,(+ (* t. x z) (* s y)) 0.0)
	 (,(+ (* t. x y) (* s z)) ,(+ (* t. y y) c)       ,(- (* t. y z) (* s x)) 0.0)
	 (,(- (* t. x z) (* s y)) ,(+ (* t. y z) (* s x)) ,(+ (* t. z z) c)       0.0)
	 (  0.0                     0.0                     0.0                   1.0))))))

(defun rotate (&rest args)
  (multiple-value-bind (x y z) (vector-values (if (= 1 (length args))
						  (car args)
						args))
    (combine (rotate-x x)
	     (rotate-y y)
	     (rotate-z z))))

(defun rotate-z (deg)
  (let ((rad (deg-to-rad deg)))
    (make-matrix
     '(4 4)
     :initial-contents `((,(fcos rad) ,(- (fsin rad)) 0.0 0.0)
			 (,(fsin rad) ,(fcos rad)     0.0 0.0)
			 (  0.0         0.0           1.0 0.0)
			 (  0.0         0.0           0.0 1.0)))))

(defun rotate-x (deg)
  (let ((rad (deg-to-rad deg)))
    (make-matrix
     '(4 4)
     :initial-contents `((1.0   0.0         0.0           0.0)
			 (0.0 ,(fcos rad) ,(- (fsin rad)) 0.0)
			 (0.0 ,(fsin rad) ,(fcos rad)     0.0)
			 (0.0   0.0         0.0           1.0)))))
  
(defun rotate-y (deg)
  (let ((rad (deg-to-rad deg)))
    (make-matrix
     '(4 4)
     :initial-contents `((,(fcos rad)     0.0 ,(fsin rad)  0.0)
			    (0.0          1.0   0.0        0.0)
			 (,(- (fsin rad)) 0.0 ,(fcos rad)  0.0)
			 (  0.0           0.0   0.0        1.0)))))

