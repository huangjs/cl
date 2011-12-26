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

(in-package grt)
(export '(rgb))

(defstruct (color (:constructor rgb (red green blue))
		  (:conc-name nil))
  "RGB-color type."
  (red   0.0 :type grt-float)
  (green 0.0 :type grt-float)
  (blue  0.0 :type grt-float))


(export '(+black+ +white+ +red+ +green+ +blue+))

(defvar +black+ (rgb 0.0 0.0 0.0))
(defvar +white+ (rgb 1.0 1.0 1.0))
(defvar +red+   (rgb 1.0 0.0 0.0))
(defvar +green+ (rgb 0.0 1.0 0.0))
(defvar +blue+  (rgb 0.0 0.0 1.0))

(defmacro rgb-bind (bindings &body body)
  `(let
       ,(mapcan #'(lambda (bind)
		    (destructuring-bind ((r g b) c) bind
		      `((,r (red ,c))
			(,g (green ,c))
			(,b (blue ,c)))))
		bindings)
     (declare (type grt-float
		    ,@(mapcan #'(lambda (bind)
				  (destructuring-bind ((r g b) c) bind
				    (declare (ignore c))
				    `(,r ,g ,b)))
			      bindings)))
     ,@body))

(declaim
 (inline rgb-8-byte-components)
 (ftype
  (function (color)
	    (values (unsigned-byte 8) (unsigned-byte 8) (unsigned-byte 8)))
  rgb-8-byte-components))
(defun rgb-8-byte-components (color)
  "Returns as multiple values the RGB components of the given color scaled
into 8-byte range (0-255)."
  (flet ((scale (f) (max 0 (min 255 (truncate (* 255.0 f))))))
    (values (scale (red color)) (scale (green color)) (scale (blue color)))))

(declaim
 (inline color-add2)
 (ftype (function (color color) color) color-add2))
(defun color-add2 (c1 c2)
  "Adds two colors togather. What you probably want is the macro color-add."
  (declare (type color c1 c2))
  (rgb-bind (((r1 g1 b1) c1)
	     ((r2 g2 b2) c2))
	    (rgb (+ r1 r2) (+ g1 g2) (+ b1 b2))))

(declaim (inline color-product2))
(defun color-product2 (c1 c2)  
  "Return a product of two colors (the first muted by the second)."
  (declare (type color c1 c2))
  (rgb-bind (((r1 g1 b1) c1)
	     ((r2 g2 b2) c2))
	    (rgb (* r1 r2) (* g1 g2) (* b1 b2))))

(defmacro color-add (&rest colors)
  "Add N colors together."
  (n-to-2 'color-add2 colors))

(defmacro color-product (&rest colors)
  "Product of N colors."
  (n-to-2 'color-product2 colors))

(declaim
 (inline color-mul)
 (ftype (function (color grt-float) color) color-mul))
(defun color-mul (color f)
  "Multiply a color by a float."
  (rgb-bind (((r g b) color))
	    (rgb (* r f) (* g f) (* b f))))

(defun interpolate-color (f c1 c2)
  "Linear interpolation between two colors."
  (declare (type grt-float f)
	   (type color c1 c2))
  (rgb-bind (((r1 g1 b1) c1)
	     ((r2 g2 b2) c2))
	    (rgb (linear-interpolate f r1 r2)
		 (linear-interpolate f g1 g2)
		 (linear-interpolate f b1 b2))))
