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
(export '(light))

(defstruct (light (:constructor make-light (&key color location intensity)))
  (color     +white+                        :type color)
  (location  (grt-vector 100.0 100.0 100.0) :type grt-vector)
  (intensity 1.0                            :type grt-float))

(defun light (&key (color +white+) (intensity 1.0) (location '(100 100 100)))
  (make-light :color color
	      :location (as-grt-vector location)
	      :intensity (grt-float intensity)))
