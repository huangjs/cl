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
(export '(make-pigment))

(defun make-pigment (&key pattern list solid)
  (cond (solid
	 (lambda (pos)
	   (declare (ignore pos))
	   (let ((c solid))
	     (declare (type color c))
	     c)))
	((and pattern list (= 2(length list)))
	 (lambda (pos)
	   (declare (type grt-vector pos))
	   (interpolate-color (funcall pattern pos) (car list) (cadr list))))))

  