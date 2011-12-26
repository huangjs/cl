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
(export '(scene new-scene
	  set-background-color background-color
	  set-ambient-light ambient-light))

(defstruct scene
  (background-color   +black+ :type color)
  (ambient-light      +white+ :type color) ; FIXME: intensity ?
  (adaptive-threshold (* 10 (/ 1.0 256.0)) :type grt-float)
  (objects            nil :type t)
  (lights             nil :type t))

(defun scene-add (scn element)
  (declare (type scene scn))
  (cond ((object-p element)
	 (setf (scene-objects scn) (cons element (scene-objects scn))))
	((light-p element)
	 (setf (scene-lights scn) (cons element (scene-lights scn))))
	(t (error "~%invalid elemens in scene-add: ~S" element))))

(declaim (type scene *scene*))
(defvar *scene* (make-scene))

(defun new-scene ()
  "Initializes a new default scene."
  (setq *scene* (make-scene)))

(defun scene (element)
  "Insert ELEMENT (object or light) to default scene."
  (declare (type (or object light)))
  (scene-add *scene* element))

(defun ambient-light ()
  "Ambient light color of the default scene."
  (scene-ambient-light *scene*))

(defun set-ambient-light (color)
  "Set default scene ambient light to COLOR."
  (declare (type color color))
  (setf (scene-ambient-light *scene*) color))

(defun backgound-color ()
  "Background color of the default scene."
  (scene-background-color *scene*))

(defun set-background-color (color)
  "Set default scene backgound color to COLOR."
  (declare (type color color))
  (setf (scene-background-color *scene*) color))

