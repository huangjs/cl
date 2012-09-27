;;;;
;;;; datastructs.lisp
;;;;
;;;; Copyright (C) 2006, Jack D. Unrue
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 
;;;;     1. Redistributions of source code must retain the above copyright
;;;;        notice, this list of conditions and the following disclaimer.
;;;; 
;;;;     2. Redistributions in binary form must reproduce the above copyright
;;;;        notice, this list of conditions and the following disclaimer in the
;;;;        documentation and/or other materials provided with the distribution.
;;;; 
;;;;     3. Neither the names of the authors nor the names of its contributors
;;;;        may be used to endorse or promote products derived from this software
;;;;        without specific prior written permission.
;;;; 
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS "AS IS" AND ANY
;;;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DIS-
;;;; CLAIMED.  IN NO EVENT SHALL THE AUTHORS AND CONTRIBUTORS BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;;;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;;;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

(in-package :graphic-forms.uitoolkit.system)

(defstruct point (x 0) (y 0) (z 0))

(defstruct size (width 0) (height 0) (depth 0))

(defstruct rectangle (location (make-point)) (size (make-size)))

(defstruct span (start 0) (end 0))

(declaim (inline create-rectangle))
(defun create-rectangle (&key (height 0) (width 0) (x 0) (y 0))
  (make-rectangle :location (make-point :x x :y y)
                  :size (make-size :width width :height height)))

(declaim (inline location))
(defun location (rect)
  (rectangle-location rect))

(defun (setf location) (pnt rect)
  (setf (rectangle-location rect) pnt))

(declaim (inline size))
(defun size (size)
  (rectangle-size size))

(defun (setf size) (size rect)
  (setf (rectangle-size rect) size))

(declaim (inline empty-span-p))
(defun empty-span-p (span)
  (= (span-start span) (span-end span)))

(defun equal-size-p (size1 size2)
  (and (= (size-width size1) (size-width size2))
       (= (size-height size1) (size-height size2))))

(defmethod cffi:free-translated-object (ptr (type point-pointer-type) param)
  (declare (ignore param))
  (cffi:foreign-free ptr))

(defmethod cffi:free-translated-object (ptr (type rect-pointer-type) param)
  (declare (ignore param))
  (cffi:foreign-free ptr))

(defmethod cffi:translate-from-foreign (ptr (type point-pointer-type))
  (if (cffi:null-pointer-p ptr)
    (make-point)
    (cffi:with-foreign-slots ((x y) ptr point)
      (make-point :x x :y y))))

(defmethod cffi:translate-from-foreign (ptr (type rect-pointer-type))
  (if (cffi:null-pointer-p ptr)
    (make-rectangle)
    (cffi:with-foreign-slots ((left top right bottom) ptr rect)
      (let ((pnt (make-point :x left :y top))
            (size (make-size :width (- right left) :height (- bottom top))))
        (make-rectangle :location pnt :size size)))))

(defmethod cffi:translate-to-foreign ((lisp-pnt point) (type point-pointer-type))
  (let ((ptr (cffi:foreign-alloc 'point)))
    (cffi:with-foreign-slots ((x y) ptr point)
      (setf x (point-x lisp-pnt)
            y (point-y lisp-pnt)))
    ptr))

(defmethod cffi:translate-to-foreign ((lisp-rect rectangle) (type rect-pointer-type))
  (let ((ptr (cffi:foreign-alloc 'rect))
        (pnt (location lisp-rect))
        (size (size lisp-rect)))
    (cffi:with-foreign-slots ((left top right bottom) ptr rect)
      (setf left   (gfs:point-x pnt)
            top    (gfs:point-y pnt)
            right  (+ (gfs:point-x pnt) (gfs:size-width size))
            bottom (+ (gfs:point-y pnt) (gfs:size-height size))))
    ptr))
