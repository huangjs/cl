;;;;
;;;; graphics-generics.lisp
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

(in-package :graphic-forms.uitoolkit.graphics)

(defgeneric background-color (self)
  (:documentation "Returns a color object corresponding to the current background color."))

(defgeneric (setf background-color) (color self)
  (:documentation "Sets the current background color."))

(defgeneric clear (self color)
  (:documentation "Fills self with the specified color."))

(defgeneric data-object (self &optional gc)
  (:documentation "Returns the data structure representing the raw form of self."))

(defgeneric (setf data-object) (data self)
  (:documentation "Sets a data structure representing the raw form of self."))

(defgeneric depth (self)
  (:documentation "Returns the bits-per-pixel depth of the object."))

(defgeneric draw-arc (self rect start-pnt end-pnt)
  (:documentation "Draws the outline of an elliptical arc within the specified rectangular area."))

(defgeneric draw-bezier (self start-pnt end-pnt ctrl-pnt-1 ctrl-pnt-2)
  (:documentation "Draws a Bezier curve between start-pnt and end-pnt."))

(defgeneric draw-chord (self rect start-pnt end-pnt)
  (:documentation "Draws a region bounded by the intersection of an ellipse and a line segment."))

(defgeneric draw-ellipse (self rect)
  (:documentation "Draws an ellipse defined by a rectangle."))

(defgeneric draw-filled-chord (self rect start-pnt end-pnt)
  (:documentation "Fills a region bounded by the intersection of an ellipse and a line segment."))

(defgeneric draw-filled-ellipse (self rect)
  (:documentation "Fills the interior of the ellipse defined by a rectangle."))

(defgeneric draw-filled-pie-wedge (self rect start-pnt end-pnt)
  (:documentation "Filles the interior of a pie-shaped wedge."))

(defgeneric draw-filled-polygon (self points)
  (:documentation "Fills the interior of the closed polygon defined by points."))

(defgeneric draw-filled-rectangle (self rect)
  (:documentation "Fills the interior of a rectangle in the current background color."))

(defgeneric draw-filled-rounded-rectangle (self rect size)
  (:documentation "Fills the interior of the rectangle with rounded corners."))

(defgeneric draw-filled-wedge (self rect start-pnt end-pnt)
  (:documentation "Fills the interior of an elliptical arc within the rectangle."))

(defgeneric draw-image (self image pnt)
  (:documentation "Draws an image at the specified coordinates."))

(defgeneric draw-line (self start-pnt end-pnt)
  (:documentation "Draws a line using the foreground color between start-pnt and end-pnt."))

(defgeneric draw-pie-wedge (self rect start-pnt end-pnt)
  (:documentation "Draws a pie-shaped wedge defined by the intersection of an ellipse and two radials."))

(defgeneric draw-point (self pnt)
  (:documentation "Draws a pixel in the foreground color at the specified point."))

(defgeneric draw-poly-bezier (self start-pnt points)
  (:documentation "Draws a series of connected Bezier curves."))

(defgeneric draw-polygon (self points)
  (:documentation "Draws the closed polygon defined by the list of points."))

(defgeneric draw-polyline (self points)
  (:documentation "Draws the polyline defined by the list of points."))

(defgeneric draw-rectangle (self rect)
  (:documentation "Draws the outline of a rectangle in the current foreground color."))

(defgeneric draw-rounded-rectangle (self rect size)
  (:documentation "Draws the outline of the rectangle with rounded corners."))

(defgeneric draw-text (self text pnt &optional style tab-width)
  (:documentation "Draws the given string in the current font and foreground color."))

(defgeneric font (self)
  (:documentation "Returns the current font."))

(defgeneric (setf font) (font self)
  (:documentation "Sets the current font."))

(defgeneric foreground-color (self)
  (:documentation "Returns a color object corresponding to the current foreground color."))

(defgeneric (setf foreground-color) (color self)
  (:documentation "Sets the current foreground color."))

(defgeneric load (self path)
  (:documentation "Loads the object from filesystem data identified by the specified pathname or string."))

(defgeneric metrics (self font)
  (:documentation "Returns a font-metrics object describing key attributes of the specified font."))

(defgeneric obtain-pixels (self pixels-pointer)
  (:documentation "Plugins implement this to populate pixels-pointer with image pixel data."))

(defgeneric size (self)
  (:documentation "Returns a size object describing the dimensions of self."))

(defgeneric (setf size) (size self)
  (:documentation "Sets the dimensions of self."))

(defgeneric text-extent (self str &optional style tab-width)
  (:documentation "Returns the size of the rectangular area that would be covered by the string if drawn in the current font."))

(defgeneric transparency-mask (self)
  (:documentation "Returns an image object that will serve as the transparency mask for the original image, based on the original image's assigned transparency."))
