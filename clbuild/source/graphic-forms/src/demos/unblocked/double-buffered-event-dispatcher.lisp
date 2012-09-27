;;;;
;;;; double-buffered-event-dispatcher.lisp
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

(in-package :graphic-forms.uitoolkit.tests)

(defvar *background-color* (gfg:make-color :red 0 :green #x80 :blue #x80))

(defgeneric update-buffer (disp)
  (:documentation "Revises the image buffer so that the associated window can be repainted.")
  (:method (disp)
    (declare (ignorable disp))))

(defclass double-buffered-event-dispatcher (gfw:event-dispatcher)
  ((image-buffer
    :accessor image-buffer-of
    :initform nil)))

(defmethod clear-buffer ((self double-buffered-event-dispatcher) gc)
  (gfg:clear gc *background-color*))

(defmethod dispose ((self double-buffered-event-dispatcher))
  (let ((image (image-buffer-of self)))
    (unless (or (null image) (gfs:disposed-p image))
      (gfs:dispose image))
    (setf (image-buffer-of self) nil)))

(defmethod initialize-instance :after ((self double-buffered-event-dispatcher) &key buffer-size)
  (setf (image-buffer-of self) (make-instance 'gfg:image :size buffer-size)))

(defmethod gfw:event-paint ((self double-buffered-event-dispatcher) window gc rect)
  (declare (ignore window rect))
  (gfg:draw-image gc (image-buffer-of self) (gfs:make-point)))
