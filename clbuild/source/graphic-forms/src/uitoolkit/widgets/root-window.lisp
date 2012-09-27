;;;;
;;;; root-window.lisp
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

(in-package :graphic-forms.uitoolkit.widgets)

;;;
;;; macros and helper functions
;;;

(defmacro with-root-window ((win) &body body)
  `(let ((,win (make-instance 'root-window)))
     (unwind-protect
         (progn
           ,@body)
       (gfs:dispose ,win))))

;;;
;;; methods
;;;

(defmethod gfs:dispose ((self root-window))
  (setf (slot-value self 'gfs:handle) nil))

(defmethod (setf dispatcher) (disp (self root-window))
  (declare (ignore disp))
  (error 'gfs:toolkit-error :detail "The root window cannot be assigned an event-dispatcher."))

(defmethod enable ((self root-window) flag)
  (declare (ignore flag))
  (error 'gfs:toolkit-error :detail "The root window cannot be enabled or disabled."))

(defmethod enable-layout ((self root-window) flag)
  (declare (ignore flag))
  (error 'gfs:toolkit-error :detail "The root window has no layout functionality."))

(defmethod initialize-instance :after ((self root-window) &key)
  (setf (slot-value self 'gfs:handle) (gfs::get-desktop-window)))

(defmethod (setf location) (pnt (self root-window))
  (declare (ignore pnt))
  (error 'gfs:toolkit-error :detail "The root window cannot be repositioned."))

(defmethod layout ((self root-window))
  (error 'gfs:toolkit-error :detail "The root window has no layout functionality."))

(defmethod owner ((self root-window))
  nil)

(defmethod pack ((self root-window))
  (error 'gfs:toolkit-error :detail "The root window has no layout functionality."))

(defmethod parent ((self root-window))
  nil)

(defmethod show ((self root-window) flag)
  (declare (ignore flag))
  (error 'gfs:toolkit-error :detail "The root window cannot be shown or hidden."))

(defmethod text ((self root-window))
  (error 'gfs:toolkit-error :detail "The root window has no title."))

(defmethod (setf text) (str (self root-window))
  (declare (ignore str))
  (error 'gfs:toolkit-error :detail "The root window has no title."))
