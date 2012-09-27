;;;;
;;;; hello-world.lisp
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

(in-package #:graphic-forms.uitoolkit.tests)

(defvar *hello-win* nil)

(defclass hellowin-events (gfw:event-dispatcher) ())

(defun exit-fn (disp item)
  (declare (ignore disp item))
  (gfs:dispose *hello-win*)
  (setf *hello-win* nil)
  (gfw:shutdown 0))

(defmethod gfw:event-close ((disp hellowin-events) window)
  (declare (ignore window))
  (exit-fn disp nil))

(defmethod gfw:event-paint ((disp hellowin-events) window gc rect)
  (declare (ignore window rect))
  (gfg:clear gc gfg:*color-white-smoke*)
  (setf (gfg:background-color gc) gfg:*color-red*)
  (setf (gfg:foreground-color gc) gfg:*color-green*)
  (gfg:draw-text gc "Hello World!" (gfs:make-point)))

(defun hello-world-internal ()
  (let ((menubar nil))
    (setf *hello-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'hellowin-events)
                                                    :style '(:frame)))
    (setf menubar (gfw:defmenu ((:item "&File"
                                 :submenu ((:item "E&xit" :callback #'exit-fn))))))
    (setf (gfw:menu-bar *hello-win*) menubar)
    (gfw:show *hello-win* t)))

(defun hello-world ()
  (gfw:startup "Hello World" #'hello-world-internal))
