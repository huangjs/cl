;;;;
;;;; scroll-grid-panel.lisp
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

(defconstant +grid-cell-extent+ 50)
(defconstant +grid-half-extent+ 25)

(defvar *grid-model-size* (gfs:make-size :width 15 :height 10)) ; grid cells

(defvar *grid-char-size* (gfs:make-size))

(defclass scroll-grid-panel-events (gfw:event-dispatcher) ())

(defun select-grid (disp item)
  (declare (ignore disp item)))

(defun make-scroll-grid-panel (parent)
  (setf *default-pathname-defaults* (parse-namestring gfsys::*gf-tests-dir*))
  (let ((panel-size (gfs:make-size :width (1+ (* (gfs:size-width *grid-model-size*) +grid-cell-extent+))
                                   :height (1+ (* (gfs:size-height *grid-model-size*) +grid-cell-extent+))))
        (panel (make-instance 'gfw:panel :dispatcher (make-instance 'scroll-grid-panel-events)
                                         :parent parent)))
    (setf (gfw:maximum-size panel) panel-size
          (gfw:minimum-size panel) panel-size)
    (assert (gfs:equal-size-p panel-size (slot-value panel 'gfw::max-size)))
    (setf (gfs:size-width *grid-char-size*) (floor +grid-half-extent+ 2)
          (gfs:size-height *grid-char-size*) (floor +grid-half-extent+ 2))
    (setf (gfw:cursor-of panel)
          (make-instance 'gfg:cursor
                         :file (merge-pathnames "custom.cur")))
    panel))

(defun set-grid-scroll-params (window)
  (let* ((disp (gfw:dispatcher window))
         (panel (gfw::obtain-top-child window))
         (panel-size (gfw:size panel))
         (scrollbar (gfw:obtain-horizontal-scrollbar window)))
    (setf (gfw:outer-limit scrollbar) (gfs:size-width panel-size))
    (setf (gfw:thumb-position scrollbar) 0)
    (setf scrollbar (gfw:obtain-vertical-scrollbar window))
    (setf (gfw:outer-limit scrollbar) (gfs:size-height panel-size))
    (setf (gfw:thumb-position scrollbar) 0)
    (setf (gfw:step-increments disp) (gfs:make-size :width 1 :height 1))
    (setf (slot-value disp 'gfw::viewport-origin) (gfs:make-point))
    (gfw:event-resize disp window (gfw:size window) :restored)))

(defmethod gfw:event-paint ((disp scroll-grid-panel-events) window gc rect)
  (declare (ignore window))
  (gfg:clear gc gfg:*color-button-face*)
  (setf (gfg:foreground-color gc) gfg:*color-black*
        (gfg:pen-style gc) '(:solid :flat-endcap))
  (let* ((pnt (gfs:location rect))
         (size (gfs:size rect))
         (first-row (floor (gfs:point-y pnt) +grid-cell-extent+))
         (last-row (floor (+ (gfs:point-y pnt) (gfs:size-height size)) +grid-cell-extent+))
         (first-col (floor (gfs:point-x pnt) +grid-cell-extent+))
         (last-col (floor (+ (gfs:point-x pnt) (gfs:size-width size)) +grid-cell-extent+))
         (lr-pnt (gfs:make-point :x (* +grid-cell-extent+ (gfs:size-width *grid-model-size*))
                                 :y (* +grid-cell-extent+ (gfs:size-height *grid-model-size*)))))
    (loop for row from first-row upto last-row
          for start-pnt = (gfs:make-point :y (* row +grid-cell-extent+))
          do (progn
               (gfg:draw-line gc start-pnt (gfs:make-point :x (gfs:point-x lr-pnt)
                                                           :y (gfs:point-y start-pnt)))
               (loop for col from first-col upto last-col
                     for text = (format nil "~d ~d" col row)
                     for start-pnt = (gfs:make-point :x (* col +grid-cell-extent+))
                     for text-pnt = (gfs:make-point :x (+ (* col +grid-cell-extent+)
                                                          (- +grid-half-extent+
                                                             (gfs:size-width *grid-char-size*)))
                                                    :y (+ (* row +grid-cell-extent+)
                                                          (- +grid-half-extent+
                                                             (gfs:size-height *grid-char-size*))))
                     do (progn
                          (if (= row first-row)
                            (gfg:draw-line gc start-pnt (gfs:make-point :x (gfs:point-x start-pnt)
                                                                        :y (gfs:point-y lr-pnt))))
                          (gfg:draw-text gc text text-pnt '(:transparent))))))))
