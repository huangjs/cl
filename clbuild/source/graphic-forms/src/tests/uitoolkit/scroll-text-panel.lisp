;;;;
;;;; scroll-text-panel.lisp
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

(defvar *text-to-draw* "ABCDEFGHIJKLMNOPQRSTUVWXYZ[]0123456789{}")

(defvar *text-model-size* (gfs:make-size :width 100 :height 100)) ; character cells

(defvar *text-panel-font-data* (gfg:make-font-data :face-name  "Lucida Console"
                                                   :point-size 10))

(defclass scroll-text-panel-events (gfw:event-dispatcher)
  ((font
    :accessor font-of
    :initform nil)))

(defun draw-text-chunk (gc metrics row first-col last-col)
  (let* ((col-diff (1+ (- last-col first-col)))
         (text-len (length *text-to-draw*))
         (text-start (mod first-col text-len))
         (text-end (mod last-col text-len))
         (ch-width (gfg:average-char-width metrics))
         (ch-height (gfg:height metrics))
         (pnt (gfs:make-point :x (* ch-width first-col)
                              :y (* ch-height row))))
    (cond
      ((and (<= col-diff text-len) (<= text-start text-end))
         (gfg:draw-text gc (subseq *text-to-draw* text-start (1+ text-end)) pnt))
      ((or (> col-diff text-len) (> text-start text-end))
         (gfg:draw-text gc (subseq *text-to-draw* text-start text-len) pnt)
         (incf (gfs:point-x pnt) (* (- text-len text-start) ch-width))
         (dotimes (i (floor col-diff text-len))
           (gfg:draw-text gc *text-to-draw* pnt)
           (incf (gfs:point-x pnt) (* text-len ch-width)))
         (gfg:draw-text gc (subseq *text-to-draw* 0 (1+ text-end)) pnt)))))

(defun make-scroll-text-panel (parent)
  (let* ((disp (make-instance 'scroll-text-panel-events))
         (panel (make-instance 'gfw:panel :dispatcher disp :parent parent)))
    (gfw:with-graphics-context (gc panel)
      (let* ((metrics (gfg:metrics gc (font-of disp)))
             (panel-size (gfs:make-size :width (* (gfs:size-width *text-model-size*)
                                                  (gfg:average-char-width metrics))
                                        :height (* (gfs:size-height *text-model-size*)
                                                   (gfg:height metrics)))))
        (setf (gfw:maximum-size panel) panel-size
              (gfw:minimum-size panel) panel-size)))
    panel))

(defun set-text-scroll-params (window)
  (let* ((disp (gfw:dispatcher window))
         (panel (gfw::obtain-top-child window))
         (panel-size (gfw:size panel)))
    (gfw:with-graphics-context (gc panel)
      (let ((metrics (gfg:metrics gc (font-of (gfw:dispatcher panel))))
            (scrollbar (gfw:obtain-horizontal-scrollbar window)))
        (setf (gfw:outer-limit scrollbar) (gfs:size-width panel-size))
        (setf (gfw:thumb-position scrollbar) 0)
        (setf scrollbar (gfw:obtain-vertical-scrollbar window))
        (setf (gfw:outer-limit scrollbar) (gfs:size-height panel-size))
        (setf (gfw:thumb-position scrollbar) 0)
        (setf (gfw:step-increments disp) (gfs:make-size :width (gfg:average-char-width metrics)
                                                        :height (gfg:height metrics)))))
    (setf (slot-value disp 'gfw::viewport-origin) (gfs:make-point))
    (gfw:event-resize disp window (gfw:size window) :restored)))

(defmethod initialize-instance ((self scroll-text-panel-events) &key)
  (gfw:with-graphics-context (gc)
    (setf (font-of self) (make-instance 'gfg:font :gc gc :data *text-panel-font-data*))))

(defmethod gfw:event-dispose ((disp scroll-text-panel-events) (panel gfw:panel))
  (let ((font (font-of disp)))
    (if font
      (gfs:dispose font))
    (setf (font-of disp) nil)))

(defmethod gfw:event-paint ((disp scroll-text-panel-events) window gc rect)
  (declare (ignore window))
  (gfg:clear gc gfg:*color-white*)
  (setf (gfg:foreground-color gc) gfg:*color-black*
        (gfg:font gc) (font-of disp))
  (let* ((metrics (gfg:metrics gc (font-of disp)))
         (pnt (gfs:location rect))
         (size (gfs:size rect))
         (first-row (floor (gfs:point-y pnt) (gfg:height metrics)))
         (last-row (floor (+ (gfs:point-y pnt) (gfs:size-height size)) (gfg:height metrics)))
         (first-col (floor (gfs:point-x pnt) (gfg:average-char-width metrics)))
         (last-col (floor (+ (gfs:point-x pnt) (gfs:size-width size)) (gfg:average-char-width metrics))))
    (setf (gfs:point-x pnt) (* first-col (gfg:average-char-width metrics)))
    (loop for row from first-row upto last-row
          do (draw-text-chunk gc metrics row first-col last-col))))
               
