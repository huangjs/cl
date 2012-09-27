;;;;
;;;; flow-layout.lisp
;;;;
;;;; Copyright (C) 2006-2007, Jack D. Unrue
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
;;; This implementation attempts to maximize code re-use by handling both
;;; possible orientations with the same logic. Hence the terminology is a
;;; little confusing. Here is a quick primer:
;;;
;;; primary axis   -- the axis parallel to the layout's orientation
;;;
;;; secondary axis -- the axis orthogonal to the layout's orientation
;;;
;;; distance       -- offset from one point to the next along the primary axis
;;;
;;; extent         -- offset from one point to the next along the secondary axis
;;;

(defstruct flow-data
  (hint 0)                 ; the width or height hint passed to the layout manager
  (kid-sizes nil)          ; list of pairs of child widgets and their sizes
  (distance-total 0)       ; total (un-wrapped) widget size in primary axis
  (max-distance 0)         ; maximum widget size in primary axis
  (max-extent 0)           ; maximum widget size in secondary axis
  (last-wrap-max-extent 0) ; maximum widget size in secondary axis for previous wrap
  (next-coord 0)           ; position in primary axis where next widget goes
  (wrap-coord 0)           ; position in secondary axis where next widget wraps to
  (spacing 0)              ; layout's spacing attribute
  (distance-fn nil)        ; either #'gfs:size-width or #'gfs:size-height
  (extent-fn nil)          ; opposite of distance-fn
  (limit-margin-fn nil)    ; either #'bottom-margin-of or #'right-margin-of
  (start-margin-fn nil)    ; either #'top-margin-of or #'left-margin-of
  (current nil))           ; flow data list

;;;
;;; helper functions
;;;

(defun init-flow-data (layout visible items width-hint height-hint)
  (let ((state (if (find :vertical (style-of layout))
                 (make-flow-data :hint height-hint
                                 :next-coord (top-margin-of layout)
                                 :wrap-coord (left-margin-of layout)
                                 :spacing (spacing-of layout)
                                 :distance-fn #'gfs:size-height
                                 :extent-fn #'gfs:size-width
                                 :limit-margin-fn #'bottom-margin-of
                                 :start-margin-fn #'top-margin-of)
                 (make-flow-data :hint width-hint
                                 :next-coord (left-margin-of layout)
                                 :wrap-coord (top-margin-of layout)
                                 :spacing (spacing-of layout)
                                 :distance-fn #'gfs:size-width
                                 :extent-fn #'gfs:size-height
                                 :limit-margin-fn #'right-margin-of
                                 :start-margin-fn #'left-margin-of))))
    (loop for item in items
          for kid = (first item)
          when (or (visible-p kid) (not visible))
          do (let* ((size (preferred-size kid -1 -1))
                    (dist (funcall (flow-data-distance-fn state) size))
                    (extent (funcall (flow-data-extent-fn state) size)))
               (incf (flow-data-distance-total state) dist)
               (if (< (flow-data-max-distance state) dist)
                 (setf (flow-data-max-distance state) dist))
               (if (< (flow-data-max-extent state) extent)
                 (setf (flow-data-max-extent state) extent))
               (push (list kid size) (flow-data-kid-sizes state))))
    (setf (flow-data-kid-sizes state) (reverse (flow-data-kid-sizes state)))
    state))

(defun wrap-needed-p (state layout kid-size)
  (and (>= (flow-data-hint state) 0)
       (> (+ (flow-data-next-coord state)
             (funcall (flow-data-distance-fn state) kid-size)
             (funcall (flow-data-limit-margin-fn state) layout))
          (flow-data-hint state))))

(defun wrap-flow (state layout)
  (let ((curr-flow (flow-data-current state)))
    (setf (flow-data-current state) nil)
    (setf (flow-data-next-coord state) (funcall (flow-data-start-margin-fn state) layout))
    (incf (flow-data-wrap-coord state) (+ (flow-data-last-wrap-max-extent state)
                                          (flow-data-spacing state)))
    (setf (flow-data-last-wrap-max-extent state) 0)
    (reverse curr-flow)))

(defun new-flow-element (state layout kid kid-size)
  (let ((pnt (gfs:make-point))
        (vertical (find :vertical (style-of layout)))
        (extent (funcall (flow-data-extent-fn state) kid-size)))
    (if vertical
      (setf (gfs:point-x pnt) (flow-data-wrap-coord state)
            (gfs:point-y pnt) (flow-data-next-coord state))
      (setf (gfs:point-x pnt) (flow-data-next-coord state)
            (gfs:point-y pnt) (flow-data-wrap-coord state)))
    (incf (flow-data-next-coord state) (+ (funcall (flow-data-distance-fn state) kid-size)
                                          (flow-data-spacing state)))
    (if (> extent (flow-data-last-wrap-max-extent state))
        (setf (flow-data-last-wrap-max-extent state) extent))
    (cons kid (gfs:make-rectangle :size kid-size :location pnt))))

;;;
;;; methods
;;;

(defmethod compute-size ((self flow-layout) (container layout-managed) width-hint height-hint)
  (let ((data (compute-layout self container width-hint height-hint)))
    (gfs:size (layout-bounds data
                             (list (left-margin-of self)
                                   (top-margin-of self)
                                   (right-margin-of self)
                                   (bottom-margin-of self))))))

(defmethod compute-layout ((self flow-layout) (container layout-managed) width-hint height-hint)
  (cleanup-disposed-items self)
  (let ((flows nil)
        (normal (find :normalize (style-of self)))
        (vertical (find :vertical (style-of self)))
        (state (init-flow-data self (visible-p container) (data-of self) width-hint height-hint)))
    (loop with wrap = (find :wrap (style-of self))
          for (kid kid-size) in (flow-data-kid-sizes state)
          do (cond
               ((and normal vertical)
                  (setf (gfs:size-width kid-size) (flow-data-max-extent state)
                        (gfs:size-height kid-size) (flow-data-max-distance state)))
               ((and normal (not vertical))
                  (setf (gfs:size-width kid-size) (flow-data-max-distance state)
                        (gfs:size-height kid-size) (flow-data-max-extent state))))
             (if (and wrap
                      (flow-data-current state)
                      (wrap-needed-p state self kid-size))
                 (setf flows (append flows (wrap-flow state self))))
             (push (new-flow-element state self kid kid-size) (flow-data-current state)))
    (if (flow-data-current state)
      (setf flows (append flows (wrap-flow state self))))
    flows))


(defmethod initialize-instance :after ((self flow-layout) &key)
  (unless (intersection (style-of self) '(:horizontal :vertical))
    (setf (style-of self) (list :horizontal))))
