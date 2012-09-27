;;;;
;;;; scrolling-helper.lisp
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
;;; helper functions
;;;

(defun clamp-scroll-pos (pos total-steps page-size)
  (setf pos (min pos (- total-steps page-size)))
  (max pos 0))

(defun update-scrollbar (scrollbar step-size detail)
  (let ((page-size (page-increment scrollbar))
        (limit (outer-limit scrollbar))
        (curr-pos (thumb-position scrollbar)))
    (let ((new-pos (case detail
                     (:start          0)
                     (:end            limit)
                     (:step-back      (- curr-pos step-size))
                     (:step-forward   (+ curr-pos step-size))
                     (:page-back      (- curr-pos page-size))
                     (:page-forward   (+ curr-pos page-size))
                     (:thumb-position curr-pos)
                     (:thumb-track    (thumb-track-position scrollbar))
                     (otherwise       curr-pos))))
      (setf new-pos (clamp-scroll-pos new-pos limit page-size))
      (setf (thumb-position scrollbar) new-pos)
      new-pos)))

(defun update-scrolling-state (window axis &optional detail)
  (unless axis
    (return-from update-scrolling-state nil))
  (unless detail
    (setf detail :thumb-position))
  (let ((disp (dispatcher window))
        (hscrollbar (obtain-horizontal-scrollbar window))
        (vscrollbar (obtain-vertical-scrollbar window)))
    (let ((child (obtain-top-child window))
          (origin (slot-value disp 'viewport-origin))
          (h-step (gfs:size-width (step-increments disp)))
          (v-step (gfs:size-height (step-increments disp)))
          (new-hpos 0)
          (new-vpos 0))
      (cond
        ((eql axis :horizontal)
           (setf new-hpos (update-scrollbar hscrollbar h-step detail))
           (setf new-vpos (thumb-position vscrollbar)))
        ((eql axis :vertical)
           (setf new-hpos (thumb-position hscrollbar))
           (setf new-vpos (update-scrollbar vscrollbar v-step detail)))
        ((eql axis :both)
           (setf new-hpos (update-scrollbar hscrollbar h-step detail))
           (setf new-vpos (update-scrollbar vscrollbar v-step detail))))
      (let ((new-x (* (floor new-hpos h-step) h-step))
            (new-y (* (floor new-vpos v-step) v-step)))
        (scroll child (- (gfs:point-x origin) new-x) (- (gfs:point-y origin) new-y) nil 0)
        (setf (gfs:point-x origin) new-x)
        (setf (gfs:point-y origin) new-y))))
  detail)

(defun validate-step-values (amounts)
  (if (or (<= (gfs:size-width amounts) 0) (<= (gfs:size-height amounts) 0))
    (error 'gfs:toolkit-error :detail "invalid step increment")))

(defun update-scrollbar-page-sizes (window)
  (setf (page-increment (obtain-vertical-scrollbar window))
        (gfs:size-height (client-size window)))
  (setf (page-increment (obtain-horizontal-scrollbar window))
        (gfs:size-width (client-size window)))) ; recalculate client size on purpose

(defun update-viewport-origin-for-resize (window)
  (let* ((top (obtain-top-child window))
         (viewport-size (client-size window))
         (hscrollbar (obtain-horizontal-scrollbar window))
         (vscrollbar (obtain-vertical-scrollbar window))
         (origin (slot-value (dispatcher window) 'viewport-origin))
         (saved-x (gfs:point-x origin))
         (saved-y (gfs:point-y origin))
         (delta-x (- (+ (gfs:size-width viewport-size) saved-x)
                     (outer-limit hscrollbar)))
         (delta-y (- (+ (gfs:size-height viewport-size) saved-y)
                     (outer-limit vscrollbar))))
    (if (and (> delta-x 0) (> saved-x 0))
      (setf (gfs:point-x origin) (max 0 (- saved-x delta-x)))
      (setf delta-x 0))
    (if (and (> delta-y 0) (> saved-y 0))
      (setf (gfs:point-y origin) (max 0 (- saved-y delta-y)))
      (setf delta-y 0))
    (if (or (and (zerop (gfs:point-x origin)) (/= saved-x 0))
            (and (zerop (gfs:point-y origin)) (/= saved-y 0)))
      (progn
        (redraw top)
        (update top))
      (scroll top delta-x delta-y nil 0))
    origin))

;;;
;;; methods
;;;

(defmethod event-pre-resize ((disp scrolling-helper) (window window) rect type)
  (let ((h-step (gfs:size-width (step-increments disp)))
        (v-step (gfs:size-height (step-increments disp)))
        (outer-size (gfw:size window))
        (client-size (gfw:client-size window))
        (pnt (gfs:location rect))
        (size (gfs:size rect)))
    (when (/= h-step 1)
      (let* ((width-diff (- (gfs:size-width outer-size) (gfs:size-width client-size)))
             (amount (+ (* (floor (- (gfs:size-width size) width-diff) h-step) h-step)
                        width-diff)))
        (if (find type '(:bottom-left :left :top-left))
          (decf (gfs:point-x pnt) (- amount (gfs:size-width size))))
        (setf (gfs:size-width size) amount)))
    (when (/= v-step 1)
      (let* ((height-diff (- (gfs:size-height outer-size) (gfs:size-height client-size)))
             (amount (+ (* (floor (- (gfs:size-height size) height-diff) v-step) v-step)
                        height-diff)))
        (if (find type '(:top-left :top :top-right))
          (decf (gfs:point-y pnt) (- amount (gfs:size-height size))))
        (setf (gfs:size-height size) amount)))
    (setf (gfs:size rect) size)))

(defmethod event-resize ((disp scrolling-helper) (window window) size type)
  (declare (ignore size type))
  (call-next-method)
  (when (typep (layout-of window) 'heap-layout)
    (update-scrollbar-page-sizes window)
    (update-viewport-origin-for-resize window)))

(defmethod event-scroll ((disp scrolling-helper) (window window) axis detail)
  (declare (ignore disp))
  (when (typep (layout-of window) 'heap-layout)
    (update-scrolling-state window axis detail)))

(defmethod initialize-instance :after ((self scrolling-helper) &key)
  (validate-step-values (step-increments self)))

(defmethod print-object ((self scrolling-helper) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "horizontal policy: ~a " (horizontal-policy-of self))
    (format stream "vertical policy: ~a "   (vertical-policy-of self))
    (format stream "step increments: ~a"    (step-increments self))))

(defmethod (setf step-increment) :after (amounts (self scrolling-helper))
  (validate-step-values amounts)
  (setf (slot-value self 'step-increment) (gfs:copy-size amounts)))
