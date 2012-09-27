;;;;
;;;; border-layout.lisp
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
;;; helpers
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct borders layout hint-size inside-size outer-size
                     pref-top-height pref-left-width pref-right-width pref-bottom-height
                     center-widget top-widget left-widget bottom-widget right-widget
                     center-rect top-rect left-rect bottom-rect right-rect))

(defun map-border-rects (data map-func)
  (loop for region in '(center top left bottom right)
        for sym = (symbol-name region)
        for widget-acc = (find-symbol (concatenate 'string "BORDERS-" sym "-WIDGET") :gfw)
        for rect-acc = (find-symbol (concatenate 'string "BORDERS-" sym "-RECT") :gfw)
        for widget = (funcall widget-acc data)
        when widget
        collect (funcall map-func widget (funcall rect-acc data))))

(defun init-borders (layout width-hint height-hint)
  (let* ((data (make-borders
                 :layout layout
                 :hint-size (gfs:make-size :width width-hint
                                           :height height-hint)
                 :center-widget (first (first (obtain-children-with-attribute layout :center)))
                 :top-widget (first (first (obtain-children-with-attribute layout :top)))
                 :left-widget (first (first (obtain-children-with-attribute layout :left)))
                 :bottom-widget (first (first (obtain-children-with-attribute layout :bottom)))
                 :right-widget (first (first (obtain-children-with-attribute layout :right)))))
         (c-size (if (borders-center-widget data)
                   (preferred-size (borders-center-widget data) -1 -1)
                   (gfs:size *empty-rect*)))
         (t-size (if (borders-top-widget data)
                   (preferred-size (borders-top-widget data) -1 -1)
                   (gfs:size *empty-rect*)))
         (l-size (if (borders-left-widget data)
                   (preferred-size (borders-left-widget data) -1 -1)
                   (gfs:size *empty-rect*)))
         (b-size (if (borders-bottom-widget data)
                   (preferred-size (borders-bottom-widget data) -1 -1)
                   (gfs:size *empty-rect*)))
         (r-size (if (borders-right-widget data)
                   (preferred-size (borders-right-widget data) -1 -1)
                   (gfs:size *empty-rect*))))
    (setf (borders-pref-top-height data)    (gfs:size-height t-size)
          (borders-pref-left-width data)    (gfs:size-width l-size)
          (borders-pref-right-width data)   (gfs:size-width r-size)
          (borders-pref-bottom-height data) (gfs:size-height b-size))
    (setf (borders-inside-size data)
          (gfs:make-size :width (max (gfs:size-width c-size)
                                     (- (gfs:size-width t-size)
                                        (gfs:size-width l-size)
                                        (gfs:size-width r-size))
                                     (- (gfs:size-width b-size)
                                        (gfs:size-width l-size)
                                        (gfs:size-width r-size)))
                         :height (max (gfs:size-height l-size)
                                      (gfs:size-height c-size)
                                      (gfs:size-height r-size))))
    (setf (borders-outer-size data)
          (gfs:make-size :width (+ (max (gfs:size-width t-size)
                                        (gfs:size-width b-size)
                                        (+ (gfs:size-width l-size)
                                           (gfs:size-width c-size)
                                           (gfs:size-width r-size)))
                                   (left-margin-of layout)
                                   (right-margin-of layout))
                         :height (+ (gfs:size-height t-size)
                                    (gfs:size-height (borders-inside-size data))
                                    (gfs:size-height b-size)
                                    (top-margin-of layout)
                                    (bottom-margin-of layout))))
    data))

(defun top-border-rect (data)
  (unless (borders-top-widget data)
    (return-from top-border-rect *empty-rect*))
  (or (borders-top-rect data)
      (setf (borders-top-rect data)
            (let ((layout (borders-layout data))
                  (size (borders-outer-size data)))
              (gfs:create-rectangle :x (left-margin-of layout)
                                    :y (top-margin-of layout)
                                    :width (- (gfs:size-width size)
                                              (+ (left-margin-of layout)
                                                 (right-margin-of layout)))
                                    :height (borders-pref-top-height data))))))

(defun bottom-border-rect (data)
  (unless (borders-bottom-widget data)
    (return-from bottom-border-rect *empty-rect*))
  (or (borders-bottom-rect data)
      (setf (borders-bottom-rect data)
            (let ((layout (borders-layout data))
                  (size (borders-outer-size data)))
              (gfs:create-rectangle :x (left-margin-of layout)
                                    :y (- (gfs:size-height size)
                                          (borders-pref-bottom-height data)
                                          (bottom-margin-of layout))
                                    :width (- (gfs:size-width size)
                                              (+ (left-margin-of layout)
                                                 (right-margin-of layout)))
                                    :height (borders-pref-bottom-height data))))))

(defun left-border-rect (data)
  (unless (borders-left-widget data)
    (return-from left-border-rect *empty-rect*))
  (or (borders-left-rect data)
      (let ((layout (borders-layout data)))
        (setf (borders-left-rect data)
              (gfs:create-rectangle :x (left-margin-of layout)
                                    :y (+ (top-margin-of layout)
                                          (gfs:size-height (gfs:size (top-border-rect data))))
                                    :width (borders-pref-left-width data)
                                    :height (gfs:size-height (borders-inside-size data)))))))

(defun right-border-rect (data)
  (unless (borders-right-widget data)
    (return-from right-border-rect *empty-rect*))
  (or (borders-right-rect data)
      (let ((layout (borders-layout data)))
        (setf (borders-right-rect data)
              (gfs:create-rectangle :x (+ (left-margin-of layout)
                                          (gfs:size-width (gfs:size (left-border-rect data)))
                                          (gfs:size-width (gfs:size (center-border-rect data))))
                                    :y (+ (top-margin-of layout)
                                          (gfs:size-height (gfs:size (top-border-rect data))))
                                    :width (borders-pref-right-width data)
                                    :height (gfs:size-height (borders-inside-size data)))))))

(defun center-border-rect (data)
  (unless (borders-center-widget data)
    (return-from center-border-rect *empty-rect*))
  (or (borders-center-rect data)
      (let ((layout (borders-layout data))
            (size (borders-inside-size data)))
        (setf (borders-center-rect data)
              (gfs:create-rectangle :x (+ (left-margin-of layout)
                                          (gfs:size-width (gfs:size (left-border-rect data))))
                                    :y (+ (top-margin-of layout)
                                          (gfs:size-height (gfs:size (top-border-rect data))))
                                    :width (gfs:size-width size)
                                    :height (gfs:size-height size))))))

;;;
;;; methods
;;;

(defmethod compute-size ((self border-layout) (container layout-managed) width-hint height-hint)
  (cleanup-disposed-items self)
  (let ((size (borders-outer-size (init-borders self width-hint height-hint))))
    (if (>= width-hint 0)
      (setf (gfs:size-width size) width-hint))
    (if (>= height-hint 0)
      (setf (gfs:size-height size) height-hint))
    size))

(defmethod compute-layout ((self border-layout) (container layout-managed) width-hint height-hint)
  (cleanup-disposed-items self)
  (let ((data (init-borders self width-hint height-hint)))
    (loop for func in (list #'top-border-rect #'bottom-border-rect
                            #'left-border-rect #'right-border-rect
                            #'center-border-rect)
          do (funcall func data))
    (if (or (>= width-hint 0) (>= height-hint 0))
      (let ((total-size (borders-outer-size data))
            (hint-size (gfs:make-size :width width-hint :height height-hint)))
        (map-border-rects data
                          (lambda (widget rect)
                            (declare (ignore widget))
                            (let ((pnt (gfs:location rect))
                                  (size (gfs:size rect)))
                              (setf (gfs:location rect) (scale-point total-size hint-size pnt)
                                    (gfs:size rect) (scale-size total-size hint-size size)))))))
    (map-border-rects data #'cons)))
