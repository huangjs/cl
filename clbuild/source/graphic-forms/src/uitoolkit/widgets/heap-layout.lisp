;;;;
;;;; heap-layout.lisp
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
;;; helper functions
;;;

(defun obtain-top-child (window)
  (let* ((layout (layout-of window))
         (top (top-child-of layout)))
    (if top
      top
      (car (first (compute-layout layout window -1 -1))))))

;;;
;;; methods
;;;

(defmethod compute-size ((self heap-layout) (container layout-managed) width-hint height-hint)
  (cleanup-disposed-items self)
  (let ((size (gfs:make-size)))
    (mapc (lambda (item)
            (let ((kid-size (preferred-size (first item) width-hint height-hint)))
              (setf (gfs:size-width size)  (max (gfs:size-width size)
                                                (gfs:size-width kid-size))
                    (gfs:size-height size) (max (gfs:size-height size)
                                                (gfs:size-height kid-size)))))
          (data-of self))
    (incf (gfs:size-width size)  (+ (left-margin-of self) (right-margin-of self)))
    (incf (gfs:size-height size) (+ (top-margin-of self) (bottom-margin-of self)))
    size))

(defmethod compute-layout ((self heap-layout) (container layout-managed) width-hint height-hint)
  (declare (ignore width-hint height-hint))
  (cleanup-disposed-items self)
  (let ((size (client-size container))
        (sbar (if (or (typep container 'top-level) (typep container 'dialog))
                (status-bar-of container))))
    (if sbar
      (decf (gfs:size-height size) (gfs:size-height (preferred-size sbar -1 -1))))
    (let* ((horz-margin (+ (left-margin-of self) (right-margin-of self)))
           (vert-margin (+ (top-margin-of self) (bottom-margin-of self)))
           (bounds (gfs:create-rectangle :x (left-margin-of self)
                                         :y (top-margin-of self)
                                         :width (- (gfs:size-width size)
                                                   horz-margin)
                                         :height (- (gfs:size-height size)
                                                    vert-margin))))
      (mapcar (lambda (item) (cons (first item) bounds)) (data-of self)))))

(defmethod perform ((self heap-layout) (container layout-managed) width-hint height-hint)
  (if (layout-p container)
    (let ((top (top-child-of self))
          (kid-specs (compute-layout self container width-hint height-hint)))
      (let ((spec (if top
                    (find-if (lambda (x) (eql x top)) kid-specs :key #'car)
                    (progn
                      (setf top (car (first kid-specs)))
                      (first kid-specs)))))
        (if spec
          (let ((bounds (cdr spec)))
            (setf (gfs:size bounds) (gfs::clamp-size (gfs:size bounds)
                                                     (slot-value top 'min-size)
                                                     (slot-value top 'max-size)))
            (setf (cdr spec) bounds))))
      (arrange-hwnds kid-specs (lambda (item)
                                 (if (eql top item)
                                   (logior +window-pos-flags+ gfs::+swp-showwindow+)
                                   (logior +window-pos-flags+ gfs::+swp-hidewindow+)))))))

(defmethod (setf top-child-of) :after (child (self heap-layout))
  (unless (typep child 'widget)
    (error 'gfs:toolkit-error :detail "top child must be an instance of a widget subclass")))
