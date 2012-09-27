;;;;
;;;; layout.lisp
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

(defun layout-attribute (layout thing name)
  "Return the value associated with name for thing; or NIL if no value is set."
  (let ((item-data (assoc thing (data-of layout))))
    (unless item-data
      (error 'gfs:toolkit-error :detail (format nil "~a is not managed by ~a" thing layout)))
    (getf (second item-data) name)))

(defun set-layout-attribute (layout thing name value)
  "Sets a value associated with name for thing in the specified layout."
  (let ((item-data (assoc thing (data-of layout))))
    (unless item-data
      (error 'gfs:toolkit-error :detail (format nil "~a is not managed by ~a" thing layout)))
    (setf (getf (second item-data) name) value)))

(defsetf layout-attribute set-layout-attribute)

(defun obtain-children-with-attribute (layout name)
  "Returns a list of layout entries that have the named attribute."
  (loop for pair in (data-of layout)
        when (getf (second pair) name)
        collect pair))

(defun append-layout-item (layout thing)
  "Adds thing to layout. Duplicate entries are not prevented."
  (setf (data-of layout) (nconc (data-of layout) (list (list thing nil)))))

(defun delete-layout-item (layout thing)
  "Removes thing from layout."
  (setf (data-of layout) (remove thing (data-of layout) :key #'first)))

(defun cleanup-disposed-items (layout)
  (setf (data-of layout) (remove-if #'gfs:disposed-p (data-of layout) :key #'first)))

(declaim (inline scale-coord))
(defun scale-coord (total hint orig-value)
  (if (and (> total 0) (>= hint 0))
    (floor (* (/ hint total) orig-value))
    orig-value))

(declaim (inline scale-point))
(defun scale-point (total-size hint-size orig-pnt)
  (gfs:make-point :x (scale-coord (gfs:size-width total-size)
                                  (gfs:size-width hint-size)
                                  (gfs:point-x orig-pnt))
                  :y (scale-coord (gfs:size-height total-size)
                                  (gfs:size-height hint-size)
                                  (gfs:point-y orig-pnt))))

(declaim (inline scale-size))
(defun scale-size (total-size hint-size orig-size)
  (gfs:make-size :width (scale-coord (gfs:size-width total-size)
                                     (gfs:size-width hint-size)
                                     (gfs:size-width orig-size))
                 :height (scale-coord (gfs:size-height total-size)
                                      (gfs:size-height hint-size)
                                      (gfs:size-height orig-size))))

(declaim (inline scale-rectangle))
(defun scale-rectangle (total-size hint-size orig-rect)
  (let ((pnt (gfs:location orig-rect))
        (size (gfs:size orig-rect)))
    (gfs:make-rectangle :location (scale-point total-size hint-size pnt)
                        :size (scale-size total-size hint-size size))))

(defun arrange-hwnds (kid-specs flags-func)
  (let ((hdwp (gfs::begin-defer-window-pos (length kid-specs))))
    (loop for k in kid-specs
          for rect = (cdr k)
          for widget = (car k)
          for size = (gfs:size rect)
          for pnt = (gfs:location rect)
          do (if (gfs:null-handle-p hdwp)
               (gfs::set-window-pos   (gfs:handle widget)
                                      (cffi:null-pointer)
                                      (gfs:point-x pnt)
                                      (gfs:point-y pnt)
                                      (gfs:size-width size)
                                      (gfs:size-height size)
                                      (funcall flags-func widget))
               (gfs::defer-window-pos hdwp
                                      (gfs:handle widget)
                                      (cffi:null-pointer)
                                      (gfs:point-x pnt)
                                      (gfs:point-y pnt)
                                      (gfs:size-width size)
                                      (gfs:size-height size)
                                      (funcall flags-func widget))))
    (unless (gfs:null-handle-p hdwp)
      (gfs::end-defer-window-pos hdwp))))

(defun layout-bounds (children margins)
  (multiple-value-bind (min-x min-y max-x max-y)
      (loop for entry in children
            for location = (gfs:location (cdr entry))
            for size = (gfs:size (cdr entry))
            minimizing (gfs:point-x location) into min-x
            minimizing (gfs:point-y location) into min-y
            maximizing (+ (gfs:point-x location) (gfs:size-width size)) into max-x
            maximizing (+ (gfs:point-y location) (gfs:size-height size)) into max-y
            finally (return (values min-x min-y max-x max-y)))
    (let ((location (gfs:make-point :x (- min-x (first margins))
                                    :y (- min-y (second margins))))
          (size (gfs:make-size :width (+ max-x (third margins))
                               :height (+ max-y (fourth margins)))))
      (gfs:make-rectangle :location location :size size))))

;;;
;;; methods
;;;

(defmethod initialize-instance :after ((self layout-manager)
                                       &key style margins horizontal-margins vertical-margins
                                       &allow-other-keys)
  (setf (style-of self) (if (listp style) style (list style)))
  (unless (null margins)
    (setf (left-margin-of self) margins
          (right-margin-of self) margins
          (top-margin-of self) margins
          (bottom-margin-of self) margins))
  (unless (null horizontal-margins)
    (setf (left-margin-of self) horizontal-margins
          (right-margin-of self) horizontal-margins))
  (unless (null vertical-margins)
    (setf (top-margin-of self) vertical-margins
          (bottom-margin-of self) vertical-margins)))

(defmethod (setf layout-of) :after ((layout layout-manager) (self layout-managed))
  (let ((orig-layout (layout-of self)))
    (if orig-layout
      (setf (data-of layout) (loop for item in (data-of orig-layout)
                                   when (not (gfs:disposed-p (first item)))
                                   collect item)
            (data-of orig-layout) nil)
      (if (typep self 'window)
        (setf (data-of layout) (mapchildren self (lambda (parent child)
                                                    (declare (ignore parent))
                                                    (list child nil))))))))

(defmethod perform ((self layout-manager) (container layout-managed) width-hint height-hint)
  (if (layout-p container)
    (arrange-hwnds (compute-layout self container width-hint height-hint)
                   (constantly +window-pos-flags+))))
