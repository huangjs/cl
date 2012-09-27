;;;;
;;;; test-utils.lisp
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

(defvar *child-size-1* (gfs:make-size :width 25 :height 5))
(defvar *child-size-2* (gfs:make-size :width 20 :height 10))
(defvar *child-size-3* (gfs:make-size :width 40 :height 40))

(defun make-flow-layout (kids style &optional spacing left-margin top-margin right-margin bottom-margin)
  (let ((layout (make-instance 'gfw:flow-layout
                               :style         style
                               :spacing       (or spacing       0)
                               :left-margin   (or left-margin   0)
                               :top-margin    (or top-margin    0)
                               :right-margin  (or right-margin  0)
                               :bottom-margin (or bottom-margin 0))))
    (loop for kid in kids do (gfw::append-layout-item layout kid))
    layout))

(defun make-border-layout (kids &optional left-margin top-margin right-margin bottom-margin)
  (let ((layout (make-instance 'gfw:border-layout
                               :left-margin   (or left-margin   0)
                               :top-margin    (or top-margin    0)
                               :right-margin  (or right-margin  0)
                               :bottom-margin (or bottom-margin 0))))
    (loop for kid in kids
          for region in '(:top :right :bottom :left :center)
          when kid
          do (progn
               (gfw::append-layout-item layout kid)
               (setf (gfw:layout-attribute layout kid region) t)))
    layout))

(defun validate-image (image expected-size expected-depth)
  (declare (ignore expected-depth))
  (assert-false (null image))
  (assert-false (gfs:disposed-p image))
  ;; (assert-equal expected-depth (gfg:depth image))  ; FIXME: image->data needed
  (assert-equality #'gfs:equal-size-p expected-size (gfg:size image)))

(defun validate-rects (entries expected-rects)
  (assert-equal (length expected-rects) (length entries))
  (let ((actual-rects (loop for entry in entries collect (cdr entry))))
    (mapc #'(lambda (expected actual)
              (let ((pnt-a (gfs:location actual))
                    (sz-a (gfs:size actual)))
                (assert-equal (first expected) (gfs:point-x pnt-a))
                (assert-equal (second expected) (gfs:point-y pnt-a))
                (assert-equal (third expected) (gfs:size-width sz-a))
                (assert-equal (fourth expected) (gfs:size-height sz-a))))
          expected-rects
          actual-rects)))

(defmacro define-layout-test (name width-hint height-hint
                              expected-width expected-height
                              customizer expected-rects
                              factory &rest factory-args)
  (let ((layout (gensym))
        (size (gensym))
        (dummy (gensym))
        (data (gensym)))
   `(define-test ,name
      (let* ((,layout (apply ,factory (list ,@factory-args)))
             (,dummy (if ,customizer (funcall ,customizer ,layout)))
             (,size (gfw::compute-size ,layout *mock-container* ,width-hint ,height-hint))
             (,data (gfw::compute-layout ,layout *mock-container* ,width-hint ,height-hint)))
        (assert-equal ,expected-width (gfs::size-width ,size))
        (assert-equal ,expected-height (gfs::size-height ,size))
        (validate-rects ,data ,expected-rects)))))
