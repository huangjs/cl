;;;;
;;;; button.lisp
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

(defconstant +horizontal-button-text-margin+ 7)
(defconstant +vertical-button-text-margin+   5)

;;;
;;; methods
;;;

(defmethod check ((self button) flag)
  (let ((bits (if flag gfs::+bst-checked+ gfs::+bst-unchecked+)))
    (gfs::send-message (gfs:handle self) gfs::+bm-setcheck+ bits 0)))

(defmethod checked-p ((self button))
  (let ((bits (gfs::send-message (gfs:handle self) gfs::+bm-getcheck+ 0 0)))
    (= (logand bits gfs::+bst-checked+) gfs::+bst-checked+)))

(defmethod compute-style-flags ((self button) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags +default-child-style+)
        (style (style-of self)))
    (loop for sym in style
          do (cond
               ;; primary button styles
               ;;
               ((eq sym :check-box)
                  (setf std-flags (logior std-flags gfs::+bs-autocheckbox+)))
               ((eq sym :default-button)
                  (setf std-flags (logior std-flags gfs::+bs-defpushbutton+)))
               ((or (eq sym :push-button) (eq sym :cancel-button))
                  (setf std-flags (logior std-flags gfs::+bs-pushbutton+)))
               ((eq sym :radio-button)
                  (setf std-flags (logior std-flags gfs::+bs-autoradiobutton+)))
               ((eq sym :toggle-button)
                  (setf std-flags (logior std-flags gfs::+bs-autocheckbox+ gfs::+bs-pushlike+)))
               ((eq sym :tri-state)
                  (setf std-flags (logior std-flags gfs::+bs-auto3state+)))))
    (if (null style)
      (logior std-flags gfs::+bs-pushbutton+))
    (values std-flags 0)))

(defmethod initialize-instance :after ((self button) &key parent text &allow-other-keys)
  (let ((id (cond
              ((find :default-button (style-of self))
                 gfs::+idok+)
              ((find :cancel-button (style-of self))
                 gfs::+idcancel+)
              (t
                 (increment-widget-id (thread-context))))))
    (create-control self parent text gfs::+icc-standard-classes+ id)
    (if (test-native-style self gfs::+bs-defpushbutton+)
      (gfs::send-message (gfs:handle parent)
                         gfs::+dm-setdefid+
                         (cffi:pointer-address (gfs:handle self))
                         0))))

(defmethod preferred-size ((self button) width-hint height-hint)
  (let ((text-size (widget-text-size self #'text gfs::+dt-singleline+))
        (size (gfs:make-size))
        (b-width (* (border-width self) 2))
        (need-cb-size (intersection '(:check-box :radio-button :tri-state) (style-of self)))
        (cb-size (check-box-size)))
    (cond
      ((>= width-hint 0)
         (setf (gfs:size-width size) width-hint))
      (need-cb-size
         (setf (gfs:size-width size) (+ +horizontal-button-text-margin+
                                        (gfs:size-width cb-size)
                                        (gfs:size-width text-size))))
      (t
         (setf (gfs:size-width size) (+ b-width
                                        (* +horizontal-button-text-margin+ 2)
                                        (gfs:size-width text-size)))))
    (cond
      ((>= height-hint 0)
         (setf (gfs:size-height size) height-hint))
      (need-cb-size
         (setf (gfs:size-height size) (+ (* +vertical-button-text-margin+ 2)
                                         (max (gfs:size-height text-size)
                                              (gfs:size-height cb-size)))))
      (t
         (setf (gfs:size-height size) (+ b-width
                                         (* +vertical-button-text-margin+ 2)
                                         (gfs:size-height text-size)))))
    size))

(defmethod select ((self button) flag)
  (check self flag))

(defmethod selected-p ((self button))
  (checked-p self))

(defmethod text ((self button))
  (get-widget-text self))

(defmethod (setf text) (str (self button))
  (set-widget-text self str))

(defmethod text-baseline ((self button))
  (widget-text-baseline self +vertical-button-text-margin+))

(defmethod update-native-style ((self button) flags)
  (gfs::send-message (gfs:handle self) gfs::+bm-setstyle+ flags 1)
  flags)
