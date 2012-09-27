;;;;
;;;; slider.lisp
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

(defun sl-auto-ticks-flags (orig-flags)
  (logior (logand orig-flags (lognot gfs::+tbs-noticks+)) gfs::+tbs-autoticks+))

(defun sl-no-ticks-flags (orig-flags)
  (setf orig-flags (logand orig-flags (lognot (logior gfs::+tbs-top+ gfs::+tbs-left+))))
  (logior (logand orig-flags (lognot gfs::+tbs-autoticks+)) gfs::+tbs-noticks+))

(defun sl-ticks-before-flags (orig-flags)
  (logior orig-flags gfs::+tbs-top+))

(defun sl-ticks-both-flags (orig-flags)
  (setf orig-flags (logand orig-flags (lognot gfs::+tbs-top+)))
  (logior orig-flags gfs::+tbs-both+))

(defun sl-horizontal-flags (orig-flags)
  (logand orig-flags (lognot gfs::+tbs-vert+)))

(defun sl-sel-range-flags (orig-flags)
  (logior orig-flags gfs::+tbs-enableselrange+))

(defun sl-tooltip-flags (orig-flags)
  (logior orig-flags gfs::+tbs-tooltips+))

(defun sl-vertical-flags (orig-flags)
  (logior orig-flags gfs::+tbs-vert+))

(defun sl-border-flags (orig-flags)
  (logior orig-flags gfs::+ws-border+))

;;;
;;; methods
;;;

(defmethod compute-style-flags ((self slider) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags +default-child-style+)
        (style (style-of self)))
    (loop for sym in style
          do (ecase sym
               ;; primary slider styles
               ;;
               (:horizontal       (setf std-flags  (sl-horizontal-flags std-flags)))
               (:vertical         (setf std-flags  (sl-vertical-flags std-flags)))
               (:auto-ticks       (setf std-flags  (sl-auto-ticks-flags std-flags)))
               (:no-ticks         (setf std-flags  (sl-no-ticks-flags std-flags)))

               ;; styles that can be combined
               ;;
               (:border            (setf std-flags (sl-border-flags std-flags)))
               (:ticks-after)      ; will be handled below
               (:ticks-before      (setf std-flags (sl-ticks-before-flags std-flags)))
               (:tooltip           (setf std-flags (sl-tooltip-flags std-flags)))))
    (if (and (find :ticks-before style) (find :ticks-after style))
      (setf std-flags (sl-ticks-both-flags std-flags)))
    (values std-flags 0)))

(defmethod initialize-instance :after ((self slider) &key outer-limit parent &allow-other-keys)
  (create-control self parent "" gfs::+icc-win95-classes+)
  (setf (gfg:background-color self)
        (gfg:rgb->color (gfs::get-sys-color gfs::+color-btnface+)))
  (if outer-limit
    (setf (outer-limit self) outer-limit)))

(defmethod inner-limits ((self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((hwnd (gfs:handle self)))
    (gfs:make-span :start (gfs::send-message hwnd gfs::+tbm-getselstart+ 0 0)
                   :end   (gfs::send-message hwnd gfs::+tbm-getselend+   0 0))))

(defmethod (setf inner-limits) (limits (self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (test-native-style self gfs::+tbs-enableselrange+)
    (update-native-style self (logior (get-native-style self) gfs::+tbs-enableselrange+)))
  (let ((start (gfs:span-start limits))
        (end (gfs:span-end limits)))
    (if (or (< start 0) (< end 0))
      (error 'gfs:toolkit-error :detail "negative slider thumb limit"))
    (gfs::send-message (gfs:handle self)
                       gfs::+tbm-setsel+
                       1
                       (if (<= start end)
                         (gfs::make-lparam start end)
                         (gfs::make-lparam end start))))
  limits)

(defmethod outer-limit ((self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((hwnd (gfs:handle self)))
    (gfs::send-message hwnd gfs::+tbm-getrangemax+ 0 0)))

(defmethod (setf outer-limit) (limit (self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (if (< limit 0)
    (error 'gfs:toolkit-error :detail "negative slider thumb limit"))
  (gfs::send-message (gfs:handle self) gfs::+tbm-setrange+ 1 (gfs::make-lparam 0 limit))
  limit)

(defmethod page-increment ((self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::send-message (gfs:handle self) gfs::+tbm-getpagesize+ 0 0))

(defmethod (setf page-increment) (amount (self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (if (< amount 0)
    (error 'gfs:toolkit-error :detail "negative slider page increment"))
  (if (< amount (step-increment self))
    (warn 'gfs::toolkit-warning :detail "slider page increment less than step increment"))
  (gfs::send-message (gfs:handle self) gfs::+tbm-setpagesize+ 0 amount)
  amount)

(defmethod preferred-size ((self slider) width-hint height-hint)
  (let* ((b-width (* (border-width self) 2))
         (limit (outer-limit self))
         (size (gfs:make-size)))
    (if (find :vertical (style-of self))
      (setf (gfs:size-width size)  (floor (* (vertical-scrollbar-width) 5) 2)
            (gfs:size-height size) (+ (* 10 limit) b-width))
      (setf (gfs:size-width size)  (+ (* 10 limit) b-width)
            (gfs:size-height size) (floor (* (horizontal-scrollbar-height) 5) 2)))
    (if (>= width-hint 0)
      (setf (gfs:size-width size) width-hint))
    (if (>= height-hint 0)
      (setf (gfs:size-height size) height-hint))
    size))

(defmethod step-increment ((self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::send-message (gfs:handle self) gfs::+tbm-getlinesize+ 0 0))

(defmethod (setf step-increment) (amount (self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (if (< amount 0)
    (error 'gfs:toolkit-error :detail "negative slider step increment"))
  (if (> amount (page-increment self))
    (warn 'gfs::toolkit-warning :detail "slider step increment greater than page increment"))
  (gfs::send-message (gfs:handle self) gfs::+tbm-setlinesize+ 0 amount)
  amount)

(defmethod thumb-position ((self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::send-message (gfs:handle self) gfs::+tbm-getpos+ 0 0))

(defmethod (setf thumb-position) (pos (self slider))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::send-message (gfs:handle self) gfs::+tbm-setpos+ 1 pos)
  (gfs::send-message (gfs:handle self) gfs::+tbm-getpos+ 0 0)) ; might have been adjusted
