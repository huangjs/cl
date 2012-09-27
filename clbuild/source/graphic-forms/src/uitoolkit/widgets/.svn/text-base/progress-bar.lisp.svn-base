;;;;
;;;; progress-bar.lisp
;;;;
;;;; Copyright (C) 2007, Jack D. Unrue
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

(declaim (inline pb-get-pos))
(defun pb-get-pos (p-bar)
  "Returns the current position of a progress bar."
  (gfs::send-message (gfs:handle p-bar) gfs::+pbm-getpos+ 0 0))

(defun pb-get-range (p-bar)
  "Returns the range of a progress bar."
  (cffi:with-foreign-object (r-ptr 'gfs::pbrange)
    (gfs::send-message (gfs:handle p-bar) gfs::+pbm-getrange+ 0 (cffi:pointer-address r-ptr))
    (cffi:with-foreign-slots ((gfs::low gfs::high) r-ptr gfs::pbrange)
      (gfs:make-span :start gfs::low :end gfs::high))))

(declaim (inline pb-get-step))
(defun pb-get-step (p-bar)
  "Returns the step increment for a progress bar."
  (gfs::send-message (gfs:handle p-bar) gfs::+pbm-getstep+ 0 0))

(declaim (inline pb-horz-flags))
(defun pb-horz-flags (flags)
  (logand flags (lognot gfs::+pbs-vertical+)))
  
(declaim (inline pb-set-pos-absolute))
(defun pb-set-pos-absolute (p-bar pos)
  "Sets the absolute position of a progress bar and redraws it; returns the previous position."
  (gfs::send-message (gfs:handle p-bar) gfs::+pbm-setpos+ (logand pos #xFFFF) 0))

(declaim (inline pb-set-pos-delta))
(defun pb-set-pos-delta (p-bar delta)
  "Updates the position of a progress bar by delta and redraws it; returns the previous position."
  (gfs::send-message (gfs:handle p-bar) gfs::+pbm-deltapos+ (logand delta #xFFFF) 0))

(defun pb-set-range (p-bar span)
  "Sets the range of a progress bar; returns the previous range."
  (let ((result (gfs::send-message (gfs:handle p-bar)
                                   gfs::+pbm-setrange32+
                                   (logand (gfs:span-start span) #xFFFFFFFF)
                                   (logand (gfs:span-end span) #xFFFFFFFF))))
    (gfs:make-span :start (gfs::lparam-low-word result)
                   :end (gfs::lparam-high-word result))))

(declaim (inline pb-set-step))
(defun pb-set-step (p-bar increment)
  "Sets the step increment for a progress bar; returns the previous increment."
  (gfs::send-message (gfs:handle p-bar) gfs::+pbm-setstep+ (logand increment #xFFFF) 0))

(declaim (inline pb-smooth-flags))
(defun pb-smooth-flags (flags)
  (logior flags gfs::+pbs-smooth+))

(declaim (inline pb-stepit))
(defun pb-stepit (p-bar)
  "Advances the progress bar's position by its step increment and redraws it; returns the previous position."
  (gfs::send-message (gfs:handle p-bar) gfs::+pbm-stepit+ 0 0))

(declaim (inline pb-vert-flags))
(defun pb-vert-flags (flags)
  (logior flags gfs::+pbs-vertical+))

;;;
;;; methods
;;;

(defmethod bar-position ((p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-get-pos p-bar))

(defmethod (setf bar-position) (pos (p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-set-pos-absolute p-bar pos))

(defmethod compute-style-flags ((p-bar progress-bar) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags +default-child-style+)
        (style (style-of p-bar)))
    (loop for sym in style
          do (ecase sym
               ;; primary progress-bar styles
               ;;
               (:horizontal (setf std-flags (pb-horz-flags std-flags)))
               (:vertical   (setf std-flags (pb-vert-flags std-flags)))

               ;; styles that can be combined
               ;;
               (:smooth     (setf std-flags (pb-smooth-flags std-flags)))))
    (values std-flags 0)))

(defmethod initialize-instance :after ((p-bar progress-bar) &key parent &allow-other-keys)
  (create-control p-bar parent "" gfs::+icc-win95-classes+))

(defmethod inner-limits ((p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-get-range p-bar))

(defmethod (setf inner-limits) (limits (p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-set-range p-bar limits))

(defmethod preferred-size ((p-bar progress-bar) width-hint height-hint)
  (let ((size (gfs:make-size :width width-hint :height height-hint))
        (b-width (* (border-width p-bar) 2)))
    (if (<= width-hint 0)
      (setf (gfs:size-width size) +default-widget-width+))
    (incf (gfs:size-width size) b-width)
    (if (<= height-hint 0)
      (setf (gfs:size-height size)
            (floor (* (gfs::get-system-metrics gfs::+sm-cyvscroll+) 3) 4)))
    (incf (gfs:size-height size) b-width)
    size))

(defmethod step ((p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-stepit p-bar))

(defmethod step-increment ((p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-get-step p-bar))

(defmethod (setf step-increment) (increment (p-bar progress-bar))
  (if (gfs:disposed-p p-bar)
      (error 'gfs:disposed-error))
  (pb-set-step p-bar increment))
