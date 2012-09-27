;;;;
;;;; timer.lisp
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

(defun clamp-delay-values (init-delay delay)
  "Adjust delay settings based on system-defined limits."
  ;;
  ;; SetTimer is going to impose them anyway, so might as
  ;; well make the slot values agree with reality.
  ;; On original WinXP (pre-SP1) and earlier, delay values less
  ;; than USER_TIMER_MINIMUM get set to 1ms, which MS rectified
  ;; in later releases.
  ;;
  (when (and (> init-delay 0) (< init-delay gfs::+user-timer-minimum+))
    (setf init-delay gfs::+user-timer-minimum+))
  (when (> init-delay gfs::+user-timer-maximum+)
    (setf init-delay gfs::+user-timer-maximum+))
  (when (and (> delay 0) (< delay gfs::+user-timer-minimum+))
    (setf delay gfs::+user-timer-minimum+))
  (when (> delay gfs::+user-timer-maximum+)
    (setf delay gfs::+user-timer-maximum+))
  (values init-delay delay))

(defun reset-timer-to-delay (timer delay)
  (multiple-value-bind (init-delay clamped)
      (clamp-delay-values 0 delay)
    (declare (ignore init-delay))
    (let ((tc (thread-context))
          (id (id-of timer)))
      (when (zerop id)
        (setf (slot-value timer 'id) (increment-widget-id tc))
        (put-timer tc timer))
      (if (zerop (gfs::set-timer (utility-hwnd tc) (id-of timer) clamped (cffi:null-pointer)))
        (error 'gfs:win32-error :detail "set-timer failed")))
    clamped))

;;;
;;; methods
;;;

(defmethod (setf delay-of) :around (value (self timer))
  (setf (slot-value self 'delay) (reset-timer-to-delay self value)))

(defmethod gfs:dispose ((self timer))
  (let ((tc (thread-context)))
    (delete-timer tc self)
    (gfs::kill-timer (utility-hwnd tc) (id-of self))))

(defmethod initialize-instance :after ((self timer) &key)
  (if (null (delay-of self))
    (error 'gfs:toolkit-error :detail ":delay value required"))
  (if (null (initial-delay-of self))
    (setf (slot-value self 'initial-delay) (delay-of self)))
  (multiple-value-bind (init-delay delay)
      (clamp-delay-values (initial-delay-of self) (delay-of self))
    (setf (slot-value self 'initial-delay) init-delay)
    (setf (slot-value self 'delay) delay)))

(defmethod enable ((self timer) flag)
  (if flag
    (progn
      ;; use init-delay as the elapse interval for the very first
      ;; tick; the interval will be adjusted (or the timer killed)
      ;; as part of processing the first event
      ;;
      (let ((init-delay (initial-delay-of self)))
        (if (> init-delay 0)
          (reset-timer-to-delay self init-delay)
          (setf (delay-of self) (delay-of self)))))
    (gfs:dispose self)))

(defmethod enabled-p ((self timer))
  (get-timer (thread-context) (id-of self)))
