;;;;
;;;; display.lisp
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

(cffi:defcallback (display-visitor :cconv :stdcall) gfs::BOOL
    ((hmonitor :pointer) (hdc :pointer) (monitorrect :pointer) (data gfs::LPARAM))
  (declare (ignore hdc monitorrect))
  (call-display-visitor-func (thread-context) hmonitor data)
  1)

(defun query-display-info (hmonitor)
  (let ((info nil))
    (cffi:with-foreign-object (mi-ptr 'gfs::monitorinfoex)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::flags) mi-ptr gfs::monitorinfoex)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::monitorinfoex))
        (if (zerop (gfs::get-monitor-info hmonitor mi-ptr))
          (error 'gfs:win32-warning :detail "get-monitor-info failed"))
        (push (= (logand gfs::flags gfs::+monitorinfoof-primary+) gfs::+monitorinfoof-primary+) info)
        (let ((str-ptr (cffi:foreign-slot-pointer mi-ptr 'gfs::monitorinfoex 'gfs::device)))
          (push (cffi:foreign-string-to-lisp str-ptr (1- gfs::+cchdevicename+)) info))
        (let ((rect-ptr (cffi:foreign-slot-pointer mi-ptr 'gfs::monitorinfoex 'gfs::monitor)))
          (cffi:with-foreign-slots ((gfs::left gfs::top gfs::right gfs::bottom)
                                    rect-ptr gfs::rect)
            (push (gfs:make-size :width (- gfs::right gfs::left) :height (- gfs::bottom  gfs::top))
                  info)))
        (let ((rect-ptr (cffi:foreign-slot-pointer mi-ptr 'gfs::monitorinfoex 'gfs::work)))
          (cffi:with-foreign-slots ((gfs::left gfs::top gfs::right gfs::bottom)
                                    rect-ptr gfs::rect)
            (push (gfs:make-size :width (- gfs::right gfs::left) :height (- gfs::bottom  gfs::top))
                  info)))))
    (reverse info)))

(defun mapdisplays (func)
  ;;
  ;; func should expect two parameters:
  ;;  display handle
  ;;  flag data
  ;;
  (let ((tc (thread-context)))
    (setf (display-visitor-func tc) func)
    (unwind-protect
        (gfs::enum-display-monitors (cffi:null-pointer)
                                    (cffi:null-pointer)
                                    (cffi:callback display-visitor) 0)
      (setf (display-visitor-func tc) nil))
    (let ((tmp (reverse (display-visitor-results tc))))
      (setf (display-visitor-results tc) nil)
      tmp)))

(defun obtain-displays ()
  (mapdisplays (lambda (hmonitor data)
                 (declare (ignore data))
                 (push (make-instance 'display :handle hmonitor)
                       (display-visitor-results (thread-context))))))

(declaim (inline obtain-primary-display))
(defun obtain-primary-display ()
  ;; In http://blogs.msdn.com/oldnewthing/archive/2007/08/09/4300545.aspx
  ;; Raymond Chen recommends the following technique for obtaining the
  ;; primary display.
  ;;
  (make-instance 'display
                 :handle (gfs::monitor-from-point 0 0 gfs::+monitor-defaulttoprimary+)))

(cffi:defcallback (top-level-window-visitor :cconv :stdcall) gfs::BOOL
    ((hwnd :pointer) (lparam gfs::LPARAM))
  (declare (ignore lparam))
  (let* ((tc (thread-context))
         (win (get-widget tc hwnd)))
    (unless (null win)
      (call-top-level-visitor-func tc win)))
  1)

(defun maptoplevels (func)
  ;;
  ;; func should expect one parameter:
  ;;  top-level window
  ;;
  (let ((tc (thread-context)))
    (setf (top-level-visitor-func tc) func)
    (unwind-protect
        (gfs::enum-thread-windows (gfs::get-window-thread-process-id (utility-hwnd tc) (cffi:null-pointer))
                                  (cffi:callback top-level-window-visitor)
                                  0)
      (setf (top-level-visitor-func tc) nil))
    (let ((tmp (reverse (top-level-visitor-results tc))))
      (setf (top-level-visitor-results tc) nil)
      tmp)))

;;;
;;; methods
;;;

(defmethod client-size ((self display))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (primary name size client-size) (query-display-info (gfs:handle self))
    (declare (ignore primary name size))
    client-size))

(defmethod gfs:dispose ((self display))
  (setf (slot-value self 'gfs:handle) nil))

(defun primary-p (self)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (primary name size client-size) (query-display-info (gfs:handle self))
    (declare (ignore name size client-size))
    primary))

(defmethod size ((self display))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (primary name size client-size) (query-display-info (gfs:handle self))
    (declare (ignore primary name client-size))
    size))

(defmethod text ((self display))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (primary name size client-size) (query-display-info (gfs:handle self))
    (declare (ignore primary size client-size))
    name))
