;;;;
;;;; control.lisp
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

(defun initialize-comctl-classes (icc-flags)
  (cffi:with-foreign-object (ic-ptr 'gfs::initcommoncontrolsex)
    (cffi:with-foreign-slots ((gfs::size gfs::icc) ic-ptr gfs::initcommoncontrolsex)
      (setf gfs::size (cffi:foreign-type-size 'gfs::initcommoncontrolsex)
            gfs::icc icc-flags))
    (if (and (zerop (gfs::init-common-controls ic-ptr)) (/= (gfs::get-last-error) 0))
      (warn 'gfs:win32-warning :detail "init-common-controls failed"))))

(defun create-control (ctrl parent text icc-flags &optional id extra-data)
  (initialize-comctl-classes icc-flags)
  (multiple-value-bind (std-style ex-style)
      (compute-style-flags ctrl extra-data)
    (let ((hwnd (create-window (system-classname-of ctrl)
                               (or text " ")
                               (gfs:handle parent)
                               std-style
                               ex-style
                               (or id (increment-widget-id (thread-context))))))
      (setf (slot-value ctrl 'gfs:handle) hwnd)
      (subclass-wndproc hwnd)
      (put-widget (thread-context) ctrl)
      (let ((hfont (gfs::get-stock-object gfs::+default-gui-font+)))
        (unless (gfs:null-handle-p hfont)
          (gfs::send-message hwnd gfs::+wm-setfont+ (cffi:pointer-address hfont) 0)))
      ;; FIXME: this is a temporary hack to allow layout management testing;
      ;; it won't work if virtual containers like group are implemented.
      ;;
      (when (and parent (layout-of parent) (not (typep ctrl 'status-bar)))
        (append-layout-item (layout-of parent) ctrl))
      hwnd)))

;;;
;;; methods
;;;

(defmethod gfg:background-color :before ((ctrl control))
  (if (gfs:disposed-p ctrl)
    (error 'gfs:disposed-error)))

(defmethod gfg:background-color ((ctrl control))
  (or (brush-color-of ctrl) (gfg:rgb->color (gfs::get-sys-color gfs::+color-btnface+))))

(defmethod (setf gfg:background-color) :before (color (ctrl control))
  (declare (ignore color))
  (if (gfs:disposed-p ctrl)
    (error 'gfs:disposed-error)))

(defmethod (setf gfg:background-color) (color (ctrl control))
  (let ((hbrush (brush-handle-of ctrl)))
    (when (not (gfs:null-handle-p hbrush))
      (gfs::delete-object hbrush)
      (setf (brush-handle-of ctrl) (cffi:null-pointer)))
    (setf hbrush (gfs::create-solid-brush (gfg:color->rgb color)))
    (if (gfs:null-handle-p hbrush)
      (error 'gfs:win32-error :detail "create-solid-brush failed"))
    (setf (brush-color-of ctrl) (gfg:copy-color color))
    (setf (brush-handle-of ctrl) hbrush))
  (redraw ctrl))

(defmethod gfs:dispose ((ctrl control))
  (let ((hbrush (brush-handle-of ctrl))
        (font (font-of ctrl)))
    (if font
      (gfs:dispose font))
    (setf (font-of ctrl) nil)
    (if (not (gfs:null-handle-p hbrush))
      (gfs::delete-object hbrush))
    (setf (brush-handle-of ctrl) (cffi:null-pointer)))
  (call-next-method))

(defmethod focus-p :before ((ctrl control))
  (if (gfs:disposed-p ctrl)
    (error 'gfs:disposed-error)))

(defmethod focus-p ((ctrl control))
  (let ((focus-hwnd (gfs::get-focus)))
    (and (not (gfs:null-handle-p focus-hwnd)) (cffi:pointer-eq focus-hwnd (gfs:handle ctrl)))))

(defmethod gfg:font :before ((ctrl control))
  (if (gfs:disposed-p ctrl)
    (error 'gfs:disposed-error)))

(defmethod gfg:font ((self control))
  (let ((font (font-of self)))
    (unless font
      (let ((result (gfs::send-message (gfs:handle self) gfs::+wm-getfont+ 0 0))
            (gc nil))
        (if (zerop result)
          (unwind-protect
              (progn
                (setf gc (make-instance 'gfg:graphics-context :widget self))
                (setf font (gfg:font gc)))
            (gfs:dispose gc))
          (setf font (make-instance 'gfg:font :handle (cffi:make-pointer result))))))
    font))

(defmethod (setf gfg:font) :before (font (self control))
  (if (or (gfs:disposed-p self) (gfs:disposed-p font))
    (error 'gfs:disposed-error)))

(defmethod (setf gfg:font) (font (self control))
  (setf (font-of self) font)
  (gfs::send-message (gfs:handle self)
                     gfs::+wm-setfont+
                     (cffi:pointer-address (gfs:handle font))
                     1))
#|
  (redraw self))
|#

(defmethod gfg:foreground-color :before ((self control))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod gfg:foreground-color ((self control))
  (or (text-color-of self) (gfg:rgb->color (gfs::get-sys-color gfs::+color-btntext+))))

(defmethod (setf gfg:foreground-color) :before (color (self control))
  (declare (ignore color))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf gfg:foreground-color) (color (self control))
  (setf (text-color-of self) (gfg:copy-color color))
  (redraw self))

(defmethod give-focus :before ((self control))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod give-focus ((self control))
  (gfs::set-focus (gfs:handle self)))

(defmethod initialize-instance :after ((self control) &key callback callbacks dispatcher parent &allow-other-keys)
  (if (gfs:disposed-p parent)
    (error 'gfs:disposed-error))
  (unless (or dispatcher callbacks (not (functionp callback)))
    (let ((class (define-dispatcher (class-name (class-of self)) callback)))
      (setf (dispatcher self) (make-instance (class-name class))))))

(defmethod maximum-size ((self control))
  (slot-value self 'max-size))

(defmethod (setf maximum-size) (max-size (self control))
  (setf (slot-value self 'max-size) max-size)
  (unless (gfs:disposed-p self)
    (let ((size (constrain-new-size max-size (size self) #'min)))
      (setf (size self) size))))

(defmethod minimum-size ((self control))
  (let ((size (slot-value self 'min-size)))
    (if (null size)
      (preferred-size self -1 -1)
      size)))

(defmethod (setf minimum-size) (min-size (self control))
  (setf (slot-value self 'min-size) min-size)
  (unless (gfs:disposed-p self)
    (let ((size (constrain-new-size min-size (size self) #'max)))
      (setf (size self) size))))

(defmethod preferred-size :before ((self control) width-hint height-hint)
  (declare (ignorable width-hint height-hint))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod print-object ((self control) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "handle: ~x " (gfs:handle self))
    (format stream "dispatcher: ~a" (dispatcher self))
    (unless (gfs:disposed-p self)      
      (format stream "size: ~a " (size self))
      (format stream "text baseline: ~a" (text-baseline self)))))

(defmethod text-baseline ((self control))
  (floor (gfs:size-height (size self)) 2))

(defmethod update-native-style ((self control) flags)
  (let ((hwnd (gfs:handle self)))
    (gfs::set-window-long hwnd gfs::+gwl-style+ flags)
    (gfs::set-window-pos hwnd (cffi:null-pointer) 0 0 0 0 (logior gfs::+swp-framechanged+
                                                                  gfs::+swp-nomove+
                                                                  gfs::+swp-nosize+
                                                                  gfs::+swp-nozorder+)))
  flags)
