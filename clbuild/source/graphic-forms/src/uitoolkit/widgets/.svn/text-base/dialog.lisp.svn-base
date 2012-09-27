;;;;
;;;; dialog.lisp
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

(defparameter *default-dialog-title*  " ")

(defconstant  +dlgwindowextra+         48)

(defvar       *disabled-top-levels*   nil)

;;;
;;; helper functions
;;;

(defun register-dialog-class ()
  (register-window-class *dialog-classname*
                         (cffi:get-callback 'uit_widgets_wndproc)
                         (logior gfs::+cs-dblclks+
                                 gfs::+cs-savebits+
                                 gfs::+cs-bytealignwindow+)
                         gfs::+color-btnface+
                         +dlgwindowextra+))

(defun disable-top-levels (hdlg)
  (let ((hutility (utility-hwnd (thread-context))))
    (setf *disabled-top-levels* nil)
    (maptoplevels (lambda (win)
                    (unless (or (cffi:pointer-eq (gfs:handle win) hdlg)
                                (cffi:pointer-eq (gfs:handle win) hutility))
                      (if (enabled-p win)
                        (push win *disabled-top-levels*))
                        (enable win nil))))))

(defun reenable-top-levels ()
  (loop for win in *disabled-top-levels*
        do (enable win t))
  (setf *disabled-top-levels* nil))

;;;
;;; methods
;;;

(defmethod gfg:background-color ((dlg dialog))
  (gfg:rgb->color (gfs::get-sys-color gfs::+color-btnface+)))

(defmethod compute-style-flags ((dlg dialog) &rest extra-data)
  (declare (ignore extra-data))
  (values (logior gfs::+ws-caption+
                  gfs::+ws-popup+
                  gfs::+ws-sysmenu+)
          (logior gfs::+ws-ex-controlparent+
                  gfs::+ws-ex-dlgmodalframe+
                  gfs::+ws-ex-windowedge+)))

(defmethod cancel-widget :before ((self dialog))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod cancel-widget ((self dialog))
  (let ((kid nil))
    (mapchildren self
                 (lambda (parent child)
                   (declare (ignore parent))
                   (if (= (gfs::get-window-long (gfs:handle child) gfs::+gwlp-id+) gfs::+idcancel+)
                     (setf kid child))))
    kid))

(defmethod (setf cancel-widget) :before ((def-widget widget) (self dialog))
  (if (or (gfs:disposed-p self) (gfs:disposed-p def-widget))
    (error 'gfs:disposed-error)))

(defmethod (setf cancel-widget) ((cancel-widget widget) (self dialog))
  (when (or (not (typep cancel-widget 'button))
            (and (style-of cancel-widget)
                 (null (intersection '(:push-button :cancel-button :default-button)
                                      (style-of cancel-widget)))))
    (warn 'gfs:toolkit-warning :detail "only push buttons may serve as cancel widgets in a dialog")
    (return-from cancel-widget nil))
  (let ((old-widget (cancel-widget self)))
    (if old-widget
      (let* ((hwnd (gfs:handle old-widget))
             (style (get-native-style old-widget)))
        (setf style (logand style (lognot gfs::+bs-defpushbutton+)))
        (gfs::set-window-long hwnd gfs::+gwlp-id+ (increment-widget-id (thread-context)))
        (gfs::send-message (gfs:handle self) gfs::+dm-setdefid+ 0 0)
        (update-native-style old-widget style))))
  (let* ((hwnd (gfs:handle cancel-widget))
         (style (get-native-style cancel-widget)))
    (setf style (logior style gfs::+bs-pushbutton+))
    (gfs::set-window-long hwnd gfs::+gwlp-id+ gfs::+idcancel+)
    (update-native-style cancel-widget style)))

(defmethod compute-outer-size ((self dialog) desired-client-size)
  (declare (ignore desired-client-size))
  (let ((size (call-next-method))
        (sbar (status-bar-of self)))
    (if sbar
      (incf (gfs:size-height size) (gfs:size-height (preferred-size sbar -1 -1))))
    size))

(defmethod default-widget :before ((self dialog))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod default-widget ((self dialog))
  (let ((kid nil))
    (mapchildren self
                 (lambda (parent child)
                   (declare (ignore parent))
                   (if (= (gfs::get-window-long (gfs:handle child) gfs::+gwlp-id+) gfs::+idok+)
                     (setf kid child))))
    kid))

(defmethod (setf default-widget) :before ((def-widget widget) (self dialog))
  (if (or (gfs:disposed-p self) (gfs:disposed-p def-widget))
    (error 'gfs:disposed-error)))

(defmethod (setf default-widget) ((def-widget widget) (self dialog))
  (when (or (not (typep def-widget 'button))
            (and (style-of def-widget)
                 (null (intersection '(:push-button :cancel-button :default-button)
                                      (style-of def-widget)))))
    (warn 'gfs:toolkit-warning :detail "only push buttons may serve as default widgets in a dialog")
    (return-from default-widget nil))
  (let ((old-widget (default-widget self)))
    (if old-widget
      (let* ((hwnd (gfs:handle old-widget))
             (style (get-native-style old-widget)))
        (setf style (logand style (lognot gfs::+bs-defpushbutton+)))
        (gfs::set-window-long hwnd gfs::+gwlp-id+ (increment-widget-id (thread-context)))
        (gfs::send-message (gfs:handle self) gfs::+dm-setdefid+ 0 0)
        (update-native-style old-widget style))))
  (let* ((hdlg (gfs:handle self))
         (hwnd (gfs:handle def-widget))
         (style (get-native-style def-widget)))
    (setf style (logior style gfs::+bs-defpushbutton+))
    (gfs::set-window-long hwnd gfs::+gwlp-id+ gfs::+idok+)
    (gfs::send-message hdlg gfs::+dm-setdefid+ (cffi:pointer-address hwnd) 0)
    (update-native-style def-widget style)))

(defmethod gfs:dispose ((self dialog))
  (reenable-top-levels)
  (if (visible-p self)
    (show self nil))
  (let ((sbar (status-bar-of self)))
    (when sbar
      (delete-widget (thread-context) (gfs:handle sbar))
      (setf (slot-value self 'status-bar) nil)))
  (call-next-method))

(defmethod event-resize (disp (self dialog) size type)
  (declare (ignore disp size type))
  (let ((event (raw-event (thread-context)))
        (sbar (status-bar-of self)))
    (if (and sbar (not (gfs:disposed-p sbar)))
      (gfs::send-message (gfs:handle sbar)
                         gfs::+wm-size+
                         (event-wparam event)
                         (logand (event-lparam event) #xFFFFFFFF))))
  (call-next-method))

(defmethod initialize-instance :after ((self dialog) &key owner text &allow-other-keys)
  (unless (null owner)
    (if (gfs:disposed-p owner)
      (error 'gfs:disposed-error)))
  (if (null text)
    (setf text *default-dialog-title*))
  ;; Don't allow apps to specify the desktop window as the
  ;; owner of the dialog; it would cause the desktop to become
  ;; disabled.
  ;;
  (if (and owner (cffi:pointer-eq (gfs:handle owner) (gfs::get-desktop-window)))
    (setf owner nil))
  (push :keyboard-navigation (style-of self))
  ;; FIXME: check if owner is actually a top-level or dialog, and if not,
  ;; walk up the ancestors until one is found. Only top level hwnds can
  ;; be owners.
  ;;
  (init-window self *dialog-classname* #'register-dialog-class owner text))

(defmethod preferred-size ((self dialog) width-hint height-hint)
  (declare (ignore width-hint height-hint))
  (let ((size (call-next-method))
        (sbar (status-bar-of self)))
    (if sbar
      (incf (gfs:size-height size) (gfs:size-height (preferred-size sbar -1 -1))))
    size))

(defmethod show ((self dialog) flag)
  (let ((app-modal (find :application-modal (style-of self)))
        (owner-modal (find :owner-modal (style-of self)))
        (owner (owner self))
        (hdlg (gfs:handle self)))
    (cond
      ((and app-modal flag)
         (disable-top-levels hdlg))
      ((and app-modal (null flag))
         (reenable-top-levels))
      ((and owner-modal owner)
         (enable owner (null flag))))
    (gfs::show-window hdlg (if flag gfs::+sw-shownormal+ gfs::+sw-hide+))
    (let ((focus-hwnd (gfs::get-next-dlg-tab-item hdlg (cffi:null-pointer) 0)))
      (unless (gfs:null-handle-p focus-hwnd)
        (gfs::send-message hdlg gfs::+wm-nextdlgctl+ (cffi:pointer-address focus-hwnd) 1)))
    (when (and flag (or app-modal owner-modal))
      (message-loop (lambda (gm-code msg-ptr)
                      (cond
                        ((or (gfs:disposed-p self) (not (visible-p self)))
                           t) ; dialog closed, so exit loop
                        ((zerop gm-code)
                           ;; IMPORTANT: allow WM_QUIT to propagate back through
                           ;; nested message loops to the main loop, so that we
                           ;; shut down correctly -- whether the system injected
                           ;; the WM_QUIT or it was something the app did, we
                           ;; handle the shutdown request the same way.
                           ;;
                           (gfs::post-quit-message (cffi:foreign-slot-value msg-ptr
                                                                            'gfs::msg
                                                                            'gfs::wparam))
                           t)
                        ((= gm-code -1)
                           (warn 'gfs:win32-warning :detail "get-message failed")
                           t)
                        ((/= (gfs::is-dialog-message (gfs:handle self) msg-ptr) 0)
                           ;; It was a dialog message and has been processed,
                           ;; so nothing else to do.
                           ;;
                           nil)
                        (t
                           (translate-and-dispatch msg-ptr)
                           nil)))))))
