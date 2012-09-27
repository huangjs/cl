;;;;
;;;; top-level.lisp
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

(defparameter *default-window-title* "New Window")

;;;
;;; helper functions
;;;

(defun register-toplevel-erasebkgnd-window-class ()
  (register-window-class *toplevel-erasebkgnd-window-classname*
                         (cffi:get-callback 'uit_widgets_wndproc)
                         gfs::+cs-dblclks+
                         gfs::+color-appworkspace+))

(defun register-toplevel-noerasebkgnd-window-class ()
  (register-window-class *toplevel-noerasebkgnd-window-classname*
                         (cffi:get-callback 'uit_widgets_wndproc)
                         gfs::+cs-dblclks+
                         -1))

(defun update-top-level-resizability (win same-size-flag)
  (let ((orig-flags (get-native-style win))
        (new-flags 0))
    (cond
      (same-size-flag
         (setf new-flags (logand orig-flags (lognot gfs::+ws-maximizebox+)))
         (setf new-flags (logand new-flags (lognot gfs::+ws-thickframe+))))
      (t
         (setf new-flags (logior orig-flags gfs::+ws-maximizebox+))
         (setf new-flags (logior new-flags gfs::+ws-thickframe+))))
    (when (/= orig-flags new-flags)
      (update-native-style win new-flags))))

;;;
;;; methods
;;;

(defmethod compute-style-flags ((self top-level) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags 0)
        (ex-flags 0))
    (loop for sym in (style-of self)
          do (ecase sym
               ;; pre-packaged combinations of window styles
               ;;
               (:borderless
                  (setf std-flags (logior gfs::+ws-clipchildren+
                                          gfs::+ws-clipsiblings+
                                          gfs::+ws-border+
                                          gfs::+ws-popup+))
                  (setf ex-flags gfs::+ws-ex-topmost+))
               (:palette
                  (setf std-flags (logior gfs::+ws-clipchildren+
                                          gfs::+ws-clipsiblings+
                                          gfs::+ws-popupwindow+
                                          gfs::+ws-caption+))
                  (setf ex-flags (logior gfs::+ws-ex-toolwindow+
                                         gfs::+ws-ex-windowedge+)))
               (:miniframe
                  (setf std-flags (logior gfs::+ws-clipchildren+
                                          gfs::+ws-clipsiblings+
                                          gfs::+ws-popup+
                                          gfs::+ws-thickframe+
                                          gfs::+ws-sysmenu+
                                          gfs::+ws-caption+))
                  (setf ex-flags (logior gfs::+ws-ex-appwindow+
                                         gfs::+ws-ex-toolwindow+)))
               (:frame
                  (setf std-flags (logior gfs::+ws-overlappedwindow+
                                          gfs::+ws-clipsiblings+
                                          gfs::+ws-clipchildren+))
                  (setf ex-flags 0))
               (:workspace
                  (setf std-flags (logior gfs::+ws-overlappedwindow+
                                          gfs::+ws-clipsiblings+
                                          gfs::+ws-clipchildren+))
                  (setf ex-flags 0))

               ;; styles that can be combined
               ;;
               (:fixed-size
                  (setf std-flags (logand std-flags
                                          (lognot (logior gfs::+ws-maximizebox+
                                                          gfs::+ws-thickframe+)))))
               (:horizontal-scrollbar
                  (setf std-flags (logior std-flags gfs::+ws-hscroll+)))
               (:status-bar) ;; nothing to do, but need to allow this style symbol
               (:vertical-scrollbar
                  (setf std-flags (logior std-flags gfs::+ws-vscroll+)))))
    (values std-flags ex-flags)))

(defmethod gfs:dispose ((self top-level))
  (let ((menu (menu-bar self)))
    (when menu
      (visit-menu-tree menu #'menu-cleanup-callback)
      (delete-widget (thread-context) (gfs:handle menu))))
  (let ((sbar (status-bar-of self)))
    (when sbar
      (delete-widget (thread-context) (gfs:handle sbar))
      (setf (slot-value self 'status-bar) nil)))
  (call-next-method))

(defmethod event-resize (disp (self top-level) size type)
  (declare (ignore disp size type))
  (let ((event (raw-event (thread-context)))
        (sbar (status-bar-of self)))
    (if (and sbar (not (gfs:disposed-p sbar)))
      (gfs::send-message (gfs:handle sbar)
                         gfs::+wm-size+
                         (event-wparam event)
                         (logand (event-lparam event) #xFFFFFFFF))))
  (call-next-method))

(defmethod initialize-instance :after ((self top-level) &key owner text &allow-other-keys)
  (unless (null owner)
    (if (gfs:disposed-p owner)
      (error 'gfs:disposed-error)))
  (if (null text)
    (setf text *default-window-title*))
  (let ((classname *toplevel-noerasebkgnd-window-classname*)
        (register-func #'register-toplevel-noerasebkgnd-window-class))
    (when (find :workspace (style-of self))
      (setf classname *toplevel-erasebkgnd-window-classname*)
      (setf register-func #'register-toplevel-erasebkgnd-window-class))
    (init-window self classname register-func owner text)
    (show self nil)))

(defmethod (setf maximum-size) :after (max-size (self top-level))
  (when (and max-size (minimum-size self))
    (update-top-level-resizability self (gfs:equal-size-p (minimum-size self) max-size))))

(defmethod menu-bar :before ((self top-level))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod menu-bar ((self top-level))
  (let ((hmenu (gfs::get-menu (gfs:handle self))))
    (if (gfs:null-handle-p hmenu)
      (return-from menu-bar nil))
    (let ((m (get-widget (thread-context) hmenu)))
      (if (null m)
        (error 'gfs:toolkit-error :detail "no object for menu handle"))
      m)))

(defmethod (setf menu-bar) :before ((m menu) (self top-level))
  (declare (ignore m))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf menu-bar) ((m menu) (self top-level))
  (let* ((hwnd (gfs:handle self))
         (hmenu (gfs::get-menu hwnd))
         (old-menu (get-widget (thread-context) hmenu)))
    (unless (gfs:null-handle-p hmenu)
      (gfs::destroy-menu hmenu))
    (unless (null old-menu)
      (gfs:dispose old-menu))
    (gfs::set-menu hwnd (gfs:handle m))
    (gfs::draw-menu-bar hwnd)))

(defmethod (setf minimum-size) :after (min-size (self top-level))
  (when (and (maximum-size self) min-size)
    (update-top-level-resizability self (gfs:equal-size-p min-size (maximum-size self)))))

(defmethod pack ((win top-level))
  (if (find :fixed-size (style-of win))
    (let ((size (gfw:preferred-size win -1 -1)))
      (setf (gfw:minimum-size win) size
            (gfw:maximum-size win) size)))
  (call-next-method))

(defmethod preferred-size ((self top-level) width-hint height-hint)
  (declare (ignore width-hint height-hint))
  (let ((size (call-next-method))
        (sbar (status-bar-of self)))
    (if sbar
      (incf (gfs:size-height size) (gfs:size-height (preferred-size sbar -1 -1))))
    size))

(defmethod print-object ((self top-level) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "handle: ~x " (gfs:handle self))
    (format stream "dispatcher: ~a " (dispatcher self))
    (format stream "client size: ~a " (size self))
    (format stream "min size: ~a " (minimum-size self))
    (format stream "max size: ~a" (maximum-size self))))

(defmethod resizable-p ((self top-level))
  (test-native-style self gfs::+ws-thickframe+))

(defmethod (setf resizable-p) (flag (self top-level))
  (let ((style (style-of self)))
    (if (or (find :frame style) (find :workspace style))
      (update-top-level-resizability self (not flag)))))

(defmethod text :before ((self top-level))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod text ((self top-level))
  (get-widget-text self))

(defmethod (setf text) :before (str (self top-level))
  (declare (ignore str))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf text) (str (self top-level))
  (set-widget-text self str))
