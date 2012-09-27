;;;;
;;;; window.lisp
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

(defconstant +max-classname-string-length+             256)

(defparameter *dialog-classname*                       "GraphicFormsDialog")
(defparameter *toplevel-erasebkgnd-window-classname*   "GraphicFormsTopLevelEraseBkgnd")
(defparameter *toplevel-noerasebkgnd-window-classname* "GraphicFormsTopLevelNoEraseBkgnd")

;;;
;;; helper functions
;;;

(defun init-window (win classname register-class-fn parent text)
  (let ((tc (thread-context)))
    (setf (widget-in-progress tc) win)
    (funcall register-class-fn)
    (multiple-value-bind (std-style ex-style)
        (compute-style-flags win)
      (create-window classname
                     text
                     (if (null parent) (cffi:null-pointer) (gfs:handle parent))
                     std-style
                     ex-style))
    (clear-widget-in-progress tc)
    (let ((hwnd (gfs:handle win)))
      (if (not hwnd) ; handle slot should have been set during create-window
        (error 'gfs:win32-error :detail "create-window failed"))
      (if (find :keyboard-navigation (style-of win))
        (put-kbdnav-widget tc win))
      (put-widget tc win))
    (if (find :status-bar (style-of win))
      (setf (slot-value win 'status-bar) (make-instance 'status-bar :parent win)))
    ;; FIXME: this is a temporary hack to allow layout management testing;
    ;; it breaks in the presence of virtual containers like group
    ;;
    (let ((parent (parent win)))
      (if (and parent (layout-of parent))
        (append-layout-item (layout-of parent) win)))))

(cffi:defcallback (child-window-visitor :cconv :stdcall) gfs::BOOL
    ((hwnd :pointer) (lparam gfs::LPARAM))
  (let* ((tc (thread-context))
         (child (get-widget tc hwnd))
         (parent (get-widget tc (cffi:make-pointer lparam))))
    (unless (or (null parent) (null child) (typep child 'status-bar))
      (let ((ancestor-hwnd (gfs::get-ancestor (gfs:handle child) gfs::+ga-parent+))
            (tmp-list (child-visitor-results tc)))
        (if (cffi:pointer-eq (gfs:handle parent) ancestor-hwnd)
          (setf (child-visitor-results tc) (push (call-child-visitor-func tc parent child) tmp-list))))))
  1)

(defun window-class-registered-p (class-name)
  (cffi:with-foreign-string (str-ptr class-name)
    (cffi:with-foreign-object (wc-ptr 'gfs::wndclassex)
      (cffi:with-foreign-slots ((gfs::cbsize) wc-ptr gfs::wndclassex)
        (gfs::zero-mem wc-ptr gfs::wndclassex)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::wndclassex))
        (/= (gfs::get-class-info (gfs::get-module-handle (cffi:null-pointer)) str-ptr wc-ptr)
            0)))))

(defun get-window-class-name (hwnd)
  (cffi:with-foreign-pointer-as-string (str-ptr +max-classname-string-length+)
    (if (zerop (gfs::get-class-name hwnd str-ptr +max-classname-string-length+))
      (error 'gfs:win32-error :detail "get-class-name failed"))
    (cffi:foreign-string-to-lisp str-ptr)))

(defun get-window-class-cursor (hwnd)
  (cffi:with-foreign-string (str-ptr (get-window-class-name hwnd))
    (cffi:with-foreign-object (wc-ptr 'gfs::wndclassex)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::hcursor) wc-ptr gfs::wndclassex)
        (gfs::zero-mem wc-ptr gfs::wndclassex)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::wndclassex))
        (when (zerop (gfs::get-class-info (gfs::get-module-handle (cffi:null-pointer)) str-ptr wc-ptr))
          (warn 'gfs:win32-warning
                :detail (format nil "class ~a not registered" (get-window-class-name hwnd)))
          (return-from get-window-class-cursor nil))
        (if (not (gfs::null-handle-p gfs::hcursor))
          (make-instance 'gfg:cursor :handle gfs::hcursor :shared t))))))

(defun register-window-class (class-name proc-ptr style bkgcolor &optional wndextra)
  (if (window-class-registered-p class-name)
    (return-from register-window-class 1))
  (let ((retval 0))
    (cffi:with-foreign-string (str-ptr class-name)
      (cffi:with-foreign-object (wc-ptr 'gfs::wndclassex)
        (cffi:with-foreign-slots ((gfs::cbsize gfs::style gfs::wndproc
                                   gfs::clsextra gfs::wndextra gfs::hinst
                                   gfs::hicon gfs::hcursor gfs::hbrush
                                   gfs::menuname gfs::classname gfs::smallicon)
                                  wc-ptr gfs::wndclassex)
          (gfs::zero-mem wc-ptr gfs::wndclassex)
          (setf gfs::cbsize    (cffi:foreign-type-size 'gfs::wndclassex)
                gfs::style     style
                gfs::wndproc   proc-ptr
                gfs::clsextra  0
                gfs::wndextra  (or wndextra 0)
                gfs::hinst     (gfs::get-module-handle (cffi:null-pointer))
                gfs::hicon     (cffi:null-pointer)
                gfs::hcursor   (gfs::load-image (cffi:null-pointer)
                                 (cffi:make-pointer gfs::+ocr-normal+)
                                                    gfs::+image-cursor+ 0 0
                                                    (logior gfs::+lr-defaultcolor+
                                                            gfs::+lr-shared+))
                gfs::hbrush    (if (< bkgcolor 0)
                                 (cffi:null-pointer)
                                 (cffi:make-pointer (1+ bkgcolor)))
                gfs::menuname  (cffi:null-pointer)
                gfs::classname str-ptr
                gfs::smallicon (cffi:null-pointer))
          (setf retval (gfs::register-class wc-ptr)))))
    (if (/= retval 0)
      retval
      (error 'gfs::win32-error :detail "register-class failed"))))

(defun capture-mouse (self)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (typep self 'window)
    (error 'gfs:toolkit-error :detail "capture-mouse is restricted to window subclasses"))
  (gfs::set-capture (gfs:handle self)))

(defun release-mouse ()
  (gfs::release-capture))

(defun get-window-origin (gc)
  (let ((pnt (gfs:make-point)))
    (gfs::get-window-org (gfs:handle gc) pnt)
    pnt))

(defun set-window-origin (gc pnt)
  (gfs::set-window-org (gfs:handle gc) (gfs:point-x pnt) (gfs:point-y pnt) (cffi:null-pointer))
  pnt)

(defun scroll-children (window delta-x delta-y)
  (let ((specs (mapchildren window (lambda (parent child)
                                     (declare (ignore parent))
                                     (let ((pnt (location child))
                                           (size (size child)))
                                       (incf (gfs:point-x pnt) delta-x)
                                       (incf (gfs:point-y pnt) delta-y)
                                       (list child (gfs:make-rectangle :location pnt :size size)))))))
    (arrange-hwnds specs (lambda (child) (declare (ignore child)) +window-pos-flags+))))

;;;
;;; methods
;;;

(defmethod gfg:background-color ((self window))
  (let ((hwnd (gfs:handle self))
        (color nil))
    (if (string= (get-window-class-name hwnd) *toplevel-erasebkgnd-window-classname*)
      (setf color (gfg:rgb->color (gfs::get-sys-color gfs::+color-appworkspace+)))
      (setf color (gfg:rgb->color (gfs::get-class-long hwnd gfs::+gclp-hbrbackground+))))
    color))

(defmethod compute-outer-size ((self window) desired-client-size)
  (let* ((hwnd (gfs:handle self))
         (has-menu (not (cffi:null-pointer-p (gfs::get-menu hwnd))))
         (new-size (gfs:make-size)))
    (gfs::with-rect (rect-ptr)
      (setf gfs::right  (gfs:size-width desired-client-size)
            gfs::bottom (gfs:size-height desired-client-size))
      (if (zerop (gfs::adjust-window-rect rect-ptr
                                          (get-native-style self)
                                          (if has-menu 1 0)
                                          (get-native-exstyle self)))
        (error 'gfs:win32-error :detail "adjust-window-rect failed"))
      (setf (gfs:size-width new-size)  (- gfs::right gfs::left)
            (gfs:size-height new-size) (- gfs::bottom gfs::top))
      ;; check how much wrapping occurs if there is a menu and we
      ;; size a window to the above-computed width and infinite
      ;; height
      (when has-menu
        (setf gfs::bottom #x7FFFFFFF) ; ensures we handle all possible menu wrap
        (gfs::send-message hwnd gfs::+wm-nccalcsize+ 0 (cffi:pointer-address rect-ptr))
        ;; gfs::top is now the bottom-most position of the top part of the window's
        ;; non-client area, which is the area that the wrapped menu occupies and for
        ;; which compensation is needed.
        (incf (gfs:size-height new-size) gfs::top)))
    new-size))

(defmethod gfs:dispose ((self window))
  (delete-kbdnav-widget (thread-context) self)
  (call-next-method))

(defmethod enable-layout :before ((self window) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod enable-layout ((self window) flag)
  (setf (slot-value self 'layout-p) flag)
  (if (and flag (layout-of self))
    (let ((sz (client-size self)))
      (perform (layout-of self) self (gfs:size-width sz) (gfs:size-height sz)))))

(defmethod enable-scrollbars ((self window) horizontal vertical)
  (let ((style (style-of self))
        (bits (get-native-style self)))
    (if horizontal
      (pushnew :horizontal-scrollbar style)
      (progn
        (setf style (remove :horizontal-scrollbar style))
        (update-native-style self (logand bits (lognot gfs::+ws-hscroll+)))))
    (if vertical
      (pushnew :vertical-scrollbar style)
      (progn
        (setf style (remove :vertical-scrollbar style))
        (update-native-style self (logand bits (lognot gfs::+ws-vscroll+)))))
    (setf (style-of self) style))
  (if (and (layout-of self) (layout-p self))
    (let ((size (client-size self)))
      (perform (layout-of self) self (gfs:size-width size) (gfs:size-height size))))
  (update-scrollbar-page-sizes self)
  (update-scrolling-state self :both))

(defmethod event-resize (disp (self window) size type)
  (declare (ignore disp size type))
  (unless (null (layout-of self))
    (let ((client-size (client-size self)))
      (perform (layout-of self) self (gfs:size-width client-size) (gfs:size-height client-size)))))

(defmethod focus-p :before ((self window))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod focus-p ((self window))
  (let ((focus-hwnd (gfs::get-focus)))
    (and (not (gfs:null-handle-p focus-hwnd)) (cffi:pointer-eq focus-hwnd (gfs:handle self)))))

(defmethod gfg:font ((self window))
  (gfs::with-retrieved-dc ((gfs:handle self) hdc)
    (let ((hfont (gfs::get-current-object hdc gfs::+obj-font+)))
      (if (gfs:null-handle-p hfont)
        (error 'gfs:win32-error :detail "get-current-object failed"))
      (make-instance 'gfg:font :handle hfont))))

(defmethod give-focus :before ((self window))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod give-focus ((self window))
  (gfs::set-focus (gfs:handle self)))

(defmethod horizontal-scrollbar-p ((self top-level))
  (find :horizontal-scrollbar (style-of self)))

(defmethod image ((self window))
  (let ((small (gfs::send-message (gfs:handle self) gfs::+wm-geticon+ gfs::+icon-small+ 0))
        (large (gfs::send-message (gfs:handle self) gfs::+wm-geticon+ gfs::+icon-big+ 0))
        (handles nil))
    (unless (zerop small)
      (push (cffi:make-pointer small) handles))
    (unless (zerop large)
      (push (cffi:make-pointer large) handles))
    (make-instance 'gfg:icon-bundle :handle handles)))

(defmethod (setf image) ((image gfg:image) (self window))
  (setf (image self) (make-instance 'gfg:icon-bundle :images (list image))))

(defmethod (setf image) ((bundle gfg:icon-bundle) (self window))
  (let ((hwnd (gfs:handle self))
        (small (gfg::icon-handle-ref bundle :small))
        (large (gfg::icon-handle-ref bundle :large)))
    (unless (gfs:null-handle-p small)
      (gfs::send-message hwnd gfs::+wm-seticon+ gfs::+icon-small+ (cffi:pointer-address small)))
    (unless (gfs:null-handle-p large)
      (gfs::send-message hwnd gfs::+wm-seticon+ gfs::+icon-big+ (cffi:pointer-address large)))))

(defmethod location ((self window))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((pnt (gfs:make-point)))
    (outer-location self pnt)
    pnt))

(defmethod layout ((self window))
  (unless (null (layout-of self))
    (let ((sz (client-size self)))
      (perform (layout-of self) self (gfs:size-width sz) (gfs:size-height sz)))))

(defmethod mapchildren ((self window) func)
  (let ((tc (thread-context))
        (hwnd (gfs:handle self)))
    (setf (child-visitor-func tc) func)
    (unwind-protect
        (gfs::enum-child-windows hwnd
                                 (cffi:callback child-window-visitor)
                                 (cffi:pointer-address hwnd))
      (setf (child-visitor-func tc) nil))
    (let ((tmp (reverse (child-visitor-results tc))))
      (setf (child-visitor-results tc) nil)
      tmp)))

(defmethod maximum-size ((self window))
  (slot-value self 'max-size))

(defmethod (setf maximum-size) (max-size (self window))
  (setf (slot-value self 'max-size) max-size)
  (unless (gfs:disposed-p self)
    (let ((size (constrain-new-size max-size (size self) #'min)))
      (setf (size self) size)
      (unless (null (layout-of self))
        (perform (layout-of self) self (gfs:size-width size) (gfs:size-height size)))
      size)))

(defmethod minimum-size ((self window))
  (slot-value self 'min-size))

(defmethod (setf minimum-size) (min-size (self window))
  (setf (slot-value self 'min-size) min-size)
  (unless (gfs:disposed-p self)
    (let ((size (constrain-new-size min-size (size self) #'max)))
      (setf (size self) size)
      (unless (null (layout-of self))
        (perform (layout-of self) self (gfs:size-width size) (gfs:size-height size)))
      size)))

(defmethod obtain-horizontal-scrollbar :before ((self window))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod obtain-horizontal-scrollbar ((self window))
  (if (horizontal-scrollbar-p self)
    (make-instance 'standard-scrollbar :handle (gfs:handle self) :orientation gfs::+sb-horz+)))

(defmethod obtain-vertical-scrollbar :before ((self window))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod obtain-vertical-scrollbar ((self window))
  (if (vertical-scrollbar-p self)
    (make-instance 'standard-scrollbar :handle (gfs:handle self) :orientation gfs::+sb-vert+)))

(defmethod pack ((self window))
  (unless (null (layout-of self))
    (perform (layout-of self) self -1 -1))
  (call-next-method))

(defmethod preferred-size :before ((self window) width-hint height-hint)
  (declare (ignorable width-hint height-hint))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod preferred-size ((self window) width-hint height-hint)
  (let ((layout (layout-of self)))
    (if (and (layout-p self) layout)
      (let ((new-client-sz (compute-size layout self width-hint height-hint)))
        (compute-outer-size self new-client-sz))
      (size self))))

(defmethod print-object ((self window) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "handle: ~x " (gfs:handle self))
    (format stream "dispatcher: ~a " (dispatcher self))
    (if (not (gfs:disposed-p self))
      (format stream "size: ~a" (size self)))))

(defmethod scroll ((self window) delta-x delta-y children-p millis)
  (let ((flags gfs::+sw-invalidate+))
    (if (> millis 0)
      (let ((tmp (ash (logand millis #xFFFF) 16)))
        (setf flags (logior flags tmp gfs::+sw-smoothscroll+))))
    (if children-p
      (scroll-children self delta-x delta-y))
    (gfs::scroll-window (gfs:handle self)
                        delta-x
                        delta-y
                        (cffi:null-pointer)
                        (cffi:null-pointer)
                        (cffi:null-pointer)
                        (cffi:null-pointer)
                        flags)))

(defmethod show ((self window) flag)
  (declare (ignore flag))
  (call-next-method)
  (gfs::update-window (gfs:handle self)))

(defmethod size ((self window))
  (let ((sz (gfs:make-size)))
    (outer-size self sz)
    sz))

(defmethod update-native-style ((self window) flags)
  (let ((hwnd (gfs:handle self)))
    (gfs::set-window-long hwnd gfs::+gwl-style+ flags)
    (gfs::set-window-pos hwnd (cffi:null-pointer) 0 0 0 0 (logior gfs::+swp-framechanged+
                                                                  gfs::+swp-nomove+
                                                                  gfs::+swp-nosize+
                                                                  gfs::+swp-nozorder+)))
  flags)

(defmethod vertical-scrollbar-p ((self top-level))
  (find :vertical-scrollbar (style-of self)))

(defmethod window->display :before ((self window))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod window->display ((self window))
  (let* ((hmonitor (gfs::monitor-from-window (gfs:handle self) gfs::+monitor-defaulttonearest+))
         (display (make-instance 'display)))
    (setf (slot-value display 'gfs:handle) hmonitor)
    display))
