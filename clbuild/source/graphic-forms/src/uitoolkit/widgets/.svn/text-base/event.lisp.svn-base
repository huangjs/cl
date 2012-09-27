;;;;
;;;; event.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +key-event-peek-flags+ (logior gfs::+pm-noremove+
                                              gfs::+pm-noyield+
                                              gfs::+pm-qs-input+
                                              gfs::+pm-qs-postmessage+)))

;;;
;;; window procedures
;;;

;;; NOTE: these defcallback's work even without stdcall support in
;;; CFFI because Windows looks for wndprocs that are not stdcall
;;; and takes care of stack fixup

(cffi:defcallback uit_widgets_wndproc
                  gfs::UINT
                  ((hwnd gfs::HANDLE)
                   (msg gfs::UINT)
                   (wparam gfs::WPARAM)
                   (lparam gfs::LPARAM))
  (process-message hwnd msg wparam lparam))

(cffi:defcallback subclassing_wndproc
                  gfs::UINT
                  ((hwnd gfs::HANDLE)
                   (msg gfs::UINT)
                   (wparam gfs::WPARAM)
                   (lparam gfs::LPARAM))
  (process-subclass-message hwnd msg wparam lparam))

;;;
;;; helper functions
;;;

(defun message-loop (msg-filter)
  (push msg-filter (message-filters (thread-context)))
  (cffi:with-foreign-object (msg-ptr 'gfs::msg)
    (loop
      (let ((gm (gfs::get-message msg-ptr (cffi:null-pointer) 0 0)))
        (cffi:with-foreign-slots ((gfs::message gfs::wparam) msg-ptr gfs::msg)
          (when (funcall msg-filter gm msg-ptr)
            (pop (message-filters (thread-context)))
            (return-from message-loop gfs::wparam)))))))

(defun process-events ()
  (let ((filter (first (message-filters (thread-context)))))
    (unless filter
      (return-from process-events nil))
    (cffi:with-foreign-object (msg-ptr 'gfs::msg)
      (loop until (zerop (gfs::peek-message msg-ptr (cffi:null-pointer) 0 0 gfs::+pm-remove+))
            do (funcall filter 1 msg-ptr)))))

(defun key-down-p (key-code)
  "Return T if the key corresponding to key-code is currently down."
  (= (logand (gfs::get-async-key-state key-code) #x8000) #x8000))

(defun key-toggled-p (key-code)
  "Return T if the key corresponding to key-code is toggled on; nil otherwise."
  (= (gfs::get-key-state key-code) 1))

(defun process-mouse-message (fn hwnd lparam btn-symbol)
  (let* ((tc (thread-context))
         (w (get-widget tc hwnd))
         (pnt (mouse-event-pnt tc)))
    (when w
      (setf (gfs:point-x pnt) (gfs::lparam-low-word lparam))
      (setf (gfs:point-y pnt) (gfs::lparam-high-word lparam))
      (funcall fn (dispatcher w) w pnt btn-symbol)))
  0)

(defun get-class-wndproc (hwnd)
  (let ((wndproc-val (gfs::get-class-long hwnd gfs::+gclp-wndproc+)))
    (if (zerop wndproc-val)
      (error 'gfs:win32-error :detail "get-class-long failed"))
    (logand wndproc-val #xFFFFFFFF)))

(defun subclass-wndproc (hwnd)
  (if (zerop (gfs::set-window-long hwnd
                                   gfs::+gwlp-wndproc+
                                   (cffi:pointer-address (cffi:get-callback 'subclassing_wndproc))))
    (error 'gfs:win32-error :detail "set-window-long failed")))

(defun dispatch-control-notification (widget wparam-hi)
  (let ((disp (dispatcher widget)))
    (case wparam-hi
      (0                       (event-select         disp widget))
      (#.gfs::+en-killfocus+   (event-focus-loss     disp widget))
      (#.gfs::+en-setfocus+    (event-focus-gain     disp widget))
      (#.gfs::+en-update+      (event-modify         disp widget))
      (#.gfs::+lbn-dblclk+     (event-default-action disp widget))
      (#.gfs::+lbn-killfocus+  (event-focus-loss     disp widget))
      (#.gfs::+lbn-selchange+  (event-select         disp widget))
      (#.gfs::+lbn-setfocus+   (event-focus-gain     disp widget)))))

(defun process-nccalcsize-message (widget wparam lparam)
  (declare (ignore widget))
  (let ((size (gfs:make-size)))
    (cond
      ((zerop wparam)
         (cffi:with-foreign-slots ((gfs::bottom)
                                   (cffi:make-pointer (logand #xFFFFFFFF lparam))
                                   gfs::rect)
           (setf  gfs::bottom (- gfs::bottom (gfs:size-height size))))
         0)
      (t
         (cffi:with-foreign-slots ((gfs::clientnewbottom)
                                   (cffi:make-pointer (logand #xFFFFFFFF lparam))
                                   gfs::nccalcsize-params)
           (setf  gfs::clientnewbottom (- gfs::clientnewbottom (gfs:size-height size))))
         0))))

(defun process-ctlcolor-message (wparam lparam)
  (let ((widget (get-widget (thread-context) (cffi:make-pointer (logand #xFFFFFFFF lparam))))
        (hdc (cffi:make-pointer wparam)))
    (if widget
      (let ((bkgdcolor (brush-color-of widget))
            (textcolor (text-color-of widget)))
        (if bkgdcolor
          (gfs::set-bk-color hdc (gfg:color->rgb bkgdcolor)))
        (if textcolor
          (gfs::set-text-color hdc (gfg:color->rgb textcolor)))
        (cffi:pointer-address (brush-handle-of widget)))
      0)))

(defun dispatch-scroll-notification (widget axis wparam-lo)
  (let ((disp (dispatcher widget))
        (detail (case wparam-lo
                  (#.gfs::+sb-top+           :start)
;                 (#.gfs::+sb-left+          :start)
;                 (#.gfs::+tb-top+           :start)
                  (#.gfs::+sb-bottom+        :end)
;                 (#.gfs::+sb-right+         :end)
;                 (#.gfs::+tb-bottom+        :end)
                  (#.gfs::+sb-lineup+        :step-back)
;                 (#.gfs::+sb-lineleft+      :step-back)
;                 (#.gfs::+tb-linedown+      :step-back)
                  (#.gfs::+sb-linedown+      :step-forward)
;                 (#.gfs::+sb-lineright+     :step-forward)
;                 (#.gfs::tsb-linedown+      :step-forward)
                  (#.gfs::+sb-pageup+        :page-back)
;                 (#.gfs::+sb-pageleft+      :page-back)
;                 (#.gfs::+tb-pageup+        :page-back)
                  (#.gfs::+sb-pagedown+      :page-forward)
;                 (#.gfs::+sb-pageright+     :page-forward)
;                 (#.gfs::+tb-pagedown+      :page-forward)
;                 (#.gfs::+tb-thumbposition+ :thumb-position)
;                 (#.gfs::+tb-thumbtrack+    :thumb-track)
                  (#.gfs::+sb-thumbposition+ :thumb-position)
                  (#.gfs::+sb-thumbtrack+    :thumb-track)
;                 (#.gfs::+tb-endtrack+      :finished)
                  (#.gfs::+sb-endscroll+     :finished))))
    (event-scroll disp widget axis detail)))

(defun obtain-event-time ()
  (gfs::get-message-time))

(defun option->reason (lparam)
  ;; MSDN says the value is a bitmask, so must be tested bit-wise.
  (cond
    ((zerop lparam)
       :shutdown)
    ((oddp lparam)
       :replacing-file)
    ((= (logand lparam #x80000000) #x80000000)
       :logoff)
    (t
       :shutdown)))

;;;
;;; process-message methods
;;;

(defmethod process-message (hwnd msg wparam lparam)
  (let ((w (get-widget (thread-context) hwnd)))
    (if (typep w 'dialog)
      (return-from process-message (gfs::def-dlg-proc hwnd msg wparam lparam))))
  (gfs::def-window-proc hwnd msg wparam lparam))

(defmethod process-message (hwnd (msg (eql gfs::+wm-close+)) wparam lparam)
  (declare (ignore wparam lparam))
  (let ((w (get-widget (thread-context) hwnd)))
    (if w
      (event-close (dispatcher w) w)
      (error 'gfs:toolkit-error :detail "no object for hwnd")))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-command+)) wparam lparam)
  (let* ((tc (thread-context))
         (wparam-hi (gfs::lparam-high-word wparam))
         (wparam-lo (gfs::lparam-low-word wparam))
         (owner (get-widget tc hwnd)))
    ; (format t "wparam-hi: ~x  wparam-lo: ~x  lparam: ~x~%" wparam-hi wparam-lo lparam)
    (if owner
      (if (zerop lparam)
        (let ((item (get-item tc wparam-lo)))
          (if (null item)
            (warn 'gfs:toolkit-warning :detail (format nil "no menu item for id ~x" wparam-lo))
            (unless (null (dispatcher item))
              (event-select (dispatcher item) item))))
        (let ((widget (get-widget tc (cffi:make-pointer (logand #xFFFFFFFF lparam)))))
          (when (and widget (dispatcher widget))
            (dispatch-control-notification widget wparam-hi))))
      (warn 'gfs:toolkit-warning :detail "no object for hwnd")))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-initmenupopup+)) wparam lparam)
  (declare (ignore hwnd lparam))
  (let* ((tc (thread-context))
         (menu (get-widget tc (cffi:make-pointer wparam))))
    (unless (null menu)
      (let ((d (dispatcher menu)))
        (unless (null d)
          (event-activate d menu)))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-menuselect+)) wparam lparam)
  (declare (ignore hwnd lparam)) ; FIXME: handle system menus
  (let* ((tc (thread-context))
         (item (get-item tc (gfs::lparam-low-word wparam))))
    (unless (null item)
      (let ((d (dispatcher item)))
        (unless (null d)
          (event-arm d item)))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-create+)) wparam lparam)
  (let ((widget (get-widget (thread-context) hwnd))) ; has side-effect of setting handle slot
    (if (typep widget 'dialog)
      (gfs::def-dlg-proc hwnd msg wparam lparam)
      0)))

(defmethod process-message (hwnd (msg (eql gfs::+wm-destroy+)) wparam lparam)
  (declare (ignore wparam lparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (if widget
      (event-dispose (dispatcher widget) widget)))
  ;; If widget is registered with a layout manager, that reference
  ;; is not cleared until the next time the layout manager is invoked.
  ;; This alleviates the need for slow messy code here.
  ;;
  (delete-widget (thread-context) hwnd)
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-queryendsession+)) wparam lparam)
  (declare (ignore wparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (unless (null widget)
      (if (event-session (dispatcher widget) widget :query (option->reason lparam)) 1 0))))

(defmethod process-message (hwnd (msg (eql gfs::+wm-endsession+)) wparam lparam)
  (declare (ignore wparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (unless (null widget)
      (event-session (dispatcher widget) widget :end (option->reason lparam))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-char+)) wparam lparam)
  (declare (ignore lparam))
  (let* ((tc (thread-context))
         (widget (get-widget tc hwnd))
         (ch (code-char (gfs::lparam-low-word wparam))))
    (when widget
      (event-key-down (dispatcher widget) widget (virtual-key tc) ch)))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-keydown+)) wparam lparam)
  (declare (ignore lparam))
  (let* ((tc (thread-context))
         (wparam-lo (gfs::lparam-low-word wparam))
         (ch (gfs::map-virtual-key wparam-lo 2))
         (w (get-widget tc hwnd)))
    (setf (virtual-key tc) wparam-lo)
    (when (and w (zerop ch))
      (event-key-down (dispatcher w) w wparam-lo (code-char ch))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-keyup+)) wparam lparam)
  (declare (ignore lparam))
  (let ((tc (thread-context)))
    (let* ((wparam-lo (gfs::lparam-low-word wparam))
           (ch (gfs::map-virtual-key wparam-lo 2))
           (w (get-widget tc hwnd)))
      (when w
        (event-key-up (dispatcher w) w wparam-lo (code-char ch))))
    (setf (virtual-key tc) 0))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-lbuttondblclk+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-double hwnd lparam :left-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-lbuttondown+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-down hwnd lparam :left-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-lbuttonup+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-up hwnd lparam :left-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-mbuttondblclk+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-double hwnd lparam :middle-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-mbuttondown+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-down hwnd lparam :middle-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-mbuttonup+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-up hwnd lparam :middle-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-mousemove+)) wparam lparam)
  (let ((btn-sym :left-button))
    (cond
      ((= (logand wparam gfs::+mk-mbutton+) gfs::+mk-mbutton+)
        (setf btn-sym :middle-button))
      ((= (logand wparam gfs::+mk-rbutton+) gfs::+mk-rbutton+)
        (setf btn-sym :right-button))
      (t
        (setf btn-sym :left-button)))
    (process-mouse-message #'event-mouse-move hwnd lparam btn-sym)))

(defmethod process-message (hwnd (msg (eql gfs::+wm-move+)) wparam lparam)
  (declare (ignore wparam lparam))
  (let* ((tc (thread-context))
         (w (get-widget tc hwnd)))
    (when w
      (outer-location w (move-event-pnt tc))
      (event-move (dispatcher w) w (move-event-pnt tc))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-moving+)) wparam lparam)
  (declare (ignore wparam))
  (let* ((w (get-widget (thread-context) hwnd))
         (ptr (cffi:make-pointer (logand #xFFFFFFFF lparam)))
         (rect (cffi:convert-from-foreign ptr 'gfs::rect-pointer)))
    (event-pre-move (dispatcher w) w rect)
    (cffi:with-foreign-slots ((gfs::left gfs::top gfs::right gfs::bottom) ptr gfs::rect)
      (let ((pnt (gfs:location rect))
            (size (gfs:size rect)))
        (setf gfs::left   (gfs:point-x pnt)
              gfs::top    (gfs:point-y pnt)
              gfs::right  (+ (gfs:point-x pnt) (gfs:size-width size))
              gfs::bottom (+ (gfs:point-y pnt) (gfs:size-height size))))))
  1)

(defmethod process-message (hwnd (msg (eql gfs::+wm-hscroll+)) wparam lparam)
  (let ((widget (get-widget (thread-context)
                            (if (zerop lparam)
                              hwnd
                              (cffi:make-pointer (logand #xFFFFFFFF lparam))))))
    (if widget
      (dispatch-scroll-notification widget :horizontal (gfs::lparam-low-word wparam))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-vscroll+)) wparam lparam)
  (let ((widget (get-widget (thread-context)
                            (if (zerop lparam)
                              hwnd
                              (cffi:make-pointer (logand #xFFFFFFFF lparam))))))
    (if widget
      (dispatch-scroll-notification widget :vertical (gfs::lparam-low-word wparam))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-paint+)) wparam lparam)
  (declare (ignore wparam lparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (if widget
      (cffi:with-foreign-object (ps-ptr 'gfs::paintstruct)
        (cffi:with-foreign-slots ((gfs::rcpaint-x gfs::rcpaint-y
                                   gfs::rcpaint-width gfs::rcpaint-height)
                                  ps-ptr gfs::paintstruct)
          (let ((gc (make-instance 'gfg:graphics-context :handle (gfs::begin-paint hwnd ps-ptr)))
                (pnt (gfs:make-point :x gfs::rcpaint-x :y gfs::rcpaint-y))
                (size (gfs:make-size :width gfs::rcpaint-width :height gfs::rcpaint-height))
                (disp (dispatcher widget)))
            (setf (gfg::surface-size-of gc) (client-size widget))
            (unwind-protect
                (let ((parent (gfw:parent widget)))
                  (when (and parent (typep (dispatcher parent) 'scrolling-helper))
                    (let ((origin (slot-value (dispatcher parent) 'viewport-origin)))
                      (set-window-origin gc origin)
                      (incf (gfs:point-x pnt) (gfs:point-x origin))
                      (incf (gfs:point-y pnt) (gfs:point-y origin))))
                  (event-paint disp widget gc (gfs:make-rectangle :location pnt :size size)))
              (gfs:dispose gc)
              (gfs::end-paint hwnd ps-ptr)))))
      (error 'gfs:toolkit-error :detail "no object for hwnd")))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-ctlcolorbtn+)) wparam lparam)
  (let ((retval (process-ctlcolor-message wparam lparam)))
    (if (zerop retval)
      (gfs::def-window-proc hwnd msg wparam lparam)
      retval)))

(defmethod process-message (hwnd (msg (eql gfs::+wm-ctlcoloredit+)) wparam lparam)
  (let ((retval (process-ctlcolor-message wparam lparam)))
    (if (zerop retval)
      (gfs::def-window-proc hwnd msg wparam lparam)
      retval)))

(defmethod process-message (hwnd (msg (eql gfs::+wm-ctlcolorlistbox+)) wparam lparam)
  (let ((retval (process-ctlcolor-message wparam lparam)))
    (if (zerop retval)
      (gfs::def-window-proc hwnd msg wparam lparam)
      retval)))

(defmethod process-message (hwnd (msg (eql gfs::+wm-ctlcolorstatic+)) wparam lparam)
  (let ((retval (process-ctlcolor-message wparam lparam)))
    (if (zerop retval)
      (gfs::def-window-proc hwnd msg wparam lparam)
      retval)))

(defmethod process-message (hwnd (msg (eql gfs::+wm-setcursor+)) wparam lparam)
  (let* ((widget (get-widget (thread-context) hwnd))
         (cursor (slot-value widget 'cursor)))
    (cond
      (cursor
        (gfs::set-cursor (gfs:handle cursor))
        1)
      (t
        (gfs::def-window-proc hwnd msg wparam lparam)))))

(defmethod process-message (hwnd (msg (eql gfs::+wm-rbuttondblclk+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-double hwnd lparam :right-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-rbuttondown+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-down hwnd lparam :right-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-rbuttonup+)) wparam lparam)
  (declare (ignore wparam))
  (process-mouse-message #'event-mouse-up hwnd lparam :right-button))

(defmethod process-message (hwnd (msg (eql gfs::+wm-activate+)) wparam lparam)
  (declare (ignore lparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (if widget
      (case wparam
        (#.gfs::+wa-active+      (event-activate   (dispatcher widget) widget))
        (#.gfs::+wa-clickactive+ (event-activate   (dispatcher widget) widget))
        (#.gfs::+wa-inactive+    (event-deactivate (dispatcher widget) widget)))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-killfocus+)) wparam lparam)
  (declare (ignore wparam lparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (if widget
      (event-focus-loss (dispatcher widget) widget)))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-setfocus+)) wparam lparam)
  (declare (ignore wparam lparam))
  (let ((widget (get-widget (thread-context) hwnd)))
    (if widget
      (event-focus-gain (dispatcher widget) widget)))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-getminmaxinfo+)) wparam lparam)
  (declare (ignore wparam))
  (let* ((tc (thread-context))
         (w (get-widget tc hwnd))
         (info-ptr (cffi:make-pointer (logand #xFFFFFFFF lparam))))
    (if (typep w 'top-level)
      (let ((max-size (maximum-size w))
            (min-size (minimum-size w)))
        (if max-size
          (cffi:with-foreign-slots ((gfs::x gfs::y)
                                    (cffi:foreign-slot-pointer info-ptr
                                                               'gfs::minmaxinfo
                                                               'gfs::maxtracksize)
                                    gfs::point)
            (setf gfs::x (gfs:size-width max-size)
                  gfs::y (gfs:size-height max-size))))
        (if min-size
          (cffi:with-foreign-slots ((gfs::x gfs::y)
                                    (cffi:foreign-slot-pointer info-ptr
                                                               'gfs::minmaxinfo
                                                               'gfs::mintracksize)
                                    gfs::point)
            (setf gfs::x (gfs:size-width min-size)
                  gfs::y (gfs:size-height min-size)))))))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-size+)) wparam lparam)
  (let* ((tc (thread-context))
         (w (get-widget tc hwnd))
         (type (cond
                 ((= wparam gfs::+size-maximized+) :maximized)
                 ((= wparam gfs::+size-minimized+) :minimized)
                 ((= wparam gfs::+size-restored+) :restored)
                 (t nil))))
    (record-raw-event tc hwnd msg wparam lparam)
    (when w
      (outer-size w (size-event-size tc))
      (event-resize (dispatcher w) w (size-event-size tc) type)))
  0)

(defmethod process-message (hwnd (msg (eql gfs::+wm-sizing+)) wparam lparam)
  (let* ((w (get-widget (thread-context) hwnd))
         (ptr (cffi:make-pointer (logand #xFFFFFFFF lparam)))
         (rect (cffi:convert-from-foreign ptr 'gfs::rect-pointer))
         (type (case wparam
                 (#.gfs::+wmsz-bottom+      :bottom)
                 (#.gfs::+wmsz-bottomleft+  :bottom-left)
                 (#.gfs::+wmsz-bottomright+ :bottom-right)
                 (#.gfs::+wmsz-left+        :left)
                 (#.gfs::+wmsz-right+       :right)
                 (#.gfs::+wmsz-top+         :top)
                 (#.gfs::+wmsz-topleft+     :top-left)
                 (#.gfs::+wmsz-topright+    :top-right))))
    (event-pre-resize (dispatcher w) w rect type)
    (cffi:with-foreign-slots ((gfs::left gfs::top gfs::right gfs::bottom) ptr gfs::rect)
      (let ((pnt (gfs:location rect))
            (size (gfs:size rect)))
        (setf gfs::left   (gfs:point-x pnt)
              gfs::top    (gfs:point-y pnt)
              gfs::right  (+ (gfs:point-x pnt) (gfs:size-width size))
              gfs::bottom (+ (gfs:point-y pnt) (gfs:size-height size))))))
  1)

#|
(defmethod process-message (hwnd (msg (eql gfs::+wm-nccalcsize+)) wparam lparam)
  (let ((widget (get-widget (thread-context) hwnd)))
    (cond
      ((typep widget 'dialog)
         (let ((retval (gfs::def-dlg-proc hwnd msg wparam lparam)))
           (if (status-bar-of widget)
             (setf retval (process-nccalcsize-message widget wparam lparam)))
           retval))
      ((typep widget 'top-level)
         (let ((retval (gfs::def-window-proc hwnd msg wparam lparam)))
           (if (status-bar-of widget)
             (setf retval (process-nccalcsize-message widget wparam lparam)))
           retval))
      (t
         (gfs::def-window-proc hwnd msg wparam lparam)))))
|#

(defmethod process-message (hwnd (msg (eql gfs::+wm-timer+)) wparam lparam)
  (declare (ignore lparam))
  (let* ((tc (thread-context))
         (timer (get-timer tc wparam)))
    (if (null timer)
      (gfs::kill-timer hwnd wparam)
      (cond
        ((<= (delay-of timer) 0)
          (event-timer (dispatcher timer) timer)
          (gfs:dispose timer))
        ((/= (delay-of timer) (initial-delay-of timer))
          (let ((delay (reset-timer-to-delay timer (delay-of timer))))
            (setf (slot-value timer 'delay) delay)
            (setf (slot-value timer 'initial-delay) delay))
          (event-timer (dispatcher timer) timer))
        (t
          (event-timer (dispatcher timer) timer)))))
  0)

;;;
;;; process-subclass-message methods
;;;

(defmethod process-subclass-message (hwnd msg wparam lparam)
  (gfs::call-window-proc (cffi:make-pointer (get-class-wndproc hwnd)) hwnd msg wparam lparam))

(defmethod process-subclass-message (hwnd (msg (eql gfs::+wm-destroy+)) wparam lparam)
  (declare (ignore wparam lparam))
  (delete-widget (thread-context) hwnd)
  (call-next-method))

;;;
;;; event-dispatcher methods
;;;

(defmethod gfs:dispose ((d event-source))
  (setf (dispatcher d) nil)
  (call-next-method))
