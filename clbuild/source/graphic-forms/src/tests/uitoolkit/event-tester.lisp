;;;;
;;;; event-tester.lisp
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

(in-package #:graphic-forms.uitoolkit.tests)

(defparameter *event-tester-window* nil)
(defparameter *event-tester-text* "Hello!")
(defvar *event-counter* 0)
(defvar *mouse-down-flag* nil)
(defvar *timer* nil)

(defun exit-event-tester ()
  (let ((w *event-tester-window*))
    (setf *event-tester-window* nil)
    (gfs:dispose w))
  (gfw:shutdown 0))

(defclass event-tester-window-events (gfw:event-dispatcher) ())

(defmethod gfw:event-paint ((d event-tester-window-events) window gc rect)
  (declare (ignore rect))
  (setf (gfg:background-color gc) gfg:*color-white*)
  (setf (gfg:foreground-color gc) gfg:*color-blue*)
  (let* ((sz (gfw:client-size window))
         (pnt (gfs:make-point :x 0 :y (floor (gfs:size-height sz) 2))))
    (gfg:draw-text gc *event-tester-text* pnt)))

(defmethod gfw:event-close ((d event-tester-window-events) widget)
  (declare (ignore widget))
  (exit-event-tester))

(defun initialize-scrollbars ()
  ;; yucky test code to set scrollbar parameters -- this
  ;; is not how applications will be expected to do it.
  ;;
  (cffi:with-foreign-object (info-ptr 'gfs::scrollinfo)
    (gfs::zero-mem info-ptr gfs::scrollinfo)
    (cffi:with-foreign-slots ((gfs::cbsize gfs::fmask gfs::maxpos gfs::pagesize)
                              info-ptr gfs::scrollinfo)
      (setf gfs::cbsize   (cffi:foreign-type-size 'gfs::scrollinfo)
            gfs::fmask    (logior gfs::+sif-page+ gfs::+sif-range+ gfs::+sif-disablenoscroll+)
            gfs::maxpos   500
            gfs::pagesize 50))
    (gfs::set-scroll-info (gfs:handle *event-tester-window*) gfs::+sb-horz+ info-ptr 0)
    (gfs::set-scroll-info (gfs:handle *event-tester-window*) gfs::+sb-vert+ info-ptr 0)))

(defun update-scrollbars (axis detail)
  ;; yucky test code to set scrollbar parameters -- this
  ;; is not how applications will be expected to do it.
  ;;
  (let ((which-sb (if (eql axis :vertical) gfs::+sb-vert+ gfs::+sb-horz+))
        (hwnd (gfs:handle *event-tester-window*)))
    (cffi:with-foreign-object (info-ptr 'gfs::scrollinfo)
      (gfs::zero-mem info-ptr gfs::scrollinfo)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::fmask gfs::pos gfs::pagesize
                                 gfs::minpos gfs::maxpos gfs::trackpos)
                                info-ptr gfs::scrollinfo)
        (setf gfs::cbsize   (cffi:foreign-type-size 'gfs::scrollinfo)
              gfs::fmask    gfs::+sif-all+)
        (gfs::get-scroll-info hwnd which-sb info-ptr)
        (case detail
          (:start        (setf gfs::pos gfs::minpos))
          (:end          (setf gfs::pos gfs::maxpos))
          (:step-back    (setf gfs::pos (- gfs::pos 5)))
          (:step-forward (setf gfs::pos (+ gfs::pos 5)))
          (:page-back    (setf gfs::pos (- gfs::pos gfs::pagesize)))
          (:page-forward (setf gfs::pos (+ gfs::pos gfs::pagesize)))
          (:thumb-track  (setf gfs::pos gfs::trackpos)))
        (gfs::set-scroll-info hwnd which-sb info-ptr 1)))))

(defun text-for-modifiers ()
  (format nil
          "~:[SHIFT~;~] ~:[CTRL~;~] ~:[ALT~;~] ~:[L-WIN~;~] ~:[R-WIN~;~] ~:[ESC~;~] ~:[CAPSLOCK~;~] ~:[NUMLOCK~;~] ~:[SCROLLOCK~;~]"
          (not (gfw:key-down-p gfw:+vk-shift+))
          (not (gfw:key-down-p gfw:+vk-control+))
          (not (gfw:key-down-p gfw:+vk-alt+))
          (not (gfw:key-down-p gfw:+vk-left-win+))
          (not (gfw:key-down-p gfw:+vk-right-win+))
          (not (gfw:key-toggled-p gfw:+vk-escape+))
          (not (gfw:key-toggled-p gfw:+vk-caps-lock+))
          (not (gfw:key-toggled-p gfw:+vk-num-lock+))
          (not (gfw:key-toggled-p gfw:+vk-scroll-lock+))))

(defun text-for-activation (action)
  (format nil
          "~a action: ~s  time: 0x~x  ~s"
          (incf *event-counter*)
          action
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-mouse (action button pnt)
  (format nil
          "~a mouse action: ~s  button: ~a  point: (~d,~d)  time: 0x~x  ~s"
          (incf *event-counter*)
          action
          button
          (gfs:point-x pnt)
          (gfs:point-y pnt)
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-key (action key-code char)
  (format nil
          "~a key action: ~s  char: ~s  code: 0x~x  time: 0x~x  ~s"
          (incf *event-counter*)
          action
          char
          key-code
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-item (text desc)
  (format nil
          "~a ~s: ~s  time: 0x~x  ~s"
          (incf *event-counter*)
          desc
          text
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-size (type size)
  (format nil
          "~a resize action: ~s  size: (~d,~d)  time: 0x~x  ~s"
          (incf *event-counter*)
          (symbol-name type)
          (gfs:size-width size)
          (gfs:size-height size)
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-move (pnt)
  (format nil
          "~a move  point: (~d,~d)  time: 0x~x  ~s"
          (incf *event-counter*)
          (gfs:point-x pnt)
          (gfs:point-y pnt)
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-timer ()
  (format nil
          "~a timer tick id: ~d  time: 0x~x  ~s"
          (incf *event-counter*)
          (gfw:id-of *timer*)
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defun text-for-scroll (axis detail)
  (format nil
          "~a scroll: ~s  detail: ~s  time: 0x~x  ~s"
          (incf *event-counter*)
          axis
          detail
          (gfw:obtain-event-time)
          (text-for-modifiers)))

(defmethod gfw:event-activate ((d event-tester-window-events) window)
  (setf *event-tester-text* (text-for-activation "window activated"))
  (gfw:redraw window))

(defmethod gfw:event-deactivate ((d event-tester-window-events) window)
  (setf *event-tester-text* (text-for-activation "window deactivated"))
  (gfw:redraw window))

(defmethod gfw:event-key-down ((d event-tester-window-events) window key-code char)
  (setf *event-tester-text* (text-for-key "down" key-code char))
  (gfw:redraw window))

(defmethod gfw:event-key-up ((d event-tester-window-events) window key-code char)
  (setf *event-tester-text* (text-for-key "up" key-code char))
  (gfw:redraw window))

(defmethod gfw:event-mouse-double ((d event-tester-window-events) window pnt button)
  (setf *event-tester-text* (text-for-mouse "double" button pnt))
  (gfw:redraw window))

(defmethod gfw:event-mouse-down ((d event-tester-window-events) window pnt button)
  (setf *event-tester-text* (text-for-mouse "down" button pnt))
  (setf *mouse-down-flag* t)
  (gfw:redraw window))

(defmethod gfw:event-mouse-move ((d event-tester-window-events) window pnt button)
  (when *mouse-down-flag*
    (setf *event-tester-text* (text-for-mouse "move" button pnt))
    (gfw:redraw window)))

(defmethod gfw:event-mouse-up ((d event-tester-window-events) window pnt button)
  (setf *event-tester-text* (text-for-mouse "up" button pnt))
  (setf *mouse-down-flag* nil)
  (gfw:redraw window))

(defmethod gfw:event-move ((d event-tester-window-events) window pnt)
  (setf *event-tester-text* (text-for-move pnt))
  (gfw:redraw window))

(defmethod gfw:event-resize ((d event-tester-window-events) window size type)
  (setf *event-tester-text* (text-for-size type size))
  (gfw:redraw window))

(defmethod gfw:event-scroll ((d event-tester-window-events) window axis detail)
  (update-scrollbars axis detail)
  (setf *event-tester-text* (text-for-scroll axis detail))
  (gfw:redraw window))

(defclass event-tester-exit-dispatcher (gfw:event-dispatcher) ())

(defmethod gfw:event-select ((d event-tester-exit-dispatcher) item)
  (declare (ignore item))
  (exit-event-tester))

(defmethod gfw:event-arm ((d event-tester-exit-dispatcher) item)
  (setf *event-tester-text* (text-for-item (gfw:text item) "item armed"))
  (gfw:redraw *event-tester-window*))

(defclass event-tester-echo-dispatcher (gfw:event-dispatcher) ())

(defmethod gfw:event-select ((d event-tester-echo-dispatcher) item)
  (setf *event-tester-text* (text-for-item (gfw:text item) "item selected"))
  (gfw:redraw *event-tester-window*))

(defmethod gfw:event-arm ((d event-tester-echo-dispatcher) item)
  (setf *event-tester-text* (text-for-item (gfw:text item) "item armed"))
  (gfw:redraw *event-tester-window*))

(defmethod gfw:event-activate ((d event-tester-echo-dispatcher) widget)
  (setf *event-tester-text* (text-for-item (format nil "~a" widget) "menu activated"))
  (gfw:redraw *event-tester-window*))

(defmethod gfw:event-timer ((disp event-tester-echo-dispatcher) timer)
  (declare (ignore timer))
  (setf *event-tester-text* (text-for-timer))
  (gfw:redraw *event-tester-window*))

(defun manage-file-menu (disp menu)
  (declare (ignore disp))
  (let ((item (elt (gfw:items-of menu) 0)))
    (setf (gfw:text item) (if *timer* "Sto&p Timer" "&Start Timer"))))

(defun manage-timer (disp item)
  (declare (ignore disp item))
  (if *timer*
    (progn
      (gfw:enable *timer* nil)
      (setf *timer* nil)
      (setf *event-tester-text* "timer stopped by user"))
    (progn
      (setf *timer* (make-instance 'gfw:timer :delay 1000 :dispatcher (make-instance 'event-tester-echo-dispatcher)))
      (gfw:enable *timer* t)
      (setf *event-tester-text* (format nil
                                        "timer ~d started init delay: ~d delay ~d"
                                        (gfw:id-of *timer*)
                                        (gfw:initial-delay-of *timer*)
                                        (gfw:delay-of *timer*)))))
  (gfw:redraw *event-tester-window*))

(defun event-tester-internal ()
  (setf *event-tester-text* "Hello!")
  (setf *event-counter* 0)
  (let ((echo-md (make-instance 'event-tester-echo-dispatcher))
        (exit-md (make-instance 'event-tester-exit-dispatcher))
        (menu-factory nil))
    (setf *event-tester-window* (make-instance 'gfw:top-level :dispatcher (make-instance 'event-tester-window-events)
                                                              :style '(:workspace :horizontal-scrollbar :vertical-scrollbar)))
    (initialize-scrollbars)
    (setf menu-factory
          (gfw:defmenu2 :name 'event-tester-menu
                        :menu ((:item "&File" :callback #'manage-file-menu
                                              :submenu ((:item "Timer" :callback #'manage-timer)
                                                        (:item "" :separator)
                                                        (:item "E&xit" :dispatcher exit-md)))
                               (:item "&Test Menu" :dispatcher echo-md
                                                   :submenu ((:item "&Checked Item" :checked :dispatcher echo-md)
                                                             (:item "&Submenu" :dispatcher echo-md 
                                                                               :submenu ((:item "&Item A" :dispatcher echo-md :disabled)
                                                                                         (:item "&Item B" :dispatcher echo-md)))))
                               (:item "&Help" :dispatcher echo-md
                                              :submenu ((:item "&About" :dispatcher echo-md))))))
    (setf (gfw:menu-bar *event-tester-window*) (gfw:make-menu 'event-tester-menu))
    (setf (gfw:image *event-tester-window*) (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico")))
    (gfw:show *event-tester-window* t)))

(defun event-tester ()
  (gfw:startup "Event Tester" #'event-tester-internal))
