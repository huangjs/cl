;;;;
;;;; widget.lisp
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

(defun centered-coord-inside (ancest-coord ancest-size desc-size)
  (+ ancest-coord (floor (- (/ ancest-size 2) (/ desc-size 2)))))

(defun centered-coord-outside (ancest-coord ancest-size desc-size)
  (- ancest-coord (floor (- desc-size ancest-size) 2)))

(defun center-object (ancestor descendant)
  (let* ((ancest-size (client-size ancestor))
         (ancest-width (gfs:size-width ancest-size))
         (ancest-height (gfs:size-height ancest-size))
         (ancest-pnt (location ancestor))
         (desc-size (size descendant))
         (desc-width (gfs:size-width desc-size))
         (desc-height (gfs:size-height desc-size))
         (new-x 0)
         (new-y 0))
    (incf (gfs:point-y ancest-pnt) (- (gfs:size-height (size ancestor)) ancest-height))
    (if (> ancest-width desc-width)
      (setf new-x (centered-coord-inside (gfs:point-x ancest-pnt) ancest-width desc-width))
      (setf new-x (centered-coord-outside (gfs:point-x ancest-pnt) ancest-width desc-width)))
    (if (> ancest-height desc-height)
      (setf new-y (centered-coord-inside (gfs:point-y ancest-pnt) ancest-height desc-height))
      (setf new-y (centered-coord-outside (gfs:point-y ancest-pnt) ancest-height desc-height)))
    (setf (location descendant) (gfs:make-point :x new-x :y new-y))))

(defun cursor-of (widget)
  "Return the cursor assigned to widget."
  (if (gfs:disposed-p widget)
    (error 'gfs:disposed-error))
  (let ((cursor (slot-value widget 'cursor)))
    (if cursor
      (return-from cursor-of cursor)))
  (get-window-class-cursor (gfs:handle widget)))

(defun (setf cursor-of) (cursor widget)
  (if (gfs:disposed-p widget)
    (error 'gfs:disposed-error))
  (let ((old-cursor (slot-value widget 'cursor)))
    (if (and old-cursor (not (gfs:disposed-p old-cursor)))
      (gfs:dispose old-cursor)))
  (setf (slot-value widget 'cursor) cursor)
  (let ((capture-hwnd (gfs::get-capture)))
    (if (or (gfs:null-handle-p capture-hwnd)
            (cffi:pointer-eq capture-hwnd (gfs:handle widget)))
      (if cursor
        (gfs::set-cursor (gfs:handle cursor))
        (gfs::set-cursor (cffi:null-pointer))))))

(defmacro with-cursor ((widget &key file hotspot image system) &body body)
  (lispworks:with-unique-names (old new retval)
   `(let ((,old (slot-value ,widget 'cursor))
          (,new (make-instance 'gfg:cursor
                               :file ,file
                               :hotspot ,hotspot
                               :image ,image
                               :system ,system))
          (,retval nil))
      (setf (slot-value ,widget 'cursor) nil)
      (setf (cursor-of ,widget) ,new)
      (process-events)
      (unwind-protect
          (setf ,retval (progn ,@body))
        (setf (cursor-of ,widget) ,old))
      ,retval)))

(defmacro with-wait-cursor ((widget) &body body)
  `(with-cursor (,widget :system gfg:+wait-cursor+)
     ,@body))
;;;
;;; widget methods
;;;

(defmethod ancestor-p :before ((ancestor widget) (descendant widget))
  (if (or (gfs:disposed-p ancestor) (gfs:disposed-p descendant))
    (error 'gfs:disposed-error)))

(defmethod ancestor-p ((ancestor widget) (descendant widget))
  (let* ((parent-hwnd (gfs::get-ancestor (gfs:handle descendant) gfs::+ga-parent+))
         (parent (get-widget (thread-context) parent-hwnd)))
    (if (cffi:pointer-eq (gfs:handle ancestor) parent-hwnd)
      (return-from ancestor-p t))
    (if (null parent)
      (error 'gfs:toolkit-error :detail "no widget for parent handle"))
    (ancestor-p ancestor parent)))

(defmethod auto-hscroll-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod auto-vscroll-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod border-width :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod border-width ((self widget))
  (let ((bits (get-native-exstyle self)))
    (cond
      ((/= (logand bits gfs::+ws-ex-clientedge+) 0)
        (return-from border-width (gfs::get-system-metrics gfs::+sm-cxedge+)))
      ((/= (logand bits gfs::+ws-ex-dlgmodalframe+) 0)
        (return-from border-width (gfs::get-system-metrics gfs::+sm-cxdlgframe+)))
      ((/= (logand bits gfs::+ws-ex-staticedge+) 0)
        (return-from border-width (gfs::get-system-metrics gfs::+sm-cxborder+)))
      ((/= (logand bits gfs::+ws-ex-windowedge+) 0)
        (return-from border-width (gfs::get-system-metrics gfs::+sm-cxdlgframe+))))
    (when (test-native-style self gfs::+ws-border+)
      (return-from border-width (gfs::get-system-metrics gfs::+sm-cxborder+)))
    0))

(defmethod center-on-owner :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod center-on-owner ((self widget))
  (let ((owner (owner self)))
    (if (null owner)
      nil
      (center-object owner self))))

(defmethod center-on-parent :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod center-on-parent ((self widget))
  (center-object (parent self) self))

(defmethod check :before ((self widget) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod checked-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod checked-p ((self widget))
  nil)

(defmethod client-size :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod client-size ((self widget))
  (cffi:with-foreign-object (wi-ptr 'gfs::windowinfo)
    (cffi:with-foreign-slots ((gfs::cbsize
                               gfs::clientleft
                               gfs::clienttop
                               gfs::clientright
                               gfs::clientbottom)
                              wi-ptr gfs::windowinfo)
      (setf gfs::cbsize (cffi::foreign-type-size 'gfs::windowinfo))
      (when (zerop (gfs::get-window-info (gfs:handle self) wi-ptr))
        (error 'gfs:win32-error :detail "get-window-info failed"))
      (gfs:make-size :width (- gfs::clientright gfs::clientleft)
                     :height (- gfs::clientbottom gfs::clienttop)))))

(defmethod copy-text :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod cut-text :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod delete-all :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod delete-span :before ((self widget) span)
  (declare (ignore span))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod gfs:dispose ((self widget))
  (if (gfs:disposed-p self)
    (warn 'gfs:toolkit-warning :detail "widget already disposed"))
  (unless (null (slot-value self 'cursor))
    (gfs:dispose (slot-value self 'cursor)))
  (unless (null (dispatcher self))
    (event-dispose (dispatcher self) self))
  (let ((hwnd (gfs:handle self)))
    (if (not (gfs:null-handle-p hwnd))
      (if (zerop (gfs::destroy-window hwnd))
        (error 'gfs:win32-error :detail "destroy-window failed"))))
  (setf (slot-value self 'gfs:handle) nil))

(defmethod enable :before ((self widget) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod enable ((self widget) flag)
  (gfs::enable-window (gfs:handle self) (if (null flag) 0 1)))

(defmethod enable-auto-scrolling :before ((self widget) hscroll vscroll)
  (declare (ignore hscroll vscroll))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod enabled-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod enable-redraw :before ((self widget) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod enable-redraw ((self widget) flag)
  (gfs::send-message (gfs:handle self) gfs::+wm-setredraw+ (if flag 1 0) 0)
  (if flag
    (redraw self)))

(defmethod enable-scrollbars :before ((self widget) horizontal vertical)
  (declare (ignore horizontal vertical))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod enabled-p ((self widget))
  (/= (gfs::is-window-enabled (gfs:handle self)) 0))

(defmethod gfg:font :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod horizontal-scrollbar-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod horizontal-scrollbar-p ((self widget))
  nil)

(defmethod image :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf image) :before (image (self widget))
  (declare (ignore image))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod initialize-instance :after ((self widget) &key style &allow-other-keys)
  (setf (slot-value self 'style) (if (listp style) style (list style))))

(defmethod location :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod location ((self widget))
  (cffi:with-foreign-object (wi-ptr 'gfs::windowinfo)
    (cffi:with-foreign-slots ((gfs::cbsize
                               gfs::clientleft
                               gfs::clienttop)
                              wi-ptr gfs::windowinfo)
      (setf gfs::cbsize (cffi::foreign-type-size 'gfs::windowinfo))
      (when (zerop (gfs::get-window-info (gfs:handle self) wi-ptr))
        (error 'gfs:win32-error :detail "get-window-info failed"))
      (cffi:with-foreign-object (pnt-ptr 'gfs::point)
        (cffi:with-foreign-slots ((gfs::x gfs::y)
                                 pnt-ptr gfs::point)
          (setf gfs::x gfs::clientleft)
          (setf gfs::y gfs::clienttop)
          (gfs::screen-to-client (gfs:handle self) pnt-ptr)
          (gfs:make-point :x gfs::x :y gfs::y))))))

(defmethod (setf location) :before ((pnt gfs:point) (self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf location) ((pnt gfs:point) (self widget))
  (if (zerop (gfs::set-window-pos (gfs:handle self)
                                   (cffi:null-pointer)
                                   (gfs:point-x pnt)
                                   (gfs:point-y pnt)
                                   0 0
                                   (logior gfs::+swp-nosize+ gfs::+swp-nozorder+)))
    (error 'gfs:win32-error :detail "set-window-pos failed")))

(defmethod owner ((self widget))
  ;; I know the following is confusing, but the docs
  ;; for MSDN state that GetParent() returns the owner
  ;; when the window in question is a top-level,
  ;; whereas for child windows the owner and parent
  ;; are the same.
  ;;
  ;; And since GetParent() can return owners, this
  ;; means it can return NULL, too.
  ;;
  (let ((hwnd (gfs::get-parent (gfs:handle self))))
    (if (gfs:null-handle-p hwnd)
      nil
      (get-widget (thread-context) hwnd))))

(defmethod pack :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod pack ((self widget))
  (setf (size self) (preferred-size self -1 -1)))

(defmethod parent ((self widget))
  ;; Unlike the owner method, this method should
  ;; only return nil if self is the root window,
  ;; which is taken care of by a specialization
  ;; on root-window (see root-window.lisp).
  ;;
  (let* ((hwnd (gfs::get-ancestor (gfs:handle self) gfs::+ga-parent+))
         (widget (get-widget (thread-context) hwnd)))
    (when (null widget)
      (if (cffi:pointer-eq hwnd (gfs::get-desktop-window))
        (setf widget (make-instance 'root-window :handle hwnd))
        (error 'gfs:toolkit-error :detail "no widget for hwnd")))
    widget))

(defmethod paste-text :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod print-object ((self widget) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "handle: ~x " (gfs:handle self))
    (format stream "dispatcher: ~a" (dispatcher self))))

(defmethod redo-available-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod redo-available-p ((self widget))
  nil)

(defmethod redraw :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod redraw ((self widget))
  (let ((hwnd (gfs:handle self)))
    (unless (gfs:null-handle-p hwnd)
      (gfs::invalidate-rect hwnd (cffi:null-pointer) 1))))

(defmethod resizable-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod resizable-p ((self widget))
  nil)

(defmethod scroll :before ((self widget) delta-x delta-y children-p millis)
  (declare (ignore delta-x delta-y children-p millis))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod select :before ((self widget) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod select-all :before ((self widget) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod selected-count :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod selected-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod selected-p ((self widget))
  nil)

(defmethod selected-span :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod selected-span ((self widget))
  nil)

(defmethod (setf selected-span) :before (span (self widget))
  (declare (ignore span))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod size :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod size ((self widget))
  (client-size self))

(defmethod (setf size) :before ((size gfs:size) (self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf size) ((size gfs:size) (self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (if (zerop (gfs::set-window-pos (gfs:handle self)
                                   (cffi:null-pointer)
                                   0 0
                                   (gfs:size-width size)
                                   (gfs:size-height size)
                                   (logior gfs::+swp-nomove+ gfs::+swp-nozorder+)))
    (error 'gfs:win32-error :detail "set-window-pos failed"))
  size)

(defmethod show :before ((self widget) flag)
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod show ((self widget) flag)
  (gfs::show-window (gfs:handle self) (if flag gfs::+sw-shownormal+ gfs::+sw-hide+)))

(defmethod text-baseline :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod text-for-pasting-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod text-for-pasting-p ((self widget))
  nil)

(defmethod (setf text-modified-p) :before (flag (self widget))
  (declare (ignore flag))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod text-modified-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod undo-available-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod undo-available-p ((self widget))
  nil)

(defmethod update :before ((w widget))
  (if (gfs:disposed-p w)
    (error 'gfs:disposed-error)))

(defmethod update ((self widget))
  (let ((hwnd (gfs:handle self)))
    (unless (gfs:null-handle-p hwnd)
      (gfs::update-window hwnd))))

(defmethod update-native-style :before ((self widget) bits)
  (declare (ignore bits))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod vertical-scrollbar-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod vertical-scrollbar-p ((self widget))
  nil)

(defmethod visible-p :before ((self widget))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod visible-p ((self widget))
  (/= (gfs::is-window-visible (gfs:handle self)) 0))
