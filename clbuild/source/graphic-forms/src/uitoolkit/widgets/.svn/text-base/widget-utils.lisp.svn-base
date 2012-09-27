;;;;
;;;; widget-utils.lisp
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

(in-package #:graphic-forms.uitoolkit.widgets)

(defvar *check-box-size* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-graphics-context ((gc &optional thing) &body body)
    (let ((tmp-thing (gensym)))
     `(let* ((,tmp-thing ,thing)
             (,gc (cond
                    ((null ,tmp-thing)
                       (make-instance 'gfg:graphics-context)) ; DC compatible with display
                    ((typep ,tmp-thing 'gfw:widget)
                       (make-instance 'gfg:graphics-context :widget ,tmp-thing))
                    ((typep ,tmp-thing 'gfg:image)
                       (make-instance 'gfg:graphics-context :image ,tmp-thing))
                    (t
                       (error 'gfs:toolkit-error
                              :detail (format nil "~a is an unsupported type" ,tmp-thing))))))
         (unwind-protect
             (progn
               ,@body)
           (gfs:dispose ,gc)))))

  (defmacro with-drawing-disabled ((widget) &body body)
    ;; FIXME: should this macro use enable-redraw instead?
    ;; One immediate problem is that only one window can be
    ;; locked at a time by LockWindowUpdate.
    ;;
    (let ((tmp-widget (gensym)))
      `(let ((,tmp-widget ,widget))
         (unwind-protect
           (progn
             (unless (gfs:disposed-p ,tmp-widget)
               (error 'gfs:disposed-error))
             (gfs::lock-window-update (gfs:handle ,tmp-widget))
             ,@body)
         (gfs::lock-window-update (cffi:null-pointer)))))))

(defun translate-and-dispatch (msg-ptr)
  (gfs::translate-message msg-ptr)
  (gfs::dispatch-message msg-ptr))

(defun default-message-filter (gm-code msg-ptr)
  (cond
    ((zerop gm-code)
       (dispose-thread-context)
       t)
    ((= gm-code -1)
       (warn 'gfs:win32-warning :detail "get-message failed")
       t)
    ((intercept-kbdnav-message (thread-context) msg-ptr)
       nil)
    (t
       (translate-and-dispatch msg-ptr)
       nil)))

#+(or clisp sbcl)
(defun startup (thread-name start-fn)
  (declare (ignore thread-name))
  (funcall start-fn)
  (message-loop #'default-message-filter))

#+allegro
(eval-when (:compile-top-level :load-top-level :execute) (require :process))

#+allegro
(defun startup (thread-name start-fn)
  (mp:process-run-function thread-name
                           (lambda ()
                             (funcall start-fn)
                             (message-loop #'default-message-filter))))

#+lispworks
(defun startup (thread-name start-fn)
  (hcl:add-special-free-action 'gfs::native-object-special-action)
  (if (null (mp:list-all-processes))
    (mp:initialize-multiprocessing))
  (mp:process-run-function thread-name
                           nil
                           (lambda ()
                             (funcall start-fn)
                             (message-loop #'default-message-filter))))

(defun shutdown (exit-code)
  (gfs::post-quit-message exit-code))

(defun translate-point (widget system pnt)
  (if (gfs:disposed-p widget)
    (error 'gfs:disposed-error))
  (multiple-value-bind (ptr params)
      (cffi:convert-to-foreign pnt 'gfs:point)
    (ecase system
      (:client (if (zerop (gfs::screen-to-client (gfs:handle widget) ptr))
                 (error 'gfs:win32-error :detail "screen-to-client failed")))
      (:display (if (zerop (gfs::client-to-screen (gfs:handle widget) ptr))
                  (error 'gfs::win32-error :detail "client-to-screen failed"))))
    (let ((pnt (cffi:convert-from-foreign ptr 'gfs:point)))
      (cffi:free-converted-object ptr 'gfs:point params)
      pnt)))

(declaim (inline show-cursor))
(defun show-cursor (flag)
  (gfs::show-cursor (if flag 1 0)))

(defun obtain-pointer-location ()
  (cffi:with-foreign-object (ptr 'gfs:point)
    (cffi:with-foreign-slots ((gfs::x gfs::y) ptr gfs:point)
      (when (zerop (gfs::get-cursor-pos ptr))
        (warn 'gfs:win32-warning :detail "get-cursor-pos failed")
        (return-from obtain-pointer-location (gfs:make-point)))
      (gfs:make-point :x gfs::x :y gfs::y))))

(defun create-window (class-name title parent-hwnd std-style ex-style &optional child-id)
  (cffi:with-foreign-string (cname-ptr class-name)
    (cffi:with-foreign-string (title-ptr title)
      (let ((hwnd (gfs::create-window
                    ex-style
                    cname-ptr
                    title-ptr
                    std-style
                    gfs::+cw-usedefault+
                    gfs::+cw-usedefault+
                    gfs::+cw-usedefault+
                    gfs::+cw-usedefault+
                    parent-hwnd
                    (if (zerop (logand gfs::+ws-child+ std-style))
                      (cffi:null-pointer)
                      (cffi:make-pointer (or child-id (increment-widget-id (thread-context)))))
                    (cffi:null-pointer)
                    0)))
        (if (gfs:null-handle-p hwnd)
          (error 'gfs:win32-error :detail "create-window failed"))
        hwnd))))

(defun show-common-dialog (dlg dlg-func)
  (let* ((struct-ptr (gfs:handle dlg))
         (retval (funcall dlg-func struct-ptr)))
    (if (and (zerop retval) (/= (gfs::comm-dlg-extended-error) 0))
      (error 'gfs:comdlg-error :detail (format nil "~a failed" (symbol-name dlg-func))))
    retval))

(defun get-widget-text (widget)
  (if (gfs:disposed-p widget)
    (error 'gfs:disposed-error))
  (let* ((hwnd (gfs:handle widget))
         (length (gfs::get-window-text-length hwnd)))
    (if (zerop length)
      ""
      (cffi:with-foreign-pointer-as-string (str-ptr (1+ length))
        (gfs::get-window-text hwnd str-ptr (1+ length))))))

(defun outer-location (w pnt)
  (cffi:with-foreign-object (wi-ptr 'gfs::windowinfo)
    (cffi:with-foreign-slots ((gfs::cbsize
                               gfs::windowleft
                               gfs::windowtop)
                              wi-ptr gfs::windowinfo)
      (setf gfs::cbsize (cffi::foreign-type-size 'gfs::windowinfo))
      (when (zerop (gfs::get-window-info (gfs:handle w) wi-ptr))
        (error 'gfs:win32-error :detail "get-window-info failed"))
      (setf (gfs:point-x pnt) gfs::windowleft)
      (setf (gfs:point-y pnt) gfs::windowtop))))

(defun outer-size (w sz)
  (cffi:with-foreign-object (wi-ptr 'gfs::windowinfo)
    (cffi:with-foreign-slots ((gfs::cbsize
                               gfs::windowleft
                               gfs::windowtop
                               gfs::windowright
                               gfs::windowbottom)
                              wi-ptr gfs::windowinfo)
      (setf gfs::cbsize (cffi::foreign-type-size 'gfs::windowinfo))
      (when (zerop (gfs::get-window-info (gfs:handle w) wi-ptr))
        (error 'gfs:win32-error :detail "get-window-info failed"))
      (setf (gfs:size-width sz) (- gfs::windowright gfs::windowleft))
      (setf (gfs:size-height sz) (- gfs::windowbottom gfs::windowtop)))))

(defun horizontal-scrollbar-height ()
  (gfs::get-system-metrics gfs::+sm-cyhscroll+))

(defun horizontal-scrollbar-arrow-width ()
  (gfs::get-system-metrics gfs::+sm-cxhscroll+))

(defun vertical-scrollbar-arrow-height ()
  (gfs::get-system-metrics gfs::+sm-cyvscroll+))

(defun vertical-scrollbar-width ()
  (gfs::get-system-metrics gfs::+sm-cxvscroll+))

(defun set-widget-text (w str)
  (if (gfs:disposed-p w)
    (error 'gfs:disposed-error))
  (gfs::set-window-text (gfs:handle w) str))

(defun widget-text-size (widget text-func dt-flags)
  (let ((hwnd (gfs:handle widget))
        (hfont nil))
    (gfs::with-retrieved-dc (hwnd hdc)
      (setf hfont (cffi:make-pointer (gfs::send-message hwnd gfs::+wm-getfont+ 0 0)))
      (gfs::with-hfont-selected (hdc hfont)
        (gfg::text-bounds hdc (funcall text-func widget) dt-flags 0)))))

;;;
;;; This algorithm adapted from the calculate_best_bounds()
;;; function in ui_core_implementation.cpp from the
;;; Adobe Source Libraries / UI Core Widget API
;;;
(defun widget-text-baseline (widget top-margin)
  (let ((size (gfw:size widget))
        (b-width (border-width widget))
        (font (gfg:font widget)))
    (with-graphics-context (gc widget)
      (let ((metrics (gfg:metrics gc font)))
        (+ b-width
           top-margin
           (gfg:ascent metrics)
           (floor (- (gfs:size-height size)
                     (+ (gfg:ascent metrics) (gfg:descent metrics)))
                  2))))))

(defun check-box-size ()
  (if *check-box-size*
    (return-from check-box-size (gfs:copy-size *check-box-size*)))
  (let ((hbitmap (gfs::load-bitmap (cffi:null-pointer)
                                   (cffi:make-pointer gfs::+obm-checkboxes+))))
    (if (gfs:null-handle-p hbitmap)
      ;; if for some reason the OBM_CHECKBOXES resource could not be retrieved,
      ;; use scrollbar system metric values as a rough approximation
      ;;
      (return-from check-box-size
                   (gfs:make-size :width  (vertical-scrollbar-width)
                                  :height (vertical-scrollbar-arrow-height))))

    (unwind-protect
        (cffi:with-foreign-object (bm-ptr 'gfs::bitmap)
          (cffi:with-foreign-slots ((gfs::width gfs::height) bm-ptr gfs::bitmap)
            (gfs::get-object hbitmap (cffi:foreign-type-size 'gfs::bitmap) bm-ptr)
            (setf *check-box-size* (gfs:make-size :width (floor gfs::width 4)
                                                  :height (floor gfs::height 3)))))
      (gfs::delete-object hbitmap)))
  (gfs:copy-size *check-box-size*))

(defun extract-foreign-strings (buffer)
  (let ((strings nil))
    (do ((curr-ptr buffer))
        ((zerop (cffi:mem-ref curr-ptr :char)))
      (let ((tmp (cffi:foreign-string-to-lisp curr-ptr)))
        (push tmp strings)
        (setf curr-ptr (cffi:make-pointer (+ (cffi:pointer-address curr-ptr) (1+ (length tmp)))))))
    (reverse strings)))

(defun collect-foreign-strings (strings)
  (let* ((total-size (1+ (loop for str in strings
                               sum (1+ (length (namestring str))))))
         (buffer (cffi:foreign-alloc :char :initial-element 0 :count total-size))
         (curr-addr (cffi:pointer-address buffer)))
    (loop for str in strings
          do (let* ((tmp-str (namestring str))
                    (str-len (1+ (length tmp-str))))
               (cffi:lisp-string-to-foreign tmp-str (cffi:make-pointer curr-addr) str-len)
               (incf curr-addr str-len)))
    buffer))

(defun constrain-new-size (new-size current-size compare-fn)
  (let ((new-width (funcall compare-fn (gfs:size-width new-size) (gfs:size-width current-size)))
        (new-height (funcall compare-fn (gfs:size-height new-size) (gfs:size-height current-size))))
    (gfs:make-size :width new-width :height new-height)))

(defun get-native-style (widget)
  (gfs::get-window-long (gfs:handle widget) gfs::+gwl-style+))

(defun get-native-exstyle (widget)
  (gfs::get-window-long (gfs:handle widget) gfs::+gwl-exstyle+))

(defun test-native-style (widget bits)
  (/= (logand (gfs::get-window-long (gfs:handle widget) gfs::+gwl-style+) bits) 0))

(defun test-native-exstyle (widget bits)
  (/= (logand (gfs::get-window-long (gfs:handle widget) gfs::+gwl-exstyle+) bits) 0))
