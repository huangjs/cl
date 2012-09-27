;;;;
;;;; thread-context.lisp
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

(defstruct event (hwnd (cffi:null-pointer)) (msg 0) (wparam 0) (lparam 0))

(defclass thread-context ()
  ((child-visitor-func        :initform nil :accessor child-visitor-func)
   (child-visitor-results     :initform nil :accessor child-visitor-results)
   (display-visitor-func      :initform nil :accessor display-visitor-func)
   (display-visitor-results   :initform nil :accessor display-visitor-results)
   (raw-event                 :initform (make-event) :reader raw-event)
   (job-table                 :initform (make-hash-table :test #'equal))
   (job-table-lock            :initform nil)
   (virtual-key               :initform 0 :accessor virtual-key)
   (items-by-id               :initform (make-hash-table :test #'equal))
   (message-filters           :initform nil :accessor message-filters)
   (mouse-event-pnt           :initform (gfs:make-point) :accessor mouse-event-pnt)
   (move-event-pnt            :initform (gfs:make-point) :accessor move-event-pnt)
   (next-item-id              :initform 10000 :reader next-item-id)
   (next-job-id               :initform 1 :reader next-job-id)
   (next-widget-id            :initform 100 :reader next-widget-id)
   (size-event-size           :initform (gfs:make-size) :accessor size-event-size)
   (widgets-by-hwnd           :initform (make-hash-table :test #'equal))
   (kbdnav-widgets            :initform nil :accessor kbdnav-widgets)
   (timers-by-id              :initform (make-hash-table :test #'equal))
   (top-level-visitor-func    :initform nil :accessor top-level-visitor-func)
   (top-level-visitor-results :initform nil :accessor top-level-visitor-results)
   (utility-hwnd              :initform (cffi:null-pointer) :accessor utility-hwnd)
   (menu-factories            :initform (make-hash-table :test #'eql))
   (window-factories          :initform (make-hash-table :test #'eql))
   (widget-in-progress        :initform nil :accessor widget-in-progress))
  (:documentation "Thread context objects maintain 'global' data for each thread possessing an event loop."))

;; TODO: change this when CLISP acquires MT support
;;
;; TODO: change this once we understand SBCL MT support
;;
#+(or clisp sbcl)
(defvar *the-thread-context* nil)

#+(or clisp sbcl)
(defun thread-context ()
  (when (null *the-thread-context*)
    (setf *the-thread-context* (make-instance 'thread-context))
    (handler-case
        (init-utility-hwnd *the-thread-context*)
      (gfs:win32-error (e)
        (setf *the-thread-context* nil)
        (format *error-output* "~a~%" e))))
  *the-thread-context*)

#+(or clisp sbcl)
(defun dispose-thread-context ()
  (let ((hwnd (utility-hwnd *the-thread-context*)))
    (unless (gfs:null-handle-p hwnd)
      (gfs::destroy-window hwnd)))
  (setf *the-thread-context* nil))

#+allegro
(eval-when (:compile-top-level :load-top-level :execute) (require :process))

#+allegro
(defun thread-context ()
  (let ((tc (getf (mp:process-property-list mp:*current-process*) 'thread-context)))
    (when (null tc)
      (setf tc (make-instance 'thread-context))
      (setf (getf (mp:process-property-list mp:*current-process*) 'thread-context) tc)
      (handler-case
          (init-utility-hwnd tc)
        (gfs:win32-error (e)
          (setf (getf (mp:process-property-list mp:*current-process*) 'thread-context) nil)
          (format *error-output* "~a~%" e))))
    tc))

#+allegro
(defun dispose-thread-context ()
  (let ((tc (getf (mp:process-property-list mp:*current-process*) 'thread-context)))
    (if tc
      (let ((hwnd (utility-hwnd tc)))
        (unless (gfs:null-handle-p hwnd)
          (gfs::destroy-window hwnd)))))
  (setf (getf (mp:process-property-list mp:*current-process*) 'thread-context) nil))


#+lispworks
(defun thread-context ()
  (let ((tc (getf (mp:process-plist mp:*current-process*) 'thread-context)))
    (when (null tc)
      (setf tc (make-instance 'thread-context))
      (setf (getf (mp:process-plist mp:*current-process*) 'thread-context) tc)
      (handler-case
          (init-utility-hwnd tc)
        (gfs:win32-error (e)
          (setf (getf (mp:process-plist mp:*current-process*) 'thread-context) nil)
          (format *error-output* "~a~%" e))))
    tc))

#+lispworks
(defun dispose-thread-context ()
  (let ((tc (getf (mp:process-plist mp:*current-process*) 'thread-context)))
    (if tc
      (let ((hwnd (utility-hwnd tc)))
        (unless (gfs:null-handle-p hwnd)
          (gfs::destroy-window hwnd)))))
  (setf (getf (mp:process-plist mp:*current-process*) 'thread-context) nil))

(defun init-utility-hwnd (tc)
  (register-toplevel-noerasebkgnd-window-class)
  (let ((hwnd (create-window "GraphicFormsTopLevelNoEraseBkgnd" ; can't use constant here
                             ""                                 ; because of circular dependency
                             (cffi:null-pointer)
                             (logior gfs::+ws-clipchildren+
                                     gfs::+ws-clipsiblings+
                                     gfs::+ws-border+
                                     gfs::+ws-popup+)
                             0)))
    (setf (slot-value tc 'utility-hwnd) hwnd)))
  
(defun call-child-visitor-func (tc parent child)
  (let ((func (child-visitor-func tc)))
    (if func
      (funcall func parent child)
      (warn 'gfs:toolkit-warning :detail "child visitor function is nil"))))

(defun call-display-visitor-func (tc hmonitor data)
  (let ((func (display-visitor-func tc)))
    (if func
      (funcall func hmonitor data)
      (warn 'gfs:toolkit-warning :detail "display visitor function is nil"))))

(defun call-top-level-visitor-func (tc win)
  (let ((func (top-level-visitor-func tc)))
    (if func
      (funcall func win)
      (warn 'gfs:toolkit-warning :detail "top-level visitor function is nil"))))

(defun get-widget (tc hwnd)
  "Return the widget object corresponding to the specified native window handle."
  (let ((tmp-widget (widget-in-progress tc)))
    (when tmp-widget
      (setf (slot-value tmp-widget 'gfs:handle) hwnd)
      (return-from get-widget tmp-widget)))
  (unless (gfs:null-handle-p hwnd)
    (gethash (cffi:pointer-address hwnd) (slot-value tc 'widgets-by-hwnd))))

(defun put-widget (tc w)
  "Add the specified widget to the widget table using its native handle as the key."
  (setf (gethash (cffi:pointer-address (gfs:handle w)) (slot-value tc 'widgets-by-hwnd)) w))

(defun delete-widget (tc hwnd)
  "Remove the widget object corresponding to the specified native window handle."
  (when (not (widget-in-progress tc))
    (remhash (cffi:pointer-address hwnd) (slot-value tc 'widgets-by-hwnd))))

(defun clear-widget-in-progress (tc)
  "Store the widget currently under construction."
  (setf (widget-in-progress tc) nil))

(defun put-kbdnav-widget (tc widget)
  (if (find :keyboard-navigation (style-of widget))
    (setf (kbdnav-widgets tc) (push widget (kbdnav-widgets tc)))))

(defun delete-kbdnav-widget (tc widget)
  (setf (kbdnav-widgets tc)
        (remove-if (lambda (hwnd) (cffi:pointer-eq (gfs:handle widget) hwnd))
                   (kbdnav-widgets tc)
                   :key #'gfs:handle)))

(defun intercept-kbdnav-message (tc msg-ptr)
  (let ((widgets (kbdnav-widgets tc)))
    (unless widgets
      (return-from intercept-kbdnav-message nil))
    (let ((widget (first widgets)))
      (if (/= (gfs::is-dialog-message (gfs:handle widget) msg-ptr) 0)
        (return-from intercept-kbdnav-message widget))
      (setf widget (find-if (lambda (w) (/= (gfs::is-dialog-message (gfs:handle w) msg-ptr)))
                            (rest widgets)))
      (when (and widget (/= (gfs::is-dialog-message (gfs:handle widget) msg-ptr) 0))
        (let ((tmp (delete-kbdnav-widget tc widget)))
          (setf (kbdnav-widgets tc) (push widget tmp)))
        (return-from intercept-kbdnav-message widget))))
  nil)

(defun get-item (tc id)
  "Returns the item identified by id."
  (gethash id (slot-value tc 'items-by-id)))

(defun put-item (tc it)
  "Stores an item using its id as the key."
  (setf (gethash (item-id it) (slot-value tc 'items-by-id)) it))

(defun delete-tc-item (tc it)
  "Removes the item using its id as the key."
  (maphash
    #'(lambda (k v)
        (declare (ignore v))
        (if (eql k (item-id it))
          (remhash k (slot-value tc 'items-by-id))))
    (slot-value tc 'items-by-id)))

(defun increment-item-id (tc)
  "Return the next menu item ID; also increment the internal value."
  (let ((id (next-item-id tc)))
    (incf (slot-value tc 'next-item-id))
    id))

(defun put-job (tc id closure)
  "Stores a closure using the specified ID for later retrieval."
  ;; FIXME: thread-safety
  (setf (gethash id (slot-value tc 'job-table)) closure))

(defun take-job (tc id)
  (let ((closure (gethash id (slot-value tc 'job-table))))
    (remhash id (slot-value tc 'job-table))
    closure))

(defun increment-job-id (tc)
  "Return the next job ID; also increment the internal value."
  (let ((id (next-job-id tc)))
    (incf (slot-value tc 'next-job-id))
    id))

(defun get-timer (tc id)
  "Returns the timer identified by the specified (system-defined) id."
  (gethash id (slot-value tc 'timers-by-id)))

(defun put-timer (tc timer)
  "Stores a timer using its id as the key."
  (setf (gethash (id-of timer) (slot-value tc 'timers-by-id)) timer))

(defun delete-timer (tc timer)
  "Removes the timer using its id as the key."
  (maphash
    #'(lambda (k v)
        (declare (ignore v))
        (if (eql k (id-of timer))
          (remhash k (slot-value tc 'timers-by-id))))
    (slot-value tc 'timers-by-id)))

(defun increment-widget-id (tc)
  "Return the next timer ID; also increment the internal value."
  (let ((id (next-widget-id tc)))
    (incf (slot-value tc 'next-widget-id))
    id))

(defun record-raw-event (tc hwnd msg wparam lparam)
  (let ((event (raw-event tc)))
    (setf (event-hwnd event)   hwnd
          (event-msg  event)   msg
          (event-wparam event) wparam
          (event-lparam event) lparam)
    event))

(defun get-menu-factory (tc menu-name)
  "Returns a function that creates a menu hierarchy based on a template defined via DEFMENU2."
  (gethash menu-name (slot-value tc 'menu-factories)))

(defun put-menu-factory (tc menu-name fn)
  "Stores a function that creates a menu hierarchy based on a template defined via DEFMENU2."
  (when menu-name
    (if (not (symbolp menu-name))
        (error 'gfs:toolkit-error :detail "the menu name must be a symbol"))
    (setf (gethash menu-name (slot-value tc 'menu-factories)) fn))
  fn)

(defun get-window-factory (tc win-name)
  "Returns a function that creates a window based on a template defined via DEFWINDOW."
  (gethash win-name (slot-value tc 'window-factories)))

(defun put-window-factory (tc win-name fn)
  "Stores a function that creates a window based on a template defined via DEFWINDOW."
  (when win-name
    (if (not (symbolp win-name))
        (error 'gfs:toolkit-error :detail "the window name must be a symbol"))
    (setf (gethash win-name (slot-value tc 'win-factories)) fn))
  fn)
