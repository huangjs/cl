;;;;
;;;; menu.lisp
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

(defun append-menuitem (hmenu mid label hbmp hchildmenu disabled checked)
  (declare (ignore hbmp)) ; FIXME: ignore hbmp until we support images in menu items
  (let ((info-mask (logior gfs::+miim-id+
                           (if label (logior gfs::+miim-state+ gfs::+miim-string+) gfs::+miim-ftype+)
                           (if hchildmenu gfs::+miim-submenu+ 0)))
        (info-type (if label 0 gfs::+mft-separator+))
        (info-state (logior (if checked gfs::+mfs-checked+ 0)
                            (if disabled gfs::+mfs-disabled+ 0))))
    (cffi:with-foreign-object (mii-ptr 'gfs::menuiteminfo)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::mask gfs::type
                                 gfs::state gfs::id gfs::hsubmenu
                                 gfs::hbmpchecked gfs::hbmpunchecked
                                 gfs::idata gfs::tdata gfs::cch
                                 gfs::hbmpitem)
                                mii-ptr gfs::menuiteminfo)
        (setf gfs::cbsize        (cffi:foreign-type-size 'gfs::menuiteminfo)
              gfs::mask          info-mask
              gfs::type          info-type
              gfs::state         info-state
              gfs::id            mid
              gfs::hsubmenu      hchildmenu
              gfs::hbmpchecked   (cffi:null-pointer)
              gfs::hbmpunchecked (cffi:null-pointer)
              gfs::idata         0
              gfs::tdata         (cffi:null-pointer))
        (if label
          (cffi:with-foreign-string (str-ptr label)
            (setf gfs::tdata str-ptr)
            (if (zerop (gfs::insert-menu-item hmenu #x7FFFFFFF 1 mii-ptr))
              (error 'gfs::win32-error :detail "insert-menu-item failed")))
          (if (zerop (gfs::insert-menu-item hmenu #x7FFFFFFF 1 mii-ptr))
            (error 'gfs::win32-error :detail "insert-menu-item failed")))))))

(defun sub-menu (m index)
  (if (gfs:disposed-p m)
    (error 'gfs:disposed-error))
  (let ((hwnd (gfs::get-submenu (gfs:handle m) index)))
    (if (not (gfs:null-handle-p hwnd))
      (get-widget (thread-context) hwnd)
      nil)))

(defun visit-menu-tree (menu fn)
  (dotimes (index (length (slot-value menu 'items)))
    (let ((it (elt (slot-value menu 'items) index))
          (child (sub-menu menu index)))
      (unless (null child)
        (visit-menu-tree child fn))
      (funcall fn menu it))))

;;;
;;; methods
;;;

(defmethod append-item ((self menu) thing disp &optional disabled checked classname)
  (let* ((tc (thread-context))
         (hmenu (gfs:handle self))
         (item (create-item-with-callback hmenu (or classname 'menu-item) thing disp))
         (text (call-text-provider self thing)))
    (append-menuitem hmenu (item-id item) text (cffi:null-pointer) (cffi:null-pointer) disabled checked)
    (put-item tc item)
    (vector-push-extend item (slot-value self 'items))
    item))

(defmethod append-separator ((self menu))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let* ((tc (thread-context))
         (hmenu (gfs:handle self))
         (item (make-instance 'menu-item :handle hmenu)))
    (append-menuitem hmenu (item-id item) nil (cffi:null-pointer) (cffi:null-pointer) nil nil)
    (put-item tc item)
    (vector-push-extend item (slot-value self 'items))
    item))

(defmethod append-submenu ((self menu) text (submenu menu) disp &optional disabled checked)
  (if (or (gfs:disposed-p self) (gfs:disposed-p submenu))
    (error 'gfs:disposed-error))
  (let* ((tc (thread-context))
         (hparent (gfs:handle self))
         (hmenu (gfs:handle submenu))
         (item (make-instance 'menu-item :handle hparent)))
    (append-menuitem hparent (item-id item) text (cffi:null-pointer) hmenu disabled checked)
    (put-item tc item)
    (vector-push-extend item (slot-value self 'items))
    (put-widget tc submenu)
    (cond
      ((null disp))
      ((functionp disp)
         (let ((class (define-dispatcher 'gfw:menu disp)))
           (setf (dispatcher submenu) (make-instance (class-name class)))))
      ((typep disp 'gfw:event-dispatcher)
         (setf (dispatcher submenu) disp))
      (t
         (error 'gfs:toolkit-error
           :detail "callback must be a function, instance of gfw:event-dispatcher, or null")))
    item))

(defun menu-cleanup-callback (menu item)
  (let ((tc (thread-context)))
    (delete-widget tc (gfs:handle menu))
    (delete-tc-item tc item)))

(defmethod gfs:dispose ((self menu))
  (unless (null (dispatcher self))
    (event-dispose (dispatcher self) self))
  (visit-menu-tree self #'menu-cleanup-callback)
  (let ((hwnd (gfs:handle self)))
    (when (not (gfs:null-handle-p hwnd))
      (delete-widget (thread-context) hwnd)
      (if (zerop (gfs::destroy-menu hwnd))
        (error 'gfs:win32-error :detail "destroy-menu failed"))))
  (setf (slot-value self 'gfs:handle) nil))
