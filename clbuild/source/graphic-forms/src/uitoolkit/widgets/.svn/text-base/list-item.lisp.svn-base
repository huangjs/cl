;;;;
;;;; list-item.lisp
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

(defun lb-insert-item (hwnd index label hbmp)
  (declare (ignore hbmp)) ; FIXME: re-enable when we support images in list-box
  (let ((text (or label "")))
    (cffi:with-foreign-string (str-ptr text)
      (let ((retval (gfs::send-message hwnd gfs::+lb-insertstring+ index (cffi:pointer-address str-ptr))))
        (if (< retval 0)
          (error 'gfs:toolkit-error :detail (format nil "LB_INSERTSTRING failed: ~d" retval)))))))

(defun lb-item-height (hwnd)
  (let ((height (gfs::send-message hwnd gfs::+lb-getitemheight+ 0 0)))
    (if (< height 0)
      (error 'gfs:win32-error :detail "LB_GETITEMHEIGHT failed"))
    height))

(defun lb-item-text-length (hwnd index)
  (let ((length (gfs::send-message hwnd gfs::+lb-gettextlen+ index 0)))
    (if (< length 0)
      (error 'gfs:win32-error :detail "LB_GETTEXTLEN failed"))
    length))

(defun lb-item-text (hwnd index &optional buffer-size)
  (if (or (null buffer-size) (<= buffer-size 0))
    (setf buffer-size (lb-item-text-length hwnd index)))
  (cffi:with-foreign-pointer-as-string (str-ptr (1+ buffer-size))
    (if (< (gfs::send-message hwnd gfs::+lb-gettext+ index (cffi:pointer-address str-ptr)) 0)
      (error 'gfs:win32-error :detail "LB_GETTEXT failed"))
    (cffi:foreign-string-to-lisp str-ptr)))

;;;
;;; methods
;;;

(defmethod gfs:dispose ((self list-item))
  (let ((hwnd (gfs:handle self)))
    (unless (or (null hwnd) (cffi:null-pointer-p hwnd))
      (let ((owner (get-widget (thread-context) hwnd)))
        (if (and owner (cffi:pointer-eq hwnd (gfs:handle owner)))
          (gfs::send-message hwnd gfs::+lb-deletestring+ (item-index owner self) 0)))))
  (call-next-method))

(defmethod select ((self list-item) flag)
  (let ((owner (owner self)))
    (if flag
      (lb-select-item owner (item-index owner self))
      (lb-deselect-item owner (item-index owner self)))))

(defmethod selected-p ((self list-item))
  (let ((owner (owner self)))
    (> (gfs::send-message (gfs:handle self) gfs::+lb-getsel+ (item-index owner self) 0) 0)))

(defmethod text ((self list-item))
  (let ((hwnd (gfs:handle self)))
    (if (or (null hwnd) (cffi:null-pointer-p hwnd))
      ""
      (lb-item-text hwnd (item-index (get-widget (thread-context) hwnd) self)))))
