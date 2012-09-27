;;;;
;;;; status-bar.lisp
;;;;
;;;; Copyright (C) 2007, Jack D. Unrue
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

(declaim (inline stb-is-simple))
(defun stb-is-simple (status-bar)
  (/= (gfs::send-message (gfs:handle status-bar) gfs::+sb-issimple+ 0 0) 0))

(defun stb-get-border-widths (status-bar)
  "Returns a list of integer widths (0: horz border, 1: vert border, 2: internal)"
  (cffi:with-foreign-pointer (array (* (cffi:foreign-type-size :int) 3))
    (when (zerop (gfs::send-message (gfs:handle status-bar)
                                    gfs::+sb-getborders+
                                    0
                                    (cffi:pointer-address array)))
      (warn 'gfs:win32-warning :detail "SB_GETBORDERS message failed")
      (return-from stb-get-border-widths (list 0 0 0)))
    (loop for index from 0 to 2
          collect (cffi:mem-aref array :int index))))

(defun stb-set-min-height (status-bar height)
  (let ((widths (stb-get-border-widths status-bar))
        (hstatus (gfs:handle status-bar)))
    (when (zerop (gfs::send-message hstatus
                                    gfs::+sb-setminheight+
                                    (+ height (* (second widths) 2))
                                    0))
      (warn 'gfs:win32-warning :detail "SB_SETMINHEIGHT message failed")
      (return-from stb-set-min-height nil))
    (gfs::send-message hstatus gfs::+wm-size+ 0 0))
  height)

(defun stb-set-text (status-bar str &optional item-index)
  (let ((part-id (if (stb-is-simple status-bar) gfs::+sb-simpleid+ item-index)))
    (cffi:with-foreign-string (str-ptr str)
      (if (zerop (gfs::send-message (gfs:handle status-bar)
                                    gfs::+sb-settext+
                                    part-id
                                    (cffi:pointer-address str-ptr)))
        (warn 'gfs:win32-warning :detail "SB_SETTEXT message failed"))))
  str)

(defun stb-get-text-properties (status-bar item-index)
  "Returns the text length and operation type of the status bar part at item-index."
  (let ((hresult (gfs::send-message (gfs:handle status-bar)
                                    gfs::+sb-gettextlength+
                                    item-index
                                    0)))
    (values (gfs::lparam-low-word hresult) (gfs::lparam-high-word hresult))))

(defun stb-get-text (status-bar item-index)
  (multiple-value-bind (length op-type)
      (stb-get-text-properties status-bar item-index)
    (declare (ignore op-type))
    (if (zerop length)
      ""
      (cffi:with-foreign-pointer-as-string (str-ptr (1+ length))
        (gfs::send-message (gfs:handle status-bar)
                           gfs::+sb-gettext+
                           item-index
                           (cffi:pointer-address str-ptr))))))

;;;
;;; methods
;;;

(defmethod border-width ((self status-bar))
  (let ((widths (stb-get-border-widths self)))
    (max (first widths) (second widths))))

(defmethod compute-style-flags ((self status-bar) &rest extra-data)
  (let ((extra-bits (if (first extra-data) 0 gfs::+sbars-sizegrip+)))
    (values (logior gfs::+ws-child+ gfs::+ws-visible+ extra-bits) 0)))

(defmethod initialize-instance :after ((self status-bar) &key parent &allow-other-keys)
  (let ((hctl (create-control self
                              parent
                              ""
                              gfs::+icc-win95-classes+
                              nil
                              (find :fixed-size (style-of parent)))))
    (gfs::send-message hctl gfs::+sb-simple+ 1 0))
  (let ((widths (stb-get-border-widths self)))
    (setf (layout-of self) (make-instance 'flow-layout :spacing (third widths)))))

(defmethod preferred-size ((self status-bar) width-hint height-hint)
  (declare (ignore height-hint))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((tmp-size (if (data-of (layout-of self))
                    (compute-size (layout-of self) self width-hint -1)
                    (widget-text-size self
                                      (lambda (widget)
                                        (declare (ignore widget))
                                        "X")
                                      gfs::+dt-singleline+)))
        (widths (stb-get-border-widths self)))
    (gfs:make-size :width 0
                   :height (+ (gfs:size-height tmp-size) (* (second widths) 2) 1))))

(defmethod text ((sbar status-bar))
  (stb-get-text sbar 0))

(defmethod (setf text) (str (sbar status-bar))
  (stb-set-text sbar str))
