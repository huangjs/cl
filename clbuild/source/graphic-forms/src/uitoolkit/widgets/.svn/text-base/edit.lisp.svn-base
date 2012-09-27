;;;;
;;;; edit.lisp
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

(defconstant +horizontal-edit-text-margin+ 2)
(defconstant +vertical-edit-text-margin+   2)

;;;
;;; methods
;;;

(defmethod auto-hscroll-p ((self edit))
  (test-native-style self gfs::+es-autohscroll+))

(defmethod auto-vscroll-p ((self edit))
  (test-native-style self gfs::+es-autovscroll+))

(defmethod compute-style-flags ((self edit) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags +default-child-style+)
        (style (style-of self)))
    (loop for sym in style
          do (ecase sym
               ;; primary edit styles
               ;;
               (:multi-line           (setf std-flags (logior +default-child-style+
                                                              gfs::+es-multiline+)))
               ;; styles that can be combined
               ;;
               (:auto-hscroll         (setf std-flags (logior std-flags gfs::+es-autohscroll+)))
               (:auto-vscroll         (setf std-flags (logior std-flags gfs::+es-autovscroll+)))
               (:horizontal-scrollbar (setf std-flags (logior std-flags gfs::+ws-hscroll+)))
               (:mask-characters      (setf std-flags (logior std-flags gfs::+es-password+)))
               (:no-border            )
               (:no-hide-selection    (setf std-flags (logior std-flags gfs::+es-nohidesel+)))
               (:read-only            (setf std-flags (logior std-flags gfs::+es-readonly+)))
               (:vertical-scrollbar   (setf std-flags (logior std-flags gfs::+ws-vscroll+)))
               (:want-return          (setf std-flags (logior std-flags gfs::+es-wantreturn+)))))
    (if (not (find :multi-line style))
      (setf std-flags (logior std-flags gfs::+es-autohscroll+)))
    (values std-flags (if (find :no-border style) 0 gfs::+ws-ex-clientedge+))))

(defmethod copy-text ((self edit))
  (gfs::send-message (gfs:handle self) gfs::+wm-copy+ 0 0))

(defmethod cut-text ((self edit))
  (gfs::send-message (gfs:handle self) gfs::+wm-cut+ 0 0))

(defmethod delete-selection ((self edit))
  (gfs::send-message (gfs:handle self) gfs::+wm-clear+ 0 0))

(defmethod enable-scrollbars ((self edit) horizontal vertical)
  (let ((bits (get-native-style self)))
    (if horizontal
      (setf bits (logior bits gfs::+ws-hscroll+))
      (setf bits (logand bits (lognot gfs::+ws-hscroll+))))
    (if vertical
      (setf bits (logior bits gfs::+ws-vscroll+))
      (setf bits (logand bits (lognot gfs::+ws-vscroll+))))
    (update-native-style self bits)))

(defmethod initialize-instance :after ((self edit) &key parent text &allow-other-keys)
  (create-control self parent text gfs::+icc-standard-classes+))

(defmethod line-count ((self edit))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::send-message (gfs:handle self) gfs::+em-getlinecount+ 0 0))

(defmethod paste-text ((self edit))
  (gfs::send-message (gfs:handle self) gfs::+wm-paste+ 0 0))

(defmethod preferred-size ((self edit) width-hint height-hint)
  (let ((text-size (widget-text-size self #'text (logior gfs::+dt-editcontrol+ gfs::+dt-noprefix+)))
        (size (gfs:make-size))
        (b-width (* (border-width self) 2)))
    (if (>= width-hint 0)
      (setf (gfs:size-width size) width-hint)
      (setf (gfs:size-width size) (+ b-width
                                     (gfs:size-width text-size)
                                     (* +horizontal-edit-text-margin+ 2))))
    (if (>= height-hint 0)
      (setf (gfs:size-height size) height-hint)
      (setf (gfs:size-height size) (+ b-width
                                      (* (gfs:size-height text-size) (line-count self))
                                      (* +vertical-edit-text-margin+ 2))))
    size))

(defmethod select-all ((self edit) flag)
  (if flag
    (gfs::send-message (gfs:handle self) gfs::+em-setsel+ 0 (length (text self)))
    (gfs::send-message (gfs:handle self) gfs::+em-setsel+ 0 0)))

(defmethod selected-span ((self edit))
  (cffi:with-foreign-object (start-ptr :unsigned-long)
    (cffi:with-foreign-object (end-ptr :unsigned-long)
      (gfs::send-message (gfs:handle self)
                         gfs::+em-getsel+
                         (cffi:pointer-address start-ptr)
                         (cffi:pointer-address end-ptr))
      (let ((start (cffi:mem-ref start-ptr :unsigned-long))
            (end (cffi:mem-ref end-ptr :unsigned-long))
            (str (text self)))
        (if (= start end)
          (values nil nil)
          (values (subseq str start end) (gfs:make-span :start start :end end)))))))

(defmethod (setf selected-span) ((span gfs:span) (self edit))
  (with-drawing-disabled (self)
    (let ((hwnd (gfs:handle self)))
      (gfs::send-message hwnd gfs::+em-setsel+ 1 1)
      (gfs::send-message hwnd gfs::+em-setsel+ (gfs:span-start span) (gfs:span-end span)))))

(defmethod text ((self edit))
  (get-widget-text self))

(defmethod (setf text) (str (self edit))
  (set-widget-text self str))

(defmethod text-baseline ((self edit))
  (widget-text-baseline self +vertical-edit-text-margin+))

(defmethod text-for-pasting-p ((self edit))
  (/= (gfs::is-clipboard-format-available gfs::+cf-text+) 0))

(defmethod text-modified-p ((self edit))
  (/= (gfs::send-message (gfs:handle self) gfs::+em-getmodify+ 0 0) 0))

(defmethod (setf text-modified-p) (flag (self edit))
  (gfs::send-message (gfs:handle self) gfs::+em-setmodify+ (if flag 1 0) 0))

(defmethod undo-available-p ((self edit))
  (/= (gfs::send-message (gfs:handle self) gfs::+em-canundo+ 0 0) 0))
