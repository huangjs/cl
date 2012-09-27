;;;;
;;;; panel.lisp
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

(defparameter *panel-window-classname* "GraphicFormsPanel")

;;;
;;; helper functions
;;;

(defun register-panel-window-class ()
  (register-window-class *panel-window-classname*
                         (cffi:get-callback 'uit_widgets_wndproc)
                         gfs::+cs-dblclks+
                         -1))

;;;
;;; methods
;;;

(defmethod compute-outer-size ((self panel) desired-client-size)
  (declare (ignore self))
  (gfs:copy-size desired-client-size))

(defmethod compute-style-flags ((self panel) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags (logior gfs::+ws-clipchildren+ gfs::+ws-clipsiblings+ gfs::+ws-child+ gfs::+ws-visible+)))
    (loop for sym in (style-of self)
          do (ecase sym
                ;; styles that can be combined
                ;;
               (:border
                  (setf std-flags (logior std-flags gfs::+ws-border+)))
               (:horizontal-scrollbar
                  (setf std-flags (logior std-flags gfs::+ws-hscroll+)))
               (:vertical-scrollbar
                  (setf std-flags (logior std-flags gfs::+ws-vscroll+)))))
    (values std-flags gfs::+ws-ex-controlparent+)))

(defmethod initialize-instance :after ((self panel) &key parent &allow-other-keys)
  (if (null parent)
    (error 'gfs:toolkit-error :detail "parent is required for panel"))
  (if (gfs:disposed-p parent)
    (error 'gfs:disposed-error))
  (init-window self *panel-window-classname* #'register-panel-window-class parent ""))
