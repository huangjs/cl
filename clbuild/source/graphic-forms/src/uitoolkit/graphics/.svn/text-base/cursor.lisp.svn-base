;;;;
;;;; cursor.lisp
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

(in-package :graphic-forms.uitoolkit.graphics)

;;;
;;; functions
;;;


;;;
;;; methods
;;;

(defmethod gfs:dispose ((self cursor))
  (if (gfs:disposed-p self)
    (warn 'gfs:toolkit-warning :detail "cursor already disposed"))
  (unless (sharedp self)
    (gfs::destroy-cursor (gfs:handle self)))
  (setf (slot-value self 'gfs:handle) nil))

(defmethod initialize-instance :after ((self cursor) &key file hotspot image system
                                                     &allow-other-keys)
  (let ((resource-id (if system (cffi:make-pointer system))))
    (cond
      (resource-id
        (setf (slot-value self 'gfs:handle)
              (gfs::load-image (cffi:null-pointer)
                               resource-id
                               gfs::+image-cursor+
                               0 0
                               (logior gfs::+lr-defaultsize+ gfs::+lr-shared+)))
        (setf (slot-value self 'shared) t))
      (file
        (let ((tmp (make-instance 'image :file file)))
          (setf (slot-value self 'gfs:handle) (image->hicon tmp))))
      ((typep image 'image)
        (setf (slot-value self 'gfs:handle) (image->hicon image hotspot))))))
