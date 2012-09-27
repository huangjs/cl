;;;;
;;;; font.lisp
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
;;; methods
;;;

(defmethod data-object ((self font) &optional gc)
  (if (null gc)
    (error 'gfs:toolkit-error :detail "gc argument required when calling data-object for font"))
  (if (or (gfs:disposed-p self) (gfs:disposed-p gc))
    (error 'gfs:disposed-error))
  (font->data (gfs:handle gc) (gfs:handle self)))

(defmethod gfs:dispose ((self font))
  (let ((hgdi (gfs:handle self)))
    (unless (gfs:null-handle-p hgdi)
      (gfs::delete-object hgdi)))
  (setf (slot-value self 'gfs:handle) nil))

(defmethod initialize-instance :after ((self font) &key gc data &allow-other-keys)
  (when (or gc data)
    (unless (and gc data (typep gc 'graphics-context) (typep data 'font-data))
      (error 'gfs:toolkit-error :detail "font initialize-instance requires graphics-context and font-data"))
    (setf (slot-value self 'gfs:handle) (data->font (gfs:handle gc) data))))
