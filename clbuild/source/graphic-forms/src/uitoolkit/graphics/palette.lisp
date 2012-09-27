;;;;
;;;; palette.lisp
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

#|
(defun pixel-color (pal pixel-val)
  "Returns the color struct corresponding to the given pixel value; the inverse of the pixel function."
  (if (direct-p pal)
    (error 'toolkit-error :detail "not yet implemented")
    (aref (palette-table pal) pixel-val)))
|#

(defun dump-colors (pal)
  (let* ((tmp (palette-table pal))
         (len (length tmp)))
    (when (zerop len)
      (format t "<empty color table>~%"))
    (dotimes (i len)
      (let ((clr (aref tmp i)))
        (format t "(~a,~a,~a)" (color-red clr) (color-green clr) (color-blue clr))))))

(defmethod print-object ((obj palette) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "direct: ~a " (palette-direct obj))
    (format stream "mask: (~a,~a,~a) "
                   (palette-red-mask obj)
                   (palette-green-mask obj)
                   (palette-blue-mask obj))
    (format stream "shift: (~a,~a,~a) "
                   (palette-red-shift obj)
                   (palette-green-shift obj)
                   (palette-blue-shift obj))
    (format stream "table: ")
    (dump-colors obj)))

(defmethod size ((obj palette))
  (length (palette-table obj)))
