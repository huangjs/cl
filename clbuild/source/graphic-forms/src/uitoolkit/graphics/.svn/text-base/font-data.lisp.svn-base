;;;;
;;;; font-data.lisp
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

(defun pntsize->lfheight (hdc pntsize)
  (let ((log-height (gfs::get-device-caps hdc gfs::+logpixelsy+)))
    (- (floor (+ (/ (* pntsize log-height) 72) 0.5)))))

(defun lfheight->pntsize (hdc lfheight)
  (let ((log-height (gfs::get-device-caps hdc gfs::+logpixelsy+)))
    (floor (* (+ (- lfheight) 0.5) 72) log-height)))

(defun style->logfont (style lf-ptr)
  (cffi:with-foreign-slots ((gfs::lfweight gfs::lfitalic gfs::lfunderline
                             gfs::lfstrikeout gfs::lfoutprec gfs::lfpitchandfamily)
                            lf-ptr gfs::logfont)
    (setf gfs::lfweight    (if (find :bold style)      gfs::+fw-bold+ gfs::+fw-normal+))
    (setf gfs::lfitalic    (if (find :italic style)    1 0))
    (setf gfs::lfunderline (if (find :underline style) 1 0))
    (setf gfs::lfstrikeout (if (find :strikeout style) 1 0))
    (setf gfs::lfoutprec   (cond
                             ((find :truetype-only style) gfs::+out-tt-only-precis+)
                             ((find :outline       style) gfs::+out-outline-precis+)
                             (t                           gfs::+out-default-precis+)))
    (setf gfs::lfpitchandfamily (cond
                                  ((find :fixed style)    gfs::+fixed-pitch+)
                                  ((find :variable style) gfs::+variable-pitch+)
                                  (t                      gfs::+default-pitch+)))))

(defun logfont->style (lf-ptr)
  (let ((style nil))
    (cffi:with-foreign-slots ((gfs::lfweight gfs::lfitalic gfs::lfunderline
                               gfs::lfstrikeout gfs::lfoutprec gfs::lfpitchandfamily)
                              lf-ptr gfs::logfont)
      (if (= gfs::lfweight gfs::+fw-bold+)
        (push :bold style))
      (unless (zerop gfs::lfitalic)
        (push :italic style))
      (unless (zerop gfs::lfunderline)
        (push :underline style))
      (unless (zerop gfs::lfstrikeout)
        (push :strikeout style))
      (case gfs::lfoutprec
        (#.gfs::+out-tt-only-precis+ (push :truetype-only style))
        (#.gfs::+out-outline-precis+ (push :outline       style)))
      (case gfs::lfpitchandfamily
        (#.gfs::+fixed-pitch+        (push :fixed         style))
        (#.gfs::+variable-pitch+     (push :variable      style))))
    style))

(defun data->logfont (hdc data)
  (let ((lf-ptr (cffi:foreign-alloc 'gfs::logfont))
        (style (font-data-style data)))
    (gfs:zero-mem lf-ptr gfs::logfont)
    (cffi:with-foreign-slots ((gfs::lfheight gfs::lfcharset gfs::lffacename) lf-ptr gfs::logfont)
      (setf gfs::lfheight (pntsize->lfheight hdc (font-data-point-size data)))
      (setf gfs::lfcharset (font-data-char-set data))
      (style->logfont style lf-ptr)
      (cffi:with-foreign-string (str (font-data-face-name data))
        (let ((lffacename-ptr (cffi:foreign-slot-pointer lf-ptr 'gfs::logfont 'gfs::lffacename)))
          (gfs::strncpy lffacename-ptr str (1- gfs::+lf-facesize+))
          (setf (cffi:mem-aref lffacename-ptr :char (1- gfs::+lf-facesize+)) 0))))
    lf-ptr))

(defun logfont->data (hdc lf-ptr)
  (let ((char-set 0)
        (face-name "")
        (point-size 0)
        (style nil))
    (cffi:with-foreign-slots ((gfs::lfheight gfs::lfcharset gfs::lffacename) lf-ptr gfs::logfont)
      (setf point-size (lfheight->pntsize hdc gfs::lfheight))
      (setf char-set gfs::lfcharset)
      (setf style (logfont->style lf-ptr))
      (let ((lffacename-ptr (cffi:foreign-slot-pointer lf-ptr 'gfs::logfont 'gfs::lffacename)))
        (setf face-name (cffi:foreign-string-to-lisp lffacename-ptr))))
    (gfg:make-font-data :char-set char-set
                        :face-name face-name
                        :point-size point-size
                        :style style)))

(defun data->font (hdc data)
  (let ((hfont (cffi:null-pointer)))
    (setf hfont (gfs::create-font-indirect (data->logfont hdc data)))
    (if (gfs:null-handle-p hfont)
      (error 'gfs:win32-error :detail "create-font-indirect failed"))
    hfont))

(defun font->data (hdc hfont)
  (cffi:with-foreign-object (lf-ptr 'gfs::logfont)
    (gfs:zero-mem lf-ptr gfs::logfont)
    (if (zerop (gfs::get-object hfont (cffi:foreign-type-size 'gfs::logfont) lf-ptr))
      (error 'gfs:win32-error :detail "get-object failed"))
    (logfont->data hdc lf-ptr)))

(defmethod print-object ((self font-data) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "face name: ~a " (font-data-face-name self))
    (format stream "point size: ~d " (font-data-point-size self))
    (format stream "style: ~a " (font-data-style self))
    (format stream "char-set: ~d" (font-data-char-set self))))
