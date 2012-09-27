;;;;
;;;; font-dialog.lisp
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

(defconstant +font-dialog-flags+ (logior gfs::+cf-effects+ gfs::+cf-inittologfontstruct+))

;;;
;;; helper functions
;;;

(defun obtain-chosen-font (dlg gc)
  (if (or (gfs:disposed-p dlg) (gfs:disposed-p gc))
    (error 'gfs:disposed-error))
  (cffi:with-foreign-slots ((gfs::logfont gfs::color) (gfs:handle dlg) gfs::choosefont)
    (values (make-instance 'gfg:font :handle (gfs::create-font-indirect gfs::logfont))
            (gfg::rgb->color gfs::color))))

(defun lookup-default-font ()
  (let ((lf-ptr (cffi:foreign-alloc 'gfs::logfont)))
    (gfs:zero-mem lf-ptr gfs::logfont)
    (gfs::get-object (gfs::get-stock-object gfs::+system-font+)
                     (cffi:foreign-type-size 'gfs::logfont)
                     lf-ptr)
    lf-ptr))

(defmacro with-font-dialog ((owner style font color &key gc initial-color initial-font) &body body)
  (let ((dlg (gensym)))
   `(let ((,font nil)
          (,color nil)
          (,dlg (make-instance 'font-dialog
                               :gc ,gc
                               :initial-color ,initial-color
                               :initial-font ,initial-font
                               :owner ,owner
                               :style ,style)))
      (unwind-protect
          (unless (zerop (show ,dlg t))
            (multiple-value-bind (f c) (obtain-chosen-font ,dlg ,gc)
              (setf ,font f)
              (setf ,color c))
            ,@body)
        (gfs:dispose ,dlg)))))

;;;
;;; methods
;;;

(defmethod compute-style-flags ((self font-dialog) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags (logior gfs::+cf-both+ +font-dialog-flags+)))
    (loop for sym in (style-of self)
          do (ecase sym
               ;; primary styles
               ;;
               (:all-fonts
                 (setf std-flags (logior gfs::+cf-both+ +font-dialog-flags+)))
               (:fixed-pitch-fonts
                 (setf std-flags (logior gfs::+cf-fixedpitchonly+ +font-dialog-flags+)))
               (:printer-fonts
                 (setf std-flags (logior gfs::+cf-printerfonts+ +font-dialog-flags+)))
               (:screen-fonts
                 (setf std-flags (logior gfs::+cf-screenfonts+ +font-dialog-flags+)))
               (:truetype-fonts
                 (setf std-flags (logior gfs::+cf-ttonly+ +font-dialog-flags+)))
               (:wsyiwyg-fonts
                 (setf std-flags (logior gfs::+cf-both+
                                         gfs::+cf-scalableonly+
                                         gfs::+cf-wysiwyg+
                                         +font-dialog-flags+)))

               ;; styles that can be combined
               ;;
               (:no-effects
                 (setf std-flags (logand std-flags (lognot gfs::+cf-effects+))))))
    (values std-flags 0)))

(defmethod gfs:dispose ((self font-dialog))
  (let ((cf-ptr (gfs:handle self)))
    (unless (cffi:null-pointer-p cf-ptr)
      (cffi:with-foreign-slots ((gfs::logfont) cf-ptr gfs::choosefont)
        (unless (cffi:null-pointer-p gfs::logfont)
          (cffi:foreign-free gfs::logfont)))
      (cffi:foreign-free cf-ptr)))
  (setf (slot-value self 'gfs:handle) (cffi:null-pointer)))

(defmethod initialize-instance :after ((self font-dialog) &key gc initial-color initial-font owner &allow-other-keys)
  (if (null gc)
    (error 'gfs:toolkit-error :detail ":gc initarg is required"))
  (if (null owner)
    (error 'gfs:toolkit-error :detail ":owner initarg is required"))
  (if (gfs:disposed-p owner)
    (error 'gfs:disposed-error))
  (let ((cf-ptr (cffi:foreign-alloc 'gfs::choosefont))
        (lf-ptr (if initial-font
                  (gfg::data->logfont (gfs:handle gc) (gfg:data-object initial-font gc))
                  (lookup-default-font))))
    (multiple-value-bind (std-style ex-style) (compute-style-flags self)
      (declare (ignore ex-style))
      (cffi:with-foreign-slots ((gfs::structsize gfs::howner gfs::hdc gfs::logfont
                                 gfs::flags gfs::color)
                                cf-ptr gfs::choosefont)
        (setf gfs::structsize (cffi:foreign-type-size 'gfs::choosefont)
              gfs::howner     (gfs:handle owner)
              gfs::hdc        (gfs:handle gc)
              gfs::logfont    lf-ptr
              gfs::flags      std-style
              gfs::color      (if initial-color (gfg:color->rgb initial-color) 0))))
    (setf (slot-value self 'gfs:handle) cf-ptr)))

(defmethod show ((self font-dialog) flag)
  (declare (ignore flag))
  (show-common-dialog self #'gfs::choose-font))
