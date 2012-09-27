;;;;
;;;; color-dialog.lisp
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

(in-package #:graphic-forms.uitoolkit.widgets)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +custom-color-array-size+ 16))

;;;
;;; helper functions
;;;

(defun obtain-chosen-color (dlg)
  (let ((cc-ptr (gfs:handle dlg)))
    (if (cffi:null-pointer-p cc-ptr)
      (error 'gfs:disposed-error))
    (cffi:with-foreign-slots ((gfs::result gfs::ccolors) cc-ptr gfs::choosecolor)
      (values (gfg:rgb->color gfs::result)
              (loop for index to (1- +custom-color-array-size+)
                    collect (gfg:rgb->color (cffi:mem-aref gfs::ccolors 'gfs::colorref index)))))))

(defmacro with-color-dialog ((owner style color custom-colors &key initial-color initial-custom-colors) &body body)
  (let ((dlg (gensym)))
    `(let ((,color nil)
           (,custom-colors nil)
           (,dlg (make-instance 'color-dialog
                                :initial-custom-colors ,initial-custom-colors
                                :initial-color         ,initial-color
                                :owner                 ,owner
                                :style                 ,style)))
       (unwind-protect
           (unless (zerop (show ,dlg t))
             (multiple-value-bind (tmp-color tmp-custom)
                 (obtain-chosen-color ,dlg)
               (setf ,color         tmp-color
                     ,custom-colors tmp-custom)
               ,@body))
         (gfs:dispose ,dlg)))))

;;;
;;; methods
;;;

(defmethod compute-style-flags ((self color-dialog) &rest extra-data)
  (let ((std-flags (logior gfs::+cc-anycolor+ gfs::+cc-preventfullopen+ (if extra-data gfs::+cc-rgbinit+ 0))))
    (loop for sym in (style-of self)
          do (ecase sym
               (:allow-custom-colors
                  (setf std-flags (logand std-flags (lognot gfs::+cc-preventfullopen+))))
               (:display-solid-only)
                  (setf std-flags (logior std-flags gfs::+cc-solidcolor+))))
    (values std-flags 0)))

(defmethod gfs:dispose ((self color-dialog))
  (let ((cc-ptr (gfs:handle self)))
    (unless (cffi:null-pointer-p cc-ptr)
      (cffi:with-foreign-slots ((gfs::ccolors) cc-ptr gfs::choosecolor)
        (unless (cffi:null-pointer-p gfs::ccolors)
          (cffi:foreign-free gfs::ccolors)))
      (cffi:foreign-free cc-ptr)
      (setf (slot-value self 'gfs:handle) nil))))

(defmethod initialize-instance :after ((self color-dialog) &key initial-color initial-custom-colors owner &allow-other-keys)
  (if (null owner)
    (error 'gfs:toolkit-error :detail ":owner initarg is required"))
  (if (gfs:disposed-p owner)
    (error 'gfs:disposed-error))
  (let ((cc-ptr (cffi:foreign-alloc 'gfs::choosecolor))
        (colors-ptr (cffi:foreign-alloc 'gfs::colorref :count +custom-color-array-size+))
        (index 0)
        (default-rgb (gfg:color->rgb gfg:*color-black*)))
    (loop for color in initial-custom-colors
          when (< index +custom-color-array-size+)
          do (progn
               (setf (cffi:mem-aref colors-ptr 'gfs::colorref index) (gfg:color->rgb color))
               (incf index)))
    (loop until (>= index +custom-color-array-size+)
          do (progn
               (setf (cffi:mem-aref colors-ptr 'gfs::colorref index) default-rgb)
               (incf index)))
    (multiple-value-bind (std-style ex-style)
        (compute-style-flags self initial-color)
      (declare (ignore ex-style))
      (cffi:with-foreign-slots ((gfs::ccsize gfs::howner gfs::hinst gfs::result
                                 gfs::ccolors gfs::flags gfs::cdata gfs::hookfn gfs::templname)
                                cc-ptr gfs::choosecolor)
        (setf gfs::ccsize    (cffi:foreign-type-size 'gfs::choosecolor)
              gfs::howner    (gfs:handle owner)
              gfs::hinst     (cffi:null-pointer)
              gfs::result    (gfg:color->rgb (or initial-color (gfg:make-color)))
              gfs::ccolors   colors-ptr
              gfs::flags     std-style
              gfs::cdata     0
              gfs::hookfn    (cffi:null-pointer)
              gfs::templname (cffi:null-pointer))))
    (setf (slot-value self 'gfs:handle) cc-ptr)))

(defmethod show ((self color-dialog) flag)
  (declare (ignore flag))
  (show-common-dialog self #'gfs::choose-color))
