;;;;
;;;; magick-core-api.lisp
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

(in-package :graphic-forms.uitoolkit.graphics.imagemagick)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cffi)
  (pushnew cl-user::*magick-library-directory* cffi:*foreign-library-directories* :test #'equal))

(defvar *magick-initialized* nil)

(load-foreign-library "wsock32.dll")
(load-foreign-library "msvcr71.dll")
(load-foreign-library "x11.dll")
(load-foreign-library "CORE_RL_bzlib_.dll")
(load-foreign-library "CORE_RL_jbig_.dll")
(load-foreign-library "CORE_RL_jpeg_.dll")
(load-foreign-library "CORE_RL_lcms_.dll")
(load-foreign-library "CORE_RL_zlib_.dll")
(load-foreign-library "CORE_RL_png_.dll")
(load-foreign-library "CORE_RL_tiff_.dll")
(load-foreign-library "CORE_RL_ttf_.dll")
(load-foreign-library "CORE_RL_xlib_.dll")
(load-foreign-library "CORE_RL_magick_.dll")

;;;
;;; translated from constitute.h
;;;

(defcfun
  ("ConstituteImage" constitute-image)
  :pointer                    ;; Image*
  (columns    :unsigned-long)
  (rows       :unsigned-long)
  (map        :pointer)       ;; const char*
  (storage    storage-type)
  (pixels     :pointer)       ;; void*
  (exception  :pointer))      ;; ExceptionInfo*

(defcfun
  ("PingImage" ping-image)
  :pointer                    ;; Image*
  (image-info :pointer)       ;; ImageInfo*
  (exception  :pointer))      ;; ExceptionInfo*

(defcfun
  ("ReadImage" read-image)
  :pointer                    ;; Image*
  (image-info :pointer)       ;; ImageInfo*
  (exception  :pointer))      ;; ExceptionInfo*

(defcfun
  ("WriteImage" write-image)
  boolean-type
  (image-info :pointer)       ;; ImageInfo*
  (image      :pointer))      ;; Image*

;;;
;;; translated from exception.h
;;;

(defcfun
  ("AcquireExceptionInfo" acquire-exception-info)
  :pointer)

(defcfun
  ("CatchException" catch-exception)
  :void
  (exception  :pointer))      ;; ExceptionInfo*

(defcfun
  ("ClearMagickException" clear-magick-exception)
  :void
  (exception  :pointer))      ;; ExceptionInfo*

(defcfun
  ("DestroyExceptionInfo" destroy-exception-info)
  :pointer                    ;; ExceptionInfo*
  (exception  :pointer))      ;; ExceptionInfo*

;;;
;;; translated from image.h
;;;

(defcfun
  ("CloneImageInfo" clone-image-info)
  :pointer                    ;; ImageInfo*
  (orig       :pointer))      ;; ImageInfo*

(defcfun
  ("DestroyImage" destroy-image)
  :pointer                    ;; Image*
  (victim     :pointer))      ;; Image*

(defcfun
  ("DestroyImageInfo" destroy-image-info)
  :pointer                    ;; ImageInfo*
  (victim     :pointer))      ;; ImageInfo*

(defcfun
  ("GetImagePixels" get-image-pixels)
  :pointer                    ;; PixelPacket*
  (image      :pointer)       ;; Image*
  (x          :long)
  (y          :long)
  (width      :unsigned-long)
  (height     :unsigned-long))

(defcfun
  ("GetIndexes" get-indexes)
  :pointer                    ;; IndexPacket*
  (image      :pointer))      ;; Image*

(defun scale-quantum-to-byte (quant)
  (floor quant 257))

;;;
;;; translated from list.h
;;;

(defcfun
  ("GetFirstImageInList" get-first-image-in-list)
  :pointer                    ;; Image*
  (images     :pointer))      ;; Image*

(defcfun
  ("GetImageListLength" get-image-list-length)
  :unsigned-long
  (images     :pointer))      ;; Image*

(defcfun
  ("GetNextImageInList" get-next-image-in-list)
  :pointer                    ;; Image*
  (images     :pointer))      ;; Image*

;;;
;;; translated from magick.h
;;;

(defcfun
  ("DestroyMagick" destroy-magick)
  :void)

(defcfun
  ("InitializeMagick" initialize-magick)
  :void
  (args       :pointer))      ;; char*

;;;
;;; translated from resize.h
;;;

(defcfun
  ("ResizeImage" resize-image)
  :pointer                    ;; Image*
  (orig       :pointer)       ;; Image*
  (width      :unsigned-long)
  (height     :unsigned-long)
  (filter     :int)           ;; filter-type
  (blur       :double)
  (exception  :pointer))      ;; ExceptionInfo*

;;;
;;; helper macros
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-image-path ((path info ex) &body body)
   `(let ((,info (clone-image-info (cffi:null-pointer)))
          (,ex (acquire-exception-info)))
      (if (cffi:null-pointer-p ,info)
        (error 'gfs:toolkit-error :detail "could not allocate Magick ImageInfo object"))
      (unwind-protect
          (cffi:with-foreign-string (str ,path)
            (let ((filename-ptr (cffi:foreign-slot-pointer ,info 'magick-image-info 'filename)))
              (gfs::strncpy filename-ptr str (1- +magick-max-text-extent+))
              (setf (cffi:mem-aref filename-ptr :char (1- +magick-max-text-extent+)) 0))
            ,@body)
        (destroy-image-info ,info)
        (destroy-exception-info ,ex)))))
