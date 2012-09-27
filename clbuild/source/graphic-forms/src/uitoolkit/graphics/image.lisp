;;;;
;;;; image.lisp
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
;;; helper macros and functions
;;;

(defmacro with-image-transparency ((image pnt) &body body)
  (let ((tmp-image (gensym))
        (orig-pnt (gensym)))
    `(let* ((,tmp-image ,image)
            (,orig-pnt (transparency-pixel-of ,tmp-image)))
       (unwind-protect
           (progn
             (setf (transparency-pixel-of ,tmp-image) ,pnt)
             ,@body)
         (setf (transparency-pixel-of ,tmp-image) ,orig-pnt)))))

(defun clone-bitmap (horig)
  (let ((hclone (cffi:null-pointer))
        (screen-dc (gfs::get-dc (cffi:null-pointer)))
        (nptr (cffi:null-pointer)))
    (gfs::with-compatible-dcs (nptr memdc-src memdc-dest)
      (cffi:with-foreign-object (bmp-ptr 'gfs::bitmap)
        (cffi:with-foreign-slots ((gfs::width gfs::height) bmp-ptr gfs::bitmap)
          (gfs::get-object horig (cffi:foreign-type-size 'gfs::bitmap) bmp-ptr)
          (setf hclone (gfs::create-compatible-bitmap screen-dc gfs::width gfs::height))
          (gfs::select-object memdc-dest hclone)
          (gfs::select-object memdc-src horig)
          (gfs::bit-blt memdc-dest 0 0 gfs::width gfs::height memdc-src 0 0 gfs::+blt-srccopy+))))
    (unless (gfs:null-handle-p screen-dc)
      (gfs::release-dc (cffi:null-pointer) screen-dc))
    hclone))

;;;
;;; methods
;;;

(defmethod gfs:dispose ((im image))
  (let ((hgdi (gfs:handle im)))
    (unless (gfs:null-handle-p hgdi)
      (gfs::delete-object hgdi)))
  (setf (slot-value im 'gfs:handle) nil))

(defmethod data-object ((self image) &optional gc)
  (declare (ignore gc))
  (when (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (image->data (gfs:handle self)))

(defmethod (setf data-object) ((id image-data) (self image))
  (unless (gfs:disposed-p self)
    (gfs:dispose self))
  (setf (slot-value self 'gfs:handle) (data->image id)))

(defmethod initialize-instance :after ((self image) &key file size &allow-other-keys)
  (cond
    (file
      (load self file))
    (size
      (cffi:with-foreign-object (bih-ptr 'gfs::bitmapinfoheader)
        (gfs::zero-mem bih-ptr gfs::bitmapinfoheader)
        (cffi:with-foreign-slots ((gfs::bisize gfs::biwidth gfs::biheight gfs::biplanes
                                   gfs::bibitcount gfs::bicompression)
                                  bih-ptr gfs::bitmapinfoheader)
          (setf gfs::bisize        (cffi:foreign-type-size 'gfs::bitmapinfoheader)
                gfs::biwidth       (gfs:size-width size)
                gfs::biheight      (- (gfs:size-height size))
                gfs::biplanes      1
                gfs::bibitcount    32
                gfs::bicompression gfs::+bi-rgb+)
          (let ((nptr (cffi:null-pointer))
                (hbmp (cffi:null-pointer)))
            (cffi:with-foreign-object (buffer :pointer)
              (gfs::with-compatible-dcs (nptr memdc)
                (setf hbmp (gfs::create-dib-section memdc bih-ptr gfs::+dib-rgb-colors+ buffer nptr 0))))
            (setf (slot-value self 'gfs:handle) hbmp)))))))

(defmethod load ((self image) path)
  (let ((data (make-instance 'image-data)))
    (load data path)
    (setf (data-object self) data)
    data))

(defmethod size ((self image))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((size (gfs:make-size))
        (himage (gfs:handle self)))
    (cffi:with-foreign-object (bmp-ptr 'gfs::bitmap)
      (cffi:with-foreign-slots ((gfs::width gfs::height) bmp-ptr gfs::bitmap)
        (gfs::get-object himage (cffi:foreign-type-size 'gfs::bitmap) bmp-ptr)
        (setf (gfs:size-width size) gfs::width
              (gfs:size-height size) gfs::height)))
    size))

(defmethod transparency-mask ((self image))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((pixel-pnt (transparency-pixel-of self))
        (hbmp (gfs:handle self))
        (hmask (cffi:null-pointer))
        (nptr (cffi:null-pointer)))
    (if pixel-pnt
      (progn
        (cffi:with-foreign-object (bmp-ptr 'gfs::bitmap)
          (gfs::get-object (gfs:handle self) (cffi:foreign-type-size 'gfs::bitmap) bmp-ptr)
          (cffi:with-foreign-slots ((gfs::width gfs::height) bmp-ptr gfs::bitmap)
            (setf hmask (gfs::create-bitmap gfs::width gfs::height 1 1 (cffi:null-pointer)))
            (if (gfs:null-handle-p hmask)
              (error 'gfs:win32-error :detail "create-bitmap failed"))
            (gfs::with-compatible-dcs (nptr memdc1 memdc2)
              (gfs::select-object memdc1 hbmp)
              (gfs::set-bk-color memdc1 (gfs::get-pixel memdc1
                                                        (gfs:point-x pixel-pnt)
                                                        (gfs:point-y pixel-pnt)))
              (gfs::select-object memdc2 hmask)
              (gfs::bit-blt memdc2 0 0 gfs::width gfs::height memdc1 0 0 gfs::+blt-srccopy+))))
        (make-instance 'image :handle hmask))
      nil)))
