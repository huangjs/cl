;;;;
;;;; image-data.lisp
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *image-plugins* nil)

  (cffi:defctype bmp-pointer :pointer))

;;
;; list the superset of file extensions for formats that any
;; plugin might support (clearly there are more formats than
;; this extant in the world, so add more as needed)
;;
(defvar *image-file-types* (let ((table (make-hash-table :test #'equal)))
                             (loop for (key value) in '(("ani"  "Microsoft Windows animated cursor")
                                                        ("bmp"  "Microsoft Windows bitmap")
                                                        ("cur"  "Microsoft Windows cursor")
                                                        ("dib"  "Microsoft Windows device-independent bitmap")
                                                        ("emf"  "Microsoft Windows Enhanced Metafile")
                                                        ("eps"  "Adobe Encapsulated PostScript")
                                                        ("fax"  "Group 3 TIFF")
                                                        ("fig"  "FIG graphics format")
                                                        ("gif"  "CompuServe Graphics Interchange Format")
                                                        ("ico"  "Microsoft Windows icon")
                                                        ("jpeg" "Joint Photographic Experts Group")
                                                        ("jpg"  "Joint Photographic Experts Group")
                                                        ("pbm"  "Portable bitmap format (b/w)")
                                                        ("pcd"  "Photo CD")
                                                        ("pcl"  "HP Page Control Language")
                                                        ("pcx"  "ZSoft IBM PC Paintbrush")
                                                        ("pdf"  "Portable Document Format")
                                                        ("pgm"  "Portable graymap")
                                                        ("pix"  "Alias/Wavefront RLE")
                                                        ("png"  "Portable Network Graphics")
                                                        ("ppm"  "Portable pixmap (color)")
                                                        ("ps"   "Adobe PostScript")
                                                        ("svg"  "Scalable Vector Graphics")
                                                        ("tga"  "Truevision Targa")
                                                        ("tiff" "Tagged Image File")
                                                        ("wmf"  "Microsoft Windows Metafile")
                                                        ("xbm"  "X Window System bitmap (b/w)")
                                                        ("xpm"  "X Window System pixmap (color)"))
                               do (setf (gethash key table) value))
                             table))

;;;
;;; helper functions
;;;

(defun make-initial-bitmapinfo (plugin)
  (let ((bi-ptr (cffi:foreign-alloc 'gfs::bitmapinfo)))
    (cffi:with-foreign-slots ((gfs::bisize gfs::biwidth gfs::biheight gfs::biplanes gfs::bibitcount
                               gfs::bicompression gfs::bmicolors)
                              bi-ptr gfs::bitmapinfo)
      (gfs::zero-mem bi-ptr gfs::bitmapinfo)
      (setf gfs::bisize        (cffi:foreign-type-size 'gfs::bitmapinfoheader)
            gfs::biplanes      1
            gfs::bibitcount    (depth plugin)
            gfs::bicompression gfs::+bi-rgb+)
      (let ((im-size (size plugin)))
        (setf gfs::biwidth  (gfs:size-width im-size)
              gfs::biheight (- (gfs:size-height im-size)))))
    bi-ptr))

(defun load-image-data (path)
  (loop for loader in *image-plugins*
        for data = (funcall loader path)
        until data
        finally (return data)))

(defun plugin->image (plugin)
  (let ((screen-dc (gfs::get-dc (cffi:null-pointer)))
        (hbmp (cffi:null-pointer)))
    (unwind-protect
        (cffi:with-foreign-object (pix-bits-ptr :pointer)
          (setf hbmp (gfs::create-dib-section screen-dc
                                              plugin
                                              gfs::+dib-rgb-colors+
                                              pix-bits-ptr
                                              (cffi:null-pointer)
                                              0))
          (if (gfs:null-handle-p hbmp)
            (error 'gfs:win32-error :detail "create-dib-section failed"))
          (copy-pixels plugin (cffi:mem-ref pix-bits-ptr :pointer)))
      (gfs::release-dc (cffi:null-pointer) screen-dc))
    hbmp))

(defun data->image (self)
  (plugin->image (data-plugin-of self)))

(defun image->data (hbmp) (declare (ignore hbmp)))
#|
(defun image->data (hbmp)
  "Convert the native bitmap handle to an image-data."
  (let ((mem-dc (gfs::create-compatible-dc (cffi:null-pointer)))
        (raw-bits nil)
        (data nil)
        (sz nil)
        (byte-count 0))
    (unwind-protect
        (progn
          (cffi:with-foreign-object (bc-ptr 'gfs::bitmapcoreheader)
            (cffi:with-foreign-slots ((gfs::bcsize
                                       gfs::bcwidth
                                       gfs::bcheight
                                       gfs::bcbitcount)
                                      bc-ptr gfs::bitmapcoreheader)
              (setf gfs::bcsize (cffi:foreign-type-size 'gfs::bitmapcoreheader))
              (setf gfs::bcbitcount 0)
              (when (zerop (gfs::get-di-bits mem-dc
                                                hbmp
                                                0 0
                                                (cffi:null-pointer)
                                                bc-ptr
                                                gfs::+dib-rgb-colors+))
                (error 'gfs:win32-error :detail "get-di-bits failed <1>"))
              (setf sz (gfs:make-size :width gfs::bcwidth :height gfs::bcheight))
              (setf data (make-image-data :bits-per-pixel gfs::bcbitcount :size sz))))
          (setf byte-count (* (bmp-pixel-row-length (gfs:size-width sz) (bits-per-pixel data))
                              (gfs:size-height sz)))
          (setf raw-bits (cffi:foreign-alloc :unsigned-char :count byte-count))
          (cffi:with-foreign-object (bi-ptr 'gfs::bitmapinfo)
            (cffi:with-foreign-slots ((gfs::bisize
                                       gfs::biwidth
                                       gfs::biheight
                                       gfs::biplanes
                                       gfs::bibitcount
                                       gfs::bicompression
                                       gfs::biclrused
                                       gfs::bmicolors)
                                      bi-ptr gfs::bitmapinfo)
              (setf gfs::bisize (cffi:foreign-type-size 'gfs::bitmapinfoheader))
              (setf gfs::biwidth (gfs:size-width sz))
              (setf gfs::biheight (gfs:size-height sz))
              (setf gfs::biplanes 1)
              (setf gfs::bibitcount (bits-per-pixel data))
              (setf gfs::bicompression gfs::+bi-rgb+)
              (when (zerop (gfs::get-di-bits mem-dc
                                                hbmp
                                                0 (gfs:size-height sz)
                                                raw-bits
                                                bi-ptr
                                                gfs::+dib-rgb-colors+))
                (error 'gfs:win32-error :detail "get-di-bits failed <2>"))

              ;; process the RGBQUADs
              ;;
              (let ((color-count 0))
                (if (= gfs::biclrused 0)
                  (progn
                    (case (bits-per-pixel data)
                      (1 (setf color-count 2))
                      (4 (setf color-count 16))
                      (8 (setf color-count 256))))
                  (setf color-count gfs::biclrused))
                (let ((colors (make-array color-count)))
                  (dotimes (i color-count)
                    (cffi:with-foreign-slots ((gfs::rgbblue gfs::rgbgreen gfs::rgbred)
                                              (cffi:mem-aref gfs::bmicolors 'gfs::rgbquad i)
                                              gfs::rgbquad)
                      (setf (aref colors i) (make-color :red gfs::rgbred
                                                        :green gfs::rgbgreen
                                                        :blue gfs::rgbblue))))
                  (setf (image-data-palette data) (make-palette :direct nil :table colors))))))

          ;; process the pixel data
          ;;
          (let ((pix-bytes (make-array byte-count :element-type '(unsigned-byte 8))))
            (dotimes (i byte-count)
              (setf (aref pix-bytes i) (cffi:mem-aref raw-bits :unsigned-char i)))
            (setf (image-data-pixels data) pix-bytes)))
      (unless (cffi:null-pointer-p raw-bits)
        (cffi:foreign-free raw-bits))
      (gfs::delete-dc mem-dc))
    data))
|#

;;;
;;; methods
;;;

(defgeneric copy-pixels (self pixels-pointer))

(defmethod depth ((self image-data))
  (depth (data-plugin-of self)))

(defmethod gfs:dispose ((self image-data))
  (let ((victim (data-plugin-of self)))
    (unless (null victim)
      (gfs:dispose victim)))
  (setf (slot-value self 'data-plugin) nil))

(defmethod load ((self image-data) path)
  (setf path (cond
               ((typep path 'pathname) (namestring (merge-pathnames path)))
               ((typep path 'string) (namestring (merge-pathnames path)))
               (t
                 (error 'gfs:toolkit-error :detail "pathname or string required"))))
  (let ((plugin (data-plugin-of self))
        (plugins nil))
    (if plugin
      (setf plugins (load plugin path))
      (setf plugins (load-image-data path)))
    (unless plugins
      (error 'gfs:toolkit-error :detail (format nil "no image data plugin supports: ~a" path)))
    (setf (slot-value self 'data-plugin) (first plugins))
    (append (list self) (loop for p in (rest plugins)
                              collect (make-instance 'image-data :data-plugin p)))))

(defmethod size ((self image-data))
  (size (data-plugin-of self)))

(defmethod (setf size) (size (self image-data))
  (setf (size (data-plugin-of self)) size))

(defmethod print-object ((self image-data) stream)
  (if (or (null (gfs:handle self)) (cffi:null-pointer-p (gfs:handle self)))
    (error 'gfs:disposed-error))
  (let ((size (size self)))
    (print-unreadable-object (self stream :type t)
      ;; FIXME: dump palette info, too
      ;;
      (format stream "width: ~a " (gfs:size-width size))
      (format stream "height: ~a " (gfs:size-height size))
      (format stream "bits per pixel: ~a " (depth self)))))
