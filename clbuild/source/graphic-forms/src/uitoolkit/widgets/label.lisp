;;;;
;;;; label.lisp
;;;;
;;;; Copyright (C) 2006-2007, Jack D. Unrue
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

;;;
;;; helper functions
;;;

(defun compute-image-style-flags (style)
  (let ((flags (logior gfs::+ss-bitmap+ gfs::+ss-realsizeimage+ gfs::+ss-centerimage+)))
    (when (find :raised style) ; FIXME: this style not yet working
      (setf flags (logand (lognot gfs::+ss-sunken+) flags))
      (setf flags (logior flags gfs::+ss-etchedframe+)))
    (when (find :sunken style)
      (setf flags (logand (lognot gfs::+ss-etchedframe+) flags))
      (setf flags (logior flags gfs::+ss-sunken+)))
    flags))

(defun compute-text-style-flags (style)
  (let ((flags 0))
    (unless (intersection style (list :beginning :center :end))
      (setf flags (logior gfs::+ss-center+ gfs::+ss-centerimage+ flags)))
    (loop for sym in style
          do (cond
               ;; primary text static styles
               ;;
               ((eq sym :beginning)
                  (setf flags (logior flags gfs::+ss-leftnowordwrap+))) ; FIXME: i18n
               ((eq sym :center)
                  (setf flags (logior flags gfs::+ss-center+)))
               ((eq sym :end)
                  (setf flags (logior flags gfs::+ss-right+))) ; FIXME: i18n

               ;; styles that can be combined
               ;;
               ((eq sym :ellipsis)
                  (setf flags (logior flags gfs::+ss-endellipsis+)))
               ((eq sym :raised) ; FIXME: this style not yet working
                  (setf flags (logand (lognot gfs::+ss-sunken+) flags))
                  (setf flags (logior flags gfs::+ss-etchedframe+)))
               ((eq sym :sunken)
                  (setf flags (logand (lognot gfs::+ss-etchedframe+) flags))
                  (setf flags (logior flags gfs::+ss-sunken+)))
               ((eq sym :wrap)
                  (setf flags (logand (lognot gfs::+ss-leftnowordwrap+) flags))
                  (setf flags (logior flags gfs::+ss-left+)))))
    flags))

;;;
;;; methods
;;;

(defmethod (setf gfg:background-color) (color (self label))
  (declare (ignorable color))
  (call-next-method)
  (let ((image (image self))
        (pnt (pixel-point-of self)))
    (when image
      (if pnt
        (setf (gfg:transparency-pixel-of image) pnt))
      (setf (image self) image))))

(defmethod compute-style-flags ((self label) &rest extra-data)
  (if (> (count-if-not #'null extra-data) 1)
    (error 'gfs:toolkit-error :detail "only one of :image, :separator, or :text are allowed"))
  (let ((std-style (logior gfs::+ws-child+
                           gfs::+ws-visible+
                           (cond
                             ((first extra-data)
                                (compute-image-style-flags (style-of self)))
                             ((second extra-data)
                                (if (find :vertical (style-of self))
                                  gfs::+ss-etchedvert+
                                  gfs::+ss-etchedhorz+))
                             (t
                                 (compute-text-style-flags (style-of self)))))))
    (values std-style 0)))

(defmethod initialize-instance :after ((self label) &key image parent text &allow-other-keys)
  (create-control self parent text gfs::+icc-standard-classes+)
  (if image
    (setf (image self) image)))

(defmethod image ((label label))
  (if (gfs:disposed-p label)
    (error 'gfs:disposed-error))
  (let ((addr (gfs::send-message (gfs:handle label) gfs::+stm-getimage+ gfs::+image-bitmap+ 0)))
    (if (zerop addr)
      nil
      (make-instance 'gfg:image :handle (cffi:make-pointer addr)))))

(defmethod (setf image) ((image gfg:image) (label label))
  (if (or (gfs:disposed-p label) (gfs:disposed-p image))
    (error 'gfs:disposed-error))
  (let* ((orig-flags (get-native-style label))
         (etch-flags (logior (logand orig-flags gfs::+ss-etchedframe+)
                             (logand orig-flags gfs::+ss-sunken+)))
         (flags (logior etch-flags
                        gfs::+ss-bitmap+
                        gfs::+ss-realsizeimage+
                        gfs::+ss-centerimage+
                        (logior gfs::+ws-child+ gfs::+ws-visible+)))
         (tr-pnt (gfg:transparency-pixel-of image)))
    (if tr-pnt
      (let* ((color (gfg:background-color label))
             (size (gfg:size image))
             (bounds (gfs:make-rectangle :size size))
             (tmp-image (make-instance 'gfg:image :size size)))
        (with-graphics-context (gc tmp-image)
          (setf (gfg:background-color gc) color)
          (let ((orig-color (gfg:foreground-color gc)))
            (setf (gfg:foreground-color gc) color)
            (gfg:draw-filled-rectangle gc bounds)
            (setf (gfg:foreground-color gc) orig-color))
          (gfg:draw-image gc image (gfs:location bounds))
          (setf (pixel-point-of label) (gfs:copy-point tr-pnt)))
        (setf image tmp-image)))
    (if (/= orig-flags flags)
      (update-native-style label flags))
    (gfs::send-message (gfs:handle label)
                       gfs::+stm-setimage+
                       gfs::+image-bitmap+
                       (cffi:pointer-address (gfs:handle image)))))

(defmethod preferred-size ((self label) width-hint height-hint)
  (let ((bits (get-native-style self))
        (b-width (* (border-width self) 2)))
    (if (= (logand bits gfs::+ss-bitmap+) gfs::+ss-bitmap+)
      (let ((image (image self)))
        (if image
          (let ((size (gfg:size image)))
            (gfs:make-size :width (+ (gfs:size-width size) b-width)
                           :height (+ (gfs:size-height size) b-width)))
          (gfs:make-size)))
      (let ((flags (logior gfs::+dt-editcontrol+ gfs::+dt-expandtabs+))
            (size nil))
        (if (and (= (logand bits gfs::+ss-left+) gfs::+ss-left+) (> width-hint 0))
          (setf flags (logior flags gfs::+dt-wordbreak+)))
        (setf size (widget-text-size self #'text flags))
        (if (>= width-hint 0)
          (setf (gfs:size-width size) width-hint)
          (incf (gfs:size-width size) b-width))
        (if (>= height-hint 0)
          (setf (gfs:size-height size) height-hint)
          (incf (gfs:size-width size) b-width))
        size))))

(defmethod text ((self label))
  (get-widget-text self))

(defmethod (setf text) (str (self label))
  (let* ((orig-flags (get-native-style self))
         (etch-flags (logior (logand orig-flags gfs::+ss-etchedframe+)
                             (logand orig-flags gfs::+ss-sunken+))))
    (multiple-value-bind (std-flags ex-flags)
        (compute-style-flags self nil nil str)
      (declare (ignore ex-flags))
      (update-native-style self (logior etch-flags std-flags gfs::+ws-child+ gfs::+ws-visible+))))
  (set-widget-text self str))

(defmethod text-baseline ((self label))
  (let ((b-width (border-width self)))
    (if (test-native-style self gfs::+ss-bitmap+)
      (let ((image (image self)))
        (if image
          (+ (gfs:size-height (gfg:size image)) b-width)
          (floor b-width 2)))
      (widget-text-baseline self 0))))
