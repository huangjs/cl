;;;;
;;;; image-tester.lisp
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

(in-package #:graphic-forms.uitoolkit.tests)

(defvar *image-win*      nil)
(defvar *happy-image*    nil)
(defvar *bw-image*       nil)
(defvar *comp-image*     nil)
(defvar *folder-image*   nil)
(defvar *true-image*     nil)

(defclass image-events (gfw:event-dispatcher) ())

(defun dispose-images ()
  (loop for var in '(*happy-image* *bw-image* *folder-image* *true-image* *comp-image*)
        do (unless (null (symbol-value var))
             (gfs:dispose (symbol-value var))
             (setf (symbol-value var) nil))))

(defmethod gfw:event-close ((d image-events) window)
  (declare (ignore window))
  (dispose-images)
  (gfs:dispose *image-win*)
  (setf *image-win* nil)
  (gfw:shutdown 0))

(defun draw-test-image (gc image origin pixel-pnt)
  (gfg:draw-image gc image origin)
  (incf (gfs:point-x origin) 36)
  (gfg:with-image-transparency (image pixel-pnt)
    (gfg:draw-image gc (gfg:transparency-mask image) origin)
    (incf (gfs:point-x origin) 36)
    (gfg:draw-image gc image origin)))

(defmethod gfw:event-paint ((d image-events) window gc rect)
  (declare (ignore window rect))
  (let ((pnt (gfs:make-point))
        (pixel-pnt1 (gfs:make-point))
        (pixel-pnt2 (gfs:make-point :x 15 :y 0))
        (pixel-pnt3 (gfs:make-point :x 31 :y 31)))
    (declare (ignorable pixel-pnt3))
    (draw-test-image gc *happy-image* pnt pixel-pnt1)
    (setf (gfs:point-x pnt) 0)
    (incf (gfs:point-y pnt) 36)
    (draw-test-image gc *bw-image*    pnt pixel-pnt1)
    (setf (gfs:point-x pnt) 0)
    (incf (gfs:point-y pnt) 36)
    (draw-test-image gc *true-image*  pnt pixel-pnt2)
#+load-imagemagick-plugin
    (progn
      (setf (gfs:point-x pnt) 112)
      (setf (gfs:point-y pnt) 0)
      (draw-test-image gc *folder-image* pnt pixel-pnt1)
      (setf (gfs:point-x pnt) 112)
      (incf (gfs:point-y pnt) 36)
      (draw-test-image gc *comp-image* pnt pixel-pnt3))))

(defun exit-image-fn (disp item)
  (declare (ignorable disp item))
  (dispose-images)
  (gfs:dispose *image-win*)
  (setf *image-win* nil)
  (gfw:shutdown 0))

(defun load-images ()
  (let ((*default-pathname-defaults* (parse-namestring gfsys::*gf-tests-dir*)))
    (setf *happy-image*       (make-instance 'gfg:image :file "happy.bmp")
          *bw-image*          (make-instance 'gfg:image :file "blackwhite20x16.bmp")
          *true-image*        (make-instance 'gfg:image :file "truecolor16x16.bmp"))
    
#+load-imagemagick-plugin
    (progn
      (setf *folder-image* (make-instance 'gfg:image :file "open-folder.gif")
            *comp-image*   (make-instance 'gfg:image :file "computer.png")))))

(defun image-tester-internal ()
  (load-images)
  (let ((menubar nil))
    (setf *image-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'image-events)
                                                    :style '(:workspace)))
    (setf (gfw:size *image-win*) (gfs:make-size :width 250 :height 200))
    (setf (gfw:text *image-win*) "Image Tester")
    (setf menubar (gfw:defmenu ((:item "&File"
                                 :submenu ((:item "E&xit" :callback #'exit-image-fn))))))
    (setf (gfw:menu-bar *image-win*) menubar)
    (setf (gfw:image *image-win*)
          (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico")))
    (gfw:show *image-win* t)))

(defun image-tester ()
  (gfw:startup "Image Tester" #'image-tester-internal))
