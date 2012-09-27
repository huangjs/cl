;;;;
;;;; icon-bundle-unit-tests.lisp
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

(in-package :graphic-forms.uitoolkit.tests)

(define-test bmp-file-icon-bundle-test
  (let ((bundle (make-instance 'gfg:icon-bundle :file (merge-pathnames "happy.bmp")))
        (size (gfs:make-size :width 32 :height 32)))
    (unwind-protect
        (progn
          (assert-equal 1 (gfg:icon-bundle-length bundle))
          (validate-image (gfg:icon-image-ref bundle 0) size 8)
          (validate-image (gfg:icon-image-ref bundle :large) size 8)
          (validate-image (gfg:icon-image-ref bundle :small) size 8))
      (gfs:dispose bundle))
    (assert-true (gfs:disposed-p bundle))))

(define-test images-icon-bundle-test
  (let ((bundle (make-instance 'gfg:icon-bundle
                               :images (list (make-instance 'gfg:image :file (merge-pathnames "happy.bmp"))
                                             (make-instance 'gfg:image :file (merge-pathnames "blackwhite20x16.bmp"))
                                             (make-instance 'gfg:image :file (merge-pathnames "truecolor16x16.bmp")))))
        (happy-size (gfs:make-size :width 32 :height 32))
        (bw-size (gfs:make-size :width 20 :height 16))
        (tc-size (gfs:make-size :width 16 :height 16)))
    (unwind-protect
        (progn
          (assert-equal 3 (gfg:icon-bundle-length bundle))
          (validate-image (gfg:icon-image-ref bundle 0) happy-size 8)
          (validate-image (gfg:icon-image-ref bundle 1) bw-size 8)
          (validate-image (gfg:icon-image-ref bundle 2) tc-size 16000000)
          (validate-image (gfg:icon-image-ref bundle :small) tc-size 8)
          (validate-image (gfg:icon-image-ref bundle :large) happy-size 8))
      (gfs:dispose bundle))
    (assert-true (gfs:disposed-p bundle))))

(define-test push-images-icon-bundle-test
  (let ((bundle (make-instance 'gfg:icon-bundle))
        (happy-image (make-instance 'gfg:image :file (merge-pathnames "happy.bmp")))
        (bw-image (make-instance 'gfg:image :file (merge-pathnames "blackwhite20x16.bmp")))
        (tc-image (make-instance 'gfg:image :file (merge-pathnames "truecolor16x16.bmp")))
        (happy-size (gfs:make-size :width 32 :height 32))
        (bw-size (gfs:make-size :width 20 :height 16))
        (tc-size (gfs:make-size :width 16 :height 16))
        (bw-point (gfs:make-point :x 0 :y 15)))
    (unwind-protect
        (progn
          (gfg:push-icon-image bw-image bundle bw-point)
          (gfg:push-icon-image tc-image bundle)
          (gfg:push-icon-image happy-image bundle)
          (assert-equal 3 (gfg:icon-bundle-length bundle))
          (validate-image (gfg:icon-image-ref bundle 0) happy-size 8)
          (validate-image (gfg:icon-image-ref bundle 1) tc-size 16000000)
          (validate-image (gfg:icon-image-ref bundle 2) bw-size 8)
          (validate-image (gfg:icon-image-ref bundle :small) tc-size 8)
          (validate-image (gfg:icon-image-ref bundle :large) happy-size 8))
      (gfs:dispose bundle))
    (assert-true (gfs:disposed-p bundle))))

(define-test system-icon-bundle-test
  (let ((size (gfs:make-size :width (gfs::get-system-metrics gfs::+sm-cxicon+)
                             :height (gfs::get-system-metrics gfs::+sm-cyicon+)))
        (bundle (make-instance 'gfg:icon-bundle :system gfg:+warning-icon+)))
    (unwind-protect
        (progn
          (assert-equal 1 (gfg:icon-bundle-length bundle))
          (validate-image (gfg:icon-image-ref bundle 0) size 8)
          (validate-image (gfg:icon-image-ref bundle :small) size 8)
          (validate-image (gfg:icon-image-ref bundle :large) size 8))
      (gfs:dispose bundle))
    (assert-true (gfs:disposed-p bundle))))

(define-test setf-images-icon-bundle-test
  (let ((bundle (make-instance 'gfg:icon-bundle
                               :images (list (make-instance 'gfg:image :file (merge-pathnames "happy.bmp"))
                                             (make-instance 'gfg:image :file (merge-pathnames "truecolor16x16.bmp")))))
        (happy-image (make-instance 'gfg:image :file (merge-pathnames "happy.bmp")))
        (bw-image (make-instance 'gfg:image :file (merge-pathnames "blackwhite20x16.bmp")))
        (happy-size (gfs:make-size :width 32 :height 32))
        (bw-size (gfs:make-size :width 20 :height 16)))
    (unwind-protect
        (progn
          (assert-equal 2 (gfg:icon-bundle-length bundle))
          (setf (gfg:icon-image-ref bundle 0) bw-image)
          (setf (gfg:icon-image-ref bundle 1) happy-image)
          (assert-equal 2 (gfg:icon-bundle-length bundle))
          (validate-image (gfg:icon-image-ref bundle 0) bw-size 16000000)
          (validate-image (gfg:icon-image-ref bundle 1) happy-size 8))
      (gfs:dispose bundle))
    (assert-true (gfs:disposed-p bundle))))
