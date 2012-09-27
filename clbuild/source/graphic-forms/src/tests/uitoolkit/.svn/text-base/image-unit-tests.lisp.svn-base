;;;;
;;;; image-unit-tests.lisp
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

(defun image-data-tester (path)
  (let ((d1 (make-instance 'gfg:image-data))
        (d2 nil)
        (d3 nil)
        (im (make-instance 'gfg:image))
        (hbmp (cffi:null-pointer)))
    (unwind-protect
        (progn
          (gfg:load d1 path)
          (cffi:with-foreign-string (ptr path)
            (setf hbmp (gfs::load-image nil
                                        ptr
                                        gfs::+image-bitmap+
                                        0 0
                                        (logior gfs::+lr-loadfromfile+
                                                gfs::+lr-createdibsection+))))
          (if (gfs:null-handle-p hbmp)
            (error 'gfs:win32-error :detail "load-image failed"))
          (setf d2 (gfg::image->data hbmp))
          (assert-equal (gfg:depth d1) (gfg:depth d2) path)
          (let ((size1 (gfg:size d1))
                (size2 (gfg:size d2)))
            (assert-equal (gfs:size-width size1) (gfs:size-width size2) path)
            (assert-equal (gfs:size-height size1) (gfs:size-height size2) path))
          (gfg:load im path)
          (setf d3 (gfg:data-object im))
          (assert-equal (gfg:depth d1) (gfg:depth d3) path)
          (let ((size1 (gfg:size d1))
                (size2 (gfg:size d3)))
            (assert-equal (gfs:size-width size1) (gfs:size-width size2) path)
            (assert-equal (gfs:size-height size1) (gfs:size-height size2) path))
      (unless (gfs:disposed-p im)
        (gfs:dispose im))
      (unless (gfs:null-handle-p hbmp)
        (gfs::delete-object hbmp))))))

#|
(define-test image-data-loading-test
  (mapc #'image-data-tester '("blackwhite20x16.bmp" "happy.bmp" "truecolor16x16.bmp")))
|#
