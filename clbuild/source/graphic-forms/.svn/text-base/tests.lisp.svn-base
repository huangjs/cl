;;;;
;;;; tests.lisp
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

(in-package #:graphic-forms-system)

(defun load-tests ()
    (setf *gf-tests-dir* (concatenate 'string gfsys::*gf-dir* "src/tests/uitoolkit/"))
    (setf *textedit-dir* (concatenate 'string gfsys::*gf-dir* "src/demos/textedit/"))
    (setf *unblocked-dir* (concatenate 'string gfsys::*gf-dir* "src/demos/unblocked/"))
    (setf *default-pathname-defaults* (parse-namestring *gf-tests-dir*))
    (asdf:operate 'asdf:load-op :graphic-forms-tests)
    (loop for file in '("test-utils.lisp" "mock-objects" "color-unit-tests"
                        "graphics-context-unit-tests" "image-unit-tests"
                        "icon-bundle-unit-tests" "layout-unit-tests"
                        "flow-layout-unit-tests" "widget-unit-tests"
                        "item-manager-unit-tests" "misc-unit-tests"
                        "border-layout-unit-tests")
          do (load (merge-pathnames file *gf-tests-dir*))))
