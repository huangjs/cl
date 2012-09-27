;;;;
;;;; config.lisp
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

(defvar *magick-library-directory* "c:/Program Files/ImageMagick-6.2.6-Q16/")

(defpackage #:graphic-forms-system
  (:nicknames #:gfsys)
  (:use :common-lisp :asdf))

(in-package #:graphic-forms-system)

(defvar *cffi-dir*            "cffi-060925/")
(defvar *closer-mop-dir*      "closer-mop/")
(defvar *lw-compat-dir*       "lw-compat/")
(defvar *gf-dir*              "graphic-forms/")
(defvar *gf-tests-dir*        "graphic-forms/src/tests/uitoolkit/")
(defvar *binary-data-dir*     "src/external-libraries/practicals-1.0.3/Chapter08/")
(defvar *macro-utilities-dir* "src/external-libraries/practicals-1.0.3/Chapter24/")
(defvar *textedit-dir*        "src/demos/textedit/")
(defvar *unblocked-dir*       "src/demos/unblocked/")

(defvar *lisp-unit-file*      "src/external-libraries/lisp-unit/lisp-unit.lisp")

(defun configure-asdf ()
  (let ((dir-list (list (concatenate 'string *gf-dir* *binary-data-dir*)
                        (concatenate 'string *gf-dir* *macro-utilities-dir*)
                        *cffi-dir* *closer-mop-dir* *lw-compat-dir* *gf-dir*)))
    (loop for var in dir-list
          do (pushnew var asdf:*central-registry* :test #'equal))))
