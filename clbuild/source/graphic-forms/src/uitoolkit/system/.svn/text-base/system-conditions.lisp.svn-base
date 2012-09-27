;;;;
;;;; system-conditions.lisp
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

(in-package :graphic-forms.uitoolkit.system)

(define-condition toolkit-error (simple-error)
  ((detail :reader detail :initarg :detail :initform nil)))

(defmethod print-object ((obj toolkit-error) stream)
  (let ((detail (detail obj)))
    (cond
      (detail
       (format stream "~a" detail))
      (t
       (call-next-method)))))

(define-condition toolkit-warning (simple-warning)
  ((detail :reader detail :initarg :detail :initform nil)))

(defmethod print-object ((obj toolkit-warning) stream)
  (let ((detail (detail obj)))
    (cond
      (detail
       (format stream "~a" detail))
      (t
       (call-next-method)))))

(define-condition disposed-error (error) ())

(define-condition win32-error (toolkit-error)
  ((code :reader code :initarg :code :initform (get-last-error))))

(defmethod print-object ((obj win32-error) stream)
  (format stream "code ~a: ~a" (code obj) (detail obj)))

(define-condition win32-warning (toolkit-warning)
  ((code :reader code :initarg :code :initform (get-last-error))))

(defmethod print-object ((obj win32-warning) stream)
  (format stream "code ~a: ~a" (code obj) (detail obj)))

(define-condition comdlg-error (win32-error)
  ((dlg-code :reader dlg-code :initarg :dlg-code :initform (comm-dlg-extended-error))))

(defmethod print-object ((obj comdlg-error) stream)
  (format stream "common dialog code ~a: ~a" (code obj) (detail obj)))
