;;;;
;;;; layout-classes.lisp
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

(in-package :graphic-forms.uitoolkit.widgets)

(defclass layout-manager ()
  ((style
    :accessor style-of
    :initarg :style
    :initform nil)
   (left-margin
    :accessor left-margin-of
    :initarg :left-margin
    :initform 0)
   (top-margin
    :accessor top-margin-of
    :initarg :top-margin
    :initform 0)
   (right-margin
    :accessor right-margin-of
    :initarg :right-margin
    :initform 0)
   (bottom-margin
    :accessor bottom-margin-of
    :initarg :bottom-margin
    :initform 0)
   (data
    :accessor data-of
    :initform nil))
  (:documentation "Subclasses implement layout strategies to manage space within windows."))

(defclass border-layout (layout-manager) ()
  (:documentation "Window children are assigned a position on the edges or center of a container."))

(defclass flow-layout (layout-manager)
  ((spacing
    :accessor spacing-of
    :initarg :spacing
    :initform 0))
  (:documentation "Window children are arranged in a row or column."))

(defclass heap-layout (layout-manager)
  ((top-child
    :accessor top-child-of
    :initarg :top-child
    :initform nil))
  (:documentation "Window children are stacked one on top of the other."))
