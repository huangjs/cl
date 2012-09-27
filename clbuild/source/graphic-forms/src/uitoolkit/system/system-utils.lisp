;;;;
;;;; system-utils.lisp
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

(in-package :graphic-forms.uitoolkit.system)

;;;
;;; convenience functions
;;;

(defun debug-format (str &rest args)
  (apply #'format *trace-output* str args)
  (finish-output))

(defun debug-print (thing)
  (print thing *trace-output*)
  (finish-output))

(defun recreate-array (array)
  (make-array (array-dimensions array)
              :element-type (array-element-type array)
              :adjustable (adjustable-array-p array)
              :fill-pointer (if (array-has-fill-pointer-p array) 0 nil)))

(defun indexed-sort (sequence predicate key)
  (let ((result (cond
                  ((listp sequence)
                     nil)
                  ((vectorp sequence)
                     (recreate-array sequence))
                  (t
                     (error 'gfs:toolkit-error :detail (format nil "unsupported type: ~a" sequence)))))
        (tmp nil))
    (dotimes (i (length sequence))
      (let ((item (elt sequence i)))
        (pushnew (list (funcall key item) item) tmp)))
    (setf tmp (sort (reverse tmp) predicate :key #'first))
    (cond
      ((listp result)
        (setf result (loop for item in tmp collect (second item))))
      ((adjustable-array-p result)
        (dotimes (i (length tmp)) (vector-push (second (elt tmp i)) result)))
      (t
        (dotimes (i (length tmp)) (setf (elt result i) (second (elt tmp i))))))
    result))

(defun pick-elements (sequence indices &optional count)
  (let ((picks nil))
    (if (cffi:pointerp indices)
      (dotimes (i count)
        (push (elt sequence (mem-aref indices :unsigned-int i)) picks))
      (dotimes (i (length indices))
        (push (elt sequence (elt indices i)) picks)))
    (reverse picks)))

(defun add-element (element sequence index)
  (cond
    ((listp sequence)
       (push element sequence))
    ((adjustable-array-p sequence)
       (vector-push-extend element sequence))
    (t
       (setf (elt sequence index) element)))
  sequence)

(defun remove-element (sequence index creator)
  (let ((result nil)
        (victim nil))
    (dotimes (i (length sequence))
      (if (= i index)
        (setf victim (elt sequence i))
        (setf result (add-element (elt sequence i)
                                  (or result (if creator (funcall creator) nil))
                                  (if victim (1- i) i)))))
    (if (listp result)
      (values (reverse result) victim)
      (values result victim))))

(defun remove-elements (sequence span creator)
  (let ((result nil)
        (victims nil))
    (dotimes (i (length sequence))
      (if (and (>= i (gfs:span-start span)) (<= i (gfs:span-end span)))
        (push (elt sequence i) victims)
        (setf result (add-element (elt sequence i)
                                  (or result (if creator (funcall creator) nil))
                                  (- i (length victims))))))
    (if (listp result)
      (values (reverse result) (reverse victims))
      (values result (reverse victims)))))

(defun flatten (tree)
  (if (cl:atom tree)
    (list tree)
    (mapcan (function flatten) tree)))

(defun clamp-size (proposed-size min-size max-size)
  (let ((clamped-size (make-size :width (gfs:size-width proposed-size)
                                 :height (gfs:size-height proposed-size))))
    (when min-size
      (if (< (gfs:size-width proposed-size) (gfs:size-width min-size))
        (setf (gfs:size-width clamped-size) (gfs:size-width min-size)))
      (if (< (gfs:size-height proposed-size) (gfs:size-height min-size))
        (setf (gfs:size-height clamped-size) (gfs:size-height min-size))))
    (when max-size
      (if (> (gfs:size-width proposed-size) (gfs:size-width max-size))
        (setf (gfs:size-width clamped-size) (gfs:size-width max-size)))
      (if (> (gfs:size-height proposed-size) (gfs:size-height max-size))
        (setf (gfs:size-height clamped-size) (gfs:size-height max-size))))
    clamped-size))

;;; lifted from lispbuilder-windows/windows/util.lisp
;;; author: Frank Buss
;;;
(defmacro zero-mem (object type)
  (let ((i (gensym)))
    `(loop for ,i from 0 below (foreign-type-size (quote ,type)) do
           (setf (mem-aref ,object :char ,i) 0))))

#+lispworks
(defun native-object-special-action (obj)
  (if (typep obj 'gfs:native-object)
    (gfs:dispose obj)))

(declaim (inline lparam-high-word))
(defun lparam-high-word (lparam)
  (ash (logand #xFFFF0000 lparam) -16))

(declaim (inline lparam-low-word))
(defun lparam-low-word (lparam)
  (logand #x0000FFFF lparam))

(declaim (inline make-lparam))
(defun make-lparam (hi lo)
  (logior (ash (logand lo #xFFFF) 16) (logand hi #xFFFF)))

(defun load-library-wrapper (dll-path)
  (let ((hmodule (cffi:null-pointer)))
    (cffi:with-foreign-string (str-ptr dll-path)
      (setf hmodule (load-library str-ptr (cffi:null-pointer) 0)))
    (when (null-handle-p hmodule)
      (warn 'toolkit-warning :detail (format nil "could not load ~s" dll-path)))
    hmodule))

(defun retrieve-function-pointer (hmodule func-name)
  (let ((func-ptr (cffi:null-pointer)))
    (if (null-handle-p hmodule)
      (error 'toolkit-error :detail "null module handle"))
    (cffi:with-foreign-string (str-ptr func-name)
      (setf func-ptr (gfs::get-proc-address hmodule str-ptr)))
    (if (gfs:null-handle-p func-ptr)
      (let ((detail (format nil "could not get function pointer for ~s" func-name)))
        (warn 'gfs:toolkit-warning :detail detail)))
    func-ptr))

;;;
;;; convenience macros
;;;

(defmacro with-rect ((rect-var) &body body)
  `(cffi:with-foreign-object (,rect-var 'gfs::rect)
     (cffi:with-foreign-slots ((gfs::left gfs::right gfs::top gfs::bottom)
                               ,rect-var gfs::rect)
       (zero-mem ,rect-var gfs::rect)
       ,@body)))

(defmacro with-hfont-selected ((hdc hfont) &body body)
  (let ((hfont-old (gensym)))
    `(let ((,hfont-old nil))
       (unwind-protect
           (progn
             (setf ,hfont-old (gfs::select-object ,hdc ,hfont))
             ,@body)
         (unless (gfs:null-handle-p ,hfont-old)
           (gfs::select-object ,hdc ,hfont-old))))))

(defmacro with-retrieved-dc ((hwnd hdc-var) &body body)
  `(let ((,hdc-var nil))
     (unwind-protect
         (progn
           (setf ,hdc-var (gfs::get-dc ,hwnd))
           (if (gfs:null-handle-p ,hdc-var)
              (error 'gfs:win32-error :detail "get-dc failed"))
           ,@body)
       (gfs::release-dc ,hwnd ,hdc-var))))

(defmacro with-compatible-dcs ((orig-dc &rest hdc-vars) &body body)
  `(let ,(loop for var in hdc-vars
               collect `(,var (gfs::create-compatible-dc ,orig-dc)))
     (unwind-protect
         (progn
           ,@body)
       ,@(loop for var2 in hdc-vars
               collect `(gfs::delete-dc ,var2)))))
