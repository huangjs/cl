;;;;
;;;; item.lisp
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

;;;
;;; helper functions
;;;

(defun create-item-with-callback (howner class-symbol thing disp)
  (let ((item nil))
    (cond
      ((null disp)
         (setf item (make-instance class-symbol :data thing :handle howner)))
      ((functionp disp)
         (setf item (make-instance class-symbol :data thing :handle howner :callback disp)))
      ((typep disp 'gfw:event-dispatcher)
         (setf item (make-instance class-symbol :data thing :handle howner :dispatcher disp)))
      (t
         (error 'gfs:toolkit-error
           :detail "callback must be a function, instance of gfw:event-dispatcher, or null")))
    item))

(defun items-equal (item1 item2)
  (= (item-id item1) (item-id item2)))

;;;
;;; methods
;;;

(defmethod check :before ((self item) flag)
  (declare (ignore flag))
  (if (gfs:null-handle-p (gfs:handle self))
    (error 'gfs:toolkit-error :detail "null owner handle")))

(defmethod checked-p :before ((self item))
  (if (gfs:null-handle-p (gfs:handle self))
    (error 'gfs:toolkit-error :detail "null owner handle")))

(defmethod gfs:dispose ((self item))
  (let ((hwnd (gfs:handle self)))
    (unless (or (null hwnd) (cffi:null-pointer-p hwnd))
      (let ((owner (get-widget (thread-context) hwnd)))
        (if owner
          (setf (slot-value owner 'items)
                (remove self (slot-value owner 'items) :test #'items-equal))))))
  (delete-tc-item (thread-context) self)
  (setf (slot-value self 'gfs:handle) nil))

(defmethod initialize-instance :after ((self item) &key callback &allow-other-keys)
  (setf (item-id self) (increment-item-id (thread-context)))
  (when callback
    (unless (typep callback 'function)
      (error 'gfs:toolkit-error :detail ":callback value must be a function"))
    (setf (dispatcher self)
          (make-instance (define-dispatcher (class-name (class-of self)) callback)))))

(defmethod owner ((self item))
  (let ((hwnd (gfs:handle self)))
    (if (gfs:null-handle-p hwnd)
      (error 'gfs:toolkit-error :detail "null owner widget handle"))
    (let ((widget (get-widget (thread-context) hwnd)))
      (if (null widget)
        (error 'gfs:toolkit-error :detail "no owner widget"))
      widget)))

(defmethod print-object ((self item) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "id: ~d " (item-id self))
    (format stream "data: ~a " (data-of self))
    (format stream "handle: ~x " (gfs:handle self))
    (format stream "dispatcher: ~a" (dispatcher self))))
