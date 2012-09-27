;;;;
;;;; item-manager.lisp
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

(defun make-items-array (&optional (count 7))
  (make-array count :fill-pointer 0 :adjustable t))

(defun call-text-provider (manager thing)
  (let ((func (text-provider-of manager))
        (*print-readably* nil))
    (cond
      ((stringp thing)
         thing)
      ((null func)
         (format nil "~a" thing))
      (t
         (funcall func thing)))))

(defun copy-item-sequence (handle new-items item-class)
  (let ((tc (thread-context))
        (replacements (make-items-array)))
    (cond
      ((null new-items)
         replacements)
      ((vectorp new-items)
         (dotimes (i (length new-items))
           (let ((item (elt new-items i)))
             (if (typep item item-class)
               (progn
                 (setf (slot-value item 'gfs:handle) handle)
                 (vector-push-extend item replacements))
               (let ((tmp (make-instance item-class :handle handle :data item)))
                 (put-item tc tmp)
                 (vector-push-extend tmp replacements)))))
         replacements)
      ((listp new-items)
         (loop for item in new-items
               do (if (typep item item-class)
                    (progn
                      (setf (slot-value item 'gfs:handle) handle)
                      (vector-push-extend item replacements))
                    (let ((tmp (make-instance item-class :handle handle :data item)))
                      (put-item tc tmp)
                      (vector-push-extend tmp replacements))))
         replacements)
      (t
         (error 'gfs:toolkit-error :detail (format nil "invalid data structure type: ~a" new-items))))))

;;;
;;; methods
;;;

(defmethod append-item :before ((self item-manager) thing (disp event-dispatcher) &optional checked disabled classname)
  (declare (ignore thing disp checked disabled classname))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod delete-all ((self item-manager))
  (let ((items (slot-value self 'items)))
    (dotimes (i (length items))
      (gfs:dispose (aref items i))))
  (setf (slot-value self 'items) (make-items-array)))

(defmethod delete-item :before ((self item-manager) index)
  (declare (ignore index))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod delete-item ((self item-manager) index)
  (if (or (< index 0) (>= index (length (slot-value self 'items))))
    (error 'gfs:toolkit-error :detail "invalid item index"))
  (multiple-value-bind (new-items victim)
      (gfs::remove-element (slot-value self 'items) index #'make-items-array)
    (setf (slot-value self 'items) new-items)
    (gfs:dispose victim)))

(defmethod delete-selection :before ((self item-manager))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod delete-span :before ((self item-manager) (sp gfs:span))
  (declare (ignore sp))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod gfs:dispose ((self item-manager))
  (let ((items (slot-value self 'items))
        (tc (thread-context)))
    (dotimes (i (length items))
      (delete-tc-item tc (elt items i)))))

(defmethod item-count :before ((self item-manager))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod item-count ((self item-manager))
  (length (slot-value self 'items)))

(defmethod item-index :before ((self item-manager) (it item))
  (declare (ignore it))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod item-index ((self item-manager) (it item))
  (let ((pos (position it (slot-value self 'items) :test #'items-equal)))
    (if (null pos)
      (return-from item-index 0))
    pos))

(defmethod items-of ((self item-manager))
  (coerce (slot-value self 'items) 'list))

(defmethod selected-items :before ((self item-manager))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod (setf selected-items) :before (items (self item-manager))
  (declare (ignore items))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod update-from-items :before ((self item-manager))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))
