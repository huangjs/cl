;;;;
;;;; event-source.lisp
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

(defparameter *callback-info* '((gfw:event-activate . (gfw:event-source))
                                (gfw:event-arm      . (gfw:event-source))
                                (gfw:event-modify   . (gfw:event-source))
                                (gfw:event-select   . (gfw:event-source))
                                (gfw:event-scroll   . (gfw:event-source symbol symbol))))

(defun make-specializer-list (disp-class arg-info)
  (let ((tmp (mapcar #'find-class arg-info)))
    (push disp-class tmp)
    tmp))

(defun define-dispatcher-for-callbacks (callbacks)
  (let ((*print-gensym* nil)
        (class (c2mop:ensure-class (gentemp "EDCLASS" :gfgen)
                                  :direct-superclasses '(event-dispatcher))))
    (loop for pair in callbacks
          do (let* ((method-sym (car pair))
                    (fn (cdr pair))
                    (arg-info (cdr (assoc method-sym *callback-info*)))
                    (args nil))
              `(unless (or (symbolp ,fn) (functionp ,fn))
                 (error 'gfs:toolkit-error
                        :detail "callback must be function or symbol naming function"))
               (if (null arg-info)
                 (error 'gfs:toolkit-error :detail (format nil
                                                           "unsupported event method for callbacks: ~a"
                                                           method-sym)))
               (dotimes (i (1+ (length arg-info)))
                 (push (gentemp "ARG" :gfgen) args))
               (c2mop:ensure-method (ensure-generic-function method-sym :lambda-list args)
                                    `(lambda ,args (funcall ,fn ,@args))
                                    :specializers (make-specializer-list class arg-info))))
    class))

(defun define-dispatcher (classname callback)
  (let ((proto (c2mop:class-prototype (find-class classname))))
    (define-dispatcher-for-callbacks `((,(callback-event-name-of proto) . ,callback)))))

;;;
;;; methods
;;;

(defmethod initialize-instance :after ((self event-source) &key callbacks dispatcher &allow-other-keys)
  (unless (or dispatcher (null callbacks))
    (let ((class (define-dispatcher-for-callbacks callbacks)))
      (setf (dispatcher self) (make-instance (class-name class))))))

(defmethod owner :before ((self event-source))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod parent :before ((self event-source))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error)))

(defmethod print-object ((self event-source) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "handle: ~x " (gfs:handle self))
    (format stream "dispatcher: ~a " (dispatcher self))))
