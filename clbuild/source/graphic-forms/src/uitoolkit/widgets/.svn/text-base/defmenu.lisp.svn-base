;;;;
;;;; defmenu.lisp
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

(in-package :graphic-forms.uitoolkit.widgets)

;;;
;;; an example menubar definition:
;;;

#|
(gfw:defmenu ((:item "&File" :submenu ((:item "&Open...")
                                       (:item "&Save..." :disabled)
                                       (:item :separator)
                                       (:item "E&xit" :callback #'some-fn)))
              (:item "&Options" :submenu ((:item "&Enabled" :checked)
                                          (:item "&Tools" :submenu ((:item "&Fonts" :disabled)
                                                                    (:item "&Colors")))))
              (:item "&Help" :submenu ((:item "&About" :image some-image)))))
|#

;;;
;;; base class and generic functions
;;;

(defclass base-menu-generator ()
  ((menu-stack :accessor menu-stack-of
               :initform nil)))

(defgeneric define-item (generator label dispatcher disabled checked image)
  (:documentation "Defines a single menu item.")
  (:method (generator label dispatcher disabled checked image)
    (declare (ignorable generator label dispatcher disabled checked image))))

(defgeneric define-submenu (generator label dispatcher disabled)
  (:documentation "Defines a submenu and its associated item on the parent menu.")
  (:method (generator label dispatcher disabled)
    (declare (ignorable generator label dispatcher disabled))))

(defgeneric define-separator (generator)
  (:documentation "Defines a separator.")
  (:method (generator)
    (declare (ignorable generator))))

(defgeneric complete-submenu (generator)
  (:documentation "Finish any processing associated with the menu.")
  (:method (generator)
    (declare (ignorable generator))))

(defun item-form-p (form)
  (and (consp form)
       (eq (car form) :item)))

;;;
;;; menu system form parser
;;;

(defun process-item-form (form generator-sym)
  (if (not (item-form-p form))
    (error 'gfs:toolkit-error :detail (format nil "form ~a not a menu item definition" form)))
  (let ((callback nil)
        (code nil)
        (checked nil)
        (disabled nil)
        (disp nil)
        (image nil)
        (label nil)
        (sep nil)
        (sub nil)
        (cb-tmp nil)
        (disp-tmp nil)
        (image-tmp nil)
        (sub-tmp nil))
    (loop for opt in form
          do (cond
               (cb-tmp
                  (setf callback opt)
                  (setf cb-tmp nil)
                  (setf disp nil))
               (disp-tmp
                  (setf disp opt)
                  (setf disp-tmp nil)
                  (setf callback nil))
               (image-tmp
                  (setf image opt)
                  (setf image-tmp nil))
               (sub-tmp
                  (setf sub opt)
                  (setf sub-tmp nil))
               ((and (not (eq opt :item)) (null label))
                  (setf label opt))
               ((eq opt :callback)
                  (setf cb-tmp t))
               ((eq opt :checked)
                  (setf checked t))
               ((eq opt :disabled)
                  (setf disabled t))
               ((eq opt :dispatcher)
                  (setf disp-tmp t))
               ((eq opt :separator)
                  (setf sep t))
               ((eq opt :image)
                  (setf image-tmp t))
               ((eq opt :submenu)
                  (setf sub-tmp t))
               ((eq opt :item))
               (t
                  (error 'gfs:toolkit-error
                         :detail (format nil "invalid menu item option: ~a" opt)))))
    (when sep
      (if (or callback checked disabled disp image sub)
        (error 'gfs:toolkit-error :detail "invalid separator options")))
    (when callback
      (if sub
        (setf disp `(make-instance (define-dispatcher 'gfw:menu      ,callback)))
        (setf disp `(make-instance (define-dispatcher 'gfw:menu-item ,callback)))))
    (when sub
      (if (or checked image (not (listp sub)))
        (error 'gfs:toolkit-error :detail "invalid option for submenu")))
    (cond
      (sep (push `(define-separator ,generator-sym) code))
      (sub (push `(define-submenu ,generator-sym
                                  ,label
                                  ,disp
                                  ,disabled) code)
                  (loop for subform in sub
                        do (setf code (append (process-item-form subform generator-sym) code)))
           (push `(complete-submenu ,generator-sym) code))
      (t (push `(define-item ,generator-sym
                             ,label
                             ,disp
                             ,disabled
                             ,checked
                             ,image) code)))
    code))

;;;
;;; code generation
;;;

(defstruct menu-item-data text image)

(defun generate-menusystem-code (sexp generator-sym)
  (if (null sexp)
      (error 'gfs:toolkit-error :detail "a value for :MENU is required"))
  (let ((code nil))
    (mapcar #'(lambda (var)
                (setf code (append (process-item-form var generator-sym) code)))
            sexp)
    (reverse code)))

(defclass win32-menu-generator (base-menu-generator) ())

(defmethod initialize-instance :after ((gen win32-menu-generator) &key)
  (let ((m (make-instance 'menu :handle         (gfs::create-menu)
                                :image-provider #'menu-item-data-image
                                :text-provider  #'menu-item-data-text)))
    (put-widget (thread-context) m)
    (push m (menu-stack-of gen))))

(defmethod define-item ((gen win32-menu-generator) label dispatcher disabled checked image)
  (append-item (first (menu-stack-of gen))
               (make-menu-item-data :text label :image image)
               dispatcher disabled checked))

(defmethod define-separator ((gen win32-menu-generator))
  (let ((owner (first (menu-stack-of gen))))
    (append-separator owner)))

(defmethod define-submenu ((gen win32-menu-generator) label dispatcher disabled)
  (let ((submenu (make-instance 'menu :handle (gfs::create-popup-menu)
                                :image-provider #'menu-item-data-image
                                :text-provider  #'menu-item-data-text)))
    (append-submenu (first (menu-stack-of gen)) label submenu dispatcher disabled)
    (push submenu (menu-stack-of gen))))

(defmethod complete-submenu ((gen win32-menu-generator))
  (pop (menu-stack-of gen)))

;;;
;;; top-level API for the menu language
;;;

(defmacro defmenu2 (&key name menu)
  (let ((gen (gensym))
        (tmp-name (gensym)))
    `(let ((,tmp-name ,name))
       (if (get-menu-factory (thread-context) ,tmp-name)
           (warn 'gfs:toolkit-warning
                 :detail (format nil "a menu with name ~S already exists" ,tmp-name)))
       (put-menu-factory (thread-context)
                         ,tmp-name
                         (lambda ()
                           (let ((,gen (make-instance 'win32-menu-generator)))
                             ,@(generate-menusystem-code menu gen)
                             (pop (menu-stack-of ,gen))))))))

(defmacro defmenu (sexp)
  `(funcall (defmenu2 :menu ,sexp)))

(defun make-menu (menu-name)
  (if (not (symbolp menu-name))
      (error 'toolkit-error :detail "the menu name must be a symbol"))
  (let ((menu-fn (get-menu-factory (thread-context) menu-name)))
    (unless menu-fn
      (error 'gfs:toolkit-error
             :detail (format nil "~a does not identify any existing menu" menu-name)))
    (funcall menu-fn)))
