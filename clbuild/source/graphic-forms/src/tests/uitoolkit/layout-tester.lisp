;;;;
;;;; layout-tester.lisp
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

(in-package #:graphic-forms.uitoolkit.tests)

(defparameter *btn-text-before* "Push Me")
(defparameter *btn-text-after*  "Again!")
(defparameter *edit-text*       "something to edit")
(defparameter *label-text*      "Label")

(defconstant +margin-delta+     4)
(defconstant +spacing-delta+    3)

(defvar *widget-counter* 0)

(defparameter *layout-tester-win* nil)

(defun exit-layout-tester ()
  (let ((w *layout-tester-win*))
    (setf *layout-tester-win* nil)
    (gfs:dispose w))
  (gfw:shutdown 0))

(defclass layout-tester-events (gfw:event-dispatcher) ())

(defmethod gfw:event-close ((d layout-tester-events) widget)
  (declare (ignore widget))
  (exit-layout-tester))

(defclass pack-layout-dispatcher (gfw:event-dispatcher) ())

(defmethod gfw:event-select ((d pack-layout-dispatcher) item)
  (declare (ignore item))
  (gfw:pack *layout-tester-win*))

(defclass layout-tester-widget-events (gfw:event-dispatcher)
  ((toggle-fn
    :accessor toggle-fn
    :initform nil)
   (id
    :accessor id
    :initarg :id
    :initform 0)))

(defmethod gfw:event-paint ((self layout-tester-widget-events) window gc rect)
  (declare (ignore window rect))
  (gfg:clear gc gfg:*color-white*))

(defclass test-panel (gfw:panel) ())

(defmethod gfw:preferred-size ((win test-panel) width-hint height-hint)
  (declare (ignore width-hint height-hint))
  (gfs:make-size :width 45 :height 45))

(defmethod gfw:text ((win test-panel))
  (declare (ignore win))
  "Test Panel")

(defun create-button-toggler (be)
  (let ((flag nil))
    (lambda ()
      (if (null flag)
        (progn
          (setf flag t)
          (format nil "~d ~a" (id be) *btn-text-before*))
        (progn
          (setf flag nil)
          (format nil "~d ~a" (id be) *btn-text-after*))))))

(defun add-layout-tester-widget (widget-class subtype)
  (let ((be (make-instance 'layout-tester-widget-events :id *widget-counter*))
        (w nil))
    (cond
      ((or (eql subtype :check-box)
           (eql subtype :push-button)
           (eql subtype :radio-button)
           (eql subtype :toggle-button)
           (eql subtype :tri-state))
         (setf w (make-instance widget-class
                                :parent *layout-tester-win*
                                :dispatcher be
                                :style (list subtype)))
         (setf (toggle-fn be) (create-button-toggler be))
         (setf (gfw:text w) (funcall (toggle-fn be)))
         (if (eql subtype :tri-state)
           (gfw:check w t)
           (gfw:check w t)))
      ((eql subtype :single-line-edit)
         (setf w (make-instance widget-class
                                :parent *layout-tester-win*
                                :text (format nil "~d ~a" (id be) *edit-text*))))
      ((eql subtype :image-label)
         ;; NOTE: we are leaking a bitmap handle by not tracking the
         ;; image being created here
         (setf w (make-instance widget-class
                                :parent *layout-tester-win*
                                :dispatcher be))
         (setf (gfg:background-color w) (gfg:background-color *layout-tester-win*))
         (let ((tmp-image (make-instance 'gfg:image :file "happy.bmp")))
           (gfg:with-image-transparency (tmp-image (gfs:make-point))
             (setf (gfw:image w) tmp-image))))
      ((eql subtype :text-label)
         (setf w (make-instance widget-class
                                :parent *layout-tester-win*
                                :dispatcher be
                                :style '(:sunken)))
         (setf (gfw:text w) (format nil "~d ~a" (id be) *label-text*)))
      (t
         (setf w (make-instance widget-class
                                :parent *layout-tester-win*
                                :dispatcher be))))
    (incf *widget-counter*)))

(defmethod gfw:event-select ((d layout-tester-widget-events) btn)
  (setf (gfw:text btn) (funcall (toggle-fn d)))
  (gfw:layout *layout-tester-win*))

(defclass add-child-dispatcher (gfw:event-dispatcher)
  ((widget-class
    :accessor widget-class
    :initarg :widget-class
    :initform 'gfw:button)
   (subtype
    :accessor subtype
    :initarg :subtype
    :initform :push-button)))

(defmethod gfw:event-select ((d add-child-dispatcher) item)
  (declare (ignore item))
  (add-layout-tester-widget (widget-class d) (subtype d))
  (gfw:pack *layout-tester-win*))

(defclass child-menu-dispatcher (gfw:event-dispatcher)
  ((check-test-fn
    :accessor check-test-fn
    :initarg :check-test-fn
    :initform nil)
   (sub-disp-class
    :accessor sub-disp-class-of
    :initarg :sub-disp-class
    :initform nil)))

(defmethod gfw:event-activate ((d child-menu-dispatcher) menu)
  (gfw:delete-all menu)
  (gfw:mapchildren *layout-tester-win*
                   (lambda (parent child)
                     (declare (ignore parent))
                     (let ((it (gfw::append-item menu (gfw:text child) nil)))
                       (unless (null (sub-disp-class-of d))
                         (setf (gfw:dispatcher it) (make-instance (sub-disp-class-of d))))
                       (unless (null (check-test-fn d))
                         (gfw:check it (funcall (check-test-fn d) child)))))))

(defun find-victim (text)
  (let ((victim nil))
    (gfw:mapchildren *layout-tester-win*
                     (lambda (parent child)
                       (declare (ignore parent))
                       (if (string= (gfw:text child) text)
                         (setf victim child))))
    victim))

(defclass remove-child-dispatcher (gfw:event-dispatcher) ())  

(defmethod gfw:event-select ((d remove-child-dispatcher) item)
  (let ((victim (find-victim (gfw:text item))))
    (unless (null victim)
      (gfs:dispose victim)
      (gfw:layout *layout-tester-win*))))

(defclass visibility-child-dispatcher (gfw:event-dispatcher) ())  

(defmethod gfw:event-select ((d visibility-child-dispatcher) item)
  (let ((victim (find-victim (gfw:text item))))
    (unless (null victim)
      (gfw:show victim (not (gfw:visible-p victim)))
      (gfw:layout *layout-tester-win*))))

(defun check-flow-orient-items (disp menu)
  (declare (ignore disp))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (gfw:check (elt (gfw:items-of menu) 0) (find :horizontal (gfw:style-of layout)))
    (gfw:check (elt (gfw:items-of menu) 1) (find :vertical (gfw:style-of layout)))))

(defun set-flow-horizontal (disp item)
  (declare (ignorable disp item))
  (let* ((layout (gfw:layout-of *layout-tester-win*))
         (style (gfw:style-of layout)))
    (setf style (remove :vertical style))
    (push :horizontal style)
    (setf (gfw:style-of layout) style)
    (gfw:layout *layout-tester-win*)))

(defun set-flow-vertical (disp item)
  (declare (ignorable disp item))
  (let* ((layout (gfw:layout-of *layout-tester-win*))
         (style (gfw:style-of layout)))
    (setf style (remove :horizontal style))
    (push :vertical style)
    (setf (gfw:style-of layout) style)
    (gfw:layout *layout-tester-win*)))

(defun set-flow-layout-normalize (disp item)
  (declare (ignorable disp item))
  (let* ((layout (gfw:layout-of *layout-tester-win*))
         (style (gfw:style-of layout)))
    (if (find :normalize style)
      (setf (gfw:style-of layout) (remove :normalize style))
      (setf (gfw:style-of layout) (push :normalize style)))
    (gfw:layout *layout-tester-win*)))

(defun set-flow-layout-wrap (disp item)
  (declare (ignorable disp item))
  (let* ((layout (gfw:layout-of *layout-tester-win*))
         (style (gfw:style-of layout)))
    (if (find :wrap style)
      (setf (gfw:style-of layout) (remove :wrap style))
      (setf (gfw:style-of layout) (push :wrap style)))
    (gfw:layout *layout-tester-win*)))

(defun enable-flow-spacing-items (disp menu)
  (declare (ignore disp))
  (let ((spacing (gfw:spacing-of (gfw:layout-of *layout-tester-win*))))
    (gfw:enable (elt (gfw:items-of menu) 0) (> spacing 0))))

(defun decrease-flow-spacing (disp item)
  (declare (ignore disp item))
  (let* ((layout (gfw:layout-of *layout-tester-win*))
         (spacing (gfw:spacing-of layout)))
    (unless (zerop spacing)
      (decf spacing +spacing-delta+)
      (setf (gfw:spacing-of layout) spacing)
      (gfw:layout *layout-tester-win*))))

(defun increase-flow-spacing (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (incf (gfw:spacing-of layout) +spacing-delta+)
    (gfw:layout *layout-tester-win*)))

(defun enable-left-flow-margin-items (disp menu)
  (declare (ignore disp))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (gfw:enable (elt (gfw:items-of menu) 0) (> (gfw:left-margin-of layout) 0))))

(defun enable-top-flow-margin-items (disp menu)
  (declare (ignore disp))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (gfw:enable (elt (gfw:items-of menu) 0) (> (gfw:top-margin-of layout) 0))))

(defun enable-right-flow-margin-items (disp menu)
  (declare (ignore disp))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (gfw:enable (elt (gfw:items-of menu) 0) (> (gfw:right-margin-of layout) 0))))

(defun enable-bottom-flow-margin-items (disp menu)
  (declare (ignore disp))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (gfw:enable (elt (gfw:items-of menu) 0) (> (gfw:bottom-margin-of layout) 0))))

(defun inc-left-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (incf (gfw:left-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun inc-top-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (incf (gfw:top-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun inc-right-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (incf (gfw:right-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun inc-bottom-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (incf (gfw:bottom-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun dec-left-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (decf (gfw:left-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun dec-top-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (decf (gfw:top-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun dec-right-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (decf (gfw:right-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun dec-bottom-flow-margin (disp item)
  (declare (ignore disp item))
  (let ((layout (gfw:layout-of *layout-tester-win*)))
    (decf (gfw:bottom-margin-of layout) +margin-delta+)
    (gfw:layout *layout-tester-win*)))

(defun flow-mod-callback (disp menu)
  (declare (ignore disp))
  (gfw:delete-all menu)
  (let ((it nil)
        (margin-menu (gfw:defmenu ((:item "Left"
                                    :callback #'enable-left-flow-margin-items
                                    :submenu ((:item "Decrease"
                                               :callback #'dec-left-flow-margin)
                                              (:item "Increase"
                                               :callback #'inc-left-flow-margin)))
                                   (:item "Top"
                                    :callback #'enable-top-flow-margin-items
                                    :submenu ((:item "Decrease"
                                               :callback #'dec-top-flow-margin)
                                              (:item "Increase"
                                               :callback #'inc-top-flow-margin)))
                                   (:item "Right"
                                    :callback #'enable-right-flow-margin-items
                                    :submenu ((:item "Decrease"
                                               :callback #'dec-right-flow-margin)
                                              (:item "Increase"
                                               :callback #'inc-right-flow-margin)))
                                   (:item "Bottom"
                                    :callback #'enable-bottom-flow-margin-items
                                    :submenu ((:item "Decrease"
                                               :callback #'dec-bottom-flow-margin)
                                              (:item "Increase"
                                               :callback #'inc-bottom-flow-margin))))))
        (orient-menu (gfw:defmenu ((:item "Horizontal"
                                    :callback #'set-flow-horizontal)
                                   (:item "Vertical"
                                    :callback #'set-flow-vertical))))
        (spacing-menu (gfw:defmenu ((:item "Decrease"
                                     :callback #'decrease-flow-spacing)
                                    (:item "Increase"
                                     :callback #'increase-flow-spacing)))))
    (gfw:append-submenu menu "Margin" margin-menu nil)
    (gfw:append-submenu menu "Orientation" orient-menu #'check-flow-orient-items)
    (gfw:append-submenu menu "Spacing" spacing-menu #'enable-flow-spacing-items)
    (let ((style (gfw:style-of (gfw:layout-of *layout-tester-win*))))
      (setf it (gfw:append-item menu "Normalize" #'set-flow-layout-normalize))
      (gfw:check it (find :normalize style))
      (setf it (gfw:append-item menu "Wrap" #'set-flow-layout-wrap))
      (gfw:check it (find :wrap style)))))

(defun exit-layout-callback (disp item)
  (declare (ignorable disp item))
  (exit-layout-tester))

(defun layout-tester-internal ()
  (setf *default-pathname-defaults* (parse-namestring gfsys::*gf-tests-dir*))
  (setf *widget-counter* 0)
  (let ((menubar nil)
        (pack-disp (make-instance 'pack-layout-dispatcher))
        (add-btn-disp (make-instance 'add-child-dispatcher))
        (add-checkbox-disp (make-instance 'add-child-dispatcher :subtype :check-box))
        (add-edit-disp (make-instance 'add-child-dispatcher :widget-class 'gfw:edit
                                                            :subtype :single-line-edit))
        (add-radio-disp (make-instance 'add-child-dispatcher :subtype :radio-button))
        (add-toggle-disp (make-instance 'add-child-dispatcher :subtype :toggle-button))
        (add-tri-state-disp (make-instance 'add-child-dispatcher :subtype :tri-state))
        (add-panel-disp (make-instance 'add-child-dispatcher :widget-class 'test-panel
                                                             :subtype :panel))
        (add-image-label-disp (make-instance 'add-child-dispatcher :widget-class 'gfw:label
                                                                   :subtype :image-label))
        (add-text-label-disp (make-instance 'add-child-dispatcher :widget-class 'gfw:label
                                                                  :subtype :text-label))
        (rem-menu-disp (make-instance 'child-menu-dispatcher :sub-disp-class 'remove-child-dispatcher))
        (vis-menu-disp (make-instance 'child-menu-dispatcher :sub-disp-class 'visibility-child-dispatcher
                                                             :check-test-fn #'gfw:visible-p)))
    (setf *layout-tester-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'layout-tester-events)
                                                            :style '(:workspace)
                                                            :layout (make-instance 'gfw:flow-layout
                                                                                   :spacing +spacing-delta+
                                                                                   :margins +margin-delta+)))
    (setf menubar (gfw:defmenu ((:item "&File"
                                 :submenu ((:item "E&xit"
                                            :callback #'exit-layout-callback)))
                                (:item "&Children"
                                 :submenu ((:item "Add"
                                            :submenu ((:item "Button"        :dispatcher add-btn-disp)
                                                      (:item "Checkbox"      :dispatcher add-checkbox-disp)
                                                      (:item "Edit"          :dispatcher add-edit-disp)
                                                      (:item "Label - Image" :dispatcher add-image-label-disp)
                                                      (:item "Label - Text"  :dispatcher add-text-label-disp)
                                                      (:item "Panel"         :dispatcher add-panel-disp)
                                                      (:item "Radiobutton"   :dispatcher add-radio-disp)
                                                      (:item "Toggle"        :dispatcher add-toggle-disp)
                                                      (:item "Tri-State"     :dispatcher add-tri-state-disp)))
                                           (:item "Remove" :dispatcher rem-menu-disp
                                            :submenu ((:item "")))
                                           (:item "Visible" :dispatcher vis-menu-disp
                                            :submenu ((:item "")))))
                                (:item "&Window"
                                 :submenu ((:item "Modify Layout" :callback #'flow-mod-callback
                                            :submenu ((:item "")))
                                           (:item "Select Layout"
                                            :submenu ((:item "Flow")))
                                           (:item "Pack" :dispatcher pack-disp))))))
    (setf (gfw:menu-bar *layout-tester-win*) menubar)
    (dotimes (i 3)
      (add-layout-tester-widget 'gfw:button :push-button))
    (setf (gfw:text *layout-tester-win*) "Layout Tester")
    (setf (gfw:image *layout-tester-win*) (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico")))
    (gfw:pack *layout-tester-win*)
    (gfw:show *layout-tester-win* t)))

(defun layout-tester ()
  (gfw:startup "Layout Tester" #'layout-tester-internal))
