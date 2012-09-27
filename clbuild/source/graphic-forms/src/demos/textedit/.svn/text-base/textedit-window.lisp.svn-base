;;;;
;;;; textedit-window.lisp
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

(in-package :graphic-forms.uitoolkit.tests)

(defvar *textedit-control*      nil)
(defvar *textedit-win*          nil)

(defvar *textedit-file-filters* '(("Text Files (*.txt)" . "*.txt")
                                  ("All Files (*.*)"    . "*.*")))

(defvar *textedit-new-title*    "new file - TextEdit")


(defun manage-textedit-file-menu (disp menu)
  (declare (ignore disp))
  (gfw:enable (elt (gfw:items-of menu) 2) (gfw:text-modified-p *textedit-control*))
  (gfw:enable (elt (gfw:items-of menu) 3) (> (length (gfw:text *textedit-control*)) 0)))

(defun textedit-file-new (disp item)
  (declare (ignore disp item))
  (when *textedit-control*
    (setf (gfw:text *textedit-control*) "")
    (setf (gfw:text-modified-p *textedit-control*) nil)
    (setf (file-path-of *textedit-model*) nil)
    (setf (gfw:text *textedit-win*) *textedit-new-title*)))

(defun textedit-file-open (disp item)
  (declare (ignore disp item))
  (gfw:with-file-dialog (*textedit-win*
                         '(:open :add-to-recent :path-must-exist)
                         paths
                         :filters *textedit-file-filters*)
    (when paths
      (gfw:with-wait-cursor (*textedit-win*)
        (setf (gfw:text *textedit-control*) (load-textedit-doc (first paths))))
      (setf (file-path-of *textedit-model*) (namestring (first paths)))
      (setf (gfw:text *textedit-win*) (format nil "~a - TextEdit" (first paths))))))

(defun textedit-file-save (disp item)
  (if (file-path-of *textedit-model*)
    (gfw:with-wait-cursor (*textedit-win*)
      (save-textedit-doc (file-path-of *textedit-model*) (gfw:text *textedit-control*)))
    (textedit-file-save-as disp item))
  (if (file-path-of *textedit-model*)
    (setf (gfw:text-modified-p *textedit-control*) nil)))

(defun textedit-file-save-as (disp item)
  (declare (ignore disp item))
  (gfw:with-file-dialog (*textedit-win*
                         '(:save :add-to-recent)
                         paths
                         :filters *textedit-file-filters*
                         :text "Save As")
    (when paths
      (save-textedit-doc (first paths) (gfw:text *textedit-control*))
      (setf (file-path-of *textedit-model*) (namestring (first paths))
            (gfw:text *textedit-win*) (format nil "~a - TextEdit" (first paths))
            (gfw:text-modified-p *textedit-control*) nil))))

(defun textedit-file-quit (disp item)
  (declare (ignore disp item))
  (setf *textedit-control* nil)
  (gfs:dispose *textedit-win*)
  (setf *textedit-win* nil)
  (gfw:shutdown 0))

(defun manage-textedit-edit-menu (disp menu)
  (declare (ignore disp))
  (unless *textedit-control*
    (return-from manage-textedit-edit-menu nil))
  (let ((items (gfw:items-of menu))
        (text (gfw:text *textedit-control*)))
    (multiple-value-bind (sub-text text-sel)
        (gfw:selected-span *textedit-control*)
      (declare (ignore sub-text))
      (gfw:enable (elt items 0) (gfw:undo-available-p *textedit-control*))
      (gfw:enable (elt items 2) text-sel)
      (gfw:enable (elt items 3) text-sel)
      (gfw:enable (elt items 4) (gfw:text-for-pasting-p *textedit-control*))
      (gfw:enable (elt items 5) text-sel)
      (gfw:enable (elt items 12) (and (> (length text) 0)
                                      (or (null text-sel)
                                          (> (gfs:span-start text-sel) 0)
                                          (< (gfs:span-end text-sel) (length text))))))))

(defun textedit-edit-copy (disp item)
  (declare (ignore disp item))
  (gfw:copy-text *textedit-control*))

(defun textedit-edit-cut (disp item)
  (declare (ignore disp item))
  (gfw:cut-text *textedit-control*))

(defun textedit-edit-delete (disp item)
  (declare (ignore disp item))
  (gfw:delete-selection *textedit-control*))

(defun textedit-edit-paste (disp item)
  (declare (ignore disp item))
  (gfw:paste-text *textedit-control*))

(defun textedit-edit-selall (disp item)
  (declare (ignore disp item))
  (gfw:select-all *textedit-control* t))

(defun textedit-edit-undo (disp item)
  (declare (ignore disp item)))

(defun textedit-font (disp item)
  (declare (ignore disp item))
  (gfw:with-graphics-context (gc *textedit-control*)
    (gfw:with-font-dialog (*textedit-win* '(:no-effects) font color :gc gc :initial-font (gfg:font *textedit-control*))
      (if font
        (setf (gfg:font *textedit-control*) font)))))

(defclass textedit-win-events (gfw:event-dispatcher) ())

(defmethod gfw:event-activate ((self textedit-win-events) window)
  (declare (ignore window))
  (when *textedit-control*
    (gfw:give-focus *textedit-control*)))

(defmethod gfw:event-close ((disp textedit-win-events) window)
  (declare (ignore window))
  (textedit-file-quit disp nil))

(defun about-textedit (disp item)
  (declare (ignore disp item))
  (let* ((*default-pathname-defaults* (parse-namestring gfsys::*textedit-dir*))
         (image-path (merge-pathnames "about.bmp")))
    (about-demo *textedit-win* image-path "About TextEdit" "TextEdit version 0.9")))

(defun textedit-startup ()
  (let ((menubar (gfw:defmenu ((:item "&File"                      :callback #'manage-textedit-file-menu
                                :submenu ((:item "&New"            :callback #'textedit-file-new)
                                          (:item "&Open..."        :callback #'textedit-file-open)
                                          (:item "&Save"           :callback #'textedit-file-save   :disabled)
                                          (:item "Save &As..."     :callback #'textedit-file-save-as)
                                          (:item ""                :separator)
                                          (:item "E&xit"           :callback #'textedit-file-quit)))
                               (:item "&Edit"                      :callback #'manage-textedit-edit-menu
                                :submenu ((:item "&Undo"           :callback #'textedit-edit-undo   :disabled)
                                          (:item "" :separator)
                                          (:item "Cu&t"            :callback #'textedit-edit-cut    :disabled)
                                          (:item "&Copy"           :callback #'textedit-edit-copy   :disabled)
                                          (:item "&Paste"          :callback #'textedit-edit-paste  :disabled)
                                          (:item "De&lete"         :callback #'textedit-edit-delete :disabled)
                                          (:item "" :separator)
                                          (:item "&Find...")
                                          (:item "Find &Next"                                       :disabled)
                                          (:item "&Replace..."                                      :disabled)
                                          (:item "&Go To...")
                                          (:item "" :separator)
                                          (:item "Select &All"     :callback #'textedit-edit-selall)))
                               (:item "F&ormat"
                                :submenu ((:item "&Font..."        :callback #'textedit-font)))
                               (:item "&Help"
                                :submenu ((:item "&About TextEdit" :callback #'about-textedit)))))))
    (setf *textedit-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'textedit-win-events)
                                                       :layout (make-instance 'gfw:heap-layout)
                                                       :style '(:frame :status-bar)))
    (setf *textedit-control* (make-instance 'gfw:edit :parent *textedit-win*
                                                      :style '(:multi-line
                                                               :auto-vscroll
                                                               :vertical-scrollbar
                                                               :want-return)))
    (setf (gfw:menu-bar *textedit-win*) menubar
          (gfw:size *textedit-win*) (gfs:make-size :width 500 :height 500)
          (gfw:text *textedit-win*) *textedit-new-title*)
    (let ((*default-pathname-defaults* (parse-namestring gfsys::*textedit-dir*)))
     (setf (gfw:image *textedit-win*) (make-instance 'gfg:icon-bundle :file (merge-pathnames "textedit.ico"))))
    (gfw:show *textedit-win* t)))

(defun textedit ()
  (gfw:startup "TextEdit" #'textedit-startup))
