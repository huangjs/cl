;;;;
;;;; windlg.lisp
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

(in-package #:graphic-forms.uitoolkit.tests)

(defvar *main-win* nil)

(defclass main-win-events (gfw:event-dispatcher) ())

(defun windlg-exit-fn (disp item)
  (declare (ignore disp item))
  (gfs:dispose *main-win*)
  (setf *main-win* nil)
  (gfw:shutdown 0))

(defmethod gfw:event-close ((self main-win-events) window)
  (declare (ignore window))
  (windlg-exit-fn self nil))

(defclass test-win-events (gfw:event-dispatcher) ())

(defmethod gfw:event-paint ((d test-win-events) window gc rect)
  (declare (ignore rect))
  (setf (gfg:background-color gc) gfg:*color-white*)
  (setf (gfg:foreground-color gc) gfg:*color-white*)
  (gfg:draw-filled-rectangle gc (gfs:make-rectangle :size (gfw:client-size window))))

(defclass test-mini-events (test-win-events) ())

(defmethod gfw:event-close ((d test-mini-events) window)
  (gfs:dispose window))

(defclass test-borderless-events (test-win-events) ())

(defmethod gfw:event-mouse-down ((d test-borderless-events) window point button)
  (declare (ignore point button))
  (gfs:dispose window))

(defun create-borderless-win (disp item)
  (declare (ignore disp item))
  (let ((window (make-instance 'gfw:top-level :dispatcher (make-instance 'test-borderless-events)
                                              :owner *main-win*
                                              :style '(:borderless))))
    (setf (gfw:size window) (gfs:make-size :width 300 :height 250))
    (gfw:center-on-owner window)
    (gfw:show window t)))

(defun create-miniframe-win (disp item)
  (declare (ignore disp item))
  (let ((window (make-instance 'gfw:top-level :dispatcher (make-instance 'test-mini-events)
                                              :owner *main-win*
                                              :text "Mini Frame"
                                              :style '(:miniframe))))
    (setf (gfw:location window) (gfs:make-point :x 250 :y 150))
    (setf (gfw:size window) (gfs:make-size :width 150 :height 225))
    (gfw:show window t)))

(defun create-palette-win (disp item)
  (declare (ignore disp item))
  (let ((window (make-instance 'gfw:top-level :dispatcher (make-instance 'test-mini-events)
                                              :owner *main-win*
                                              :text "Palette"
                                              :style '(:palette))))
    (setf (gfw:location window) (gfs:make-point :x 250 :y 150))
    (setf (gfw:size window) (gfs:make-size :width 150 :height 225))
    (gfw:show window t)))

(defun open-file-dlg (disp item)
  (declare (ignore disp item))
  (gfw:with-file-dialog (*main-win*
                         '(:open :add-to-recent :multiple-select)
                         paths
                         :filters '(("FASL Files (*.fas;*.fsl)"  . "*.fas;*.fsl")
                                    ("Lisp Source Files (*.lisp;*.lsp)" . "*.lisp;*.lsp")
                                    ("All Files (*.*)"   . "*.*"))
                         :initial-directory #P"c:/"
                         :text "Select Lisp-related files...")
    (print paths)))

(defun save-file-dlg (disp item)
  (declare (ignore disp item))
  (gfw:with-file-dialog (*main-win*
                         '(:save)
                         paths
                         :filters '(("Data files (*.dat)"  . "*.dat")
                                    ("All Files (*.*)"     . "*.*"))
                         :initial-directory #P"c:/")
    (print paths)))

(defun choose-color-dlg (disp item)
  (declare (ignore disp item))
  (gfw:with-color-dialog (*main-win* '(:allow-custom-colors) color custom-colors :initial-custom-colors (list gfg:*color-red* gfg:*color-blue*))
    (if color
      (print color))
    (if custom-colors
      (print custom-colors))))

(defun choose-font-dlg (disp item)
  (declare (ignore disp item))
  (gfw:with-graphics-context (gc *main-win*)
    (gfw:with-font-dialog (*main-win* nil font color :gc gc)
      (if color
        (print color))
      (if font
        (print (gfg:data-object font gc))))))

(defclass dialog-events (gfw:event-dispatcher) ())

(defmethod gfw:event-close ((disp dialog-events) (dlg gfw:dialog))
  (call-next-method)
  (let ((ownerp (gfw:owner dlg)))
    (gfs:dispose dlg)
    (unless ownerp
      (gfw:shutdown 0))))

(defclass edit-control-events (gfw:event-dispatcher) ())

(defun truncate-text (str)
  (subseq str 0 (min (length str) 5)))

(defmethod gfw:event-focus-gain ((disp edit-control-events) (ctrl gfw:edit))
  (format t "gained focus: ~a...~%" (truncate-text (gfw:text ctrl))))

(defmethod gfw:event-focus-loss ((disp edit-control-events) (ctrl gfw:edit))
  (format t "lost focus: ~a...~%" (truncate-text (gfw:text ctrl))))

(defmethod gfw:event-modify ((disp edit-control-events) (ctrl gfw:edit))
  (format t "modified: ~a...~%" (truncate-text (gfw:text ctrl))))

(defun open-dlg (title style parent)
  (let* ((dlg (make-instance 'gfw:dialog :owner parent
                                         :dispatcher (make-instance 'dialog-events)
                                         :layout (make-instance 'gfw:flow-layout
                                                                :margins 8
                                                                :spacing 8
                                                                :style '(:horizontal))
                                         :style style
                                         :text title))
         (edit-disp (make-instance 'edit-control-events))
         (left-panel (make-instance 'gfw:panel
                                    :layout (make-instance 'gfw:flow-layout
                                                           :spacing 4
                                                           :style '(:vertical))
                                    :parent dlg))
         (name-label (make-instance 'gfw:label
                                    :text "Name:"
                                    :parent left-panel))
         (name-edit (make-instance 'gfw:edit
                                   :text "WWWWWWWWWWWWWWWWWWWWWWWW"
                                   :dispatcher edit-disp
                                   :parent left-panel))
         (serial-label (make-instance 'gfw:label
                                      :text "Serial Number:"
                                      :parent left-panel))
         (serial-edit (make-instance 'gfw:edit
                                     :style '(:read-only)
                                     :text "323K DSKL3 DSKE23"
                                     :dispatcher edit-disp
                                     :parent left-panel))
         (pw-label (make-instance 'gfw:label
                                  :text "Password:"
                                  :parent left-panel))
         (pw-edit (make-instance 'gfw:edit
                                 :style '(:mask-characters)
                                 :text "WWWWWWWWWWWWWWWWWWWWWWWW"
                                 :dispatcher edit-disp
                                 :parent left-panel))
         (desc-label (make-instance 'gfw:label
                                    :text "Description:"
                                    :parent left-panel))
         (desc-edit (make-instance 'gfw:edit
                                   :style '(:multi-line :auto-hscroll :auto-vscroll :vertical-scrollbar :want-return)
                                   :text (format nil "WWWWWWWWWWWWWWWWWWWWWWWW~%W~%W~%W~%W~%W")
                                   :dispatcher edit-disp
                                   :parent left-panel))
         (btn-panel (make-instance 'gfw:panel
                                   :layout (make-instance 'gfw:flow-layout
                                                          :spacing 4
                                                          :style '(:vertical :normalize))
                                   :parent dlg))
         (ok-btn (make-instance 'gfw:button
                                :callback (lambda (disp btn)
                                            (declare (ignore disp btn))
                                            (let ((ownerp (gfw:owner dlg)))
                                              (gfs:dispose dlg)
                                              (unless ownerp
                                                (gfw:shutdown 0))))
                                :style '(:default-button)
                                :text "OK"
                                :parent btn-panel))
         (cancel-btn (make-instance 'gfw:button
                                    :callback (lambda (disp btn)
                                                (declare (ignore disp btn))
                                                (let ((ownerp (gfw:owner dlg)))
                                                  (gfs:dispose dlg)
                                                  (unless ownerp
                                                    (gfw:shutdown 0))))
                                    :style '(:cancel-button)
                                    :text "Cancel"
                                    :parent btn-panel)))
    (declare (ignore name-label serial-label serial-edit pw-label desc-label ok-btn cancel-btn))
    (gfw:pack dlg)
    (setf (gfw:text name-edit) ""
          (gfw:text pw-edit) ""
          (gfw:text desc-edit) "")
    (if parent
        (gfw:center-on-owner dlg))
    (gfw:show dlg t)
    dlg))

(defun open-modal-dlg (disp item)
  (declare (ignore disp item))
  (open-dlg "Modal" '(:owner-modal) *main-win*))

(defun open-modeless-dlg (disp item)
  (declare (ignore disp item))
  (open-dlg "Modeless" '(:modeless) *main-win*))

(defun windlg-internal ()
  (let ((menubar nil))
    (setf *main-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'main-win-events)
                                                   :style '(:workspace)))
    (setf menubar (gfw:defmenu ((:item "&File"
                                 :submenu ((:item "E&xit" :callback #'windlg-exit-fn)))
                                           (:item "&Custom Dialogs"
                                            :submenu ((:item "&Modal"        :callback #'open-modal-dlg)
                                                      (:item "&Modeless"     :callback #'open-modeless-dlg)))
                                           (:item "&System Dialogs"
                                            :submenu ((:item "Choose &Color" :callback #'choose-color-dlg)
                                                      (:item "Choose &Font"  :callback #'choose-font-dlg)
                                                      (:item "&Open File"    :callback #'open-file-dlg)
                                                      (:item "&Save File"    :callback #'save-file-dlg)))
                                           (:item "&Windows"
                                            :submenu ((:item "&Borderless"   :callback #'create-borderless-win)
                                                      (:item "&Mini Frame"   :callback #'create-miniframe-win)
                                                      (:item "&Palette"      :callback #'create-palette-win))))))
    (setf (gfw:menu-bar *main-win*) menubar)
    (setf (gfw:image *main-win*) (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico")))
    (gfw:show *main-win* t)))

(defun windlg ()
  (gfw:startup "Window/Dialog Tester" #'windlg-internal))

(defun standalone-dialog-internal ()
  (open-dlg "Standalone Dialog" '(:modeless) nil))

(defun standalone-dialog ()
  (gfw:startup "Standalone Dialog Test" #'standalone-dialog-internal))
