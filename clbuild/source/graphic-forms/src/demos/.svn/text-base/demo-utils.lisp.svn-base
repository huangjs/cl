;;;;
;;;; demo-utils.lisp
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

(defclass demo-about-dialog-events (gfw:event-dispatcher) ())

(defmethod gfw:event-close ((disp demo-about-dialog-events) (dlg gfw:dialog))
  (call-next-method)
  (gfs:dispose dlg))

(defun about-demo (owner image-path title desc)
  (let* ((image (make-instance 'gfg:image :file image-path))
         (dlg (make-instance 'gfw:dialog :owner owner
                                         :dispatcher (make-instance 'demo-about-dialog-events)
                                         :layout (make-instance 'gfw:flow-layout
                                                                :margins 8
                                                                :spacing 8)
                                         :style '(:owner-modal)
                                         :text title))
         (label (make-instance 'gfw:label :parent dlg))
         (text-panel (make-instance 'gfw:panel
                                    :layout (make-instance 'gfw:flow-layout
                                                           :margins 0
                                                           :spacing 2
                                                           :style '(:vertical))
                                    :parent dlg))
         (line1 (make-instance 'gfw:label
                               :parent text-panel
                               :text desc))
         (line2 (make-instance 'gfw:label
                               :parent text-panel
                               :text " "))
         (line3 (make-instance 'gfw:label
                               :parent text-panel
                               :text (format nil "Copyright ~c 2006-2007 by Jack D. Unrue" (code-char 169))))
         (line4 (make-instance 'gfw:label
                               :parent text-panel
                               :text "All Rights Reserved."))
         (line5 (make-instance 'gfw:label
                               :parent text-panel
                               :text " "))
         (line6 (make-instance 'gfw:label
                               :parent text-panel
                               :text " "))
         (btn-panel (make-instance 'gfw:panel
                                   :parent dlg
                                   :layout (make-instance 'gfw:flow-layout
                                                          :margins 0
                                                          :spacing 0
                                                          :style '(:vertical :normalize))))
         (close-btn (make-instance 'gfw:button
                                   :callback (lambda (disp btn)
                                               (declare (ignore disp btn))
                                               (gfs:dispose dlg))
                                   :style '(:default-button)
                                   :text "Close"
                                   :parent btn-panel)))
    (declare (ignore line1 line2 line3 line4 line5 line6 close-btn))
    (unwind-protect
        (gfg:with-image-transparency (image (gfs:make-point))
          (setf (gfw:image label) image))
      (gfs:dispose image))
    (gfw:pack dlg)
    (gfw:center-on-owner dlg)
    (gfw:show dlg t)))
