;;;;
;;;; scroll-tester.lisp
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

(defvar *scroll-tester-win* nil)

(defun scroll-tester-exit (disp item)
  (declare (ignore disp item))
  (gfs:dispose *scroll-tester-win*)
  (setf *scroll-tester-win* nil)
  (gfw:shutdown 0))

(defclass scroll-tester-events (gfw:scrolling-helper) ())

(defmethod gfw:event-close ((disp scroll-tester-events) window)
  (declare (ignore window))
  (scroll-tester-exit disp nil))

(defun scroll-tester-internal ()
  (setf *default-pathname-defaults* (parse-namestring gfsys::*gf-tests-dir*))
  (let ((layout (make-instance 'gfw:heap-layout))
        (icons (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico"))))
    (setf *scroll-tester-win* (make-instance 'gfw:top-level
                                             :dispatcher (make-instance 'scroll-tester-events)
                                             :layout layout
                                             :style '(:workspace :horizontal-scrollbar :vertical-scrollbar)))
    (setf (gfw:image *scroll-tester-win*) icons)
    (let* ((grid-panel (make-scroll-grid-panel *scroll-tester-win*))
           (text-panel (make-scroll-text-panel *scroll-tester-win*))
           (select-grid (lambda (disp item)
                          (declare (ignore disp item))
                          (setf (gfw:top-child-of layout) grid-panel)
                          (gfw:layout *scroll-tester-win*)
                          (set-grid-scroll-params *scroll-tester-win*)))
           (select-text (lambda (disp item)
                          (declare (ignore disp item))
                          (setf (gfw:top-child-of layout) text-panel)
                          (gfw:layout *scroll-tester-win*)
                          (set-text-scroll-params *scroll-tester-win*)))
           (manage-tests-menu (lambda (disp menu)
                                (declare (ignore disp))
                                (let ((top (gfw::obtain-top-child *scroll-tester-win*))
                                      (items (gfw:items-of menu)))
                                  (gfw:check (elt items 0) (eql top grid-panel))
                                  (gfw:check (elt items 1) (eql top text-panel)))))
           (menubar (gfw:defmenu ((:item    "&File"
                                   :submenu ((:item "E&xit"        :callback #'scroll-tester-exit)))
                                  (:item    "&Tests"               :callback manage-tests-menu
                                   :submenu ((:item "&Simple Grid" :callback select-grid)
                                             (:item "&Text"        :callback select-text)))))))
      (setf (gfw:menu-bar *scroll-tester-win*) menubar
            (gfw:top-child-of layout) grid-panel))
    (setf (gfw:text *scroll-tester-win*) "Scroll Tester"
          (gfw:size *scroll-tester-win*) (gfs:make-size :width 300 :height 275))
    (set-grid-scroll-params *scroll-tester-win*)
    (gfw:show *scroll-tester-win* t)))

(defun scroll-tester ()
  (gfw:startup "Scroll Tester" #'scroll-tester-internal))
