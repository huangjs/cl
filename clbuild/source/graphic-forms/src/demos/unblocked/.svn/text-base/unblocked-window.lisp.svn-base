;;;;
;;;; unblocked-window.lisp
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

(defconstant +spacing+           4)
(defconstant +margin+            4)

(defvar *scoreboard-panel*      nil)
(defvar *tiles-panel*           nil)
(defvar *unblocked-win*         nil)

(defun get-unblocked-win ()
  *unblocked-win*)

(defun get-tiles-panel ()
  *tiles-panel*)

(defun get-scoreboard-panel ()
  *scoreboard-panel*)

(defun new-unblocked (disp item)
  (declare (ignore disp item))
  (ctrl-start-game))

(defun restart-unblocked (disp item)
  (declare (ignore disp item))
  (ctrl-restart-game))

(defun update-panel (panel)
  (update-buffer (gfw:dispatcher panel))
  (gfw:redraw panel))

(defun update-status-bar (msg)
  (if *unblocked-win*
    (setf (gfw:text (gfw:status-bar-of *unblocked-win*)) msg)))

(defun reveal-unblocked (disp item)
  (declare (ignore disp item))
  (ctrl-reveal-move))

(defun quit-unblocked (disp item)
  (declare (ignore disp item))
  (setf *scoreboard-panel* nil)
  (setf *tiles-panel* nil)
  (gfs:dispose *unblocked-win*)
  (setf *unblocked-win* nil)
  (gfw:shutdown 0))

(defclass unblocked-win-events (gfw:event-dispatcher) ())

(defmethod gfw:event-close ((disp unblocked-win-events) window)
  (declare (ignore window))
  (quit-unblocked disp nil))

(defmethod gfw:event-timer ((disp unblocked-win-events) timer)
  (declare (ignore timer))
  (update-panel *tiles-panel*))

(defun about-unblocked (disp item)
  (declare (ignore disp item))
  (let* ((*default-pathname-defaults* (parse-namestring gfsys::*unblocked-dir*))
         (image-path (merge-pathnames "about.bmp")))
    (about-demo *unblocked-win* image-path "About UnBlocked" "UnBlocked version 0.9")))

(defun unblocked-startup ()
  (let ((menubar (gfw:defmenu ((:item "&File"
                                :submenu ((:item "&New"             :callback #'new-unblocked)
                                          (:item "&Restart"         :callback #'restart-unblocked)
                                          (:item "Reveal &Move"     :callback #'reveal-unblocked)
                                          (:item ""                 :separator)
                                          (:item "E&xit"            :callback #'quit-unblocked)))
                               (:item "&Help"
                                :submenu ((:item "&About UnBlocked" :callback #'about-unblocked))))))
        (scoreboard-buffer-size (compute-scoreboard-size))
        (tile-buffer-size (gfs:make-size :width (+ (* +horz-tile-count+ +tile-bmp-width+)
                                                   2)
                                         :height (+ (* +vert-tile-count+ +tile-bmp-height+)
                                                    2))))
    (setf *unblocked-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'unblocked-win-events)
                                                        :layout (make-instance 'gfw:flow-layout
                                                                               :style :vertical
                                                                               :spacing +spacing+
                                                                               :margins +margin+)
                                                        :style '(:fixed-size :workspace :status-bar)))
    (setf (gfw:menu-bar *unblocked-win*) menubar)
    (setf *scoreboard-panel* (make-instance 'scoreboard-panel
                                            :parent *unblocked-win*
                                            :style '(:border)
                                            :dispatcher (make-instance 'scoreboard-panel-events
                                                                       :buffer-size scoreboard-buffer-size)))
    (setf *tiles-panel* (make-instance 'tiles-panel
                                       :parent *unblocked-win*
                                       :style '(:border)
                                       :dispatcher (make-instance 'tiles-panel-events
                                                                  :buffer-size tile-buffer-size)))
    (setf (gfw:text *unblocked-win*) "UnBlocked")

    (gfw:pack *unblocked-win*)

    (new-unblocked nil nil)
    (let ((*default-pathname-defaults* (parse-namestring gfsys::*unblocked-dir*)))
      (setf (gfw:image *unblocked-win*)
            (make-instance 'gfg:icon-bundle :file (merge-pathnames "unblocked.ico"))))
    (gfw:show *unblocked-win* t)))

(defun unblocked ()
  (gfw:startup "UnBlocked" #'unblocked-startup))
