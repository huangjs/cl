;;;;
;;;; drawing-tester.lisp
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

(defvar *drawing-dispatcher* nil)
(defvar *drawing-win* nil)
(defvar *last-checked-drawing-item* nil)

(defun update-drawing-item-check (item)
  (unless (null *last-checked-drawing-item*)
    (gfw:check *last-checked-drawing-item* nil))
  (gfw:check item t))

(defun find-checked-item (disp menu)
  (declare (ignore disp))
  (dotimes (i (length (gfw:items-of menu)))
    (let ((item (elt (gfw:items-of menu) i)))
      (when (gfw:checked-p item)
        (setf *last-checked-drawing-item* item)
        (return)))))

(defun drawing-exit-fn (disp item)
  (declare (ignore disp item))
  (gfs:dispose *drawing-win*)
  (setf *drawing-win* nil)
  (gfw:shutdown 0))

(defclass drawing-win-events (gfw:event-dispatcher)
  ((draw-func
    :accessor draw-func-of
    :initform nil)))

(defmethod gfw:event-close ((self drawing-win-events) window)
  (declare (ignore window))
  (drawing-exit-fn self nil))

(defmethod gfw:event-paint ((self drawing-win-events) window gc rect)
  (declare (ignore window rect))
  (gfg:clear gc gfg:*color-white*)
  (let ((func (draw-func-of self)))
    (unless (null func)
      (funcall func gc))))

(defun clone-point (orig)
  (gfs:make-point :x (gfs:point-x orig) :y (gfs:point-y orig)))

(defun clone-size (orig)
  (gfs:make-size :width (gfs:size-width orig) :height (gfs:size-height orig)))

(defun set-gc-params (gc column filled)
  (ecase column
    (0
      (setf (gfg:foreground-color gc) gfg:*color-blue*)
      (setf (gfg:background-color gc) gfg:*color-green*)
      (if filled
        (progn
          (setf (gfg:pen-width gc) 5)
          (setf (gfg:pen-style gc) '(:dashdotdot :bevel-join)))
        (progn
          (setf (gfg:pen-width gc) 5)
          (setf (gfg:pen-style gc) '(:dot :round-join :flat-endcap)))))
    (1
      (setf (gfg:pen-width gc) 3)
      (if filled
        (setf (gfg:pen-style gc) '(:solid))
        (setf (gfg:pen-style gc) '(:dot))))
    (2
      (setf (gfg:pen-width gc) 1)
      (setf (gfg:pen-style gc) '(:solid)))
    (3
      (setf (gfg:foreground-color gc) (gfg:background-color gc)))))

(defun draw-rectangular (gc rect arc-size delta-x draw-fn filled)
  (dotimes (i 4)
    (set-gc-params gc i filled)
    (if arc-size
      (funcall draw-fn gc rect arc-size)
      (funcall draw-fn gc rect))
    (incf (gfs:point-x (gfs:location rect)) delta-x)))

(defun draw-start-end (gc start-pnt end-pnt delta-x draw-fn filled)
  (dotimes (i 4)
    (set-gc-params gc i filled)
    (funcall draw-fn gc start-pnt end-pnt)
    (loop for pnt in (list start-pnt end-pnt) do (incf (gfs:point-x pnt) delta-x))))

(defun draw-rect-start-end (gc rect start-pnt end-pnt delta-x draw-fn filled)
  (dotimes (i 4)
    (set-gc-params gc i filled)
    (funcall draw-fn gc rect start-pnt end-pnt)
    (loop for pnt in (list start-pnt end-pnt) do (incf (gfs:point-x pnt) delta-x))
    (incf (gfs:point-x (gfs:location rect)) delta-x)))

(defun draw-points (gc points delta-x draw-fn filled)
  (dotimes (i 4)
    (set-gc-params gc i filled)
    (funcall draw-fn gc points)
    (loop for pnt in points do (incf (gfs:point-x pnt) delta-x))))

(defun draw-start-points (gc start-pnt points delta-x draw-fn filled)
  (dotimes (i 4)
    (set-gc-params gc i filled)
    (funcall draw-fn gc start-pnt points)
    (loop for pnt in (append (list start-pnt) points) do (incf (gfs:point-x pnt) delta-x))))

(defun draw-start-end-controls (gc start-pnt end-pnt ctrl-pnt-1 ctrl-pnt-2 delta-x draw-fn)
  (dotimes (i 4)
    (set-gc-params gc i nil)
    (funcall draw-fn gc start-pnt end-pnt ctrl-pnt-1 ctrl-pnt-2)
    (loop for pnt in (list start-pnt end-pnt ctrl-pnt-1 ctrl-pnt-2) do (incf (gfs:point-x pnt) delta-x))))

(defun draw-arcs (gc)
  (let* ((rect-pnt (gfs:make-point :x 15 :y 10))
         (rect-size (gfs:make-size :width 80 :height 65))
         (rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
         (start-pnt (gfs:make-point :x 15 :y 60))
         (end-pnt (gfs:make-point :x 75 :y 25))
         (delta-x (+ (gfs:size-width rect-size) 10))
         (delta-y (+ (gfs:size-height rect-size) 10)))
    (draw-rect-start-end gc rect (clone-point start-pnt) (clone-point end-pnt) delta-x #'gfg:draw-filled-chord t)
    (incf (gfs:point-y rect-pnt) delta-y)
    (incf (gfs:point-y start-pnt) delta-y)
    (incf (gfs:point-y end-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rect-start-end gc rect (clone-point start-pnt) (clone-point end-pnt) delta-x #'gfg:draw-chord nil)
    (incf (gfs:point-y rect-pnt) delta-y)
    (incf (gfs:point-y start-pnt) delta-y)
    (incf (gfs:point-y end-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rect-start-end gc rect (clone-point start-pnt) (clone-point end-pnt) delta-x #'gfg:draw-arc nil)))

(defun select-arcs (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-arcs)
  (gfw:redraw *drawing-win*))

(defun draw-beziers (gc)
  (let ((start-pnt (gfs:make-point :x 10 :y 32))
        (end-pnt   (gfs:make-point :x 70 :y 32))
        (ctrl-pnt-1 (gfs:make-point :x 40 :y 0))
        (ctrl-pnt-2 (gfs:make-point :x 40 :y 65)))
    (draw-start-end-controls gc start-pnt end-pnt ctrl-pnt-1 ctrl-pnt-2 85 #'gfg:draw-bezier)
    (let ((poly-pnts (list (list (gfs:make-point :x 40 :y 100)
                                 (gfs:make-point :x 35 :y 200)
                                 (gfs:make-point :x 300 :y 180))
                           (list (gfs:make-point :x 260 :y 190)
                                 (gfs:make-point :x 140 :y 150)
                                 (gfs:make-point :x 80 :y 200)))))
      (setf (gfg:foreground-color gc) gfg:*color-blue*)
      (setf (gfg:pen-width gc) 3)
      (setf (gfg:pen-style gc) '(:dot :square-endcap))
      (gfg:draw-poly-bezier gc (gfs:make-point :x 10 :y 110) poly-pnts))))

(defun select-beziers (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-beziers)
  (gfw:redraw *drawing-win*))

(defun draw-ellipses (gc)
  (let* ((rect-pnt (gfs:make-point :x 15 :y 10))
         (rect-size (gfs:make-size :width 80 :height 65))
         (rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
         (delta-x (+ (gfs:size-width rect-size) 10))
         (delta-y (+ (gfs:size-height rect-size) 10)))
    (draw-rectangular gc rect nil delta-x #'gfg:draw-filled-ellipse t)
    (incf (gfs:point-y rect-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rectangular gc rect nil delta-x #'gfg:draw-ellipse nil)))

(defun select-ellipses (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-ellipses)
  (gfw:redraw *drawing-win*))

(defun draw-lines (gc)
  (let ((pnt-1 (gfs:make-point :x 15 :y 60))
        (pnt-2 (gfs:make-point :x 75 :y 30))
        (pnt-3 (gfs:make-point :x 40 :y 10))
        (delta-x 75)
        (delta-y 60))
    (draw-points gc
                 (list (clone-point pnt-1) (clone-point pnt-2) (clone-point pnt-3))
                 delta-x
                 #'gfg:draw-filled-polygon
                 t)
    (draw-points gc 
                 (mapcar #'(lambda (pnt) (gfs:make-point :x (gfs:point-x pnt)
                                                         :y (+ (gfs:point-y pnt) delta-y)))
                        (list pnt-1 pnt-2 pnt-3))
                 delta-x
                 #'gfg:draw-polygon
                 nil)
    (draw-points gc 
                 (mapcar #'(lambda (pnt) (gfs:make-point :x (gfs:point-x pnt)
                                                         :y (+ (gfs:point-y pnt) (* delta-y 2))))
                        (list pnt-1 pnt-2 pnt-3))
                 delta-x
                 #'gfg:draw-polyline
                 nil)
    (draw-start-end gc
                    (gfs:make-point :x (gfs:point-x pnt-1) :y (+ (gfs:point-y pnt-1) (* delta-y 3)))
                    (gfs:make-point :x (gfs:point-x pnt-2) :y (+ (gfs:point-y pnt-2) (* delta-y 3)))
                    delta-x
                    #'gfg:draw-line
                    nil)))

(defun select-lines (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-lines)
  (gfw:redraw *drawing-win*))

(defun draw-rects (gc)
  (let* ((rect-pnt (gfs:make-point :x 15 :y 10))
         (rect-size (gfs:make-size :width 80 :height 50))
         (rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
         (delta-x (+ (gfs:size-width rect-size) 10))
         (delta-y (+ (gfs:size-height rect-size) 10))
         (arc-size (gfs:make-size :width 10 :height 10)))
    (draw-rectangular gc rect arc-size delta-x #'gfg:draw-filled-rounded-rectangle t)
    (incf (gfs:point-y rect-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rectangular gc rect nil delta-x #'gfg:draw-filled-rectangle t)
    (incf (gfs:point-y rect-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rectangular gc rect arc-size delta-x #'gfg:draw-rounded-rectangle nil)
    (incf (gfs:point-y rect-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rectangular gc rect nil delta-x #'gfg:draw-rectangle nil)))

(defun select-rects (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-rects)
  (gfw:redraw *drawing-win*))

(defun draw-a-string (gc pnt text face-name pt-size font-style text-style)
  (let* ((font (make-instance 'gfg:font :gc gc
                                        :data (gfg:make-font-data :face-name face-name
                                                                  :style font-style
                                                                  :point-size pt-size)))
         (metrics (gfg:metrics gc font)))
    (if (or (null text) (zerop (length text)))
      (setf text face-name))
    (unwind-protect
        (progn
          (setf (gfg:font gc) font)
          (gfg:draw-text gc text pnt text-style)
          (gfs:make-point :x (gfs:point-x pnt) :y (+ (gfs:point-y pnt) (gfg:height metrics))))
      (gfs:dispose font))))

(defun draw-strings (gc)
  (setf (gfg:foreground-color gc) gfg:*color-blue*)
  (let ((pnt (gfs:make-point :x 2 :y 0)))  
    (setf pnt (draw-a-string gc pnt nil "Times New Roman" 10 nil nil))
    (setf pnt (draw-a-string gc pnt nil "Times New Roman" 14 '(:italic :bold :underline) nil))
    (setf pnt (draw-a-string gc pnt nil "Times New Roman" 18 '(:strikeout) nil))
    (setf pnt (draw-a-string gc pnt nil "Tahoma" 10 nil nil))
    (setf pnt (draw-a-string gc pnt nil "Tahoma" 14 '(:italic :bold :underline) nil))
    (setf pnt (draw-a-string gc pnt nil "Tahoma" 18 '(:strikeout) nil))
    (setf pnt (draw-a-string gc pnt nil "Lucida Console" 10 nil nil))
    (setf pnt (draw-a-string gc pnt nil "Lucida Console" 14 '(:italic :bold :underline) nil))
    (setf pnt (draw-a-string gc pnt nil "Lucida Console" 18 '(:strikeout) nil))
    (setf pnt (draw-a-string gc pnt nil "Courier New" 10 nil nil))
    (setf pnt (draw-a-string gc pnt nil "Courier New" 14 '(:italic :bold :underline) nil))
    (setf pnt (draw-a-string gc pnt nil "Courier New" 18 '(:strikeout) nil))

    (setf (gfs:point-x pnt) (+ (floor (gfs:size-width (gfw:client-size *drawing-win*)) 2) 10))
    (setf (gfs:point-y pnt) 0)
    (setf pnt (draw-a-string gc pnt (format nil "tab~ctab~ctab" #\Tab #\Tab) "Verdana" 10 nil '(:tab)))
    (setf pnt (draw-a-string gc pnt (format nil "even~cmore~ctabs" #\Tab #\Tab) "Verdana" 10 nil '(:tab)))
    (setf pnt (draw-a-string gc pnt " " "Verdana" 10 nil nil))
    (setf pnt (draw-a-string gc pnt "and a &mnemonic" "Verdana" 10 nil '(:mnemonic)))

    (setf pnt (draw-a-string gc pnt " " "Arial" 18 nil nil))
    (draw-a-string gc pnt "transparent" "Arial" 18 '(:bold) nil)
    (incf (gfs:point-x pnt) 50)
    (setf (gfg:foreground-color gc) gfg:*color-red*)
    (draw-a-string gc pnt "text" "Arial" 12 nil '(:transparent))))

(defun select-text (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-strings)
  (gfw:redraw *drawing-win*))

(defun draw-wedges (gc)
  (let* ((rect-pnt (gfs:make-point :x 5 :y 10))
         (rect-size (gfs:make-size :width 80 :height 65))
         (rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
         (delta-x (+ (gfs:size-width rect-size) 10))
         (delta-y (gfs:size-height rect-size))
         (start-pnt (gfs:make-point :x 35 :y 75))
         (end-pnt (gfs:make-point :x 85 :y 35)))

    (draw-rect-start-end gc rect (clone-point start-pnt) (clone-point end-pnt) delta-x #'gfg:draw-filled-pie-wedge t)
    (incf (gfs:point-y rect-pnt) delta-y)
    (incf (gfs:point-y start-pnt) delta-y)
    (incf (gfs:point-y end-pnt) delta-y)
    (setf rect (gfs:make-rectangle :location (clone-point rect-pnt) :size rect-size))
    (draw-rect-start-end gc rect (clone-point start-pnt) (clone-point end-pnt) delta-x #'gfg:draw-pie-wedge nil)))

(defun select-wedges (disp item)
  (declare (ignore disp))
  (update-drawing-item-check item)
  (setf (draw-func-of *drawing-dispatcher*) #'draw-wedges)
  (gfw:redraw *drawing-win*))

(defun drawing-tester-internal ()
  (setf *last-checked-drawing-item* nil)
  (let ((menubar (gfw:defmenu ((:item "&File"
                                :submenu ((:item "E&xit" :callback #'drawing-exit-fn)))
                               (:item "&Tests"
                                :callback #'find-checked-item
                                :submenu ((:item "&Arcs and Chords" :checked :callback #'select-arcs)
                                          (:item "&Bézier Curves" :callback #'select-beziers)
                                          (:item "&Ellipses" :callback #'select-ellipses)
                                          (:item "&Lines and Polylines" :callback #'select-lines)
                                          (:item "&Pie Wedges" :callback #'select-wedges)
                                          (:item "&Rectangles" :callback #'select-rects)
                                          (:item "&Text" :callback #'select-text)))))))
    (setf *drawing-dispatcher* (make-instance 'drawing-win-events))
    (setf (draw-func-of *drawing-dispatcher*) #'draw-arcs)
    (setf *drawing-win* (make-instance 'gfw:top-level :dispatcher *drawing-dispatcher*
                                                      :style '(:frame)))
    (setf (gfw:menu-bar *drawing-win*) menubar)
    (setf (gfw:size *drawing-win*) (gfs:make-size :width 390 :height 310))
    (setf (gfw:text *drawing-win*) "Drawing Tester")
    (setf (gfw:image *drawing-win*) (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico")))
    (gfw:show *drawing-win* t)))

(defun drawing-tester ()
  (gfw:startup "Drawing Tester" #'drawing-tester-internal))
