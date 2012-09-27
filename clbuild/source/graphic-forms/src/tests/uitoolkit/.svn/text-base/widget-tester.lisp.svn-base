;;;;
;;;; widget-tester.lisp
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

                               ;; drop cookies
(defvar *list-box-test-data* '("chocolate chip" "butterscotch crunch" "peanut butter" "oatmeal"
                               ;; molded cookies
                               "butterfinger chunkies" "jam thumbprints" "cappuccino flats"
                               ;; pressed cookies
                               "langues de chat" "macaroons" "shortbread"
                               ;; refrigerator cookies
                               "brysell" "caramel" "mosaic" "praline" "toffee"))

(defvar *widget-tester-win* nil)

(defun widget-tester-exit (disp item)
  (declare (ignore disp item))
  (gfs:dispose *widget-tester-win*)
  (setf *widget-tester-win* nil)
  (gfw:shutdown 0))

(defclass widget-tester-events (gfw:event-dispatcher) ())

(defmethod gfw:event-close ((disp widget-tester-events) window)
  (declare (ignore window))
  (widget-tester-exit disp nil))

(defclass widget-tester-panel-events (gfw:event-dispatcher) ())

(defmethod gfw:event-paint ((disp widget-tester-panel-events) window gc rect)
  (declare (ignore rect))
  (let ((color (gfg:rgb->color (gfs::get-sys-color gfs::+color-btnface+))))
    (setf (gfg:background-color gc) color
          (gfg:foreground-color gc) color))
  (gfg:draw-filled-rectangle gc (gfs:make-rectangle :size (gfw:client-size window))))

(defclass tester-panel (gfw:panel) ())

(defmethod initialize-instance :after ((panel tester-panel) &key &allow-other-keys)
  (setf (gfw:dispatcher panel) (make-instance 'widget-tester-panel-events)))

(defun manage-lb-button-states (lb move-btn selected-btn all-btn none-btn)
  (let ((sel-count (gfw:selected-count lb))
        (item-count (gfw:item-count lb)))
    (gfw:enable move-btn (> sel-count 0))
    (if selected-btn
      (gfw:check selected-btn (> sel-count 0)))
    (if all-btn
      (gfw:enable all-btn  (and (> item-count 0) (< sel-count item-count))))
    (if none-btn
      (gfw:enable none-btn (> sel-count 0)))))

(defun move-lb-content (orig-lb dest-lb)
  (let ((sel-items (gfw:selected-items orig-lb)))
    (gfw:delete-selection orig-lb)
    (if sel-items
      (setf (gfw:items-of dest-lb) (append sel-items (gfw:items-of dest-lb))))))

(defun select-lb-content (lb state)
  (let ((count (gfw:item-count lb))
        (func (if state #'gfw::lb-select-item #'gfw::lb-deselect-item)))
    (if (> count 0)
      (gfw:select (first (gfw:items-of lb)) state))
    (loop for index in '(2 4)
          when (>= count (1+ index))
          do (funcall func lb index))))
#|
  (let ((items (gfw:items-of lb)))
    (setf (gfw:selected-items lb) (subseq items 0 (min 4 (length items))))))
|#

(defun populate-list-box-test-panel ()
  (setf (gfw:text *widget-tester-win*) "Widget Tester (List Boxes)")
  (let* ((latch      nil)
         (lb1        nil)
         (lb2        nil)
         (btn-left   nil)
         (btn-right  nil)
         (btn-all    nil)
         (btn-none   nil)
         (btn-select nil)
         (lb1-callback        (lambda (disp lb)
                                (declare (ignore disp))
                                (manage-lb-button-states lb btn-right (if latch nil btn-select) btn-all btn-none)))
         (lb2-callback        (lambda (disp lb)
                                (declare (ignore disp))
                                (manage-lb-button-states lb btn-left nil nil nil)))
         (btn-left-callback   (lambda (disp btn)
                                (declare (ignore disp btn))
                                (move-lb-content lb2 lb1)
                                (manage-lb-button-states lb1 btn-right btn-select btn-all btn-none)
                                (manage-lb-button-states lb2 btn-left nil nil nil)))
         (btn-right-callback  (lambda (disp btn)
                                (declare (ignore disp btn))
                                (move-lb-content lb1 lb2)
                                (manage-lb-button-states lb1 btn-right btn-select btn-all btn-none)
                                (manage-lb-button-states lb2 btn-left nil nil nil)))
         (btn-all-callback    (lambda (disp btn)
                                (declare (ignore disp btn))
                                (gfw:select-all lb1 t)
                                (manage-lb-button-states lb1 btn-right btn-select btn-all btn-none)))
         (btn-none-callback   (lambda (disp btn)
                                (declare (ignore disp btn))
                                (gfw:select-all lb1 nil)
                                (manage-lb-button-states lb1 btn-right btn-select btn-all btn-none)))
         (btn-reset-callback  (lambda (disp btn)
                                (declare (ignore disp btn))
                                (gfw:delete-all lb2)
                                (setf (gfw:items-of lb1) *list-box-test-data*)
                                (manage-lb-button-states lb1 btn-right btn-select btn-all btn-none)
                                (manage-lb-button-states lb2 btn-left nil nil nil)))
         (btn-select-callback (lambda (disp btn)
                                (declare (ignore disp))
                                (setf latch t)
                                (select-lb-content lb1 (gfw:selected-p btn))
                                (manage-lb-button-states lb1 btn-right nil btn-all btn-none)
                                (setf latch nil)))
         (outer-layout (make-instance 'gfw:border-layout :spacing 4 :margins 4))
         (outer-panel (make-instance 'tester-panel :parent     *widget-tester-win*
                                                   :layout     outer-layout))
         (lb1-panel (make-instance 'tester-panel :parent     outer-panel
                                                 :layout     (make-instance 'gfw:flow-layout :style '(:vertical) :spacing 4 :margins 4)))
         (btn-panel (make-instance 'tester-panel :parent     outer-panel
                                                 :layout     (make-instance 'gfw:flow-layout :style '(:vertical :normalize) :spacing 4 :margins 4)))
         (lb2-panel (make-instance 'tester-panel :parent     outer-panel
                                                 :layout     (make-instance 'gfw:flow-layout :style '(:vertical) :spacing 4 :margins 4))))

    (make-instance 'gfw:label :text "Multiple Select:" :parent lb1-panel)
    (setf lb1 (make-instance 'gfw:list-box :parent lb1-panel
                                           :callback lb1-callback
                                           :sort-predicate #'string<
                                           :style '(:multiple-select)
                                           :items (subseq *list-box-test-data* 4)))
    (gfw:pack lb1-panel)
    (setf (gfw:layout-attribute outer-layout lb1-panel :left) t)

    (setf btn-right  (make-instance 'gfw:button :parent btn-panel
                                                :text " ==> "
                                                :callback btn-right-callback))
    (gfw:enable btn-right nil)
    (setf btn-left   (make-instance 'gfw:button :parent btn-panel
                                                :text " <== "
                                                :callback btn-left-callback))
    (gfw:enable btn-left nil)
    (setf btn-all    (make-instance 'gfw:button :parent btn-panel
                                                :text "Select All"
                                                :callback btn-all-callback))
    (setf btn-none   (make-instance 'gfw:button :parent btn-panel
                                                :text "Select None"
                                                :callback btn-none-callback))
    (gfw:enable btn-none nil)
                     (make-instance 'gfw:button :parent btn-panel
                                                :text "Reset"
                                                :callback btn-reset-callback)
    (setf btn-select (make-instance 'gfw:button :parent btn-panel
                                                :text "Select 0,2,4"
                                                :style '(:check-box)
                                                :callback btn-select-callback))
    (gfw:pack btn-panel)
    (setf (gfw:layout-attribute outer-layout btn-panel :center) t)

    (make-instance 'gfw:label :text "Extended Select:" :parent lb2-panel)
    (setf lb2 (make-instance 'gfw:list-box :parent lb2-panel
                                           :callback lb2-callback
                                           :style '(:extend-select :scrollbar-always)
                                           :items (subseq *list-box-test-data* 4)))
    (gfw:pack lb2-panel)
    (setf (gfw:layout-attribute outer-layout lb2-panel :right) t)

    (gfw:pack outer-panel)
    ;; FIXME: need to think of a more elegant solution for the following
    ;; use-case where we want synchronize the sizes of two or more
    ;; layout children
    ;;
    (let ((size (gfw:size lb1)))
      (setf (gfw:maximum-size lb1) size
            (gfw:minimum-size lb1) size
            (gfw:maximum-size lb2) size
            (gfw:minimum-size lb2) size))
    (setf (gfw:items-of lb1) *list-box-test-data*)
    (manage-lb-button-states lb1 btn-right btn-select btn-all btn-none)
    (gfw:delete-all lb2)

    outer-panel))

(defun thumb->string (thing)
  (format nil "~d" (gfw:thumb-position thing)))

(defun populate-slider-test-panel ()
  (setf (gfw:text *widget-tester-win*) "Widget Tester (Sliders)")
  (let* ((layout1 (make-instance 'gfw:flow-layout :style '(:vertical) :spacing 4))
         (layout2 (make-instance 'gfw:flow-layout :style '(:horizontal) :margins 4 :spacing 4))
         (layout3 (make-instance 'gfw:flow-layout :style '(:horizontal) :margins 4 :spacing 4))
         (outer-panel (make-instance 'tester-panel :parent     *widget-tester-win*
                                                   :layout     layout1))
         (panel-1  (make-instance 'tester-panel :parent     outer-panel
                                                :layout     layout2))
         (label-1     (make-instance 'gfw:label  :parent panel-1
                                                 :text "0  "))
         (sl-1-cb     (lambda (disp slider axis detail)
                        (declare (ignore disp axis detail))
                        (setf (gfw:text label-1) (thumb->string slider))))
         (sl-1        (make-instance 'gfw:slider :parent panel-1
                                                 :callback sl-1-cb
                                                 :outer-limit 10))
         (label-3     (make-instance 'gfw:label  :parent panel-1
                                                 :text "0  "))
         (sb-1-cb     (lambda (disp scrollbar axis detail)
                        (declare (ignore disp axis detail))
                        (setf (gfw:text label-3) (thumb->string scrollbar))))
         (sb-1        (make-instance 'gfw:scrollbar :parent panel-1
                                                    :callback sb-1-cb
                                                    :outer-limit 10))
         (panel-2  (make-instance 'tester-panel :parent     outer-panel
                                                :layout     layout3))
         (label-2     (make-instance 'gfw:label  :parent panel-2
                                                 :text "0  "))
         (sl-2-cb     (lambda (disp slider axis detail)
                        (declare (ignore disp axis detail))
                        (setf (gfw:text label-2) (thumb->string slider))))
         (sl-2        (make-instance 'gfw:slider :parent panel-2
                                                 :callback sl-2-cb
                                                 :style '(:vertical :auto-ticks :ticks-after :ticks-before)
                                                 :outer-limit 10))
         (label-4     (make-instance 'gfw:label     :parent panel-2
                                                    :text "0  "))
         (sb-2-cb     (lambda (disp scrollbar axis detail)
                        (declare (ignore disp axis detail))
                        (setf (gfw:text label-4) (thumb->string scrollbar))))
         (sb-2        (make-instance 'gfw:scrollbar :parent panel-2
                                                    :callback sb-2-cb
                                                    :style '(:vertical)
                                                    :outer-limit 10)))
    (declare (ignore sl-1 sl-2 sb-1 sb-2))
    (gfw:pack panel-1)
    (gfw:pack panel-2)
    (gfw:pack outer-panel)
    outer-panel))

(defun populate-progress-test-panel ()
  (setf (gfw:text *widget-tester-win*) "Widget Tester (Progress Bar)")
  (let* ((layout1 (make-instance 'gfw:border-layout :margins 4 :spacing 4))
         (layout2 (make-instance 'gfw:flow-layout :margins 4))
         (outer-panel (make-instance 'tester-panel :parent *widget-tester-win*
                                                   :layout layout1))
         (p-bar (make-instance 'gfw:progress-bar :parent outer-panel
                                                 :style '(:horizontal)))
         (lower-panel (make-instance 'tester-panel :parent outer-panel
                                                   :layout layout2))
         (btn-cb (lambda (disp button)
                   (declare (ignore disp button))
                   (let ((curr-pos (gfw:bar-position p-bar)))
                     (if (< curr-pos 10)
                         (gfw:step p-bar)
                         (setf (gfw:bar-position p-bar) 0)))))
         (step-btn (make-instance 'gfw:button :parent lower-panel
                                               :text "Step Progress"
                                               :callback btn-cb)))
    (declare (ignore step-btn))
    (setf (gfw:inner-limits p-bar) (gfs:make-span :start 0 :end 10))
    (setf (gfw:layout-attribute layout1 p-bar :top) t
          (gfw:layout-attribute layout1 lower-panel :bottom) t)
    (setf (gfw:step-increment p-bar) 2)
    outer-panel))

(defun widget-tester-internal ()
  (setf *default-pathname-defaults* (parse-namestring gfsys::*gf-tests-dir*))
  (setf *widget-tester-win* (make-instance 'gfw:top-level :dispatcher (make-instance 'widget-tester-events)
                                                          :layout (make-instance 'gfw:heap-layout)
                                                          :style '(:frame :status-bar)))
  (let* ((layout (gfw:layout-of *widget-tester-win*))
         (test-panels (list (populate-list-box-test-panel)
                            (populate-slider-test-panel)
                            (populate-progress-test-panel)))
         (select-lb-callback (lambda (disp item)
                               (declare (ignore disp item))
                               (setf (gfw:top-child-of layout) (first test-panels))
                               (gfw:layout *widget-tester-win*)))
         (select-sb-callback (lambda (disp item)
                               (declare (ignore disp item))
                               (setf (gfw:top-child-of layout) (second test-panels))
                               (gfw:layout *widget-tester-win*)))
         (select-pb-callback (lambda (disp item)
                               (declare (ignore disp item))
                               (setf (gfw:top-child-of layout) (third test-panels))
                               (gfw:layout *widget-tester-win*)))
         (menubar (gfw:defmenu ((:item    "&File"
                                 :submenu ((:item "E&xit" :callback #'widget-tester-exit)))
                                (:item    "&Panels"
                                 :submenu ((:item "&List Boxes"    :callback select-lb-callback)
                                           (:item "&Progress Bars" :callback select-pb-callback)
                                           (:item "&Sliders"       :callback select-sb-callback)))))))
    (setf (gfw:menu-bar *widget-tester-win*) menubar
          (gfw:top-child-of layout) (first test-panels)
          (gfw:image *widget-tester-win*) (make-instance 'gfg:icon-bundle :file (merge-pathnames "default.ico"))))
  (gfw:pack *widget-tester-win*)
  (gfw:show *widget-tester-win* t))

(defun widget-tester ()
  (gfw:startup "Widget Tester" #'widget-tester-internal))
