;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIMI; -*-

;;;  (c) copyright 2003 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Cut and Paste

(in-package :climi)

;;;; Interaction implemented:

;; Shift-Mouse-L down: clear active selection and set the first point
;; Shift-Mouse-L drag: drag the second point
;; Shift-Mouse-L up:   set the second point

;; Shift-Mouse-R down: pick the nearest point, if any
;; Shift-Mouse-R drag: drag said point
;; Shift-Mouse-R up:   leave said point where it was dragged to.

;; Shift-Mouse-M:      paste

;;;; Interaction to implement:

;; Shift-Mouse-L single click: (maybe) select current presentation, if any.
;; Shift-Mouse-L double click: select word
;; Shift-Mouse-L triple click: select "line".

;; TODO:
;;   * Editor input (both active and old) is not currently highlighted.
;;   * Selecting large regions gets slow.
;;   * Structure of line breaks in the original text is not preserved (CLIM/McCLIM design issue)


;;;; Preferences

(defparameter *marking-border* 1)

(defparameter *marked-foreground* +white+
  "Foreground ink to use for marked stuff.")

(defparameter *marked-background* +blue4+
  "Background ink to use for marked stuff.")


;;;; Text Selection Protocol

(defgeneric release-selection (port &optional time)
  (:documentation "Relinquish ownership of the selection."))

(defgeneric request-selection (port requestor time)
  (:documentation "Request that the window system retrieve the selection from
its current owner. This should cause a selection-notify-event to be delivered."))

(defgeneric bind-selection (port window &optional time)
  (:documentation "Take ownership of the selection."))

(defgeneric send-selection (port request-event string)
  (:documentation "Send 'string' to a client in response to a selection-request-event."))

(defgeneric get-selection-from-event (port event)
  (:documentation "Given a selection-notify event, return a string containing
the incoming selection."))

;; These events are probably very X11 specific.

;; Backends will likely produce subclasses of selection-notify-event
;; and selection-request-event.

(defclass selection-event (window-event)
  ((selection :initarg :selection
              :reader selection-event-selection)))

(defclass selection-clear-event (selection-event)  ())
(defclass selection-notify-event (selection-event) ())
(defclass selection-request-event (selection-event)
  ((requestor :initarg :requestor :reader selection-event-requestor)))


;;;; Random Notes

;; - McCLIM still has absolutely no idea of lines.

(defclass marking ()
  ()
  (:documentation "A common super class for markings (= stuff marked)."))

(defgeneric marking-region (stream marking)
  (:documentation "Region marked/affected."))

(defclass string-marking (marking)
  ((record :initarg :record
           :documentation "The text output record this belongs to.")
   (styled-string :initarg :styled-string
                  :documentation "The styled string sub-record of 'record'.")
   (start :initarg :start :reader mark-start
          :documentation "Start index within string.")
   (end :initarg :end :reader mark-end
        :documentation "End index within string. Caution: Could be one off the end to indicate a newline implied."))
  (:documentation "Some part of a styled-string marked."))

(defmethod marking-region (stream (marking string-marking))
  (with-slots (record styled-string start end) marking
    (with-slots (baseline start-y) record
      (if (= start end)
          +nowhere+
          (with-slots (start-x string text-style) styled-string
            (make-rectangle* (+ start-x
                                (stream-string-width stream string
                                                     :start 0 :end start
                                                     :text-style text-style)
                                (- *marking-border*))
                             (+ start-y baseline
                                (- (text-style-ascent text-style stream))
                                (- *marking-border*))
                             (+ start-x
                                (stream-string-width stream string
                                                     :start 0 :end end
                                                     :text-style text-style)
                                *marking-border*)
                             (+ start-y baseline (text-style-descent text-style stream)
                                *marking-border*)))))))

;(defgeneric draw-marking (medium marking)
;  (:documentation "Draw the marking to medium."))
;
;(defmethod draw-marking (stream (marking string-marking))
;  (draw-design (sheet-medium stream) (marking-region marking)
;               :ink +flipping-ink+))

;;;;

(defclass cut-and-paste-mixin ()
  ((markings   :initform nil)
   (point-1-x  :initform nil)
   (point-1-y  :initform nil)
   (point-2-x  :initform nil)
   (point-2-y  :initform nil)
   (dragging-p :initform nil)))

(defclass paste-as-keypress-mixin ()
  ()
  (:documentation "Implements the old McCLIM behavior of pasting via a
  sequence of key press events. You couldn't possibly want this."))

(defmethod handle-repaint :around ((pane cut-and-paste-mixin) region)
  (with-slots (markings) pane
    (cond ((null markings)
           (call-next-method))
          (t
           (let ((marked-region
                  (reduce #'region-union (mapcar #'(lambda (x) (marking-region pane x)) (slot-value pane 'markings))
                          :initial-value +nowhere+)))
             (with-sheet-medium (medium pane)
               (let ((R (region-difference region marked-region)))
                 (with-drawing-options (medium :clipping-region R)
                   (call-next-method pane R))))
             (with-sheet-medium (medium pane)
               (let ((R (region-intersection region marked-region)))
                 (with-drawing-options (medium :clipping-region R)
                   (letf (((medium-foreground medium) *marked-foreground*)
                          ((medium-background medium) *marked-background*))
                     (call-next-method pane R))))))))))

(defmethod dispatch-event :around ((pane cut-and-paste-mixin)
                           (event pointer-button-press-event))  
  (if (eql (event-modifier-state event) +shift-key+)
      (eos/shift-click pane event)
      (call-next-method)))

(defmethod dispatch-event :around ((pane cut-and-paste-mixin)
                           (event pointer-button-release-event))
  (if (eql (event-modifier-state event) +shift-key+)
      (eos/shift-release pane event)
      (call-next-method)))

(defmethod dispatch-event :around ((pane cut-and-paste-mixin)
                           (event pointer-motion-event))
  (with-slots (point-1-x dragging-p) pane
    (if (and (eql (event-modifier-state event) +shift-key+))
        (when dragging-p (eos/shift-drag pane event))
        (call-next-method))))


(defun pane-clear-markings (pane &optional time)
  (repaint-markings pane (slot-value pane 'markings)
                    (setf (slot-value pane 'markings) nil))
  (release-selection (port pane) time))
 

(defmethod eos/shift-click ((pane extended-output-stream) event)
  (with-slots (point-1-x point-1-y point-2-x point-2-y dragging-p) pane
    (cond ((eql +pointer-left-button+ (pointer-event-button event))
           (pane-clear-markings pane (event-timestamp event))
           ;; start dragging, set point-1 where the mouse is
           (setf point-1-x (pointer-event-x event))
           (setf point-1-y (pointer-event-y event))
           (setf dragging-p t))
          ((eql +pointer-middle-button+ (pointer-event-button event))
           ;; paste           
           (request-selection (port pane) #|:UTF8_STRING|# (sheet-direct-mirror pane) (event-timestamp event)))
          ((eql +pointer-right-button+ (pointer-event-button event))
           (when (and point-1-x point-1-y point-2-x point-2-y)
             ;; If point-1 and point-2 are set up pick the nearest (what metric?) and drag it around.
             (when (< (+ (expt (- (pointer-event-x event) point-1-x) 2)
                         (expt (- (pointer-event-y event) point-1-y) 2))
                      (+ (expt (- (pointer-event-x event) point-2-x) 2)
                         (expt (- (pointer-event-y event) point-2-y) 2)))
               (rotatef point-1-x point-2-x)
               (rotatef point-1-y point-2-y))
             (eos/shift-drag pane event)
             (setf dragging-p t)))
          (t (describe event)))))

(defmethod eos/shift-release ((pane extended-output-stream) event)  
  (with-slots (point-1-x point-1-y point-2-x point-2-y dragging-p) pane
    (when dragging-p
      (setf point-2-x (pointer-event-x event)
            point-2-y (pointer-event-y event)
            dragging-p nil)
      ;;
      (let ((owner (selection-owner (port pane))))
        (when (and owner (not (eq owner pane)))
          (distribute-event (port pane)
                            (make-instance 'selection-clear-event
                                           :sheet owner
                                           :selection :primary))))
      (when (bind-selection (port pane) pane (event-timestamp event))
	(setf (selection-owner (port pane)) pane)
	(setf (selection-timestamp (port pane)) (event-timestamp event))))))

(defun repaint-markings (pane old-markings new-markings)
  (let ((old-region (reduce #'region-union (mapcar #'(lambda (x) (marking-region pane x)) old-markings)
                            :initial-value +nowhere+))
        (new-region (reduce #'region-union (mapcar #'(lambda (x) (marking-region pane x)) new-markings)
                            :initial-value +nowhere+)))
    (handle-repaint pane (region-exclusive-or old-region new-region))))

(defmethod eos/shift-drag ((pane extended-output-stream) event)
  (with-slots (point-1-x point-1-y) pane
    (let ((old-markings (slot-value pane 'markings)))
      (setup-marked-extents pane (stream-output-history pane) +everywhere+
                            point-1-x point-1-y
                            (pointer-event-x event)
                            (pointer-event-y event))
      (repaint-markings pane old-markings (slot-value pane 'markings)))))

(defun map-over-text (record function)  
  (cond ((typep record 'standard-text-displayed-output-record)
         (with-slots (strings baseline max-height start-y wrapped x1 y1) record
           (loop for substring in strings do
                 (with-slots (start-x string marked-extent text-style) substring
                   (funcall function start-x (+ start-y baseline) string text-style
                            substring record)))))
        (t
         (map-over-output-records-overlapping-region
          #'(lambda (x)
              (map-over-text x function))
          record +everywhere+))))

(defun setup-marked-extents (stream record region bx1 by1 bx2 by2)
  (declare (ignore region))
  (when (> by1 by2)
    (rotatef by1 by2)
    (rotatef bx1 bx2))
  (let ((*lines* nil)
        (*all-lines* nil))
    (map-over-text record
                   (lambda (x y string ts record full-record)
                     (let ((q (assoc y *lines*)))
                       (unless q
                         (push (setf q (cons y nil)) *lines*))
                       (push (list x y string ts record full-record)
                             (cdr q)))
                     (force-output *trace-output*)))
    (setf *lines*
          (sort (mapcar (lambda (line)
                          (cons (car line)
                                (sort (cdr line) #'< :key #'first)))
                        *lines*)
                #'< :key #'car))
    (setf *all-lines* *lines*)
    ;; Nuke every line that is above by1
    (setf *lines* (remove-if (lambda (line) (< (+ (car line) 3) by1)) *lines*))
    ;; Also nuke all that are below by2
    (setf *lines* (remove-if (lambda (line) (> (- (car line) 10) by2)) *lines*))
    ;; Special case:
    (when (= 1 (length *lines*))
      (psetf bx1 (min bx1 bx2)
             bx2 (max bx1 bx2)))
    ;; Then, in the first line find the index farthest to the right
    ;; which is still less than bx1.
    (let ((start-i 0)
          (start-record (fifth (cadar *lines*)))
          (end-i 0)
          (end-record (fifth (cadar (last *lines*)))))
      
      (loop for chunk in (cdr (first *lines*)) do
        (destructuring-bind (x y string ts record full-record) chunk
          (declare (ignorable x y string ts record full-record))
          (loop for i to (length string) do
            (when (< (+ x (stream-string-width stream string :start 0 :end i :text-style ts))
                     bx1)
              (setf start-i i
                    start-record record)))))

      ;; Finally in the last line find the index farthest to the left
      ;; which still is greater than bx2.  Or put differently: Search
      ;; from the left and while we are still in bounds maintain end-i
      ;; and end-record.
      (loop for chunk in (cdr (car (last *lines*))) do
        (destructuring-bind (x y string ts record full-record) chunk
          (declare (ignorable x y string ts record full-record))
          (loop for i to (length string) do
            (when (< (+ x (stream-string-width stream string :start 0 :end i :text-style ts))
                     bx2)
              (setf end-i i
                    end-record record)))))

      ;; Now grovel over the records, in order ...
      (let ((in-p nil)
            (marks nil))
        (labels ((visit (chunk)
                   (destructuring-bind (x y string ts record full-record) chunk
                     (declare (ignorable x y string ts record full-record))
                     (let ((marked-extent nil))
                       (cond ((eq record start-record)
                              (cond ((eq record end-record)
                                     (setf marked-extent
                                           (cons start-i end-i)))
                                    (t
                                     (setf marked-extent
                                           (cons start-i (length string)))
                                     (setf in-p t))))
                             ((eq record end-record)
                              (setf marked-extent
                                    (cons 0 end-i))
                              (setf in-p nil))
                             (t
                              (setf marked-extent
                                    (if in-p
                                        (cons 0 (length string))
                                      nil))) )
                       (when marked-extent
                         (push (destructuring-bind (x y string ts record full-record) chunk
                                 (declare (ignorable x y string ts record full-record))
                                 (make-instance 'string-marking
                                                :record full-record
                                                :styled-string record
                                                :start (car marked-extent)
                                                :end (cdr marked-extent)))
                               marks)) ))))
          (loop for line in *all-lines* do
            (loop for chunk in (cdr line) do
              (visit chunk)) )
          (setf (slot-value stream 'markings) (reverse marks)))))))


;;;; Selections Events

(defmethod dispatch-event :around ((pane cut-and-paste-mixin)
                                   (event selection-clear-event))  
  (pane-clear-markings pane (event-timestamp event)))

(defmethod dispatch-event :around ((pane cut-and-paste-mixin)
                                   (event selection-request-event))  
  (send-selection (port pane) event (fetch-selection pane)))

(define-condition selection-notify ()
  ((event :reader event-of :initarg :event)))

(defmethod handle-event ((pane cut-and-paste-mixin)
                         (event selection-notify-event))
  (signal 'selection-notify :event event))

(defmethod dispatch-event :around ((pane paste-as-keypress-mixin)
                                   (event selection-notify-event))
  (let ((matter (get-selection-from-event (port pane) event)))
    (loop for c across matter do
         (dispatch-event pane
                         (make-instance 'key-press-event
                                        :timestamp (event-timestamp event)
                                        :sheet pane
                                        :modifier-state 0
                                        :x 0 :y 0 :graft-x 0 :graft-y 0
                                        :key-name nil
                                        :key-character c)))))


;; FIXME: Non-text target conversions.. (?)
(defun fetch-selection (pane)
  (let (old-y2 old-x2)
    (with-output-to-string (bag)
      (map nil
           (lambda (m)
             (with-slots (record styled-string start end) m
               (with-standard-rectangle*
                   (:x1 x1 :x2 x2 :y1 y1 :y2 y2) record
                   (cond ((and old-y2 (>= y1 old-y2))
                          (setf old-y2 nil
                                old-x2 0 ;<-- ### we should use the minimum of all x1 coordinates.
                                )
                          (terpri bag))
                         (t
                          (setf old-y2 (max y2 (or old-y2 y2)))))
                   (when old-x2
                     (loop repeat (round
                                   (- x1 old-x2)
                                   (text-style-width (slot-value styled-string 'text-style)
                                                     pane))
                       do
                       (princ " " bag)))
                   (setf old-x2 x2)
                   (princ (subseq (styled-string-string styled-string) start end) bag))))
           (slot-value pane 'markings)))))
