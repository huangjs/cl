;;;;
;;;; list-box.lisp
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

(in-package :graphic-forms.uitoolkit.widgets)

;;;
;;; helper functions
;;;

(defun lb-extend-select-flags (orig-flags)
  (setf orig-flags (logand orig-flags
                           (lognot (logior gfs::+lbs-nosel+ gfs::+lbs-multiplesel+))))
  (logior orig-flags gfs::+lbs-extendedsel+))

(defun lb-multi-select-flags (orig-flags)
  (setf orig-flags (logand orig-flags
                           (lognot (logior gfs::+lbs-nosel+ gfs::+lbs-extendedsel+))))
  (logior orig-flags gfs::+lbs-multiplesel+))

(defun lb-no-select-flags (orig-flags)
  (setf orig-flags (logand orig-flags
                           (lognot (logior gfs::+lbs-multiplesel+ gfs::+lbs-extendedsel+))))
  (logior orig-flags gfs::+lbs-nosel+))

(defun lb-single-select-flags (orig-flags)
  (logand orig-flags
          (lognot (logior gfs::+lbs-nosel+ gfs::+lbs-extendedsel+ gfs::+lbs-multiplesel+))))

(defun lb-is-single-select (lb)
  (not (test-native-style lb (logior gfs::+lbs-extendedsel+
                                     gfs::+lbs-multiplesel+
                                     gfs::+lbs-nosel+))))

(defun lb-width (hwnd)
  (let ((width (gfs::send-message hwnd gfs::+lb-gethorizontalextent+ 0 0)))
    (if (< width 0)
      (error 'gfs:win32-error :detail "LB_GETHORIZONTALEXTENT failed"))
    width))

(defun lb-item-count (hwnd)
  (let ((count (gfs::send-message hwnd gfs::+lb-getcount+ 0 0)))
    (if (< count 0)
      (error 'gfs:win32-error :detail "LB_GETCOUNT failed"))
    count))

(defun lb-delete-all (lb)
  (let ((old-items (slot-value lb 'items)))
    (gfs::send-message (gfs:handle lb) gfs::+lb-resetcontent+ 0 0)
    (dotimes (i (length old-items))
      (let ((victim (elt old-items i)))
        (setf (slot-value victim 'gfs:handle) nil)
        (gfs:dispose victim)))))

;;; This function is based on the package private select( int, boolean )
;;; method from SWT 3.2 located in List.java starting on line 998, without
;;; the additional scrolling logic.
;;;
(defun lb-select-item (lb index)
  (let ((hwnd (gfs:handle lb)))

    ;; sanity-check the index
    ;;
    (if (or (< index 0) (>= index (gfs::send-message hwnd gfs::+lb-getcount+ 0 0)))
      (return-from lb-select-item nil))

    ;; save the index of the top-most item
    ;;
    (let ((top-index (gfs::send-message hwnd gfs::+lb-gettopindex+ 0 0)))
      (cffi:with-foreign-object (top-item-rect-ptr 'gfs::rect)
        (cffi:with-foreign-object (sel-item-rect-ptr 'gfs::rect)

          ;; get the rectangle for the top-most item which we
          ;; will repaint at the end
          ;;
          (gfs::send-message hwnd      gfs::+lb-getitemrect+
                             top-index (cffi:pointer-address top-item-rect-ptr))
          (let ((redraw-needed (zerop (gfs::is-window-visible hwnd)))
                (has-sel-item nil))

            ;; if the list box is visible, disable repainting
            ;;
            (if redraw-needed
              (enable-redraw lb nil))
            (unwind-protect
                (progn
                  (if (lb-is-single-select lb)

                    ;; single-select list boxes must be configured differently
                    ;; from multi-select
                    ;;
                    (let ((old-index (gfs::send-message hwnd gfs::+lb-getcursel+ 0 0)))
                      (setf has-sel-item (/= old-index -1))

                      ;; get the rectangle for the old selected item
                      ;;
                      (if has-sel-item
                        (gfs::send-message hwnd      gfs::+lb-getitemrect+
                                           old-index (cffi:pointer-address sel-item-rect-ptr)))

                      ;; set the new selection
                      ;;
                      (gfs::send-message hwnd gfs::+lb-setcursel+ index 0))

                    ;; configure new selection for multi-select list boxes
                    ;;
                    (let ((focus-index (gfs::send-message hwnd gfs::+lb-getcaretindex+ 0 0)))

                      ;; set the new selection
                      ;;
                      (gfs::send-message hwnd gfs::+lb-setsel+ 1 index)

                      ;; if there was an item with focus, restore it
                      ;;
                      (if (/= focus-index -1)
                        (gfs::send-message hwnd gfs::+lb-setcaretindex+ focus-index 0)))))

              ;; restore the original top-index, then update the
              ;; list box and the top item and the selection item
              ;; 
              (gfs::send-message hwnd gfs::+lb-settopindex+ top-index 0)
              (when redraw-needed
                (enable-redraw lb t)
                (gfs::validate-rect hwnd (cffi:null-pointer))
                (gfs::invalidate-rect hwnd top-item-rect-ptr 1)
                (if has-sel-item
                  (gfs::invalidate-rect hwnd sel-item-rect-ptr 1))))))))))

(defun lb-deselect-item (lb index)
  (let ((hwnd (gfs:handle lb)))
    (if (or (< index 0) (>= index (gfs::send-message hwnd gfs::+lb-getcount+ 0 0)))
      (return-from lb-deselect-item nil))
    (if (lb-is-single-select lb)
      (let ((curr-index (gfs::send-message hwnd gfs::+lb-getcursel+ 0 0)))
        (if (= curr-index index)
          (gfs::send-message hwnd gfs::+lb-setcursel+ -1 0)))
      (gfs::send-message hwnd gfs::+lb-setsel+ 0 index))))

;;;
;;; methods
;;;

(defmethod append-item ((self list-box) thing disp &optional disabled checked classname)
  (declare (ignore disabled checked))
  (let* ((tc (thread-context))
         (hcontrol (gfs:handle self))
         (text (call-text-provider self thing))
         (item (create-item-with-callback hcontrol (or classname 'list-item) thing disp)))
    (lb-insert-item hcontrol #xFFFFFFFF text (cffi:null-pointer))
    (put-item tc item)
    (vector-push-extend item (slot-value self 'items))
    item))

(defmethod compute-style-flags ((self list-box) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags (logior +default-child-style+ gfs::+lbs-notify+
                           gfs::+ws-vscroll+ gfs::+ws-border+))
        (style (style-of self)))
    (loop for sym in style
          do (ecase sym
               ;; primary list-box styles
               ;;
               (:extend-select    (setf std-flags (lb-extend-select-flags std-flags)))
               (:multiple-select  (setf std-flags (lb-multi-select-flags  std-flags)))
               (:no-select        (setf std-flags (lb-no-select-flags     std-flags)))
               (:single-select    (setf std-flags (lb-single-select-flags std-flags)))

               ;; styles that can be combined
               ;;
               (:tab-stops        (setf std-flags (logior std-flags gfs::+lbs-usetabstops+)))
               (:scrollbar-always (setf std-flags (logior std-flags gfs::+lbs-disablenoscroll+)))
               (:want-keys        (setf std-flags (logior std-flags gfs::+lbs-wantkeyboardinput+)))))
    (values std-flags 0)))

(defmethod delete-all ((self list-box))
  (lb-delete-all self)
  (setf (slot-value self 'items) (make-items-array)))

(defmethod delete-selection ((self list-box))
  (enable-redraw self nil)
  (unwind-protect
      (loop for item in (selected-items self)
            do (gfs:dispose item))
    (enable-redraw self t)))

(defmethod delete-span ((self list-box) (span gfs:span))
  (enable-redraw self nil)
  (unwind-protect
      (dotimes (i (1+ (- (gfs:span-end span) (gfs:span-start span))))
        (delete-item self (gfs:span-start span)))
    (enable-redraw self t)))

(defmethod initialize-instance :after ((self list-box) &key estimated-count items parent &allow-other-keys)
  (create-control self parent "" gfs::+icc-standard-classes+)
  (if (and estimated-count (> estimated-count 0))
    (gfs::send-message (gfs:handle self)
                       gfs::+lb-initstorage+
                       estimated-count
                       (* estimated-count +estimated-text-size+)))
  (if items
    (setf (slot-value self 'items) (copy-item-sequence (gfs:handle self) items 'list-item)))
  (update-from-items self))

(defmethod (setf items-of) (new-items (self list-box))
  (lb-delete-all self)
  (setf (slot-value self 'items) (copy-item-sequence (gfs:handle self) new-items 'list-item))
  (update-from-items self))

(defmethod preferred-size ((self list-box) width-hint height-hint)
  (let ((hwnd (gfs:handle self))
        (min-size (slot-value self 'min-size))
        (max-size (slot-value self 'max-size))
        (size (gfs:make-size :width width-hint :height height-hint))
        (b-width (* (border-width self) 2)))
    (cond
      ((and min-size (< width-hint (gfs:size-width min-size)))
         (setf (gfs:size-width size) (gfs:size-width min-size)))
      ((and max-size (> width-hint (gfs:size-width max-size)))
         (setf (gfs:size-width size) (gfs:size-width max-size)))
      ((>= width-hint 0)
         (setf (gfs:size-width size) width-hint))
      (t
         (flet ((item-text (index)
                  (lb-item-text hwnd index (1+ (lb-item-text-length hwnd index)))))
           (setf (gfs:size-width size)
                 (loop for index to (1- (lb-item-count hwnd))
                        with dt-flags = (logior gfs::+dt-singleline+ gfs::+dt-noprefix+)
                        maximizing (gfs:size-width (widget-text-size self
                                                                     (lambda (unused)
                                                                       (declare (ignore unused))
                                                                       (item-text index))
                                                                     dt-flags))
                                   into max-width
                        finally (return (or max-width 0)))))))
    (cond
      ((and min-size (< height-hint (gfs:size-height min-size)))
         (setf (gfs:size-height size) (gfs:size-height min-size)))
      ((and max-size (> height-hint (gfs:size-height max-size)))
         (setf (gfs:size-height size) (gfs:size-height max-size)))
      ((>= height-hint 0)
         (setf (gfs:size-height size) height-hint))
      (t
        (setf (gfs:size-height size) (* (lb-item-count hwnd) (1+ (lb-item-height hwnd))))))
    (if (zerop (gfs:size-width size))
      (setf (gfs:size-width size) +default-widget-width+)
      (incf (gfs:size-width size) (+ b-width 4)))
    (if (zerop (gfs:size-height size))
      (setf (gfs:size-height size) +default-widget-height+)
      (incf (gfs:size-height size) b-width))
    (if (test-native-style self gfs::+ws-vscroll+)
      (incf (gfs:size-width size) (vertical-scrollbar-width)))
    size))

(defmethod select-all ((self list-box) flag)
  (when (test-native-style self (logior gfs::+lbs-extendedsel+ gfs::+lbs-multiplesel+))
    (gfs::send-message (gfs:handle self) gfs::+lb-setsel+ (if flag 1 0) #xFFFFFFFF)))

(defmethod selected-count ((self list-box))
  (let ((hwnd (gfs:handle self)))
    (if (test-native-style self gfs::+lbs-nosel+)
      (if (>= (gfs::send-message hwnd gfs::+lb-getcursel+ 0 0) 0) 1 0)
      (let ((count (gfs::send-message hwnd gfs::+lb-getselcount+ 0 0)))
        (if (< count 0) 0 count)))))

(defmethod selected-items ((self list-box))
  (let ((hwnd (gfs:handle self))
        (items (slot-value self 'items)))
    (if (lb-is-single-select self)
      (let ((index (gfs::send-message hwnd gfs::+lb-getcursel+ 0 0)))
        (if (and (>= index 0) (< index (length items)))
          (list (elt items index))
          nil))
      (let ((count (gfs::send-message hwnd gfs::+lb-getselcount+ 0 0)))
        (if (<= count 0)
          nil
          (cffi:with-foreign-object (indices :unsigned-int count)
            (if (/= (gfs::send-message hwnd gfs::+lb-getselitems+ count (cffi:pointer-address indices)) count)
              nil
              (gfs::pick-elements items indices count))))))))

(defmethod update-from-items ((self list-box))
  (let ((sort-func (sort-predicate-of self))
        (hwnd (gfs:handle self)))
    (unless (zerop (lb-item-count hwnd))
      (error 'gfs:toolkit-error :detail "list-box has existing content"))
    (when sort-func
      (setf (slot-value self 'items) (gfs::indexed-sort (slot-value self 'items) sort-func #'data-of)))
    (enable-redraw self nil)
    (unwind-protect
        (let ((items (slot-value self 'items)))
          (dotimes (index (length items))
            (let* ((item (elt items index))
                   (text (call-text-provider self (data-of item))))
              (lb-insert-item hwnd #xFFFFFFFF text (cffi:null-pointer)))))
      (enable-redraw self t))))
