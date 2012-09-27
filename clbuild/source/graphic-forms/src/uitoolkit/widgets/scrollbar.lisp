;;;;
;;;; scrollbar.lisp
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

(defun sb-horizontal-flags (orig-flags)
  (logand orig-flags (lognot gfs::+sbs-vert+)))

(defun sb-vertical-flags (orig-flags)
  (logior orig-flags gfs::+sbs-vert+))

(defun validate-scrollbar-type (type)
  (unless (or (= type gfs::+sb-ctl+) (= type gfs::+sb-horz+) (= type gfs::+sb-vert+))
    (error 'gfs:toolkit-error :detail "invalid scrollbar type ID")))

(defun sb-get-info (scrollbar type)
  (if (gfs:disposed-p scrollbar)
    (error 'gfs:disposed-error))
  (validate-scrollbar-type type)
  (let ((hwnd (gfs:handle scrollbar)))
    (cffi:with-foreign-object (info-ptr 'gfs::scrollinfo)
      (gfs::zero-mem info-ptr gfs::scrollinfo)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::fmask gfs::pagesize
                                 gfs::pos gfs::maxpos gfs::trackpos)
                                info-ptr gfs::scrollinfo)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::scrollinfo)
              gfs::fmask  gfs::+sif-all+)
        (gfs::get-scroll-info hwnd type info-ptr)
        (list gfs::maxpos
              gfs::pagesize
              gfs::pos
              gfs::trackpos)))))

(defun sb-set-page-increment (scrollbar type amount)
  (validate-scrollbar-type type)
  (when (< amount 0)
    (warn 'gfs:toolkit-warning :detail "negative scrollbar page increment")
    (return-from sb-set-page-increment 0))
  (if (gfs:disposed-p scrollbar)
    (error 'gfs:disposed-error))
  (let ((hwnd (gfs:handle scrollbar)))
    (cffi:with-foreign-object (info-ptr 'gfs::scrollinfo)
      (gfs::zero-mem info-ptr gfs::scrollinfo)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::fmask gfs::pagesize)
                                info-ptr gfs::scrollinfo)
        (setf gfs::cbsize   (cffi:foreign-type-size 'gfs::scrollinfo)
              gfs::fmask    gfs::+sif-page+
              gfs::pagesize amount))
      (gfs::set-scroll-info hwnd type info-ptr 1)))
  amount)

(defun sb-set-thumb-limit (scrollbar type limit)
  (when (< limit 0)
    (warn 'gfs:toolkit-warning :detail "negative scrollbar limit")
    (return-from sb-set-thumb-limit nil))
  (if (gfs:disposed-p scrollbar)
    (error 'gfs:disposed-error))
  (let ((hwnd (gfs:handle scrollbar)))
    (cffi:with-foreign-object (info-ptr 'gfs::scrollinfo)
      (gfs::zero-mem info-ptr gfs::scrollinfo)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::fmask gfs::maxpos gfs::minpos)
                                info-ptr gfs::scrollinfo)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::scrollinfo)
              gfs::fmask  gfs::+sif-range+
              gfs::minpos 0
              gfs::maxpos limit))
      (gfs::set-scroll-info hwnd type info-ptr 1)))
  limit)

(defun sb-set-thumb-position (scrollbar type position)
  (when (< position 0)
    (warn 'gfs:toolkit-warning :detail "negative scrollbar position")
    (return-from sb-set-thumb-position 0))
  ;;
  ;; TODO: should check position against limit, but doing that
  ;; is not cheap, whereas the application will be calling this
  ;; method frequently to maintain the scrollbar's position;
  ;; more thought needed.
  ;;
  (if (gfs:disposed-p scrollbar)
    (error 'gfs:disposed-error))
  (let ((hwnd (gfs:handle scrollbar)))
    (cffi:with-foreign-object (info-ptr 'gfs::scrollinfo)
      (gfs::zero-mem info-ptr gfs::scrollinfo)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::fmask gfs::pos)
                                info-ptr gfs::scrollinfo)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::scrollinfo)
              gfs::fmask  gfs::+sif-pos+
              gfs::pos    position))
      (gfs::set-scroll-info hwnd type info-ptr 1)))
  position)

;;;
;;; standard scrollbar implementation
;;;

(defmethod gfs:dispose ((self standard-scrollbar))
  (setf (slot-value self 'gfs:handle) nil))

(defmethod initialize-instance :after ((self standard-scrollbar) &key)
  (if (gfs:null-handle-p (gfs:handle self))
    (error 'gfs:disposed-error))
  (let ((orient (orientation-of self)))
    (unless (or (= orient gfs::+sb-horz+) (= orient gfs::+sb-vert+))
      (error 'gfs:toolkit-error :detail "invalid standard scrollbar orientation")))
  (setf (slot-value self 'dispatcher) nil)) ; standard scrollbars don't use dispatchers

(defmethod outer-limit ((self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self (orientation-of self))
    (declare (ignore pagesize pos trackpos))
    limit))

(defmethod (setf outer-limit) (limit (self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (sb-set-thumb-limit self (orientation-of self) limit))

(defmethod owner ((self standard-scrollbar))
  (parent self))

(defmethod page-increment ((self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self (orientation-of self))
    (declare (ignore limit pos trackpos))
    pagesize))

(defmethod (setf page-increment) (amount (self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (sb-set-page-increment self (orientation-of self) amount))

(defmethod parent ((self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((parent (get-widget (thread-context) (gfs:handle self))))
    (unless parent
      (error 'gfs:toolkit-error :detail "missing parent for standard scrollbar"))
    parent))

(defmethod step-increment ((self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((disp (dispatcher (parent self))))
    (cond
      ((typep disp 'scrolling-helper)
         (if (eql (orientation-of self) :horizontal)
           (gfs:size-width (step-increments self))
           (gfs:size-height (step-increments self))))
      (t
         (warn 'gfs:toolkit-warning :detail "parent dispatcher is wrong type")
         0))))

(defmethod (setf step-increment) (amount (self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (>= amount 0)
    (warn 'gfs:toolkit-warning :detail "negative step increment"))
  (let ((disp (dispatcher (parent self))))
    (cond
      ((typep disp 'scrolling-helper)
         (if (eql (orientation-of self) :horizontal)
           (setf (gfs:size-width (step-increments self)) amount)
           (setf (gfs:size-height (step-increments self)) amount)))
      (t
         (warn 'gfs:toolkit-warning :detail "parent dispatcher is wrong type")))))

(defmethod thumb-position ((self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self (orientation-of self))
    (declare (ignore limit pagesize trackpos))
    pos))

(defmethod (setf thumb-position) (position (self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (sb-set-thumb-position self (orientation-of self) position))

(defmethod thumb-track-position ((self standard-scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self (orientation-of self))
    (declare (ignore limit pagesize pos))
    trackpos))

;;;
;;; scrollbar control implementation
;;;

(defmethod compute-style-flags ((self scrollbar) &rest extra-data)
  (declare (ignore extra-data))
  (let ((std-flags +default-child-style+)
        (style (style-of self)))
    (loop for sym in style
          do (ecase sym
               (:horizontal (setf std-flags (sb-horizontal-flags std-flags)))
               (:vertical   (setf std-flags (sb-vertical-flags std-flags)))))
    (values std-flags 0)))

(defmethod initialize-instance :after ((self scrollbar) &key outer-limit page-increment parent &allow-other-keys)
  (create-control self parent "" gfs::+icc-standard-classes+)
  (if outer-limit
    (setf (outer-limit self) outer-limit))
  (if page-increment
    (setf (page-increment self) page-increment)))

(defmethod outer-limit ((self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self gfs::+sb-ctl+)
    (declare (ignore pagesize pos trackpos))
    limit))

(defmethod (setf outer-limit) (span (self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (sb-set-thumb-limit self gfs::+sb-ctl+ span))

(defmethod owner ((self scrollbar))
  (parent self))

(defmethod page-increment ((self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self gfs::+sb-ctl+)
    (declare (ignore limit pos trackpos))
    pagesize))

(defmethod (setf page-increment) (amount (self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (sb-set-page-increment self gfs::+sb-ctl+ amount))

(defmethod preferred-size ((self scrollbar) width-hint height-hint)
  (let ((size (gfs:make-size)))
    (if (find :vertical (style-of self))
      (setf (gfs:size-width size)  (vertical-scrollbar-width)
            (gfs:size-height size) +default-widget-height+)
      (setf (gfs:size-width size)  +default-widget-width+
            (gfs:size-height size) (horizontal-scrollbar-height)))
    (if (>= width-hint 0)
      (setf (gfs:size-width size) width-hint))
    (if (>= height-hint 0)
      (setf (gfs:size-height size) height-hint))
    size))

(defmethod thumb-position ((self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self gfs::+sb-ctl+)
    (declare (ignore limit pagesize trackpos))
    pos))

(defmethod (setf thumb-position) (position (self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (sb-set-thumb-position self gfs::+sb-ctl+ position))

(defmethod thumb-track-position ((self scrollbar))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (destructuring-bind (limit pagesize pos trackpos)
      (sb-get-info self gfs::+sb-ctl+)
    (declare (ignore limit pagesize pos))
    trackpos))
