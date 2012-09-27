;;;;
;;;; graphics-context.lisp
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

(in-package :graphic-forms.uitoolkit.graphics)

;;;
;;; helper functions
;;;

(defun compute-pen-style (style width)
  (let ((main-styles (list (cons :alternate  gfs::+ps-alternate+)
                           (cons :dash       gfs::+ps-dash+)
                           (cons :dashdotdot gfs::+ps-dashdotdot+)
                           (cons :dot        gfs::+ps-dot+)
                           (cons :solid      gfs::+ps-solid+)))
        (endcap-styles (list (cons :flat-endcap   gfs::+ps-endcap-flat+)
                             (cons :round-endcap  gfs::+ps-endcap-round+)
                             (cons :square-endcap gfs::+ps-endcap-square+)))
        (join-styles (list (cons :bevel-join gfs::+ps-join-bevel+)
                           (cons :miter-join gfs::+ps-join-miter+)
                           (cons :round-join gfs::+ps-join-round+)))
        (native-style (if (> width 1) gfs::+ps-geometric+ gfs::+ps-cosmetic+))
        (tmp nil))
    (if (null style)
      (return-from compute-pen-style (logior gfs::+ps-cosmetic+ gfs::+ps-null+)))
    (setf tmp (intersection style (mapcar #'first main-styles)))
    (if (/= (length tmp) 1)
      (error 'gfs:toolkit-error :detail "main pen style keyword [:alternate | :dash | :dashdotdot | :dot | :solid] is required"))
    (setf native-style (logior native-style (cdr (assoc (car tmp) main-styles))))
    (setf tmp (intersection style (mapcar #'first endcap-styles)))
    (if (> (length tmp) 1)
      (error 'gfs:toolkit-error :detail "only one end cap pen style keyword [:flat-endcap | :round-endcap | :square-endcap] is allowed"))
    (setf native-style (logior native-style (if tmp
                                              (cdr (assoc (car tmp) endcap-styles)) 0)))
    (unless (null tmp)
      (setf native-style (logior (logand native-style (lognot gfs::+ps-cosmetic+))
                                 gfs::+ps-geometric+)))
    (setf tmp (intersection style (mapcar #'first join-styles)))
    (if (> (length tmp) 1)
      (error 'gfs:toolkit-error :detail "only one join pen style keyword [:bevel-join | :miter-join | :round-join] is allowed"))
    (setf native-style (logior native-style (if tmp
                                              (cdr (assoc (car tmp) join-styles)) 0)))
    (unless (null tmp)
      (setf native-style (logior (logand native-style (lognot gfs::+ps-cosmetic+))
                         gfs::+ps-geometric+)))
    native-style))

(defun update-pen-for-gc (gc)
  (cffi:with-foreign-object (lb-ptr 'gfs::logbrush)
    (cffi:with-foreign-slots ((gfs::style gfs::color gfs::hatch) lb-ptr gfs::logbrush)
      (setf gfs::style (logbrush-style-of gc))
      (setf gfs::color (logbrush-color-of gc))
      (setf gfs::hatch (logbrush-hatch-of gc))
      (let ((old-hpen (cffi:null-pointer))
            (new-hpen (gfs::ext-create-pen (compute-pen-style (pen-style gc) (pen-width gc))
                                           (pen-width gc)
                                           lb-ptr 0
                                           (cffi:null-pointer))))
        (if (gfs:null-handle-p new-hpen)
          (error 'gfs:win32-error :detail "ext-create-pen failed"))
        (setf (pen-handle-of gc) new-hpen)
        (setf old-hpen (gfs::select-object (gfs:handle gc) new-hpen))
        (gfs::set-miter-limit (gfs:handle gc) (miter-limit gc) (cffi:null-pointer))
        (unless (gfs:null-handle-p old-hpen)
          (gfs::delete-object old-hpen))))))

(defun call-rect-function (fn name hdc rect)
  (let ((pnt (gfs:location rect))
        (size (gfs:size rect)))
    (if (zerop (funcall fn
                        hdc
                        (gfs:point-x pnt)
                        (gfs:point-y pnt)
                        (+ (gfs:point-x pnt) (gfs:size-width size))
                        (+ (gfs:point-y pnt) (gfs:size-height size))))
      (error 'gfs:toolkit-error :detail (format nil "~a failed" name)))))

(defun call-rounded-rect-function (fn name hdc rect arc-size)
  (let ((pnt (gfs:location rect))
        (size (gfs:size rect)))
    (if (zerop (funcall fn
                        hdc
                        (gfs:point-x pnt)
                        (gfs:point-y pnt)
                        (+ (gfs:point-x pnt) (gfs:size-width size))
                        (+ (gfs:point-y pnt) (gfs:size-height size))
                        (gfs:size-width arc-size)
                        (gfs:size-height arc-size)))
      (error 'gfs:toolkit-error :detail (format nil "~a failed" name)))))

(defun call-rect-and-range-function (fn name hdc rect start-pnt end-pnt)
  (let ((rect-pnt (gfs:location rect))
        (rect-size (gfs:size rect)))
    (if (zerop (funcall fn
                        hdc
                        (gfs:point-x rect-pnt)
                        (gfs:point-y rect-pnt)
                        (+ (gfs:point-x rect-pnt) (gfs:size-width rect-size))
                        (+ (gfs:point-y rect-pnt) (gfs:size-height rect-size))
                        (gfs:point-x start-pnt)
                        (gfs:point-y start-pnt)
                        (gfs:point-x end-pnt)
                        (gfs:point-y end-pnt)))
      (error 'gfs:win32-error :detail (format nil "~a failed" name)))))

(defun call-points-function (fn name hdc points)
  (let* ((count (length points))
         (array (cffi:foreign-alloc 'gfs::point :count count)))
    (unwind-protect
        (progn
          (loop for pnt in points
                with i = 0
                do (progn
                     (cffi:with-foreign-slots ((gfs::x gfs::y)
                                               (cffi:mem-aref array 'gfs::point i) gfs::point)
                       (setf gfs::x (gfs:point-x pnt))
                       (setf gfs::y (gfs:point-y pnt)))
                     (incf i)))
          (if (zerop (funcall fn hdc array count))
            (error 'gfs:win32-error :detail (format nil "~a failed" name))))
      (cffi:foreign-free array))))

(defun compute-draw-text-style (style)
  (let ((flags (logior gfs::+dt-noclip+ gfs::+dt-noprefix+ gfs::+dt-singleline+ gfs::+dt-vcenter+)))
    (unless (null style)
      (loop for sym in style
            do (cond
                 ((eq sym :mnemonic)
                    (setf flags (logand flags (lognot gfs::+dt-noprefix+))))
                 ((eq sym :tab)
                    (setf flags (logior flags gfs::+dt-expandtabs+)))
                  ;; FIXME: the :transparent style needs to be implemented
                  ;;
                 ((eq sym :transparent)))))
    flags))

(defun text-bounds (hdc str dt-flags tab-width)
  (let ((len (length str))
        (sz (gfs:make-size)))
    (when (> len 0)
      (cffi:with-foreign-object (dt-ptr 'gfs::drawtextparams)
        (cffi:with-foreign-slots ((gfs::cbsize gfs::tablength gfs::leftmargin gfs::rightmargin)
                                  dt-ptr gfs::drawtextparams)
          (setf gfs::cbsize (cffi:foreign-type-size 'gfs::drawtextparams))
          (setf gfs::tablength tab-width)
          (setf gfs::leftmargin 0)
          (setf gfs::rightmargin 0)
          (gfs::with-rect (rect-ptr)
            (gfs::draw-text-ex hdc str -1 rect-ptr (logior dt-flags gfs::+dt-calcrect+) dt-ptr)
            (setf (gfs:size-width sz) (- gfs::right gfs::left))
            (setf (gfs:size-height sz) (- gfs::bottom gfs::top))))))
    (when (or (zerop len) (zerop (gfs:size-height sz)))
      (cffi:with-foreign-object (tm-ptr 'gfs::textmetrics)
        (cffi:with-foreign-slots ((gfs::tmheight gfs::tmexternalleading) tm-ptr gfs::textmetrics)
          (if (zerop (gfs::get-text-metrics hdc tm-ptr))
            (error 'gfs:win32-error :detail "get-text-metrics failed"))
          (setf (gfs:size-height sz) (+ gfs::tmheight gfs::tmexternalleading)))))
    sz))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-null-brush ((gc) &body body)
    (let ((hdc (gensym))
          (tmp-hbr (gensym))
          (orig-hbr (gensym)))
      `(let* ((,hdc (gfs:handle ,gc))
              (,tmp-hbr (gfs::get-stock-object gfs::+null-brush+))
              (,orig-hbr (gfs::select-object ,hdc ,tmp-hbr)))
         (unwind-protect
             (progn
               ,@body)
           (gfs::select-object ,hdc ,orig-hbr))))))

;;;
;;; methods
;;;

(defmethod background-color ((self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (rgb->color (gfs::get-bk-color (gfs:handle self))))

(defmethod (setf background-color) ((clr color) (self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((hdc (gfs:handle self))
        (hbrush (gfs::get-stock-object gfs::+dc-brush+))
        (rgb (color->rgb clr)))
    (gfs::select-object hdc hbrush)
    (gfs::set-dc-brush-color hdc rgb)
    (gfs::set-bk-color hdc rgb)))

(defmethod clear ((self graphics-context) (color color))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (setf (background-color self) color
        (foreground-color self) color)
  (let ((hdc (gfs:handle self))
        (size (surface-size-of self)))
    (if size
            (gfs::with-rect (rect-ptr)
              (setf gfs::top    0
                    gfs::left   0
              gfs::right  (gfs:size-width size)
              gfs::bottom (gfs:size-height size))
        (gfs::ext-text-out hdc 0 0 gfs::+eto-opaque+ rect-ptr "" 0 (cffi:null-pointer)))
      (warn 'gfs:toolkit-warning :detail "null surface size"))))

(defmethod gfs:dispose ((self graphics-context))
  (gfs::select-object (gfs:handle self) (gfs::get-stock-object gfs::+null-pen+))
  (gfs::delete-object (pen-handle-of self))
  (setf (pen-handle-of self) nil)
  (let ((fn (dc-destructor-of self)))
    (unless (null fn)
      (if (null (widget-handle-of self))
        (funcall fn (gfs:handle self))
        (funcall fn (widget-handle-of self) (gfs:handle self)))))
  (setf (surface-size-of self) nil)
  (setf (widget-handle-of self) nil)
  (setf (slot-value self 'gfs:handle) (cffi:null-pointer)))

(defmethod draw-arc ((self graphics-context) rect start-pnt end-pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-rect-and-range-function #'gfs::arc "arc" (gfs:handle self) rect start-pnt end-pnt))

(defmethod draw-bezier ((self graphics-context) start-pnt end-pnt ctrl-pnt-1 ctrl-pnt-2)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-points-function #'gfs::poly-bezier
                        "poly-bezier"
                        (gfs:handle self)
                        (list start-pnt ctrl-pnt-1 ctrl-pnt-2 end-pnt)))

(defmethod draw-chord ((self graphics-context) rect start-pnt end-pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (with-null-brush (self)
    (call-rect-and-range-function #'gfs::chord "chord" (gfs:handle self) rect start-pnt end-pnt)))

(defmethod draw-ellipse ((self graphics-context) rect)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (with-null-brush (self)
    (call-rect-function #'gfs::ellipse "ellipse" (gfs:handle self) rect)))

(defmethod draw-filled-chord ((self graphics-context) rect start-pnt end-pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-rect-and-range-function #'gfs::chord "chord" (gfs:handle self) rect start-pnt end-pnt))

(defmethod draw-filled-ellipse ((self graphics-context) rect)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-rect-function #'gfs::ellipse "ellipse" (gfs:handle self) rect))

(defmethod draw-filled-pie-wedge ((self graphics-context) rect start-pnt end-pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-rect-and-range-function #'gfs::pie "pie" (gfs:handle self) rect start-pnt end-pnt))

(defmethod draw-filled-polygon ((self graphics-context) points)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (< (length points) 3)
    (call-points-function #'gfs::polygon "polygon" (gfs:handle self) points)))

(defmethod draw-filled-rectangle ((self graphics-context) (rect gfs:rectangle))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-rect-function #'gfs::rectangle "rectangle" (gfs:handle self) rect))

(defmethod draw-filled-rounded-rectangle ((self graphics-context) rect size)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-rounded-rect-function #'gfs::round-rect "round-rect" (gfs:handle self) rect size))

;;;
;;; TODO: support addressing elements within bitmap as if it were an array
;;;
(defmethod draw-image ((self graphics-context) (im image) (pnt gfs:point))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (if (gfs:disposed-p im)
    (error 'gfs:disposed-error))
  (let ((gc-dc (gfs:handle self))
        (himage (gfs:handle im))
        (tr-mask nil)
        (memdc (gfs::create-compatible-dc (cffi:null-pointer))))
    (cffi:with-foreign-object (bmp-ptr 'gfs::bitmap)
      (cffi:with-foreign-slots ((gfs::width gfs::height) bmp-ptr gfs::bitmap)
        (gfs::get-object himage (cffi:foreign-type-size 'gfs::bitmap) bmp-ptr)
        (if (transparency-pixel-of im)
          (progn
            (setf tr-mask (transparency-mask im))
            (let ((hmask (gfs:handle tr-mask))
                  (hcopy (clone-bitmap himage))
                  (memdc2 (gfs::create-compatible-dc (cffi:null-pointer)))
                  (black (make-color :red 0 :green 0 :blue 0))
                  (white (make-color :red #xFF :green #xFF :blue #xFF)))
              (gfs::select-object memdc hmask)
              (gfs::select-object memdc2 hcopy)
              (gfs::set-bk-color memdc2 (color->rgb black))
              (gfs::set-text-color memdc2 (color->rgb white))
              (gfs::bit-blt memdc2
                            0 0
                            gfs::width
                            gfs::height
                            memdc
                            0 0 gfs::+blt-srcand+)
              (gfs::bit-blt gc-dc
                            (gfs:point-x pnt)
                            (gfs:point-y pnt)
                            gfs::width
                            gfs::height
                            memdc
                            0 0 gfs::+blt-srcand+)
              (gfs::bit-blt gc-dc
                            (gfs:point-x pnt)
                            (gfs:point-y pnt)
                            gfs::width
                            gfs::height
                            memdc2
                            0 0 gfs::+blt-srcpaint+)
              (gfs::delete-dc memdc2)
              (gfs::delete-object hcopy))
            (gfs:dispose tr-mask))
          (progn
            (gfs::select-object memdc himage)
            (gfs::bit-blt gc-dc
                          (gfs:point-x pnt)
                          (gfs:point-y pnt)
                          gfs::width
                          gfs::height
                          memdc
                          0 0 gfs::+blt-srccopy+)))))
    (gfs::delete-dc memdc)))

(defmethod draw-line ((self graphics-context) start-pnt end-pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (call-points-function #'gfs::polyline "polyline" (gfs:handle self) (list start-pnt end-pnt)))

(defmethod draw-pie-wedge ((self graphics-context) rect start-pnt end-pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (with-null-brush (self)
    (call-rect-and-range-function #'gfs::pie "pie" (gfs:handle self) rect start-pnt end-pnt)))

(defmethod draw-point ((self graphics-context) pnt)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::set-pixel (gfs:handle self)
                  (gfs:point-x pnt)
                  (gfs:point-y pnt)
                  (color->rgb (foreground-color self))))

(defmethod draw-poly-bezier ((self graphics-context) start-pnt points)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (null points)
    (let ((tmp (loop for triplet in points
                     append (list (second triplet) (third triplet) (first triplet)))))
      (push start-pnt tmp)
      (call-points-function #'gfs::poly-bezier "poly-bezier" (gfs:handle self) tmp))))

(defmethod draw-polygon ((self graphics-context) points)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (< (length points) 3)
    (with-null-brush (self)
      (call-points-function #'gfs::polygon "polygon" (gfs:handle self) points))))

(defmethod draw-polyline ((self graphics-context) points)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (unless (< (length points) 2)
    (call-points-function #'gfs::polyline "polyline" (gfs:handle self) points)))

(defmethod draw-rectangle ((self graphics-context) rect)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (with-null-brush (self)
    (call-rect-function #'gfs::rectangle "rectangle" (gfs:handle self) rect)))

(defmethod draw-rounded-rectangle ((self graphics-context) rect size)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (with-null-brush (self)
    (call-rounded-rect-function #'gfs::round-rect "round-rect" (gfs:handle self) rect size)))

(defmethod draw-text ((self graphics-context) text (pnt gfs:point) &optional style tab-width)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((flags (compute-draw-text-style style))
        (tb-width (if (null tab-width) 0 tab-width))
        (old-bk-mode (gfs::get-bk-mode (gfs:handle self))))
    (if (find :transparent style)
      (gfs::set-bk-mode (gfs:handle self) gfs::+transparent+))
    (cffi:with-foreign-object (dt-ptr 'gfs::drawtextparams)
      (cffi:with-foreign-slots ((gfs::cbsize gfs::tablength gfs::leftmargin gfs::rightmargin)
                                dt-ptr gfs::drawtextparams)
        (setf gfs::cbsize (cffi:foreign-type-size 'gfs::drawtextparams))
        (setf gfs::tablength tb-width)
        (setf gfs::leftmargin 0)
        (setf gfs::rightmargin 0)
        (gfs::with-rect (rect-ptr)
          (setf gfs::left (gfs:point-x pnt))
          (setf gfs::top (gfs:point-y pnt))
          (gfs::draw-text-ex (gfs:handle self)
                             text
                             -1
                             rect-ptr
                             (logior gfs::+dt-calcrect+ (logand flags (lognot gfs::+dt-vcenter+)))
                             dt-ptr)
          (gfs::draw-text-ex (gfs:handle self)
                             text
                             (length text)
                             rect-ptr
                             flags
                             dt-ptr)
          (gfs::set-bk-mode (gfs:handle self) old-bk-mode))))))

(defmethod (setf font) ((font font) (self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (gfs::select-object (gfs:handle self) (gfs:handle font)))

(defmethod foreground-color ((self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (rgb->color (gfs::get-text-color (gfs:handle self))))

(defmethod (setf foreground-color) ((clr color) (self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (let ((rgb (color->rgb clr)))
    (gfs::set-text-color (gfs:handle self) rgb)
    (setf (logbrush-color-of self) rgb)
    (update-pen-for-gc self)))

(defmethod initialize-instance :after ((self graphics-context) &key image widget &allow-other-keys)
  (when (null (gfs:handle self))
    (let ((hdc (cffi:null-pointer)))
      (if (null widget)
        (progn
          (setf hdc (gfs::create-compatible-dc (cffi:null-pointer)))
          (setf (dc-destructor-of self) #'gfs::delete-dc))
        (progn
          (setf hdc (gfs::get-dc (gfs:handle widget)))
          (setf (dc-destructor-of self) #'gfs::release-dc)
          (setf (widget-handle-of self) (gfs:handle widget))
          (setf (surface-size-of self) (gfw:client-size widget))))
      (setf (slot-value self 'gfs:handle) hdc)
      (unless (null image)
        (setf (surface-size-of self) (gfg:size image))
        (gfs::select-object hdc (gfs:handle image)))))
  ;; ensure world-to-device transformation conformance
  (gfs::set-graphics-mode (gfs:handle self) gfs::+gm-advanced+)
  (update-pen-for-gc self))

(defmethod metrics ((self graphics-context) (font font))
  (if (or (gfs:disposed-p self) (gfs:disposed-p font))
    (error 'gfs:disposed-error))
  (let ((hdc (gfs:handle self))
        (hfont (gfs:handle font))
        (metrics nil))
    (gfs::with-hfont-selected (hdc hfont)
      (cffi:with-foreign-object (tm-ptr 'gfs::textmetrics)
        (cffi:with-foreign-slots ((gfs::tmascent gfs::tmdescent gfs::tmexternalleading
                                   gfs::tmavgcharwidth gfs::tmmaxcharwidth)
                                  tm-ptr gfs::textmetrics)
          (if (zerop (gfs::get-text-metrics hdc tm-ptr))
            (error 'gfs:win32-error :detail "get-text-metrics failed"))
          (setf metrics (make-font-metrics :ascent gfs::tmascent
                                           :descent gfs::tmdescent
                                           :leading gfs::tmexternalleading
                                           :avg-char-width gfs::tmavgcharwidth
                                           :max-char-width gfs::tmmaxcharwidth)))))
    metrics))

(defmethod (setf pen-style) :around (style (self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (setf (slot-value self 'pen-style) style)
  (update-pen-for-gc self))

(defmethod (setf pen-width) :around (width (self graphics-context))
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (setf (slot-value self 'pen-width) width)
  (update-pen-for-gc self))

(defmethod text-extent ((self graphics-context) str &optional style tab-width)
  (if (gfs:disposed-p self)
    (error 'gfs:disposed-error))
  (text-bounds (gfs:handle self)
               str
               (compute-draw-text-style style)
               (if (or (null tab-width) (< tab-width 0)) 0 tab-width)))
