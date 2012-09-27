;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOSURE/CLIM-DEVICE; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM device for the renderer
;;;   Created: ???
;;;    Author: Gilbert Baumann <gilbert@base-engineering.com>
;;;   License: MIT style (see below)
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Gilbert Baumann

;;;  Permission is hereby granted, free of charge, to any person obtaining
;;;  a copy of this software and associated documentation files (the
;;;  "Software"), to deal in the Software without restriction, including
;;;  without limitation the rights to use, copy, modify, merge, publish,
;;;  distribute, sublicense, and/or sell copies of the Software, and to
;;;  permit persons to whom the Software is furnished to do so, subject to
;;;  the following conditions:
;;; 
;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.
;;; 
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :CLOSURE/CLIM-DEVICE)

(defclass clim-device ()
  ((medium :accessor clim-device-medium :initarg :medium)
   (font-database :initform nil)
   (zoom-factor :initform gui:*zoom-factor* :initarg :zoom-factor)))

(defmethod device-dpi ((device clim-device))
  (with-slots (zoom-factor) device
    (* (graft-pixels-per-inch (graft (clim-device-medium device)))
       zoom-factor)))

(defmethod device-font-ascent ((device clim-device) font)
  (text-style-ascent font (clim-device-medium device))
  )

(defmethod device-font-descent ((device clim-device) font)
  (text-style-descent font (clim-device-medium device))
  )

(defmethod device-font-underline-position ((self clim-device) font)
  0
  )

(defmethod device-font-underline-thickness ((self clim-device) font)
  0
  )

(defmethod device-font-has-glyph-p ((self clim-device) font code-point)
  (<= 0 code-point 255)                 ;hmm
  )

(defresource one-character-string (char)
  :constructor (make-string 1)
  :initializer (setf (aref one-character-string 0) char)
  :matcher t)

(defmethod device-font-glyph-width ((self clim-device) font code-point)
  (using-resource (string one-character-string (code-char code-point))
     (text-size (clim-device-medium self) string  :text-style font)))

(defmethod scale-font-desc ((self clim-device) font-desc size)
  font-desc)

(defmethod device-realize-font-desc ((self clim-device) font-desc)
  (font-desc-ddp font-desc))

(defmethod device-font-database ((self clim-device))
  (with-slots (font-database) self
    (or font-database
        (setf font-database
              (let ((fdb (make-font-database ;xxx
                          :cache (make-hash-table :test #'equal)
                          :device self)))
                (loop
                    for family      in '("Times" "Helvetica" "Courier")
                    for clim-family in '(:serif  :sans-serif :fix) 
                    for size-adjust in '(-2 -2 -2) do 
                    (loop
                        for weight+style in '((400 :normal) (400 :italic) (700 :normal) (700 :italic))
                        for clim-face    in '(:roman       :italic       :bold         (:bold :italic))  do
                        (loop
                            for size in '(8 10 12 14 18 24) do
                            (font-database-relate fdb
                                                  (make-font-desc
                                                   :family family
                                                   :weight (first weight+style)
                                                   :style  (second weight+style)
                                                   :size   (+ size size-adjust)
                                                   :ddp    (make-text-style clim-family clim-face size)
                                                   :charset (ws/charset:find-charset :iso-8859-1) ;xxx
                                                   )))))
                fdb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod scale-font-desc ((self clim-device) font-desc size)
  (let ((r (r2::copy-font-desc font-desc)))
    (incf size
          (if (eql (nth-value 0 (text-style-components (font-desc-ddp r)))
                   :sans-serif)
              0 ;;-2
              0))
    (setf (font-desc-size r) size
          (font-desc-ddp  r) (make-text-style (nth-value 0 (text-style-components (font-desc-ddp r)))
                                              (nth-value 1 (text-style-components (font-desc-ddp r)))
                                              size))
    r))

(defmethod device-realize-font-desc ((self clim-device) font-desc)
  (font-desc-ddp font-desc))

(defmethod device-font-database ((self clim-device))
  (with-slots (font-database) self
    (or font-database
        (setf font-database
              (let ((fdb (make-font-database ;xxx
                          :cache (make-hash-table :test #'equal)
                          :device self)))
                (loop
                    for family      in '("Times" "Helvetica" "Courier")
                    for clim-family in '(:serif  :sans-serif :fix) 
                    for size-adjust in '(-2 0 -2) do 
                    (loop
                        for weight+style in '((400 :normal) (400 :italic) (700 :normal) (700 :italic))
                        for clim-face    in '(:roman       :italic       :bold         (:bold :italic))  do
                        (font-database-relate fdb
                                              (make-font-desc
                                               :family family
                                               :weight (first weight+style)
                                               :style  (second weight+style)
                                               :size   0
                                               :ddp    (make-text-style clim-family clim-face 12)
                                               :charset (ws/charset:find-charset :iso-8859-1) ;xxx
                                               ))))
                fdb)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *buffer* (make-array 1000))

(defun resolve-background-position (spec image-dim box-dim)
  (cond ((css:percentage-p spec)
         (let ((i (r2::maybe-resolve-percentage spec image-dim))
               (b (r2::maybe-resolve-percentage spec box-dim)))
           (- b i)))
        ((null spec)
         (warn "~S: null spec." 'resolve-background-position)
         0)
        (t
         spec)))

;; newer definition below
;;;(defun background-pixmap+mask (document drawable bg)
;;;  (cond ((r2::background-%pixmap bg)
;;;         (values (r2::background-%pixmap bg)
;;;                 (r2::background-%mask bg)))
;;;        (t
;;;         (setf (r2::background-%pixmap bg) :none)
;;;         (funcall ;;r2::run-process-on-behalf-of-document document
;;;          (lambda ()  
;;;            (let ((aimage (clue-gui2::aimage-from-url document (r2::background-image bg))))
;;;              (cond ((eq aimage :error)
;;;                     (setf (r2::background-%pixmap bg) :none)
;;;                     (values (r2::background-%pixmap bg)
;;;                             (r2::background-%mask bg)))
;;;                    (t
;;;                     (let ((pm (ws/x11::aimage->pixmap+mask drawable aimage)))
;;;                       (setf (r2::background-%pixmap bg) (car pm)
;;;                             (r2::background-%mask bg) (cadr pm))
;;;                       #+NIL
;;;                       (clue-gui2::gui-post
;;;                        nil
;;;                        ;; we do it the hard way via an exposure round trip.
;;;                        'xlib:clear-area
;;;                        drawable
;;;                        :exposures-p t))))))
;;;          ;;:name "Lazy Document background fetch."
;;;          )
;;;         (values (r2::background-%pixmap bg)
;;;                 (r2::background-%mask bg)))))

;; apparently unused  --dfl
;;;(defmethod update-lazy-object (document (self null))
;;;  nil)

(defun map-region-rectangles (fun region)
  (clim:map-over-region-set-regions
   (lambda (r)
     (apply fun (mapcar #'round
                        (multiple-value-list (clim:rectangle-edges* r)))))
   region
   :normalize :y-banding))

(defun region-to-x11-rectangle-list (region)
  (let ((res nil))
    (map-region-rectangles (lambda (x1 y1 x2 y2)
                             (push (- y2 y1) res)
                             (push (- x2 x1) res)
                             (push y1 res)
                             (push x1 res))
                           region)
    (mapcar #'round res)))

(defun region-from-x11-rectangle-list (rectangle-sequence)
  (let ((res clim:+nowhere+))
    (do ((q rectangle-sequence (nthcdr 4 q)))
        ((endp q))
      (setf res (region-union res
                              (make-rectangle* (first q) (second q)
                                               (+ (first q) (third q))
                                               (+ (second q) (fourth q))))))
    res))

;; apparently unused  --dfl

;;;(defun background-pixmap+mask (document drawable bg)
;;;  (cond ((r2::background-%pixmap bg)
;;;         ;; already there
;;;         (values (r2::background-%pixmap bg)
;;;                 (r2::background-%mask bg)))
;;;        (t
;;;         (let ((aimage #+NIL(r2::document-fetch-image document nil (r2::background-image bg))
;;;                       (r2::url->aimage document (r2::background-image bg) nil)
;;;                       ))
;;;           ;; arg, jetzt haben wir wieder broken images
;;;           (cond ((eql nil aimage)
;;;                  (values :none))
;;;                 (t
;;;                  (cond ((eq aimage :error)
;;;                         (setf (r2::background-%pixmap bg) :none) )
;;;                        (t
;;;                         (let ((pm (ws/x11::aimage->pixmap+mask drawable aimage)))
;;;                           (setf (r2::background-%pixmap bg) (car pm)
;;;                                 (r2::background-%mask bg) (cadr pm)))))
;;;                  (values (r2::background-%pixmap bg)
;;;                          (r2::background-%mask bg)))))) ))

(defun ws/x11::x11-put-pixmap-tiled (drawable ggc pixmap mask x y w h &optional (xo 0) (yo 0))
  (cond ((null mask) ;; xxx
         (xlib:with-gcontext (ggc :exposures :off
                                  :fill-style :tiled
                                  :tile pixmap
                                  :ts-x xo
                                  :ts-y yo)
           ;;mask wird momentan noch ignoriert!
           (xlib:draw-rectangle drawable ggc x y w h t)))
        (t
         (let* ((old-clip-mask (car (or (ignore-errors (list (xlib:gcontext-clip-mask ggc)))
                                        (list :none))))
                (clip-region (let ((q old-clip-mask))
                               (if (consp q)
                                   (region-from-x11-rectangle-list q)
                                   +everywhere+)))
                (paint-region (region-intersection 
                               clip-region
                               (make-rectangle* x y (+ x w) (+ y h)))) )
           ;; There is a bug in CLX wrt to clip-x / clip-y
           ;; Turning off caching helps
           (setf (xlib:gcontext-cache-p ggc) nil)

           ;; we have to do our own clipping here.
           (let ((iw (xlib:drawable-width pixmap))
                 (ih (xlib:drawable-height pixmap)))
             (loop for i from (floor (- x xo) iw) to (ceiling (- (+ x w) (+ xo iw)) iw)
                   do
                   (loop for j from (floor (- y yo) ih) to (ceiling (- (+ y h) (+ yo ih)) ih)
                         do
                         (let ((rect (make-rectangle*
                                      (+ xo (* i iw))
                                      (+ yo (* j ih))
                                      (+ (+ xo (* i iw)) iw)
                                      (+ (+ yo (* j ih)) ih))))
                           (map-region-rectangles 
                            (lambda (rx0 ry0 rx1 ry1)
                              (xlib:with-gcontext (ggc :exposures :off
                                                       :fill-style :tiled
                                                       :tile pixmap
                                                       :clip-mask mask
                                                       :clip-x (+ xo (* i iw))
                                                       :clip-y (+ yo (* j ih))
                                                       :ts-x xo
                                                       :ts-y yo)
                                (xlib:draw-rectangle drawable ggc
                                                     rx0 ry0 (max 0 (- rx1 rx0)) (max 0 (- ry1 ry0))
                                                     t)))
                            (region-intersection paint-region rect))))) )
           ;; turn on caching again (see above)
           (setf (xlib:gcontext-cache-p ggc) t)
           ;;
           ;; and xlib:with-gcontext also is broken!
           (setf (xlib:gcontext-clip-mask ggc) old-clip-mask)))))

#+emarsden
#.((lambda (x)
     #+:CMU `(eval ',x)                 ;compiler bug
     #-:CMU x)
   '(defun ws/x11::x11-put-pixmap-tiled (drawable ggc pixmap mask x y w h &optional (xo 0) (yo 0))
      (cond ((null mask) ;; xxx
             (xlib:with-gcontext (ggc :exposures :off
                                      :fill-style :tiled
                                      :tile pixmap
                                      :ts-x xo
                                      :ts-y yo)
                                 ;;mask wird momentan noch ignoriert!
                                 (xlib:draw-rectangle drawable ggc x y w h t)))
            (t
             (let* ((old-clip-mask (car (or (ignore-errors (list (xlib:gcontext-clip-mask ggc)))
                                            (list :none))))
                    (clip-region (let ((q old-clip-mask))
                                   (if (consp q)
                                       (region-from-x11-rectangle-list q)
                                     +everywhere+)))
                    (paint-region (region-intersection 
                                   clip-region
                                   (make-rectangle* x y (+ x w) (+ y h)))) )
               ;; There is a bug in CLX wrt to clip-x / clip-y
               ;; Turning off caching helps
               (setf (xlib:gcontext-cache-p ggc) nil)

               ;; we have to do our own clipping here.
               (let ((iw (xlib:drawable-width pixmap))
                     (ih (xlib:drawable-height pixmap)))
                 (loop for i from (floor (- x xo) iw) to (ceiling (- (+ x w) (+ xo iw)) iw)
                   do
                   (loop for j from (floor (- y yo) ih) to (ceiling (- (+ y h) (+ yo ih)) ih)
                     do
                     (let ((rect (make-rectangle*
                                  (+ xo (* i iw))
                                  (+ yo (* j ih))
                                  (+ (+ xo (* i iw)) iw)
                                  (+ (+ yo (* j ih)) ih))))
                       (map-region-rectangles 
                        (lambda (rx0 ry0 rx1 ry1)
                          (xlib:with-gcontext (ggc :exposures :off
                                                   :fill-style :tiled
                                                   :tile pixmap
                                                   :clip-mask mask
                                                   :clip-x (+ xo (* i iw))
                                                   :clip-y (+ yo (* j ih))
                                                   :ts-x xo
                                                   :ts-y yo)
                            (xlib:draw-rectangle drawable ggc
                                                 rx0 ry0 (max 0 (- rx1 rx0)) (max 0 (- ry1 ry0))
                                                 t)))
                        (region-intersection paint-region rect))))) )
               ;; turn on caching again (see above)
               (setf (xlib:gcontext-cache-p ggc) t)
               ;;
               ;; and xlib:with-gcontext also is broken!
               (setf (xlib:gcontext-clip-mask ggc) old-clip-mask))))))

;; apparently unused  --dfl

;;;(defun x11-draw-background (document medium bg x y width height
;;;                            &optional (bix x) (biy y) (biwidth width) (biheight height))
;;;  (when bg
;;;    ;; #+NIL
;;;    ;; (unless (eql (background-color bg) :transparent)
;;;    ;;   (ws/x11::fill-rectangle* drawable gcontext
;;;    ;;                            (round x) (round y) 
;;;    ;;                            (max 0 (round width))
;;;    ;;                            (max 0 (round height))
;;;    ;;                            (background-color bg)) )
;;;    (unless (eql (r2::background-image bg) :none)
;;;      (multiple-value-bind (pixmap mask)
;;;          (background-pixmap+mask document (sheet-direct-mirror (medium-sheet medium)) bg)
;;;        #+emarsden2005-07-15
;;;        (print (list 'x11-draw-background pixmap mask))
;;;        (unless (eql pixmap :none)
;;;          (let* ((iw (xlib:drawable-width pixmap))
;;;                 (ih (xlib:drawable-height pixmap))
;;;                 (w (ecase (r2::background-repeat bg)
;;;                      ((:repeat :repeat-x) width)
;;;                      ((:no-repeat :repeat-y) iw)))
;;;                 (h (ecase (r2::background-repeat bg)
;;;                      ((:repeat :repeat-y) height)
;;;                      ((:no-repeat :repeat-x) ih))) )
;;;            (let ((hp (car (r2::background-position bg)))
;;;                  (vp (cdr (r2::background-position bg))))
;;;              (let ((xo (+ bix (resolve-background-position hp iw biwidth)))
;;;                    (yo (+ biy (resolve-background-position vp ih biheight))))
;;;                (medium-draw-pm3-tiled* medium pixmap mask
;;;                               (round (ecase (r2::background-repeat bg)
;;;                                        ((:repeat :repeat-x) x)
;;;                                        ((:no-repeat :repeat-y) (+ xo))))
;;;                               (round (ecase (r2::background-repeat bg)
;;;                                        ((:repeat :repeat-y) y)
;;;                                        ((:no-repeat :repeat-x) (+ yo))))
;;;                               (round w) (round h)
;;;                               (round (+ xo)) (round (+ yo)))))) ))) ))

;;;; --------------------------------------------------------------------------------

(defclass ro/img ()
  ((url :initarg :url)
   (aim :initarg :aim)
   (actual-width :initarg :actual-width
                 :initform nil
                 :documentation "The actual (scaled) width of this image.")
   (actual-height :initarg :actual-height
                 :initform nil
                 :documentation "The actual (scaled) height of this image.")
   (design :initform nil)))

(defmethod gui::deconstruct-robj ((self ro/img))
  ;; no deconstructor for now ...
  )

(defmethod r2:ro/intrinsic-size ((self ro/img)) ;; -> width; height; depth
  (with-slots (aim) self
    (values (r2::aimage-width aim)
            (r2::aimage-height aim)
            0)))

(defmethod r2:ro/size ((self ro/img));; -> width; height; depth
  (with-slots (aim actual-width actual-height) self
    (values (or actual-width (r2::aimage-width aim))
            (or actual-height (r2::aimage-height aim))
            0)))

(defmethod r2:ro/resize ((self ro/img) new-width new-height)
  (with-slots (actual-width actual-height) self
    (setf actual-width new-width
          actual-height new-height)))

(defmethod gui::make-image-replacement ((device clim-device) document &key url width height)
  (let (aim)
    (cond (nil (and width height)
           ;; when width and height are known, we do not bother to fetch
           ;; the image _now_.
           )
          (t
           (setf aim (r2::url->aimage document url))
           (when (eql aim :error)
             (setf aim (renderer::broken-aimage document)))))
    (make-instance 'ro/img
                   :url url
                   :aim aim
                   :actual-width  (or width (r2::aimage-width aim))
                   :actual-height (or height (r2::aimage-height aim)))))

(climi::def-grecording draw-ro (() ro x y) ()
  (values x
	  (- y (nth-value 1 (r2::ro/size ro)))
	  (+ x (nth-value 0 (r2::ro/size ro)))
	  (+ y 0)))
(climi::def-graphic-op draw-ro (ro x y))

(defun draw-ro* (sheet ro x y &rest args)
  (climi::with-medium-options (sheet args)
    (medium-draw-ro* medium ro x y)))


(defmethod medium-draw-ro* ((medium clim:medium) (self ro/img) x y)
  (assert (realp x))
  (assert (realp y))
  (with-slots (aim design actual-width actual-height) self
    (when aim				;only draw something, if the image is already there.
      ;; xxx
      (when (and actual-width actual-height (not design)) ;xxx
	(setf design
	      (clue-gui2::make-design-from-aimage medium
						  aim
						  (max 1 (round actual-width))
						  (max 1 (round actual-height)))))
      (climi::medium-draw-image-design* medium design x y))))

;; apparently unused  --dfl
;;;#+NIL
;;;(climi::def-grecording draw-pm3-tiled (() pixmap mask x1 y1 w h x0 y0)
;;;                       (values x1 y1 (+ x1 w) (+ y1 h)))
;;;
;;;(defmethod medium-draw-pm3-tiled* (medium pixmap mask x1 y1 w h x0 y0)
;;;  (let* ((da (sheet-direct-mirror (medium-sheet medium)))
;;;         #+NIL
;;;         (pixmap+mask (clue-gui2::make-pixmap-from-aimage da aim
;;;                                                          (r2::aimage-width aim)
;;;                                                          (r2::aimage-height aim)))
;;;         #+NIL
;;;         (pixmap (first pixmap+mask))
;;;         #+NIL
;;;         (mask   (second pixmap+mask)))
;;;    (multiple-value-bind (x1 y1) (transform-position (sheet-device-transformation (medium-sheet medium))
;;;                                                     x1 y1)
;;;      (setf x1 (round x1))
;;;      (setf y1 (round y1))
;;;      ;;;
;;;      (let ((gcontext (xlib:create-gcontext :drawable da)))
;;;        (ws/x11::x11-put-pixmap-tiled da gcontext pixmap mask x1 y1 w h x0 y0) ))))


#+NIL
(setf clim-clx::*clx-text-family+face-map*
  '(:fix
    ("*-courier new"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-o"
      :bold-italic         "bold-o"
      :italic-bold         "bold-o"))
    :sans-serif
    ("*-verdana"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-i"
      :bold-italic         "bold-i"
      :italic-bold         "bold-i"))
    :serif
    ("*-times new roman"
     (:roman               "medium-r"
      :bold                "bold-r"
      :italic              "medium-i"
      :bold-italic         "bold-i"
      :italic-bold         "bold-i")) ))
