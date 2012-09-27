;;;;
;;;; color.lisp
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

(in-package :graphic-forms.uitoolkit.graphics)

(defvar *color-table*
#+clisp
  (make-hash-table :weak :value)
#+lispworks
  (make-hash-table :weak-kind :value)
#-(or clisp lispworks)
  (make-hash-table))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro color->rgb (color)
    (let ((tmp-color (gensym))
          (result (gensym)))
      `(let ((,tmp-color ,color)
             (,result 0))
         (setf (ldb (byte 8 0)  ,result) (color-red   ,tmp-color))
         (setf (ldb (byte 8 8)  ,result) (color-green ,tmp-color))
         (setf (ldb (byte 8 16) ,result) (color-blue  ,tmp-color))
         ,result)))

  (defmacro rgb->color (colorref)
    (let ((tmp-colorref (gensym)))
      `(let ((,tmp-colorref ,colorref))
         (make-color :red   (ldb (byte 8 0)  ,tmp-colorref)
                     :green (ldb (byte 8 8)  ,tmp-colorref)
                     :blue  (ldb (byte 8 16) ,tmp-colorref))))))

(defun obtain-color-from-table (name red green blue)
  (let ((color (gethash name *color-table*)))
    (if color
      (return-from obtain-color-from-table color))
    (setf (gethash name *color-table*) (make-color :red red :green green :blue blue))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro *color-black*
    (obtain-color-from-table "BLACK" 0 0 0))
  (define-symbol-macro *color-blue*
    (obtain-color-from-table "BLUE" 0 0 #xFF))
  (define-symbol-macro *color-gray*
    (obtain-color-from-table "GRAY" 128 128 128))
  (define-symbol-macro *color-green*
    (obtain-color-from-table "GREEN" 0 #xFF 0))
  (define-symbol-macro *color-red*
    (obtain-color-from-table "RED" #xFF 0 0))
  (define-symbol-macro *color-yellow*
    (obtain-color-from-table "YELLOW" 255 255 0))
  (define-symbol-macro *color-white*
    (obtain-color-from-table "WHITE" #xFF #xFF #xFF)))

(define-symbol-macro *color-alice-blue*
  (obtain-color-from-table "ALICE-BLUE" 240 248 255))
(define-symbol-macro *color-antique-white*
  (obtain-color-from-table "ANTIQUE-WHITE" 250 235 215))
(define-symbol-macro *color-aqua*
  (obtain-color-from-table "AQUA" 0 255 255))
(define-symbol-macro *color-aquamarine*
  (obtain-color-from-table "AQUAMARINE" 127 255 212))
(define-symbol-macro *color-azure*
  (obtain-color-from-table "AZURE" 240 255 255))
(define-symbol-macro *color-beige*
  (obtain-color-from-table "BEIGE" 245 245 220))
(define-symbol-macro *color-bisque*
  (obtain-color-from-table "BISQUE" 255 228 196))
(define-symbol-macro *color-blanched-almond*
  (obtain-color-from-table "BLANCHED-ALMOND" 255 235 205))
(define-symbol-macro *color-blue-violet*
  (obtain-color-from-table "BLUE-VIOLET" 138 43 226))
(define-symbol-macro *color-brown*
  (obtain-color-from-table "BROWN" 165 42 42))
(define-symbol-macro *color-burlywood*
  (obtain-color-from-table "BURLYWOOD" 222 184 135))
(define-symbol-macro *color-cadet-blue*
  (obtain-color-from-table "CADET-BLUE" 95 158 160))
(define-symbol-macro *color-chartreuse*
  (obtain-color-from-table "CHARTREUSE" 127 255 0))
(define-symbol-macro *color-chocolate*
  (obtain-color-from-table "CHOCOLATE" 210 105 30))
(define-symbol-macro *color-coral*
  (obtain-color-from-table "CORAL" 255 127 80))
(define-symbol-macro *color-cornflower-blue*
  (obtain-color-from-table "CORNFLOWER-BLUE" 100 149 237))
(define-symbol-macro *color-cornsilk*
  (obtain-color-from-table "CORNSILK" 255 248 220))
(define-symbol-macro *color-crimson*
  (obtain-color-from-table "CRIMSON" 220 20 60))
(define-symbol-macro *color-cyan*
  (obtain-color-from-table "CYAN" 0 255 255))
(define-symbol-macro *color-dark-blue*
  (obtain-color-from-table "DARK-BLUE" 0 0 139))
(define-symbol-macro *color-dark-cyan*
  (obtain-color-from-table "DARK-CYAN" 0 139 139))
(define-symbol-macro *color-dark-goldenrod*
  (obtain-color-from-table "DARK-GOLDENROD" 184 134 11))
(define-symbol-macro *color-dark-gray*
  (obtain-color-from-table "DARK-GRAY" 169 169 169))
(define-symbol-macro *color-dark-green*
  (obtain-color-from-table "DARK-GREEN" 0 100 0))
(define-symbol-macro *color-dark-khaki*
  (obtain-color-from-table "DARK-KHAKI" 189 183 107))
(define-symbol-macro *color-dark-magenta*
  (obtain-color-from-table "DARK-MAGENTA" 139 0 139))
(define-symbol-macro *color-dark-olive-green*
  (obtain-color-from-table "DARK-OLIVE-GREEN" 85 107 47))
(define-symbol-macro *color-dark-orange*
  (obtain-color-from-table "DARK-ORANGE" 255 140 0))
(define-symbol-macro *color-dark-orchid*
  (obtain-color-from-table "DARK-ORCHID" 153 50 204))
(define-symbol-macro *color-dark-red*
  (obtain-color-from-table "DARK-RED" 139 0 0))
(define-symbol-macro *color-dark-salmon*
  (obtain-color-from-table "DARK-SALMON" 233 150 122))
(define-symbol-macro *color-dark-sea-green*
  (obtain-color-from-table "DARK-SEA-GREEN" 143 188 139))
(define-symbol-macro *color-dark-slate-blue*
  (obtain-color-from-table "DARK-SLATE-BLUE" 72 61 139))
(define-symbol-macro *color-dark-slate-gray*
  (obtain-color-from-table "DARK-SLATE-GRAY" 47 79 79))
(define-symbol-macro *color-dark-turquoise*
  (obtain-color-from-table "DARK-TURQUOISE" 0 206 209))
(define-symbol-macro *color-dark-violet*
  (obtain-color-from-table "DARK-VIOLET" 148 0 211))
(define-symbol-macro *color-deep-pink*
  (obtain-color-from-table "DEEP-PINK" 255 20 147))
(define-symbol-macro *color-deep-sky-blue*
  (obtain-color-from-table "DEEP-SKY-BLUE" 0 191 255))
(define-symbol-macro *color-dim-gray*
  (obtain-color-from-table "DIM-GRAY" 105 105 105))
(define-symbol-macro *color-dodger-blue*
  (obtain-color-from-table "DODGER-BLUE" 30 144 255))
(define-symbol-macro *color-firebrick*
  (obtain-color-from-table "FIREBRICK" 178 34 34))
(define-symbol-macro *color-floral-white*
  (obtain-color-from-table "FLORAL-WHITE" 255 250 240))
(define-symbol-macro *color-forest-green*
  (obtain-color-from-table "FOREST-GREEN" 34 139 34))
(define-symbol-macro *color-fuchsia*
  (obtain-color-from-table "FUCHSIA" 255 0 255))
(define-symbol-macro *color-gainsboro*
  (obtain-color-from-table "GAINSBORO" 220 220 220))
(define-symbol-macro *color-ghost-white*
  (obtain-color-from-table "GHOST-WHITE" 248 248 255))
(define-symbol-macro *color-gold*
  (obtain-color-from-table "GOLD" 255 215 0))
(define-symbol-macro *color-goldenrod*
  (obtain-color-from-table "GOLDENROD" 218 165 32))
(define-symbol-macro *color-green-yellow*
  (obtain-color-from-table "GREEN-YELLOW" 173 255 47))
(define-symbol-macro *color-honeydew*
  (obtain-color-from-table "HONEY-DEW" 240 255 240))
(define-symbol-macro *color-hot-pink*
  (obtain-color-from-table "HOT-PINK" 255 105 180))
(define-symbol-macro *color-indian-red*
  (obtain-color-from-table "INDIAN-RED" 205 92 92))
(define-symbol-macro *color-indigo*
  (obtain-color-from-table "INDIGO" 75 0 130))
(define-symbol-macro *color-ivory*
  (obtain-color-from-table "IVORY" 255 255 240))
(define-symbol-macro *color-khaki*
  (obtain-color-from-table "KHAKI" 240 230 140))
(define-symbol-macro *color-lavender*
  (obtain-color-from-table "LAVENDER" 230 230 250))
(define-symbol-macro *color-lavender-blush*
  (obtain-color-from-table "LAVENDER-BLUSH" 255 240 245))
(define-symbol-macro *color-lawn-green*
  (obtain-color-from-table "LAWN-GREEN" 124 252 0))
(define-symbol-macro *color-lemon-chiffon*
  (obtain-color-from-table "LEMON-CHIFFON" 255 250 205))
(define-symbol-macro *color-light-blue*
  (obtain-color-from-table "LIGHT-BLUE" 173 216 230))
(define-symbol-macro *color-light-coral*
  (obtain-color-from-table "LIGHT-CORAL" 240 128 128))
(define-symbol-macro *color-light-cyan*
  (obtain-color-from-table "LIGHT-CYAN" 224 255 255))
(define-symbol-macro *color-light-goldenrod-yellow*
  (obtain-color-from-table "LIGHT-GOLDENROD-YELLOW" 250 250 210))
(define-symbol-macro *color-light-gray*
  (obtain-color-from-table "LIGHT-GRAY" 211 211 211))
(define-symbol-macro *color-light-green*
  (obtain-color-from-table "LIGHT-GREEN" 144 238 144))
(define-symbol-macro *color-light-pink*
  (obtain-color-from-table "LIGHT-PINK" 255 182 193))
(define-symbol-macro *color-light-salmon*
  (obtain-color-from-table "LIGHT-SALMON" 255 160 122))
(define-symbol-macro *color-light-sea-green*
  (obtain-color-from-table "LIGHT-SEA-GREEN" 32 178 170))
(define-symbol-macro *color-light-sky-blue*
  (obtain-color-from-table "LIGHT-SKY-BLUE" 135 206 250))
(define-symbol-macro *color-light-slate-gray*
  (obtain-color-from-table "LIGHT-SKY-GRAY" 119 136 153))
(define-symbol-macro *color-light-steel-blue*
  (obtain-color-from-table "LIGHT-STEEL-BLUE" 176 196 222))
(define-symbol-macro *color-light-yellow*
  (obtain-color-from-table "LIGHT-YELLOW" 255 255 224))
(define-symbol-macro *color-lime*
  (obtain-color-from-table "LIME" 0 255 0))
(define-symbol-macro *color-lime-green*
  (obtain-color-from-table "LIME-GREEN" 50 205 50))
(define-symbol-macro *color-linen*
  (obtain-color-from-table "LINEN" 250 240 230))
(define-symbol-macro *color-magenta*
  (obtain-color-from-table "MAGENTA" 255 0 255))
(define-symbol-macro *color-maroon*
  (obtain-color-from-table "MAROON" 128 0 0))
(define-symbol-macro *color-medium-aquamarine*
  (obtain-color-from-table "MEDIUM-AQUAMARINE" 102 205 170))
(define-symbol-macro *color-medium-blue*
  (obtain-color-from-table "MEDIUM-BLUE" 0 0 205))
(define-symbol-macro *color-medium-orchid*
  (obtain-color-from-table "MEDIUM-ORCHID" 186 85 211))
(define-symbol-macro *color-medium-purple*
  (obtain-color-from-table "MEDIUM-PURPLE" 147 112 219))
(define-symbol-macro *color-medium-sea-green*
  (obtain-color-from-table "MEDIUM-SEA-GREEN" 60 179 113))
(define-symbol-macro *color-medium-slate-blue*
  (obtain-color-from-table "MEDIUM-SLATE-BLUE" 123 104 238))
(define-symbol-macro *color-medium-spring-green*
  (obtain-color-from-table "MEDIUM-SPRING-GREEN" 0 250 154))
(define-symbol-macro *color-medium-turquoise*
  (obtain-color-from-table "MEDIUM-TURQUOISE" 72 209 204))
(define-symbol-macro *color-medium-violet-red*
  (obtain-color-from-table "MEDIUM-VIOLET-RED" 199 21 133))
(define-symbol-macro *color-midnight-blue*
  (obtain-color-from-table "MIDNIGHT-BLUE" 25 25 112))
(define-symbol-macro *color-mint-cream*
  (obtain-color-from-table "MINT-CREAM" 245 255 250))
(define-symbol-macro *color-misty-rose*
  (obtain-color-from-table "MISTY-ROSE" 255 228 225))
(define-symbol-macro *color-moccasin*
  (obtain-color-from-table "MOCCASIN" 255 228 181))
(define-symbol-macro *color-navajo-white*
  (obtain-color-from-table "NAVAJO-WHITE" 255 222 173))
(define-symbol-macro *color-navy*
  (obtain-color-from-table "NAVY" 0 0 128))
(define-symbol-macro *color-old-lace*
  (obtain-color-from-table "OLD-LACE" 253 245 230))
(define-symbol-macro *color-olive*
  (obtain-color-from-table "OLIVE" 128 128 0))
(define-symbol-macro *color-olive-drab*
  (obtain-color-from-table "OLIVE-DRAB" 107 142 35))
(define-symbol-macro *color-orange*
  (obtain-color-from-table "ORANGE" 255 165 0))
(define-symbol-macro *color-pale-turquoise*
  (obtain-color-from-table "PALE-TURQUOISE" 175 238 238))
(define-symbol-macro *color-pale-violet-red*
  (obtain-color-from-table "PALE-VIOLET-RED" 219 112 147))
(define-symbol-macro *color-papaya-whip*
  (obtain-color-from-table "PAPAYA-WHIP" 255 239 213))
(define-symbol-macro *color-peach-puff*
  (obtain-color-from-table "PEACH-PUFF" 255 218 185))
(define-symbol-macro *color-peru*
  (obtain-color-from-table "PERU" 205 133 63))
(define-symbol-macro *color-pink*
  (obtain-color-from-table "PINK" 255 192 203))
(define-symbol-macro *color-plum*
  (obtain-color-from-table "PLUM" 221 160 221))
(define-symbol-macro *color-powder-blue*
  (obtain-color-from-table "POWDER-BLUE" 176 224 230))
(define-symbol-macro *color-purple*
  (obtain-color-from-table "PURPLE" 128 0 128))
(define-symbol-macro *color-rosy-brown*
  (obtain-color-from-table "ROSY-BROWN" 188 143 143))
(define-symbol-macro *color-royal-blue*
  (obtain-color-from-table "ROYAL-BLUE" 65 105 225))
(define-symbol-macro *color-saddle-brown*
  (obtain-color-from-table "SADDLE-BROWN" 139 69 19))
(define-symbol-macro *color-salmon*
  (obtain-color-from-table "SALMON" 250 128 114))
(define-symbol-macro *color-sandy-brown*
  (obtain-color-from-table "SANDY-BROWN" 244 164 96))
(define-symbol-macro *color-sea-green*
  (obtain-color-from-table "SEA-GREEN" 46 139 87))
(define-symbol-macro *color-sea-shell*
  (obtain-color-from-table "SEA-SHELL" 255 245 238))
(define-symbol-macro *color-sienna*
  (obtain-color-from-table "SIENNA" 160 82 45))
(define-symbol-macro *color-silver*
  (obtain-color-from-table "SILVER" 192 192 192))
(define-symbol-macro *color-sky-blue*
  (obtain-color-from-table "SKY-BLUE" 135 206 235))
(define-symbol-macro *color-slate-blue*
  (obtain-color-from-table "SLATE-BLUE" 106 90 205))
(define-symbol-macro *color-snow*
  (obtain-color-from-table "SNOW" 255 250 250))
(define-symbol-macro *color-spring-green*
  (obtain-color-from-table "SPRING-GREEN" 0 255 127))
(define-symbol-macro *color-steel-blue*
  (obtain-color-from-table "STEEL-BLUE" 70 130 180))
(define-symbol-macro *color-tan*
  (obtain-color-from-table "TAN" 210 180 140))
(define-symbol-macro *color-teal*
  (obtain-color-from-table "TEAL" 0 128 128))
(define-symbol-macro *color-thistle*
  (obtain-color-from-table "THISTLE" 216 191 216))
(define-symbol-macro *color-tomato*
  (obtain-color-from-table "TOMATO" 255 99 71))
(define-symbol-macro *color-turquoise*
  (obtain-color-from-table "TURQUOISE" 64 224 208))
(define-symbol-macro *color-violet*
  (obtain-color-from-table "VIOLET" 238 130 238))
(define-symbol-macro *color-wheat*
  (obtain-color-from-table "WHEAT" 245 222 179))
(define-symbol-macro *color-white-smoke*
  (obtain-color-from-table "WHITE-SMOKE" 245 245 245))
(define-symbol-macro *color-yellow-green*
  (obtain-color-from-table "YELLOW-GREEN" 154 205 50))

(define-symbol-macro *color-3d-dark-shadow*
  (rgb->color (gfs::get-sys-color gfs::+color-3ddkshadow+)))
(define-symbol-macro *color-3d-face*
  (rgb->color (gfs::get-sys-color gfs::+color-3dface+)))
(define-symbol-macro *color-3d-highlight*
  (rgb->color (gfs::get-sys-color gfs::+color-3dhighlight+)))
(define-symbol-macro *color-3d-light*
  (rgb->color (gfs::get-sys-color gfs::+color-3dlight+)))
(define-symbol-macro *color-3d-shadow*
  (rgb->color (gfs::get-sys-color gfs::+color-3dshadow+)))
(define-symbol-macro *color-active-border*
  (rgb->color (gfs::get-sys-color gfs::+color-activeborder+)))
(define-symbol-macro *color-active-caption*
  (rgb->color (gfs::get-sys-color gfs::+color-activecaption+)))
(define-symbol-macro *color-active-caption-gradient*
  (rgb->color (gfs::get-sys-color gfs::+color-gradientactivecaption+)))
(define-symbol-macro *color-app-workspace*
  (rgb->color (gfs::get-sys-color gfs::+color-appworkspace+)))
(define-symbol-macro *color-background*
  (rgb->color (gfs::get-sys-color gfs::+color-background+)))
(define-symbol-macro *color-button-face*
  (rgb->color (gfs::get-sys-color gfs::+color-btnface+)))
(define-symbol-macro *color-button-highlight*
  (rgb->color (gfs::get-sys-color gfs::+color-btnhightlight+)))
(define-symbol-macro *color-button-shadow*
  (rgb->color (gfs::get-sys-color gfs::+color-btnshadow+)))
(define-symbol-macro *color-button-text*
  (rgb->color (gfs::get-sys-color gfs::+color-btntext+)))
(define-symbol-macro *color-caption-text*
  (rgb->color (gfs::get-sys-color gfs::+color-captiontext+)))
(define-symbol-macro *color-desktop*
  (rgb->color (gfs::get-sys-color gfs::+color-desktop+)))
(define-symbol-macro *color-gray-text*
  (rgb->color (gfs::get-sys-color gfs::+color-graytext+)))
(define-symbol-macro *color-highlight*
  (rgb->color (gfs::get-sys-color gfs::+color-highlight+)))
(define-symbol-macro *color-highlight-text*
  (rgb->color (gfs::get-sys-color gfs::+color-highlight-text+)))
(define-symbol-macro *color-hotlight*
  (rgb->color (gfs::get-sys-color gfs::+color-hotlight+)))
(define-symbol-macro *color-inactive-border*
  (rgb->color (gfs::get-sys-color gfs::+color-inactiveborder+)))
(define-symbol-macro *color-inactive-caption*
  (rgb->color (gfs::get-sys-color gfs::+color-inactivecaption+)))
(define-symbol-macro *color-inactive-caption-gradient*
  (rgb->color (gfs::get-sys-color gfs::+color-gradientinactivecaption+)))
(define-symbol-macro *color-inactive-caption-text*
  (rgb->color (gfs::get-sys-color gfs::+color-inactivecaptiontext+)))
(define-symbol-macro *color-menu*
  (rgb->color (gfs::get-sys-color gfs::+color-menu+)))
(define-symbol-macro *color-menu-bar*
  (rgb->color (gfs::get-sys-color gfs::+color-menubar+)))
(define-symbol-macro *color-menu-highlight*
  (rgb->color (gfs::get-sys-color gfs::+color-menuhighlight+)))
(define-symbol-macro *color-menu-text*
  (rgb->color (gfs::get-sys-color gfs::+color-menutext+)))
(define-symbol-macro *color-scroll-bar*
  (rgb->color (gfs::get-sys-color gfs::+color-scrollbar+)))
(define-symbol-macro *color-tooltip-background*
  (rgb->color (gfs::get-sys-color gfs::+color-infobk+)))
(define-symbol-macro *color-tooltip-text*
  (rgb->color (gfs::get-sys-color gfs::+color-infotext+)))
(define-symbol-macro *color-window*
  (rgb->color (gfs::get-sys-color gfs::+color-window+)))
(define-symbol-macro *color-window-frame*
  (rgb->color (gfs::get-sys-color gfs::+color-windowframe+)))
(define-symbol-macro *color-window-text*
  (rgb->color (gfs::get-sys-color gfs::+color-windowtext+)))

(defmethod print-object ((obj color) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~a,~a,~a)" (color-red obj) (color-green obj) (color-blue obj))))
