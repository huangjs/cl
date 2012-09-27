;;;;
;;;; graphics-constants.lisp
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

;;; The following are transcribed from WinGDI.h;
;;; specify one of them as the value of the char-set
;;; slot in the font-data structure.
;;; 
(defconstant +ansi-charset+                     0)
(defconstant +default-charset+                  1)
(defconstant +symbol-charset+                   2)
(defconstant +shiftjis-charset+               128)
(defconstant +hangeul-charset+                129)
(defconstant +hangul-charset+                 129)
(defconstant +gb2312-charset+                 134)
(defconstant +chinesebig5-charset+            136)
(defconstant +oem-charset+                    255)
(defconstant +johab-charset+                  130)
(defconstant +hebrew-charset+                 177)
(defconstant +arabic-charset+                 178)
(defconstant +greek-charset+                  161)
(defconstant +turkish-charset+                162)
(defconstant +vietnamese-charset+             163)
(defconstant +thai-charset+                   222)
(defconstant +easteurope-charset+             238)
(defconstant +russian-charset+                204)
(defconstant +mac-charset+                     77)
(defconstant +baltic-charset+                 186)

;;; The following are from WinUser.h; specify one of
;;; them as the value of the :system keyword arg when
;;; creating an icon-bundle
;;;
(defconstant +application-icon+             32512)
(defconstant +error-icon+                   32513)
(defconstant +information-icon+             32516)
(defconstant +question-icon+                32514)
(defconstant +warning-icon+                 32515)


;;; The following are from WinUser.h; specify one of
;;; them as the value of the :system keyword arg when
;;; creating an image.
;;;
(defconstant +app-starting-cursor+ gfs::+ocr-appstarting+)
(defconstant +crosshair-cursor+    gfs::+ocr-cross+)
(defconstant +default-cursor+      gfs::+ocr-normal+)
(defconstant +hand-cursor+         gfs::+ocr-hand+)
(defconstant +help-cursor+         32651)
(defconstant +ibeam-cursor+        gfs::+ocr-ibeam+)
(defconstant +no-cursor+           gfs::+ocr-no+)
(defconstant +size-all-cursor+     gfs::+ocr-sizeall+)
(defconstant +size-nesw-cursor+    gfs::+ocr-sizenesw+)
(defconstant +size-ns-cursor+      gfs::+ocr-sizens+)
(defconstant +size-nwse-cursor+    gfs::+ocr-sizenwse+)
(defconstant +size-we-cursor+      gfs::+ocr-sizewe+)
(defconstant +up-arrow-cursor+     gfs::+ocr-up+)
(defconstant +wait-cursor+         gfs::+ocr-wait+)
