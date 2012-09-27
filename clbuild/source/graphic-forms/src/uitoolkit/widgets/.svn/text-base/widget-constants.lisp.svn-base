;;;;
;;;; widget-constants.lisp
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

(defconstant +vk-break+            #x03)
(defconstant +vk-backspace+        #x08)
(defconstant +vk-tab+              #x09)
(defconstant +vk-clear+            #x0C) ; numpad-5 when numlock off
(defconstant +vk-return+           #x0D)
(defconstant +vk-shift+            #x10)
(defconstant +vk-control+          #x11)
(defconstant +vk-alt+              #x12)
(defconstant +vk-pause+            #x13)
(defconstant +vk-caps-lock+        #x14)
(defconstant +vk-escape+           #x1B)
(defconstant +vk-page-up+          #x21)
(defconstant +vk-page-down+        #x22)
(defconstant +vk-end+              #x23)
(defconstant +vk-home+             #x24)
(defconstant +vk-left+             #x25)
(defconstant +vk-up+               #x26)
(defconstant +vk-right+            #x27)
(defconstant +vk-down+             #x28)
(defconstant +vk-insert+           #x2D)
(defconstant +vk-delete+           #x2E)
(defconstant +vk-help+             #x2F)
(defconstant +vk-left-win+         #x5B)
(defconstant +vk-right-win+        #x5C)
(defconstant +vk-applications+     #x5D)
(defconstant +vk-numpad-0+         #x60)
(defconstant +vk-numpad-1+         #x61)
(defconstant +vk-numpad-2+         #x62)
(defconstant +vk-numpad-3+         #x63)
(defconstant +vk-numpad-4+         #x64)
(defconstant +vk-numpad-5+         #x65)
(defconstant +vk-numpad-6+         #x66)
(defconstant +vk-numpad-7+         #x67)
(defconstant +vk-numpad-8+         #x68)
(defconstant +vk-numpad-9+         #x69)
(defconstant +vk-numpad-*+         #x6A)
(defconstant +vk-numpad-++         #x6B)
(defconstant +vk-numpad--+         #x6D)
(defconstant +vk-numpad-.+         #x6E)
(defconstant +vk-numpad-/+         #x6F)
(defconstant +vk-numpad-f1+        #x70)
(defconstant +vk-numpad-f2+        #x71)
(defconstant +vk-numpad-f3+        #x72)
(defconstant +vk-numpad-f4+        #x73)
(defconstant +vk-numpad-f5+        #x74)
(defconstant +vk-numpad-f6+        #x75)
(defconstant +vk-numpad-f7+        #x76)
(defconstant +vk-numpad-f8+        #x77)
(defconstant +vk-numpad-f9+        #x78)
(defconstant +vk-numpad-f10+       #x79)
(defconstant +vk-numpad-f11+       #x7A)
(defconstant +vk-numpad-f12+       #x7B)
(defconstant +vk-num-lock+         #x90)
(defconstant +vk-scroll-lock+      #x91)
(defconstant +vk-left-shift+       #xA0)
(defconstant +vk-right-shift+      #xA1)
(defconstant +vk-left-control+     #xA2)
(defconstant +vk-right-control+    #xA3)
(defconstant +vk-left-alt+         #xA4)
(defconstant +vk-right-alt+        #xA5)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +default-child-style+   (logior gfs::+ws-child+
                                               gfs::+ws-tabstop+
                                               gfs::+ws-visible+))
  (defconstant +default-widget-width+  64)
  (defconstant +default-widget-height+ 64)
  (defconstant +estimated-text-size+   32) ; bytes
  (defconstant +window-pos-flags+      (logior gfs::+swp-nozorder+
                                               gfs::+swp-noownerzorder+
                                               gfs::+swp-noactivate+
                                               gfs::+swp-nocopybits+)))

(defvar *empty-rect* (gfs:make-rectangle))
