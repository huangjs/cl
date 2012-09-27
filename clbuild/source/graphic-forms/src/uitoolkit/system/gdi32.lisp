;;;;
;;;; gdi32.lisp
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

(in-package :graphic-forms.uitoolkit.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cffi))

(load-foreign-library "gdi32.dll")
(load-foreign-library "msimg32.dll")

(defcfun
  ("AddFontResourceExA" add-font-resource)
  INT
  (filename :string)
  (flags DWORD)
  (reserved LPTR))

(defcfun
  ("Arc" arc)
  BOOL
  (hdc HANDLE)
  (rectleft INT)
  (recttop INT)
  (rectright INT)
  (rectbottom INT)
  (startx INT)
  (starty INT)
  (endx INT)
  (endy INT))

(defcfun
  ("BitBlt" bit-blt)
  BOOL
  (hdc HANDLE)
  (xdest INT)
  (ydest INT)
  (width INT)
  (height INT)
  (srchdc HANDLE)
  (xsrc INT)
  (ysrc INT)
  (rop DWORD))

(defcfun
  ("Chord" chord)
  BOOL
  (hdc HANDLE)
  (rectleft INT)
  (recttop INT)
  (rectright INT)
  (rectbottom INT)
  (radial1x INT)
  (radial1y INT)
  (radial2x INT)
  (radial2y INT))

(defcfun
  ("CreateBitmap" create-bitmap)
  HANDLE
  (width INT)
  (height INT)
  (planes UINT)
  (bpp UINT)
  (pixels LPTR))

(defcfun
  ("CreateBitmapIndirect" create-bitmap-indirect)
  HANDLE
  (lpbm LPTR))

(defcfun
  ("CreateCompatibleBitmap" create-compatible-bitmap)
  HANDLE
  (hdc HANDLE)
  (width INT)
  (height INT))

(defcfun
  ("CreateCompatibleDC" create-compatible-dc)
  HANDLE
  (hdc HANDLE))

(defcfun
  ("CreateDIBitmap" create-di-bitmap)
  HANDLE
  (hdc HANDLE)
  (pheader LPTR)
  (option DWORD)
  (pinit bitmap-pixels-pointer)
  (pbmp LPTR)
  (usage UINT))

(defcfun
  ("CreateDIBSection" create-dib-section)
  HANDLE
  (hdc HANDLE)
  (bmi bitmapinfo-pointer)
  (usage UINT)
  (values LPTR)  ;; VOID **
  (section HANDLE)
  (offset DWORD))

(defcfun
  ("CreateFontIndirectA" create-font-indirect)
  HANDLE
  (logfont LPTR))

(defcfun
  ("CreatePen" create-pen)
  HANDLE
  (style INT)
  (width INT)
  (color COLORREF))

(defcfun
  ("CreateScalableFontResourceA" create-scalable-font-resource)
  BOOL
  (hidden DWORD)
  (resfile :string)
  (fontfile :string)
  (path :string))

(defcfun
  ("CreateSolidBrush" create-solid-brush)
  HANDLE
  (color COLORREF))

(defcfun
  ("DeleteDC" delete-dc)
  BOOL
  (hdc HANDLE))

(defcfun
  ("DeleteObject" delete-object)
  BOOL
  (hdc HANDLE))

(defcfun
  ("Ellipse" ellipse)
  BOOL
  (hdc HANDLE)
  (rectleft INT)
  (recttop INT)
  (rectright INT)
  (rectbottom INT))

(defcfun
  ("ExtCreatePen" ext-create-pen)
  HANDLE
  (style DWORD)
  (width DWORD)
  (logbrush LPTR)
  (count DWORD)
  (stylearray LPTR))

(defcfun
  ("ExtTextOutA" ext-text-out)
  BOOL
  (hdc HANDLE)
  (x INT)
  (y INT)
  (options UINT)
  (rect LPRECT)
  (str :string)
  (count UINT)
  (dx LPTR))

(defcfun
  ("GetBkColor" get-bk-color)
  COLORREF
  (hdc HANDLE))

(defcfun
  ("GetBkMode" get-bk-mode)
  INT
  (hdc HANDLE))

(defcfun
  ("GetCurrentObject" get-current-object)
  HANDLE
  (hdc  HANDLE)
  (type UINT))

(defcfun
  ("GetDCBrushColor" get-dc-brush-color)
  COLORREF
  (hdc HANDLE))

(defcfun
  ("GetDCPenColor" get-dc-pen-color)
  COLORREF
  (hdc HANDLE))

(defcfun
  ("GetDeviceCaps" get-device-caps)
  INT
  (hdc HANDLE)
  (index INT))

(defcfun
  ("GetDIBits" get-di-bits)
  INT
  (hdc HANDLE)
  (hbmp HANDLE)
  (startscan UINT)
  (scanlines UINT)
  (bits LPTR)
  (binfo LPTR)
  (usage UINT))

(defcfun
  ("GetObjectA" get-object)
  INT
  (hgdiobj HANDLE)
  (buffsize INT)
  (buffer LPTR))

(defcfun
  ("GetPixel" get-pixel)
  COLORREF
  (hdc HANDLE)
  (x INT)
  (y INT))

(defcfun
  ("GetStockObject" get-stock-object)
  HANDLE
  (type INT))

(defcfun
  ("GetTextColor" get-text-color)
  COLORREF
  (hdc HANDLE))

(defcfun
  ("GetTextExtentPoint32A" get-text-extent-point)
  BOOL
  (hdc HANDLE)
  (str :string)
  (count INT)
  (size LPTR))

(defcfun
  ("GetTextMetricsA" get-text-metrics)
  BOOL
  (hdc HANDLE)
  (lpm LPTR))

(defcfun
  ("GetWindowOrgEx" get-window-org)
  BOOL
  (hdc   HANDLE)
  (point point-pointer))

(defcfun
  ("MaskBlt" mask-blt)
  BOOL
  (hdest HANDLE)
  (xdest INT)
  (ydest INT)
  (width INT)
  (height INT)
  (hsrc HANDLE)
  (xsrc INT)
  (ysrc INT)
  (hmask HANDLE)
  (xmask INT)
  (ymask INT)
  (rop DWORD))

(defcfun
  ("Pie" pie)
  BOOL
  (hdc HANDLE)
  (rectleft INT)
  (recttop INT)
  (rightrect INT)
  (bottomrect INT)
  (radial1x INT)
  (radial1y INT)
  (radial2x INT)
  (radial2y INT))

(defcfun
  ("PolyBezier" poly-bezier)
  BOOL
  (hdc HANDLE)
  (points LPTR)
  (count DWORD))

(defcfun
  ("Polygon" polygon)
  BOOL
  (hdc HANDLE)
  (points LPTR)
  (count INT))

(defcfun
  ("Polyline" polyline)
  BOOL
  (hdc HANDLE)
  (points LPTR)
  (count INT))

(defcfun
  ("Rectangle" rectangle)
  BOOL
  (hdc HANDLE)
  (x1 INT)
  (y1 INT)
  (x2 INT)
  (y2 INT))

(defcfun
  ("RemoveFontResourceExA" remove-font-resource)
  BOOL
  (filename :string)
  (flags DWORD)
  (reserved LPTR))

(defcfun
  ("RoundRect" round-rect)
  BOOL
  (hdc HANDLE)
  (rectleft INT)
  (recttop INT)
  (rectright INT)
  (rectbottom INT)
  (width INT)
  (height INT))

(defcfun
  ("SelectObject" select-object)
  HANDLE
  (hdc HANDLE)
  (hgdiobj HANDLE))

(defcfun
  ("SetArcDirection" set-arc-direction)
  INT
  (hdc HANDLE)
  (direction INT))

(defcfun
  ("SetBkColor" set-bk-color)
  COLORREF
  (hdc HANDLE)
  (color COLORREF))

(defcfun
  ("SetBkMode" set-bk-mode)
  INT
  (hdc HANDLE)
  (mode INT))

(defcfun
  ("SetDCBrushColor" set-dc-brush-color)
  COLORREF
  (hdc HANDLE)
  (color COLORREF))

(defcfun
  ("SetDCPenColor" set-dc-pen-color)
  COLORREF
  (hdc HANDLE)
  (color COLORREF))

(defcfun
  ("SetDIBits" set-di-bits)
  INT
  (hdc HANDLE)
  (hbmp HANDLE)
  (start UINT)
  (lines UINT)
  (pbits LPTR)
  (pbmi LPTR)
  (color-use UINT))

(defcfun
  ("SetGraphicsMode" set-graphics-mode)
  INT
  (hdc HANDLE)
  (mode INT))

(defcfun
  ("SetMiterLimit" set-miter-limit)
  BOOL
  (hdc HANDLE)
  (newlimit :float)
  (oldlimit LPTR))

(defcfun
  ("SetPixel" set-pixel)
  COLORREF
  (hdc HANDLE)
  (x INT)
  (y INT)
  (color COLORREF))

(defcfun
  ("SetTextColor" set-text-color)
  COLORREF
  (hdc HANDLE)
  (color COLORREF))

(defcfun
  ("SetWindowOrgEx" set-window-org)
  BOOL
  (hdc   HANDLE)
  (x     INT)
  (y     INT)
  (point point-pointer))

(defun makerop4 (fore back)
  (logior (logand (ash back 8) #xFF000000) fore))
