;;;;
;;;; user32.lisp
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

(in-package :graphic-forms.uitoolkit.system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :cffi)
#+cffi-features:no-stdcall
  (error "Graphic-Forms requires stdcall support enabled in CFFI."))

(load-foreign-library "user32.dll")

(defcfun
  ("AdjustWindowRectEx" adjust-window-rect)
  BOOL
  (rect LPTR)
  (style LONG)
  (menu BOOL)
  (exstyle LONG))

(defcfun
  ("BeginDeferWindowPos" begin-defer-window-pos)
  HANDLE
  (numwin INT))

(defcfun
  ("BeginPaint" begin-paint)
  HANDLE
  (hwnd HANDLE)
  (ps LPTR))

(defcfun
  ("CallWindowProcA" call-window-proc)
  LRESULT
  (wndproc :pointer)
  (hwnd HANDLE)
  (msg UINT)
  (wp WPARAM)
  (lp LPARAM))

(defcfun
  ("CharLowerA" char-lower)
  UINT
  (ch UINT))

(defcfun
  ("ChildWindowFromPointEx" child-window-from-point)
  HANDLE
  (hwnd HANDLE)
  (pntx LONG)
  (pnty LONG)
  (flags UINT))

(defcfun
  ("ClientToScreen" client-to-screen)
  BOOL
  (hwnd HANDLE)
  (pnt :pointer))

(defcfun
  ("CreateIconIndirect" create-icon-indirect)
  HANDLE
  (iconinfo iconinfo-pointer))

(defcfun
  ("CreateMenu" create-menu)
  HANDLE)

(defcfun
  ("CreatePopupMenu" create-popup-menu)
  HANDLE)

(defcfun
  ("CreateWindowExA" create-window)
  HANDLE
  (ex-style DWORD)
  (class-name LPCTSTR)
  (window-name LPCTSTR)
  (style DWORD)
  (x UINT)
  (y UINT)
  (width UINT)
  (height UINT)
  (parent HANDLE)
  (menu HANDLE)
  (instance HANDLE)
  (param LPVOID))

(defcfun
  ("DeferWindowPos" defer-window-pos)
  HANDLE
  (posinfo HANDLE)
  (hwnd HANDLE)
  (hwndafter HANDLE)
  (x INT)
  (y INT)
  (cx INT)
  (cy INT)
  (flags UINT))

(defcfun
  ("DefDlgProcA" def-dlg-proc)
  LRESULT
  (hwnd HANDLE)
  (msg UINT)
  (wp WPARAM)
  (lp LPARAM))

(defcfun
  ("DefWindowProcA" def-window-proc)
  LRESULT
  (hwnd HANDLE)
  (msg UINT)
  (wp WPARAM)
  (lp LPARAM))

(defcfun
  ("DestroyCursor" destroy-cursor)
  BOOL
  (hcursor HANDLE))

(defcfun
  ("DestroyIcon" destroy-icon)
  BOOL
  (hicon HANDLE))

(defcfun
  ("DestroyMenu" destroy-menu)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("DestroyWindow" destroy-window)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("DispatchMessageA" dispatch-message)
  LRESULT
  (msg-ptr LPTR))

(defcfun
  ("DrawMenuBar" draw-menu-bar)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("DrawTextExA" draw-text-ex)
  INT
  (hdc HANDLE)
  (text :string)
  (count INT)
  (rect LPTR)
  (format UINT)
  (params LPTR))

(defcfun
  ("EnableMenuItem" enable-menu-item)
  BOOL
  (hmenu HANDLE)
  (id UINT)
  (flag UINT))

(defcfun
  ("EnableWindow" enable-window)
  BOOL
  (hwnd HANDLE)
  (enable BOOL))

(defcfun
  ("EndDeferWindowPos" end-defer-window-pos)
  BOOL
  (posinfo HANDLE))

(defcfun
  ("EndPaint" end-paint)
  BOOL
  (hwnd HANDLE)
  (ps LPTR))

(defcfun
  ("EnumChildWindows" enum-child-windows :cconv :stdcall)
  INT
  (hwnd HANDLE)
  (func :pointer)
  (lparam LPARAM))

(defcfun
  ("EnumDisplayMonitors" enum-display-monitors :cconv :stdcall)
  INT
  (hdc HANDLE)
  (cliprect LPTR)
  (enumproc LPTR)
  (data LPARAM))

(defcfun
  ("EnumThreadWindows" enum-thread-windows :cconv :stdcall)
  INT
  (threadid DWORD)
  (func :pointer)
  (lparam LPARAM))

(defcfun
  ("GetAncestor" get-ancestor)
  HANDLE
  (hwnd HANDLE)
  (flags UINT))

(defcfun
  ("GetAsyncKeyState" get-async-key-state)
  SHORT
  (virtkey INT))

(defcfun
  ("GetCapture" get-capture)
  HANDLE)

(defcfun
  ("GetClassInfoExA" get-class-info)
  BOOL
  (instance HANDLE)
  (class-name LPCTSTR)
  (wnd-class LPTR))

;;; GetClassLong is deprecated in favor of GetClassLongPtr
;;; which can be used to write code compatible to both Win32
;;; and Win64. But on Win32, GetClassLongPtr is actually
;;; typedef'd to GetClassLong, so that is the function
;;; that actually has to be referenced by this defcfun form.
;;;
(defcfun
  ("GetClassLongA" get-class-long)
  :long
  (hwnd HANDLE)
  (index INT))

(defcfun
  ("GetClassNameA" get-class-name)
  INT
  (hwnd HANDLE)
  (classname LPTSTR)
  (maxcount INT))

(defcfun
  ("GetClientRect" get-client-rect)
  BOOL
  (hwnd HANDLE)
  (rct LPTR))

(defcfun
  ("GetCursorPos" get-cursor-pos)
  BOOL
  (pnt :pointer))

(defcfun
  ("GetDC" get-dc)
  HANDLE
  (hwnd HANDLE))

(defcfun
  ("GetDesktopWindow" get-desktop-window)
  HANDLE)

(defcfun
  ("GetFocus" get-focus)
  HANDLE)

(defcfun
  ("GetIconInfo" get-icon-info)
  BOOL
  (hicon    HANDLE)
  (iconinfo LPTR))

(defcfun
  ("GetKeyState" get-key-state)
  SHORT
  (virtkey INT))

(defcfun
  ("GetMenu" get-menu)
  HANDLE
  (hwnd HANDLE))

(defcfun
  ("GetMenuInfo" get-menu-info)
  BOOL
  (hmenu HANDLE)
  (miptr LPTR))

(defcfun
  ("GetMenuItemInfoA" get-menu-item-info)
  BOOL
  (hmenu HANDLE)
  (item UINT)
  (by-pos BOOL)
  (item-info LPTR))

(defcfun
  ("GetMessageA" get-message)
  BOOL
  (msg LPTR)
  (hwnd HANDLE)
  (filter-min UINT)
  (filter-max UINT))

(defcfun
  ("GetMessageTime" get-message-time)
  LONG)

(defcfun
  ("GetMonitorInfoA" get-monitor-info)
  BOOL
  (hmonitor HANDLE)
  (monitor-info LPTR))

(defcfun
  ("GetNextDlgTabItem" get-next-dlg-tab-item)
  HANDLE
  (hdlg HANDLE)
  (hctl HANDLE)
  (flag BOOL))

(defcfun
  ("GetParent" get-parent)
  HANDLE
  (hwnd HANDLE))

(defcfun
  ("GetScrollInfo" get-scroll-info)
  BOOL
  (hwnd   HANDLE)
  (bar    INT)
  (info   LPTR))

(defcfun
  ("GetSubMenu" get-submenu)
  HANDLE
  (hwnd HANDLE)
  (pos INT))

(defcfun
  ("GetSysColor" get-sys-color)
  DWORD
  (index INT))

(defcfun
  ("GetSystemMetrics" get-system-metrics)
  INT
  (index INT))

(defcfun
  ("GetWindowInfo" get-window-info)
  BOOL
  (hwnd HANDLE)
  (pwi LPTR))

;;; GetWindowLong is deprecated in favor of GetWindowLongPtr
;;; which can be used to write code compatible to both Win32
;;; and Win64. But on Win32, GetWindowLongPtr is actually
;;; typedef'd to GetWindowLong, so that is the function
;;; that actually has to be referenced by this defcfun form.
;;;
(defcfun
  ("GetWindowLongA" get-window-long)
  :long
  (hwnd HANDLE)
  (index INT))

(defcfun
  ("GetWindowTextLengthA" get-window-text-length)
  INT
  (hwnd HANDLE))

(defcfun
  ("GetWindowTextA" get-window-text)
  INT
  (hwnd HANDLE)
  (str LPTR)
  (max INT))

(defcfun
  ("GetWindowThreadProcessId" get-window-thread-process-id)
  DWORD
  (hwnd HANDLE)
  (pid LPTR))

(defcfun
  ("InsertMenuItemA" insert-menu-item)
  BOOL
  (hmenu HANDLE)
  (item UINT)
  (by-pos BOOL)
  (item-info LPTR))

(defcfun
  ("InvalidateRect" invalidate-rect)
  BOOL
  (hwnd HANDLE)
  (rct LPTR)
  (erase BOOL))

(defcfun
  ("IsClipboardFormatAvailable" is-clipboard-format-available)
  BOOL
  (format UINT))

(defcfun
  ("IsDialogMessageA" is-dialog-message)
  BOOL
  (hwnd HANDLE)
  (msg LPTR))

(defcfun
  ("IsWindowEnabled" is-window-enabled)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("IsWindowVisible" is-window-visible)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("KillTimer" kill-timer)
  BOOL
  (hwnd HANDLE)
  (id UINT))

(defcfun
  ("LoadBitmapA" load-bitmap)
  HANDLE
  (hinst HANDLE)
  (name LPTR)) ; LPTR to make it easier to pass constants like +obm-checkboxes+

(defcfun
  ("LoadIconA" load-icon)
  HANDLE
  (instance HANDLE)
  (name LPCTSTR))

(defcfun
  ("LoadImageA" load-image)
  HANDLE
  (instance HANDLE)
  (name LPCTSTR)
  (type UINT)
  (cx INT)
  (cy INT)
  (fu-load UINT))

(defcfun
  ("LockWindowUpdate" lock-window-update)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("MapVirtualKeyA" map-virtual-key)
  UINT
  (code UINT)
  (type UINT))

(defcfun
  ("MessageBoxExA" message-box)
  INT
  (hwnd    HANDLE)
  (text    :string)
  (caption :string)
  (type    UINT)
  (langid  WORD))

(defcfun
  ("MonitorFromPoint" monitor-from-point)
  HANDLE
  (pntx  LONG)
  (pnty  LONG)
  (flags DWORD))

(defcfun
  ("MonitorFromWindow" monitor-from-window)
  HANDLE
  (hwnd HANDLE)
  (flags DWORD))

(defcfun
  ("PeekMessageA" peek-message)
  BOOL
  (msg LPTR)
  (hwnd HANDLE)
  (filter-min UINT)
  (filter-max UINT)
  (remove-msg UINT))

(defcfun
  ("PostMessageA" post-message)
  BOOL
  (hwnd HANDLE)
  (msg UINT)
  (wparam WPARAM)
  (lparam LPARAM))

(defcfun
  ("PostQuitMessage" post-quit-message)
  :void
  (exit-code INT))

(defcfun
  ("RegisterClassExA" register-class)
  ATOM
  (wndclass LPTR))

(defcfun
  ("RegisterWindowMessageA" register-window-message)
  UINT
  (str :string))

(defcfun
  ("ReleaseCapture" release-capture)
  BOOL)

(defcfun
  ("ReleaseDC" release-dc)
  INT
  (hwnd HANDLE)
  (hdc HANDLE))

(defcfun
  ("RemoveMenu" remove-menu)
  BOOL
  (hmenu HANDLE)
  (pos UINT)
  (flags UINT))

(defcfun
  ("ScreenToClient" screen-to-client)
  BOOL
  (hwnd HANDLE)
  (pnt :pointer))

(defcfun
  ("ScrollWindowEx" scroll-window)
  INT
  (hwnd       HANDLE)
  (dx         INT)
  (dy         INT)
  (scrollrect LPTR)
  (cliprect   LPTR)
  (updatergn  HANDLE)
  (updaterect LPTR)
  (flags      UINT))

(defcfun
  ("SendMessageA" send-message)
  LRESULT
  (hwnd HANDLE)
  (msg UINT)
  (wparam WPARAM)
  (lparam WPARAM))

(defcfun
  ("SetActiveWindow" set-active-window)
  HANDLE
  (hwnd HANDLE))

(defcfun
  ("SetCapture" set-capture)
  HANDLE
  (hwnd HANDLE))

(defcfun
  ("SetCursor" set-cursor)
  HANDLE
  (hcursor HANDLE))

(defcfun
  ("SetFocus" set-focus)
  HANDLE
  (hwnd HANDLE))

(defcfun
  ("SetMenu" set-menu)
  BOOL
  (hwnd HANDLE)
  (hmenu HANDLE))

(defcfun
  ("SetMenuInfo" set-menu-info)
  BOOL
  (hmenu HANDLE)
  (miptr LPTR))

(defcfun
  ("SetMenuItemInfoA" set-menu-item-info)
  BOOL
  (hmenu HANDLE)
  (item UINT)
  (by-pos BOOL)
  (item-info LPTR))

(defcfun
  ("SetScrollInfo" set-scroll-info)
  INT
  (hwnd   HANDLE)
  (bar    INT)
  (info   LPTR)
  (redraw BOOL))

(defcfun
  ("SetTimer" set-timer)
  UINT
  (hwnd HANDLE)
  (id UINT)
  (elapse UINT)
  (callback :pointer)) ;; TIMERPROC (requires _stdcall, do not use yet)

;;; SetWindowLong is deprecated in favor of SetWindowLongPtr
;;; which can be used to write code compatible to both Win32
;;; and Win64. But on Win32, SetWindowLongPtr is actually
;;; typedef'd to SetWindowLong, so that is the function
;;; that actually has to be referenced by this defcfun form.
;;;
(defcfun
  ("SetWindowLongA" set-window-long)
  :long
  (hwnd HANDLE)
  (index INT)
  (new-data :long))

(defcfun
  ("SetWindowPos" set-window-pos)
  BOOL
  (hwnd HANDLE)
  (hwndafter HANDLE)
  (x INT)
  (y INT)
  (cx INT)
  (cy INT)
  (flags UINT))

(defcfun
  ("SetWindowTextA" set-window-text)
  BOOL
  (hwnd HANDLE)
  (str :string))

(defcfun
  ("ShowCursor" show-cursor)
  INT
  (flag BOOL))

(defcfun
  ("ShowWindow" show-window)
  BOOL
  (hwnd HANDLE)
  (cmd INT))

(defcfun
  ("SystemParametersInfoA" system-parameters-info)
  BOOL
  (action  UINT)
  (iparam  UINT)
  (vparam  LPTR)
  (ini     UINT))

(defcfun
  ("TrackPopupMenuEx" track-popup-menu)
  BOOL
  (hmenu HANDLE)
  (flags UINT)
  (x INT)
  (y INT)
  (hwnd HANDLE)
  (params LPTR))

(defcfun
  ("TranslateMessage" translate-message)
  BOOL
  (msg LPTR))

(defcfun
  ("UpdateWindow" update-window)
  BOOL
  (hwnd HANDLE))

(defcfun
  ("ValidateRect" validate-rect)
  BOOL
  (hwnd HANDLE)
  (rct LPTR))

(defcfun
  ("WindowFromDC" window-from-dc)
  HANDLE
  (hdc HANDLE))

(defcfun
  ("WindowFromPoint" window-from-point)
  HANDLE
  (pnt :pointer))
