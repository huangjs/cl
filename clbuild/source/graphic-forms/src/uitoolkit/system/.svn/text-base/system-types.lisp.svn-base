;;;;
;;;; system-types.lisp
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
  (use-package :cffi))

;;; This function is copied from http://cl-cookbook.sourceforge.net/win32.html
;;;
;;; Copyright (c) 2002-2005 The Common Lisp Cookbook Project
;;;
(defun external-format ()
  (if (string= (software-type) "Windows NT")
      :unicode
    :ascii))

(defctype ATOM     :unsigned-short) ; shadowed in gfs: package
(defctype BOOL     :int)
(defctype BOOLEAN  :char)        ; shadowed in gfs: package
(defctype COLORREF :unsigned-long)
(defctype DWORD    :unsigned-long)
(defctype HANDLE   :pointer)
(defctype HRESULT  :unsigned-int)
(defctype INT      :int)
(defctype LANGID   :short)
(defctype LONG     :long)
(defctype LPARAM   :long)
(defctype LPCSTR   :pointer)
(defctype LPCTSTR  :pointer)
(defctype LPRECT   :pointer)
(defctype LPSTR    :pointer)
(defctype LPTR     :pointer)
(defctype LPTSTR   :pointer)
(defctype LPVOID   :long)
(defctype LRESULT  :unsigned-long)
(defctype SHORT    :unsigned-short)
(defctype TCHAR    :char)
(defctype UINT     :unsigned-int)
(defctype ULONG    :unsigned-long)
(defctype USHORT   :unsigned-short)
(defctype WORD     :short)
(defctype WPARAM   :unsigned-int)

#+sbcl
(sb-alien:define-alien-type enumchildproc
  (sb-alien:* (sb-alien:function sb-alien:int
                sb-alien:system-area-pointer
                sb-alien:long)))

#+sbcl
(sb-alien:define-alien-type enumthreadwndproc
  (sb-alien:* (sb-alien:function sb-alien:int
                sb-alien:system-area-pointer
                sb-alien:long)))

#+sbcl
(sb-alien:define-alien-type monitorsenumproc
  (sb-alien:* (sb-alien:function sb-alien:int
                sb-alien:system-area-pointer
                sb-alien:system-area-pointer
                sb-alien:system-area-pointer
                sb-alien:long)))

(defcstruct actctx
  (cbsize  ULONG)
  (flags   DWORD)
  (source  :string)
  (arch    USHORT)
  (langid  LANGID)
  (dir     :string)
  (resname :string)
  (appname :string)
  (hmodule HANDLE))

(defcstruct bitmap
  (type LONG)
  (width LONG)
  (height LONG)
  (wbytes LONG)
  (planes LONG)
  (pbits LONG)
  (bits LPTR))

(defcstruct bitmapcoreheader
  (bcsize DWORD)
  (bcwidth WORD)
  (bcheight WORD)
  (bcplanes WORD)
  (bcbitcount WORD))

(defcstruct bitmapinfo
  (bisize        DWORD)
  (biwidth       LONG)
  (biheight      LONG)
  (biplanes      WORD)
  (bibitcount    WORD)
  (bicompression DWORD)
  (bisizeimage   DWORD)
  (bixpels       LONG)
  (biypels       LONG)
  (biclrused     DWORD)
  (biclrimp      DWORD)
  (bmicolors     :unsigned-char :count 1024)) ; allocate space for max palette (256 RGBQUADs)

(define-foreign-type bitmapinfo-pointer-type () ()
  (:actual-type :pointer)
  (:simple-parser bitmapinfo-pointer))

(define-foreign-type bitmap-pixels-pointer-type () ()
  (:actual-type :pointer)
  (:simple-parser bitmap-pixels-pointer))

(defcstruct bitmapinfoheader
  (bisize DWORD)
  (biwidth LONG)
  (biheight LONG)
  (biplanes WORD)
  (bibitcount WORD)
  (bicompression DWORD)
  (bisizeimage DWORD)
  (bixpels LONG)
  (biypels LONG)
  (biclrused DWORD)
  (biclrimp DWORD))

(defcstruct choosecolor
  (ccsize     DWORD)
  (howner     HANDLE)
  (hinst      HANDLE)
  (result     COLORREF)
  (ccolors    LPTR)
  (flags      DWORD)
  (cdata      LPARAM)
  (hookfn     LPTR)  ; CCHookProc
  (templname  :string))

(defcstruct choosefont
  (structsize DWORD)
  (howner     HANDLE)
  (hdc        HANDLE)
  (logfont    LPTR)
  (pointsize  INT)
  (flags      DWORD)
  (color      COLORREF)
  (data       LPARAM)
  (hookfn     LPTR)  ; CFHookProc
  (templname  :string)
  (hinstance  HANDLE)
  (style      :string)
  (fonttype   WORD)
  (minsize    INT)
  (maxsize    INT))

(defcstruct dllversioninfo
  (size     DWORD)
  (vermajor DWORD)
  (verminor DWORD)
  (buildnum DWORD)
  (platform DWORD))

(define-foreign-type dllversioninfo-pointer-type () ()
  (:actual-type :pointer)
  (:simple-parser dllversioninfo-pointer))

(defcstruct drawitemstruct
  (ctltype    UINT)
  (ctlid      UINT)
  (itemid     UINT)
  (itemaction UINT)
  (itemstate  UINT)
  (hwnd       HANDLE)
  (hdc        HANDLE)
  (itemleft   LONG)
  (itemtop    LONG)
  (itemright  LONG)
  (itembottom LONG)
  (itemdata   :pointer))

(defcstruct drawtextparams
  (cbsize      UINT)
  (tablength   INT)
  (leftmargin  INT)
  (rightmargin INT)
  (lengthdrawn UINT))

(defcstruct findreplace
  (structsize DWORD)
  (howner     HANDLE)
  (hinst      HANDLE)
  (flags      DWORD)
  (whatstr    :string)
  (withstr    :string)
  (whatlen    WORD)
  (withlen    WORD)
  (data       LPARAM)
  (hookfn     LPTR) ; FRHookProc
  (templname  :string))

(defcstruct iconinfo
  (flag       BOOL)
  (hotspotx   DWORD)
  (hotspoty   DWORD)
  (hmask      HANDLE)
  (hcolor     HANDLE))

(define-foreign-type iconinfo-pointer-type () ()
  (:actual-type :pointer)
  (:simple-parser iconinfo-pointer))

(defcstruct initcommoncontrolsex
  (size DWORD)
  (icc  DWORD))

(defcstruct logbrush
  (style UINT)
  (color COLORREF)
  (hatch LONG))

(defcstruct logfont
  (lfheight         LONG)
  (lfwidth          LONG)
  (lfescapement     LONG)
  (lforientation    LONG)
  (lfweight         LONG)
  (lfitalic         :unsigned-char)
  (lfunderline      :unsigned-char)
  (lfstrikeout      :unsigned-char)
  (lfcharset        :unsigned-char)
  (lfoutprec        :unsigned-char)
  (lfclipprec       :unsigned-char)
  (lfquality        :unsigned-char)
  (lfpitchandfamily :unsigned-char)
  (lffacename TCHAR :count 32)) ; LF_FACESIZE is 32

(defcstruct menuinfo
  (cbsize DWORD)
  (mask DWORD)
  (style DWORD)
  (cymax UINT)
  (hbrback HANDLE)
  (helpid DWORD)
  (menudata ULONG))

(defcstruct menuiteminfo
  (cbsize UINT)
  (mask UINT)
  (type UINT)
  (state UINT)
  (id UINT)
  (hsubmenu HANDLE)
  (hbmpchecked HANDLE)
  (hbmpunchecked HANDLE)
  (idata ULONG)
  (tdata LPTSTR)
  (cch UINT)
  (hbmpitem HANDLE))

(define-foreign-type point-pointer-type () ()
  (:actual-type :pointer)
  (:simple-parser point-pointer))

(defcstruct point
  (x LONG)
  (y LONG))

(defcstruct minmaxinfo
  (reserved point)
  (maxsize point)
  (maxposition point)
  (mintracksize point)
  (maxtracksize point))

(defcstruct msg
  (hwnd HANDLE)
  (message UINT)
  (wparam WPARAM)
  (lparam LPARAM)
  (time DWORD)
  (pnt point))

(defcstruct paintstruct
  (hdc            HANDLE)
  (erase          BOOL)
  (rcpaint-x      LONG)
  (rcpaint-y      LONG)
  (rcpaint-width  LONG)
  (rcpaint-height LONG)
  (restore        BOOL)
  (incupdate      BOOL)
  (reserved       :unsigned-char :count 32))

(defcstruct pbrange
  (low            INT)
  (high           INT))

(define-foreign-type rect-pointer-type () ()
  (:actual-type :pointer)
  (:simple-parser rect-pointer))

(defcstruct rect
  (left LONG)
  (top LONG)
  (right LONG)
  (bottom LONG))

(defcstruct monitorinfoex
  (cbsize UINT)
  (monitor rect)
  (work rect)
  (flags DWORD)
  (device TCHAR :count 32)) ; CCHDEVICENAME

(defcstruct nccalcsize-params
  (clientnewleft   LONG)
  (clientnewtop    LONG)
  (clientnewright  LONG)
  (clientnewbottom LONG)
  (destvalidleft   LONG)
  (destvalidtop    LONG)
  (destvalidright  LONG)
  (destvalidbottom LONG)
  (srcvalidleft    LONG)
  (srcvalidtop     LONG)
  (srcvalidright   LONG)
  (srcvalidbottom  LONG)
  (lppos           LPTR))

(defcstruct openfilename
  (ofnsize DWORD)
  (ofnhwnd HANDLE)
  (ofnhinst HANDLE)
  (ofnfilter LPTR)
  (ofncustomfilter LPTR)
  (ofnmaxcustfilter DWORD)
  (ofnfilterindex DWORD)
  (ofnfile LPTR)
  (ofnmaxfile DWORD)
  (ofnfiletitle :pointer)
  (ofnmaxfiletitle DWORD)
  (ofninitialdir :pointer)
  (ofntitle :pointer)
  (ofnflags DWORD)
  (ofnfileoffset WORD)
  (ofnfileext WORD)
  (ofndefext :pointer)
  (ofncustdata LPARAM)
  (ofnhookfn LPTR)
  (ofntemplname :pointer)
  (ofnpvreserved LPTR)
  (ofndwreserved DWORD)
  (ofnexflags DWORD))

(defcstruct rgbquad
  (rgbblue     :unsigned-char)
  (rgbgreen    :unsigned-char)
  (rgbred      :unsigned-char)
  (rgbreserved :unsigned-char))

(defcstruct scrollinfo
  (cbsize   UINT)
  (fmask    UINT)
  (minpos   INT)
  (maxpos   INT)
  (pagesize UINT)
  (pos      INT)
  (trackpos INT))

(defcstruct size
  (cx LONG)
  (cy LONG))

(defcstruct textmetrics
  (tmheight          LONG)
  (tmascent          LONG)
  (tmdescent         LONG)
  (tminternalleading LONG)
  (tmexternalleading LONG)
  (tmavgcharwidth    LONG)
  (tmmaxcharwidth    LONG)
  (tmweight          LONG)
  (tmoverhang        LONG)
  (tmdigaspectx      LONG)
  (tmdigaspecty      LONG)
  (tmfirstchar       :char)
  (tmlastchar        :char)
  (tmdefaultchar     :char)
  (tmbreakchar       :char)
  (tmitalic          :unsigned-char)
  (tmunderlined      :unsigned-char)
  (tmstruckout       :unsigned-char)
  (tmpitchfam        :unsigned-char)
  (tmcharset         :unsigned-char))

(defcstruct windowinfo
  (cbsize DWORD)
  (windowleft LONG)
  (windowtop LONG)
  (windowright LONG)
  (windowbottom LONG)
  (clientleft LONG)
  (clienttop LONG)
  (clientright LONG)
  (clientbottom LONG)
  (style DWORD)
  (exstyle DWORD)
  (winstatus DWORD)
  (cxwinborders UINT)
  (cywinborders UINT)
  (wintype ATOM)
  (version WORD))

(defcstruct windowpos
  (hwnd      HANDLE)
  (hwndafter HANDLE)
  (x         INT)
  (y         INT)
  (cx        INT)
  (cy        INT)
  (flags    UINT))
  
(defcstruct wndclassex
  (cbsize UINT)
  (style UINT)
  (wndproc :pointer)
  (clsextra INT)
  (wndextra INT)
  (hinst HANDLE)
  (hicon HANDLE)
  (hcursor HANDLE)
  (hbrush HANDLE)
  (menuname LPCTSTR)
  (classname LPCTSTR)
  (smallicon HANDLE))
