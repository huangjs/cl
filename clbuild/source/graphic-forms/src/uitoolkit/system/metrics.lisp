;;;;
;;;; metrics.lisp
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

(defun obtain-dll-version-info (dll-path)
  (let ((hmodule (load-library-wrapper dll-path))
        (version (list 0 0 0)))
    (unless (null-handle-p hmodule)
      (unwind-protect
          (let ((func-ptr (retrieve-function-pointer hmodule "DllGetVersion")))
            (unless (cffi:null-pointer-p func-ptr)
              (cffi:with-foreign-object (info-ptr 'gfs::dllversioninfo)
              (cffi:with-foreign-slots ((gfs::size gfs::vermajor gfs::verminor gfs::buildnum)
                                        info-ptr gfs::dllversioninfo)
                (setf gfs::size (cffi:foreign-type-size 'gfs::dllversioninfo))
                (cffi:foreign-funcall-pointer func-ptr (:cconv :stdcall) :pointer info-ptr gfs::hresult)
                (setf version (list gfs::vermajor gfs::verminor gfs::buildnum))))))
        (gfs::free-library hmodule)))
    version))

(defun obtain-system-metrics ()
  "Query system metrics and return them via a hash table."
  (let ((table (make-hash-table)))
    ;;
    ;; :arrangement
    ;;
    ;; A two-valued result describing the starting position and direction
    ;; of minimized windows."
    ;;
    (setf (gethash :arrangement table)
          (let ((metric (get-system-metrics +sm-arrange+))
                (topright-bits (logior +arw-startright+ +arw-starttop+)))
            (list (cond
                    ((= (logand metric topright-bits) topright-bits)
                       :top-right)
                    ((= (logand metric +arw-starttop+) +arw-starttop+)
                       :top-left)
                    ((= (logand metric +arw-startright+) +arw-startright+)
                       :bottom-right)
                    ((= (logand metric +arw-hide+) +arw-hide+)
                       :hide)
                    (t
                       :bottom-left))
                  (if (= (logand metric +arw-up+) +arw-up+)
                    :vertical
                    :horizontal))))
    ;;
    ;; :boot-mode
    ;;
    ;; A keyword symbol describing how the system was started.
    ;;
    (setf (gethash :boot-mode table)
          (case (get-system-metrics +sm-cleanboot+)
            (0         :normal)
            (1         :fail-safe)
            (2         :fail-safe-no-network)
            (otherwise :unknown)))
    ;;
    ;; :border-sizes
    ;;
    ;; The thickness of resizable and fixes window borders in pixels.
    ;;
    (setf (gethash :border-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxedge+)
                           :height (get-system-metrics +sm-cyedge+))
                (make-size :width (get-system-metrics +sm-cxborder+)
                           :height (get-system-metrics +sm-cyborder+))))
    ;;
    ;; :button-count
    ;;
    ;; The number of mouse buttons, or zero if no mouse is installed.
    ;;
    (setf (gethash :button-count table)
          (get-system-metrics +sm-cmousebuttons+))
    ;;
    ;; :buttons-swapped
    ;;
    ;; T if the meaning of the left and right mouse buttons are swapped;
    ;; NIL otherwise.
    ;;
    (setf (gethash :buttons-swapped table)
          (/= (get-system-metrics +sm-swapbutton+) 0))
    ;;
    ;; :caption-button-sizes
    ;;
    ;; A list of the sizes of a button in a window's caption or title bar in pixels.
    ;;
    (setf (gethash :caption-button-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxsize+)
                           :height (get-system-metrics +sm-cysize+))
                (make-size :width (get-system-metrics +sm-cxsmsize+)
                           :height (get-system-metrics +sm-cysmsize+))))
    ;;
    ;; :comctl32-version
    ;;
    ;; A list of integers describing the version of comctl32.dll.
    ;;
    (setf (gethash :comctl32-version table)
          (obtain-dll-version-info "comctl32.dll"))
    ;;
    ;; :cursor-size
    ;;
    ;; The size of the cursor image in pixels.
    ;;
    (setf (gethash :cursor-size table)
          (make-size :width (get-system-metrics +sm-cxcursor+)
                     :height (get-system-metrics +sm-cycursor+)))
    ;;
    ;; :dbcs-enabled
    ;;
    ;; T if user32.dll supports DBCS; NIL otherwise.
    ;;
    (setf (gethash :dbcs-enabled table)
          (/= (get-system-metrics +sm-dbcsenabled+) 0))
    ;;
    ;; :debug-version
    ;;
    ;; T if the debug version of user32.dll is installed; NIL otherwise.
    ;;
    (setf (gethash :debug-version table)
          (/= (get-system-metrics +sm-debug+) 0))
    ;;
    ;; :display-count
    ;;
    ;; A count of the display monitors on the desktop.
    ;;
    (setf (gethash :display-count table)
          (get-system-metrics +sm-cmonitors+))
    ;;
    ;; :display-sizes
    ;;
    ;; A list containing two sizes of the display (with and without the taskbar).
    ;;
    (setf (gethash :display-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxscreen+)
                           :height (get-system-metrics +sm-cyscreen+))
                (cffi:with-foreign-object (rect-ptr 'rect)
                  (if (zerop (system-parameters-info +spi-getworkarea+ 0 rect-ptr 0))
                    (error 'win32-error :detail "system-parameters-info failed"))
                  (let ((tmp (cffi:convert-from-foreign rect-ptr 'rect-pointer)))
                    (size tmp)))))
    ;;
    ;; :double-click-size
    ;;
    ;; The size in pixels of the area surrounding a first click in a double-click sequence.
    ;;
    (setf (gethash :double-click-size table)
          (make-size :width (get-system-metrics +sm-cxdoubleclk+)
                     :height (get-system-metrics +sm-cydoubleclk+)))
    ;;
    ;; :drag-size
    ;;
    ;; The size in pixels of the area surrounding the start of a drag gesture.
    ;;
    (setf (gethash :drag-size table)
          (make-size :width (get-system-metrics +sm-cxdrag+)
                     :height (get-system-metrics +sm-cydrag+)))
    ;;
    ;; :frame-sizes
    ;;
    ;; The thickness of a fixed border (or dialog border) in pixels.
    ;;
    (setf (gethash :frame-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxframe+)
                           :height (get-system-metrics +sm-cyframe+))
                (make-size :width (get-system-metrics +sm-cxdlgframe+)
                           :height (get-system-metrics +sm-cydlgframe+))))
    ;;
    ;; :focus-size
    ;;
    ;; The thickness in pixels of the edges of the focus rectangle.
    ;;
    (setf (gethash :focus-size table)
          (make-size :width (get-system-metrics +sm-cxfocusborder+)
                     :height (get-system-metrics +sm-cyfocusborder+)))
    ;;
    ;; :icon-sizes
    ;;
    ;; The default and small sizes of an icon in pixels.
    ;;
    (setf (gethash :icon-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxicon+)
                           :height (get-system-metrics +sm-cyicon+))
                (make-size :width (get-system-metrics +sm-cxsmicon+)
                           :height (get-system-metrics +sm-cysmicon+))))
    ;;
    ;; :icon-spacing
    ;;
    ;; The width and height of a grid cell for items in a large icon view;
    ;; these values will be greater than or equal to the large icon size.
    ;;
    (setf (gethash :icon-spacing table)
          (make-size :width (get-system-metrics +sm-cxiconspacing+)
                     :height (get-system-metrics +sm-cyiconspacing+)))
    ;;
    ;; :ime-enabled
    ;;
    ;; T if Input Method Manager/Input Method Editor features are
    ;; enabled; NIL otherwise.
    ;;
    (setf (gethash :ime-enabled table)
          (/= (get-system-metrics +sm-immenabled+) 0))
    ;;
    ;; :low-end-processor
    ;;
    ;; T if the system has determined that the CPU meets criteria associated
    ;; with a low-end (slow) model.
    ;;
    (setf (gethash :low-end-processor table)
          (/= (get-system-metrics +sm-slowmachine+) 0))
    ;;
    ;; :media-center
    ;;
    ;; T if the installed system is Media Center Edition; NIL otherwise.
    ;;
    (setf (gethash :media-center table)
          (/= (get-system-metrics +sm-mediacenter+) 0))
    ;;
    ;; :menu-button-size
    ;;
    ;; The size of menubar buttons in pixels.
    ;;
    (setf (gethash :menu-button-size table)
          (make-size :width (get-system-metrics +sm-cxmenusize+)
                     :height (get-system-metrics +sm-cymenusize+)))
    ;;
    ;; :menu-check-size
    ;;
    ;; The size of the default menu checkmark image in pixels.
    ;;
    (setf (gethash :menu-check-size table)
          (make-size :width (get-system-metrics +sm-cxmenucheck+)
                     :height (get-system-metrics +sm-cymenucheck+)))
    ;;
    ;; :menu-drop-alignment
    ;;
    ;; Value is :right if menus are right-aligned with the corresponding menubar
    ;; item, or :left if menus are left-aligned.
    ;;
    (setf (gethash :menu-drop-alignment table)
          (if (zerop (get-system-metrics +sm-menudropalignment+)) :left :right))
    ;;
    ;; :mideast-enabled
    ;;
    ;; T if the system is c0nfigured to support Hebrew and Arabic languages; NIL
    ;; otherwise.
    ;;
    (setf (gethash :mideast-enabled table)
          (/= (get-system-metrics +sm-mideastenabled+) 0))
    ;;
    ;; :minimized-window-size
    ;;
    ;; The size of a minimized window in pixels.
    ;;
    (setf (gethash :minimized-window-size table)
          (make-size :width (get-system-metrics +sm-cxminimized+)
                     :height (get-system-metrics +sm-cyminimized+)))
    ;;
    ;; :minimized-window-spacing
    ;;
    ;; The width and height of a grid cell for a minimized window in pixels.
    ;;
    (setf (gethash :minimized-window-spacing table)
          (make-size :width (get-system-metrics +sm-cxminspacing+)
                     :height (get-system-metrics +sm-cyminspacing+)))
    ;;
    ;; :mouse-wheel
    ;;
    ;; T if a mouse with a wheel is installed; NIL otherwise.
    ;;
    (setf (gethash :mouse-wheel table)
          (/= (get-system-metrics +sm-mousewheelpresent+) 0))
    ;;
    ;; :notify-visually
    ;;
    ;; T if the user requires applications to provide visual notification
    ;; in situations where only an audible notification would normally occur.
    ;;
    (setf (gethash :notify-visually table)
          (/= (get-system-metrics +sm-showsounds+) 0))
    ;;
    ;; :pen-extensions
    ;;
    ;; T if the Windows for Pen extensions are installed; NIL otherwise.
    ;;
    (setf (gethash :pen-extensions table)
          (/= (get-system-metrics +sm-penwindows+) 0))
    ;;
    ;; :remote-session
    ;;
    ;; T if the calling process is associated with a Terminal Services client
    ;; session; NIL otherwise.
    ;;
    (setf (gethash :remote-session table)
          (/= (get-system-metrics +sm-remotesession+) 0))
    ;;
    ;; :remotely-controlled
    ;;
    ;; T if the current session is remotely controlled (in a Terminal Services
    ;; environment); NIL otherwise.
    ;;
    (setf (gethash :remotely-controlled table)
          (/= (get-system-metrics +sm-remotecontrol+) 0))
    ;;
    ;; :same-display-format
    ;;
    ;; T if all displays use the same color encoding; NIL otherwise.
    ;;
    (setf (gethash :same-display-format table)
          (/= (get-system-metrics +sm-samedisplayformat+) 0))
    ;;
    ;; :scrollbar-dimensions
    ;;
    ;; The width of a vertical scrollbar and the height of a horizontal scrollbar.
    ;;
    (setf (gethash :scrollbar-dimensions table)
          (make-size :width (get-system-metrics +sm-cxvscroll+)
                     :height (get-system-metrics +sm-cyhscroll+)))
    ;;
    ;; :scrollbar-arrow-dimensions
    ;;
    ;; The width of a vertical scrollbar's arrow bitmap and the height of a
    ;; horizontal-scrollbar's arrow bitmap.
    ;;
    (setf (gethash :scrollbar-arrow-dimensions table)
          (make-size :width (get-system-metrics +sm-cxhscroll+)
                     :height (get-system-metrics +sm-cyvscroll+)))
    ;;
    ;; :shell32-version
    ;;
    ;; A list of integers describing the version of comctl32.dll.
    ;;
    (setf (gethash :shell32-version table)
          (obtain-dll-version-info "shell32.dll"))
    ;;
    ;; :shutting-down
    ;;
    ;; T if the current session is shutting down; NIL otherwise.
    ;;
    (setf (gethash :shutting-down table)
          (/= (get-system-metrics +sm-shuttingdown+) 0))
    ;;
    ;; :tablet-pc
    ;;
    ;; T if the system is Windows XP Tablet PC edition; NIL otherwise.
    ;;
    (setf (gethash :tablet-pc table)
          (/= (get-system-metrics +sm-tabletpc+) 0))
    ;;
    ;; :tracking-sizes
    ;;
    ;; The minimum and maximum sizes to which a window can be dragged.
    ;;
    (setf (gethash :tracking-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxmintrack+)
                           :height (get-system-metrics +sm-cymintrack+))
                (make-size :width (get-system-metrics +sm-cxmaxtrack+)
                           :height (get-system-metrics +sm-cymaxtrack+))))
    ;;
    ;; :virtual-display-size
    ;;
    ;; The size of the bounding rectangle for all displays.
    ;;
    (setf (gethash :virtual-display-size table)
          (make-size :width (get-system-metrics +sm-cxvirtualscreen+)
                     :height (get-system-metrics +sm-cyvirtualscreen+)))
    ;;
    ;; :window-sizes
    ;;
    ;; A list of size objects representing various window extremums.
    ;;
    (setf (gethash :window-sizes table)
          (list (make-size :width (get-system-metrics +sm-cxfullscreen+)
                           :height (get-system-metrics +sm-cyfullscreen+))
                (make-size :width (get-system-metrics +sm-cxmaximized+)
                           :height (get-system-metrics +sm-cymaximized+))
                (make-size :width (get-system-metrics +sm-cxminimized+)
                           :height (get-system-metrics +sm-cyminimized+))
                (make-size :width (get-system-metrics +sm-cxmin+)
                           :height (get-system-metrics +sm-cymin+))))

    table))
