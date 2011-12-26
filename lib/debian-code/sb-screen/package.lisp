(defpackage :sb-screen (:use :sb-sys :sb-ext :sb-alien :common-lisp)
            (:nicknames :screen)
            (:export
             :tty-screen
             :encode-key
             :decode-key
             :key-hook
             :initialize-screen
             :release-screen
             :set-cursor
             :get-cursor
             :clear-screen
             :finish-screen
             :write-string-at-cursor
             :erase-from-cursor-to-eol
             :erase-from-cursor-to-eos
             :draw-line-at-cursor
             :valid-color-p
             :valid-colors
             :set-color
             :set-to-default-color
             :get-screen-size
             :window-resize-hook))