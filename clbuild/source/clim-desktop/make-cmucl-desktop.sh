#!/bin/bash

lisp << EOF

(require 'asdf)
(load "clim-desktop.asd")
(asdf:oos 'asdf:load-op :clim-desktop)
(load "debugger.lisp")
(setf *debug-hook* #'clim-debugger::debugger)
(ext:save-lisp-and-die "clim-desktop.core")

EOF
