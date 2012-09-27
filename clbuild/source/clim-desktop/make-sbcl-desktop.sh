#!/bin/bash

sbcl  << EOF
(require 'asdf)
(load "clim-desktop.asd")
(asdf:oos 'asdf:load-op :clim-desktop)
(load "debugger.lisp")
(setf *debugger-hook* #'clim-debugger::debugger)
(sb-ext:save-lisp-and-die "clim-desktop.core")

EOF
