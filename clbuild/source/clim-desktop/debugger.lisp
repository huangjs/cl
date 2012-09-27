(in-package :common-lisp-user)
#+sbcl (progn
         (setf *debugger-hook* #'clim-debugger:debugger)
         (setf sb-ext:*invoke-debugger-hook* #'clim-debugger:debugger))
#+cmucl (setf *debug-hook* #'clim-debugger:debugger)

