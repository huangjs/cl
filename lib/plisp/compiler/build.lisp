

;;;  This builds an executable postscript compiler

(load "vars")     ; has defvars
(load "macros")   ; useful macros
(load "top")      ; top level control
(load "compile")  ; guts of the compilation process
(load "output")   ; output routines
(load "defps")    ; definitions of postscript functions
(load "args")
(load "names")
(load "flow")
(load "util")

;; Simple-minded iteration through the common-lisp directory -
;; may need changing depending of OS

(let ((dir "common-lisp/"))
    (dolist (x '("bind" "control" "functional" "lisp-util"
		 "loop" "mvalues" "numeric" "for"))
      (load (concatenate 'string dir x))))

;; This only needs to be done once.  ps-init is in defps

(ps-init)

