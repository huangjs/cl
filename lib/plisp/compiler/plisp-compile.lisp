
;;;  This builds an executable postscript compiler

(load "vars")  ; has defvars
(load "macros")   ; useful macros
(compile-file "top")      ; top level control
(compile-file "compile")  ; guts of the compilation process
(compile-file "output")   ; output routines
(compile-file "defps")    ; definitions of postscript functions
(compile-file "args")
(compile-file "names")
(compile-file "flow")
(compile-file "util")

;; Simple-minded iteration through the common-lisp directory -
;; may need changing depending of OS

(let ((dir "common-lisp/"))
    (dolist (x '("bind" "control" "functional" "lisp-util"
		 "loop" "mvalues" "numeric" "for"))
      (compile-file (concatenate 'string dir x))))

