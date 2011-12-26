;;; -*- Mode: Lisp; Syntax: ANSI-Common-lisp; Package: CL-USER; Base: 10 -*-

(in-package "CL-USER")

(asdf:defsystem lexer
    :depends-on (regex)
    :components ((:file "packages")
                 (:file "lexer")))

