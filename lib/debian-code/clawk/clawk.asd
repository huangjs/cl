;;; -*- Mode: Lisp; Syntax: ANSI-Common-lisp; Package: CL-USER; Base: 10 -*-

(in-package "CL-USER")


(asdf:defsystem clawk
    :depends-on (regex)
    :components ((:file "packages")
                 (:file "utils" :depends-on ("packages"))
                 (:file "clawk" :depends-on ("packages"
                                             "utils"))))
