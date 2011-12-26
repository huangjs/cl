;;; -*- Mode: Lisp; Syntax: ANSI-Common-lisp; Package: CL-USER; Base: 10 -*-

(in-package "CL-USER")


(asdf:defsystem regex
    :components ((:file "packages")
                 (:file "macs" :depends-on ("packages"))
                 (:file "parser" :depends-on ("packages" "macs"))
                 (:file "optimize" :depends-on ("packages" "macs"))
                 (:file "gen" :depends-on ("packages" "macs"))
                 (:file "closure" :depends-on ("packages" "macs"))
                 (:file "regex" :depends-on ("packages"
                                             "macs"
                                             "parser"
                                             "optimize"
                                             "gen"
                                             "closure"))))

