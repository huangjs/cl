;;;; Silly emacs, this is -*- Lisp -*-

#+sbcl
(defpackage :sb-screen-system (:use :cl :asdf))
#+sbcl
(in-package :sb-screen-system)

#+sbcl
(defsystem :sb-screen
    :name "SB-SCREEN"
    :author "Brian Mastenbrook"
    :version "1.0"
    :licence "MIT"
    :description "Console library for SBCL"
    :components ((:file "package")
                 (:file "api" :depends-on ("package"))
                 (:file "aliens" :depends-on ("package"))
                 (:file "sb-screen" :depends-on ("package" "api" "aliens"))))
