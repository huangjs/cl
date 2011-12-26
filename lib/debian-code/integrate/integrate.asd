;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:integrate-system (:use #:asdf #:cl))
(in-package #:integrate-system)

(defsystem integrate
  :depends-on (:kmrcl)
  :components ((:file "package")
	       (:file "src" :depends-on ("package"))))

