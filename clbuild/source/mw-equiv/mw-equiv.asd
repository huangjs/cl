;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem mw-equiv
  :version "0.1.3"
  :description "Extensible object equivalence protocol"
  :maintainer "Michael Weber <michaelw+equiv@foldr.org>"
  :author "Michael Weber <michaelw+equiv@foldr.org>"
  :licence "BSD-like"
  :serial t
  :components
  ((:file "package")
   (:file "equiv")
   (:static-file "README")
   (:static-file "AUTHORS")
   (:static-file "LICENCE")
   (:static-file "ChangeLog")
   (:module "doc"
            :components
            ((:html-file "index"))))
  :long-description "Extensible object equivalence protocol,
based on <http://home.pipeline.com/~hbaker1/ObjectIdentity.html>.")
