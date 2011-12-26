;;; -*- Lisp -*-
;;;
;;; Cross-implementation Portability System
;;;
;;; $Id: port.asd 7061 2003-09-07 06:34:45Z kevin $
;;; $Source: /opt/cvsroot/debian/src/port/port.asd,v $

(in-package :asdf)

(defsystem port
    :components
    ((:file "ext")
     (:file "gray" :depends-on ("ext"))
     (:file "net" :depends-on ("ext" "sys"))
     (:file "path" :depends-on ("ext"))
     (:file "proc" :depends-on ("ext"))
     (:file "shell" :depends-on ("ext"))
     (:file "sys" :depends-on ("ext" "path"))))
