;;;; -*- Mode: Lisp -*-
;;;; ASDF definition for rlc
;;;; $Id: rlc.asd,v 1.1 2003/12/14 16:10:29 krosenberg Exp $

(in-package #:cl-user)
(defpackage #:rlc-system (:use #:cl #:asdf))
(in-package #:rlc-system)

(defsystem rlc
    :depends-on (kmrcl)
    :components ((:file "package")
		 (:file "main" :depends-on ("package"))))



