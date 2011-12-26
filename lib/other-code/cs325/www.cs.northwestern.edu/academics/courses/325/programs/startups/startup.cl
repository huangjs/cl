(in-package :cl-user)

;;; Suggested start up file for CS 325
;;; Platform: Allegro Common Lisp on Windows
;;;
;;; Place this file in the Allegro directory, e.g. C:\Program Files\acl62\


;;; Turn off the list printing limits.

(setq tpl:*print-length* nil)
(setq tpl:*print-level* nil)


;;; Load cs325.lisp to create the cs325 package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "c:/courses/cs325/cs325.lisp"))  ;; CHANGE TO MATCH YOUR SETUP


