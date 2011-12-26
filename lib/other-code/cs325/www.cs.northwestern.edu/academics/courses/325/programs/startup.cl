(in-package :cl-user)

;;; Allegro Common Lisp start-up file for CS 325, Winter 2005

;;; Turn off the list printing limits.

(setq tpl:*print-length* nil)
(setq tpl:*print-level* nil)


;;; Load cs325.lisp to create the cs325 package.
;;; CHANGE THE LOAD PATH TO MATCH WHERE YOU PUT CS325.LISP

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "c:/courses/cs325/cs325.lisp"))


