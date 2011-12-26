(in-package :cl-user)

;;; Suggested start up file for CS 325
;;; Platform: LispWorks on Windows

;;; Place this file in your "home" directory, e.g., C:\Documents and Settings\your-name


;; Set the depth of trace output to a large but finite level.
(setq hcl:*trace-print-level* 10)

;; Change the directory where "File | Open" looks to the CS325 directory
(hcl:change-directory "c:/courses/cs325/")  ;; CHANGE TO FIT YOUR SETUP

;;; Load cs325.lisp to create the cs325 package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "cs325.lisp"))


