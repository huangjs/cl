;;; Creates the cs325-user package for CS 325 students.

;;; Update history:
;;; 02-15-05 Renamed module variable to avoid conflict with ASDF [CKR]
;;; 12-01-04 New include with integrated use-package and pathname defaults [CKR]
;;; 02-04-03 Replaced require with include [CKR]

(defpackage cs325-user
  (:use :common-lisp)
  )

(in-package :cs325-user)

(defparameter *cs325-defaults*
  (make-pathname :host (pathname-host *load-truename*)
                 :device (pathname-device *load-truename*)
                 :directory (pathname-directory *load-truename*)
                 :type "lisp"))

;;; (INCLUDE name [pathname-defaults])
;;;   Loads the code file for name, uses the package called name,
;;;   adds name to *MODULES*, and returns the code file name, if 
;;;   name is not already in *MODULES* and a code file can be found.
;;;
;;;    Example: (INCLUDE "tables")

(defun include (name &key defaults package)
  (unless (included-p name)
    (let ((file (get-code-file name defaults)))
      (cond ((null file)
             (error "~S not found" name))
            (t
             (load file)
             (push name *modules*)
             (let ((module-package (or package (get-module-package name))))
               (unless (null module-package)
                 (use-package module-package)))
             file)))))

;;; (GET-CODE-FILE name [pathname-defaults])
;;;   Returns the code file to load for name. name is
;;;   merged with pathname-defaults, if non-NIL, and
;;;   *CS325-DEFAULTS*, and then the newer of that file
;;;   and its compiled version is returned.

(defun get-code-file (name &optional defaults)
  (let ((source
         (merge-pathnames
          (if (null defaults) name (merge-pathnames name defaults))
          *cs325-defaults*)))
    (get-newer-file (compile-file-pathname source) source)))

;;; Returns the newer file. In case of a tie, returns file1.
(defun get-newer-file (file1 file2)
  (with-open-file (stream1 file1 :if-does-not-exist nil)
    (with-open-file (stream2 file2 :if-does-not-exist nil)
      (cond ((and (null stream1) (null stream2))
             nil)
            ((null stream2) file1)
            ((null stream1) file2)
            ((> (file-write-date stream2)
                (file-write-date stream1))
             file2)
            (t file1)))))

(defun included-p (name)
  (member name *modules* :test #'equal))

(defun get-module-package (name)
  (or (find-package name)
      (find-package (string-upcase name))
      (find-package (string-downcase name))))


;;; Load the modules I always want

(eval-when (:compile-toplevel :load-toplevel :execute)
  (mapc #'include
        '("tables" "frames" "mops" "show-frame"
          "extend-match" "write-wrap" "lisp-unit" "lisp-critic"
          "exercise-tests" "lisp-rules" "clyde")))

(format t "~&REMINDER: call (in-package #:cs325-user) first.~%")
