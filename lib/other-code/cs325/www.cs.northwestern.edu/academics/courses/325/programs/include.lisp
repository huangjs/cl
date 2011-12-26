;;; A REQUIRE-like alternative to DEFSYS/ASDF

;;; Update history:
;;;
;;; 06/15/06: first release in this form [CKR]

(defpackage #:include
  (:use #:common-lisp)
  (:export #:get-code-file #:include #:*defaults*)
  )

(in-package #:include)


;;; (INCLUDE:INCLUDE name [defaults]) => pathname
;;;   Loads the code file for module name, as determined
;;;   by INCLUDE:GET-CODE-FILE, if that code file has not
;;;   been loaded, or has changed since it was last loaded.
;;;
;;; (INCLUDE:GET-CODE-FILE name [defaults]) => pathname
;;;   Returns the code file for name that INCLUDE would load.
;;;
;;; INCLUDE:*DEFAULTS* = pathname
;;;   Used by GET-CODE-FILE to construct the pathname for a
;;;   module. Default value: (MAKE-PATHNAME :TYPE "lisp")
;;;
;;; Unlike REQUIRE, INCLUDE has a well-defined algorithm for
;;; constructing the code file. First, GET-CODE-FILE merges:
;;;
;;;   - the defaults (if given)
;;;   - name
;;;   - INCLUDE:*DEFAULTS*
;;;   - the pathname of the file containing the call to INCLUDE,
;;;     if any, or the value of CL:*DEFAULT-PATHNAME-DEFAULTS*
;;;
;;; in that order. Then GET-CODE-FILE returns the pathname of
;;; the source or compiled version, whichever is newer. 
;;;
;;; This pathname construction algorithm supports file that
;;; load other files in a fixed relative location to each other,
;;; with reasonable compactness, while still allowing for the
;;; specification of absolute locations.
;;;
;;; For example, if INCLUDE is called in a file in the directory
;;; a/b/, then
;;;
;;;     (include "foo") -- loads a/b/foo.lisp
;;;     (include "foo" "../c/") -- loads a/c/foo.lisp
;;;     (include "foo" "../c/baz") -- loads a/c/baz.lisp
;;;
;;; Note: the compiled version of these files will be loaded if
;;; present in the same directory and the source is not newer.

;;; Public

(defvar *defaults* (make-pathname :type "lisp"))

(defun include (name &optional defaults)
  (let ((file (get-code-file name defaults)))
    (unless (included-p name file)
      (cond ((null file)
             (error "~S not found" name))
            (t
             (load file)
             (mark-included name)
             file)))))

;;; (GET-CODE-FILE name [ defaults ])
;;;   Returns the code file to load for name. 

(defun get-code-file (name &optional defaults)
  (let ((source
         (merge-multiple-pathnames
          defaults name *defaults* (get-starting-pathname))))
    (get-newer-file (compile-file-pathname source) source)))

;;; Private

(defvar *module-load-times* nil)

(defun merge-multiple-pathnames (&rest pathnames)
  (reduce #'(lambda (result pathname)
              (cond ((null pathname) result)
                    ((null result) pathname)
                    (t (merge-pathnames result pathname))))
          pathnames))

(defun get-starting-pathname ()
  (let ((origin
         (or *load-pathname* *compile-file-pathname* 
             *default-pathname-defaults*)))
    (make-pathname :host (pathname-host origin)
                   :device (pathname-device origin)
                   :directory (pathname-directory origin)
                   :type "lisp")))

;;; Returns the newer file. In case of a tie, returns file1.
(defun get-newer-file (file1 file2)
  (with-open-file (stream1 file1 :if-does-not-exist nil)
    (with-open-file (stream2 file2 :if-does-not-exist nil)
      (cond ((and (null stream1) (null stream2))
             nil)
            ((null stream2) file1)
            ((null stream1) file2)
            ((later-p (file-write-date stream2)
                      (file-write-date stream1))
             file2)
            (t file1)))))

;;; (INCLUDED-P name file) => boolean
;;;   Return false if file has changed since the last time
;;;   file was loaded. To be compatible with REQUIRE, if name
;;;   is in *MODULES* but no load time is recorded, assume
;;;   name was loaded when INCLUDED-P was first called with name.

(defun included-p (name file)
  (let ((load-time (cdr (assoc name *module-load-times* :test #'equal))))
    (cond ((not (null load-time))
           (later-p load-time  (file-write-date file)))
          ((member name *modules* :test #'equal)
           (mark-included name)
           t)
          (t nil))))

;;; (MARK-INCLUDED name) => universal-time
;;;   Records when name was loaded. 
(defun mark-included (name)
  (let ((entry (assoc name *module-load-times* :test #'equal))
        (time (get-universal-time)))
    (if (null entry)
        (push (cons name time) *module-load-times*)
      (setf (cdr entry) time))
    (pushnew name *modules* :test #'equal)
    time))
   
(defun later-p (time1 time2)
  (and (not (null time1))
       (not (null time2))
       (> time1 time2)))

(provide "include")