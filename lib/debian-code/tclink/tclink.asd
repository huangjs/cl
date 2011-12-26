;;; -*- Mode: Lisp -*-
;;; CL-TCLink ASDF:DEFSYSTEM definition
;;; Copyright 2002 Matthew Danish <mrd@debian.org>
;;; Distributed under the terms of the LLGPL.
;;; See LICENSE file for more details.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :uffi)
  (require :split-sequence))

(defpackage #:CL-TCLINK-SYSTEM
  (:use #:COMMON-LISP #:ASDF))
(in-package #:CL-TCLINK-SYSTEM)

#||
;; copied from db-sockets.asd

(defclass unix-dso (module) ())
(defun unix-name (pathname)
  (namestring 
   (typecase pathname
     (logical-pathname (translate-logical-pathname pathname))
     (t pathname))))

(defmethod output-files ((operation compile-op) (dso unix-dso))
  (let ((dir (component-pathname dso)))
    (list
     (make-pathname :type "so"
                    :name (car (last (pathname-directory dir)))
                    :directory (butlast (pathname-directory dir))
                    :defaults dir))))

(defmethod perform :after ((operation compile-op) (dso unix-dso))
  (let ((dso-name (unix-name (car (output-files operation dso))))
        (ldflags (component-property dso :ldflags)))
    (unless (zerop
             (run-shell-command
              "gcc -shared -o ~S ~{~S ~} ~A"
              dso-name
              (mapcar #'unix-name
                      (mapcan (lambda (c)
                                (output-files operation c))
                              (module-components dso)))
              ldflags))
      (error 'operation-error :operation operation :component dso))))

(defmethod perform ((operation load-op) (dso unix-dso))
  (let* ((pn (component-pathname dso))
         (so-filename (make-pathname
                      :directory (butlast (pathname-directory pn))
                      :name (first (last (pathname-directory pn)))
                      :type "so")))
    (uffi:load-foreign-library so-filename)))

(defmethod output-files ((op compile-op) (c c-source-file))
  (list 
   (make-pathname :type "o" :defaults
                  (component-pathname c))))
(defmethod perform ((op compile-op) (c c-source-file))
  (unless
      (= 0 (run-shell-command "/usr/bin/gcc -fPIC -o ~S -c ~S"
                              (unix-name (car (output-files op c)))
                              (unix-name (component-pathname c))))
    (error 'operation-error :operation op :component c)))

(defmethod perform ((operation load-op) (c c-source-file))
  t)
||#

;; CLISP seems to have some trouble with this for some reason.
;; When I (require :tclink) from my homedir, for some reason CLISP does
;; two things wrong:
;;   (a) Searches subdirectories until it finds tclink.lisp and loads it
;;   (b) Loads tclink file before package file
;; Oh well, CLISP sucks anyway.  CMUCL/SBCL do not have any problems.

(defsystem tclink
    :version "3.3.1"
    :components ((:file "package" :properties ((:installable . t)))
		 (:file "tclink" :depends-on ("package"
					      #+nil "libtclink")
			:properties ((:installable . t)))
		 #+nil (:unix-dso "libtclink"
                                  :components ((:c-source-file "tclinkc"
                                                               :properties ((:installable . t)))
                                               (:static-file "tclink"
                                                             :pathname "tclink.h"
                                                             :properties ((:installable . t)))
                                               (:static-file "Makefile"
                                                             :properties ((:installable . t))))
                                  :properties ((:ldflags . "-lc -lssl")))
                 (:static-file "Changelog" :properties ((:changelog-source-file . t)))
		 (:static-file "LICENSE" :properties ((:licence-file . t)))
		 (:static-file "LLGPL" :properties ((:installable . t)))
		 (:static-file "Makefile" :properties ((:installable . t))))

    :maintainer "Matthew Danish <mrd@debian.org>"
    :description "Common Lisp bindings to the TrustCommerce transaction system")

