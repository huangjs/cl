;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: alpaca.asd,v 1.1.1.1 2004/03/25 06:43:01 mevins Exp $
;;; 
;;; An extensible text editor for authors
;;; asdf system definition

(require :asdf)

(in-package :asdf)

;;; pathname parameters and compile-time initialization

(defparameter  cl-user::$alpaca-base-path (make-pathname
										   :directory (pathname-directory *load-pathname*)))
(defparameter  cl-user::$alpaca-bundle-path
  (merge-pathnames
   (make-pathname :directory '(:relative "bin" "Alpaca.app"))
   cl-user::$alpaca-base-path))
(setq ccl::*default-bundle-path* cl-user::$alpaca-bundle-path)

(defparameter  cl-user::$alpaca-process-path
  (merge-pathnames
   (make-pathname :directory '(:relative "Contents" "MacOS")
				  :name "Alpaca")
   cl-user::$alpaca-bundle-path))
(defparameter cl-user::$alpaca-image-path
  (merge-pathnames
   (make-pathname :directory '(:relative "Contents" "MacOS")
				  :name "Alpaca.image")
   cl-user::$alpaca-bundle-path))
(defparameter cl-user::$alpaca-executable-path
  (merge-pathnames
   (make-pathname :directory '(:relative "Contents" "MacOS")
				  :name "Alpaca")
   cl-user::$alpaca-bundle-path))
(setq ccl::*default-bundle-executable-path* cl-user::$alpaca-executable-path)

(defparameter cl-user::$openmcl-build-executable-path (elt ccl::*command-line-argument-list* 0))

(asdf:defsystem "ALPACA"
				:version "0.5"
				:serial t
				:components
				((:module src
						  :serial t
						  :components
						  ((:module lib
									:serial t
									:components
									((:file "remote-repl")))
						   (:file "openmcl-patches")
						   (:file "utils")
						   (:file "main")
						   (:file "globals")
						   (:file "classes")
						   (:file "documents")
						   (:file "projects")
						   (:file "views")
						   (:file "delegate")
						   (:file "listener")
						   (:file "events")
						   (:file "api")))))

(defun cl-user::load-alpaca (&key (debug-port nil))
  (defun ccl::debug-server-port ()
	debug-port)
  (operate 'load-op "ALPACA"))

;;; ----------------------------------------------------------------------
;;; build the application
;;; ----------------------------------------------------------------------

(defclass ccl::cocoa-application (ccl::lisp-development-system)())
(defmethod ccl::parse-application-arguments ((a ccl::cocoa-application))
  (declare (ignore a))
  ;; ignore input arguments at launch
  (values nil nil nil))


(defun cl-user::make-alpaca ()
  (let* ((working-directory (ccl::mac-default-directory))
		 (executable-path (merge-pathnames
						   (make-pathname :directory '(:relative "bin" "Alpaca.app" "Contents" "MacOS")
										  :name "Alpaca")
						   (make-pathname :directory (pathname-directory working-directory)))))
	(format t "copying ~a to ~a"
		  cl-user::$openmcl-build-executable-path
		  executable-path)
  (ccl::copy-file (pathname cl-user::$openmcl-build-executable-path)
				  (pathname executable-path)
				  :if-exists :supersede)
  (ccl::save-application cl-user::$alpaca-image-path
						 :toplevel-function
						 #'(lambda () (ccl::main))
						 :application-class 'ccl::cocoa-application)))

(defun cl-user::make (&key (debug-port 10101))
  (progn
	(cl-user::load-alpaca :debug-port debug-port)
	(cl-user::make-alpaca)))