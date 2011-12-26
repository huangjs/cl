;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          aima.asd
;;;; Purpose:       ASDF definition file for Aima
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Sep 2002
;;;;
;;;; $Id: aima.asd 9743 2004-07-08 17:51:56Z kevin $
;;;;
;;;; This file, part of cl-aima, is Copyright (c) 2002 by Kevin M. Rosenberg
;;;;
;;;; cl-aima users are granted the rights to distribute and use this software
;;;; as governed by the terms of the GNU Lesser General Public License 
;;;; (http://www.gnu.org/licenses/lgpl.html)
;;;; *************************************************************************

(in-package :asdf)


(defclass aima-module (module) ())

#+cmu
(defclass cl-modifying-file (cl-source-file) ())

(let ((loaded nil)
      (compiled nil))
   #+cmu
   (defmethod perform ((op load-op) (c cl-modifying-file))
     (ext:without-package-locks
        (call-next-method)))
   
   #+cmu
   (defmethod perform ((op compile-op) (c cl-modifying-file))
     (ext:without-package-locks
        (call-next-method)))
   
   #+cmu
   (defmethod source-file-type ((c cl-modifying-file) (s module))
     "lisp")

  (defmethod perform ((op load-op) (c aima-module))
    (#+cmu ext:without-package-locks
     #-(or cmu) progn
      (cl-user::aima-load 'cl-user::all)
      (setq loaded t)))

  (defmethod perform ((op compile-op) (c aima-module))
    (#+cmu ext:without-package-locks
     #-(or cmu) progn
      (cl-user::aima-compile)
      (setq compiled t)))
   
  (defmethod operation-done-p ((op load-op) (c aima-module))
    loaded)

  (defmethod operation-done-p ((op compile-op) (c aima-module))
    compiled))

(defsystem :aima
    :name "cl-aima"
    :author "Peter Norvig"
    :version "1.0"
    :maintainer "Kevin M. Rosenberg <kmr@debian.org>"
    :licence "Peter Norvig's DFSG-compliant license"
    :description "Artificial Intelligence: A Modern Approach Source Code"
    :long-description "Source code from the book Artificial Intelligence: A Modern Approach by Peter Norvig."
    :components ((:cl-modifying-file "aima")
		 (:aima-module :aima :depends-on ("aima"))))






