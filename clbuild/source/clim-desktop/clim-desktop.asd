;; -*- Mode: Lisp -*-

(cl:defpackage :clim-desktop-system
  (:use :common-lisp
	:asdf))

(in-package :clim-desktop-system)

(defsystem :clim-desktop
  :depends-on (:mcclim
	       :split-sequence
	       :cl-ppcre
	       :clim-listener
	       :beirc
	       :closure
	       :clouseau
	       :climacs)
  :description "System providing integration between a range of CLIM
applications."
  :version "0.2"
  :author "Dwight Holman"
  :licence ""
  :components ((:file "packages")
               (:file "EDITME" :depends-on ("packages"))
	       (:file "abbrev" :depends-on ("EDITME"))
	       (:file "clhs-lookup" :depends-on ("abbrev"))
	       (:file "beirc" :depends-on ("clim-launcher"))
               (:file "closure" :depends-on ("beirc" "clim-launcher" "clhs-lookup"))
	       (:file "climacs" :depends-on ("EDITME"))
	       #+nil (:file "debugger" :depends-on ("EDITME"))
	       (:file "listener")
	       (:file "clim-launcher" :depends-on ("EDITME"))))
