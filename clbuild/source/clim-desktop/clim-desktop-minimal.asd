;; -*- Mode: Lisp -*-

(cl:defpackage :clim-desktop-minimal-system
  (:use :common-lisp
	:asdf))

(in-package :clim-desktop-minimal-system)

(defsystem :clim-desktop-minimal
  :depends-on (:mcclim
	       :split-sequence
	       :cl-ppcre
	       :clim-listener
               :clouseau
	       :climacs)
  :description "Minimal CLIM Desktop without Beirc and Closure (IRC
client and webbrowser)."
  :version "0.2"
  :author "Dwight Holman"
  :licence ""
  :components ((:file "packages")
               (:file "EDITME" :depends-on ("packages"))
	       (:file "abbrev" :depends-on ("EDITME"))
	       (:file "climacs" :depends-on ("EDITME"))
	       (:file "debugger" :depends-on ("EDITME"))
	       (:file "listener")
	       (:file "clim-launcher" :depends-on ("EDITME"))))