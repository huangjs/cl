;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           package.lisp
;;;;  Purpose:        Package definition for reversi
;;;;  Programer:      Kevin M. Rosenberg
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: package.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg 
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(in-package #:cl-user)

(defpackage #:reversi
  (:use #:common-lisp
	#+clisp #:ext
	#+clim #:clim
	#+clim #:clim-sys)
  #+clim
  (:shadowing-import-from :clim :pathname)
  #+clim
  (:shadowing-import-from :clim :interactive-stream-p)
  #+clim
  (:shadowing-import-from :clim :boolean)
  
  (:export
   #:reversi
   #:random-reversi-series
   #:round-robin
   #:reversi-series
   #:human
   #:iago
   #:alpha-beta-searcher
   #:alpha-beta-searcher2
   #:alpha-beta-searcher3
   #:count-difference
   #:weighted-squares
   #:modified-weighted-squares

   #:text-reversi
   #+clim #:clim-reversi
))
