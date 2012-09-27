;; -*- lisp -*-
;; climplayer -- a frontend for mplayer

;; Copyright (C) 2005  Thomas Persson

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

(defpackage climplayer-system
  (:use :common-lisp :asdf))

(in-package :climplayer-system)

(defsystem "climplayer"
  :depends-on (:mcclim :clim-listener :cl-ppcre :split-sequence)
  :components ((:file "packages")
               (:file "playlist" :depends-on ("packages"))
               (:file "directory-browser" :depends-on ("playlist"))
               (:file "gui" :depends-on ("playlist" "directory-browser"))))
