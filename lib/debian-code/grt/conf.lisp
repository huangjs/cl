;; This file is part of GRT, a Common Lisp raytracing system
;; Copyright (C) 2002 Nikodemus Siivola
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
;; MA  02111-1307  USA

;;;; This file contains all the configuration & meta-data for GRT

(defpackage "GRT"
  (:use "COMMON-LISP"))

(in-package grt)

(defparameter +version+ "pre0.1")
(defparameter +bug-url+ "http://savannah.nongnu.org/bugs/?group=grt")

