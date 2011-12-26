;; Copyright (C) 2006 Martin Bishop
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; This file contains version information.
;;
;; Code:

(in-package :stumpwm)

(defparameter *version* #.(concatenate 'string "0.0.6-CVS Compiled On " (format-time-string)))

(defun echo-version (screen)
  (echo-string screen *version*))

(define-stumpwm-command "version" ()
  (echo-version (current-screen)))
