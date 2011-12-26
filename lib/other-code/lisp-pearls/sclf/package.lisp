;;;  package.lisp --- packages description

;;;  Copyright (C) 2006, 2007 by Walter C. Pelissero

;;;  Author: Walter C. Pelissero <walter@pelissero.de>
;;;  Project: sclf

#+cmu (ext:file-comment "$Module: package.lisp, Time-stamp: <2007-03-15 11:29:52 wcp> $")

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public License
;;; as published by the Free Software Foundation; either version 2.1
;;; of the License, or (at your option) any later version.
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free
;;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;;; 02111-1307 USA

(in-package :cl-user)

(defpackage :sclf
  (:use :common-lisp)
  (:export #:be #:be*
	   #:with-gensyms
	   #:d+
	   #:s+
	   #:list->string
	   #:string-starts-with #:string-ends-with
	   #:aif #:awhen #:acond #:aand #:acase #:it
	   #:+whitespace+
	   #:string-trim-whitespace
	   #:string-right-trim-whitespace
	   #:string-left-trim-whitespace
	   #:whitespace-p
	   #:position-any
	   #:+month-names+
	   #:find-any
	   #:split-at
	   #:split-string-at-char
	   #:week-day->string
	   #:month->string
	   #:month-string->number
	   #:add-months #:add-days
	   #:read-whole-stream
	   #:read-file #:read-binary-file
	   #:string-concat
	   #:gcase
	   #:string-truncate
	   #:promise #:force #:lazy #:deflazy
	   #:copy-stream #:copy-file
	   #:keywordify
	   #:until
	   #:year #:month #:day #:week-day #:week #:day-of-the-year
	   #:beginning-of-week #:end-of-week
	   #:next-week-day #:next-monday #:full-weeks-in-span
	   #:beginning-of-first-week #:end-of-last-week
	   #:beginning-of-month #:end-of-month
	   #:run-pipe
	   #:locate-system-program
	   #:run-system-program
	   #:run-shell-command
	   #:exit-code
	   #:with-open-pipe
	   #:*bourne-shell*
	   #:*tmp-file-defaults*
	   #:temp-file-name
	   #:open-temp-file
	   #:with-temp-file
	   #:file-size
	   #:getenv
	   #:time-string #:iso-time-string #:parse-iso-time-string
	   #:soundex
	   #:string-soundex=
	   #:lru-cache
	   #:getcache
	   #:print-time-span
	   #:souble-linked-list #:limited-list #:sorted-list
	   #:insert #:size
	   #:double-linked-element #:dle-value #:dle-remove #:dle-map #:do-dle :do-dle*
	   #:sl-map #:do-dll #:do-dll*
	   #:dll-find #:dll-find-cursor
	   #:push-first #:push-last #:dll-remove
	   #:pop-first #:pop-last
	   #:leap-year-p #:last-day-of-month
	   #:getuid #:get-logname
	   #:pathname-as-directory #:pathname-as-file
	   #:alist->plist #:plist->alist
	   #:byte-vector->string
	   #:string->byte-vector
	   #:outdated-p
	   #:with-hidden-temp-file
	   #:let-slots
	   #:*decimal-point*
	   #:*thousands-comma*
	   #:format-amount
	   #:with-package
	   #:make-directory
	   #:make-temp-directory
	   #:with-temp-directory
	   #:delete-directory
	   #:delete-directory-tree
	   #:do-directory-tree
	   #:traverse-directory-tree
	   #:map-directory-tree
	   #:find-files
	   #:directory-p
	   #:string-escape
	   #:substitute-sequence
	   #:bytes-simple-string
	   #:make-lock-files
	   #:with-lock-files
	   #:getpid
	   #:on-error
	   #:floor-to
	   #:round-to
	   #:ceiling-to
	   #:insert-in-order
	   #:queue #:make-queue #:queue-append #:queue-pop))
