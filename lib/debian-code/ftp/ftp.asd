;;;; -*- Mode: Lisp -*-
;;;; Author: Matthew Danish <mrd@debian.org>
;;;; See LICENSE file for copyright details.

(asdf:defsystem ftp
    :name "cl-ftp"
    :author "Matthew Danish <mdanish@andrew.cmu.edu>"
    :version "1.3"
    :maintainer "Matthew Danish <mdanish@andrew.cmu.edu>"
    :licence "MIT/X style"
    :description "FTP library"
    :long-description "Provides FTP client functionality"
    :components ((:file "ftp"))
    :depends-on (split-sequence #-allegro acl-compat))

