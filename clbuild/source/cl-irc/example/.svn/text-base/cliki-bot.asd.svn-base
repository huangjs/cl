;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cliki-bot-system
    (:use #:cl #:asdf))

(in-package #:cliki-bot-system)

(defsystem cliki-bot
    :name "cliki-bot"
    :author "Brian Mastenbrook"
    :version "0.1.0"
    :licence "MIT"
    :description "IRC bot for SBCL"
    :depends-on
      (:cl-irc :cl-ppcre :split-sequence :trivial-http)
    :properties ((#:author-email . "cl-irc-devel@common-lisp.net")
                 (#:date . "$Date$")
                 ((#:albert #:output-dir) . "doc/api-doc/")
                 ((#:albert #:formats) . ("docbook"))
                 ((#:albert #:docbook #:template) . "book")
                 ((#:albert #:docbook #:bgcolor) . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :components ((:file "mp2eliza")
                 (:file "eliza-rules"
                        :depends-on ("mp2eliza"))
		 (:file "steel-bazooka")
                 (:file "cliki"
                        :depends-on ("mp2eliza" "steel-bazooka"))))
