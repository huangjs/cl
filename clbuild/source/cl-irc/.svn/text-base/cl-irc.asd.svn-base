;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-irc-system
    (:use #:cl #:asdf))

(in-package #:cl-irc-system)

(defsystem cl-irc
    :name "cl-irc"
    :author "Erik Enge & Contributors"
    :version "0.8-dev"
    :licence "MIT"
    :description "Common Lisp interface to the IRC protocol"
    :depends-on (:split-sequence :usocket :flexi-streams)
    :properties ((#:author-email . "cl-irc-devel@common-lisp.net")
                 (#:date . "$Date$")
                 ((#:albert #:output-dir) . "doc/api-doc/")
                 ((#:albert #:formats) . ("docbook"))
                 ((#:albert #:docbook #:template) . "book")
                 ((#:albert #:docbook #:bgcolor) . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :components ((:file "package")
                 (:file "variable"
                        :depends-on ("package"))
                 (:file "utility"
                        :depends-on ("variable"))
                 (:file "parse-message"
                        :depends-on ("utility"))
                 (:file "protocol"
                        :depends-on ("parse-message"))
                 (:file "command"
                        :depends-on ("protocol"))
                 (:file "event"
                        :depends-on ("command"))))
