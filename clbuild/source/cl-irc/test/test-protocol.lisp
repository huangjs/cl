;;;; $Id: test-protocol.lisp 184 2007-04-19 21:54:49Z ehuelsmann $
;;;; $URL: svn://common-lisp.net/project/cl-irc/svn/trunk/test/test-protocol.lisp $

;;;; See the LICENSE file for licensing information.

(in-package :cl-irc-test)

(defvar *nick1* "kire")
(defvar *nick2* "k|[]re")
(defvar *nick3* "k^{]re")
(defvar *chan1* "#liSP")

;;normalize tests are broken because they need a connection these days
;;(deftest normalize-nickname.1 (irc:normalize-nickname *nick1*) "kire")
;;(deftest normalize-nickname.2 (irc:normalize-nickname *nick2*) "k\\[]re")
;;(deftest normalize-nickname.3 (irc:normalize-nickname *nick3*) "k~[]re")

;;(deftest normalize-channel-name.1 (irc:normalize-channel-name *chan1*) "#lisp")
