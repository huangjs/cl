;;; system definition for beirc. -*- lisp -*-

(cl:defpackage :beirc.system
  (:use :cl :asdf))

(cl:in-package :beirc.system)

(defsystem :beirc
  :depends-on (:mcclim :cl-irc :split-sequence :cl-ppcre :cl-fad)
  :components ((:file "package")
               (:file "variables" :depends-on ("package"))
               (:file "events" :depends-on ("package"))
               (:file "receivers" :depends-on ("package" "variables" "events"))
               (:file "presentations" :depends-on ("package" "variables" "receivers"))
               (:file "message-display" :depends-on ("package" "variables" "presentations"))
               (:file "application" :depends-on ("package" "variables" "presentations" "events" "receivers"))
               (:file "message-processing" :depends-on ("package" "variables" "receivers" "application"))
	       (:file "post-message-hooks" :depends-on ("package" "sound-player"))
	       ;; we use the post-message-hook definer here.  This is
	       ;; probably wrong, and the dependency should be
	       ;; removed. [2006/04/06:rpg]
	       (:file "sound-player" :depends-on ("package" "variables"))
	       ))
