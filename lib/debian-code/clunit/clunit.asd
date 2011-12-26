;;;; -*- Mode: Lisp; Package: User; -*-

(in-package :asdf)

(defsystem :clunit
  :perform (load-op :after (op clunit)
		    (pushnew :clunit cl:*features*))
  :components ((:file "clunit")))

