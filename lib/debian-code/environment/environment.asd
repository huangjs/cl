;;;; -*- Mode: Lisp -*-

(in-package :asdf)

(defsystem :environment
    :perform (load-op :after (op environment)
		      (pushnew :environment cl:*features*))
    :components
    ((:file "env-package")
     (:file "feature-tagged-type-class" :depends-on ("env-package"))
     (:file "software"
	    :depends-on ("env-package"
			 "feature-tagged-type-class"))
     (:file "machine"
	    :depends-on ("env-package"
			 "feature-tagged-type-class"))
     (:file "operating-system"
	    :depends-on ("software"))
     (:file "environment"
	    :depends-on ("operating-system" "machine"))
     (:file "init-environment"
	    :depends-on ("environment"
			 "software"
			 "operating-system"
			 "machine"
			 ))
     (:module impl-dependent
	      :depends-on ("init-environment")
	      :components
	      ((:file
		#+clisp "clisp"
		#+lispworks "lispworks"
		#+allegro "allegro"
		#+cmu "cmucl"
		#+sbcl "sbcl"
		#+scl "scl"
		#+openmcl "openmcl"
		#+lcl "lcl"
		#+cormanlisp "corman"
		)))
     
     (:file "system-info"
	    :depends-on ("impl-dependent" "utilities"))
     (:file "utilities"
	    :depends-on ("init-environment" "impl-dependent"))
     ))
