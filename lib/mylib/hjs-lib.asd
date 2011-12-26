(in-package :cl-user)

(asdf:defsystem :hjs-lib
  :serial t
  :components (	;; meta
	       (:file "lisp")
	       (:file "essential" :depends-on ("lisp"))
	       (:file "macro" :depends-on ("essential" "lisp"))
	       (:file "functional" :depends-on ("macro"))
	       (:file "type")
	       (:file "clos")
	       ;; data
	       (:file "sequence" :depends-on ("macro"))
	       (:file "array" :depends-on ("macro"))
	       (:file "string" :depends-on ("macro"))
	       (:file "tree" :depends-on ("macro"))
	       ;; utils
	       (:file "asdf")
	       (:file "file" :depends-on ("macro"))
	       (:file "calendar" :depends-on ("macro"))
	       (:file "database" :depends-on ("tree" "sequence"))
	       (:file "os" :depends-on ("calendar"))
	       #+sbcl (:file "sysadmin" :depends-on ("os"))
	       (:file "math" :depends-on ("macro" "functional"))
	       (:file "math/number-theory" :depends-on ("sequence"))
	       (:file "math/combinatorics" :depends-on ("sequence"))
	       (:file "lazy-sequence")
	       ;; control
	       (:file "thread-pool")
	       (:file "concurrency" :depends-on ("thread-pool"))
	       ;; system environment
	       ;;(:file "inspection")
	       )
  :depends-on (:arnesi
	       :iterate
	       :net-telent-date
	       :split-sequence
	       :infix
	       :cl-fad
	       :parse-number
	       :lazy-list
	       :portable-threads))



