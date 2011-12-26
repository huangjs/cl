(in-package :cl-user)

(asdf:defsystem :hjs-utils
  :serial t
  :components (	;;(:file "cps")
	       (:file "expt-mod")
	       (:file "extremum")
	       (:file "lisp-unit")
	       (:file "read-delimited")
	       (:file "sharpL")
	       (:file "with")
	       (:file "memoize")
	       (:file "comparisons")
	       (:file "binary-types")
	       ;;(:file "shelisp")
	       ;;(:file "ulimyhmpqs")
	       ;;(:file "save-object-10.2")
	       ;;(:file "terminfo")
	       ;;(:file "collecting")
	       ;;(:file "collect")
	       ;;(:file "clunit")
	       ;;(:file "cover")
	       ;;(:file "screamer+")  ; loaded later with screamer
	       ))
