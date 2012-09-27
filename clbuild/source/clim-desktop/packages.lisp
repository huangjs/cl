(cl:defpackage :clim-desktop
  (:use :common-lisp
	:asdf))

(cl:defpackage :clhs-lookup
  (:use :common-lisp)
  (:export :symbol-lookup
           :populate-table
           :spec-lookup
           :climspec-lookup
           :clxdoc-lookup))

(cl:defpackage :abbrev
  (:use :cl :split-sequence)
  (:export :abbrev))

(cl:defpackage :clim-launcher
  (:use :common-lisp :clim)
  (:shadowing-import-from :clim-lisp-patch :interactive-stream-p)
  (:export :start :add-app))

(cl:defpackage :climfigurator
  (:use :common-lisp :clim)
  (:shadowing-import-from :clim-lisp-patch :interactive-stream-p))

(cl:defpackage :clim-lookup
  (:use :common-lisp :split-sequence)
  (:export :term-lookup :populate-table))

(cl:defpackage :clim-class-browser
  (:use :clim :clim-lisp))