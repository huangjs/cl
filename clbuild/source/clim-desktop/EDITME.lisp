(in-package :common-lisp-user)

;;; EDIT THESE
(defparameter *mcclim-directory*
  (asdf:component-pathname (asdf:find-system "mcclim")))

(defparameter clhs-lookup::*hyperspec-root*
  "http://www.lispworks.com/reference/HyperSpec/")

(defparameter clhs-lookup::*mop-root* "http://www.alu.org/mop/")

;;; LOAD THE CLIM DEBUGGER
(load (merge-pathnames "Apps/Debugger/clim-debugger.lisp" *mcclim-directory*))

;;; LOAD THE EXPERIMENTAL POINTER DOCUMENTATION
(load (merge-pathnames "Experimental/pointer-doc-hack.lisp" *mcclim-directory*))

