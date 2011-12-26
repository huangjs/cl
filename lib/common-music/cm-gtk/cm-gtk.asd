#-:asdf 
(error "CM-GTK requires ASDF to load.")
#-(or cmu openmcl sbcl)
(error "Sorry CM-GTK, only run in CMUCL, OpenMCL or SBCL.")

(in-package :cl-user)

; (load "/Lisp/cm-gtk/cm-gtk.asd")
; (directory "ccl:darwin-headers;cocoa;*.*")
; (directory "ccl:darwin-headers;gtk2;*.*")
; (probe-file "ccl:darwin-headers;gtk2;")
; (compile-file "/Lisp/cm-gtk/gtkffi-openmcl.lisp")

;#+openmcl
;(ccl::add-logical-pathname-translation 
; "ccl" 
; (list "darwin-headers;*.*"
;       (namestring "/Lisp/ccl/darwin-headers/*.*")))
#+openmcl
(ccl::add-logical-pathname-translation 
 "ccl" 
 (list "darwin-headers;gtk2;*.*"
       (make-pathname :name :wild :type :wild
                      :directory
                      (append (pathname-directory *load-truename*)
                              (list "openmcl"))
                      :defaults *load-truename*)))

(asdf:defsystem :cm-gtk
  :description "GTK Interface for Common Music"
  :author "Rick Taube <taube (at) uiuc (dot) edu>"
  :licence "LLGPL"
  :depends-on (cm)
  :serial t
  :components (
               #+cmu (:file "gtkffi-cmusbcl")
               #+openmcl (:file "gtkffi-openmcl")
               #+sbcl (:file "gtkffi-cmusbcl")
	       (:file "periodic")
               (:file "plotter")
               (:file "support")
               (:file "widgets")
               (:file "editing")
               (:file "drawing")
               (:file "eventio")
               )
  )



               

        
