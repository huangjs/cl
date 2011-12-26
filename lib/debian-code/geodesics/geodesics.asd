;;;; -*- Mode: Lisp -*-

(in-package :asdf)


(defsystem :gd-static-equal
    :depends-on (:integrate)
    :components ((:file "geodesics")
		 (:file "staticequal" :depends-on ("geodesics"))
		 (:file "gdequations" :depends-on ("staticequal"))
		 (:file "utilities" :depends-on ("staticequal"))))

(defsystem :gd-static-unequal
    :depends-on (:integrate)
    :components (((:file "geodesics")
		  (:file "staticunequal" :depends-on ("geodesics"))
		  (:file "gdequations" :depends-on ("staticunequal"))
		  (:file "utilities" :depends-on ("staticunequal"))))

(defsystem :gd-cosmological
    :depends-on (:integrate)
    :components ((:file "geodesics")
		 (:file "general" :depends-on ("geodesics"))
		 (:file "gdequations" :depends-on ("general"))
		 (:file "utilities" :depends-on ("general"))))

(defsystem :geodesics
    :depends-on (:infix :gd-static-equal :gd-static-unequal :gd-cosmological))
