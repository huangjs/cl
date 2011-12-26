;;; -*- Lisp -*-

(defpackage #:grt-system (:use #:cl #:asdf))
(in-package #:grt-system) 
	    
#+(and cmu common-lisp-controller)
(progn
  (let ((min-gc-limit 12000000))
	(if (< ext:*bytes-consed-between-gcs* min-gc-limit)
	    (setq ext:*bytes-consed-between-gcs* min-gc-limit)))
  (setq ext:*efficiency-note-cost-threshold* 100)
  (setq ext:*gc-verbose* nil))


(defsystem grt
    #+common-lisp-controller :depends-on #+common-lisp-controller (:sdl)
    :components	
    ((:file "conf")
     (:file "util"       :depends-on ("conf"))
     (:file "math"	   :depends-on ("util"))
     (:file "color"	   :depends-on ("util" "math"))
     (:file "transforms" :depends-on ("math"))
     (:file "ray"	   :depends-on ("math" "color"))
     (:file "pattern"    :depends-on ("math"))
     (:file "light"      :depends-on ("math" "color"))
     (:file "pigment"    :depends-on ("pattern" "color"))
     (:file "object"
	    :depends-on ("math" "ray" "pattern" "pigment"
				"transforms"))
     (:file "scene"
	    :depends-on ("object" "light" "pigment" "color"))
     (:file "camera"     :depends-on ("ray"))
     (:file "trace"      :depends-on ("ray" "scene"))
     (:file "window"     :depends-on ("color"))
     (:file "render"     :depends-on ("trace" "window" "camera"))))
