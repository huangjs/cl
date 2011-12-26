;;; -*- Lisp -*-

(asdf:defsystem grt-bare
	:components	
	((:file "conf")
	 (:file "util"       :depends-on ("conf"))
	 (:file "math"	     :depends-on ("util"))
	 (:file "color"	     :depends-on ("util" "math"))
	 (:file "transforms" :depends-on ("math"))
	 (:file "ray"	     :depends-on ("math" "color"))
	 (:file "pattern"    :depends-on ("math"))
	 (:file "light"      :depends-on ("math" "color"))
	 (:file "pigment"    :depends-on ("pattern" "color"))
	 (:file "object"
		:depends-on ("math" "ray" "pattern" "pigment"
			     "transforms"))
	 (:file "scene"
		:depends-on ("object" "light" "color" "pigment"))
	 (:file "camera"     :depends-on ("ray"))
	 (:file "trace"      :depends-on ("ray" "scene"))
	 (:file "render"     :depends-on ("trace" "camera"))))

