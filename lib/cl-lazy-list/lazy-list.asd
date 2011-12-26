(in-package :cl-user)

(asdf:defsystem :lazy-list
  :serial t
  :components ((:file "package")
			   (:file "src" :depends-on ("package"))))

