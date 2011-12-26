(in-package :cs325-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  )

(defmop programming-language)

(defmop lisp-language (programming-language)
              :name "Lisp"
              :run-time-types "yes")

(defmop common-lisp (lisp-language)
            :name "Common Lisp"
            :compile-time-types "optional")

(defmop allegro-lisp (common-lisp)
             :name "Allegro")

(defmop allegro-windows (allegro-lisp windows-application)
                :platform "windows")

(defmop application)

(defmop windows-application (application)) 

