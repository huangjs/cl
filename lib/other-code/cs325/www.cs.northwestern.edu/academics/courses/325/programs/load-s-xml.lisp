
(in-package :cs325-user)

;;; ASDF must be loaded first!
;;;  - Allegro 7 and SBCL users: (require :asdf)
;;;  - Allegro 6 users, download asdf.lisp from CS 325 library and load
;;;  - Portable AServe users: load from portableaserve/lib/asdf.lisp

#+lispworks (require "comm")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew (merge-pathnames "s-xml/" *load-pathname*)
           asdf:*central-registry* :test #'equal)
  
  (pushnew (merge-pathnames "s-xml-rpc/" *load-pathname*)
           asdf:*central-registry* :test #'equal)
  
  (asdf:operate 'asdf:load-op 's-xml)
  
  (asdf:operate 'asdf:load-op 's-xml-rpc))

(use-package :s-xml-rpc)
