;;; cmucl startup file

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (load (merge-pathnames "cl/startup-common.lisp" (user-homedir-pathname))))


