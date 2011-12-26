;;; -*- Mode: Lisp -*-  ;;;; Allegro CL startup file 

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (load (merge-pathnames "cl/startup-common.lisp" (user-homedir-pathname))))

;;; start swank
;;(setf swank:*use-dedicated-output-stream* t)
;;(setf swank:*dedicated-output-stream-port* 4006)
;;(setq swank:*communication-style* :fd-handler)
;; (setf swank:*globally-redirect-io* t)
;; (ignore-errors
;;   (swank:create-server :dont-close t :coding-system "utf-8-unix" :port 4009))

