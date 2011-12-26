;;; -*- Mode: Lisp -*-  ;;;; Allegro CL startup file 

(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (setf excl:*locale* (excl:find-locale "en_US.utf-8"))
  (load (merge-pathnames "cl/startup-common.lisp" (user-homedir-pathname))))

;;; start swank
;;(setf swank:*use-dedicated-output-stream* t)
;;(setf swank:*dedicated-output-stream-port* 4006)
;;(setq swank:*communication-style* :fd-handler)
;; (setf swank:*globally-redirect-io* t)
;; (ignore-errors
;;   (swank:create-server :dont-close t :coding-system "utf-8-unix" :port 4006))

;;; old defsystem
;;; FIXME: conflict with SBCL (coz not asdf systems)
;; (defvar *library-default-root-path* (merge-pathnames "cl/lib/" (user-homedir-pathname)))
;; (load (merge-pathnames "clocc-init" *library-default-root-path*)) ; all the initial vars, fns are in clocc-init.lisp
;; (defun load-clocc-system (system-name)
;;   (mk:oos (unless (simple-string-p system-name)
;; 	    (string-downcase (symbol-name system-name)))
;; 	  :load))
;; (defun compile-clocc-system (system-name)
;;   (mk:oos (unless (simple-string-p system-name)
;; 	    (string-downcase (symbol-name system-name)))
;; 	  :compile))


;; ;;; clocc
;; ;;; TODO: a lot of systems cannot be compiled
;; (load-clocc-system :port) ;seems a lot of package depends on it and since it's not big I decide to load it on start up.
;; ;;(load-clocc-system :cllib)					; a big collection of tools
;; ;;(load-clocc-system :ytools)					; enhanced loop and format (the author has a nice code style,btw)
;; ;;(load-clocc-system :f2cl)						; fortran to common lisp

;; ;;; I made this toplevel to make sure libraries are always loaded from right places
;; (asdf :cffi)				; popular ffi

;; ;; not needed under linux
;; ;;(pushnew #P"/usr/lib/" cffi:*foreign-library-directories* :test #'equal)
;; ;;(pushnew #P"/usr/local/lib/" cffi:*foreign-library-directories* :test #'equal)  
;; ;;;(pushnew #P"/opt/local/lib" cffi:*foreign-library-directories* :test #'equal)


(defun quit (&optional retval)
  (excl:exit retval))

