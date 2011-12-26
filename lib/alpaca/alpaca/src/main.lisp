;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: main.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;; 
;;; a minimal Cocoa app skeleton for openmcl
;;; main program definition

(in-package "CCL")

;;; --------------------------------------------------
;;; cocoa framework
;;; --------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-interface-dir :cocoa))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (open-shared-library "/System/Library/Frameworks/Cocoa.framework/Cocoa"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "OBJC-SUPPORT"))

(defun nslog (c)
  (let* ((rep (format nil "~a" c)))
    (with-cstrs ((str rep))
      (with-nsstr (nsstr str (length rep))
	(#_NSLog #@"Logging: %@" :address nsstr)))))

(defun description (c)
  (with-autorelease-pool
   (lisp-string-from-nsstring
	(send c 'description))))

(defun init-cocoa-application ()
  (with-autorelease-pool
   (let* ((appclass (#_NSClassFromString (%make-nsstring "NSApplication")))
		  (app (send appclass 'shared-application))
		  (main-bundle (send (@class ns-bundle) 'main-bundle))
		  (main-bundle-path-nsstring (send main-bundle 'bundle-path))
		  (main-bundle-path (pathname (lisp-string-from-nsstring main-bundle-path-nsstring)))
		  (info-dict (send main-bundle 'info-dictionary))
		  (main-nib-name (send info-dict :object-for-key #@"NSMainNibFile")))
	 ;; ----------------------------------------
	 ;; init important paths
	 (setq cl-user::$alpaca-bundle-path main-bundle-path)
	 ;; ----------------------------------------
	 ;; load the application nib
	 (send (@class ns-bundle)
		   :load-nib-named main-nib-name
		   :owner app)
	 app)))

(defun run-event-loop ()
  (%set-toplevel nil)
  (let* ((app *NSApp*))
	(let ((debug-port (debug-server-port)))
	  (nslog (format nil "debug port is ~S" debug-port))
	  (when debug-port
		(remote-repl::spawn-server :port debug-port)))
    (loop
	(handler-case (send (the ns-application app) 'run)
	  (error (c) (nslog c)))
	(unless (send app 'is-running)
	  (return)))
    ;; This is a little funky (OK, it's a -lot- funky.) The
    ;; -[NSApplication _deallocHardCore:] method wants an autorelease
    ;; pool to be established when it's called, but one of the things
    ;; that it does is to release all autorelease pools.  So, we create
    ;; one, but don't worry about freeing it ...
    (create-autorelease-pool)
    (objc-message-send app "_deallocHardCore:" :<BOOL> #$YES :void)))

;;; ----------------------------------------------------------------------
;;; application main
;;; ----------------------------------------------------------------------

;;; this becomes the main pprocess in the built application
(defun main ()
  (with-autorelease-pool
   (or *NSApp* (setq *NSApp* (init-cocoa-application)))
   (run-event-loop)))

