;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: delegate.lisp,v 1.1.1.1 2004/03/25 06:43:07 mevins Exp $
;;; 
;;; the lisp application delegate

(in-package "CCL")


;;; ======================================================================
;;; METHODS AND FUNCTIONS
;;; ======================================================================

(define-objc-method ((:void :application-did-finish-launching notification)
					 lisp-application-delegate)
  (declare (ignore notification))
  (init-alpaca-keymaps)
  ;; look for a '.alpaca' file:
  ;; 1. look for ~/.alpaca
  ;; 2. if 1 fails, look for environment variable $ALPACA_INIT_FILE
  ;; (maybe I should add a preferences panel with a place to look for
  ;; the init file as one preference entry)
  (let* ((*package* (find-package "CCL"))
		 (tilde-path #@"~/.alpaca")
		 (user-init-file (lisp-string-from-nsstring
						  (send tilde-path 'string-by-expanding-tilde-in-path)))
		 (env-init-file (getenv "ALPACA_INIT_FILE")))
	;; always load the user init if it's there
	(if (and user-init-file
			 (probe-file user-init-file))
		(load user-init-file)
	  ;; if the user file isn't there, try the environment variable
	  (when (and env-init-file
				 (probe-file env-init-file))
		(load env-init-file)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :new-text-editor sender)
					   lisp-application-delegate)
	(let ((doc-controller (send (@class ns-document-controller) 'shared-document-controller)))
	  (send doc-controller
			:open-untitled-document-of-type (%make-nsstring "NSStringPboardType")
			:display t))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-objc-method ((:void :new-rtf-editor sender)
					   lisp-application-delegate)
	(let ((doc-controller (send (@class ns-document-controller) 'shared-document-controller)))
	  (send doc-controller
			:open-untitled-document-of-type
			(%make-nsstring "NeXT Rich Text Format v1.0 pasteboard type")
			:display t))))

(define-objc-method ((:void :application-open-untitled-file app)
		     lisp-application-delegate)
  (send self :new-rtf-editor app))

(define-objc-method ((:void :new-listener sender)
					 lisp-application-delegate)
  (declare (ignore sender))
  (send (send (@class ns-document-controller) 'shared-document-controller)
		:open-untitled-document-of-type #@"Listener" :display t))


