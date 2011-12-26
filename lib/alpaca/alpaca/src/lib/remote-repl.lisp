;;;; -*- mode: lisp -*-
;;;;
;;;; $Id: remote-repl.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;;;
;;;; This is a remote read-eval-print-loop (REPL) server for OpenMCL.
;;;; Telnet to the port specified to use the remote REPL facility.
;;;; For added security, local-only should be t (the default): this
;;;; way only local connections will be accepted. Use ssh to login to
;;;; the machine in a secure way or ssh port forwarding
;;;; to export this functionality to remote sites in a secure way.
;;;;
;;;; openmcl -l remote-repl -e '(remote-repl:server)'
;;;;
;;;; telnet localhost 25365
;;;;
;;;; Copyright (C) 2003 Sven Van Caekenberghe.
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage :remote-repl
  (:use common-lisp ccl)
  (:export
   "SERVER"
   "SPAWN-SERVER"
   "MCL-REPL"
   "SIMPLE-REPL"
   "EXIT")
  (:documentation "A remote read-eval-print loop for OpenMCL"))

(in-package :remote-repl)

(defun server (&key (port 24365) (local-only t) (handler #'mcl-repl))
  "Enters a server loop on port that offers a remote read-eval-print-loop."
  (with-open-socket (server-socket :connect :passive
				   :local-port port
				   :reuse-address t)
    (loop
     (let ((client-socket (accept-connection server-socket)))
       (if (or (not local-only)
	       (= (remote-host client-socket)
		  (local-host client-socket)))
	   (funcall handler client-socket)
	 (progn
	   (format client-socket "Connection refused.~%")
	   (finish-output client-socket)
	   (close client-socket)))))))

(defun spawn-server (&key (port 24365) (local-only t) (handler #'mcl-repl))
  "Spawns a server process on port that offers a remote read-eval-print-loop."
  (process-run-function "remote-repl-server"
			#'(lambda () (server :port port
					     :local-only local-only
					     :handler handler))))

(defun mcl-repl (socket)
  "Make an OpenMCL repl connected to socket"
  (ccl::make-mcl-listener-process "remote-repl"
				  socket
				  socket
				  #'(lambda () (close socket))))

(defun exit ()
  "Kill the current remote repl process"
  (process-kill *current-process*))

;;; the rest is an older, less elegant/interesting approach
;;; it should however be more portable to other cl implementations

(defun simple-repl (socket)
  "Make a simple repl connected to socket"
  (process-run-function "remote-repl"
			#'remote-repl-loop
			socket))

(defun remote-repl-loop (socket)
  (format socket "~&Welcome to ~A ~A!~%"
	  (lisp-implementation-type)
	  (lisp-implementation-version))
  (format socket "You have just entered a remote read-eval-print-loop.~%")
  (loop
   (format socket "~&[Remote REPL] ? ")
   (finish-output socket)
   (block :rep
     (handler-bind ((error #'(lambda (c)
			       (format socket "Error: ~a~%" c)
			       (return-from :rep))))
       (let ((in (read socket nil :repl-eof nil)))
	 (if (member in '(:repl-eof :done :quit :exit))
	     (return)
	   (let ((result (eval in)))
	     (format socket "~s" result)))))))
  (format socket "Bye.~%")
  (finish-output socket)
  (close socket))

;;; eof
