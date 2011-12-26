;;; -*- Lisp -*-
;;; alpaca
;;; Version: $Id: openmcl-patches.lisp,v 1.1.1.1 2004/03/25 06:43:11 mevins Exp $
;;; 
;;; patches to opnemccl for alpaca

(in-package "CCL")

;;; ----------------------------------------------------------------------
;;; patched read-loop
;;; ----------------------------------------------------------------------
;;; this code patches openmcl source file l1-readloop-lds.lisp
;;; this patch leaves read-loop unchanged except that it pushes
;;; the read form onto a globally-accessible history

(defparameter *read-history* nil)
(defparameter *last-read-history-access-index* 0)

(let ((*warn-if-redefine-kernel* nil))

										;This is the part common to toplevel loop and inner break loops.
  (defun read-loop (&key (break-level *break-level*)
						 (prompt-function #'(lambda () (print-listener-prompt t)))
						 (input-stream *terminal-io*)
						 (output-stream *terminal-io*))
	(let* ((*break-level* break-level)
		   (*last-break-level* break-level)
		   *loading-file-source-file*
		   *in-read-loop*
		   (*listener-p* t)
		   *** ** * +++ ++ + /// // / -
		   form)
	  (loop
	   (restart-case
        (catch :abort					;last resort...
          (loop
		   (catch-cancel
			(loop                
			 (setq *loading-file-source-file* nil
				   *in-read-loop* nil
				   *break-level* break-level)
			 (setq form (toplevel-read :input-stream input-stream
									   :output-stream output-stream
									   :prompt-function prompt-function))
			 ;; !!! here's the patch !!!
			 (pushnew form *read-history* :test #'equalp)
			 ;; !!! end of patch !!!
			 (if (eq form *eof-value*)
				 (if (eof-transient-p (stream-device input-stream :input))
					 (progn
					   (stream-clear-input *terminal-io*)
					   (abort-break))
				   (quit))
			   (or (check-toplevel-command form)
				   (toplevel-print
					(toplevel-eval form))))))
		   (format *terminal-io* "~&Cancelled")))
        (abort () :report (lambda (stream)
                            (if (eq break-level 0)
								(format stream "Return to toplevel.")
                              (format stream "Return to break level ~D." break-level)))
										#| ; Handled by interactive-abort
										; go up one more if abort occurred while awaiting/reading input               
			   (when (and *in-read-loop* (neq break-level 0))
			   (abort))
			   |#
               )
        (abort-break () 
                     (unless (eq break-level 0)
                       (abort))))
      (clear-input *terminal-io*)
      (format *terminal-io* "~%"))))


)