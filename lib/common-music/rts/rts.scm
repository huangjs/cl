;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Michael Klingbeil, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Revision: 1.4 $
;;; $Date: 2006/05/30 13:08:55 $


(define-module rts
  (use c-wrapper)
  (export rts:scheduler-start rts:scheduler-stop
	  rts:scheduler-pause rts:scheduler-continue rts:scheduler-flush
	  rts:scheduler-state? rts:scheduler-enqueue
	  rts:scheduler-lock-lisp rts:scheduler-unlock-lisp
	  rts:scheduler-add-hook! rts:scheduler-remove-hook!
	  rts:scheduler-hook-set! rts:enqueue
	  rts:scheduler-time rts:*time-format*
	  rts:*priority* rts:*policy* rts:*resolution* rts:*error-format*
	  rts:rts-thread? rts:current-thread
	  rts:random-seed rts:between rts:rescale rts:odds rts:pickl
	  rts:drunk rts:wander rts:shuffle rts:interpl rts:rhythm)
  )

(load "gauche-scheduler.scm")
(load "cm-scheduler.scm")

;;; EOF
