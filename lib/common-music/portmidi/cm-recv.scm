;;; ****************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Rick Taube
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; ****************************************************************


;(format #t "MMMMMMMMMMMMM=~S" (current-module))


;;; $Name:  $
;;; $Revision: 1.2 $
;;; $Date: 2006/05/01 14:34:33 $

(import portmidi)

(define-method* (recv (io <portmidi-stream>) . args)
  (with-args (args &key (resolution 5) (priority 20))
    (if (io-open io)
	(if (= (pm:recv-state) 0)
	    (let ((data (slot-ref io 'receive-data)))
	      (if (= (pm:set-input (list-ref data 0)
				   (list-ref data 1)
				   (list-ref data 2)) 
		     0)
		  (if (= (pm:recv-start priority resolution ) 0)
		      (format #t "; recv running on stream ~s~%"
			      io))))
	    (warn "recv is already running on ~s" io))
	(warn "stream ~s is not open" io))
    (values)))

(define-method* (recv-stop (io <portmidi-stream>))
  (if (io-open io)
      (if (= (pm:recv-state) 1)
	  (begin
	    (pm:recv-stop)
	    (format #t "; recv stopped on stream ~s" io))
	  (warn "recv is not running on ~s" io)))
  (values))

(define-method* (recv-set! (io <portmidi-stream>) hook . args)
  (with-args (args &key recv-mode)
    recv-mode
    (if (io-open io)
	(if (procedure? hook)
	    (begin
	      (set! pm:*pm-recv-callback* 
		    (lambda (m) ( hook (pm-message->midi-message m)))))
	      (begin
		(set! pm:*pm-recv-callback* (lambda (x) x (values)))
		#f))
	    (warn "stream ~s is not open" io))
    (values)))

(define-method* (recv? (io <portmidi-stream>) . args)
  (with-args (args &optional state)
    state
    (if (io-open io)
	(if (= (pm:recv-state) 0)
	    ':stopped
	    ':running)
	(warn "stream ~s is not open" io)))
  (values))

;;; EOF
