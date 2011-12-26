;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Rick Taube
;;; This program is free software; you can redistribute it and/or   
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.27 $
;;; $Date: 2007/07/01 00:06:47 $

;;;
;;; real time stubs and core definitinos
;;;

(define *pm-not-loaded-error* "recv for portmidi not loaded.")

(define-method* (recv (io <portmidi-stream>) . args)
  (with-args (args &key resolution priority)
    io resolution priority
    (err *pm-not-loaded-error* )))

(define-method* (recv-stop (io <portmidi-stream>))
  io
  (err *pm-not-loaded-error*))

(define-method* (recv-set! (io <portmidi-stream>) hook . args)
  (with-args (args &key recv-mode)
    recv-mode io hook
    (err *pm-not-loaded-error*)))

(define-method* (recv? (io <portmidi-stream>))
  (err *pm-not-loaded-error*))

(define (rtserr fn args)
  (err "Attempt to call ~s without RTS loaded."  (cons fn args)))

(define (rts . args) (rtserr 'rts args))
(define (rts? . args) args #f)
(define (rts-stop . args) (rtserr 'rts-stop args))
(define (rts-pause . args) (rtserr 'rts-pause args))
(define (rts-continue . args) (rtserr 'rts-continue args))
(define (rts-enqueue . args) (rtserr 'rts-enqueue args))
(define (rts-now . args) (rtserr 'rts-now args))
(define (rts-thread?) #f)

;;; eof
