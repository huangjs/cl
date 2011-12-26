;;; ****************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Rick Taube
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; ****************************************************************

;;; $Name:  $
;;; $Revision: 1.4 $
;;; $Date: 2006/05/01 14:36:39 $

(select-module portmidi)

(define *libpmrecv*
  (string-append
   (sys-dirname (sys-realpath (port-name (current-load-port))))	    
   "/"
   (if (equal? (car (sys-uname)) "Darwin")
       "libgauchepmrecv.dylib"
       "libgauchepmrecv.so")))

(c-load-library *libpmrecv*) ; must exist

(define <pm_recv_callback> (make-c-func-ptr <c-int> (list <c-long>)))
(define pm-set-input (make-c-func 'pm_set_input <c-int>
				  (list (ptr <PortMidiStream>)  <c-int> <c-int>)))
(define pm-recv-start (make-c-func 'pm_recv_start <c-int>
				   (list <c-int> <c-int> <pm_recv_callback>)))      
(define pm-recv-stop (make-c-func 'pm_recv_stop <c-int> (list)))      
(define pm-recv-state (make-c-func 'pm_recv_state <c-int> (list)))      
(define pm-priority-min (make-c-func 'pm_priority_min <c-int> (list)))      
(define pm-priority-max (make-c-func 'pm_priority_max <c-int> (list)))


;;; Lisp api

(define pm:*pm-recv-callback* (lambda (x) x values))

(define %pm_receive_callback
  (lambda (m)
    (if pm:*pm-recv-callback*
	( pm:*pm-recv-callback* m))
    0))

(define (pm:recv-start pr res)
  (pm-recv-start pr res %pm_receive_callback))

(define pm:recv-stop pm-recv-stop)
(define pm:recv-state pm-recv-state)
(define (get-priority-range)
  (list (pm-priority-min) (pm-priority-max)))
(define pm:set-input pm-set-input)

(export pm:recv-start pm:recv-stop pm:recv-state
	pm:get-priority-range pm:set-input pm:*pm-recv-callback*
	)

;;; EOF
