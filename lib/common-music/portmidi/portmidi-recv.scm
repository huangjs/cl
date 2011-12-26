;;; ****************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; ****************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2006/04/29 01:33:50 $

(use c-wrapper)

(select-module portmidi)

;; load portmidi recv library
(define libpmrecv
  (if (equal? (car (sys-uname)) "Darwin")
      "./libgauchepmrecv.dylib"
    "./libgauchepmrecv.so"))

(if (file-exists? libpmrecv)
    (begin

      (c-load-library libpmrecv)
      
      (define <pm_recv_callback> (make-c-func-ptr <c-int> (list (ptr <c-void>))))      
      (define pm-set-input (make-c-func 'pm_set_input <c-int>
                                     (list (ptr <PortMidiStream>)  <c-int> <c-int>)))
      (define pm-recv-start (make-c-func 'pm_recv_start <c-int>
                                      (list <c-int> <c-int> <pm_recv_callback>)))      
      (define pm-recv-stop (make-c-func 'pm_recv_stop <c-int> (list)))      
      (define pm-recv-state (make-c-func 'pm_recv_state <c-int> (list)))      
      (define pm-priority-min (make-c-func 'pm_priority_min <c-int> (list)))      
      (define pm-priority-max (make-c-func 'pm_priority_max <c-int> (list)))
      (define (get-priority-range)
        (list (priority-min) (priority-max)))

      (define *pm-recv-callback* (lambda (x) x values))

      (define %pm_receive_callback
        (lambda (mess)
          (if *pm-recv-callback*
              (*pm-recv-callback* (pm-message->midi-message mess)))))
      
      (export pm-set-input pm-recv-start pm-recv-stop pm-recv-state
              pm-priority-min pm-priority-max *pm-recv-callback* %pm_receive_callback)
      
      ;;what is a better test for cm?
      (if %cm-version% 
          (with-module user
                       (begin
                         (import portmidi)
                         (define-method* (recv (io <portmidi-stream>) . args)
                           (with-args (args &key (resolution 5) (priority 20))
                             (if (io-open io)
                                 (if (= (pm-recv-state) 0)
                                     (begin
                                       (let ((data (slot-ref io 'receive-data)))
                                         (if (= (pm-set-input (list-ref data 0) (list-ref data 1) (list-ref data 2)) 0)
                                             (if (= (pm-recv-start priority resolution %pm_receive_callback) 0)
                                                 (format #t "recv running on stream ~s~%"io)))))
                                   (warn "recv is already running on ~s" io))
                               (warn "stream ~s is not open" io))
                             (values)))
                         (define-method* (recv-stop (io <portmidi-stream>))
                           (if (io-open io)
                               (if (= (pm-recv-state) 1)
                                   (begin
                                     (pm-recv-stop)
                                     (format #t "recv stopped on stream ~s" io))
                                 (warn "recv is not running on ~s" io)))
                           (values))
                         (define-method* (recv-set! (io <portmidi-stream>) hook . args)
                           (with-args (args &key recv-mode)
                             recv-mode
                             (if (io-open io)
                                 (if (procedure? hook)
                                     (begin
                                       (set! *pm-recv-callback* hook))
                                   (begin
                                     (set! *pm-recv-callback* (lambda (x) x (values)))
                                     #f))
                               (warn "stream ~s is not open" io))
                             (values)))
                         (define-method* (recv? (io <portmidi-stream>) . args)
                           (with-args (args &optional state)
                             state
                             (if (io-open io)
                                 (if (= (pm-recv-state) 0)
                                     ':stopped
                                   ':running)
                               (warn "stream ~s is not open" io)))
                           (values))
                         )))))
      
      


