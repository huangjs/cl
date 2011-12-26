;;; **********************************************************************
;;; Copyright (C) 2002 Heinrich Taube (taube@uiuc.edu) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2006/06/01 23:21:25 $

;;;
;;; porting code for Gauche Scheme:
;;; http://www.shiro.dreamhost.com/scheme/gauche/index.html
;;;

(use gauche.net) ; needed for socket communication
(use gauche.fcntl) ;need to set socket to non-blocking
(use gauche.sequence)



;;gauche specific stuff for sending osc messages over udp sockets.

(define (u8vector->byte-string vec)
  (let* ((vec-len (u8vector-length vec))
         (byte-string (make-byte-string vec-len)))
    (do ((i 0 (+ i 1)))
        ((= i vec-len))
      (string-byte-set! byte-string i (u8vector-ref vec i)))
    byte-string))


;; functions/methods needed
;; (make-udp-socket host port local-port) x
;; (udp-socket-close sock) x
;; (udp-socket-shutdown sock &optional) x
;; (udp-socket-recv sock bytes &optional flags) x
;; (udp-socket-send sock mess len) x
;; (make-osc-timetag offset out) x
;; (u8vector->double vec) 

(define-class* <udp-socket> ()
  ((socket :init-value #f)
   (remote-port :init-value #f :init-keyword :remote-port)
   (remote-host :init-value #f :init-keyword :remote-host)
   (local-port :init-value #f :init-keyword :local-port))
  :name 'udp-socket)

 (define (make-udp-socket host port local-port)
   (let ((usock (make <udp-socket> :remote-host host :remote-port port :local-port local-port)))
     (slot-set! usock 'socket (make-socket PF_INET SOCK_DGRAM))
                                        ;(sys-fcntl (socket-fd (slot-ref usock 'socket)) F_SETFL O_NONBLOCK)
     (socket-bind (slot-ref usock 'socket) (make <sockaddr-in> :host "127.0.0.1" :port local-port))
     usock))


(define (udp-socket-close sock)
  (socket-close (slot-ref sock 'socket)))

(define (udp-socket-shutdown sock . args)
  (with-args (args &optional how)
    (unless how
      (set! how 2))
    (socket-shutdown (slot-ref sock 'socket) how)))

;; this does a recvfrom on socket, however must
;; handle EAGAIN error since this indicates
;; the socket is non-blocking and no data
;; is waiting. in this case, return #f

(define (udp-socket-recv sock bytes . args)
   (with-args (args &optional flags)
     (unless flags
       (set! flags 0))
     (string->u8vector (socket-recvfrom (slot-ref sock 'socket) bytes flags))))

(define (udp-socket-send sock mess bytes)
  bytes
  (socket-sendto (slot-ref sock 'socket) (u8vector->byte-string mess)
                 (make <sockaddr-in> :host (slot-ref sock 'remote-host)
                       :port (slot-ref sock 'remote-port))))

