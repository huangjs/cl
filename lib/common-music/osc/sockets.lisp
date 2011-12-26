;;; **********************************************************************
;;; Copyright (C) 2005-2006 Todd Ingalls, Rick Taube
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************


(in-package :cm)


#+openmcl 
(progn
  (defun make-udp-socket (host port local-port)
    (ccl:make-socket :type :datagram :remote-port port :remote-host host
		     :local-port local-port
		     :format :binary))

  (defmethod ccl::receive-from-no-block ((socket ccl::udp-socket) size &key buffer extract offset)
    (let ((fd (ccl::socket-device socket))
	  (vec-offset offset)
	  (vec buffer)
	  (ret-size -1))
      (when vec
	(multiple-value-setq (vec vec-offset)
	  (ccl::verify-socket-buffer vec vec-offset size)))
      (ccl::rlet ((sockaddr :sockaddr_in)
		  (namelen :signed))
	(setf (ccl::pref sockaddr :sockaddr_in.sin_family) #$AF_INET)
	(setf (ccl::pref sockaddr :sockaddr_in.sin_addr.s_addr) #$INADDR_ANY)
	(setf (ccl::pref sockaddr :sockaddr_in.sin_port) 0)
	(setf (ccl::pref namelen :signed) (ccl::record-length :sockaddr_in))
	(ccl::%stack-block ((bufptr size))
	  (setq ret-size (#_recvfrom fd bufptr size 0 sockaddr namelen))
	  (when (> ret-size 0)
	    (unless vec
	      (setq vec (make-array ret-size
				    :element-type
				    (ecase (ccl::socket-format socket)
				      ((:text) 'base-character)
				      ((:binary :bivalent) '(unsigned-byte 8))))
		    vec-offset 0))
	    (ccl::%copy-ptr-to-ivector bufptr 0 vec vec-offset ret-size)))
	(if (> ret-size 0)
	    (cond ((null buffer)
		   vec)
		  ((or (not extract)
		       (and (eql 0 (or offset 0))
			    (eql ret-size (length buffer))))
		   buffer)
		  (t 
		   (subseq vec vec-offset (+ vec-offset ret-size))))
	    nil))))

  
  (defun udp-socket-close (sock)
    (ccl::close sock))
  
  (defun udp-socket-shutdown (sock &optional how)
    how sock ;;;shoudl have socket-shutdown here
    (values))

  (defun udp-socket-recv (sock bytes &optional flags)
    flags
    (ccl::receive-from sock bytes))
  
  (defun udp-socket-recv-no-block (sock bytes &optional flags)
    flags
    (ccl::receive-from-no-block sock bytes))

  (defun udp-socket-send (sock mess len)
    (ccl::send-to sock mess len)))


#+sbcl
(in-package :sb-alien)

#+sbcl 
(progn
  ;;;
;;; sbcl alien stuff for sendto 
  
  (load-shared-object "libc.dylib")
  
  (define-alien-type ssize_t int)
  
  (define-alien-type size_t int)
  
  (define-alien-type sockaddr (struct SB-BSD-SOCKETS-INTERNAL::SOCKADDR-IN))
  
  (declaim (inline sendto))
  
  (define-alien-routine ("sendto" sendto)
    ssize_t
    (s int) (msg (* t)) (len ssize_t) (flags int) (to (* sockaddr)) (tolen int))
  
  (in-package :cm)
  
  (defclass udp-socket (sb-bsd-sockets::inet-socket)
    ((sb-bsd-sockets::protocol :initform :udp)
     (sb-bsd-sockets::type :initform :datagram)
     (remote-host :initform nil :initarg :remote-host)
     (remote-port :initform nil :initarg :remote-port)))
  
  (defun make-udp-socket (host port local-port)
    (let ((sock (make-instance 'udp-socket :type :datagram :protocol :udp
			       :remote-host host :remote-port port)))
      (setf (sb-bsd-sockets:non-blocking-mode sock) nil)
      (sb-bsd-sockets:socket-bind sock 
				  (sb-bsd-sockets:make-inet-address "127.0.0.1")
				  local-port)
      sock))
  
  (Defun udp-socket-close (sock)
    (sb-bsd-sockets:socket-close sock))
  
  ;;no socket-shutdown in sbcl!!
  (defun udp-socket-shutdown (socket &optional how)
    socket how
    (values))
  
  (defun udp-socket-send (sock mess len)
    (let ((fd (sb-bsd-sockets::socket-file-descriptor sock)))
      (if (< (length mess) len)
	  (error "udp-socket-send: length ~s larger than message array" len))
      (if (= fd -1)
	  (error "udp-socket-send: ~s does not have a valid file descriptor" sock)
	(sb-bsd-sockets::with-sockaddr-for 
	 (sock sockaddr (list (sb-bsd-sockets:make-inet-address 
			       (slot-value sock 'remote-host))
			      (slot-value sock 'remote-port)))
	 (let ((bufptr (sb-alien:make-alien (array (sb-alien::unsigned 8) 1)
					    len)))
	   (unwind-protect 
	       (let ((addr-len (sb-bsd-sockets::size-of-sockaddr sock))
		     (slen nil))
		 (loop for i from 0 below len
		       do
		       (setf (sb-alien:deref (sb-alien:deref bufptr) i)
			     (aref mess i)))
		 (setf slen (sb-alien::sendto fd bufptr len 0 sockaddr addr-len))
		 (if (= -1 slen)
		     (sb-bsd-sockets::socket-error "sendto")
		   slen))
	     (sb-alien:free-alien bufptr)))))))
  
  
  

   (defun udp-socket-recv (sock bytes &optional flags)
     flags
     (sb-bsd-sockets:socket-receive sock nil bytes))
  

   (defun udp-socket-recv-no-block (sock bytes &optional flags)
     flags
     (sb-bsd-sockets::with-sockaddr-for (sock sockaddr)
       (let ((bufptr (sb-alien:make-alien (array (sb-alien::unsigned 8) 1) bytes)))
	 (unwind-protect
	      (sb-alien:with-alien 
		  ((sa-len sockint::socklen-t
                       (sb-bsd-sockets::size-of-sockaddr sock)))
		(let ((len (sockint::recvfrom 
			    (sb-bsd-sockets::socket-file-descriptor sock)
			    bufptr
			    bytes
			    0
                        sockaddr
                        (sb-alien:addr sa-len))))
		  (cond
		    ((and (= len -1) (= sockint::EAGAIN (sb-unix::get-errno))) nil)
		    ((= len -1) (sb-bsd-sockets::socket-error "recvfrom"))
		    (t (let ((buffer (make-array len :element-type
						 '(unsigned-byte 8))))
			 (loop for i from 0 below (if (> bytes len) len bytes)
			    do (setf (aref buffer i)
				     (sb-alien:deref (sb-alien:deref bufptr) i)))
			 buffer)))))
	   (sb-alien:free-alien bufptr))))))
   
  

#|
;;setup sockets
(defparameter *send-socket* (make-udp-socket "127.0.0.1" 9000 22011))
(defparameter *recv-socket* (make-udp-socket "127.0.0.1" 22011 9000))


;;evaluating this sends an array over socket
(let ((mess (make-array 64 :element-type '(unsigned-byte 8) :initial-element 2)))
  (udp-socket-send *send-socket* mess 64))

;;then---

;;;this receives on the socket. returns array or returns nil if nothing 
;;; is available to be received
(udp-socket-recv *recv-socket* 128)
(SB-BSD-SOCKETS::socket-receive *recv-socket* nil 128)

(udp-socket-close *send-socket*)
(udp-socket-close *recv-socket*)
|#












