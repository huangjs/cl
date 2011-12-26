;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.2 $
;;; $Date: 2006/06/02 07:02:18 $


(in-package :cm)

(defparameter *liboscrecv-exists* nil)

(let* ((ext #+win32 "dll"
	    #+(or darwin macos macosx) "dylib"
	    #+(or linux (and clisp unix (not macos))) "so")
       (lib (make-pathname :name "liboscrecv" :type ext
			   :defaults (truename *load-pathname*))))
  (if (probe-file lib)
      (progn
	(cffi:load-foreign-library lib)
	(setf *liboscrecv-exists* t))))

;;(cffi:load-foreign-library "/usr/local/lisp/portmidi/liboscrecv.dylib")


(if *liboscrecv-exists* 
    (progn
      (cffi::defcfun ("osc_recv_start" osc-recv-start)
	  :int
	(priority :int)
	(resolution :int)
	(recv-callback :pointer))
      
      (cffi::defcfun ("osc_recv_stop" osc-recv-stop)
	  :int)
      
      (cffi::defcfun ("osc_recv_state" osc-recv-state)
	  :int)
      
      (cffi::defcfun ("osc_priority_min" osc-priority-min)
	  :int)
      
      (cffi::defcfun ("osc_priority_max" osc-priority-max)
	  :int)
      
      (defun get-priority-range ()
	(list (priority-min) (priority-max)))
      
      (defparameter *osc-recv-callback*  #'(lambda ()  (values)))
      
      
      (cffi::defcallback osc_receive_callback :int ()
	(funcall *osc-recv-callback*)
	0)

      #+cm (progn
	     (defmethod cm::recv ((io cm::osc-stream) &key (resolution 5) (priority 20))
	       (if (cm::io-open io)
		   (if (= (osc-recv-state) 0)
		       (progn
			 (let ((data (slot-value io 'cm::receive-data)))
			   (if (= (osc-recv-start priority resolution (cffi:callback osc_receive_callback)) 0)
			       (format t "recv running on stream ~s~%" io))))
		       (warn "recv is already running on ~s" io))
		   (warn "stream ~s is not open" io))
	       (values))
	     
	     (defmethod cm::recv-stop ((io cm::osc-stream))
	       (if (cm::io-open io)
		   (if (= (osc-recv-state) 1)
		       (progn
			 (osc-recv-stop)
			 (format t "recv stopped on stream ~s" io))
		       (warn "recv is not running on ~s" io)))
	       (values))
	     
	     (defmethod cm::recv-set! ((io cm::osc-stream) hook &key (recv-mode :message))
	       (if (not (member recv-mode '(:message :raw :object)))
		   (error "receive: ~s is not a osc receive mode." recv-mode)
		   (if (io-open io)
		       (if (functionp hook)
			   (progn
			     (cond ((eq recv-mode ':raw)
				    (setf *osc-recv-callback* #'(lambda () 
								  (funcall hook (udp-socket-recv (slot-value io 'socket) (slot-value io 'buffer-size))))))
				   ((eq recv-mode ':message)
				    (setf *osc-recv-callback* #'(lambda () 
								  (funcall hook (osc-vector->osc-message 
										 (udp-socket-recv (slot-value io 'socket) (slot-value io 'buffer-size)))))))
				   ((eq recv-mode ':object)
				    (setf *osc-recv-callback* #'(lambda () (funcall hook (osc-message->sc-object 
											  (osc-vector->osc-message 
											   (udp-socket-recv (slot-value io 'socket) (slot-value io 'buffer-size))))))))))
			   (progn
			     (setf *osc-recv-callback* #'(lambda ()  (values)))
			     nil))
		       (warn "stream ~s is not open" io)))
		   (values))
	     
	     (defmethod cm::recv? ((io cm::osc-stream) &optional state)
	       state
	       (if (cm::io-open io)
		   (if (= (pm-recv-state) 0)
		       ':stopped
		       ':running)
		   (warn "stream ~s is not open" io))))))

    
