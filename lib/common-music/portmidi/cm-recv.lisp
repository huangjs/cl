;;; **********************************************************************
;;; Copyright (C) 2005 Todd Ingals, <testcase (at) asu (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.2 $
;;; $Date: 2006/10/01 15:47:41 $

(in-package :cm)

(defmethod recv ((io portmidi-stream) &key (resolution 5) (priority 20))
  (if (io-open io)
      (if (= (pm:recv-state) 0)
	  (progn
	    (let ((data (slot-value io 'receive-data)))
	      (if (= (pm:set-input (first data)
				   (second data) 
				   (third data))
		     0)
		  (if (= (pm:recv-start priority resolution ) 0)
		      (format t "; recv running on ~s~%" io)))))
	  (warn "recv is already running on ~s" io))
      (warn "stream ~s is not open" io))
  (values))
	     
(defmethod recv-stop ((io portmidi-stream))
  (if (io-open io)
      (if (= (pm:recv-state) 1)
	  (progn
	    (pm:recv-stop)
	    (format t "; recv stopped on stream ~s" io))
	  (warn "recv is not running on ~s" io)))
  (values))
	     
(defmethod recv-set! ((io portmidi-stream) hook &key (recv-mode))
  recv-mode				; make this do something
  (if (io-open io)
      (if (functionp hook)
	  (progn
	    (setf pm:*pm-recv-callback* 
		  (lambda (m) (funcall hook (pm-message->midi-message m)))))
	  (progn
	    (setf pm:*pm-recv-callback* #'(lambda (x) x (values)))
	    nil))
      (warn "stream ~s is not open" io))
  (values))
	     
(defmethod recv? ((io portmidi-stream) &optional state)
  state
  (if (io-open io)
      (if (= (pm:recv-state) 0)
	  ':stopped
	  ':running)
      (warn "stream ~s is not open" io)))

;;; eof

