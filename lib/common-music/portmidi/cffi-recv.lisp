;;; **********************************************************************
;;; Copyright (C) 2005 Todd Ingals, <testcase (at) asu (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2006/04/30 15:00:28 $

(in-package :portmidi)

(let* ((ext #+win32 "dll"
	    #+(or darwin macos macosx) "dylib"
	    #+(or linux (and clisp unix (not macos))) "so")
       (lib (make-pathname :name "libpmrecv" :type ext
			   :defaults (truename *load-pathname*))))
  (cffi:load-foreign-library lib)
  )

;;(cffi:load-foreign-library "/usr/local/lisp/portmidi/libpmrecv.dylib")

(cffi:defcfun ("pm_set_input" pm-set-input)
    :int
  (pmstream :pointer)
  (id :int)
  (bufsize :int))

(cffi:defcfun ("pm_recv_start" pm-recv-start)
    :int
  (priority :int)
  (resolution :int)
  (recv-callback :pointer))

(cffi:defcfun ("pm_recv_stop" pm-recv-stop) :int)
(cffi:defcfun ("pm_recv_state" pm-recv-state) :int)
(cffi:defcfun ("pm_priority_min" pm-priority-min) :int)
(cffi:defcfun ("pm_priority_max" pm-priority-max) :int)

;;; lisp api

(defparameter *pm-recv-callback*  #'(lambda (x) x (values)))

(cffi:defcallback pm_receive_callback :int ((p :pointer))
  (funcall *pm-recv-callback* (Event.message p))
  0)

(defun recv-start (pr res)
  (pm-recv-start pr res (cffi:callback pm_receive_callback)))
(defun recv-stop () (pm-recv-stop))
(defun recv-state () (pm-recv-state))
(defun get-priority-range ()
  (list (pm-priority-min) (pm-priority-max)))
(defun set-input (a b c) (pm-set-input a b c))

(eval-when (:load-toplevel :execute)
  (export '(recv-start recv-stop recv-state get-priority-range
	    set-input *pm-recv-callback*) 
	  :portmidi)
  (pushnew ':portmidi-recv *features*)
  )

;;; EOF

