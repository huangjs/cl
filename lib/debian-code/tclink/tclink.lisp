;;; -*- Mode: Lisp -*-
;;; TCLink foreign interface using UFFI
;;; Copyright 2002 Matthew Danish <mrd@debian.org>
;;; Distributed under the terms of the LLGPL.
;;; See LICENSE file for more details.

(in-package #:CL-TCLINK)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *library-locations-list*
    (let ((lib-list
	   (list "/usr/lib/cl-tclink/"
                 (translate-logical-pathname
		  (logical-pathname "cl-library:cl-tclink;"))
                 "/lib/" "/usr/lib/" "/usr/local/lib/")))
      (when *load-truename*
	(push (directory-namestring *load-truename*)
	      lib-list))
      (when *compile-file-truename*
	(push (directory-namestring  *compile-file-truename*)
	      lib-list))
      lib-list)
    "List of possible locations for libtclink.so")
  
  (let ((libtclink.so
	 (find-foreign-library
	  '("libtclink")
	  (mapcar (lambda (x) (typecase x
				(null "")
				(logical-pathname
				 (translate-logical-pathname x))
				(t x)))
		  *library-locations-list*)
	  :types '("so" "so.0" "so.1" "dll")
	  :drive-letters '("C" "D" "E" "F" "G"))))
    (if libtclink.so
	(load-foreign-library
	 libtclink.so
	 :module "tclink"
	 :supporting-libraries '("c" "ssl"))
	(error "Could not find libtclink.so"))))

(defmacro with-sized-foreign-string ((name size) &body body)
  `(let ((,name (allocate-foreign-string ,size)))
    (unwind-protect (progn ,@body)
      (free-foreign-object ,name))))

(defconstant +parameter-max-length+ 256
  "The maximum length for names and values sent")

(define-condition parameter-too-long ()
  ((which :initarg :which
	  :reader which-parameter)
   (value :initarg :value
	  :reader parameter-value))
  (:report
   (lambda (c s)
     (format s "~&Parameter \"~A\" with value \"~A\" exceeded the maximum parameter length ~A.~%"
	     (which-parameter c)
	     (parameter-value c)
	     +parameter-max-length+))))

(def-foreign-type handle :pointer-void)

(def-function ("TCLinkCreate" create)
    ()
  :returning handle
  :module "tclink")

(def-function ("TCLinkDestroy" destroy)
    ((h handle))
  :returning :void
  :module "tclink")

(def-function ("TCLinkPushParam" c-push-param)
    ((h handle)
     (name (* :unsigned-char))
     (value (* :unsigned-char)))
  :returning :void
  :module "tclink")

(defun push-parameter (h name value)
  (cond ((>= (length name) +parameter-max-length+)
	 (error 'parameter-too-long
		:which "name"
		:value name))
	((>= (length value) +parameter-max-length+)
	 (error 'parameter-too-long
		:which "value"
		:value name))
	(t (with-foreign-string (c-name name)
	     (with-foreign-string (c-value value)
	       (c-push-param h c-name c-value)
	       T)))))

(def-function ("TCLinkSend" send)
    ((h handle))
  :returning :void
  :module "tclink")

(def-function ("TCLinkGetResponse" c-get-response)
    ((h handle)
     (name (* :unsigned-char))
     (value (* :unsigned-char)))
  :returning (* :unsigned-char)
  :module "tclink")

(defun get-response (h name)
  (with-foreign-string (c-name name)
    (with-sized-foreign-string (c-value-buffer +parameter-max-length+)
      (let ((c-value (c-get-response h name c-value-buffer)))
	(unless (null-pointer-p c-value)
	  (convert-from-foreign-string c-value))))))


(def-function ("TCLinkGetEntireResponse" c-get-entire-response)
    ((h handle)
     (buf (* :unsigned-char))
     (size :int))
  :returning (* :unsigned-char)
  :module "tclink")

(defun get-entire-response (h &key (size 1024))
  "Returns an association-list of (NAME . VALUE) entries where KEY is a
symbol in the KEYWORD package with its name equal to the original upcased."
  (with-sized-foreign-string (buffer size)
    (let ((returned-buffer (c-get-entire-response h buffer size)))
      (unless (null-pointer-p returned-buffer)
	(mapcar (lambda (x) (let ((line (split-sequence #\= x)))
			      (cons (intern (string-upcase (first line))
					    "KEYWORD")
				    (second line))))
		(split-sequence #\Newline
				(convert-from-foreign-string
				 returned-buffer)
				:remove-empty-subseqs t))))))

(def-function ("TCLinkGetVersion" c-get-version)
    ((buf (* :unsigned-char)))
  :returning (* :unsigned-char)
  :module "tclink")

(defun get-version (&key (size 256))
  (with-sized-foreign-string (buffer size)
    (let ((returned-buffer (c-get-version buffer)))
      (unless (null-pointer-p returned-buffer)
	(convert-from-foreign-string returned-buffer)))))

(defmacro with-handle ((h) &body body)
  `(let ((,h (create)))
    (unwind-protect (progn ,@body)
      (destroy ,h))))

(defun send-with-parameters (params &optional (h nil))
  "Given a handle h, and an association-list (NAME . VALUE) of parameters
param, send to an available TCLink server.  If h is NIL, a new handle is
created, used, and destroyed.  The entire response is returned, as if
from GET-ENTIRE-RESPONSE."
  (let ((handle (or h (create))))
    (unwind-protect
	 (loop for ((name . value)) on params
	       do (push-parameter handle name value)
	       finally (progn (send handle)
			      (return (get-entire-response handle))))
      (unless h (destroy handle)))))
