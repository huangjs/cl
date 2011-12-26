;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           edge-table-storage.lisp
;;;;  Purpose:        Store precompiled edge table for reversi
;;;;  Programer:      Kevin Rosenberg
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: edge-table-storage.lisp 7061 2003-09-07 06:34:45Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(in-package #:reversi)

(defparameter *et-path* nil)

(eval-when (:load-toplevel :execute)
  (let ((precompiled-path (make-pathname
			   :directory '(:absolute "usr" "share" "common-lisp"
						  "source" "reversi")
			   :name "edge-table"
			   :type "dat")))
    (if (probe-file precompiled-path)
	(setq *et-path* precompiled-path)
      (setq *et-path* (make-pathname
		       :directory (pathname-directory *load-truename*)
		       :host (pathname-host *load-truename*)
		       :device (pathname-device *load-truename*)
		       :name "edge-table"
		       :type "dat"))))

  (defun store-edge-table (et &optional (path *et-path*))
    (declare (type edge-table et))
    (with-open-file (stream path :direction :output
			    :if-exists :supersede)
      (print (length et) stream)
      (dotimes (i (length et))
	(declare (fixnum i))
	(print (aref et i) stream))))
  
  (defun load-edge-table (&optional (path *et-path*))
    (when (probe-file path)
      (with-open-file (stream path :direction :input)
	(let* ((length (read stream))
	       (et (make-array length :element-type 'fixnum)))
	  (declare (type (simple-array fixnum (*)) et))
	  (dotimes (i length)
	    (declare (fixnum i))
	    (setf (aref et i) (read stream)))
	  et))))
  
  (unless (probe-file *et-path*)
    (format *trace-output* ";; Recompiling edge-table, this make take several minutes")
    (store-edge-table (make-edge-table)))
  
  (unless *edge-table*
    (setq *edge-table* (load-edge-table))))





