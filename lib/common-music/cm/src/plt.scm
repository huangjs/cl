;;; **********************************************************************
;;; Copyright (C) 2007 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name:  $
;;; $Revision: 1.1 $
;;; $Date: 2007/01/25 12:18:54 $

(define-class* <gnuplot-file> (<event-file>)
  ((objects :init-value '() :accessor gnuplot-file-objects))
  :name 'gnuplot-file
  :metaclass <io-class>
  :file-types '("*.plt"))

;;;
;;; override main methods since we dont write file until end.
;;; 

(define-method* (open-io (io <gnuplot-file>) dir . args)
  dir args
  (set! (gnuplot-file-objects io) (list))
  io)

(define-method* (close-io (io <gnuplot-file>) . mode)
  (if (not (eq? mode ':force)) ; no errors
      (let ((data (gnuplot-file-objects io))
	    (path #f)
	    (comm #f))
	(if (not (null? data))
	    (begin
	      (bump-version io) ; still do versioning...
	      (set! path (file-output-filename io))
	      (set! comm (format #f "# ~a output on ~a~%"
				 (cm-version)
				 (date-and-time)))
	      (apply (function gnuplot) 
		     path
		     :comment comm
		     (append (event-stream-args io)
			     (list (reverse! data))))
	      io)))))

(define-method* (write-event (obj <event>) (io <gnuplot-file>) time)
  (set! (object-time obj) time)
  (set! (gnuplot-file-objects io) 
	(cons obj (gnuplot-file-objects io) )))