;;; -*- Mode: Lisp ; Base: 10 ; Syntax: ANSI-Common-Lisp -*-
;;; csv: reading files in Comma-Separated Values format.
;;; $Id: csv.lisp,v 1.12 2005/01/26 02:37:40 fare Exp $

#| "
HOME PAGE:
	http://www.cliki.net/fare-csv

LICENSE:
	http://www.geocities.com/SoHo/Cafe/5947/bugroff.html

DEPENDENCIES:
	apt-get install cl-asdf cl-parse-number

USAGE:
	(load "csv")
	(read-csv-line)
	(read-csv-stream s)
	(read-csv-file "foo.csv")

EXAMPLE USE:
	...

BUGS:
	I implemented just enough of csv to be able to import something
	from a PC application that will remain unnamed.
	If you need more, you can cont(r)act me, and/or hack it yourself.

SEE ALSO:
	Here's what the Perl crowd think about what CSV is:
	http://www.perldoc.com/perl5.6.1/lib/Text/CSV.html

Share and enjoy!
" |#

; -----------------------------------------------------------------------------
;;; Packaging stuff

; If you have asdf and parse-number, load parse-number first.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;(pushnew :debug *features*)
  (unless (ignore-errors
	   (fboundp (intern "PARSE-NUMBER" (find-package :parse-number))))
    (pushnew :no-parse-number *features*)))

#-fare-csv
(eval-when (:compile-toplevel :load-toplevel :execute)
  #-no-parse-number (asdf:oos 'asdf:load-op :parse-number)
  (defpackage :fare-csv
    (:use #-no-parse-number :parse-number :common-lisp)
    (:export #:read-csv-line #:read-csv-stream))
  (pushnew :fare-csv *features*))

(in-package :fare-csv)

; -----------------------------------------------------------------------------
;;; Optimization
(declaim (optimize (speed 3) (safety 1) (debug 3)))

; -----------------------------------------------------------------------------
;;; Thin compatibility layer
;;; FIXME: do we need to eval-when something?
#-no-parse-number
(defun parse-number (s)
  (let* ((*read-eval* nil)
	 (n (read-from-string s)))
    (if (numberp n) n)))

; -----------------------------------------------------------------------------
;;; Choice of special characters
(defparameter *csv-separator* #\,
  "Separator between CSV fields")
(defparameter *csv-quote* #\"
  "delimiter of string data; pascal-like quoted as double itself in a string.")

; -----------------------------------------------------------------------------
;;; The parser

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name))
	  (defun ,name ,arglist ,@body)))

(defsubst char-space-p (c)
  "Is character C some kind of white space?
BUG: this only handles a tiny subset of character sets,
even if restricted to ASCII. However, it's rather portable."
  (declare (type character c))
  (member c '(#\Space #\Tab)))
(defsubst char-digit-or-dot-p (c)
  "Is character C a digit or a dot?"
  (declare (type character c))
  (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)))
(defsubst char-normal-p (c)
  (declare (type character c))
  (not (member c (list* *csv-separator* *csv-quote*
			'(#\Space #\Return #\Linefeed)))))
(defsubst ns-accept-p (x s)
  (let ((c (peek-char nil s nil nil)))
    ; #+DEBUG (format t "~%Current char: ~a" c)
    (typecase x
      (character (eql x c))
      ((or function symbol) (funcall x c))
      (integer (eql x (char-code c))))))
(defsubst ns-accept (x s)
  (and (ns-accept-p x s)
       (read-char s)))
(defsubst ns-accept-eof (s)
  (not (peek-char nil s nil nil)))
(defsubst ns-accept-eol (s)
  (let ((x (ns-accept #\Return s)))
    (or (ns-accept #\Linefeed s) x)))
(defsubst ns-accept-space (s)
  (ns-accept #'char-space-p s))
(defsubst ns-accept-normal (s)
  (ns-accept #'char-normal-p s))
(defsubst ns-accept-spaces (s)
  (loop while (ns-accept-space s)))
(defsubst ns-accept-quote (s)
  (ns-accept *csv-quote* s))
(defsubst ns-accept-separator (s)
  (ns-accept *csv-separator* s))

(defun read-csv-line (s &aux (ss (make-string-output-stream)) (l '()))
  (labels
      ((maybe-field ()
          ;#+DEBUG (format t "~%Maybe field, l=~a" l)
	  (ns-accept-spaces s)
	  (if (or (ns-accept-eol s) (ns-accept-eof s))
	      (done)
	    (do-field)))
       (do-field ()
          ;#+DEBUG (format t "~%do field")
	  (cond
	    ((ns-accept-quote s)
	     (string-field))
	    ((ns-accept-separator s)
	     (progn (add nil) (maybe-field)))
	    ((ns-accept-p #'char-digit-or-dot-p s)
	     (numeric-field))
	    (t (symbol-field))))
       (string-field ()
          ;#+DEBUG (format t "~%string field")
           (cond
	    ((ns-accept *csv-quote* s)
	     (if (ns-accept *csv-quote* s)
		 (string-field-char *csv-quote*)
	       (progn (add (current-string))
		      (end-of-field))))
	    ((ns-accept-eof s)
	     (error "unexpected end of stream"))
	    (t
	     (string-field-char (read-char s)))))
       (string-field-char (c)
	     (add-char c)
	     (string-field))
       (end-of-field ()
          ;#+DEBUG (format t "~%end of field")
	  (ns-accept-spaces s)
	  (cond
	   ((or (ns-accept-eol s) (ns-accept-eof s))
	    (done))
	   ((ns-accept-separator s)
	    (maybe-field))
	   (t (error "end of field expected"))))
       (eat-field ()
          ;#+DEBUG (format t "~%eat field")
          (loop for c = (ns-accept-normal s) while c
	    do (add-char c) finally (return (current-string))))
       (numeric-field ()
          ;#+DEBUG (format t "~%numeric field")
          (add (parse-number (eat-field)))
          ;#+DEBUG (format t "~%added number: ~a" (car l))
	  (end-of-field))
       (symbol-field ()
          ;#+DEBUG (format t "~%symbol field")
          (add (intern (eat-field) :keyword))
          ;#+DEBUG (format t "~%added symbol: ~a" (car l))
	  (end-of-field))
       (add (x)
	  (push x l))
       (add-char (c)
	  (write-char c ss))
       (current-string ()
	  (get-output-stream-string ss))
       (done () (nreverse l)))
    (maybe-field)))

(defun read-csv-stream (s)
  (loop until (ns-accept-eof s)
    collect (read-csv-line s)))

(defun read-csv-file (pathname)
  (with-open-file (s pathname :direction :input :if-does-not-exist :error)
    (read-csv-stream s)))

;(trace read-csv-line read-csv-stream)

#+DEBUG (write (read-csv-file "test.csv"))
#+DEBUG (progn
	  (setq *csv-separator* #\;)
	  (write (read-csv-file "/samba/ciev.csv")))
