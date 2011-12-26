(in-package :cl-user)

(defpackage :hjs.util.string
  (:use :cl :hjs.meta.macro :iterate)
  (:export #:parse-number
	   #:s+
	   #:string-starts-with
	   #:string-ends-with
	   #:string-trim-whitespace
	   #:string-right-trim-whitespace
	   #:string-left-trim-whitespace
	   #:whitespace-p
	   #:string-symmetric-p
	   #:safe-string
	   ))

(in-package :hjs.util.string)

(defnewconstant +whitespace+ '(#\return #\newline #\tab #\space #\page #\　))

(defun parse-number (string &key (start 0) end (radix 10) signal-error default-value)
  (handler-bind ((error
		  #'(lambda (c)
		      (declare (ignorable c))
		      (if (null signal-error)
			  (return-from parse-number default-value)))))
    (org.mapcar.parse-number:parse-number string
					  :start start
					  :end end
					  :radix radix)))

(defun s+ (&rest strings)
  "Shorthand for concatenating string."
  (apply #'concatenate 'string strings))

(defun string-starts-with (prefix string &optional (compare #'string=))
  (let ((prefix-length (length prefix)))
    (and (>= (length string) prefix-length)
	 (funcall compare prefix string :end2 prefix-length))))

(defun string-ends-with (postfix string &optional (compare #'string=))
  (let ((postfix-length (length postfix))
	(string-length (length string)))
    (and (>= string-length postfix-length)
	 (funcall compare postfix string :start2 (- string-length postfix-length)))))


(defun string-trim-whitespace (string)
  (string-trim +whitespace+ string))

(defun string-right-trim-whitespace (string)
  (string-right-trim +whitespace+ string))

(defun string-left-trim-whitespace (string)
  (string-left-trim +whitespace+ string))

(defun whitespace-p (char)
  (member char +whitespace+))

(defun string-symmetric-p (str)
  (iter (with len = (length str))
	(for i from 0 below (floor len 2))
	(for j downfrom (1- len))
	(always (char= (char str i) (char str j)))))

(defun safe-string (x)
  "Definitely return a string. if x is nil, return an empty string; otherwise, print x as a string and return."
  (cond ((stringp x)
	 x)
	((null x)
	 "")
	(t
	 (format nil "~a" x))))
