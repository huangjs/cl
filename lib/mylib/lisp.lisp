(in-package :cl-user)

(defpackage :hjs.meta.lisp
  (:use :cl)
  (:export #:self-evaluating-p
	   #:as-keyword
	   #:as-string
	   #:plist->alist
	   #:alist->plist
	   #:ensure-cons
	   #:ensure-list
	   #:ensure-function
	   #:concatenate-symbols
	   #:remove-keyword-args
	   #:parse-body
	   #:string-designator
	   #:save-lisp
	   ))

(in-package :hjs.meta.lisp)


(defun self-evaluating-p (form)
  (and (atom form) (if (symbolp form) (keywordp form) t)))

(defun as-keyword (string-designator)
  (etypecase string-designator
    (character (intern (make-string 1 :initial-element (char-upcase string-designator))
		       (find-package :keyword)))
    (string (intern (string-upcase string-designator)
		    (find-package :keyword)))
    (symbol (intern (symbol-name string-designator)
		    (find-package :keyword)))))

(defun as-string (foo)
  (typecase foo
    (string foo)
    (otherwise
     (format nil "~a" foo))))

(defun plist->alist (plist)
  (loop for list on plist by #'cddr
     for key = (first list)
     for val = (second list)
     collect (cons key val)))

(defun alist->plist (alist)
  (loop for pair in alist
     collect (car pair)
     collect (cdr pair)))

(defun concatenate-symbols (&rest symbols)
  (intern (format nil "~{~a~}" symbols)))

(defun remove-keyword-args (bag arg-list)
  (loop for (k v) on arg-list by #'cddr
     when (not (member k bag))
     collect k and collect v))


;;; type ensurance
(defun ensure-cons (cons)
  (if (consp cons)
      cons
      (cons cons nil)))

(defun ensure-list (something)
  (typecase something
    (list something)
    (otherwise (list something))))

(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))


(defun save-lisp (&rest args)
  (let ((pid (sb-posix:fork)))
    (if (zerop pid)
        (apply #'save-lisp-and-die "foo.core" args)
        (sb-posix:wait))))
