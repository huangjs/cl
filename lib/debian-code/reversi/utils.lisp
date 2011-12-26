;;;;***************************************************************************
;;;;
;;;; FILE IDENTIFICATION
;;;; 
;;;;  Name:           reversi-base.lisp
;;;;  Purpose:        Basic functions for reversi
;;;;  Programer:      Kevin M. Rosenberg
;;;;  Date Started:   1 Nov 2001
;;;;
;;;; $Id: utils.lisp 10866 2006-01-15 18:32:28Z kevin $
;;;;
;;;; This file is Copyright (c) 2001-2003 by Kevin M. Rosenberg 
;;;;
;;;; Reversi users are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser GNU Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;;***************************************************************************

(in-package #:reversi)


(defmacro missing-argument ()
  `(error "Missing an argument to a constructor"))

;; Anaphoric macros

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun random-nth (list)
  (declare (list list))
  "Pick a random element out of a list."
  (nth (random (length list)) list))

(defun concat-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun cross-product (fn xlist ylist)
  "Return a list of all (fn x y) values."
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (funcall fn x y))
                       xlist))
           ylist))


(defmacro until (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

#+excl
(defun list-to-delimited-string (list &optional (separator #\space))
  (excl:list-to-delimited-string list separator))

#-excl
(defun list-to-delimited-string (list &optional (separator #\space))
  (let ((output (when list (format nil "~A" (car list)))))
    (dolist (obj (rest list))
      (setq output (concatenate 'string output
				(format nil "~A" separator)
				(format nil "~A" obj))))
    output))


						
