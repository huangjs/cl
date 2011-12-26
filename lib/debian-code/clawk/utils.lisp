;;;; -*- Mode: Lisp -*-

(in-package #:clawk)

;; Code taken from Peter Norvig's _Paradigms of AI Programming_ book

(defun side-effect-free-p (exp)
  (or (constantp exp) (atom exp) (starts-with exp 'function)
      (and (starts-with exp 'the)
	   (side-effect-free-p (third exp)))))

(defmacro once-only (variables &rest body)
  (assert (every #'symbolp variables))
  (let ((temps (loop repeat (length variables) collect (gensym))))
    `(if (every #'side-effect-free-p (list . ,variables))
       (progn . ,body)
       (list 'let
	     ,`(list ,@(mapcar #'(lambda (tmp var)
				   `(list ',tmp ,var))
			       temps variables))
	     (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
			   variables temps)
	       . ,body)))))

