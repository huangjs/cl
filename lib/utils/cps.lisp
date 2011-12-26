#|
Copyright (C) 2006 Levente M�sz�ros melevy@freemail.hu


Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the author shall not be used in advertising or otherwise to promote the sale, use or other dealings in this Software without prior written authorization from the author. |#

;;; names that already present in common lisp preceded by @ sign for clarity

;(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
(declaim (optimize (debug 0) (safety 0) (speed 3) (space 0)))


(defpackage :hjs.util.cps
  (:use #:CL))

(in-package :hjs.util.cps)



;; top level eval
(defun @eval (k)
  (if (functionp k) (funcall k #'ret/cc) k))

;; top level eval restaring each time a continuation is returned by @eval
(defun @eval* (k)
  (do ((k k (@eval k)))
	  ((not (functionp k)) k)))

(defun cps-lambda (&rest forms)
  `(lambda (k)
	 (declare (type function k))
	 ,@forms))

(defun ret/cc (k) k)

(defmacro let/cc (variable &rest forms)
  `(call/cc ,(cps-lambda `(let ((,variable k)) ,@forms))))

(defconstant +special-forms+ '(progn if let let* setq block return-from declare tagbody go call/cc))

(defun @special-form-p (form)
  (declare (type list form))
  (member (the symbol (first form)) +special-forms+))

;; macros to generate CPS transformed code
(defun cps-transform-forms (forms)
  (loop for f in forms
	 collect (cps-transform-form f)))

(defun cps-transform-forms* (forms)
  (append '(list) (cps-transform-forms forms)))

(defun cps-transform-form (form)
  (setq form (macroexpand form))
  (if (or (atom form) (not (@special-form-p form)))
	  (cps-lambda `(funcall k ,form))
	  (ecase (first form)
		(progn (cps-lambda `(@progn k ,(cps-transform-forms* (cdr form)))))
		(if (cps-lambda `(@if k ,@(cps-transform-forms (cdr form)))))
		(let* #1=(cps-lambda
				  `(let (,@(loop for b in (second form)
							  collect (list (first b) nil)))
					 (@let* k (list ,@(loop for b in (second form)
										 collect `(lambda (v) (setq ,(first b) v))))
							(list ,@(loop for b in (second form)
									   collect (cps-transform-form (second b))))
							,(cps-transform-forms* (cddr form))))))
		(let	;; TODO: this is a temporary hack
			(format t "Warning: using let* instead of let: ~A" form) #1#)
		(setq (cps-lambda `(funcall ,(cps-transform-form (third form))
									(lambda (v)
									  (setq ,(second form) v)
									  (funcall k v)))))
		(block (cps-lambda `(let ((,(safe-symbol (second form)) k))
							  (@progn k ,(cps-transform-forms* (cddr form))))))
		(return-from (cps-lambda `(funcall ,(cps-transform-form (third form)) (safe-symbol (second form)))))
		(tagbody (let ((tagbody-forms (loop for f in (cdr form)
										 when (listp f)
										 collect (cps-transform-form f))))
				   (cps-lambda `(labels ((_tagbody_ (v) (funcall k v))
										 ,@(loop for f in (cdr form)
											  with i = 0
											  when (not (listp f))
											  collect (list (safe-symbol f) '(k)
															`(@progn k ,(append '(list) (nthcdr i tagbody-forms))))
											  when (listp f)
											  do (incf i)))
								  (@progn k ,(append '(list) tagbody-forms))))))
		(go (cps-lambda `(funcall #',(safe-symbol (second form)) #'_tagbody_)))
		(declare ;; TODO: use type declarations
		 (format t "Warning: ignoring declaration ~A" form)
		 (cps-lambda '(funcall k nil)))
		(call/cc (second form)))))

;; top level cps macro defines three functions: name @name and @name*
(defmacro @defun (name args &rest forms)
  (let ((f (cps-lambda `(@progn k ,(cps-transform-forms* forms)))))
	`(progn (defun ,name ,args ,@forms)
			(defun ,(intern (concatenate 'string "@" (symbol-name name))) ,args
			  (@eval ,f))
			(defun ,(intern (concatenate 'string "@" (symbol-name name) "*")) ,args
			  (@eval* ,f)))))

;;; special forms
(defun @progn (k forms)
  (declare (type function k) (type list forms))
  (cond ((cdr forms)
		 (funcall (the function (car forms))
				  (lambda (i)
					(declare (ignore i))
					(@progn k (cdr forms)))))
		(forms (funcall (the function (car forms)) k))
		(t (funcall k nil))))

(defun @if (k condition then &optional (else nil))
  (declare (type function k condition then))
  (funcall condition
		   (lambda (v)
			 (cond (v (funcall then k))
				   (else (funcall else k))
				   (t (funcall k nil))))))

(defun @let* (k variables values forms)
  (declare (type function k) (type list variables values forms))
  (if values
	  (funcall (the function (car values))
			   (lambda (v)
				 (funcall (the function (car variables)) v)
				 (@let* k (cdr variables) (cdr values) forms)))
	  (@progn k forms)))

(defun safe-symbol (symbol)
  (intern (concatenate 'string "@" (symbol-name symbol) "@")))

;; for non CPS function
(defun call/cc (k) (declare (ignore k)) nil) 
