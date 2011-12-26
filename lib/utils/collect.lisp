;;;; Copyright Sven-Olof Nystrom 2003-2004 svenolof@csd.uu.se
;;; Macro collect & translation

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 2.1 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


(defpackage :hjs.util.collect
  (:use #:CL)
  (:export #:collect
	   #:iterator
	   #:iter
	   #:with-collection))

(in-package :hjs.util.collect)


(defmacro collect (kind exprs &body clauses)    
  (translate-collect kind exprs clauses))

(defun translate-collect (kind exprs clauses)
  (let* ((collection (gensym))
	 (init (gensym))
	 (add (gensym))
	 (finish (gensym))
	 (block (gensym))
	 (body 
	  `((setq ,collection (funcall ,add ,collection ,@exprs))))
	 (iteration (translate-collect-clauses clauses body block)))
    (multiple-value-bind
	  (p i a f) (make-collector-0 kind)
      (declare (ignore p))
      `(let* ((,init ,i)
	      (,add ,a)
	      ,@(if f `((,finish ,f)) '())
	      (,collection (if ,init (funcall ,init) nil)))
	 (block ,block ,@iteration)
	 ,(if f `(funcall ,finish ,collection)
	      collection)))))

(defun translate-collect-clauses (clauses body block)
  (cond 
    ((null clauses)
     body)
    (t 
     (let ((body (translate-collect-clauses (cdr clauses) body block))
	   (clause (first clauses)))
       (ecase (first clause) 
	 (for (translate-for (cdr clause) body))
	 (in (translate-for (list clause) body))
	 (step (translate-for (list clause) body))
	 (do (append (cdr clause) body))
	 (let `((let ((,(cadr clause) ,(caddr clause)))
		  ,@body)))
	 (when `((when ,(cadr clause) ,@body)))
	 (while `((unless ,(cadr clause) (return-from ,block))
		  ,@body)))))))




(defun translate-for (clauses body)
  (let ((block (gensym)))
    (multiple-value-bind (loop-body let-nest)  
	(build-iterator* block clauses body)
      `((block ,block ,@(funcall let-nest `((loop ,@loop-body))))))))
	
(defun build-iterator* (block ve-list body)
  (if (null ve-list) 
      (values body #'(lambda (body) body))
      (multiple-value-bind (loop-body let-nest) 
	  (build-iterator* block (cdr ve-list) body)
	(let ((clause (car ve-list)))
	  (ecase (car clause)
	    (in
	     (let*    
		 ((vars (if (atom (cadr clause))
			    (list (cadr clause))
			    (cadr clause)))
						
		  (expr (caddr clause))
		  (fun-name (gensym "fun"))
		  (next-name (gensym "next")) 
		  (more-name (gensym "more")) 
		
		  (loop-body
		     `((multiple-value-bind (,more-name ,@vars)
			   (funcall ,next-name)
			 (unless ,more-name (return nil))
			 ,@loop-body)))
		  (let-nest 
		   #'(lambda (body)
		       `((let ((,fun-name 
				#'(lambda (,next-name)
				    ,@(funcall let-nest body))))
			   (iterator ,(length vars)
				     ,expr
				     ,fun-name))))))
	       (values loop-body let-nest)))
	    (step 
	     (let ((var (cadr clause))
		   (init (caddr clause))
		   (test (cadddr clause))
		   (step (cadr (cdddr clause))))
	       (let* ((step-name (gensym "step"))
		      
		      (loop-body 
			 `((unless ,test (return-from ,block))
			   ,@loop-body
			   (funcall ,step-name)))
		    
		      (let-nest 
		       #'(lambda (body)
			   `((let* ((,var ,init)
				    (,step-name #'(lambda () (setq ,var ,step))))
			       ,@(funcall let-nest body))))))

		 (values loop-body let-nest)))))))))

		
;;; Make-collector

(defun make-collector-0 (kind)
  (if (atom kind) 
      (make-collector kind nil)
      (make-collector (car kind) (cdr kind))))

;;nil


(defmethod make-collector  ((kind (eql 'nil)) args)
  (assert (null args) (kind args) 
	  "Collector nil expects zero arguments")
  (values '()
	  nil
	  `#'(lambda (s)(declare (ignore s)))
	  nil))

					; t

(defmethod make-collector  ((kind (eql 't)) args)
  (assert (null args) (kind args) 
	  "Collector t expects zero arguments")
  (values (list (gensym))
	  nil
	  `#'(lambda (s x)(declare (ignore s))
		     x)
	  nil))


					; sum

(defmethod make-collector ((kind (eql 'sum)) args)
  (assert (null args) (kind args) 
	  "Collector sum expects zero arguments")
  (values (list (gensym))
	  `#'(lambda () 0)
	  `#'(lambda (s x)
	       (+ s x))
	  nil))

					; max

(defmethod make-collector ((kind (eql 'max)) args)
  (assert (null args) (kind args) 
	  "Collector max expects zero arguments")
  (values (list (gensym))
	  nil
	  `#'(lambda (s x)
	       (if s (max s x)
		   x))
	  nil))

					; reduce

(defmethod make-collector ((kind (eql 'reduce)) args)
  (assert (eql (length  args) 2) (kind args) 
	  "Collector max expects two arguments")
  (let ((fun (car args))
	(fname (gensym))
	(iv (cadr args)))
    (values (list (gensym))
	    `#'(lambda () ,iv)
	    `(let ((,fname ,fun))
	       #'(lambda (s x)
		   (if s (funcall ,fname s x)
		       x)))
	    nil)))


					; List

(defmethod make-collector ((kind (eql 'list)) args)
  (assert (null args) (kind args) 
	  "Collector list expects zero arguments")
  (values (list (gensym))
	  `#'(lambda () nil)
	  `#'(lambda (s x)(cons x s))
	  `#'(lambda (s)(reverse s))))

					; Vector

(defvar *initial-vector-collection-size* 10)


(defmethod make-collector ((kind (eql 'vector)) args)
  (values (list (gensym))
	  `#'(lambda () 
	       (make-array *initial-vector-collection-size* :fill-pointer 0 ,@args))
	  `#'(lambda (s x)
	       (vector-push-extend x s)
	       s)
	  nil))




					; Hash set

(defmethod make-collector ((kind (eql 'hash-set)) args)
  (values (list (gensym))
	  `#'(lambda ()(make-hash-table ,@args))
	  `#'(lambda (s x)(setf (gethash x s) t)
		     s)
	  `#'(lambda (s)
	       s)))

					; Hash table

(defmethod make-collector ((kind (eql 'hash-table)) args)
  (multiple-value-bind
	(param init add finish)(make-collector-0 (car args))
    (values
     (cons (gensym) param)
     `#'(lambda ()(make-hash-table ,@(cdr args)))
     `#'(lambda (s k ,@param)
	  (multiple-value-bind (val flag)(gethash k s)
	    (when (and ,init (not flag))
	      (setq val (funcall ,init)))
	    (setf (gethash k s) (funcall ,add val ,@param))
	    s))
     (if finish 
	 `#'(lambda (s)
	      (with-hash-table-iterator (next-entry s)
		(loop 
		   (multiple-value-bind (more key value) (next-entry)
		     (unless more (return nil))
		     (setf (gethash key s) (funcall ,finish value)))))
	      s)
	 nil))))

;;; Array: (array <type> (d1 ... dn) &rest)

(defmethod make-collector ((kind (eql 'array)) args)
  (let ((inner-type (car args))
	(dimensions (cadr args))
	(options (cddr args)))
    (multiple-value-bind
	  (param init add finish)
	(make-collector-0 inner-type)
      (let ((this-param (map 'list #'(lambda (_)(declare (ignore _))
					     (gensym))
			     dimensions)))
	(values 
	 (cons (gensym) (append this-param param))
	 `#'(lambda ()
	      (init-array (make-array (list ,@dimensions) ,@options)
			  ,init))
	 `#'(lambda (s ,@this-param ,@param)
	      (let ((value (aref s ,@this-param)))
		(setf (aref s ,@this-param) (funcall ,add value ,@param))
		s))
	 (if finish 
	     `#'(lambda (s) 
		  (finish-array s ,finish))
	     nil))))))

(defun init-array(s f)
  (when f 
    (let ((d (array-total-size s)))
      (dotimes (i d)
	(setf (row-major-aref s i) (funcall f)))))
  s)

(defun finish-array(s f)
  (let ((d (array-total-size s)))
    (dotimes (i d)
      (setf (row-major-aref s i) (funcall f (row-major-aref s i))))
    s))





;;; Iteration

(defmethod iterator ((arity (eql 1)) (l list) f)
  (funcall f
	   #'(lambda ()
	       (let ((m l))
		 (setq l (cdr l))
		 (values (consp m) (car m))))))


(defmethod iterator ((arity (eql 1)) (a sequence) f)
  (let ((i 0))
    (funcall f
	     #'(lambda ()
		 (if (< i (length a))
		     (let ((i0 i))
		       (incf i)
		       (values t (elt a i0)))
		     (values nil nil))))))

(defmethod iterator ((arity (eql 2)) (a sequence) f)
  (let ((i 0))
    (funcall f
	     #'(lambda ()
		 (if (< i (length a))
		     (let ((i0 i))
		       (incf i)
		       (values t i0 (elt a i0)))
		     (values nil nil nil))))))

(defmethod iterator ((arity (eql 2)) (table hash-table) f)
  (with-hash-table-iterator (next table)
    (funcall f #'(lambda ()(next)))))

(defmethod iterator ((arity (eql 1)) (table hash-table) f)
  (with-hash-table-iterator (next table)
    (funcall f #'(lambda ()(next)))))




;;; Intervals

(defclass interval ()
  ((from :initarg :from :accessor interval-from)
   (length :initarg :length :accessor interval-length)
   (step :initarg :step :accessor interval-step)))

(defmethod interval-elt ((i interval) n)
  (+ (interval-from i) (* n (interval-step i))))

(defun exor (a b)
  (or (and a (not b))
      (and (not a) b)))

(defun interval (&key from to down-to by) 
  (assert (exor to down-to) (to down-to) 
          "Exactly one of keyword arguments to and down-to must be given")
  (unless by (setq by (if to 1 -1)))
  (assert (or (and to (< 0 by)) (and down-to (< by 0)))
          (by)
          "Potentially infinite iteration!
           Use positive step with to and negative with down-to")
  (let*
      ((goal (if to to down-to))
       (length (1+ (floor (/ (- goal from) by)))))
    (make-instance 'interval :from from :length length :step by)))



(defmethod iterator ((arity (eql 1)) (a interval) f)
  (let ((i 0))
    (funcall f
	     #'(lambda ()
		 (if (< i (interval-length a))
		     (let ((i0 i))
		       (incf i)
		       (values t (interval-elt a i0)))
		     (values nil nil))))))

(defmethod iterator ((arity (eql 2)) (a interval) f)
  (let ((i 0))
    (funcall f
	     #'(lambda ()
		 (if (< i (interval-length a))
		     (let ((i0 i))
		       (incf i)
		       (values t i0 (interval-elt a i0)))
		     (values nil nil nil))))))


;;;; Define a separate macro for parallel iteration

(defmacro iter (clauses &body body)
  `(progn ,@(translate-for clauses body)))


;;;; Define a macro for collecting

(defmacro with-collection (fun-name kind &body body)
  (let* ((collection (gensym))
	 (init (gensym))
	 (add-param (gensym))
	 (finish (gensym)))
    (multiple-value-bind
	  (p i a f) (make-collector-0 kind)
      (declare (ignore p))
      `(let* ((,init ,i)
	      ,@(if f `((,finish ,f)) '())
	      (,collection (funcall ,init)))
	 (flet 
	     ((,fun-name (&rest ,add-param)
		(setq ,collection (apply ,a ,collection ,add-param))))
	   ,@body
	   ,(if f `(funcall ,finish ,collection)
		collection))))))

