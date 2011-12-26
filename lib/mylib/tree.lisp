(defpackage :hjs.data.tree
  (:use :cl :hjs.meta.essential :hjs.data.sequence)
  (:export #:recursive-find-all
	   #:flatmap
	   #:maptree
	   #:dotree))

(in-package :hjs.data.tree)


(defun flatmap (function tree) 
  (if (not (listp tree))
      (list (funcall function tree))
      (foldl #'append '() (mapcar #'(lambda (x)
				      (flatmap function x))
				  tree))))

(defun maptree (function tree)
  (if (not (listp tree))
      (funcall function tree)
      (mapcar #'(lambda (x)
		  (maptree function x))
	      tree)))

(defmacro dotree ((name tree &optional ret-val) &body body)
  (with-unique-names (traverser list list-element)
    `(progn
       (labels ((,traverser (,list)
		  (dolist (,list-element ,list)
		    (if (consp ,list-element)
			(let ((,name ,list-element))
			  ,@body
			  (,traverser ,list-element))
			(let ((,name ,list-element))
			  ,@body)))))
	 (,traverser ,tree)
	 ,ret-val))))

(defun recursive-find-all (predicate tree)
  (let ((result '()))
    (labels ((iter (predicate tree)
		   (when (funcall predicate tree)
		     (push tree result))
		   (when (listp tree)
		     (dotree (x tree)
		       (iter predicate x)))))
      (iter predicate tree)
      (nreverse result))))

