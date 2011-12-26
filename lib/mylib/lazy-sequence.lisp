(in-package :cl-user)

(defpackage :hjs.data.lazy-sequence
  (:nicknames :hjs.ll)
  (:use #:common-lisp #:iterate)
  (:export #:merge2
	   #:integers
	   #:take
	   #:take-when
	   #:lazy-position
	   #:lazy-position-if
	   ))

(in-package :hjs.data.lazy-sequence)

;;; merge take take-when
(defun merge2 (list1 list2 &key (comparator #'<) (test #'=))
  (cond ((null list1) list2)
	((null list2) list1)
	(t
	 (let ((x (ll:car list1))
	       (y (ll:car list2)))
	   (cond ((funcall comparator x y)
		  (ll:cons x (merge2 (ll:cdr list1) list2)))
		 ((funcall test x y)
		  (ll:cons x (merge2 (ll:cdr list1) (ll:cdr list2))))
		 (t
		  (ll:cons y (merge2 list1 (ll:cdr list2)))))))))

(defun take (n list)
  (ll:subseq list 0 n))

(defun take-when (pred list)
  (let ((head (ll:first list)))
    (if (funcall pred head)
	(ll:cons head (take-when pred (ll:cdr list)))
	nil)))


;;; todo: complete the api, and put them into lazy-list
;;; position/position-if
(defun lazy-position (item ll &key (test #'eql) key)
  (iter (for i from 0)
	(for e in ll by #'ll:cdr)
	(when (funcall test item (if key (funcall key e) e))
	  (return (values i e)))))

(defun lazy-position-if (pred list)
  (iter (for i from 0)
	(for e in list by #'ll:cdr)
	(when (funcall pred e)
	  (return (values i e)))))

