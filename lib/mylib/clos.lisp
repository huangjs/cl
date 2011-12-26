(in-package :cl-user)

(defpackage :hjs.meta.clos
  (:use :cl :hjs.meta.essential :hjs.meta.macro :iter)
  (:export #:print-slots
	   #:copy-instance
	   #:get-source
	   ))

(in-package :hjs.meta.clos)


(defun print-slots (class-name &key apropos parent-slots)
  (let* ((slot-function (if parent-slots
			    #'sb-pcl:class-slots
			    #'sb-pcl:class-direct-slots))
	 (slots
	  (sort 
	   (remove-if-not
	    (lambda (s)
	      (if apropos
		  (search apropos s :test #'char-equal)
		  t))
	    (mapcar #'symbol-name
		    (mapcar #'sb-pcl:slot-definition-name
			    (funcall slot-function (find-class class-name)))))
	   #'string-lessp)))
    (format t "~%~{~<~%~1,80:;~20A~> ~}~&" slots)))

(defun copy-instance (i)
  (loop with i-class = (class-of i)
     with c = (allocate-instance i-class)
     for sd in (sb-mop:class-slots i-class)
     for sn = (sb-mop:slot-definition-name sd)
     when (slot-boundp i sn)
     do (progn
	  (setf (slot-value c sn)
		(slot-value i sn))
	  (reinitialize-instance c))
     finally (return c))) 

(defun get-source (class)
  `(defclass ,(class-name class) ,(mapcar #'class-name (sb-mop:class-direct-superclasses class))
     ,(loop for slot in (sb-mop:class-direct-slots class)
	 collect
	 (flet ((a (s) (funcall s slot)))
	   (append
	    (list
	     (a #'sb-mop:slot-definition-name)
	     :initargs (a #'sb-mop:slot-definition-initargs)
	     :initform (a #'sb-mop:slot-definition-initform)
	     :type (a #'sb-mop:slot-definition-type)
	     :allocation (a #'sb-mop:slot-definition-allocation))
	    (mapcan (lambda (x) (list :reader x))
		    (sb-mop:slot-definition-readers slot))
	    (mapcan (lambda (x) (list :writer x))
		    (sb-mop:slot-definition-writers slot)))))))  

