;;; Quick Arrays v. 1.0
;;;
;;; Ronald Parr
;;;
;;; Last Modification 6/6/96
;;;
;;;
;;; This code is (c) 1996 Ronald Parr.  It is distributed with no warranty
;;; of any kind.  Use at your own risk.  You may distribute this code
;;; freely as long as you do not charge for it.
;;;
;;; Compile and load this file to use quick arrays
;;;
;;; The file implements a subset of the standard array access and creation
;;; functions for quick arrays.  I may add more operations if time permits
;;; and there is sufficient need or demand.
;;;
;;; quick array functions operate (mostly) like their regular array
;;; counterparts, but have a q appended to their names.  Read the 
;;; comments for each function before using for the first time.
;;;
;;; ***IMPORTANT*** use qref whenever possible to obtain maximum speedup
;;; qaref should be used only situations where macros cannot be used.


(in-package :common-lisp-user)
(declaim (optimize (speed 3) (debug 0) (safety 0)))

;;;
;;; Currently supports only initial-contents, and initial-element.
;;; element type is ignored.
;;;
(defun qmake-array (dims &key initial-contents initial-element element-type)
  "See make-array."
  (declare (ignore element-type))
  (cond ((or (numberp dims) (null (rest dims)))
	 (if initial-contents
	     (make-array dims :initial-contents initial-contents)
	   (if initial-element
	       (make-array dims :initial-element initial-element)
	     (make-array dims))))
	(initial-contents
	 (make-array (list (first dims))
		     :initial-contents
		     (map 'list
		       #'(lambda (init)
			   (qmake-array (rest dims)
                                        :initial-contents init))
		       initial-contents)))
	(t
	 (make-array (list (first dims))
		     :initial-contents
		     (let ((temp nil))
		       (dotimes (i (first dims))
			 (push
                          (qmake-array (rest dims)
                                       :initial-element initial-element) temp))
		       temp)))))
;;;
;;; Helper macro for qref.  Do not call directly.
;;;
(defmacro qref-1 (array &rest args)
  "Main replacement for aref, but with args reversed."
  (if (null (rest args))
      `(svref ,array ,(first args))
    `(svref (qref-1 ,array ,@(rest args)) ,(first args))))

;;;
;;; Use this for accessing quick arrays.  Use (setf (qref ...) ...) to
;;; change array entries.
;;;
(defmacro qref (array &rest args)
  "Replacement for aref."
  `(qref-1 ,array ,@(reverse args)))

;;;
;;; Helper function for qaref.
;;;
(defun qaref-1 (array args)
  "Function version of qref-1."
  (declare (inline qaref-1)
           (inline svref)
           (inline first)
           (inline rest))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (if (null (rest args))
    (svref array (first args))
    (qaref-1 (svref array (first args)) (rest args))))

;;;
;;; This is the function version of qref.  It is slow.  Use it in situations
;;; where you must access an array with a function.  For example, if you need
;;; to use apply, then you must provide apply with a function instead of
;;; a macro.  See setf method below.
;;;
(defun qaref (array &rest args)
  "Function version of qref."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (declare (inline qaref-1))
  (qaref-1 array args))

;;;
;;; Helper function for setf method for qaref.  Do not call this directly.
;;;
(defun set-qaref (array args val)
  "Function to set values of quick arrays."
  (declare (optimize (speed 3)))
  (declare (inline set-qaref))
  (if (null (rest args))
    (setf (qref array (first args)) val)
    (set-qaref (qref array (first args)) (rest args) val)))

;;;
;;; setf method for qaref.  This is *very* slow.  Do not call this unless
;;; you absolutely have to use a function instead of a macro to modify
;;; an array entry.  Otherwise, use (setf (qref ...) ...) instead.
;;;

;;; I had to work around a bug in MCL 3.9 below.  I don't know why the
;;; code for MCL below.  Strictly speaking it is incorrect.
;;; If args is (1 2 3), this should be interpreted as a function call to
;;; a function named #'1.  This is what happens if you run the MCL code
;;; in another lisp.


#-:MCL
(defsetf qaref (array &rest args) (val)
  `(set-qaref ,array (list ,@args) ,val))

#+:MCL
(defsetf qaref (array &rest args) (val)
  `(set-qaref ,array ,args ,val))

;;;
;;; quick array replacement for array-dimensions
;;;
(defun qarray-dimensions (array)
  "See array-dimensions."
  (if (not (vectorp array)) nil
    (cons (length array) (qarray-dimensions (svref array 0)))))

;;;
;;; quick array replacement for array-dimension
;;;
(defun qarray-dimension (array index)
  "See array-dimension."
  (if (eql index 0)
    (length array)
    (qarray-dimension (qref array 0) (1- index))))

;;;
;;; Copies a quick array.  This can have unexpected results if the
;;; quick itself contains vectors or other quick arrays, so use with
;;; caution.
;;;
(defun qarray-copy (array)
  "Copies a quick array."
  (if (not (vectorp array))
      array
    (make-array (length array)
                :initial-contents
                (map 'vector #'qarray-copy array))))

