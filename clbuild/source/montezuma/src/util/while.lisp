(in-package #:montezuma)

;; Actually this is just kind of the dumping ground for useful macros.

(defmacro while (expr &body body)
  `(do ()
       ((not ,expr))
     ,@body))

#||
(defmacro dosequence ((var sequence &optional ret) &body body)
  `(let ((,var nil))
     (map nil
	  #'(lambda (,var)
	      ,@body body)
	  ,sequence)
     ,ret))
||#

(defmacro dosequence ((var sequence &key result index) &body body)
  `(block nil
     (let ((,var nil)
	   ,@(if index `((,index 0)) nil))
       (declare (ignorable ,var))
       (map nil
	    #'(lambda (,var)
		,@body
		,@(if index `((incf ,index)) nil))
	    ,sequence)
       ,result)))

;; Hey, it's just like Python's range function!
(defmacro do-range ((var start-form end-form &optional result-form) &body body)
  "Iterates VAR through the range of integers in [START-FORM,
  END-FORM).  Returns the value of END-FORM (at the time END-FORM is
  evaluated, VAR is bound to the value of END-FORM.

  (do-range (i 10 (length s))
    (print (elt s i)))"
  (let ((start-var (gensym))
	(end-var (gensym)))
    `(let ((,start-var ,start-form)
	   (,end-var ,end-form))
       (do ((,var ,start-var (1+ ,var)))
	   ((>= ,var ,end-var) ,result-form)
	 ,@body))))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for nil in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
	  ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
		,@body)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun inject (sequence base function &key (key #'identity))
  (let ((result base))
    (dosequence (element sequence)
      (setf result (funcall function base (funcall key element))))
    result))
