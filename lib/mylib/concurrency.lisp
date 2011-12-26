(in-package :cl-user)

(defpackage :hjs.control.concurrency
  (:use #:common-lisp #:iterate
	#:hjs.meta.lisp #:hjs.meta.functional #:hjs.meta.type
	#:hjs.meta.macro #:hjs.meta.essential
	#:hjs.control.thread-pool)
  (:export #:spawn
	   #:value
	   #:sync))

(in-package :hjs.control.concurrency)

;;; cilk like primitives and runtime
(defstruct future-object 
  (mutex (sb-thread:make-mutex) :type sb-thread:mutex)
  (evaled-p nil :type boolean)
  value)

(defun %spawn (thunk)
  (let ((result (make-future-object)))
    (sb-thread:make-thread (lambda () (funcall thunk result)))
    result))

(defmacro spawn (expression)
  (with-unique-names (result)
    (let ((thunk `(lambda (,result)
		    (sb-thread:with-mutex ((future-object-mutex ,result))
		      (setf (future-object-value ,result)
			    ,expression)
		      (setf (future-object-evaled-p ,result) t)))))
      `(%spawn ,thunk))))

(defun value (future-object)
  (sb-thread:with-mutex ((future-object-mutex future-object))
    (future-object-value future-object)))

(defun sync (&rest future-objects)
  )
