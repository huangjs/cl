(in-package :cl-user)

(defpackage :hjs.meta.macro
  (:use :cl :iterate :hjs.meta.essential :hjs.meta.lisp)
  (:export #:defcondition
	   #:defnewconstant
	   #:define-constant
	   #:with-type
	   #:with-collect
	   #:with-functions
	   #:collect-list
	   #:define-class
	   #:defalias
	   #:switch
	   #:eswitch
	   #:cswitch
	   #:whichever
	   #:xor
	   #:constantly-multiple-values
	   ))

(in-package :hjs.meta.macro)


;;;; defcondition
;;; define a condition with conventions
(defmacro defcondition (name (&rest super-conditions) 
			slot-specs format &body args)
  "Provide a conventional way to define a new condition type.

This lets us write

\(PROGN
  (EXPORT '(RECORD-NUMBER-TOO-LARGE-ERROR))
  (DEFINE-CONDITION RECORD-NUMBER-TOO-LARGE-ERROR (INVALID-RECORD-NUMBER-ERROR)
	((RECORD-COUNT :INITARG :RECORD-COUNT :ACCESSOR
				   RECORD-COUNT-OF))
	(:REPORT
	 (LAMBDA (CONDITION STREAM)
	   (FORMAT STREAM
			   \"Record number ~a is too large for this store. Store size is ~a.\"
			   (RECORD-NUMBER-OF CONDITION)
			   (RECORD-COUNT-OF CONDITION))))))

as

\(defcondition record-number-too-large-error
	(invalid-record-number-error)
	(record-count)
	\"Record number ~a is too large for this store. Store size is ~a.\"
  record-number record-count)

"
  (flet ((massage-slot (slot-spec)
	   (cond ((atom slot-spec)
		  `(,slot-spec 
		    :initarg ,(read-from-string (format nil ":~a" slot-spec))
		    :accessor ,(read-from-string (format nil "~a-of" slot-spec))))
		 (t
		  slot-spec)))
	 (massage-format-arg (arg)
	   (cond ((atom arg)
		  `(,(read-from-string (format nil "~a-of" arg)) condition)) 
		 (t
		  arg))))
    `(progn
       (export '(,name))
       (define-condition ,name ,super-conditions
	 ,(mapcar #'massage-slot slot-specs)
	 (:report (lambda (condition stream)
		    (format stream ,format 
			    ,@(mapcar #'massage-format-arg args))))))))


;;; define constant
(defmacro defnewconstant (name value)
  "Define a new constant or keep unchanged."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (boundp ',name)
       (defconstant ,name ,value
	 "Used for indentation."))))

;;; a more cautious one
(defun extract-function-name (spec)
  "Useful for macros that want to emulate the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(defmacro define-constant (name initial-value &key (test 'eql) documentation)
  "Ensures that the global variable named by NAME is a constant with a value
that is equal under TEST to the result of evaluating INITIAL-VALUE. TEST
defaults to EQL, and if given it must be a symbol naming a function. If
DOCUMENTATION is given, it becomes the documentation string of the constant.

Signals an error if NAME is already a bound non-constant variable.

Signals an error if NAME is already a constant variable whose value is not
equal under TEST to result of evaluating INITIAL-VALUE."
  (setf test (extract-function-name test))
  `(defconstant ,name
     (let ((new ,initial-value))
       (if (boundp ',name)
           (let ((old (symbol-value ',name)))
             (cond
               ((constantp ',name)
                (cond
                  ((,test old new)
                   old)
                  (t
                   (cerror "Try to redefine the constant."
                           "~@<~S is an already defined constant whose value ~
                            ~S is not equal to the provided initial value ~S ~
                            under ~S.~:@>" ',name old new ',test)
                   new)))
               (t
                (cerror "Try to redefine the variable as a constant."
                        "~@<~S is an already bound non-constant variable ~
                         whose value is ~S.~:@>" ',name old)
                new)))
           new))
     ,@(when documentation `(,documentation))))


;;;; with-type
;;;; for numerical computation especially
(defmacro with-type (type expr)
  "Evaluate the arithmetic expression in TYPE.
Adopted from P.Graham `ANSI CL', p 410; with some modifications."
  `(the ,type
     ,(if (and (consp expr)
	       (member (car expr) '(+ - * / abs sin cos tan cot
				    signum log exp expt)
		       :test #'eq))
	  (let ((nexp
		 (labels ((binarize (expr)
			    (if (and (nthcdr 3 expr)
				     (member (car expr) '(+ - * /)))
				(destructuring-bind (op a1 a2 . rest) expr
				  (binarize `(,op (,op ,a1 ,a2) ,@rest)))
				expr)))
		   (binarize expr))))
	    `(,(car nexp) ,@(mapcar #'(lambda (ee) `(with-type ,type ,ee))
				    (cdr nexp))))
	  expr)))

;;;; with-collect
(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors)))
    `(let (,@ret)
       (declare (list ,@ret))
       (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
			  collectors ret)
	 ,@forms
	 (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~s-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~s-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
       (declare (list ,@ret ,@tail))
       (macrolet ,(mapcar (lambda (co re ta tm)
			    `(,co (form)
				  `(let ((,',tm (list ,form)))
				     (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
					 (setf ,',re (setf ,',ta ,',tm))))))
			  collectors ret tail tmp)
	 ,@forms
	 (values ,@ret)))))


;;;; with-functions
(defmacro with-functions (names &body code)
  `(macrolet ,(loop for name in names
		 collect `(,name (&rest args)`(funcall ,',name ,@args)))
     ,@code)) 


;;;; collect -- list comprehension
;; collect by loop
;; (defmacro collect (element &body qualifiers)
;;   (labels ((build-form (qualifiers body)
;; 			 (if (not qualifiers)
;; 				 body
;; 				 (let ((first-form (first qualifiers))
;; 					   (rest-form (rest qualifiers)))
;; 				   (cond ((string-equal (symbol-name (first first-form))
;; 										"FOR")
;; 						  (build-for-clause first-form
;; 											(build-form rest-form body)))
;; 						 (t
;; 						  `(when ,first-form
;; 							 ,(build-form rest-form body)))))))
;; 		   (build-for-clause (form body)
;; 			 `(loop ,@form
;; 				 do ,body)))
;; 	(let ((collector (gensym "COLLECTOR")))
;; 	  `(with-collect (,collector)
;; 		 ,(build-form qualifiers
;; 					  `(,collector ,element))))))


;;;; collect-by-iter
(defmacro collect-list (element &body qualifiers)
  (labels ((build-form (qualifiers body)
	     (if (not qualifiers)
		 body
		 (let ((first-form (first qualifiers))
		       (rest-form (rest qualifiers)))
		   (cond ((string-equal (symbol-name (first first-form))
					"FOR")
			  (build-for-clause first-form
					    (build-form rest-form body)))
			 (t
			  `(when ,first-form
			     ,(build-form rest-form body)))))))
	   (build-for-clause (form body)
	     `(iterate::iter (iterate::for ,@(rest form))
			     ,body)))
    (let ((collector (gensym "COLLECTOR")))
      `(with-collect (,collector)
	 ,(build-form qualifiers
		      `(,collector ,element))))))

(defun merge-slot-option (slot-name slot-option)
  "Merge the customized slot-option with default options. Return the slot spec for defclass."
  (let ( ;; default options
	(accessor-type :accessor)
	(accessor slot-name)
	(initarg (as-keyword slot-name)))
    ;; check for redefinition
    (iter (for l on slot-option by #'cddr)
	  (for key = (first l))
	  (for val = (second l))
	  (cond ((member key '(:reader :writer :accessor))
		 (setf accessor val
		       accessor-type key))
		((eq key :initarg)
		 (setf initarg val))
		(t
		 (collect key into options)
		 (collect val into options)))
	  (finally
	   ;; making slot-option
	   (return `(,accessor-type ,accessor :initarg ,initarg ,@options))))))

(defun make-default-slot-specs (slot-specs)
  "parse the slot-specs and return the slot specs for defclass."
  (let* ((slots (mapcar #'ensure-list slot-specs)))
    (loop for s in slots
       for slot-name = (first s)
       for slot-option = (rest s)
       collect (list* slot-name (merge-slot-option slot-name slot-option)))))

(defun parse-args (name args)
  "Parse the additional class options into defun forms and return multiple values containing both def forms and class options that can be accepted by defclass (:default-initargs :documentation :metaclass).
  Additional class options are:
  (:print slot-names*) which means to display the slot-names and its value in the printed form of the class.
"
  (iter (for (type . vals) in args)
	(cond (	;; print
	       (eq type :print)
	       (let ((format-args
		      (iter (for v in vals)
			    (collect (as-keyword v))
			    (collect v))))
		 (collect
		     `(defmethod print-object ((obj ,name) stream)
			(print-unreadable-object (obj stream :type t)
			  (with-slots ,vals obj
			    (format stream "~{~s ~a ~}" (list ,@format-args)))))
		   into additional-class-options)))
	      ;; clhs class-options
	      (t
	       (collect (cons type vals) into clhs-class-options)))
	(finally (return (values clhs-class-options additional-class-options)))))

(defmacro define-class (name (&rest super-classes)
			slot-specs &body other-args)
  (let ((expanded-specs (make-default-slot-specs slot-specs)))
    (multiple-value-bind (class-options generic-fns)
	(parse-args name other-args)
      `(progn
	 (defclass ,name ,super-classes
	   ,expanded-specs
	   ,@class-options)
;; 	 (defun ,(concatenate-symbols 'make- name)
;; 	     ,(mapcar #'first expanded-specs)
;; 	   (make-instance ',name ,@(alist->plist
;; 				    (pairlis (mapcar #'fifth expanded-specs)
;; 					     (mapcar #'first expanded-specs)))))
	 ,@generic-fns))))


(defmacro with-package (package-symbol &body body)
  (let ((last-package (gensym)))
    `(let ((,last-package *package*))
       (setq *package* (find-package ,package-symbol))
       (prog1
           (progn ,@body)
         (setq *package* ,last-package))))) 


(defmacro defalias (new old &key (space :symbol))
  (ecase space
    (:symbol
     `(setf (symbol-value ',new) (symbol-value ',old)))
    (:function
     `(setf (symbol-function ',new) (symbol-function ',old)))))


;;; generic switch, eswitch, cswitch
(defun generate-switch-body (whole object clauses test key &optional default)
  (with-gensyms (value)
    (setf test (extract-function-name test))
    (when (and (consp default)
               (member (first default) '(error cerror)))
      (setf default `(,@default "No keys match in SWITCH. Testing against ~S with ~S."
			 ,value ',test)))
    `(let ((,value (,key ,object)))
       (cond ,@(mapcar (lambda (clause)
			 (if (member (first clause) '(t otherwise))
			     (progn
			       (when default
				 (error "Multiple default clauses or illegal use of a default clause in ~S."
					whole))
			       (setf default `(progn ,@(rest clause)))
			       '(()))
			     (destructuring-bind (key-form &body forms) clause
			       `((,test ,value ,key-form)
				 ,@forms))))
		       clauses)
	     (t ,default)))))

(defmacro switch (&whole whole (object &key (test 'eql) (key 'identity))
		  &body clauses)
  "Evaluates first matching clause, returning its values, or evaluates and
returns the values of DEFAULT if no keys match."
  (generate-switch-body whole object clauses test key))

(defmacro eswitch (&whole whole (object &key (test 'eql) (key 'identity))
		   &body clauses)
  "Like SWITCH, but signals an error if no key matches."
  (generate-switch-body whole object clauses test key '(error)))

(defmacro cswitch (&whole whole (object &key (test 'eql) (key 'identity))
		   &body clauses)
  "Like SWITCH, but signals a continuable error if no key matches."
  (generate-switch-body whole object clauses test key '(cerror "Return NIL from CSWITCH.")))


;;; whichever
(defmacro whichever (&rest possibilities)
  "Evaluates exactly one of POSSIBILITIES, chosen at random."
  `(funcall (the function
              (svref (load-time-value
                      (vector ,@(mapcar (lambda (possibility)
                                          `(lambda () ,possibility))
                                        possibilities))
                      t)
                     (random ,(length possibilities))))))


;;; variadic xor
(defmacro xor (&rest datums)
  "Evaluates its argument one at a time, from left to right. If more then one
argument evaluates to a true value no further DATUMS are evaluated, and NIL is
returned as both primary and secondary value. If exactly one argument
evaluates to true, its value is returned as the primary value after all the
arguments have been evaluated, and T is returned as the secondary value. If no
arguments evaluate to true NIL is retuned as primary, and T as secondary
value."
  (with-gensyms (xor tmp true)
    `(let (,tmp ,true)
       (block ,xor
         ,@(mapcar (lambda (datum)
                     `(if (setf ,tmp ,datum)
                          (if ,true
                              (return-from ,xor (values nil nil))
                              (setf ,true ,tmp))))
                   datums)
         (return-from ,xor (values ,true t))))))


;;; CONSTANTLY-MULTIPLE-VALUES
;;; note: though dynamic-extent is unnecessary in SBCL here.
(defmacro constantly-multiple-values (values)
  `(locally
       (declare (optimize speed (debug 0) (safety 0)))
     (lambda (&rest args)
       (declare (ignore args)
		(dynamic-extent args))
       ,values)))


;;; dbgv -- debugging like progn
(defmacro dbgv ((&optional (where "DEBUG")
                           (stream *standard-output*))
                &body forms)
  "Execute FORMS like PROGN, but print each form and its result to the
STREAM."
  (with-gensyms (result)
    `(let (,result)
       (progn
         (format ,stream "~&DBGV: @~a:~%" ',where)
         ,@(loop for form in forms
              collect `(progn
                         (setf ,result ,form)
                         (format t "~s = ~s~%" ',form ,result)))
         ,result))))

