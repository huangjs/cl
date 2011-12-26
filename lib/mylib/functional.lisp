(in-package :cl-user)

(defpackage :hjs.meta.functional
  (:use :cl)
  (:export #:compose
	   #:multiple-value-compose
	   #:curry
	   #:rcurry
	   #:negate
	   #:make-pipeline
	   #:make-filter
	   #:apply-fns
	   #:disjoin
	   #:conjoin
	   #:named-lambda
	   ))

(in-package :hjs.meta.functional)

(defun curry (function &rest arguments)
  "Returns a function that applies ARGUMENTS and the arguments
it is called with to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function function))
  (lambda (&rest more)
    (declare (dynamic-extent more))
    ;; Using M-V-C we don't need to append the arguments.
    (multiple-value-call function (values-list arguments) (values-list more))))

(define-compiler-macro curry (function &rest arguments)
  (let ((curries (make-gensym-list (length arguments) "CURRY")))
    `(let ,(mapcar #'list curries arguments)
       (declare (optimize (speed 3) (safety 1) (debug 1)))
       (lambda (&rest more)
         (apply ,function ,@curries more)))))

(defun rcurry (function &rest arguments)
  "Returns a function that applies the arguments it is called
with and ARGUMENTS to FUNCTION."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function function))
  (lambda (&rest more)
    (declare (dynamic-extent more))                 
    (multiple-value-call function (values-list more) (values-list arguments))))

;; (defun curry (fn &rest args)
;;   (lambda (&rest more-args)
;;     (apply fn (append args more-args))))

(defun negate (fn)
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function fn))
  #'(lambda (&rest args)
      (not (apply fn args))))

(defun  make-pipeline (&rest fns)
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (dynamic-extent fns))
  (apply #'compose (reverse fns)))

(defun make-filter (predicate)
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function predicate))
  (lambda (sequence)
    (remove-if-not predicate sequence)))


;;; compose
(defun make-gensym-list (length &optional x)
  "Returns a list of LENGTH gensyms, each generated with a call to
GENSYM using (if provided) as the argument."
  (loop repeat length
        collect (gensym x)))

(defun compose (function &rest more-functions)
  "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies its
arguments to to each in turn, starting from the rightmost of MORE-FUNCTIONS,
and then calling the next one with the primary value of the last."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function function))
  (reduce (lambda (f g)
            (lambda (&rest arguments)
              (declare (dynamic-extent arguments)
		       (type function f g))
              (funcall f (apply g arguments))))
          more-functions
          :initial-value function))

(define-compiler-macro compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(funcall ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))

;; ;; This is really slow and conses a lot. Fortunately we can speed it
;; ;; up immensely with a compiler macro.
;; (defun compose (&rest functions)
;;   "Compose FUNCTIONS right-associatively, returning a function"
;;   #'(lambda (x)
;;       (reduce #'funcall functions
;; 	      :initial-value x
;; 	      :from-end t)))

;; ;; Converts calls to COMPOSE to lambda forms with everything written
;; ;; out and some things written as direct function calls.
;; ;; Example: (compose #'1+ #'2* #'1+) => (LAMBDA (X) (1+ (2* (1+ X))))
;; (define-compiler-macro compose (&rest functions)
;;   (labels ((sharp-quoted-p (x)
;; 	     (and (listp x)
;; 		  (eql (first x) 'function)
;; 		  (symbolp (second x)))))
;;     `(lambda (x) ,(reduce #'(lambda (fun arg)
;; 			      (if (sharp-quoted-p fun)
;; 				  (list (second fun) arg)
;; 				  (list 'funcall fun arg)))
;; 			  functions
;; 			  :initial-value 'x
;; 			  :from-end t))))

(defun multiple-value-compose (function &rest more-functions)
    "Returns a function composed of FUNCTION and MORE-FUNCTIONS that applies
its arguments to to each in turn, starting from the rightmost of
MORE-FUNCTIONS, and then calling the next one with all the return values of
the last."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function function))
  (reduce (lambda (f g)
            (lambda (&rest arguments)
              (declare (dynamic-extent arguments)
		       (type function f g))
              (multiple-value-call f (apply g arguments))))
          more-functions
          :initial-value function))

(define-compiler-macro multiple-value-compose (function &rest more-functions)
  (labels ((compose-1 (funs)
             (if (cdr funs)
                 `(multiple-value-call ,(car funs) ,(compose-1 (cdr funs)))
                 `(apply ,(car funs) arguments))))
    (let* ((args (cons function more-functions))
           (funs (make-gensym-list (length args) "MV-COMPOSE")))
      `(let ,(mapcar #'list funs args)
         (declare (optimize (speed 3) (safety 1) (debug 1)))
         (lambda (&rest arguments)
           (declare (dynamic-extent arguments))
           ,(compose-1 funs))))))



(defun apply-fns (fn-list &rest args)
  "Apply every function in fn-list with the arguments, 
and collect the result."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (dynamic-extent fn-list))
  (mapcar (lambda (fn)
	    (declare (type function fn))
	    (apply fn args)) fn-list))


;;; disjoin conjoin
(defun disjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning the primary value of the first
predicate that returns true, without calling the remaining predicates.
If none of the predicates returns true, NIL is returned."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function predicate))
  (lambda (&rest arguments)
    (or (apply predicate arguments)
        (some (lambda (p)
		(declare (type function p))
                (apply p arguments))
              more-predicates))))

(defun conjoin (predicate &rest more-predicates)
  "Returns a function that applies each of PREDICATE and MORE-PREDICATE
functions in turn to its arguments, returning NIL if any of the predicates
returns false, without calling the remaining predicated. If none of the
predicates returns false, returns the primary value of the last predicate."
  (declare (optimize (speed 3) (safety 1) (debug 1))
	   (type function predicate))
  (lambda (&rest arguments)
    (and (apply predicate arguments)
         (do ((tail (cdr more-predicates) (cdr tail))
              (head (car more-predicates) (car tail)))
             ((not tail)
              (apply head arguments))
           (unless (apply head arguments)
             (return nil))))))

;;; named lambda
;;; able to define recursive anonymous functions
;;; example: (funcall (named-lambda F (N) (if (zerop N) 1 (* N (F (- N 1))))) 5) => 120
(defmacro named-lambda (name lambda-list &body body)
  "Expands into a lambda-expression within whose BODY NAME denotes the
corresponding function."
  `(labels ((,name ,lambda-list ,@body))
     #',name))

