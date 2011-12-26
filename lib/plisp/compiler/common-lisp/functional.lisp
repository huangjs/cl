;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;; Stuff which handles functional values.

;;; Common lisp functions implemented here:
;;;   function (#') funcall apply

(define-ps function
  (let ((fun (cadr ps-form)))
    (cond ((symbolp fun) (compile-q fun))
	  ((and (consp fun) (eq (car fun) 'lambda))
	   (compile-lambda (cadr fun) (cddr fun)))
	  (T (ps-error "Can't interpret the following as a function" fun))))
  1)

(defun compile-lambda (args body)
  (let*
      (
       (lexical-vars nil)
       (current-frame next-frame)
       (lexicals-here nil)
       (current-fn next-frame))
    (incf next-frame)
    (put-att frame-table current-frame 'parent '**main**)
    (emit '\{)             ; start code object
    (emit `(alloc-frame ,current-frame))
    (emit 'begin)          ; enter new environment
    (for (:in arg (reverse args))
                      ; bind parameters into local dict
	 (:do
	  (bind-it arg)))
    (ps-compile (cons 'progn body)) ; compile the body
    (wrap-frame)
    (emit '\})
    ))

(define-ps funcall
  (let ((fun (cadr ps-form))
	(args (cddr ps-form)))
      (emit '\[)
      (for (:in x args)
           (:do (compile-1 x)))
      (compile-1 fun)
      (emit 'cvx)
      (emit 'exec)
      (emit '\])
      (push-att frame-table current-frame 'uses current-frame)
      1))

(define-ps apply
  (let ((fun (cadr ps-form))
	(arg (caddr ps-form)))
      (emit '\[)
      (compile-1 arg)
      (emit 'aload)
      (ps-delete 1)
      (compile-1 fun)
      (emit 'cvx)
      (emit 'exec)
      (emit '\])
      (push-att frame-table current-frame 'uses current-frame)
      1))
