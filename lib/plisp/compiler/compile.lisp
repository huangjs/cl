;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  Postscript compiler routines
;;;;  These functions are the top level postscript compilation primitives.

;;; ps-compile is the top level entry to the compiler.  It compiles any
;;; random postscript code.  The number of values produced by the code compiled
;;; is returned.

(defun ps-compile (fm)
 (cond ((or (stringp fm) (numberp fm) (arrayp fm) (characterp fm)
	    (member fm '(T NIL)))
           ; Self evaluating
	(emit fm)
	1)
       ((symbolp fm)
        (ps-compile-symbol fm))  ; variables and constants
       (T
	(let ((fn (car fm))  ; forms
	      hndlr)
	  (if (not (symbolp fn))
	      (progn (ps-error "Symbol required as function" fn fm)
		     1)
	      (let* ((name (lookup-name fn))
		     (ty (car name)))
		(cond ((eq ty 'postscript)
		       (ps-function-call (cadr name) fm))
		      ((eq ty 'primitive)
		       (funcall (cadr name) fm))
		      ((eq ty 'ps-macro)
		       (ps-compile (ps-macro-expand fm)))
		      ((eq ty 'user-function)
		       (ps-compile-call (cadr name) fn fm))
		      ((eq ty 'user-macro)
		       (ps-compile (ps-macro-expand fm)))
		      (t (ps-error "Undefined function"
				   ty fn fm)
			 1))))))))

;;; ps-macro determines if a function is a postscript defined macro

(defun ps-macro (x)
  (let ((ty (car (lookup-name x))))
    (member ty '(user-macro ps-macro))))

;;; ps-macro-expand performs PS macro expansion (duh).  Since no
;;; apply is available for macros, a normal apply is used, precluding
;;; special macro capabilities (&whole, destructuring)

(defun ps-macro-expand (fm)
    (let ((def (lookup-name (car fm))))
      (cond ((eq (car def) 'ps-macro)
	     (apply (cadr def) (cdr fm)))
	    ((eq (car def) 'user-macro)
	     (macroexpand-1
	      (cons (att (cadr def) (car fm) 'new-name) (cdr fm))))
	    (T (ps-error "Bug")))))

;;; compile a simple symbol.  Look for variable, constant

(defun ps-compile-symbol (sym)
 (let* ((symval (lookup-name sym))
	(ty (car symval)))
   (cond
         ((eq ty 'user-const)
	  (compile-q (att (cadr symval) sym 'value)))
	 ((eq ty 'lexical)
	  (if (cdr symval)
	      (progn
		(when (null (cadddr symval))
		      (let ((new-var 
			     (gentemp (symbol-name (caddr symval)))))
			(setf (car (cdddr symval)) new-var)))
		(compile-q (cadddr symval)))
	      (compile-q sym))		      
	  (emit 'load))
	 ((eq ty 'user-var)
	  (let ((hash (cadr symval)))
	    (import-name hash sym 'V)
	    (compile-q sym)
	    (emit 'load)))
	 ((eq ty 'undefined)
	  (implicit-defvar sym)
	  (ps-compile-symbol sym))
	 (T (ps-error "Symbol can't be used as variable" sym))
	 ))
 1)


;;; Compile a call to a built-in function.  The pat argument is a list
;;; containing the info placed in the hash table when the definition was
;;; processed

(defun ps-function-call (pat body)
   (let ((args-in (car pat))   ; Number of parameters coming in on the stack
	 (args-out (cadr pat))); number of results left on the stack
      (if (/= (length (cdr body)) args-in)
	  (ps-error "Wrong number of arguments" body)
	  (for (:in arg (cdr body)) ; compile each argument; get 1 value each
	       (:do (compile-1 arg))))
      (emit (car body))  ; emit the name itself
      (stuff-false (get (car body) 'stuff-count))
      args-out))  ; return the number of new things on the stack
    
;;; Compile-1 is used when only one value is needed.  This corresponds to
;;; common lisp semantics by throwing out extra values when not needed.

(defun compile-1 (arg)
    (let ((nval (ps-compile arg)))  ; more than 1??
       (cond ((> nval 1)
	      (ps-delete (1- nval)))  ; kill off all but first
	     ((= nval 0) (ps-error "Value required" arg))))
    1)

;;; Compile-q compiles its arg as a constant.  Wrap a ' around it.

(defun compile-q (arg)
    (emit (list 'quote arg)))

;;; This function emits pop operators to clear crud of the stack.  Usually
;;; used when a function returns too many values.

(defun ps-delete (n)
    (for (:from i 1 n) (:do (emit 'pop))))

;;; Compile for no value.  This compiles and then deletes all results.
		
(defun ps-compile-novalue (fm)
    (ps-delete (ps-compile fm)))

;;; This compiles a block of code and emits a code object (enclosed by {})
;;; for postscript.

(defun ps-compile-block (fm)
    (emit '\{)
    (let ((res (ps-compile fm)))
       (emit '\})
       res))

;;; Same (almost) as previous

(defun ps-compile-block-novalue (fm)
    (emit '\{)
    (ps-compile-novalue fm)
    (emit '\}))

;;; This takes the top value on the ps stack and binds it to a name in the
;;; currently active dictionary.  The value is removed from the stack.
;;; This is how function arguments move from the stack to the dictionary.

(defun bind-it (var)
  (if (or (get var 'ps-function) (get var 'ps-lisp-primitive)
	  (get var 'ps-macro))
      (ps-error "Symbol can't use as a variable" var))
  (push (list var) lexical-vars)
  (push var lexicals-here)
  (compile-q var)  ; name of var
  (emit 'exch)
  (emit 'def))

(defun implicit-defvar (sym)
  (define-name main-table sym 'user-var)
  (put-att main-table sym 'init 0)
  (push-end sym to-init))

(defun compile-var (v)
  (let ((vdef (lookup-name v)))
    (cond ((eq (car vdef) 'undefined)
	   (implicit-defvar v))
	  ((not (member (car vdef) '(lexical user-var)))
	   (ps-error v "Symbol can't be used as variable"))))
  (compile-q v))

;;; To make funny operators which return variable args appear to return
;;; a fixed number of args.  search, for example.

(defun stuff-false (n)
  (when n
    (emit 'dup)
    (emit 'not)
    (emit '\{)
    (dotimes (i n) (emit 'false))
    (emit '\})
    (emit 'if)))


        
