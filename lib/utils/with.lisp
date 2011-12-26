(in-package :cl-user)

(defpackage :hjs.util.with
  (:use "COMMON-LISP")
  (:export "WITH"
           "DEF-WITH-EXPANDER"
           "DEF-WITH-ALIAS"
           "CANONICAL-BIND-EXPANDER"
           "CANONICAL-WITH-EXPANDER"
           ))

					; -*- Mode: Lisp; Syntax: ANSI-Common-lisp; Package: WITH; Base: 10 -*-

(in-package :hjs.util.with)

;;;
;;; WITH is a conglomeration of let, let*, multiple-value-bind, and
;;; destructuring-bind, in a sort of ML "let" type of syntax.
;;;
;;; Imagine a bastard son of LOOP and LET.
;;;
;;; But it's handy because it helps keep the indentation level down,
;;; is generally syntactically terse while remaining readable, and
;;; it keeps type declarations near their variable.
;;;

;;;   with               <- WITH bindings [ DO | IN ] . body
;;;    body               <- form*
;;;    bindings           <- binding [ conjunction binding ]*
;;;    conjunction        <- AND |
;;;    binding            <- undeclared-binding [ declaration ]
;;;    declaration        <- [ AS | :: ] type |
;;;                          DECLARE declare-clause*
;;;    undeclared-binding <- modal-var-bind-form assgn form
;;;    modal-var-bind-form<- mode var |
;;;                          mode ( var+ )
;;;    mode               <- VAR |
;;;                          SPECIAL |
;;;                          VARS |
;;;                          DESTR |
;;;                          FN |
;;;                          REC |
;;;                          OPEN-FILE |
;;;                          OPEN-STREAM |
;;;                          OUTPUT-TO-STRING |
;;;                          INPUT-FROM-STRING |
;;;                          SLOTS |
;;;                          ACCESSORS |
;;;                          other-defined-mode
;;;    assgn                 usually [ = | <- | := ], but can be defined
;;;                          for each binding-mode.
;;;

(defmacro with (&rest stuff)
  #+:Symbolics (declare (zwei:indentation . zwei:indent-tagbody))
  (expand-with stuff))
#+:Lispworks (editor:setup-indent "with" 0 3 3)

					;
					; The best formatting strategy under both Genera and Lispworks seems to be
					; to put the conjunction at the start of the line, like:
					;
					;   (with a = 1
					;     and b = 2
					;     do (frob a b)
					;        ...)
					;
					; ...although Zwei does a better job at it.  Actually, both editors need
					; a good hook to provide LOOP-style indenting support, maybe a function
					; that takes a list and returns a list of relative indents for each
					; top-level form in the list


(defstruct (bindinfo (:type list)
                     (:constructor make-bindinfo (vars vals decls assgn)))
  vars vals decls assgn)

(defun expand-with (lst)
  (multiple-value-bind (bindings body)
      (parse-with lst)
    (expand-with-bindings bindings body)))

(defun expand-with-bindings (bindings body)
  (if (null bindings)
      `(progn .,body)
      (let ((binding-group (first bindings))
	    (rest-bindings (rest bindings)))
	(expand-with-binding (first binding-group)
			     (rest binding-group)
			     (expand-with-bindings rest-bindings
						   body)))))

;;; Expand a (possibly parallel) set of bindings based on the binding-mode
(defun expand-with-binding (binding-mode bindings body)
  (let ((expanderfn (mode-expander binding-mode)))
    (unless expanderfn
      (error "~S is not a recognized WITH binding mode" binding-mode))
    (funcall expanderfn bindings body)))

;;; Return two values: the body forms, and a list of 
;;; (mode (variables assignment values declarations) ...)
(defun parse-with (lst)
  (cond ((bindings-terminator-p (car lst))
         (values nil (parse-with-body lst)))
        (t (multiple-value-bind (bindings lst)
               (parse-with-bindings lst)
             (values bindings (parse-with-body lst))))))

;;; returns list of (SEQ|PAR mode vars vals decls)
(defun parse-with-bindings (lst &aux (conj 'ALSO) mode vars assgn vals decls)
  (let ((blst (cons conj lst)))
    (values
     (compress-bindings
      (loop while (or (conjunction-kw-p (first blst))
		      (mode-symbol (first blst)))
	 do (multiple-value-setq (conj mode vars assgn vals decls blst)
	      (parse-with-binding blst))
	 collect (cons conj (cons mode (make-bindinfo vars vals decls assgn)))))
     blst)))

(defun parse-with-body (lst)
  (unless (bindings-terminator-p (first lst))
    (error "WITH error, DO or IN expected at ~S" lst))
  (rest lst))

;;; collapse consecutive ALSO bindings 
(defun compress-bindings (lst &aux new-bindings)
  (loop with bindgroup = (list (rest (rest (first lst))))
     with groupmode = (second (first lst))
     for binding in (cdr lst)
     do (cond ((and (eq (first binding) 'PAR)
		    (eq (second binding) groupmode)
		    (mode-parallelp groupmode))
	       (push (rest (rest binding)) bindgroup))
	      ((and (eq (first binding) 'PAR)
		    (not (equal (second binding) groupmode)))
	       (error "WITH cannot bind ~A in parallel with ~A"
		      groupmode (second binding)))
	      ((and (eq (first binding) 'PAR)
		    (not (mode-parallelp groupmode)))
	       (error "WITH cannot bind ~A in parallel"
		      groupmode ))
	      ((eq (first binding) 'SEQ)
	       (push (cons groupmode (nreverse bindgroup))
		     new-bindings)
	       (setq bindgroup (list (rest (rest binding))))
	       (setq groupmode (second  binding)))
	      (t (error "~S fell off end of compress-bindings case, groupmode = ~S -- ~A, ~A, ~A"
			binding groupmode
			(eq (first binding) 'SEQ)
			(eq (second binding) groupmode)
			(mode-parallelp groupmode)
			)))
     finally (push (cons groupmode (nreverse bindgroup))
		   new-bindings))
  (nreverse new-bindings))

;;; Returns (values AND|ALSO mode vars vals decls rest)
;;; Don't get here unless the first token is a conjunction or a mode
(defun parse-with-binding (lst &aux (conj 'SEQ) (mode 'VAR) vars eqtok
			   vals decltok decls mode-eqtoks)
  (when (conjunction-kw-p (first lst))
    (setq conj (canonicalize-conjunction (pop lst))))
  (let ((rest lst))
    (cond ((mode-kw-p (first rest))
           (setq mode (mode-symbol (first rest))
                 mode-eqtoks (mode-eq-toks mode))
           (pop rest))
          (t (error "WITH syntax error: Missing mode indicator (VAR, VALS etc) in ~S" lst)))
					;(setq mode 'var
					;      mode-eqtoks (mode-eq-toks mode))))
    (setq vars (pop rest))
    (setq eqtok (pop rest))
    (unless (kw-in-set-p eqtok mode-eqtoks)
      (error "WITH syntax error: Missing assignment operator; found ~A, expected ~A in ~S"
             eqtok mode-eqtoks lst))
    (setq vals (pop rest))
    (when (declaration-kw-p (first rest))
      (multiple-value-setq (decltok decls rest)
	(parse-with-decls rest))
      (if (type-declaration-kw-p decltok)
	  (setq decls `((type ,decls ,@(if (atom vars) (list vars) vars))))
	  (setq decls decls)))
    ;; specials are, well, special
    (if (eq mode 'SPECIAL)
	(values conj 'VAR vars eqtok vals `(declare (special ,vars) .,decls) rest)
	(values conj mode vars eqtok vals (if decls `(declare .,decls)) rest))))

;;; Returns (values AS|DECLARE decls rest)
;;; Don't get here unless the first symbol is a declaration-keyword
(defun parse-with-decls (lst &aux decltok)
  (setq decltok (pop lst))
  (cond ((type-declaration-kw-p decltok)
	 (values decltok (first lst) (rest lst)))
	((symbol-name-equal decltok "DECLARE")
	 (let ((decls (loop for x in lst
			 until (decl-terminating-kw-p x)
			 collect x
			 do (pop lst))))
	   (values decltok decls lst)))
	(t (error "~S is not a WITH declaration keyword" decltok))))

(defun decl-terminating-kw-p (x)
  (or (null x)
      (member x '("AND" "ALSO" "DO" "IN")
              :test #'symbol-name-equal)
      (mode-kw-p x)))

(defun type-declaration-kw-p (x)
  (or (symbol-name-equal x "AS")
      (symbol-name-equal x ":")))

(defun declaration-kw-p (x)
  (or (type-declaration-kw-p x)
      (symbol-name-equal x "DECLARE")))

(defun conjunction-kw-p (x)
  (or (symbol-name-equal x "AND")
      (symbol-name-equal x "ALSO")))

(defun canonicalize-conjunction (x)
  (cond ((symbol-name-equal x "AND") 'PAR)
        ((symbol-name-equal x "ALSO") 'SEQ)))

(defun bindings-terminator-p (x)
  (or (symbol-name-equal x "DO")
      (symbol-name-equal x "IN")))

(defparameter +default-eq-toks+ '(= := <-)) ; FIXME: use defconstant when
(defun kw-in-set-p (x &optional (valid-eq-toks +default-eq-toks+))
  (member x valid-eq-toks :test #'symbol-name-equal))


;;; Mostly gets loaded towards the end of this file, except for
;;; special, which is handled specially.
(defstruct (modeinfo (:type list)
                     (:constructor make-modeinfo (symbol expander parallelp eqtoks)))
  symbol expander parallelp eqtoks)

(defparameter *with-modes* nil)

(defun symbol-name-equal (x y)
  (cond ((and (symbolp x) (symbolp y))
	 (string= (symbol-name x) (symbol-name y)))
	((and (symbolp x) (stringp y))
	 (string= (symbol-name x) y))
	((and (stringp x) (symbolp y))
	 (string= x (symbol-name y)))))

(defun add-mode-entry (modename expanderfn parallelp eqtoks)
  (push (make-modeinfo modename expanderfn parallelp eqtoks) *with-modes*))
(defun find-mode-entry (x)
  (and (symbolp x) (assoc x *with-modes* :test #'symbol-name-equal)))
(defun mode-kw-p (x)
  (and (find-mode-entry x) t))
(defun mode-symbol (x)
  (let ((entry (find-mode-entry x)))
    (when entry (modeinfo-symbol entry))))
(defun mode-expander (x)
  (let ((entry (find-mode-entry x)))
    (when entry (modeinfo-expander entry))))
(defun mode-parallelp (x)
  (let ((entry (find-mode-entry x)))
    (when entry (modeinfo-parallelp entry))))
(defun mode-eq-toks (x)
  (let ((entry (find-mode-entry x)))
    (when entry (modeinfo-eqtoks entry))))

(defun ensure-with-expander (modename fn parallelp
			     &optional (eqtoks +default-eq-toks+))
  (let ((entry (find-mode-entry modename)))
    (if entry
	(setf (modeinfo-expander entry) fn
	      (modeinfo-parallelp entry) parallelp
              (modeinfo-eqtoks entry) eqtoks)
	(add-mode-entry modename fn parallelp eqtoks))
    modename))

(defun ensure-with-expander-alias (dst src)
  (let ((srcentry (find-mode-entry src)))
    (unless srcentry
      (error "Can't find WITH mode ~A" src))
    (ensure-with-expander dst (second srcentry) (third srcentry))))

(defmacro def-with-expander (fnname modename
			     (bindingsvar bodyvar
                                          &key (parallel nil)
                                          (assignments '+default-eq-toks+))
			     &rest body)
  #+:Symbolics (declare (zwei:indentation 3 1))
  `(progn
     (defun ,fnname (,bindingsvar ,bodyvar)
       #+:Symbolics (declare (sys:function-parent ,modename with-expander))
       .,body)
     (ensure-with-expander ',modename ',fnname ,parallel ,assignments)
     ',fnname))
#+:Lispworks (editor:setup-indent "def-with-expander" 3 2 7)

(defmacro def-with-alias (dstmodename srcmodename)
  `(ensure-with-expander-alias ',dstmodename ',srcmodename))


;;;;
;;;; Standard expanders
;;;;


;;; For expanding things like multiple-value-bind and destructuring-bind.
(defun canonical-bind-expander (type bindings bodyform)
  (let* ((bindinfo (first bindings))
	 (vars (bindinfo-vars bindinfo))
	 (vals (bindinfo-vals bindinfo))
	 (decls (bindinfo-decls bindinfo)))
    `(,type ,vars ,vals
	    ,decls
	    ,bodyform)))

;;; For expanding all those with-xyzzy forms in common lisp, statice,
;;; clos, etc.
(defun canonical-with-expander (type bindings bodyform)
  (let* ((bindinfo (first bindings))
	 (varname (bindinfo-vars bindinfo))
	 (args (bindinfo-vals bindinfo))
	 (decls (bindinfo-decls bindinfo)))
    `(,type (,varname ,@args)
	    ,decls
	    ,bodyform)))

;;; Just makes sure that special is in there.  It gets expanded
;;; specially.
(ensure-with-expander 'special nil t)

;;; Expand simple variable bindings, either serial or parallel.
(def-with-expander expand-with-var var (bindings bodyform :parallel t)
		   (let (let-bindings decls)
		     (loop for (var val decl) in bindings
			when decl do (push decl decls)
			do (push (list var val) let-bindings))
		     `(let ,(nreverse let-bindings)
			,@(nreverse decls)
			,bodyform)))


;;; Expand VALUES, VALS, and VARS as multiple-value-bind
(def-with-expander expand-with-values vars (bindings bodyform)
		   (canonical-bind-expander 'multiple-value-bind bindings bodyform))
(def-with-alias vals vars)
(def-with-alias values vals)


;;; Expand DESTRUCTURE and DESTR as destructuring-bind
(def-with-expander expand-with-destructure destructure
  (bindings bodyform :assignments '(= <- := from in))
  (canonical-bind-expander 'destructuring-bind bindings bodyform))
(def-with-alias DESTR DESTRUCTURE)


;;; Expand FN as flet
(def-with-expander expand-with-fn fn (bindings bodyform :parallel t)
		   (let (flet-bindings decls)
		     (loop for (fnname fnval decl) in bindings
			when decl do (push decl decls)
			unless (eq (first fnval) 'LAMBDA)
			do (error "FN value form ~S must be a LAMBDA form" fnval)
			do (push (cons fnname (rest fnval)) flet-bindings))
		     `(flet ,(nreverse flet-bindings)
			,@(nreverse decls)
			,bodyform)))


;;; Expand REC as labels
(def-with-expander expand-with-rec rec (bindings bodyform :parallel t)
		   (let (labels-bindings decls)
		     (loop for (fnname fnval decl) in bindings
			when decl do (push decl decls)
			unless (eq (first fnval) 'LAMBDA)
			do (error "REC value form ~S must be a LAMBDA form" fnval)
			do (push (cons fnname (rest fnval)) labels-bindings))
		     `(labels ,(nreverse labels-bindings)
			,@(nreverse decls)
			,bodyform)))

;;; Expand open-file as with-open-file
(def-with-expander expand-with-open-file open-file (bindings bodyform)
		   (canonical-with-expander 'with-open-file bindings bodyform))

;;; Expand output-to-string and string-output as with-output-to-string
(def-with-expander expand-with-output-to-string output-to-string
  (bindings bodyform)
  (canonical-with-expander 'with-output-to-string bindings bodyform))
(def-with-alias string-output output-to-string)

;;; Expand input-from-string and string-input as with-input-from-string
(def-with-expander expand-with-input-from-string input-from-string
  (bindings bodyform)
  (canonical-with-expander 'with-input-from-string bindings bodyform))
(def-with-alias string-input input-from-string)

;;; Expand with-open-stream as open-stream
(def-with-expander expand-with-open-stream open-stream
  (bindings bodyform)
  (let* ((bindinfo (first bindings))
	 (varname (bindinfo-vars bindinfo))
	 (args (bindinfo-vals bindinfo))
	 (decls (bindinfo-decls bindinfo)))
    `(with-open-stream (,varname ,args)
       ,decls
       ,bodyform)))

;;; Expand slots as with-slots
(def-with-expander expand-with-slots slots
  (bindings bodyform :assignments '(of in))
  (canonical-bind-expander 'with-slots bindings bodyform))

;;; Expand accessors as with-accessors
(def-with-expander expand-with-accessors accessors
  (bindings bodyform :assignments '(of in))
  (canonical-bind-expander 'with-accessors bindings bodyform))

