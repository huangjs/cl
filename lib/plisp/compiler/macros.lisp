;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

;;;;  These are all macros used by the postscript compiler

;;; define-ps is a defun for the postscript view of a function.  The code
;;; is executed as normal lisp.  The function itself is stored under the symbol
;;; name-PSFUN to avoid name collisions with actual lisp functions.  This
;;; macro has the side effect (gasp!) that this name is stored on the property
;;; list of the actual name where the postscript compiler will find it.

(defmacro define-ps (fn . body)
    (let ((lisp-fun
	   (intern (concatenate 'string (symbol-name fn) "-PSFUN"))))
      `(progn
        (setf (get ',fn 'ps-lisp-primitive) ',lisp-fun) ; side effect!!
       (defun ,lisp-fun (ps-form) ,@body))))   ; the real defun
	   
;;; define-mac is the analogue of define-ps.  It works as before, except the
;;; result must be compilable plisp code.

(defmacro define-mac (fn args . body)
    (let ((lisp-fun
	   (intern (concatenate 'string (symbol-name fn) "-PSMAC"))))
       `(progn
	  (setf (get ',fn 'ps-macro) ',lisp-fun)
	  (defun ,lisp-fun ,args ,@body))))
	  
;;;   This macro is used to make new frames

(defmacro with-new-frame (&rest body)
  `(progn
     (push-att frame-table current-frame 'uses next-frame)
     (let ((lexical-vars lexical-vars)
	 (lexicals-here nil)
	 (current-frame next-frame))
       (incf next-frame)
       (put-att frame-table current-frame 'parent current-fn)
       (push-att frame-table current-fn 'children current-frame)
       (emit `(alloc-frame ,current-frame))
       (emit 'begin)
       (progn ,@body)
       (wrap-frame))
     nil))


;;;  This is the (HP compatible) for macro for full common lisp portability
;;;  This is a small subset consisting of (for (:in/:on/:from) (:do/:collect))
;;;  :in iterates over the elements of a list, :on over the top (car :on = :in)
;;;  and :from is numeric (:from var init last step).  :do executes statements
;;;  for effect, :collect forms a list of all results.

  ; this quote causes this whole thing to be ignored.  (when HP for in use)

(defmacro for (iterate action)
  (let ((iterator (car iterate))
	(var (cadr iterate))
	(it (cddr iterate))
	(act (car action))
	(body (cons 'progn (cdr action)))
	var-init res-init var-step tst resform)
     (if (not (and (member iterator '(:in :on :from))
		   (member act '(:do :collect))))
	 (format t "Unknown for clause: ~A ~A~%" iterate action)
	 (progn
	    (cond ((eq iterator :from)
		   (setf var-init (car it))
		   (setf var-step `(+ ,var ,(or (caddr it) 1)))
		   (setf tst `(<= ,var ,(cadr it))))
		  ((eq iterator :on)
		   (setf var-init (car it))
		   (setf var-step `(setf ,var (cdr ,var)))
		   (setf tst `(consp ,var)))
		  ((eq iterator :in)
		   (setf var-init (car it))
		   (setf var-step `(setf ***temp*** (cdr ***temp***)))
		   (setf tst `(consp ***temp***))
		   (setf body `(let ((,var (car ***temp***))) ,body))
		   (setf var `***temp***)))
	     (cond ((eq act :collect)
		    (setf resform `(nreverse ***result***))
		    (setf body `(push ,body ***result***)))
		   ((eq act :do)
		    (setf resform nil)))
	     `(do ((,var ,var-init ,var-step)
		   (***result*** nil))
		  ((not ,tst) ,resform)
		  ,body)))))

(defmacro while (test . body)
    `(loop (if (not ,test) (return nil)) ,@body))

(defmacro push-end (thing place)
  `(setf ,place (nconc ,place (list ,thing))))
