;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module trutil)

(transl-module trutil)

;;; takes a list, and returns a cons of an a-list of (gensym . exp)
;;; and the origonal list with gensyms substututed for non-atom elements
;;; of the list. This could be used to define subr-like makros.

(defun tr-gensym ()
  (gentemp (symbol-name 'tr-gensym)))

(defun conserve-eval-args-data (l)
  (do ((sublis nil)
       (l l (cdr l))
       (nl nil))
      ((null l) (cons sublis (nreverse nl)))
    (cond ((atom (car l))
	   (push (car l) nl))
	  (t
	   (let ((sym (tr-gensym)))
	     (push (cons sym (car l)) sublis)
	     (push sym nl))))))

(defun tr-trace-handle (form)
  (let* ((level-sym (get (caar form) 'tr-trace-level))
	 (level (1+ (symbol-value level-sym)))
	 (op (caar form)))
    (progv (list level-sym)
	(list level)
      (mtell-open "~%~S Enter ~:@M~%" level op)
      (mgrind form nil)
      (setq form (apply (get op 'otranslate) form))
      (mtell-open "~%~S Exit  ~:@M" level op)
      (pprint form)
      form)))

(defun tr-trace (op)
  (if (get op 'otranslate) (tr-untrace op))
  (let ((sym (gensym)))
    (setf (symbol-value sym) 0)
    (putprop op sym 'tr-trace-level))
  (putprop op (get! op 'translate) 'otranslate)
  (putprop op (get! 'tr-trace-handle 'subr) 'translate))

(defun tr-untrace (op)
  (remprop op 'tr-trace-level)
  (putprop op (get! op 'otranslate) 'translate)
  (remprop op 'otranslate))

(defun push-defvar (var val)
  ;; makes sure there is a form in the beginning of the
  ;; file that insures the special variable is declared and bound.
  (or (member var defined_variables :test #'eq)
      ;; $NO_DEFAULT says that the user takes responsibility for binding.
      (eq $define_variable '$no_default)
      ;; $MODE is same, but double-checks with the declarations available.
      (and (eq $define_variable '$mode)
	   (get var 'mode))
      (do ((l *pre-transl-forms* (cdr l)))
	  ((null l)
	   ;; push one with a priority of 1, which will be over-rided
	   ;; by any user-specified settings.
	   (if (eq $define_variable '$mode)
	       (tr-format "~%Note: ~:M being given a default setting of ~:M"
			  var (if (atom val) val
				  ;; strip off the quote
				  (cadr val))))
	   (push-pre-transl-form `(def-mtrvar ,var ,val 1)))
	(let ((form (car l)))
	  (and (eq (car form) 'def-mtrvar)
	       (eq (cadr form) var)
	       (return ()))))))

(defun push-pre-transl-form (form)
  (cond ((member form *pre-transl-forms* :test #'equal))
	(t
	 (push form *pre-transl-forms*)
	 (and *in-translate*
	      (let ((winp nil))
		(unwind-protect (progn (eval form) (setq winp t))
		  (unless winp
		    (barfo "Bad *pre-transl-form*"))))))))

(defun push-autoload-def (old-entry new-entries)
  (and (get old-entry 'autoload)
       ;; don't need this if it is IN-CORE.
       ;; this automaticaly punts this shit for systems
       ;; that don't need it.
       (do ((entry))
	   ((null new-entries))
	 (setq entry (pop new-entries))
	 (or (member entry *new-autoload-entries* :test #'eq)
	     (push-pre-transl-form
	      `(putprop ',entry
			;; this ensures that the autoload definition
			;; will not get out of date.
		(or (get ',old-entry 'autoload) t)
		'autoload))))))

(defun tr-nargs-check (form &optional (args-p nil) (nargs (length (cdr form))))
  ;; the maclisp args info format is NIL meaning no info,
  ;; probably a lexpr. or cons (min . max)
  (and args-p
       (let ((nargs (length (cdr form)))
	     (min (or (car args-p) (cdr args-p)))
	     (max (cdr args-p)))
	 (cond ((and min (< nargs min))
		(mformat *translation-msgs-files* "~%Error: Too few arguments supplied to ~:@M~%"
			 (caar form))
		(mgrind form *translation-msgs-files*))
	       ((and max (> nargs max))
		(tr-format  "~%Error: Too many arguments supplied to ~:@M~%" (caar form))
		(mgrind form *translation-msgs-files*)))))
  nargs) ;; return the number of arguments.

