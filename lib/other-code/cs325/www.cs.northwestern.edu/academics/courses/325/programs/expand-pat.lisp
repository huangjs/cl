;;; A simple pattern compiler for the C25 pattern matcher
;;;
;;; (compile-pat pattern) => #'(lambda ...)
;;;
;;; Ex. (funcall (eval (compile-pat pat)) input)
;;; returns the same result as (pat-match pat input)
;;;
;;; Needs the pattern matcher for its variable binding
;;; functions.

(require "pat-match")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The pattern compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All subpatterns expand into code using INPUT and BINDINGS. As
;;; appropriate, the code rebinds these variables using LET.

(defun compile-pat (pat)
  `#'(lambda (input &optional (blists '(nil)))
       ,(expand-pat pat)))

(defun expand-pat (pat)
  (cond ((var-p pat) (expand-var pat))
	((atom pat) (expand-atomic-pat pat))
	(t (expand-cons-pat pat))))

(defun expand-atomic-pat (pat)
  `(and (eql input ',pat)
        blists))

(defun expand-var (pat)
  `(match-variable ',pat input blists))

(defun expand-cons-pat (pat)
  `(and (not (null blists))
        (consp input)
        (let ((blists 
	       (let ((input (car input)))
	         ,(expand-pat (car pat))))
	      (input (cdr input)))
          ,(expand-pat (cdr pat)))))

