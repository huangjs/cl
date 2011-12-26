
;;; An extensible pattern compiler for the C25 pattern matcher
;;;
;;; An extended version of expand-pat.lisp
;;;
;;; (compile-pat pattern) => #'(lambda ...)
;;;
;;; Ex. (funcall (eval (compile-pat pat)) input)
;;; returns the same result as (pat-match pat input)
;;;
;;; Needs the pattern matcher for its variable binding
;;; functions and for the matcher itself when an extension is 
;;; seen w/o an expander.


(require "extend-match")
(in-package :extend-match)

(require "tables")
(use-package :tables)

(export '(expand-pat compile-pat))

(deftable pat-expander)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The pattern compiler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All subpatterns expand into code using INPUT and BINDINGS. As
;;; appropriate, the code rebinds these variables using LET.

(defun compile-pat (pat)
  `#'(lambda (input &optional (blists '(nil)))
       ,(expand-pat pat)))

(defun expand-pat (pat)
  (cond ((pat-extension-p pat)
         (expand-extension pat))
        ((atom pat) (expand-atomic-pat pat))
        ((segment-pat-extension-p (first pat))
         (expand-segment-extension pat)) 
        (t (expand-cons-pat pat))))

(defun expand-atomic-pat (pat)
  `(and (eql input ',pat)
        blists))

(defun expand-cons-pat (pat)
  `(and (not (null blists))
        (consp input)
        (let ((blists 
	       (let ((input (car input)))
	         ,(expand-pat (car pat))))
	      (input (cdr input)))
          ,(expand-pat (cdr pat)))))

(defun expand-extension (pat)
  (funcall (or (pat-expander (first pat))
               'default-pat-expander)
           pat))

(defun expand-segment-extension (pat)
  (funcall (or (pat-expander (first (first pat)))
               'default-pat-expander)
           pat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expanders for pattern extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The default expander just calls the extensible matcher. It's
;;; used whenever no expander is found. It's not as efficient
;;; as expanded code.

(defun default-pat-expander (pat)
  `(pat-match ',pat input blists))



;;; Usage: (? [name]).  (?) is for anonymous matching

(setf (pat-expander '?) 'expand-match-variable)  

(defun expand-match-variable (pat)
  (destructuring-bind (&optional name) (rest pat)
    (if (null name) 
      'blists
      `(bind-variable ',name input blists))))


;;; Usage: (?is function)

(setf (pat-expander '?is) 'expand-is-match)

(defun expand-is-match (pat)
  (destructuring-bind (fn) (rest pat)
    `(and (not (null blists))
          (,fn input)
          blists)))


;;; Usage: (?and pat1 pat2 ...)

(setf (pat-expander '?and) 'expand-and-match)

(defun expand-and-match (pat)
  (labels ((expand (args)
             (if (null args) 
               'blists
               `(and (not (null blists))
                     (let ((blists ,(expand-pat (first args))))
                       ,(expand (rest args)))))))
    (expand (rest pat))))



;;; Usage: (?* [name]).  (?*) is for anonymous matching

(setf (pat-expander '?*) 'expand-match-segment-variable)

(defun expand-match-segment-variable (pat)
  (destructuring-bind (&optional name) 
                      (rest (first pat))
    `(match-segments ',name 
                     ,(compile-pat (rest pat))
                     input blists)))



(defun match-segments (name fn input blists)
  (do* ((tail input (rest tail))
        (new-blists (match-tail-segment 
                     tail name fn input blists)
                    (append (match-tail-segment 
                             tail name fn input blists)
                            new-blists)))
       ((null tail) new-blists)))

(defun match-tail-segment (tail name fn input blists) 
  (let ((blists (funcall fn tail blists)))
    (if (null blists)
      nil
      (bind-variable name (ldiff input tail) 
                     blists))))


(provide "expand-ext-match")
