;;; Copyright (c) 1987 John Peterson
;;;   Permission is given to freely modify and distribute this code
;;;   so long as this copyright notice is retained.

(defun put-att (table key att val)
    (let* ((cur-alist (gethash key table))
	   (p (assoc att cur-alist)))
      (if p (setf (cdr p) val)
	    (setf (gethash key table)
		  (cons (cons att val) cur-alist)))
      val))

(defun att (table key att)
    (cdr (assoc att (gethash key table))))

(defun push-att (table key att val)
  (put-att table key att (cons val (att table key att))))

(defun concat (x y) (concatenate 'string x y))

(defun new-name (name)
  (let ((def (lookup-name name)))
    (when (not (eq (car def) 'undefined))
	  (ps-error "Attempt to redefine symbol" name))))

(defun emit (x)
    (push x code-stream)
    1)  ; just in case anyone cares!

(defun numeric-constant ()
    (numberp (car code-stream)))

;;; more support for q&d constant folding

(defun remove-number ()
    (pop code-stream))

