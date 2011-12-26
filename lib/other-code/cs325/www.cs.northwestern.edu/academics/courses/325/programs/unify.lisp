(eval-when (load compile eval)
  (unless (find-package :unify)
    (make-package :unify
		  :use (list (or (find-package :common-lisp)
				 (find-package :lisp))))))

(in-package :unify)

(export '(unify var-p var-name var-bound-p var-value make-empty-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unify (pat1 pat2 &optional (blists (make-empty-bindings)))
  (cond ((null blists) nil)
        ((var-p pat1) (var-unify pat1 pat2 blists))
        ((var-p pat2) (var-unify pat2 pat1 blists))
        ((atom pat1) (when (eql pat1 pat2) blists))
        ((atom pat2) nil)
        (t (unify (cdr pat1) (cdr pat2)
                  (unify (car pat1) (car pat2) blists)))))

(defun var-unify (var pat blists)
  (mapcan #'(lambda (blist) (extend-blist var pat blist))
          blists))

(defun extend-blist (var pat blist)
  (if (and (var-p pat) (var-equalp var pat blist))
      (list blist)
      (let ((bdg (var-binding var blist)))
        (cond (bdg (unify (binding-value bdg) pat (list blist)))
              ((contained-in-p var pat blist) nil)
              (t (list (add-var-binding var pat blist)))))))
              
(defun var-equalp (var1 var2 blist)
  (and (var-p var2)
       (or (eql (var-name var1) (var-name var2))
           (var-equalp var1 (var-value var2 blist) blist))))

(defun contained-in-p (var pat blist)
  (if (var-p pat)
      (or (eql (var-name var) (var-name pat))
          (contained-in-p var (var-value pat blist) blist))
      (and (consp pat)
           (or (contained-in-p var (car pat) blist)
               (contained-in-p var (cdr pat) blist)))))

(defun add-var-binding (var val blist)
  (cons (list (var-name var) val) blist))

(defun var-p (x) (and x (symbolp x) (eq (char (symbol-name x) 0) #\?)))
(defun var-name (x) x)

(defun var-value (var blist) (binding-value (var-binding var blist)))
(defun var-binding (var blist) (assoc (var-name var) blist))
(defun binding-value (bdg) (cadr bdg))

(defun var-bound-p (var blist) (not (null (var-binding var blist))))

(defun make-empty-bindings () (list nil))

(provide "unify")

