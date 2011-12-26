;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; default-hint.lisp
;;;
;;; This book contains the definition of the default hint we
;;; will be using to control nonlinear arithmetic.  We put it
;;; into this seperate file for ease of maintenance.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "ACL2")

(defun nonlinearp-default-hint (stable-under-simplificationp hist pspv)
  (cond (stable-under-simplificationp
         (if (not (access rewrite-constant
                          (access prove-spec-var pspv :rewrite-constant)
                          :nonlinearp))
             '(:computed-hint-replacement t
               :nonlinearp t)
           nil))
        ((access rewrite-constant
                 (access prove-spec-var pspv :rewrite-constant)
                 :nonlinearp)
         (if (not (equal (caar hist) 'SETTLED-DOWN-CLAUSE))
             '(:computed-hint-replacement t
               :nonlinearp nil)
           nil))
        (t
         nil)))
