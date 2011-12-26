; This is just a little variation macros.lisp that uses skip-proofs.  We check
; the expansions in macros-skip-proofs-include.lisp, much as we check
; expansions from macros.lisp in macros-include.lisp.

(in-package "ACL2")

(defmacro my-mac (x) x)

(encapsulate
 ((local-fn (x) t))
 (local (defun local-fn (x) x))
 (local
  (make-event '(defun foo1 (x) x)))
 (my-mac
  (skip-proofs
   (make-event '(defun foo2 (x) x)
               :check-expansion t)))
 (skip-proofs
  (my-mac
   (make-event '(defun foo3 (x) x)
               :check-expansion t)))
 (make-event '(defun foo4 (x) x)))

; Identical to form above, hence should be redundant.
(encapsulate
 ((local-fn (x) t))
 (local (defun local-fn (x) x))
 (local
  (make-event '(defun foo1 (x) x)))
 (my-mac
  (skip-proofs
   (make-event '(defun foo2 (x) x)
               :check-expansion t)))
 (skip-proofs
  (my-mac
   (make-event '(defun foo3 (x) x)
               :check-expansion t)))
 (make-event '(defun foo4 (x) x)))

