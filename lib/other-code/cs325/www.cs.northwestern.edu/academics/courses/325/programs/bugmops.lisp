(in-package :cs325-user)

;;; An ontology of Lisp functions and forms

;;; Last update:
;;;
;;; 03-05-04 fixed bug in M-MISSING-TEST-BUG pattern 
;;; 03-05-04 added TEST-FIND-CASES/DISPLAY-HELP functions
;;; 03-04-04 added some example Bug and Repair Mops
;;; 02-16-04 added I/O and callable hierarchies


(defmop m-callable)

(defmop m-function (m-callable))
(defmop m-macro (m-callable))
(defmop m-special-function (m-callable))

(defmop m-test-taking-function (m-function)
  :default-test eql)

(defmop m-sequence-function (m-test-taking-function)
  :default-start-value 0)

(defmop m-non-destructive-function (m-function))

(defmop m-destructive-function (m-function))

(defmop m-io-function (m-function))
(defmop m-input-function (m-io-function))
(defmop m-output-function (m-io-function))


;;; some Lisp functions and forms

(definstance delete
  (m-sequence-function m-destructive-function))

(definstance member
  (m-test-taking-function m-non-destructive-function))

(definstance nconc (m-destructive-function))

(definstance print (m-output-function))

(definstance read (m-input-function))

(definstance remove
  (m-sequence-function m-non-destructive-function))

(definstance setq (m-special-function))


;;; Some sample Bug mops, all revolving around
;;; common mistakes with member, remove and delete.
;;;
;;; The :code slot of a bug mop should always be a list
;;; of expressions, like (progn ...) or (let ...) to allow for 
;;; bugs requiring several steps. 
;;;
;;; Bug case abstractions are mops with patterns describing the
;;; what kind of code this bug is relevant to, and 
;;; texts explaining what causes the bug.
;;;
;;; Bug mop instances, with names starting with i-m-
;;; (for instance of mop), give specific examples.
;;;
;;; Repair mops give possible repairs
;;; for a bug. There can be any number of
;;; repairs for the same bug.


(defmop m-bug-mop)
(defmop m-repair-mop)


;;; Bug: REMOVE a list from a list of lists fails.

(defmop m-missing-test-bug (m-bug-mop)
  :code ((?*) ((?is-a m-test-taking-function) (quote ((?))) (?)) (?*))
  :explanation-text "Lisp uses EQL by default to compare objects
but to compare lists you need EQUAL."
  )

(definstance i-m-remove-bug-1 (m-missing-test-bug)
  :bug-text "REMOVE doesn't remove a list from a list"
  :code (progn (remove '(b) '((a) (b) (c))))
  :expected ((a) (c))
  :actual ((a) (b) (c))
  )

(definstance i-m-remove-repair-1 (m-repair-mop)
  :bug i-m-remove-bug-1
  :repair-text "Add :TEST #'EQUAL to function call."
  :old-code (remove '(b) '((a) (b) (c)))
  :new-code (remove '(b) '((a) (b) (c)) :test #'equal)
  )


;;; Bug: REMOVE from a variable doesn't change value
;;; of variable. 

(defmop m-missing-setq-bug (m-bug-mop)
  :code ((?*) ((?is-a m-non-destructive-function) (?*)) (?*))
  :explanation-text "Most Lisp functions do not destructively modify structures."
  )

(definstance i-m-remove-bug-2 (m-missing-setq-bug)
  :bug-text "REMOVE doesn't remove an item from a list in a variable"
  :code (let ((l '(a b c))) (remove 'b l) l)
  :expected (a c)
  :actual (a b c)
  )

(definstance i-m-remove-repair-2 (m-repair-mop)
  :bug i-m-remove-bug-2
  :repair-text "Use SETQ or SETF to save the result in the variable."
  :old-code (remove 'b l)
  :new-code (setq l (remove 'b l))
  )



;;; Bug: MEMBER doesn't find an embedded element

(defmop m-member-nested-bug (m-bug-mop)
  :code ((?*) (member '(? x) '(?not ((?*) (? x) (?*)))) (?*))
  :explanation-text "Member only checks top-level list elements."
  )

(definstance i-m-member-bug-1 (m-member-nested-bug)
  :bug-text "MEMBER doesn't find an item inside a list"
  :code (progn (member 'b '(a (b c) d)))
  :expected (b c)
  :actual nil
  )

(definstance i-m-member-repair-2 (m-repair-mop)
  :bug i-m-member-bug-1
  :repair-text "Use TEST to specify a nest membership test."
  :old-code (member 'b '(a (b c) d))
  :new-code (member 'b '(a (b c) d) :test #'(lambda (x l) (member x l)))
  )



;;; Bug: DELETE used to delete first element with no SETQ

(defmop m-delete-bug (m-bug-mop)
  :code ((?*) (delete '(? x) '((? x) (?*))) (?*))
  :explanation-text "It is impossible for DELETE to destructively remove the first item of a list."
  )


(definstance i-m-delete-bug-1 (m-delete-bug)
  :bug-text "DELETE doesn't remove an item from a list"
  :code (let ((l '(a b c))) (delete 'a l) l)
  :expected (b c)
  :actual (a b c)
  )

(definstance i-m-delete-repair-2 (m-repair-mop)
  :bug i-m-delete-bug-1
  :repair-text "Store the return value in the variable."
  :old-code (delete 'a l)
  :new-code (setq l (delete 'a l))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to testing case retrieval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-find-cases ()
  (mapc #'display-help 
        (find-cases 'm-bug-mop
         `(:code ((member '(a) '((a) (b) (c))))
           :expected ((a) (b) (c))
           :actual nil))))
    

(defun display-help (mop)
  (display-bug-mop mop)
  (let ((repairs (find-repair-mops mop)))
    (if (null repairs)
        (format t "~% -- No repairs found.")
      (mapc #'display-repair-mop repairs))))

(defun display-bug-mop (mop)
  (format t "~2%Possible Match -- ~S: ~A~%  Code: ~S~
~%  Expected: ~S~%  Saw: ~S~%  Explanation: ~A"
          mop (<- mop :bug-text)
          (<- mop :code)
          (<- mop :expected) (<- mop :actual)
          (<- mop :explanation-text)))

(defun display-repair-mop (mop)
  (format t "~2%To repair: ~A~%  Before: ~S~%  After: ~S"
          (<- mop :repair-text) (<- mop :old-code) (<- mop :new-code)))
