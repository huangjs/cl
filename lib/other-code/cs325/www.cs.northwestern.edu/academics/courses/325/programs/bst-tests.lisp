;;; Bug Finder Test Cases for Graham's Binary Search Tree Code
;;;
;;; As pointed out at http://www.paulgraham.com/howbroken.html,
;;; the book code for removing (chap 4) and deleting (chap 12)
;;; nodes did not properly maintain the binary search tree
;;; invariant.
;;;
;;; The broken code is at
;;;
;;;   http://www.cs.northwestern.edu/academics/courses/325/programs/bst.lisp
;;;
;;; A local copy of the repaired BST code is at
;;;
;;;   http://www.cs.northwestern.edu/academics/courses/325/programs/bst2.lisp
;;;
;;; This is extracted from Graham's repaired code at
;;;
;;;   http://www.paulgraham.com/lib/paulgraham/acl2.lisp
;;;
;;; Graham's fixes are more complicated than necessary. If you want
;;; to try cleaning up his code, use these tests to check your code.


(in-package :cs325-user)

;;; (RUN-BST-TESTS) runs just the BST tests
;;; It's a macro to delay expanding RUN-TESTS until the tests
;;; are defined.

(defmacro run-bst-tests ()
  '(run-tests bst-insert bst-remove bst-insert! bst-delete)
  )


;;; To make it easy to write BST tests, we define a function
;;; that constructs a BST from a sequence of insertions
;;; and removals, and then returns a list of the elements. 
;;; 
;;; (BST-RESULTS commands [<]) => list
;;;   Returns the elements in the BST that results from
;;;   the given list of commands. < is the ordering predicate
;;;   to use in the tree (default #'<). A command is one of:
;;;
;;;     + meaning "bst-insert items that follow"
;;;     - meaning "bst-remove items that follow"
;;;     ++ meaning "bst-insert! items that follow"
;;;     -- meaning "bst-delete items that follow"
;;;     a number, to insert or remove as directed
;;;
;;;   Initially, bst-insert is assumed.
;;;
;;;   For example,
;;;
;;;     (BST-RESULTS '(1 2 3 - 2 1 + 4))
;;;
;;;   creates an empty tree, inserts 1, 2 and 3, removes 2 and
;;;   1, inserts 4, and returns the elements in the resulting
;;;   tree, which should be (3 4) in this case.

(defun bst-results (cmds &optional (< #'<))
  (bst-list (construct-bst cmds <)))

(defun construct-bst (cmds <)
  (let ((bst nil) (fn 'bst-insert))
    (dolist (cmd cmds)
      (case cmd
        ((+ - ++ --)
         (setq fn (ecase cmd
                    (+ 'bst-insert)
                    (- 'bst-remove)
                    (++ 'bst-insert!)
                    (-- 'bst-delete))))
        (otherwise
         (setq bst (funcall fn cmd bst <)))))
    bst))

;;; The BST-ELEMENTS exercise (Ex 4.4) asks you to define
;;; a much more efficient simple way to do this.

(defun bst-list (bst)
  (and (not (null bst))
       (append (bst-list (node-l bst))
               (list (node-elt bst))
               (bst-list (node-r bst)))))

;;; The tests

(define-test bst-insert
  (assert-equal '() (bst-results '()))
  (assert-equal '(5) (bst-results '(5)))
  (assert-equal '(1 4 6) (bst-results '(1 4 6)))
  (assert-equal '(1 4 6) (bst-results '(6 4 1)))
  (assert-equal '(1 4 6) (bst-results '(6 4 1 4)))
  (assert-equal '(1 2 3 4 5 6 7 8 9) (bst-results '(1 2 3 4 5 6 7 8 9)))
  (assert-equal '(1 2 3 4 5 6 7 8 9) (bst-results '(9 8 7 6 5 4 3 2 1)))
  (assert-equal '(1 2 3 4 5 6 7 8 9) (bst-results '(5 1 9 6 2 8 4 7 3)))
  )

(define-test bst-remove
  (assert-equal '() (bst-results '(-)))
  (assert-equal nil (bst-results '(1 - 1)))
  (assert-equal '(2) (bst-results '(1 2 - 1)))
  (assert-equal '(1) (bst-results '(1 2 - 2)))
  (assert-equal '(1 3) (bst-results '(3 2 - 2 + 1)))
  (assert-equal '(1 2 3 4 5 6 7 8) (bst-results '(1 2 3 4 5 6 7 8 9 - 9)))
  (assert-equal '(1 2 3 4 5 6 7 8) (bst-results '(9 8 7 6 5 4 3 2 1 - 9)))
  (assert-equal '(1 2 3 4 5 6 7 8) (bst-results '(5 1 9 6 2 8 4 7 3 - 9)))
  (assert-equal '(1 2 3 4 6 7 8 9) (bst-results '(5 8 4 2 1 9 6 7 3 - 5)))
  )
  
(define-test bst-insert!
  (assert-equal '() (bst-results '(++)))
  (assert-equal '(5) (bst-results '(++ 5)))
  (assert-equal '(1 4 6) (bst-results '(++ 1 4 6)))
  (assert-equal '(1 4 6) (bst-results '(++ 6 4 1)))
  (assert-equal '(1 4 6) (bst-results '(++ 6 4 1 4)))
  (assert-equal '(1 2 3 4 5 6 7 8 9) (bst-results '(++ 1 2 3 4 5 6 7 8 9)))
  (assert-equal '(1 2 3 4 5 6 7 8 9) (bst-results '(++ 9 8 7 6 5 4 3 2 1)))
  (assert-equal '(1 2 3 4 5 6 7 8 9) (bst-results '(++ 5 1 9 6 2 8 4 7 3)))
  )

(define-test bst-delete
  (assert-equal '() (bst-results '(--)))
  (assert-equal '(5) (bst-results '(++ 5)))
  (assert-equal '(1 4 6) (bst-results '(++ 1 4 6)))
  (assert-equal '(1 4 6) (bst-results '(++ 6 4 1)))
  (assert-equal '(1 4 6) (bst-results '(++ 6 4 1 4)))
  (assert-equal nil (bst-results '(++ 1 -- 1)))
  (assert-equal '(2) (bst-results '(++ 1 2 -- 1)))
  (assert-equal '(1) (bst-results '(++ 1 2 -- 2)))
  (assert-equal '(1 3) (bst-results '(++ 3 2 -- 2 ++ 1)))
  (assert-equal '(1 2 3 4 5 6 7 8) (bst-results '(++ 1 2 3 4 5 6 7 8 9 -- 9)))
  (assert-equal '(1 2 3 4 5 6 7 8) (bst-results '(++ 9 8 7 6 5 4 3 2 1 -- 9)))
  (assert-equal '(1 2 3 4 5 6 7 8) (bst-results '(++ 5 1 9 6 2 8 4 7 3 -- 9)))
  (assert-equal '(1 2 3 4 6 7 8 9) (bst-results '(5 8 4 2 1 9 6 7 3 -- 5)))
  )

