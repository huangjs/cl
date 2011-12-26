;;; Bug Finder Test Cases for Graham Exercises
;;;
;;; Test cases for Graham, Lisp, and project exercises.
;;; Most but not all assigned exercises have test cases.

;;; Update history:
;;;
;;; 03-15-06 replaced FRUGAL with MEMOIZE [CKR]
;;; 02-24-06 added Forrest Sondahl's tests for CAR-CIRCULAR-P [CKR]
;;; 02-20-06 fixed duplicate :ELSE in KEY-IF test [CKR]
;;; 02-14-06 changed the last MAP-STREAM test output to be more readable [CKR]
;;; 02-11-06 removed duplicate sublist labels in CIRCULAR-MEMBER-P [CKR]
;;; 02-10-06 added a new test for KEY-IF [CKR]
;;; 02-10-06 added William Siegrist's CIRCULAR-MEMBER-P tests [CKR]
;;; 02-10-06 modified SOLVE tests [CKR]
;;; 02-09-06 added John Otto's tests for HAS-NUMBER-P [CKR]
;;; 01-25-06 added William Siegrist's <prevent> test to EXTRACT-CODE-FROM-STRING [CKR]
;;; 12-30-05 updated tests to use ASSERT-EQUALITY etc when appropriate [CKR]
;;; 03-04-05 fixed newlines in strings in Ex 7.3 tests [CKR]
;;; 03-04-05 addes tests for ch 7 [CKR]
;;; 03-04-05 fixed bad tests for Ex 9.4 [CKR]
;;; 02-17-05 added MAP-STREAM tests [CKR]
;;; 02-17-05 added STREAM-SUBST tests [CKR]
;;; 02-09-05 added test to NTH-EXPR [CKR]
;;; 01-31-05 added test to STABLE-SET-DIFFERENCE [CKR]
;;; 01-31-05 fixed backwards BST-ELEMENTS tests [CKR]
;;; 01-27-05 added non-default coin list tests to MAKE-CHANGE [CKR]
;;; 01-18-05 updated for new lisp-unit [CKR]
;;; 12-01-04 added BST-ELEMENTS [CKR]
;;; 11-30-04 edited to be compatible with the new lisp-unit [CKR]
;;; 03-17-04 added test to CDR-CIRCULAR-P [CKR]
;;; 03-17-04 added MAKE-QUEUE and EMPTY-QUEUE-P tests [CKR]
;;; 03-16-04 added queue tests (ch 12) [CKR]
;;; 03-16-04 added ABSTS-ABSTP test [CKR]
;;; 03-15-04 changed FRUGAL to give clearer failure message [CKR]
;;; 03-15-04 added a test to ?IS-A [CKR]
;;; 03-12-04 added atom tests to SHOW-LIST [CKR]
;;; 03-08-04 added some more DELETE-CAR test [CKR]
;;; 03-04-04 added FRUGAL test [CKR]
;;; 03-03-04 removed some tests from ABSTS-ABSTP [CKR]
;;; 03-03-04 added more tests to PRESERVE [CKR]
;;; 03-02-04 added Wagner's tests for ABSTS-ABSTP [CKR]
;;; 03-02-04 added PRESERVE test [CKR]
;;; 02-27-04 added TEST-TAKING-MATCH [CKR]
;;; 02-27-04 added tests for Ex 8.4 [CKR]
;;; 02-27-04 added NTH-EXPR and N-OF tests [CKR]
;;; 02-24-04 added BIN-SEARCH tests [CKR]
;;; 02-16-04 added ABSTS-ABSTP test [CKR]
;;; 02-13-04 added empty vector test to PRECEDERS [CKR]
;;; 02-11-04 added COMMON-ABSTS test [CKR]
;;; 02-10-04 added non-string example for PRECEDERS [CKR]
;;; 02-09-04 added FUNCTIONS-REFERENCED test [CKR]
;;; 02-03-04 added 3TREE test [CKR]
;;; 02-02-04 added (((A B) C) D) test to SHOW-LIST [CKR]
;;; 01-31-04 added *PRINT-CIRCLE* to circular list tests [CKR]
;;; 01-31-04 added EQ test to MY-COPY-LIST [CKR]
;;; 01-29-04 added Earl Wagner's tests for LIST-OF [CKR]
;;; 01-29-04 added new STABLE-INTERSECTION case [CKR]


(in-package :cs325-user)

;;; Chapter 2

;;; Ex 2.4

(define-test greater
  (assert-equal 2 (greater 1 2))
  (assert-equal 2 (greater 2 1))
  (assert-equal -1 (greater -5 -1))
  (assert-equal 0 (greater -2 0))
  (assert-equal 3 (greater 3 3))
  )

;;; Ex 2.7

(define-test has-list-p
  (assert-false (has-list-p '(a b c)))
  (assert-true (has-list-p '((a) b)))
  (assert-true (has-list-p '(a b (c))))
  (assert-false (has-list-p 'nil))
  (assert-true (has-list-p '(a nil c)))
  (assert-true (has-list-p '(nil a)))
  (assert-true (has-list-p '(nil)))
  )

;;; Ex 2.8-a

(define-test print-dots
  (assert-prints "...." (print-dots 4))
  (assert-prints "." (print-dots 1))
  (assert-prints "" (print-dots 0))
  )

;;; Ex 2.8-b

(define-test get-a-count
  (assert-equal 3 (get-a-count '(a a a)))
  (assert-equal 0 (get-a-count '(b c d)))
  (assert-equal 0 (get-a-count 'nil))
  (assert-equal 0 (get-a-count '(b (a) d)))
  (assert-equal 2 (get-a-count '(a b a)))
  (assert-equal 2 (get-a-count '(nil a a)))
  )

;;; Ex 2.9

(define-test summit
  (assert-equal 6 (summit '(1 2 3)))
  (assert-equal 4 (summit '(1 nil 3)))
  (assert-equal 0 (summit '(nil nil nil)))
  (assert-equal 0 (summit nil))
  )

;;; Chapter 3

;;; Ex 3.2

(define-test stable-union
  (assert-equal '(a b c d) (stable-union '(a b c) '(b a d)))
  (assert-equal '(a b c d e f) (stable-union '(a b c) '(d e f)))
  (assert-equal '(a b c) (stable-union '(a b c) 'nil))
  (assert-equal '(a b c d e) (stable-union '(a b c) '(a d b e)))
  (assert-equal '(c b a e d) (stable-union '(c b a) '(a e b d)))
  (assert-equal '(a b c) (stable-union 'nil '(a b c)))
  (assert-equal 'nil (stable-union 'nil 'nil))
  )


(define-test stable-intersection
  (assert-equal '(a b) (stable-intersection '(a b c) '(b a d)))
  (assert-equal '(a b) (stable-intersection '(a b c) '(a b d)))
  (assert-equal 'nil (stable-intersection '(a b c) '(d e f)))
  (assert-equal 'nil (stable-intersection '(a b c) 'nil))
  (assert-equal 'nil (stable-intersection 'nil '(a b c)))
  (assert-equal 'nil (stable-intersection 'nil 'nil))
  )

(define-test stable-set-difference
  (assert-equal '(c) (stable-set-difference '(a b c) '(b a d)))
  (assert-equal '(a b c) (stable-set-difference '(a b c) '(d e f)))
  (assert-equal '(a b c) (stable-set-difference '(a b c) 'nil))
  (assert-equal '(c) (stable-set-difference '(a b c) '(a d b e)))
  (assert-equal '(d e) (stable-set-difference '(a d b e) '(a b c)))
  (assert-equal 'nil (stable-set-difference 'nil '(a b c)))
  (assert-equal 'nil (stable-set-difference 'nil 'nil))
  )

;;; Ex 3.3

(define-test occurrences
  (assert-equal '((a . 4) (c . 3) (d . 2) (b . 1)) (occurrences '(a b a d a c d c a c)))
  (assert-equal '((nil . 3) (b . 2) (a . 1)) (occurrences '(nil a b nil b nil)))
  (assert-equal 'nil (occurrences 'nil))
  )

;;; Ex 3.5

(define-test position+
  (assert-equal '(7 6 3 7) (position+ '(7 5 1 4)))
  (assert-equal '(7 6 9 8) (position+ '(7 5 7 5)))
  (assert-equal '(7) (position+ '(7)))
  (assert-equal 'nil (position+ 'nil))
  )

;;; Ex 3.8

(define-test show-dots
  (assert-prints "(A . (B . (C . NIL)))" (show-dots '(a b c)))
  (assert-prints "(A . ((B . (C . NIL)) . NIL))" (show-dots '(a (b c))))
  (assert-prints "(A . B)" (show-dots '(a . b)))
  (assert-prints "NIL" (show-dots nil))
  (assert-prints "(NIL . NIL)" (show-dots '(nil)))
  )

(define-test show-list
  (assert-prints "[A B C]" (show-list '(a b c)))
  (assert-prints "[[[A B] C] D]" (show-list '(((a b) c) d)))
  (assert-prints "[A [B C]]" (show-list '(a (b c))))
  (assert-prints "[A . B]" (show-list '(a . b)))
  (assert-prints "[A B C . D]" (show-list '(a b c . d)))
  (assert-prints "A" (show-list 'a))
  (assert-prints "12" (show-list 12))
  (assert-prints "NIL" (show-list nil))
  (assert-prints "[NIL]" (show-list '(nil)))
  )

;;; Ex 3.9

(define-test longest-path
  (assert-equal '(a b c) (longest-path 'a 'c '((a b) (b c))))
  (assert-equal '(a b c) (longest-path 'a 'c '((a b) (b a c))))
  (assert-equal '(a b a) (longest-path 'a 'a '((a b) (b a c))))
  (assert-equal nil (longest-path 'a 'c '((a b) (b a) (c))))
  (assert-equal '(a c d e f) (longest-path 'a 'f '((a b c) (b f) (c d) (d e) (e f))))
  (assert-equal '(a b c e d f) (longest-path 'a 'f '((a b c a) (b c d) (c e a) (d e f) (e d f))))
  (assert-equal '(a) (longest-path 'a 'a '((a b) (b c))))
  (assert-equal '(a a) (longest-path 'a 'a '((a a b) (b c))))
  (assert-equal '(a b) (longest-path 'a 'b '((a b) (b c) (c b))))
  (assert-equal '(a c b) (longest-path 'a 'b '((a b c) (b c) (c b))))
  )

;;; Chapter 4

;;; A utility for copying arrays for the in-place rotate in graham-4-1

(defun copy-array (a)
  (let* ((dims (array-dimensions a))
         (a-copy (make-array dims :element-type (array-element-type a))))
    (dotimes (i (array-total-size a))
      (setf (row-major-aref a-copy i)
	    (row-major-aref a i)))
    a-copy))
  
;;; Ex 4.1

(define-test rotate-array
  (assert-equalp #2a((c a) (d b)) (rotate-array #2a((a b) (c d))))
  (assert-equalp #2a((g d a) (h e b) (i f c)) (rotate-array #2a((a b c) (d e f) (g h i))))
  (assert-equalp #2a((a)) (rotate-array #2a((a))))
  (assert-equalp #2a((13 9 5 1) (14 10 6 2) (15 11 7 3) (16 12 8 4))
                 (rotate-array #2a((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))))
  (assert-equalp #2a((21 16 11 6 1) (22 17 12 7 2) (23 18 13 8 3) (24 19 14 9 4) (25 20 15 10 5))
                 (rotate-array #2a((1 2 3 4 5)
                                   (6 7 8 9 10)
                                   (11 12 13 14 15)
                                   (16 17 18 19 20)
                                   (21 22 23 24 25))))
  (let ((x #2a((a b c) (d e f) (g h i))))
    (assert-false (eq (rotate-array x) x) x))
  )

(define-test nrotate-array
  (assert-equalp #2a((c a) (d b)) (nrotate-array (copy-array #2a((a b) (c d)))))
  (assert-equalp #2a((g d a) (h e b) (i f c))
                 (nrotate-array (copy-array #2a((a b c) (d e f) (g h i)))))
  (assert-equalp #2a((a)) (nrotate-array (copy-array #2a((a)))))
  (assert-equalp #2a((13 9 5 1) (14 10 6 2) (15 11 7 3) (16 12 8 4))
                 (nrotate-array (copy-array #2a((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))))
  (assert-equalp #2a((21 16 11 6 1) (22 17 12 7 2) (23 18 13 8 3) (24 19 14 9 4) (25 20 15 10 5))
                 (nrotate-array (copy-array #2a((1 2 3 4 5)
                                                (6 7 8 9 10)
                                                (11 12 13 14 15)
                                                (16 17 18 19 20)
                                                (21 22 23 24 25)))))
  (let ((x #2a((a b c) (d e f) (g h i))))
    (assert-true (eq (nrotate-array x) x) x))
  )

;;; Ex 4.2-a

(define-test my-copy-list
  (assert-equal '(a b c) (my-copy-list '(a b c)))
  (assert-equal '(a b a c b) (my-copy-list '(a b a c b)))
  (assert-equal '(a) (my-copy-list '(a)))
  (assert-equal nil (my-copy-list nil))
  (let ((l '(a b c)))
    (assert-false (eq l (my-copy-list l)) l))
  )

;;; Ex 4.2-b

(define-test my-reverse
  (assert-equal '(c b a) (my-reverse '(a b c)))
  (assert-equal '(b a) (my-reverse '(a b)))
  (assert-equal '(a) (my-reverse '(a)))
  (assert-equal nil (my-reverse nil))
  (assert-equal '(d (b c) a) (my-reverse '(a (b c) d)))
  )


;;; Ex 4.3

;;; Utility for building 3tree from nested list

(defun list->3tree (l)
  (if (null l) nil
      (make-3tree :data (first l)
		  :left (list->3tree (second l))
		  :middle (list->3tree (third l))
		  :right (list->3tree (fourth l)))))


(define-test 3tree-clone
  (let* ((tree (list->3tree '(top (a (b nil (c nil nil) (d nil nil)) nil nil) nil (e nil nil (f nil (g nil nil) nil)))))
         (clone (3tree-clone tree)))
    (assert-equalp tree clone)
    (assert-false (eq tree clone))
    (assert-false (eq (3tree-left tree) (3tree-left clone)))
    (assert-false (eq (3tree-right tree) (3tree-right clone)))
    (assert-false (eq (3tree-left (3tree-left tree)) (3tree-left (3tree-left clone))))
    (assert-false (eq (3tree-right (3tree-right tree)) (3tree-right (3tree-right clone))))
    ))

(define-test 3tree-member
  (let ((tree (list->3tree '(top (a (b nil (c nil nil) (d nil nil))) nil (e nil nil (nil nil (g nil nil) nil))))))
    (assert-true (3tree-member 'top tree))
    (assert-true (3tree-member 'a tree))
    (assert-true (3tree-member 'b tree))
    (assert-true (3tree-member 'd tree))
    (assert-true (3tree-member 'e tree))
    (assert-false (3tree-member 'f tree))
    (assert-true (3tree-member 'nil tree))
    (assert-false (3tree-member 'nil (3tree-left tree)))
    ))


;;; Ex 4.4
;;; requires bst.lisp (Graham's code)

(defun make-bst (l)
  (do ((ll l (cdr ll))
       (bst nil (bst-insert (car ll) bst #'<)))
      ((null ll) bst)))

(define-test bst-elements
  (assert-equal nil (bst-elements (make-bst nil)))
  (assert-equal '(4) (bst-elements (make-bst '(4))))
  (assert-equal '(0 -12) (bst-elements (make-bst '(-12 0))))
  (assert-equal '(10 9 5 3 1) (bst-elements (make-bst '(3 1 5 9 10))))
  )


;;; Ex 4.6

(define-test alist->hash-table
  (let ((ht (alist->hash-table '((a . 1) (b . 2) (c . 3)))))
    (assert-equal 1 (gethash 'a ht))
    (assert-equal 2 (gethash 'b ht))
    (assert-equal 3 (gethash 'c ht))
    (assert-equal 3 (hash-table-count ht)))
  (assert-equal 0 (hash-table-count (alist->hash-table nil)))
  )

(define-test hash-table->alist
  (let ((ht (make-hash-table)))
    (setf (gethash 'a ht) 1 (gethash 'b ht) 2 (gethash 'c ht) 3)
    (assert-equality 'set-equal '((a . 1) (b . 2) (c . 3)) (hash-table->alist ht)))
  (assert-true (null (hash-table->alist (make-hash-table))))
  )


;;; Chapter 5

;;; Ex 5.5

(define-test preceders
  (assert-equality 'set-equal '(#\c #\d #\r) (preceders #\a "abracadabra"))
  (assert-equality 'set-equal '(#\c #\d #\r #\a) (preceders #\a "aabracadabra"))
  (assert-equality 'set-equal 'nil (preceders #\x "abracadabra"))
  (assert-equality 'set-equal 'nil (preceders #\a ""))
  (assert-equality 'set-equal '(#\x #\c #\d #\r) (preceders #\a "abxacadabra"))
  (assert-equality 'set-equal '(a d) (preceders 'b #(a b c d b a b)))
  (assert-equality 'set-equal nil (preceders 'b #()))
  )

;;; Ex 5.6

(define-test intersperse
  (assert-equal '(a - b - c - d) (intersperse '- '(a b c d)))
  (assert-equal '(a - b) (intersperse '- '(a b)))
  (assert-equal '(a) (intersperse '- '(a)))
  (assert-equal 'nil (intersperse '- 'nil))
  (assert-equal '(nil - b - c) (intersperse '- '(nil b c)))
  (assert-equal '(a - nil - c) (intersperse '- '(a nil c)))
  (assert-equal '(nil) (intersperse '- '(nil)))
  (assert-equal '(a - a - b) (intersperse '- '(a a b)))
  )


;;; Ex 5.7

(define-test diff-by-one-p
  (assert-true (diff-by-one-p '(1 2 3 2 1)))
  (assert-true (diff-by-one-p '(1 2)))
  (assert-equal nil (diff-by-one-p '(1 2 3 -4)))
  (assert-equal nil (diff-by-one-p '(1 3 5 6)))
  (assert-true (diff-by-one-p '(1)))
  (assert-true (diff-by-one-p 'nil))
  (assert-error 'error (diff-by-one-p '(1 nil)))
  )

;;; Ex 5.8

(define-test max-min
  (assert-equal (values 4 1) (max-min #(1 2 3 4)))
  (assert-equal (values 10 1) (max-min #(3 1 8 2 10)))
  (assert-equal (values 10 10) (max-min #(10)))
  (assert-equal (values -5 -8) (max-min #(-5 -8)))
  (assert-equal (values nil nil) (max-min #()))
  )

;;; Chapter 6

;;; Ex 6.2

(define-test bin-search
  (assert-equal 1 (bin-search 1 #(1 3 5 7 8 10)))
  (assert-equal 3 (bin-search 3 #(1 3 5 7 8 10)))
  (assert-equal 5 (bin-search 5 #(1 3 5 7 8 10)))
  (assert-equal 7 (bin-search 7 #(1 3 5 7 8 10)))
  (assert-equal 8 (bin-search 8 #(1 3 5 7 8 10)))
  (assert-equal 10 (bin-search 10 #(1 3 5 7 8 10)))
  (assert-equal nil (bin-search 0 #(1 3 5 7 8 10)))
  (assert-equal nil (bin-search 12 #(1 3 5 7 8 10)))
  (assert-equal nil (bin-search 3 #(1 3 5 7 8 10) :start 2))
  (assert-equal 3 (bin-search 3 #(1 3 5 7 8 10) :start 1))
  (assert-equal nil (bin-search 7 #(1 3 5 7 8 10) :end 3))
  (assert-equal 7 (bin-search 7 #(1 3 5 7 8 10) :end 4))
  (assert-equal 7 (bin-search 7 #(1 3 5 7 8 10) :start 2 :end 4))
  (assert-equal nil (bin-search 7 #(1 3 5 7 8 10) :start 2 :end 2))
  (assert-equal nil (bin-search 1 #(1 3 5 7 8 10) :start 3 :end 2))
  (assert-equal '(8 e) (bin-search 8 #((1 a) (3 b) (5 c) (7 d) (8 e) (10 f)) :key #'car))
  (assert-equal '(8 e)
                (bin-search 8 #((1 a) (3 b) (5 c) (7 d) (8 e) (10 f)) :key #'car :start 3 :end 5))
  (assert-equal nil
                (bin-search 8 #((1 a) (3 b) (5 c) (7 d) (8 e) (10 f)) :key #'car :start 3 :end 4))
  (assert-equal nil
                (bin-search 8 #((1 a) (3 b) (5 c) (7 d) (8 e) (10 f)) :key #'car :start 5 :end 6))
  (assert-equal 1 (bin-search 1 #(1)))
  (assert-equal nil (bin-search 1 #(1) :start 1))
  (assert-equal 1 (bin-search 1 #(1) :end 1))
  (assert-equal nil (bin-search 1 #(1) :end 0))
  (assert-equal nil (bin-search 1 #()))
  )

;;; Ex 6.4

(define-test two-most
  (assert-equal (values 1 2) (two-most '/ '(3 1 8 2 10)))
  (assert-equal (values 1 3) (two-most '/ '(3 1 8)))
  (assert-equal (values 2 5) (two-most '/ '(10 2 5 8)))
  (assert-equal (values 8 nil) (two-most '/ '(8)))
  (assert-equal (values 2 2) (two-most '/ '(2 8 2)))
  (assert-equal (values nil nil) (two-most '/ nil))
  (assert-equal (values 8 0) (two-most '1+ '(-1 0 -2 8 -5 -1 0)))
  (assert-equal (values 3 2) (two-most '1+ '(2 3)))
  (assert-equal (values 3 2) (two-most '1+ '(3 2)))
  (assert-equal (values 4 3) (two-most '1+ '(2 3 4)))
  (assert-equal (values 4 3) (two-most '1+ '(2 4 3)))
  (assert-equal (values 4 3) (two-most '1+ '(4 2 3)))
  (assert-equal (values -2 -4) (two-most '1+ '(-2 -4 -5 -8)))
  )

;;; Ex 6.5

(define-test my-remove-if
  (assert-equal '(a b c) (my-remove-if 'null '(nil a b nil c)))
  (assert-equal nil (my-remove-if 'oddp '(1)))
  )

;;; Ex 6.6

(define-test greatest-arg
  (greatest-arg)
  (assert-equal -5 (greatest-arg -5))
  (assert-equal 0 (greatest-arg 0))
  (assert-equal 2 (greatest-arg 2))
  (assert-equal 2 (greatest-arg 0))
  (assert-equal 10 (greatest-arg 10))
  (greatest-arg)
  (assert-equal -5 (greatest-arg -5))
  (assert-equal 0 (greatest-arg 0))
  )



;;; Ex 6.7

(define-test bigger-arg
  (bigger-arg)
  (assert-false (bigger-arg -5))
  (assert-true (bigger-arg 0))
  (assert-true (bigger-arg 2))
  (assert-false (bigger-arg 0))
  (assert-true (bigger-arg 10))
  (bigger-arg)
  (assert-false (bigger-arg 10))
  )

;;; Ex 6.8

;;; Note that the test memoizes a function that
;;; returns different values for the same argument.
;;; Normally, these are exactly the kinds of functions
;;; you should NOT memoize. We do it here in order to see
;;; if memoize is working.

(define-test memoize
  (let ((l nil))
    (flet ((fn (x) (push x l)))
      (let ((memo-fn (memoize #'fn)))
        (assert-equal '(3) (funcall memo-fn 3))
        (assert-equal '(2 3) (funcall memo-fn 2))
        (assert-equal '(0 2 3) (funcall memo-fn 0))
        
        ;; No consing with old arguments
        (assert-equal '(3) (funcall memo-fn 3))
        (assert-equal '(2 3) (funcall memo-fn 2))
        (assert-equal '(0 2 3) (funcall memo-fn 0))
        
        ;; Clear the cached arguments
        (assert-equal nil (funcall memo-fn))
        
        ;; Now consing happens with old arguments
        (assert-equal '(3 0 2 3) (funcall memo-fn 3))
        (assert-equal '(2 3 0 2 3) (funcall memo-fn 2))
        (assert-equal '(0 2 3 0 2 3) (funcall memo-fn 0))
        
        ;; But only once
        (assert-equal '(3 0 2 3) (funcall memo-fn 3))
        (assert-equal '(2 3 0 2 3) (funcall memo-fn 2))
        (assert-equal '(0 2 3 0 2 3) (funcall memo-fn 0))
        ))))
        

;;; Chapter 7

;;; Ex 7.2 modified

(defun map-stream-list (fn string)
  (let ((l nil))
    (with-input-from-string (in string)
      (map-stream #'(lambda (x) (push (funcall fn x) l))
                  in)
      (nreverse l))))

(defun subforms (l)
  (adjoin l
          (if (atom l) nil
	      (union (subforms (car l)) 
		     (subforms (cdr l))))))

(define-test map-stream
  (assert-equal '(1 a (2 3)) (map-stream-list #'identity "  1 a (2 3)  "))
  (assert-equal '(a nil b c) (map-stream-list #'identity "a nil b c"))
  (assert-equal '(a eof b c) (map-stream-list #'identity "a eof b c"))
  (assert-equal '(2 3 4) (map-stream-list #'1+ "1 2 3"))
  
  ;; If your code returns a number smaller than expected, it means
  ;; it used an EOF object that was found in the input stream.
  (let ((l (subforms (function-lambda-expression #'map-stream))))
    (assert-equal (list (length l) 'objects)
                  (list (length (map-stream-list #'identity (format nil "~{~S~%~}" l)))
                        'objects)))
  )


;;; Ex 7.3 modified

(defun extract-code-from-string (str)
  (with-input-from-string (in str)
    (extract-code-from-stream in)))

(define-test extract-code-from-stream
  (assert-prints "" (extract-code-from-string ""))
  (assert-prints "" (extract-code-from-string "<html>foo baz pre </html>"))
  (assert-prints "(a b)
(c 
d)"
                 (extract-code-from-string "<html>foo<pre>(a b)</pre>baz <pre>   (c 
d)</pre> pre</html>"))
  (assert-prints ""
                 (extract-code-from-string "<prevent>(b a d)</prevent>"))
  )

;;; Ex 7.5				;

;;; Requires Graham's BUF and STREAM-SUBST code ;
;;;					;
;;; Tests courtesy of David Raffensperger ;

(defun string-subst (old new str &key (wildcard '#\+))
  (with-input-from-string (in str)
    (with-output-to-string (out)
      (stream-subst old new in out :wildcard wildcard))))

(define-test stream-subst
  (assert-equal "" (string-subst "" "" ""))
  (assert-equal "d c" (string-subst "a" "d" "a c"))
  (assert-equal "d c dd" (string-subst "a" "d" "a c aa"))
  (assert-equal "ddd" (string-subst "+" "d" "abc"))
  (assert-equal "ddd" (string-subst "-" "d" "abc" :wildcard #\-))
  (assert-equal "" (string-subst "+" "" ""))
  (assert-equal "" (string-subst "+" "" "" :wildcard #\-))
  (assert-equal "" (string-subst "-" "" "" :wildcard #\-))
  (assert-equal "an apple furthermore a pear"
                (string-subst "and" "furthermore" "an apple and a pear"))
  (assert-equal "an apple and a pear and a man and a dog"
                (string-subst "furthermore" "and"
                              "an apple furthermore a pear and a man furthermore a dog"))
  (assert-equal "a dogr is open, a dog is dogted"
                (string-subst "do+" "dog"
                              "a door is open, a dog is dotted"))
  (assert-equal "a door is open, a dog is dotted"
                (string-subst "do+" "dog"
                              "a door is open, a dog is dotted"
                              :wildcard #\-))
  (assert-equal "a dogr is open, a dog is dogted"
                (string-subst "do-" "dog"
                              "a door is open, a dog is dotted"
                              :wildcard #\-))
  )



;;; Chapter 8

;;; Ex 8.4

(define-test ring-package
  (assert-equal :external (nth-value 1 (find-symbol "BUF-CLEAR" "RING")))
  (assert-equal :inherited (nth-value 1 (find-symbol "BUF-CLEAR" "FILE")))
  (assert-equal :external (nth-value 1 (find-symbol "STREAM-SUBST" "FILE")))
  )


;;; Chapter 9

;;; Ex 9.2

(define-test make-change
  (assert-equal (values 2 1 1 2) (make-change 67))
  (assert-equal (values 2 0 0 0) (make-change 50))
  (assert-equal (values 0 0 0 4) (make-change 4))
  (assert-equal (values 0 0 0 0) (make-change 0))
  
  (assert-equal (values 2 0 0 0 1 1) (make-change 67 '(32 16 8 4 2 1)))
  (assert-equal (values 1 1 0 0 1 0) (make-change 50 '(32 16 8 4 2 1)))
  (assert-equal (values 0 0 0 1 0 0) (make-change 4 '(32 16 8 4 2 1)))
  )

(define-test make-best-change
  (assert-equal (values 2 1 1 2) (make-best-change 67))
  (assert-equal (values 2 0 0 0) (make-best-change 50))
  (assert-equal (values 0 0 0 4) (make-best-change 4))
  (assert-equal (values 0 0 0 0) (make-best-change 0))
  (assert-equal (values 1 3) (make-best-change 32 '(11 7)))
  (assert-equal (values 2 1 3) (make-best-change 72 '(23 11 5)))
  (assert-equal (values 0 1 0 3) (make-best-change 93 '(30 26 24 22)))
  )

;;; Ex 9.4

(defun segment-equals (results correct)
  (or (equal results correct)
      (and (= (length correct) 4)
           (equal (first results) (third correct))
           (equal (second results) (fourth correct))
           (equal (third results) (first correct))
           (equal (fourth results) (second correct)))))


(define-test intersect-segments
  (assert-equality 'segment-equals 
                   '(2 2 3 3)
                   (multiple-value-list (intersect-segments '1 1 3 3 2 2 4 4)))
  (assert-equality 'segment-equals
		   '(2 2 3 3) 
		   (multiple-value-list (intersect-segments '3 3 1 1 2 2 4 4)))
  (assert-equality 'segment-equals 
                   '(2 2 3 3) 
                   (multiple-value-list (intersect-segments '1 1 3 3 2 2 5 5)))
  (assert-equal '(nil)
                (multiple-value-list (intersect-segments '1 1 2 2 3 3 4 4)))
  (assert-equality 'segment-equals 
                   '(2 2 2 2)
                   (multiple-value-list (intersect-segments '1 1 2 2 2 2 3 3)))
  (assert-equality 'segment-equals
                   '(2 2) 
                   (multiple-value-list (intersect-segments '1 1 3 3 3 1 1 3)))
  (assert-equal '(nil) 
                (multiple-value-list (intersect-segments '1 1 3 3 4 0 3 1)))
  (assert-equality 'segment-equals
                   '(0 0) 
                   (multiple-value-list (intersect-segments '-1 1 1 -1 0 0 1 1)))
  (assert-equal '(nil) 
                (multiple-value-list (intersect-segments '-3 1 0 -2 -2 1 -1 0)))
  (assert-equality 'segment-equals
                   '(2 2 2 2) 
                   (multiple-value-list (intersect-segments '2 2 2 2 2 2 2 2)))
  (assert-equality 'segment-equals 
                   '(2 3 2 4) 
                   (multiple-value-list (intersect-segments '2 2 2 4 2 3 2 5)))
  (assert-equal '(nil) 
                (multiple-value-list (intersect-segments '2 2 2 2 3 3 3 3)))
  )


;;; Ex 9.5

(defun poly1-2-5 (x)
  (- (* x x) (* 2 x) 5))

(defun poly1-0-9 (x)
  (- (* x x) 9))

(defun epsilon-equals (returned correct)
  (< (abs (- returned correct)) 0.01))

(define-test solve
  (assert-equality 'epsilon-equals 3.4494896 (solve 'poly1-2-5 1 5 0.01))
  (assert-equality 'epsilon-equals 3 (solve 'poly1-0-9 2.8 3.2 0.01))
  (assert-equality 'epsilon-equals -3 (solve 'poly1-0-9 -11.1 -2.9 0.01))
  )

;;; Ex 9.6

(define-test horner
  (assert-equality 'epsilon-equals -2 (horner '3 1 -2 -5))
  (assert-equality 'epsilon-equals 10 (horner '-3 1 -2 -5))
  (assert-equality 'epsilon-equals 5 (horner '3 5))
  (assert-equality 'epsilon-equals -4 (horner '-3 2 4 -2 8))
  )

;;; Chapter 10

;;; Ex 10.3

(define-test nth-expr
  (assert-equal 3 (let ((n 2)) (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0))))
  (assert-prints "win"
                 (let ((n 3) (x "lose") (y "win"))
                   (nth-expr n (princ x) (princ x) (princ y) (princ x))))
  (assert-equal 3
                (let ((n 2)) (nth-expr (incf n) 1 2 3 4 5)))
  )


;;; Ex 10.5

(define-test n-of
  (assert-equal '(1 2 3 4) (let ((i 0) (n 4)) (n-of n (incf i))))
  (assert-equal '(a) (let* ((l '(1 2 3)) (pop l)) (n-of (pop l) 'a)))
  )

;;; Ex 10.6

(define-test preserve
  (assert-equal 10 (progn (preserve (*read-base*) (setq *read-base* 2)) *read-base*))
  (assert-equal 'a (let ((x 'a)) (preserve (x) (setq x 'b)) x))
  (assert-equal 10 (preserve (*read-base*) *read-base*))
  (assert-equal 'a (let ((x 'a)) (preserve (x) x)))
  (assert-equal '(10 "1010" a)
                (let ((x 'a) (str nil))
                  (preserve (*print-base* x) (setq *print-base* 2 x 'b)
			    (setq str (format nil "~s" 10)))
                  (list *print-base* str x)))
  )

;;; Chapter 12

(define-test make-queue
  (assert-equal '(nil) (make-queue))
  (assert-equal '((1 2 3) 3) (make-queue 1 2 3))
  (assert-equal '((1 2 3) 3) (flet ((fn nil (make-queue 1 2 3))) (enqueue 4 (fn)) (fn)))
  )

(define-test empty-queue-p
  (assert-true (empty-queue-p (make-queue)))
  (assert-false (empty-queue-p (make-queue 1)))
  )

;;; Ex 12.3

(define-test copy-queue
  (assert-equal '(nil) (let* ((q (make-queue)) (qc (copy-queue q))) qc))
  (assert-equal '(nil) (let* ((q (make-queue)) (qc (copy-queue q))) (enqueue 1 q) qc))
  (assert-equal '((1 2 3 4) 4) (let* ((q (make-queue 1 2 3 4)) (qc (copy-queue q))) qc))
  (assert-equal '((1 2 3 4) 4) 
                (let* ((q (make-queue 1 2 3 4)) (qc (copy-queue q))) (dequeue q) qc))
  )


;;; Ex 12.4

(define-test enqueue-front
  (assert-equal '((a) a) (let ((q (make-queue))) (enqueue-front 'a q) q))
  (assert-equal '((4 1 2 3) 3) (let ((q (make-queue 1 2 3))) (enqueue-front 4 q) q))
  )

;;; Ex 12.5

(define-test requeue-front
  (assert-equal '((1 2 3) 3) (let ((q (make-queue 1 2 3))) (requeue-front 1 q) q))
  (assert-equal '((2 1 3) 3) (let ((q (make-queue 1 2 3))) (requeue-front 2 q) q))
  (assert-equal '((3 1 2) 2) (let ((q (make-queue 1 2 3))) (requeue-front 3 q) q))
  (assert-equal '((1 2 3) 3) (let ((q (make-queue 1 2 3))) (requeue-front 4 q) q))
  (assert-equal '((2 1 1 2 3) 3) (let ((q (make-queue 1 2 1 2 3))) (requeue-front 2 q) q))
  )

;;; Ex 12.6

(define-test circular-member-p
  (let ((*print-circle* t))
    (assert-true (circular-member-p 1 '(1 2 3 4)))
    (assert-true (circular-member-p 2 '(1 2 3 4)))
    (assert-true (circular-member-p 4 '(1 2 3 4)))
    (assert-false (circular-member-p 5 '(1 2 3 4)))
    (assert-false (circular-member-p 1 nil))
    (assert-true (circular-member-p 1 '#1=(1 2 3 4 . #1#)))
    (assert-true (circular-member-p 2 '#2=(1 2 3 4 . #2#)))
    (assert-true (circular-member-p 3 '#3=(1 2 3 4 . #3#)))
    (assert-true (circular-member-p 4 '#4=(1 2 3 4 . #4#)))
    (assert-false (circular-member-p 5 '#5=(1 2 3 4 . #5#)))
    (assert-true (circular-member-p 1 '#6=(1 . #6#)))
    (assert-false (circular-member-p 5 '#7=(1 . #7#)))
    (assert-true (circular-member-p 3 '(1 2 3 4 5 6 7 8 . #8=(9 10 11 . #8#))))
    (assert-true (circular-member-p  4 '#9=(1 2 3 4 . #9#)))
    (assert-true (circular-member-p 7 '(1 2 3 4 . #10=(5 6 7 . #10#))))
    (assert-true (circular-member-p 11 '(1 2 3 4 5 6 7 8 . #11=(9 10 11 . #11#))))
    ))

;;; Ex 12.7

(define-test cdr-circular-p
  (let ((*print-circle* t))
    (assert-false (cdr-circular-p '(1 2 3 4)))
    (assert-true (cdr-circular-p '#1=(1 2 3 4 . #1#)))
    (assert-true (cdr-circular-p '(1 2 3 4 5 6 7 8 . #2=(9 10 11 . #2#))))
    (assert-false (cdr-circular-p '(1 2 3 4 5 6 7 8 . #3=(9 10 11 #3#))))
    (assert-false (cdr-circular-p nil))
    ))

;;; Ex 12.8

(define-test car-circular-p
  (let ((*print-circle* t))
    (assert-false (car-circular-p '(1 2 3 4)))
    (assert-false (car-circular-p '#1=(1 2 3 4 . #1#)))
    (assert-false (car-circular-p '((1 2) 3 4)))
    (assert-true (car-circular-p '#2=(#2#)))
    (assert-true (car-circular-p '#3=((1 2) 3 #3#)))
    (assert-false (car-circular-p '(#4=(1 2) 3 #4#)))
    (assert-false (car-circular-p '(((1 2 3)) 4 ((5 6)))))
    (assert-false (car-circular-p '(((1 2 . #5=(3))) 4 ((5 . #5#)))))
    (assert-false (car-circular-p '(#6=((1 2 3)) 4 ((5 . #6#)))))
    (assert-true (car-circular-p '(((1 2 3)) 4 . #7=(((5 . #7#))))))
    (assert-true (car-circular-p '#8=(1 2 (((3 #8#))) 4)))
    (assert-true (car-circular-p '(#9=(1 2 3 . #9#))))
    (assert-true (car-circular-p '#10=(#11=((1 2) 3 #11#) 2 3 . #10#)))
    (assert-true (car-circular-p '#12=(a . ((b . #12#) . c))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for exercises on Lisp Exercises page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Ex Lisp #1

(define-test has-number-p
  (assert-false (has-number-p 'nil))
  (assert-true (has-number-p 1))
  (assert-false (has-number-p 'a))
  (assert-true (has-number-p '(1)))
  (assert-false (has-number-p '(a)))
  (assert-true (has-number-p '(a (1))))
  (assert-false (has-number-p '(a (a))))
  )

;;; Ex Lisp #2

(define-test key-if
  (assert-expands '(cond (flag x y) (t nil)) (key-if flag :then x y))
  (assert-expands '(cond (flag x y) (t z w)) (key-if flag :then x y :else z w))
  (assert-expands '(cond (flag nil) (t z w)) (key-if flag :else z w))
  (assert-expands '(cond (flag x y) (t z w)) (key-if flag :else z w :then x y))
  (assert-equal 'ok (let ((n 2)) (key-if (> n 1) :else 'oops :then 'ok)))
  )

;;; Ex Lisp #3

(define-test make-balance
  (let ((bal (make-balance 100)))
    (assert-equal 100 (funcall bal))
    (assert-equal 110 (funcall bal 10))
    (assert-equal 105 (funcall bal -5))
    (assert-equal 105 (funcall bal)))
  )

;;; Ex Lisp #4

(define-test delete-car
  (let ((l (list 1 2 3 4)))
    (assert-equal '(2 3 4) (delete-car l))
    (assert-equal '(2 3 4) l)
    (assert-equal '(3 4) (delete-car l))
    (assert-equal '(3 4) l)
    (assert-equal '(4) (delete-car l))
    (assert-equal '(4) l)
    (assert-equal 'nil (delete-car l)))
  (assert-equal nil (delete-car nil))
  (assert-equal '(nil 2) (delete-car (list 1 nil 2)))
  )

;;; Ex Lisp #5

(define-test collect-numbers
  (assert-equal nil (collect-numbers nil))
  (assert-equal nil (collect-numbers 'b))
  (assert-equal '(3) (collect-numbers '3))
  (assert-equal '(1 2 3) (collect-numbers '(1 2 3)))
  (assert-equal '(1 2 3) (collect-numbers '(1 (b (2 c) ((3))))))
  (assert-equal '(1 3) (collect-numbers '(1 nil (3 nil))))
  )

;;; Ex Lisp #6

(define-test tconc
  (let* ((l1 (list 1 2))
         (tc (make-tconc l1))
         (l2 (list 'a 'b)))
    (assert-eq l1 (tconc tc))
    (assert-equal '(1 2) (tconc tc))
    (assert-equal '(1 2 3 4) (tconc tc 3 4))
    (assert-equal '(1 2 3 4 a b) (tconc-list tc l2))
    (assert-equal '(a b) l2)
    (assert-equal '(1 2 3 4 a b) (tconc tc))
    (assert-eq l1 (tconc tc))
    (assert-eq (tconc tc) (tconc tc)))
  (let ((tc (make-tconc)))
    (assert-equal nil (tconc tc))
    (assert-equal '(1) (tconc tc 1))
    (assert-equal '(1 2 3) (tconc tc 2 3))
    (assert-equal '(1 2 3) (tconc tc)))
  )

;;; Ex Lisp #7

(define-test list-of
  (assert-equal '(2 3 4) (list-of (1+ x) (x :in '(1 2 3))))
  (assert-equal '(2 4) (list-of (1+ x) (x :in '(1 2 3)) (oddp x)))
  (assert-equal '((a a) (a b) (b a) (b b))
                (list-of (list x y) (x :in '(a b)) (y :in '(a b))))
  (assert-equal '((1 2) (1 4) (3 2) (3 4))
                (list-of (list x y) (x :in '(1 2 3 4))
                         (oddp x) (y :in '(1 2 3 4)) (evenp y)))
  (assert-equal '((1 2) (1 4) (3 2) (3 4))
                (list-of (list x y) (x :in '(1 2 3 4))
                         (y :in '(1 2 3 4)) (oddp x) (evenp y)))
  (assert-equal '(t) (list-of t))
  (assert-equal '(nil) (list-of nil))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BugWeb Exercises
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test functions-referenced
  (assert-equality 'set-equal nil (functions-referenced nil))
  (assert-equality 'set-equal nil (functions-referenced 'a))
  (assert-equality 'set-equal nil (functions-referenced 1))
  (assert-equality 'set-equal '(car) (functions-referenced '(car l)))
  (assert-equality 'set-equal '(+ car cdr) (functions-referenced '(+ (car l) (cdr l))))
  (assert-equality 'set-equal '(list quote) (functions-referenced '(list 'a 'b)))
  (assert-equality 'set-equal '(+ *) (functions-referenced '(+ (* x y) (* y z))))
  (assert-equality 'set-equal nil (functions-referenced "(car '(a b c))"))
  (assert-equality 'set-equal '(member function equal) (functions-referenced '(member x l :test #'equal)))
  (assert-equality 'set-equal '(let car cdr cond null +)
		   (functions-referenced '(let ((x (car l)) (r (cdr l))) (cond ((null l) (+ x 2)) (t r)))))
  (assert-equality 'set-equal '(lambda car) (functions-referenced '(lambda (l) (car l))))
  (assert-equality 'set-equal '(do length 1-  cons null reverse print cdr)
		   (functions-referenced
		    '(do ((n (length l) (1- n)) (l nil (cons n l)))
		      ((null l) (reverse l))
		      (print n)
		      (print (cdr l)))))
  (assert-equality 'set-equal '(defun + car cdr) (functions-referenced '(defun foo (x y) (+ (car x) (cdr y)))))
  (assert-equality 'set-equal '(defun 1+ + car cdr)
		   (functions-referenced '(defun foo (x y &optional (z (1+ x))) (+ (car x) (cdr y)))))
  (assert-equality 'set-equal '(defun max min list)
		   (functions-referenced '(defun foo (x y &key (big (max x y)) (small (min x y))) (list big small))))
  )

;;; requires bugmops.lisp

(define-test common-absts
  (assert-equality 'set-equal '(m-test-taking-function m-non-destructive-function)
		   (common-absts 'remove 'member))
  (assert-equality 'set-equal '(m-sequence-function) (common-absts 'remove 'delete))
  (assert-equality 'set-equal '(m-test-taking-function) (common-absts 'delete 'member))
  (assert-equality 'set-equal '(m-function) (common-absts 'nconc 'member))
  (assert-equality 'set-equal nil (common-absts 'foo 'member))
  )

(define-test absts-abstp
  ;; all the regular tests
  (assert-true (absts-abstp '(m-test-taking-function) '(m-sequence-function)))
  (assert-false (absts-abstp '(m-sequence-function) '(m-test-taking-function)))
  (assert-true (absts-abstp '(m-sequence-function) '(m-non-destructive-function m-input-function)))
  (assert-false (absts-abstp '(m-non-destructive-function m-input-function) '(m-sequence-function)))
  (assert-false (absts-abstp '(m-sequence-function) '(m-sequence-function)))
  (assert-false (absts-abstp '(m-sequence-function m-io-function) '(m-test-taking-function m-output-function)))
  (assert-false (absts-abstp '(m-test-taking-function m-output-function) '(m-sequence-function m-io-function)))
  (assert-false (absts-abstp '(member) '(m-test-taking-function m-non-destructive-function)))
  (assert-true (absts-abstp '() '(m-callable)))
  (assert-false (absts-abstp '(m-callable) '()))
  (assert-false (absts-abstp '() '()))

  ;; tests added from problem spec
  (assert-true (absts-abstp '(m-output-function) '(m-test-taking-function m-destructive-function)))

  ;; new tests
  (assert-false (absts-abstp '(m-non-destructive-function) '(m-destructive-function)))
  (assert-false (absts-abstp '(m-destructive-function) '(m-non-destructive-function)))
  (assert-false (absts-abstp '(m-non-destructive-function) '(m-destructive-function)))
  (assert-false (absts-abstp '(m-non-destructive-function) '(m-destructive-function)))
  (assert-false (absts-abstp '(m-non-destructive-function) '(m-sequence-function)))
  (assert-false (absts-abstp '(m-sequence-function) '(m-non-destructive-function)))
  )


(use-package :extend-match)

(define-test ?is-a
  (assert-true (pat-match '(?is-a m-test-taking-function) 'member))
  (assert-true (pat-match '(?is-a m-sequence-function) 'remove))
  (assert-true (pat-match '(?is-a m-test-taking-function) 'remove))
  (assert-false (pat-match '(?is-a m-sequence-function) 'member))
  (assert-equal '(((x . member)))
                (pat-match '((? x) (?is-a m-sequence-function)) '(member remove)))
  )

(use-package :extend-match)

(define-test member-match
  (let ((pat '((?or member remove) '(?and (?is listp) (? x)) '((?*) (? x) (?*)))))
    (assert-true (pat-match pat '(member '(a) '((a) (b) (c)))))
    (assert-true (pat-match pat '(member '(b) '((a) (b) (c)))))
    (assert-true (pat-match pat '(remove '(b) '((a) (b) (c)))))
    (assert-false (pat-match pat '(member 'a '(a b c))))
    (assert-false (pat-match pat '(member '(a) '(((a)) (b) (c)))))
    (assert-false (pat-match pat '(member '(a) '((a) (b)) :test #'equal)))
    ))


(define-test test-taking-match
  (let ((pat '((?is-a m-test-taking-function)
               (quote (?and (?is listp) (? x)))
               (quote ((?*) (? x) (?*))))))
    (assert-true (pat-match pat '(member '(a) '((a) (b) (c)))))
    (assert-true (pat-match pat '(member '(b) '((a) (b) (c)))))
    (assert-true (pat-match pat '(remove '(b) '((a) (b) (c)))))
    (assert-false (pat-match pat '(member 'a '(a b c))))
    (assert-false (pat-match pat '(member '(a) '(((a)) (b) (c)))))
    (assert-false (pat-match pat '(member '(a) '((a) (b)) :test #'equal)))
    ))

(provide "exercise-tests")
