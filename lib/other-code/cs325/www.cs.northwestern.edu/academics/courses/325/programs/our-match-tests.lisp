(defpackage :our-match-tests
  (:use :common-lisp :lisp-unit :our-match)
  )

(in-package :our-match-tests)

(remove-all-tests)

(define-test no-variables
  (assert-true (our-match 1 1))
  (assert-true (our-match nil nil))
  (assert-true (our-match 'a 'a))
  (assert-true (our-match '(a b c) '(a b c)))
  (assert-true (our-match '(((a b) c)) '(((a b) c))))
  )

(define-test one-variable
  (assert-true (our-match '(? x) 'a))
  (assert-true (our-match '(? x) nil))
  (assert-true (our-match '((? x)) '(a)))
  (assert-true (our-match '(? x) '(a b c)))  
  (assert-true (our-match '((? x) (? x)) '(a a)))
  )

(define-test multi-variable
  (assert-true (our-match '((? x) (? y)) '(a b)))
  (assert-true
   (our-match '((? x) (? x))
              (list (list 'a 'b) (list 'a 'b))))
  )

(define-test one-variable-blists
  (assert-equal '(((x . a))) (our-match '(? x) 'a))
  (assert-equal '(((x . a))) (our-match '((? x) (? x)) '(a a)))
  )

(define-test multi-variable-blists
  (assert-equal '(((y . b) (x . a)))
                (our-match '((? x) (? y)) '(a b)))
  (assert-equal '(((y . a) (x . a)))
                (our-match '((? x) (? y)) '(a a)))
  )

(define-test match-and
  (assert-equal '(((x . a)))
                (our-match '(?and (? x) (? x)) 'a))
  (assert-equal '(((y . a) (x . a)))
                (our-match '(?and (? x) (? y)) 'a))
  (assert-equal '(((y . b) (x . a)))
                (our-match '(?and ((? x) b)
                                  (a (? y)))
                           '(a b)))
  (assert-equal '(((x . 12)))
   (our-match '(?and (?is numberp) (? x)) 12))

  (assert-false (our-match '(?and a b) 'a))
  (assert-false (our-match '(?and a b) 'b))
  (assert-false (our-match '(?and (? x) (?not (? x))) 12))
  )
  

(define-test match-is
  (assert-equal '(nil) (our-match '(?is numberp) 12))

  (assert-false (our-match '(?is numberp) 'a))
  )

(define-test match-not
  (assert-equal '(nil) (our-match '(?not a) 'b))
  (assert-equal '(nil) (our-match '(?not (?not a)) 'a))
  (assert-equal '(((x . a))) 
                (our-match '((? x) (?not (? x))) '(a b)))
  (assert-equal '(((x a b c)))
                (our-match '((? x) (?not (? x)))
                           '((a b c) (a b d))))
  (assert-equal '(nil)
                (our-match '(?not (a (? x) c))
                           '(b a c)))

  (assert-false (our-match '(?not a) 'a))
  (assert-false (our-match '(?not (?not a)) 'b))
  (assert-false (our-match '(?not (? x)) 'a))
  (assert-false (our-match '((? x) (?not (? x))) '(a a)))
  (assert-false (our-match '((? x) (?not (? x)))
                           '((a b c) (a b c))))
  )

(define-test match-star
  (assert-equal '(nil) (our-match '((?*) d) '(d)))
  (assert-equal '(nil) (our-match '((?*) d) '(a b c d)))
  (assert-equal '(nil)
                (our-match '((?*) d (?*)) '(a b c d e f)))
  (assert-equal '(((l . nil)))
                (our-match '((?* l) d (?* l))
                           '(d)))
  (assert-equal '(((l a)))
                (our-match '((?* l) d (?* l))
                           '(a d a)))
  (assert-equal '(((l a b c)))
                (our-match '((?* l) d (?* l))
                           '(a b c d a b c)))

  (assert-equal '(((l a b)))
                (our-match '((?* l) (? l))
                           '(a b (a b))))

  (assert-equal '(((l a b)))
                (our-match '((? l) (?* l))
                           '((a b) a b)))

  (assert-true (set-equal
                '(((x . c))
                  ((x . b))
                  ((x . a)))
                (our-match '((?*) (? x) (?*))
                           '(a b c)))
               :test #'equal)

  (assert-equal '(((x . c))
                  ((x . b))
                  ((x . a)))
                (our-match '((?*) (? x) (?*))
                           '(a b c)))

  (assert-equal '(((l1 a b) (l2 . nil))
                  ((l1 a) (l2 b))
                  ((l1 . nil) (l2 a b)))
                (our-match '((?* l1) (?* l2))
                           '(a b)))

  (assert-false (our-match '((?*) d) '(a d b)))
  (assert-false (our-match '((?* l) d (?* l))
                           '(a b c d a b)))
  )


(define-test non-match
  (assert-false (our-match 1 2))
  (assert-false (our-match 'a 'b))
  (assert-false (our-match 1 'a))
  (assert-false (our-match '(a b) '(a b c)))
  (assert-false (our-match '(((a b) c)) '(((a b) c d))))

  (assert-false (our-match '((? x) (? x)) '(a b)))
  (assert-false (our-match '((? x)) '(a b)))
  (assert-false (our-match '((? x)) 'a))
  (assert-false (our-match '((? x)) '()))
  )
