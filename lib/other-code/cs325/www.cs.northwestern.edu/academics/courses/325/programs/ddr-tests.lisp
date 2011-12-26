;;; Change Log
;;;
;;; 02/14/07: changed can-satisfy-hunger rules in *tiger-kb* [CKR]
;;; 03/21/06: changed tests to use init-kb [CKR]
;;; 03/05/05 Expanded documentation on Monkeys and Bananas [CKR]
;;; 03/05/05 Added Peano addition example [CKR]


(defpackage #:ddr-tests
  (:use #:common-lisp #:lisp-unit #:ddr)
  )

(in-package :ddr-tests)

;;; Simple ancestry-type knowledge base.
;;;
;;; NOTE: This is a BAD set of rules. It's
;;; combinatorially explosive and produces 
;;; many redundant answers.
;;; E.g., try (ASK-TRACE '(HORSE ?X)).
;;; How can you fix this and still
;;; pass the tests?

(defparameter *horses-kb*
  '(
    ;; x is ancestor of y if x is a parent or a parent of an ancestor
    (<- (ancestor ?x ?y) (parent ?x ?y))
    (<- (ancestor ?x ?y) (parent ?x ?z) (ancestor ?z ?y))
      
    ;; x is a horse if x is a descendant of a horse
    (<- (horse ?x) (ancestor ?y ?x) (horse ?y))
      
    ;; some real horses, not all real relationships
    (parent man-o-war war-admiral)
    (parent war-admiral seabiscuit)
    (parent seabiscuit kelso)
    (horse man-o-war)
    ))

(define-test horses
  (init-kb *horses-kb*)

  (dolist (x '(man-o-war war-admiral seabiscuit kelso))
    (assert-true (ask `(horse ,x)) x))

  (assert-true
   (set-equal '(man-o-war war-admiral seabiscuit kelso)
              (ask '(horse ?x) '?x)))
  )


(defparameter *tiger-kb*
  '(
    (<- (can-satisfy-hunger-with ?x ?y)
        (eats ?x ?y)
        (can-bite ?x ?y))

    (<- (eats ?y ?x)
        (predator-of ?x ?y))

    (<- (can-bite ?x ?y)
        (isa ?x animal)
        (near ?x ?y))

    (<- (near ?x ?y)
        (at ?x ?loc)
        (at ?y ?loc))

    (eats antelope grass)
    (eats antelope ferns)
    (predator-of antelope tiger)
    (predator-of zebra tiger)

    (at antelope savannah)
    (at tiger savannah)
    (at grass savannah)

    (isa tiger animal)
    (isa antelope animal)
    ))

(define-test tiger
  (init-kb *tiger-kb*)
  (assert-true (ask '(at antelope savannah)))
  (assert-true (ask '(near antelope grass)))
  (assert-true (ask '(can-bite antelope grass)))
  (assert-true (ask '(can-satisfy-hunger-with antelope grass)))
  (assert-false (ask '(can-satisfy-hunger-with antelope ferns)))
  (assert-true (ask '(can-satisfy-hunger-with tiger antelope)))
  (assert-false (ask '(can-satisfy-hunger-with tiger grass)))
  (assert-false (ask '(can-bite grass antelope)))

  )


;;; Addition, Peano style
;;;
;;; A simple example of using functional terms to represent
;;; constructed results.


(defparameter *peano-kb*
  '(
    (add 0 ?x ?x)
    (<- (add (succ ?x) ?y (succ ?z))
        (add ?x ?y ?z))
    ))

(define-test peano
  (init-kb *peano-kb*)
  ;; 0 + 0 = 0
  (assert-true (ask '(add 0 0 0) '?x))
  ;; 0 + 1 = 1
  (assert-true (ask '(add 0 (succ 0) (succ 0))))
  ;; 1 + 0 = 1
  (assert-true (ask '(add (succ 0) 0 (succ 0))))
  ;; 2 + 2 = 4
  (assert-true (ask '(add (succ (succ 0)) (succ (succ 0))
                          (succ (succ (succ (succ 0)))))))
  ;; 2 + 1 != 4
  (assert-false (ask '(add (succ (succ 0)) (succ 0)
                           (succ (succ (succ (succ 0)))))))
  ;; 0 + 0 => 0
  (assert-equal '(0) (ask '(add 0 0 ?x) '?x))
  ;; 0 + 1 => 1
  (assert-equal '((succ 0)) (ask '(add 0 (succ 0) ?x) '?x))
  ;; 1 + 0 => 1
  (assert-equal '((succ 0)) (ask '(add (succ 0) 0 ?x) '?x))
  ;; 2 + 2 => 4
  (assert-equal '((succ (succ (succ (succ 0)))))
                (ask '(add (succ (succ 0)) (succ (succ 0)) ?x) 
                     '?x))
  ;; 1 + x = 3 => x = 2, or 3 - 1 => 2
  (assert-equal '((succ (succ 0)))
                (ask '(add (succ 0) ?x (succ (succ (succ 0))))
                     '?x))
  ;; x + y = 3 => <3, 0>, <2, 1>, <1, 2>, <0, 3>
  (assert-equal '((0 (succ (succ (succ 0))))
                  ((succ 0) (succ (succ 0)))
                  ((succ (succ 0)) (succ 0))
                  ((succ (succ (succ 0))) 0)
                  )
                (ask '(add ?x ?y (succ (succ (succ 0))))
                     '(?x ?y)))
  )

;;; APPEND, Prolog-style

(defparameter *append-kb*
  '(
    (append nil ?x ?x)
    (<- (append (cons ?x ?l1) ?l2 (cons ?x ?l3))
        (append ?l1 ?l2 ?l3))
    ))


(define-test append
  (init-kb *append-kb*)
  (assert-equal '((cons a (cons b (cons c nil))))
                (ask '(append (cons a (cons b nil))
                              (cons c nil)
                              ?l)
                     '?l))
  (assert-equal '((cons c nil))
                (ask '(append (cons a (cons b nil))
                              ?l
                              (cons a (cons b (cons c nil))))
                     '?l))
  (assert-equal '(nil (cons a nil) (cons a (cons b nil)))
                (ask '(append ?x ?y (cons a (cons b nil)))
                     '?x))
  )

;;; This checks for a variable renaming bug that was in 
;;; ddr.lisp for 20 years! I

(defparameter *analogy-kb*
  '(
    (<- (analogous ?x ?y ?a ?b)
        (similar ?x ?a)
        (similar ?y ?b))
    (similar ?x ?x)
    ))

(define-test analogy
  (init-kb *analogy-kb*)
  
  (assert-true (ask '(analogous a a a a)))
  (assert-true (ask '(analogous b b b b)))
  (assert-true (ask '(analogous a b a b)))
  (assert-equal '((analogous a b a b))
                (ask '(analogous a b ?x ?y)))
  )


;;; The Monkey and Bananas problem.
;;;
;;; This is a classic toy problem in AI. A monkey is
;;; in a room. Hanging out of reach in the center of
;;; the room are some bananas. In another part of the
;;; room is a box the monkey could stand on. Can the
;;; monkey get the bananas?
;;;
;;; The state of the world is represented by 
;;;
;;;   (STATE monkey-location box-location monkey-on)
;;;
;;; This is a functional term, NOT a predicate. It
;;; represents a state. It is not a claim about
;;; a state. It's equivalent to the noun phrase
;;; "the state in which the monkey is at ...,"
;;; rather than the sentence "the monkey is at ..."
;;;
;;; A plan for getting the bananas is a sequence of
;;; actions, represented with the functional term
;;; (DO action plan). DONE is used to represent the
;;; empty plan with no actions.
;;;
;;; An action is either a simple name like CLIMB-BOX,
;;; for "the action of the monkey climbing on the box,"
;;; or a functional term like (PUSH-BOX loc1 loc2)
;;; for "the action of the monkey pushing the box from 
;;; loc1 to loc2."
;;;
;;; The goal predicate is (CAN-GET start-state plan)
;;; which says that the monkey can get the bananas by
;;; doing the given plan in the given starting state.
;;;
;;; The predicate is (RESULTS state1 action state2)
;;; which says that doing action in state1 results in
;;; state2.
;;;
;;; The predicate (AT object location) says the object
;;; is at the location.
;;;
;;; A normal query will give a starting state and a
;;; variable for the plan. The resulting value(s) for
;;; the plan variable, if any, say how the monkey can
;;; get the bananas.
;;;
;;; Because DDR finds all solutions, you have to make
;;; sure your rules do not generate an infinite number
;;; of answers. E.g., you could just write a rule that
;;; says that walking from loc1 to loc2 moves the monkey
;;; from loc1 to loc2. But this leads to generating
;;; going to the door, going to the window, going to the
;;; door, ... Pretty dumb for an AI.


(defparameter *monkey-kb*
  '(
    ;; If the monkey and box are in the center of
    ;; the room and the monkey is on on the box,
    ;; then nothing more needs to be done to get the
    ;; bananas.
    (<- (can-get (state ?loc ?loc box) done)
        (at bananas ?loc))

    ;; The monkey can get the bananas in state1 with
    ;; some sequence of actions, if the first action
    ;; leads from state1 to state2 and the monkey can
    ;; get the bananas in state2 by performing the
    ;; remaining actions.
    (<- (can-get ?state1 (do ?action ?steps))
        (results ?state1 ?action ?state2)
        (can-get ?state2 ?steps))
   
    ;; The monkey can climb on the box when they're
    ;; in the same location.
    ;; CAUTION: if you add climbing down, then you 
    ;; need to constrain when the monkey should climb
    ;; on the box, to avoid an infinite number of
    ;; solutions.
    (results (state ?loc ?loc floor)
             climb-box
             (state ?loc ?loc box))
   
    ;; The monkey can push the box to where the 
    ;; bananas are if the monkey and box are in one
    ;; place and the bananas are somewhere else.
    (<- (results (state ?loc1 ?loc1 floor)
                 (push-box ?loc1 ?loc2)
                 (state ?loc2 ?loc2 floor))
        (at bananas ?loc2)
        (different ?loc1 ?loc2))
   
    ;; The monkey can walk to where the box is
    ;; if theyre in different places.
    (<- (results (state ?mloc ?bloc floor)
                 (walk ?mloc ?bloc)
                 (state ?bloc ?bloc floor))
        (different ?mloc ?bloc))
   
    ;; If x is not y, then y is not x.
    (-> (different ?x ?y) (different ?y ?x))
   
    ;; Every location is different from every other
    ;; location.
    (different window center)
    (different window door)
    (different center door)
    (different box floor)
   
    ;; The bananas are in the center of the room.
    (at bananas center)
   ))


(define-test monkey
  (init-kb *monkey-kb*)
  (assert-true (ask '(can-get (state center center box) ?steps)))
  (assert-true (ask '(can-get (state center center floor) ?steps)))
  (assert-true (ask '(can-get (state window window floor) ?steps)))
  (assert-true (ask '(can-get (state door window floor) ?steps)))
  (assert-false (ask '(can-get (state window window box) ?steps)))
  )

