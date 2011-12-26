(in-package :ddr-tests)

;;; Test cases for the exercises in ddr-exs.html

;;; MEMBER

(define-test member
  (assert-false (ask '(member a nil)))
  (assert-true (ask '(member a (cons a nil))))
  (assert-true (ask '(member b (cons a (cons b (cons c nil))))))
  (assert-false (ask '(member d (cons a (cons b (cons c nil))))))
  )


;;; ALL-DIFFERENT

(define-test all-different
  (assert-false (ask '(different a b)))
  (tell '(all-different nil))
  (assert-false (ask '(different a b)))
  (tell '(all-different (cons a (cons b (cons c nil)))))
  (assert-true (ask '(different a b)))
  (assert-true (ask '(different a c)))
  (assert-true (ask '(different b a)))
  (assert-true (ask '(different b c)))
  (assert-true (ask '(different c a)))
  (assert-true (ask '(different c b)))
  (assert-false (ask '(different a a)))
  )

;;; MAP COLORING

(define-test color-map1
  (assert-equal '((colors-for map1 red blue green yellow))
                (ask '(colors-for map1 red blue green ?d)))
  (assert-equal 2 (length (ask '(colors-for map1 red blue ?c ?d))))
  (assert-equal 24 (length (ask '(colors-for map1 ?a ?b ?c ?d))))
  (assert-equal nil (ask '(colors-for map1 red blue green red)))
  )

(define-test color-map2
  (assert-equal '((colors-for map2 red blue green blue yellow))
                (ask '(colors-for map2 red blue green ?d ?e)))
  (assert-equal 2 (length (ask '(colors-for map2 red blue ?c ?d ?e))))
  (assert-equal 24 (length (ask '(colors-for map2 ?a ?b ?c ?d ?e))))
  (assert-equal nil (ask '(colors-for map2 red blue green yellow ?e)))
  )

(define-test color-map3
  (assert-equal '((colors-for map3 red blue green yellow green blue))
                (ask '(colors-for map3 red blue green yellow ?e ?f)))
  (assert-equal 1 (length (ask '(colors-for map3 red blue green ?d ?e ?f))))
  (assert-equal 24 (length (ask '(colors-for map3 ?a ?b ?c ?d ?e ?f))))
  (assert-equal nil (ask '(colors-for map3 red blue green blue ?e ?f)))
  )


;;; SHAKEY 1.0

;;; Test cases for 1 box, no locks.
;;;
;;; The goal state is always (v1-state ?rloc room1), meaning
;;; the box has to end up in room1, and it doesn't matter where
;;; the robot ends up.


(define-test shakey-1 
  (assert-equal '(nil)
                (ask '(plan-for (v1-state room1 room1)
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box hall room1) nil))
                (ask '(plan-for (v1-state hall hall) 
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box room2 hall)
                      (cons (push-box hall room1) nil)))
                (ask '(plan-for (v1-state room2 room2) 
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (move-to hall)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (push-box hall room1) nil)))))
                (ask '(plan-for (v1-state room1 room2) 
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  )

;;; SHAKEY 2.0

;;; Test cases for 1 box with locks.
;;;
;;; The goal state is always
;;;
;;;   (v2-state ?rloc room1 ?unlocked)
;;;
;;; meaning the box has to end up in room1, and we don't care
;;; where the robot is or what rooms are unlocked.

(define-test shakey-2  
  ;; Test with rooms unlocked
  (assert-equal '(nil)
                (ask '(plan-for (v2-state room1 room1 nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box hall room1) nil))
                (ask '(plan-for (v2-state hall hall (cons room1 nil))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box room2 hall) (cons (push-box hall room1) nil)))
                (ask '(plan-for (v2-state room2 room2 (cons room1 (cons room2 nil)))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  (assert-equal '((cons (move-to hall)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (push-box hall room1) nil)))))
                (ask '(plan-for (v2-state room1 room2 (cons room1 (cons room2 nil)))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  
  ;; Test with the room with the box locked
  (assert-equal '((cons (move-to hall)
                      (cons (unlock room2)
                          (cons (move-to room2)
                              (cons (push-box room2 hall)
                                  (cons (push-box hall room1) nil))))))
                (ask '(plan-for (v2-state room1 room2 (cons room1 nil))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  
  ;; Test with the goal room locked, robot and box in hall
  (assert-equal '((cons (unlock room1)
                      (cons (push-box hall room1) nil)))
                (ask '(plan-for (v2-state hall hall nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))

  ;; Test with the goal room locked, robot in hall, box in room2
  (assert-equal '((cons (move-to room2)
                      (cons (push-box room2 hall)
                          (cons (unlock room1)
                              (cons (push-box hall room1) nil)))))
                (ask '(plan-for (v2-state hall room2 (cons room2 nil))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))

  ;; Test with both rooms locked and the robot in the hall
  (assert-equal '((cons (unlock room2)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (unlock room1)
                                  (cons (push-box hall room1) nil))))))
                (ask '(plan-for (v2-state hall room2 nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  
  ;; Test with robot in locked room
  (assert-equal nil
                (ask '(plan-for (v2-state room1 room2 nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))

  )

;;; SHAKEY 3.0

;;; Test cases for N boxes with locks, going to the same room.
;;;
;;; The goal state that stops the recursion is that the list
;;; of box locations is nil:
;;;
;;;   (v3-state ?rloc nil ?gloc ?unlocked)


(define-test shakey-3  
  ;; Test already done case
  (assert-equal '(nil)
                (ask '(plan-for (v3-state ? nil ? ?)
                                ?actions)
                     '?actions))
  
  ;; Test with 1 box, all rooms locked, the robot in the hall
  (assert-equal '((cons (unlock room2)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (unlock room1)
                                  (cons (push-box hall room1) nil))))))
                (ask '(plan-for (v3-state hall (cons room2 nil) room1 nil)
                                ?actions)
                     '?actions))
  
  ;; Test with 2 boxes, all rooms locked, the robot in the hall
  (assert-equal '((cons (unlock room2)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (unlock room1)
                                    (cons (push-box hall room1) 
                                          (cons (move-to hall)
                                                (cons (unlock room3)
                                                      (cons (move-to room3)
                                                            (cons (push-box room3 hall)
                                                                  (cons (push-box hall room1)
                                                                        nil)))))))))))
                (ask '(plan-for (v3-state hall (cons room2 (cons room3 nil)) room1 nil)
                                ?actions)
                     '?actions))
  )
