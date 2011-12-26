;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: rsm.modal.test -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modal-test.lisp
;;;; Purpose:       Regression Testing For Modal Logic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: modal-test.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.modal.test
  (:use #:cl rsm.modal #:ptester)
  (:documentation
   "Provides a test harness for modal logic systems.")
  )


(in-package rsm.modal.test)


;;;; SET UP THE MODAL LOGIC SYSTEM TO TEST.

;;; A modal logic system describing primitive propositions that are true 
;;; in certain worlds and a kripke structure describing worlds agents 
;;; think are possible.
(defmodal my-world 
    :kripke 
  ((0 1 (1 2))                          ; Agent 0 in world 1 knows that 
                                        ;  worlds 1 and 2 are possible.
   (0 2 (1 2))                          ; Agent 0 in world 2 knows that worlds 
                                        ;  1 and 2 are possible.
   (0 3 (3))                            ; Agent 0 in world 3 knows that worlds 
                                        ;  3 is possible.
   (1 1 (1 3))                          ; Agent 1 in world 1 knows that worlds 
                                        ;  1 and 3 are possible.
   (1 2 (2 1))                          ; Agent 1 in world 2 knows that worlds 
                                        ;  1 and 2 are possible.
   (1 3 (3 2)))                         ; Agent 1 in world 3 knows that worlds 
                                        ;  3 and 2 are possible.
  :props
  ((1 "sky is white" t)                 ; The sky is white in world 1.
   (2 "sky is white" t)                 ; The sky is white in world 2.
   (3 "light is on" t)))                ; The light is on in world 3.



;;;; RUN THE TESTS.

(defun run-modal-tests ()

  (with-tests (:name "Modal Logic Tests")
    
    ;; The statement is: Agent 0 in world 1 knows that
    ;; the sky is white AND that the sky is white.
    ;; Answer: true.
    (test t (agent-knows 0 1 '(and "sky is white" "sky is white"))
          :fail-info "Test 1")
    
    
    ;; The statement is: Agent 0 in world 1 knows that agent 1 knows that 
    ;; the sky is white and that t is true.
    ;; Answer: false.
    (test nil (agent-knows 0 1 '(k1 (and "sky is white" t)))
          :fail-info "Test 2")

    ;; The statement is: Agent 0 in world 1 knows that agent 0 knows that 
    ;; the sky is white and that t is true.
    ;; Answer: true.
    (test t (agent-knows 0 1 '(k0 (and "sky is white" t)))
          :fail-info "Test 3")
    
    ;; The statement is: Agent 0 in world 1 knows that agent 1 knows that 
    ;; the sky is white.
    ;; Answer: false.
    (test nil (agent-knows 0 1 '(k1 (and "sky is white" "sky is white")))
          :fail-info "Test 4")

    ;; The statement is: Agent 0 in world 1 knows that agent 0 knows that 
    ;; either the sky is white or t is true.
    ;; Answer: true.
    (test t (agent-knows 0 1 '(k0 (or "sky is white" t)))
          :fail-info "Test 5")
    
    
    ;; The statement, "sky is white", is not a statement that is true 
    ;; in all worlds.
    (test nil (satisfies? "sky is white")
          :fail-info "Test 6")
    
    ;; The statement is: In world 0, agent 0 knows that the sky is white 
    ;; and that t is true.
    ;; Answer: false. (Should be an error as world 0 does not exist.)
    (test nil (is-true? 0 '(k0 (and "sky is white" t)))
          :fail-info "Test 7")
    
    ;; The statement is: In world 1, agent 0 knows that the sky is white 
    ;; and that t is true.
    ;; Answer: true.
    (test t (is-true? 1 '(k0 (and "sky is white" t)))
          :fail-info "Test 8")
    
    ;; The statement is: In world 1, agent 0 knows that agent 1 considers 
    ;; it possible that the sky is white. 
    ;; Answer: true.
    (test t (is-true? 1 '(k0 (not (k1 (not "sky is white")))))
          :fail-info "Test 9")
    
    ;; Same thing using the possibility notation.
    (test t (is-true? 1 '(k0 (p1 "sky is white")))
          :fail-info "Test 10")
    
    ;; The statement is: In world 1, agent 0 knows that agent 1 does not 
    ;; know whether the sky is white or not.
    ;; Answer: false.
    (test nil (is-true? 1 '(k0 (and
                                   (not (k1 "sky is white"))
                                   (not (k1 (not "sky is white"))))))
          :fail-info "Test 11")
    
    ;; The same thing using the "don't know whether" notation.
    (test nil (is-true? 1 '(k0 (d1 "sky is white")))
          :fail-info "Test 12")

    
    ;; The statement is: In world 1, agent 1 does not know whether the 
    ;; sky is white or not.
    ;; Answer: true.
    (test t (is-true? 1 '(and 
                             (not (k1 "sky is white"))
                             (not (k1 (not "sky is white")))))
          :fail-info "Test 13")

    ;; The statement is: In world 2, agent 1 does not know whether the 
    ;; sky is white or not.
    ;; Answer: false.
    (test nil (is-true? 2 '(and 
                               (not (k1 "sky is white"))
                               (not (k1 (not "sky is white")))))
          :fail-info "Test 14")
    
    ;; The statement is: In world 2, agent 1 does not know that the 
    ;; sky is not white.
    ;; Answer: true.
    (test t (is-true? 2 '(not (k1 (not "sky is white"))))
          :fail-info "Test 15")
    )
  t
  )
