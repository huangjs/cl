;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          util.lisp
;;;; Purpose:       Utilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: util.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; UTILITY FUNCTIONS

(defun process-adj-function (func)
  "Turn certain predefined function descriptions into xy-pairs.
 Handles triangle and trapezoid functions."
  (case (car func)
    (tri
     (if (not (= (length func) 4))
         (error "deffuzzy: Bad triangle function.")
       (ensure-adj-as-float
        (list (list (second func) 0.0)
              (list (third func) 1.0)
              (list (fourth func) 0.0)))))
    (trap
     (if (not (= (length func) 5))
         (error "deffuzzy: Bad trapezoid function.")
       (ensure-adj-as-float
        (list (list (second func) 0.0)
              (list (third func) 1.0)
              (list (fourth func) 1.0)
              (list (fifth func) 0.0)))))
    (t (error "deffuzzy: Unknown piecewise function."))))



(defun fuz-and (&rest args)
  "Fuzzy and."
  (apply #'min args))

(defun fuz-or (&rest args)
  "Fuzzy or."
  (apply #'max args))

(defun fuz-xor (&rest args)
    "Fuzzy xor."
  (reduce #'(lambda (x y)
              (max (min x (- 1 y)) (min (- 1 x) y))) args))



(declaim (inline simple-list?))
(defun simple-list? (tree)
  "Is <tree> a simple list (meaning it has no subtrees)?"
  (every #'atom tree))

(declaim (inline valid-op?))
(defun valid-op? (op)
  "Is <op> a valid logic operation?"
  (or (eq 'and op) 
      (eq 'or op)
      (eq 'xor op)))


(defun rule->lisp (rule)
  "Transform the form representing a rule into Lisp code that can be
 used to compute the antecedent of the rule form."
  (let ((val (gensym)))
    (labels 
        ((rec (tree)
           (cond ((null tree) nil)                 
                 ((or (eq (car tree) 'and) 
                      (eq (car tree) 'or)
                      (eq (car tree) 'xor))
                  (cons (case (car tree)
                          (or 'fuz-or)
                          (and 'fuz-and)
                          (xor 'fuz-xor))
                        (mapcar #'rec (cdr tree))))
                 (t
                  (if (= (length tree) 3)
                      `(measure ,(get-var (car tree)) 
                                ,(get-adj (car (last tree))))
                    (case (third tree)
                      (not  `(- 1 (measure ,(get-var (car tree)) 
                                           ,(get-adj (car (last tree))))))
                      (very `(sqrt (measure ,(get-var (car tree)) 
                                            ,(get-adj (car (last tree))))))
                      (somewhat `(let ((,val
                                        (measure ,(get-var (car tree)) 
                                                 ,(get-adj (car (last tree))))))
                                   (* ,val ,val)))))))))
      `(lambda ()
         ,(rec rule)))))

(defun compile-rule (rule)
  "Compile the antecedent of a rule form."
  (compile nil (rule->lisp rule)))


(defun ensure-adj-as-float (adj-spec)
  "Ensure that the adjective specification values are converted 
 to single-float."
  (let (nadj-spec)
    (unless (listp adj-spec)
      (error "deffuzzy: Adjective specification, ~s, is not a valid list.~%" 
             adj-spec))
    (dolist (adj-pair adj-spec)
      (unless (listp adj-pair)
        (error "deffuzzy: Adjective pair, ~s, from adjective specification, ~s, 
is not a valid list.~%" adj-pair adj-spec))
      (unless (numberp (car adj-pair))
        (error "deffuzzy: Element ~s, of adjective pair, ~s, from 
adjective specification, ~s, is not a valid number.~%" 
               (car adj-pair) adj-pair adj-spec))
      (unless (numberp (cadr adj-pair))
        (error "deffuzzy: Element ~s, of adjective pair, ~s, from 
adjective specification, ~s, is not a valid number.~%" 
               (cadr adj-pair) adj-pair adj-spec))

      (push (list (coerce (car adj-pair) 'single-float) 
                  (coerce (cadr adj-pair) 'single-float)) nadj-spec))
    (nreverse nadj-spec)))

