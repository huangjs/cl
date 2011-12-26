;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          check.lisp
;;;; Purpose:       Argument And Type Checking Functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: check.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(defun check-func-spec (func-spec)
"Sample func-spec: '((1.0 1.0) (3.0 2.0) (4.0 0.5) (6.5 0.0)))."
  (let ((pairs (ensure-adj-as-float func-spec)))
    (let ((last (caar pairs)))
      (dolist (pair (cdr pairs))
        (let ((x (car pair)))
          (when (>= last x)
            (return (values nil (format nil "~s has an x coordinate out 
of sequential order~%" func-spec))))
          (setf last (car pair))))
      (dolist (pair pairs)
        (let ((y (cadr pair)))
          (unless (and (>= y 0.0)
                       (<= y 1.0))
            (return (values nil (format nil "~s has a y coordinate that is 
not in the range [0,1]."))))))
      pairs)))

(defun check-special-func (func-spec)
"Sample func-spec: '(tri 1 3 5)."
  (let ((op (car func-spec))
        (nums (cdr func-spec)))
    (unless (or (eql op 'tri)
                (eql op 'trap))
      (return-from check-special-func 
        (values nil (format nil "~s has an unrecognized operator, ~s~%" 
                            func-spec op))))
    (unless (every #'numberp nums)
      (return-from check-special-func 
        (values nil (format nil "~s has invalid numbers~%" func-spec))))
    t))


(defun check-rule (rule &key (fuzzy-sys *fuzzy-sys*))
  "Check a form that represents a rule."
  (labels 
      ((rec (tree)
         (cond ((null tree))
               ((simple-list? tree)
                (let ((var-sym (car tree))
                      (adj (gethash (symbol-name (car (last tree)))
                                    (adj-hash fuzzy-sys))))
                  (let ((var (gethash (symbol-name var-sym)
                                      (var-hash fuzzy-sys))))
                    (unless var
                      (error "deffuzzy: Variable, ~s, 
does not exist in the fuzzy system, ~s.~%" var-sym (fuzzy-name fuzzy-sys)))
                    (let ((adjs (group-adjs 
                                 (adj-group var))))
                      (unless (member adj adjs :test #'equal)
                        (error "deffuzzy: Rule, ~s, 
has a mismatched variable and adjective. In the statement, ~s, the 
adjective is not in the adjective group of the variable in the fuzzy 
system ~s.~%"  rule tree (fuzzy-name fuzzy-sys)))
                      (unless (eq (second tree) 'is)
                        (error "deffuzzy: Second argument in rule fragment,
~s, should be \"is\" of rule, ~s, in fuzzy system ~s.~%" 
                               tree rule (fuzzy-name fuzzy-sys)))
                      (when (= (length tree) 4)
                        (case (third tree)
                          (not t)
                          (very t)
                          (somewhat t)
                          (t
                           (error "deffuzzy: Unrecognized adverb, ~s, in 
rule fragment, ~s, in rule, ~s, in fuzzy system ~s.~%" 
                                  (third tree) tree rule 
                                  (fuzzy-name fuzzy-sys)))))))))
               (t
                (unless (valid-op? (car tree))
                  (error "deffuzzy: ~s is not a valid boolean 
operator in rule, ~s in fuzzy system ~s.~%" 
                         (car tree) rule (fuzzy-name fuzzy-sys)))
                (mapcar #'rec (cdr tree))))))
    (rec rule)))

(defun check-var-adj (adj-sym var &key (fuzzy-sys *fuzzy-sys*))
  "Check that an adjective is associated with a variable."
  (let ((adj (gethash (symbol-name adj-sym) (adj-hash fuzzy-sys)))
        (adj-group (adj-group var)))
    (unless (member adj (group-adjs adj-group) :test #'equal)
      (error "deffuzzy: Adjective ~s is not a member of the adjective 
group of the variable, ~s in the fuzzy system ~s.~%" 
             adj-sym (fuzzy-name var) (fuzzy-name fuzzy-sys)))))

