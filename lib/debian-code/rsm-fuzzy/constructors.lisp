;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          constructors.lisp
;;;; Purpose:       Constructors For Fuzzy Systems.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: constructors.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; CONSTRUCTORS

(defun make-fuzzy-system (sym adjs adj-groups vars)
  "Make a fuzzy-system associating it with <sym>."
  
  ;; Make a fuzzy system.
  (setf (gethash (symbol-name sym) *fuzzy-hash*)
    (make-instance 'standard-fuzzy-system :name (symbol-name sym)))
  (setf *fuzzy-sys* (gethash (symbol-name sym) *fuzzy-hash*))
  
  ;; Make the adjectives.
  (mapc #'(lambda (adj-descr)
            (apply #'make-adj adj-descr)) adjs)
  
  ;; Make the adjective groups.
  (mapc #'(lambda (adj-group-descr)
            (make-adj-group adj-group-descr)) adj-groups)
  
  ;; Make the variables.
  (mapc #'(lambda (var-descr)
            (apply #'make-var var-descr)) vars)
  
  ;; The newly created fuzzy system.
  *fuzzy-sys*)



(defmacro deffuzzy (sym &key adjs adj-groups vars)
  "Make a fuzzy system. Set the global variable *fuzzy-sys* to the newly
 created system. All queries default to this system.
 Returns a new fuzzy-system object."
  `(let ((last-fuzzy-sys *fuzzy-sys*)
         (success-flag nil))
     (unwind-protect
         (progn
           (setf (gethash ,(symbol-name sym) *fuzzy-hash*)
             (make-instance 'standard-fuzzy-system :name ,(symbol-name sym)))
           (setf *fuzzy-sys* (gethash ,(symbol-name sym) *fuzzy-hash*))
           (let (adj-syms adj-group-syms var-syms)
             (setf adj-syms (mapcar #'(lambda (adj) (car adj)) ',adjs))
             (unless (= (length adj-syms) 
                        (length (delete-duplicates adj-syms)))
               (error "deffuzzy: There is at least one duplicate adjective for 
the fuzzy system ~s.~%" ,(symbol-name sym)))
             (setf adj-group-syms 
               (mapcar #'(lambda (adj-group) (car adj-group)) ',adj-groups))
             (unless (= (length adj-group-syms) 
                        (length (delete-duplicates adj-group-syms)))
               (error "deffuzzy: There is at least one duplicate adjective 
group for the fuzzy system ~s.~%" ,(symbol-name sym)))
             
             (setf var-syms (mapcar #'(lambda (var) (car var)) 
                                    ',vars))
             (unless (= (length var-syms) 
                        (length (delete-duplicates var-syms)))
               (error "deffuzzy: There is at least one duplicate variable for 
the fuzzy system ~s.~%" ,(symbol-name sym)))
             
             (mapc #'(lambda (adj-descr)
                       (apply #'make-adj adj-descr)) ',adjs)
             (mapc #'(lambda (adj-group-descr)
                       (make-adj-group adj-group-descr)) ',adj-groups)
             (mapc #'(lambda (var-descr)
                       (apply #'make-var var-descr)) ',vars))
           (setf success-flag t)
           *fuzzy-sys*)
       (progn
         (unless success-flag
           (setf *fuzzy-sys* last-fuzzy-sys))))))


(defun make-adj (sym func-spec &key (fuzzy-sys *fuzzy-sys*))
  "Make a fuzzy adjective.
Example: (make-adj 'small '((1.0 1.0) (3.0 2.0) (4.0 0.5) (6.5 0.0)))"
  (let (pairs
        (name (symbol-name sym))
        (adj-hash (adj-hash fuzzy-sys)))
    (if (symbolp (car func-spec))
        (multiple-value-bind (ok error)
            (check-special-func func-spec)
          (unless ok
            (error "deffuzzy: Adjective ~s has an error in its specification: 
~s~%" 
                   sym error))
          (setf pairs (process-adj-function func-spec)))
      (multiple-value-bind (chk-pairs error)
          (check-func-spec func-spec)
        (unless chk-pairs
          (error "deffuzzy: Adjective ~s has an error in its specification: 
~s~%" 
                 sym error))
        (setf pairs chk-pairs)))
    (setf (gethash name adj-hash) 
      (make-instance 'standard-fuzzy-adj :name name :xy-pairs pairs))))

(defun make-var (sym value &key adj-group rules (fuzzy-sys *fuzzy-sys*))
  "Make a fuzzy variable.
Example: (make-var 'z 1.2 :adj-group 'humidity
                     :rules '((r1 (and (x is small) (y is blue)) dry) 
                             (r2 (and (x is large) (y is green)) wet)))"
  (let ((name (symbol-name sym))
        (var-hash (var-hash fuzzy-sys))
        var
        (adj-group (find-adj-group (symbol-name adj-group)
                                   :fuzzy-sys fuzzy-sys)))
    (let ((rule-name-syms (mapcar #'(lambda (rule) (car rule)) rules)))
      (unless (= (length rule-name-syms) 
                 (length (delete-duplicates rule-name-syms)))
        (error "deffuzzy: There is at least one duplicate rule for 
fuzzy variable, ~s, in fuzzy system ~s.~%" sym (fuzzy-name fuzzy-sys)))
      (setf var
        (make-instance 'standard-fuzzy-var 
          :name name 
          :value (coerce value 'single-float)
          :adj-group adj-group))
      (setf (rules var) 
        (mapcar #'(lambda (rule) 
                    (make-rule rule var :fuzzy-sys fuzzy-sys)) 
                rules))
      (dolist (rule (rules var))
        (setf (gethash (fuzzy-name rule) (rule-hash var)) rule))
      (setf (gethash name var-hash) var))))


(defun make-rule (rule var &key (fuzzy-sys *fuzzy-sys*))
  "Make a fuzzy rule.
Example: (make-rule '(r1 (and (x is small) (y is blue)) dry))"
  (let ((adj-hash (adj-hash fuzzy-sys)))
    (check-rule (second rule) :fuzzy-sys fuzzy-sys)
    (check-var-adj (third rule) var :fuzzy-sys fuzzy-sys)
    (make-instance 'standard-fuzzy-rule 
      :name (symbol-name (first rule))
      :rule-form rule
      :antecedent (compile-rule (second rule))
      :consequent (gethash (symbol-name (third rule)) adj-hash))))


(defun make-adj-group (group &key (fuzzy-sys *fuzzy-sys*))
  "Make a fuzzy adjective group."
  (let ((adj-group-hash (adj-group-hash fuzzy-sys))
        (name (symbol-name (first group))))
    (unless (listp group)
      (error "deffuzzy: adjective group ~s is not a valid list.~%" (car group)))
    (unless (listp (cadr group))
      (error "deffuzzy: adjective group ~s does not have a valid list 
of adjectives.~%"
             (car group)))
    (let ((adj-group
           (make-instance 'standard-fuzzy-adj-group
             :name name
             :group-adjs 
             (mapcar #'(lambda (adj-sym) 
                         (let ((adj (get-adj adj-sym)))
                           (unless adj
                             (error "deffuzzy: adjective group ~s refers to 
an adjective, ~s, that does not exist in fuzzy system, ~s.~%" 
                                          name adj-sym (fuzzy-name fuzzy-sys)))
                                 adj))
                           (cadr group)))))
      (setf (gethash name adj-group-hash) adj-group))))

