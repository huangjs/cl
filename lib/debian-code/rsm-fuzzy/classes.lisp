;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classes.lisp
;;;; Purpose:       Fuzzy Class Definitions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: classes.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;; A Named class.
(defclass named-fuzzy ()
  ((name :accessor fuzzy-name 
         :initform nil 
         :initarg :name)))


;;; Abstract Base classes used for Fuzzy Systems.
(defclass fuzzy-adj (named-fuzzy)
  ())

(defclass fuzzy-var (named-fuzzy)
  ())

(defclass fuzzy-rule (named-fuzzy)
  ())

(defclass fuzzy-adj-group (named-fuzzy)
  ())

(defclass fuzzy-system (named-fuzzy)
  ())


(defclass standard-fuzzy-system (fuzzy-system)
  ((var-hash :accessor var-hash 
             :initform (make-hash-table :test #'equal) 
             :initarg :var-hash)
   
   (adj-hash :accessor adj-hash 
             :initform (make-hash-table :test #'equal) 
             :initarg :adj-hash)
   
   (adj-group-hash :accessor adj-group-hash 
                   :initform (make-hash-table :test #'equal) 
                   :initarg :adj-group-hash))
  (:documentation
   "A Fuzzy System Class.
Contains all the adjectives, variables, and adjective groups for a 
fuzzy system."
   ))



(defclass standard-fuzzy-adj (fuzzy-adj) 
  ;; XY pairs that represent the piece-wise linear function defining
  ;; the fuzzy adjective.
  ((xy-pairs :accessor xy-pairs 
             :initform nil 
             :initarg :xy-pairs)
   
   ;; Beginning of function domain, determined from xy-pairs.
   (start :reader start 
          :initform nil 
          :initarg :start
          :type single-float)
   
   ;; End of function domain, determined from xy-pairs.
   (end :reader end 
        :initform nil 
        :initarg :end
        :type single-float))
  (:documentation
   "A Fuzzy Adjective Class.
 The Fuzzy adjective is essentially a named piece-wise linear function 
 with values between 0.0 and 1.0. This function is used with fuzzy 
 variables to get a \"measure\" of how much a fuzzy variable can be 
 thought of as belonging to an adjective.
 Example adjective: sym = 'small; xy-pairs = a list of x-y pairs that 
 determine the piecewise linear function.
 NOTE: 'start' and 'end' are determined by an after method on 
        initialize-instance.
 NOTE: The measure of a fuzzy variable whose numeric value is 
        after \"end\" or before \"start\" is zero."
   ))

(defclass standard-fuzzy-var (fuzzy-var) 
  ;; Value of fuzzy variable.
  ((value :accessor var-value 
          :initform nil 
          :initarg :value 
          :type single-float)
   
   ;; A fuzzy-adj-group 
   (adj-group :accessor adj-group 
              :initform nil 
              :initarg :adj-group)
   
   ;; A list of rules.
   (rules :accessor rules 
          :initform nil 
          :initarg :rules)
   
   ;; Rules hashed by name.
   (rule-hash :accessor rule-hash 
              :initform (make-hash-table :test #'equal) 
              :initarg :rule-hash ))
  
  (:documentation
   "A Fuzzy Variable Class.
 A named numeric value associated with a list of Fuzzy rules.
 The rules, in tern, describe relationships (which may conflict) with other 
 Fuzzy variables using Fuzzy adjectives.
 Example variable: sym = 'x; value = 2.1; rules = nil OR a list of rule 
 objects."
   ))

(defclass standard-fuzzy-rule (fuzzy-rule)   
  ;; The "if" part of rule.
  ((antecedent :accessor antecedent 
               :initform nil 
               :initarg :antecedent)
   
   ;; The "then" part of rule - a fuzzy-adj.
   (consequent :accessor consequent 
               :initform nil 
               :initarg :consequent 
               :type standard-fuzzy-adj)

   ;; The original rule as a form.
   (rule-form :accessor rule-form :initform nil
              :initarg :rule-form))
  (:documentation
   "A Fuzzy Rule Class.
 A Fuzzy rule is a named linguistic relation in the form of a logical 
 implication: antecedent -> consequent.
 Example Rule: sym = 'r1; antecedent = '(and (x is small) (y is blue)); 
               consequent = 'dry
               Rule 'r1 can only be interpreted in the context of a 
               Fuzzy variable.
               So, if 'z is a Fuzzy variable having 'r1 as a Fuzzy rule 
               then the interpretation is: 
               IF x is small AND y is blue THEN z is dry
               Given numeric values for x and y, Fuzzy arithmetic is 
               used with this rule to determine a numeric value of z."
))


(defclass standard-fuzzy-adj-group (fuzzy-adj-group)
  ((group-adjs :accessor group-adjs 
               :initform nil 
               :initarg :group-adjs))
  (:documentation
   "A Fuzzy Adjective Group Class."
   ))

