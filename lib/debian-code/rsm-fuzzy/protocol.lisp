;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol.lisp
;;;; Purpose:       Fuzzy Protocol.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: protocol.lisp,v 1.3 2003/09/17 02:13:34 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; PROTOCOL

;;;; External Protocol

(defgeneric fire-rules-get-value (var)
  (:documentation
   "Fires the rules of <var> and then uses the fuzzy centroid method to 
obtain the new value.
Note: Does not change the current value."
))

(defmacro fire (var-sym &key (fuzzy-sys *fuzzy-sys*))
  "Syntactic sugar for fire-rules-get-value.
Takes a variable (symbol) and returns the value."

  `(fire-rules-get-value ,(get-var var-sym :fuzzy-sys fuzzy-sys)))


;;;; Internal Protocol

(defgeneric fire-rule (rule)
  (:documentation
"Fire a fuzzy rule and return the area and moment."
))


(defgeneric antecedent-value (rule)
  (:documentation
   "Computes the value of the antecedent of a rule."))



(defgeneric cut-area (adj h)
  (:documentation
"Return the area of the fuzzy adjective <adj> \"cut\" at <h>."))


(defgeneric cut-moment (adj h)
  (:documentation
"Return the moment of fuzzy adjective <adj> \"cut\" at <h>."))


(defgeneric measure (var adj)
  (:documentation
"Get the measure of a variable (fuzzy or otherwise) in fuzzy adjective <adj>."))

  
;;;; PROTOCOL IMPLEMENTATION


;;;; External Protocol Implementation

;;; Fire the rules of <var> and then use the fuzzy centroid method to obtain 
;;; the new value. 
;;; Note: Does not change the current value.
(defmethod fire-rules-get-value ((var standard-fuzzy-var))
  (let ((total-area #.+zero+)
        (total-moment #.+zero+))
    (dolist (rule (rules var))
      (multiple-value-bind (area moment) (fire-rule rule)
        (incf total-area area)
        (incf total-moment moment)))
    (if (> (abs total-area) +epsilon+)
        (/ total-moment total-area)
      #.+zero+)))



;;;; Internal Protocol Implementation

(defmethod initialize-instance :after ((adj standard-fuzzy-adj) &key)
  "Set the start and end slots from the xy pairs that define the 
 fuzzy adjective (used only by the constructor)."
  (let ((pairs (xy-pairs adj)))
    (setf (slot-value adj 'start) (caar pairs))
    (setf (slot-value adj 'end) (caar (last pairs)))))

(defmethod (setf xy-pairs) :after ((adj standard-fuzzy-adj) pairs)
  "Set the start and end slots from the xy pairs that define the 
 fuzzy adjective."
  (setf (slot-value adj 'start) (caar pairs))
  (setf (slot-value adj 'end) (caar (last pairs))))


(defmethod print-object ((adj standard-fuzzy-adj) stream)
  "Print for fuzzy adjective."
  (with-standard-io-syntax
    (format stream "<~a,~a,~a,~a>~%" (fuzzy-name adj) 
            (start adj) (end adj) (xy-pairs adj))))

(defmethod print-object ((var standard-fuzzy-var) stream)
  "Print for fuzzy variable."
  (with-standard-io-syntax
    (format stream "<~a,~a>~%" (fuzzy-name var) (var-value var))))

(defmethod cut-area ((adj standard-fuzzy-adj) (h float))
  "Return the area of the fuzzy adjective <adj> \"cut\" by <h>.
 That is, the area of the function Min(f(),h()), where f() is the 
 function representing the fuzzy adjective and h() is the 
 constant function = h."
  (let* ((pairs (xy-pairs adj))
         (x-last (caar pairs))
         (y-last (cadar pairs)))
    (do* ((cursor (cdr pairs) (cdr cursor))
          (area #.+zero+)
          (x (caar cursor) (caar cursor))
          (y (cadar cursor) (cadar cursor)))
        ((null cursor) area)
      (cond ((and (<= y h) (<= y-last h))
             (incf area (* (+ y y-last) (- x x-last) #.+one-half+)))
            ((and (> y h) (> y-last h))
             (incf area (* h (- x x-last))))
            ((< y-last (- y +epsilon+))
             (let ((x* (+ x-last (/ (* (- h y-last) 
                                       (- x x-last)) (- y y-last)))))
               (incf area (+ (* (- x x*) h) 
                             (* (+ y-last h) (- x* x-last) #.+one-half+)))))
            ((< y (- y-last +epsilon+))
             (let ((x* (+ x-last (/ (* (- h y-last) 
                                       (- x x-last)) (- y y-last)))))
               (incf area (+ (* (- x* x-last) h) 
                             (* (+ y h) (- x x*) #.+one-half+)))))
            (t                          ; y is approx y-last which is approx h
             (incf area (* h (- x x-last)))))
      (setf x-last x)
      (setf y-last y))))


(defmethod cut-moment ((adj standard-fuzzy-adj) (h float))
  "Return the moment of fuzzy adjective <adj> \"cut\" at <h>.
 That is, the moment of the function Min(f(),h()), where f() is the 
 function representing the fuzzy adjective
 and h() is the constant function = h."
  (labels ((compute-linear-moment (m b x1 x2)
             (declare (single-float m b x1 x2))
             (- (* x2 x2 (+ (* m x2 #.+one-third+) (* b #.+one-half+)))
                (* x1 x1 (+ (* m x1 #.+one-third+) (* b #.+one-half+))))))
    (let* ((pairs (xy-pairs adj))
           (x-last (caar pairs))
           (y-last (cadar pairs)))
      (do* ((cursor (cdr pairs) (cdr cursor))
            (moment #.+zero+)
            (x (caar cursor) (caar cursor))
            (y (cadar cursor) (cadar cursor)))
          ((null cursor) moment)
        (let ((m (/ (- y y-last) (- x x-last)))
              (b (- y-last (* x-last (/ (- y y-last) (- x x-last))))))
          (cond ((and (<= y h) (<= y-last h))
                 (incf moment (compute-linear-moment m b x-last x)))
                ((and (> y h) (> y-last h))
                 (incf moment (* h (- x x-last) (+ x x-last) 0.5)))
                ((< y-last (- y +epsilon+))
                 (let ((x* (+ x-last (/ (* (- h y-last) 
                                           (- x x-last)) (- y y-last)))))
                   (incf moment (compute-linear-moment m b x-last x*))
                   (incf moment (* h (- x x*) (+ x x*) #.+one-half+))))
                ((< y (- y-last +epsilon+))
                 (let ((x* (+ x-last (/ (* (- h y-last) 
                                           (- x x-last)) (- y y-last)))))
                   (incf moment (compute-linear-moment m b x x*))
                   (incf moment (* h (- x* x-last) 
                                   (+ x* x-last) #.+one-half+))))
                (t                      ; y is approx y-last is approx h
                 (incf moment (* h (- x x-last) (+ x x-last) #.+one-half+))))
          (setf x-last x)
          (setf y-last y))))))


(defmethod measure ((value float) (adj standard-fuzzy-adj))
  "Get the degree to which a fuzzy variable with value <value> belongs 
 to the fuzzy adjective <adj>."
  (let* ((pairs (xy-pairs adj))
         (x-last (caar pairs))
         (y-last (cadar pairs)))
    (do* ((cursor (cdr pairs) (cdr cursor))
          (mes #.+zero+)
          (x (caar cursor) (caar cursor))
          (y (cadar cursor) (cadar cursor)))
        ((null cursor) mes)
      (when (and (>= value x-last) (< value x))
        (return-from measure (+ y-last (/ (* (- y y-last) 
                                             (- value x-last))
                                          (- x x-last)))))
      (setf x-last x)
      (setf y-last y))))


(defmethod measure ((var standard-fuzzy-var) (adj standard-fuzzy-adj))
  "Get the degree to which a fuzzy variable <var> belongs to the 
  fuzzy adjective <adj>."
  (let ((value (var-value var))
        (start (start adj))
        (end (end adj)))
    (if (or (<= value start) (>= value end))
        #.+zero+
      (measure value adj))))


(defmethod fire-rule ((rule standard-fuzzy-rule))
  "Fire a fuzzy rule and return the area and moment."
  (let ((cut-value (funcall (antecedent rule)))
        (consequent (consequent rule)))
    (if (< cut-value +epsilon+)
        (values 0.0 0.0)
      (values (cut-area consequent cut-value)
              (cut-moment consequent cut-value)))))
