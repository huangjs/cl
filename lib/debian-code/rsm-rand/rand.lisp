;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          rand.lisp
;;;; Purpose:       Discrete Random Number Generator.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: rand.lisp,v 1.6 2003/10/20 02:26:33 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.rand)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(defconstant +epsilon+ 1.0s-3)

(defclass abstract-randgen ()
  ()
  (:documentation "Base class for random generator objects.")
  )

(defclass standard-randgen (abstract-randgen)
  ((val-dens :accessor rand-val-dens 
             :initarg :val-dens)
   (density :accessor rand-dens 
            :initarg :density)
   (values :accessor rand-vals 
           :initarg :values)
   (distribution :reader rand-dist))
                 
  (:documentation "The standard randgen class."))


;;; External Protocol

(defgeneric clone (rand)
  (:documentation "Clone a randgen object <rand>."))

(defgeneric (setf rand-val-dens) (val-dens rand)
  (:documentation "Sets the probability density of random generator <rand>."))


(defgeneric next-rand (rand &optional n)
  (:documentation "Get the next <n> random numbers from <rand>."))

(defgeneric bin-rand (rand &optional num-of-trials)
  (:documentation 
   "Create an ASCII display of bining <num-of-trials> random numbers from the
random generator object, <rand>."))

(defgeneric rand-dist (rand)
  (:documentation 
   "Gets the probability distribution of the random number generator object as a
vector."))

(defgeneric rand-val-dens (rand)
  (:documentation 
   "Gets the values and probability density of the random number generator
object in the form ((val dens) ...)."))

;;; Implementation of Protocol.

(defmethod clone ((rand standard-randgen))
  "Clone the random number generator, <rand>."
  (make-standard-randgen (rand-val-dens rand)))

(defmethod sync-dist ((rand standard-randgen))
  "Form and set internally the distribution of the random number generator,
<rand> based on the probability density."
  (let* ((len (length (rand-dens rand)))
         (dist (make-array len)))
    (loop 
        :with sum = 0.0 
        :for i :from 0 :below len :do
          (incf sum (svref (rand-dens rand) i))
          (setf (svref dist i) sum))
    (setf (slot-value rand 'distribution) dist)))


(defmethod initialize-instance :after ((rand standard-randgen) &key)
  "Sync the random distribution with the random density of the random generator,
<rand>."
  (sync-dist rand))

(defmethod (setf rand-val-dens) (val-dens (rand standard-randgen))
  "Set the probability density function for random generator, <rand>. 
val-dens has the form: '((val prob) (val prob)...)"
  (let ((d-vs (remove-duplicates val-dens :key #'car)))
    (let ((vals (mapcar #'(lambda (d-v) (car d-v)) d-vs))
          (dens (mapcar #'(lambda (d-v) (cadr d-v)) d-vs)))
      (if (> (abs (- (reduce #'+ dens) 1.0)) +epsilon+)
          (error "make-standard-randgen: 
Probability densities do not add up to 1.0.
They add up to ~s~%" (reduce #'+ dens))
        (prog1
          (setf (slot-value rand 'val-dens) d-vs)
          (setf (slot-value rand 'density) 
            (make-array (length dens) :initial-contents dens))
          (setf (slot-value rand 'values) 
            (make-array (length vals) :initial-contents vals))
          (sync-dist rand))))))


(defun make-standard-randgen (val-dens)
  "Makes a standard random generator object.
Example: (rsm.rand:make-standard-randgen '((1 0.2) (2 0.25) (3 0.25) (4 0.3)))
          makes a random generator object that has values: 1,2,3,4 with
          corresponding probability density values: 0.2, 0.25, 0.25, 0.3."
  (let ((d-vs (remove-duplicates val-dens :key #'car)))
    (let ((vals (mapcar #'(lambda (d-v) (car d-v)) d-vs))
          (dens (mapcar #'(lambda (d-v) (cadr d-v)) d-vs)))
      (if (> (abs (- (reduce #'+ dens) 1.0)) +epsilon+)
          (error "make-standard-randgen: 
Probability densities do not add up to 1.0
They add up to ~s~%" (reduce #'+ dens))
        (make-instance 'standard-randgen 
                       :val-dens d-vs
                       :density (make-array (length dens) 
                                            :initial-contents dens)
                       :values (make-array (length vals) 
                                           :initial-contents vals))))))


(defmethod next-rand ((rand standard-randgen) &optional (n 1))
  "Return the next <n> \"random\" numbers from <rand>."
  (if (= n 1)
      (svref (rand-vals rand) (%bin-search (rand-dist rand) (random 1.0)))
    (loop 
      :for i :from 1 :to n 
      :collect (svref (rand-vals rand) 
                      (%bin-search (rand-dist rand) (random 1.0))))))

(defmethod bin-rand ((rand standard-randgen) &optional (num-of-trials 1000))
  "Form an ASCII display that \"bins\" outcomes of <num-of-trials> generated
random values from <rand>. Can be used as a check of <rand>."
    (let ((hash (make-hash-table :test #'equal)))
      (declare (dynamic-extent hash))
      (with-standard-io-syntax
        (loop 
            :with v = nil
            :repeat num-of-trials :do
              (progn
                (setf v (next-rand rand))
                (loop 
                    :for val :across (rand-vals rand) 
                    :when (eql val v) :do
                      (progn
                        (if (gethash v hash)
                            (incf (gethash v hash))
                          (setf (gethash v hash) 1))
                        (return nil)))))
        (format t "~&VALUE    OCCURS   Expected  (total = ~s)" num-of-trials)
        (let ((key-vals 
               (loop 
                   :for key :being :the hash-keys :of hash 
                   :using (hash-value val) 
                   :collect (cons key val))))
          (dolist (key-val (sort key-vals #'< :key #'car))
            (format t "~&  ~s      ~s     ~s" (car key-val) 
                    (cdr key-val)
                    (* num-of-trials 
                       (cadr (assoc (car key-val) (rand-val-dens rand)))))))
        (values))))


;; if val is less than vec[mid]
;; search in [mid,n1]
;; else search in [n0,mid]
(defun %bin-search (vec val)
  (let ((len (length vec)))
    (if (= len 1)
        0
      (let ((n0 0)
            (n1 (1- len))
            mid)
        (loop 
          :repeat len :do
          (when (= (1+ n0) n1)
            (return
              (if (<= val (svref vec n0))
                  n0
                n1)))
          (setf mid (truncate (+ n0 n1) 2))
          (if (<= val (svref vec mid))
              (setf n1 mid)
            (setf n0 mid)))))))

