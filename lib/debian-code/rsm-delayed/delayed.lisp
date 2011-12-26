;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delayed.lisp
;;;; Purpose:       Manipulate delayed lists.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: delayed.lisp,v 1.2 2003/08/21 19:57:11 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package rsm.delayed)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(defmacro cons (first &body body)
  "A delayed \"cons\"."
  `(cl:cons ,first #'(lambda () ,@body)))

(defmacro list (&rest elems)
  "A delayed \"list\"."
  `(when ',elems
     (reduce #'(lambda (x y) (cons x y)) ',elems
             :initial-value nil :from-end t)))


(defun delayed-p (stream)
  "Predicate that determines if <stream> is a delayed list."
  (and (consp stream)
       (functionp (cl:cdr stream))))

(deftype delayed ()
  "Type for a delayed list."
  '(satisfies delayed-p))


(defun car (stream)
  "The \"car\" for a delayed list; it is identical to cl:car when <stream> is an
ordinary list."
  (cl:car stream))

(defun cdr (stream)
  "The \"cdr\" for a delayed list; it is identical to cl:cdr when <stream> is an
ordinary list."
  (if (delayed-p stream)
      (funcall (cl:cdr stream))
    (cl:cdr stream)))


(defun nth (n stream)
  "Nth for delayed lists. Works when <stream> is an ordinary list."
  (cond ((delayed-p stream)
         (cond ((= n 0)
                (car stream))
               ((> n 0)
                (let ((st stream))
                  (loop
                    :repeat n :do
                    (setf st (cdr st))
                    (when (null st)
                        (return-from nth nil)))
                  (car st)))
               (t
                (error "delayed:nth: invalid first argument, ~s~%" n))))
        ((listp stream)
         (if (and (integerp n)
                  (>= n 0))
             (cl:nth n stream)
           (error "delayed:nth: invalid first argument, ~s~%" n)))
        (t
         (error "delayed:nth: Bad arguments."))))


(defun take (n stream)
  "Take the first <n> elements from <stream>, returning them as a list.  Works
when <stream> is an ordinary list."
  (if (not (delayed-p stream))
      (let ((st stream)
            (result (rsm.queue:create)))
        (when (> n 0)
          (loop
            :repeat n :do
            (rsm.queue:enqueue (cl:car st) result)
            (when (null (cl:cdr st))
              (return-from take (rsm.queue:nget-list result)))
            (setf st (cl:cdr st)))
          (rsm.queue:nget-list result)))
    (do ((cur stream (cdr cur))
         (count 0 (1+ count))
         (result (rsm.queue:create)))
        ((= count n) (rsm.queue:nget-list result))
      (if (null cur)
          (return-from take (rsm.queue:nget-list result))
        (rsm.queue:enqueue (car cur) result)))))

(defun drop (n stream)
  "Drop the first <n> elements from <stream>, return the resulting (possibly
delayed) list. Will work when <stream> is an ordinary lists as well.  In this
case the result is an ordinary list."
  (if (not (delayed-p stream))
      (let ((st stream))
        (when (> n 0)
          (loop
            :repeat n :do
            (when (not (cdr st))
              (return-from drop nil))
            (setf st (cdr st)))
          st))
    (let ((cur stream))
      (loop :repeat n :do
        (setf cur (cdr cur))
        (when (null cur)
          (return-from drop nil)))
      cur)))


(defun repeat (x)
  "Repeat x indefinitely."
  (cons x (repeat x)))

(defun cycle (list)
  "Repeat the list, <list>, indefinitely."
  (reduce #'(lambda (x y)
              (cons x y)) (butlast list)
              :from-end t :initial-value (cons (cl:car (last list))
                                               (cycle list))))


(defun zip-with (s1 s2 &optional (zip-func #'cl:cons))
  "A Lisp version of Haskell's zipWith function. Will work with mixtures of
ordinary lists and delayed lists."
  (cond ((or (null s1) (null s2))
         nil)
        ((and (delayed-p s1)
              (delayed-p s2))
         (let ((e1 (car s1))
               (e2 (car s2)))
           (cons (funcall zip-func e1 e2)
                 (zip-with (cdr s1) (cdr s2) zip-func))))
        ((and (listp s1)
              (delayed-p s2))
         (do ((cur1 s1 (cl:cdr cur1))
              (cur2 s2 (cdr cur2))
              (result (rsm.queue:create)))
             ((or (null cur1) (null cur2)) (rsm.queue:nget-list result))
           (rsm.queue:enqueue (funcall zip-func (cl:car cur1) (car cur2)) 
                              result)))
        ((and (listp s2)
              (delayed-p s1))
         (do ((cur1 s1 (cdr cur1))
              (cur2 s2 (cl:cdr cur2))
              (result (rsm.queue:create)))
             ((or (null cur2) (null cur1)) (rsm.queue:nget-list result))
           (rsm.queue:enqueue (funcall zip-func (car cur1) (cl:car cur2)) 
                              result)))
        ((and (listp s1)
              (listp s2))
         (do ((cur1 s1 (cl:cdr cur1))
              (cur2 s2 (cl:cdr cur2))
              (result (rsm.queue:create)))
             ((or (null cur1) (null cur2)) (rsm.queue:nget-list result))
           (rsm.queue:enqueue (funcall zip-func (cl:car cur1) (cl:car cur2)) 
                              result)))
        (t
         (error "delayed:zip-with: One or more arguments are either not a list 
or not a delayed list."))))



(defun mapcar (func &rest streams)
  "Mapcar for delayed lists. However, will work with a mixture of delayed lists
and ordinary lists."
  (if (some #'null streams)
      nil
    (cond ((every #'delayed-p streams)
           (let ((args (rsm.queue:create)))
             (dolist (stream streams)
               (rsm.queue:enqueue (car stream) args))
             (cons (apply func (rsm.queue:nget-list args))
                   (apply #'mapcar func (cl:mapcar #'cdr streams)))))
          ((and (every #'listp streams)
                (some #'(lambda (stream)
                          (not (delayed-p stream))) streams))
           (let ((args (rsm.queue:create)))
             (dolist (stream streams)
               (if (null stream)
                   (return-from mapcar nil)
                 (rsm.queue:enqueue (car stream) args)))
             (cl:cons (apply func (rsm.queue:nget-list args))
                      (apply #'mapcar func (cl:mapcar #'cdr streams)))))
          ((every #'listp streams)
           (apply #'cl:mapcar func streams))
          (t
           (error "delayed:mapcar: One or more of the arguments is not a list 
or not a delayed list. Check the order of the arguments.")))))
  

(defun filter (stream pruner)
  "Filter a stream (or an ordinary list) by excluding elements that satisfy
<pruner>. If <stream> is an ordinary list an ordinary list is returned."
  (if (delayed-p stream)
      (let ((c (car stream)))
        (if (funcall pruner c)
            (filter (cdr stream) pruner)
          (cons c (filter (cdr stream) pruner))))
    (rsm.filter:filter stream pruner)))

