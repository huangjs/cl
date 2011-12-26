;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cache.lisp
;;;; Purpose:       Cache objects of high numeric value.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: cache.lisp,v 1.4 2003/09/17 15:31:03 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package rsm.cache)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(defclass abstract-cache ()
  ()
  (:documentation "Abstract base class of a cache."))

(defclass standard-cache (abstract-cache)
  ((name :accessor name :initform "" :initarg :name)
   (cache-size :accessor cache-size :initform 0 
               :initarg :cache-size)
   (orig-cache-size :accessor orig-cache-size :initform 0 
                    :initarg :orig-cache-size)
   (cache-count :accessor cache-count :initform 0 
                :initarg :cache-count)
   (cache-filled :accessor cache-filled :initform nil 
                 :initarg :cache-filled)
   (min-value :accessor min-value :initform nil
              :initarg :min-value)
   (threshold :accessor threshold :initform nil
              :initarg :threshold)
   (value-obj-hash :accessor value-obj-hash :initform nil 
                   :initarg :value-obj-hash)
   (cache-list-limit :accessor cache-list-limit :initform nil
                     :initarg :cache-list-limit))
  (:documentation "Standard cache: stores a limited number of the 
best objects."))



;;;; External Protocol

(defgeneric cache-if-large (cache obj value &key test)
  (:documentation 
   "Place an object in the cache if either a or b below is true:

a). The internal cache threshold is nil.
b). The value of the object to cache is larger than the internal 
    cache threshold. 

AND 

if either 1, 2, or 3 below is true:

1). The cache is not full.
2). The cache is full AND the rank of the object is larger than the minimum rank
    of the cache.
3). The cache is full AND the rank of an object is the same as a list of objects
    in the cache AND the object is considered to be different than any of the
    elements in this list. The keyword <test> will be used to determine if an
    element is considered different. If the slot cache-list-limit of <cache> is
    specified, the lists of equal valued objects will be limited to length,
    cache-list-limit. Otherwise, the cache lists will have no limit."))
   


(defgeneric cache-min-value (cache)
  (:documentation 
   "Find the minimum of the values in the cache."))


(defgeneric clear-and-set-obj-cache-size (cache n)
  (:documentation
   "Clear the object <cache> and set the new size to <n>."))


(defgeneric retrieve-obj-cache (cache)
  (:documentation
   "Retrieve a list of the form, (value (list-of-objs)), from <cache> 
ordered from highest to lowest value. The list represents the \"k\" best 
solutions in the sense that each element of the list returned is a pairing 
of a fit value and a corresponding list of objects with that value."))




;;;; Protocol Implementation

(defun make-standard-cache (name cache-size 
                            &key (threshold nil) (cache-list-limit nil))
  "Create an instance of class standard-cache. The cache will have name, <name>;
and cache size, <cache-size>. If <threshold> is non-nil, then no value will 
be stored in the cache if less than <threshold>. If placed in the cache, 
an object will be added to a list of other objects of equal numerical rank.
If <cache-list-limit> is non-nil, limit the list length of equally ranked 
objects to <cache-list-limit>. Otherwise, there will be no limit the 
lists of equal rank."
  (make-instance 'standard-cache 
    :name name
    :cache-size cache-size
    :orig-cache-size cache-size
    :cache-count 0
    :cache-filled nil
    :threshold threshold
    :value-obj-hash (make-hash-table :size cache-size :test #'eql)
    :cache-list-limit cache-list-limit))


(defmethod initialize-instance :after ((cache standard-cache) &key)
  (let ((cache-size (cache-size cache))
        (threshold (threshold cache))
        (list-limit (cache-list-limit cache)))
    (unless (and (integerp cache-size) (> cache-size))
      (error "cache-size should be a positive integer."))
    (unless (or (null threshold)
                (and (integerp threshold) 
                     (> threshold)))
      (error "threshold should either be nil or a positive integer."))
    (unless (or (null list-limit)
                (and (integerp list-limit) 
                     (> list-limit)))
      (error "cache-list-limit should either be nil or a positive integer."))
    (when threshold
      (setf (min-value cache) threshold))))
  
  
(defmethod cache-if-large ((cache standard-cache) obj value 
                           &key (test #'equal))
  (with-slots (cache-size cache-count cache-filled cache-list-limit
               min-value threshold value-obj-hash) cache
    
    (when (or (null threshold) (> value threshold))
      (cond ((not cache-filled)
             (multiple-value-bind (lst exists?)
                 (gethash value value-obj-hash)
               (declare (ignore lst))
               (cond ((and exists?
                           (or (not cache-list-limit)
                               (> cache-list-limit 
                                  (length 
                                   (gethash value value-obj-hash)))))
                      (setf (gethash value value-obj-hash)
                        (pushnew obj (gethash value value-obj-hash) 
                                 :test test)))
                     ((and exists?
                           (or (not cache-list-limit)
                               (= cache-list-limit
                                  (length (gethash value value-obj-hash))))) t)
                     ((not exists?)
                      (incf cache-count)
                      (setf (gethash value value-obj-hash)
                        (list obj)))
                     (t t))
               (when (= cache-count cache-size)
                 (setf (cache-filled cache) t)
                 (setf (min-value cache) (cache-min-value cache)))))
            ((> value min-value)
             (let ((bump-out? nil))
               (multiple-value-bind (lst exists?)
                   (gethash value value-obj-hash)
                 (declare (ignore lst))
                 (when (not exists?)
                   (setf bump-out? t)
                   (remhash min-value value-obj-hash))
                 (cond ((and exists?
                             (or (not cache-list-limit)
                                 (> cache-list-limit 
                                    (length 
                                     (gethash value value-obj-hash)))))
                        (setf (gethash value value-obj-hash)
                          (pushnew obj 
                                   (gethash value value-obj-hash) :test test)))
                       ((and exists?
                             (or (not cache-list-limit)
                                 (= cache-list-limit
                                    (length (gethash value value-obj-hash))))) 
                        t)
                       ((not exists?)
                        (setf (gethash value value-obj-hash)
                          (list obj)))
                       (t t))
                 (when bump-out?
                   (setf (min-value cache) (cache-min-value cache))))))
            ((= value min-value)
             (if (or (not cache-list-limit)
                     (> cache-list-limit 
                        (length 
                         (gethash value value-obj-hash))))
                 (setf (gethash value value-obj-hash)
                   (pushnew obj (gethash value value-obj-hash)
                            :test test))))
            (t )))
    obj))


(defmethod cache-min-value ((cache standard-cache))
  (with-slots (value-obj-hash threshold) cache
    (let (val-list)
      (maphash #'(lambda (key val)
                   (declare (ignore val))
                   (when (numberp key) (push key val-list))) value-obj-hash)
      (setf val-list (sort val-list #'<))
      (or (car val-list) threshold))))


(defmethod reset-cache-params ((cache standard-cache))
  (with-slots (orig-cache-size threshold) cache
    (setf (cache-filled cache) nil)
    (setf (cache-count cache) 0)
    (setf (min-value cache) threshold)
    (setf (cache-size cache) orig-cache-size)))


(defmethod clear-and-set-obj-cache-size ((cache standard-cache) n)
  (reset-cache-params cache)
  (setf (cache-size cache) n)
  (setf (value-obj-hash cache) 
    (make-hash-table :size (cache-size cache)
                     :test #'eql)))

  
(defmethod retrieve-obj-cache ((cache standard-cache))
    (with-slots (value-obj-hash) cache
      (let ((solns (rsm.queue:create))
            s-vals)
        (maphash #'(lambda (key val)
                     (declare (ignore val))
                     (push key s-vals)) value-obj-hash)
        
        (setf s-vals (sort s-vals #'>))
        (loop :for val :in s-vals :do
          (rsm.queue:enqueue (list val (gethash val value-obj-hash)) solns))
        (rsm.queue:nget-list solns))))
