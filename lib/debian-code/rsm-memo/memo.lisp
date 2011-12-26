;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          memo.lisp
;;;; Purpose:       Memoize functions.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: memo.lisp,v 1.2 2003/08/26 03:22:51 kevinrosenberg Exp $
;;;; *************************************************************************

(in-package rsm.memo)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))



(defun memoize (fn &key (test #'eql))
  "Return a memoized function."
  (let ((hash (make-hash-table :test test)))
    #'(lambda (x)
        (multiple-value-bind (val ok)
            (gethash x hash)
          (if ok
            val
            (let ((val (funcall fn x)))
              (setf (gethash x hash) val)
              val))))))

(defun memoize-cache (cache-size fn &key (test #'eql))
  "Return a memoized function with a storage limit."
  (let ((hash (make-hash-table :size cache-size))
        (count-hash (make-hash-table :size cache-size :test test))
        (cache-count 0))
    #'(lambda (x)
        (multiple-value-bind (val ok) (gethash x hash)
          (if ok
            val
            (let ((val (funcall fn x)))
              (let ((stale-count (- cache-count cache-size)))
                (when (= stale-count 0)
                  (remhash (gethash stale-count count-hash) hash)
                  (remhash stale-count count-hash))
                (incf cache-count)
                (setf (gethash x hash) val)
                (setf (gethash cache-count count-hash) x)
                val)))))))

(defmacro defmemo (fn (arg) body &key (test #'eql))
  "Define a function of one arg which memoizes its values. <arg> is stored 
in a hash table using test <test>."
  `(progn
    (when (not (symbolp ',fn))
      (error "defmemo: First argument is not a symbol."))
    (defun ,fn (,arg)
      (declare (notinline ,fn))
      ,body)
    (setf (symbol-function ',fn) (memoize (symbol-function ',fn) :test ,test))
    ',fn))


(defmacro defcache (fn (arg) body &key (cache-size 100) (test #'eql))
  "Define a function of one arg which will memoize the last
<cache-size> values. <arg> is stored in a hash table using test <test>."
  `(progn
    (when (not (symbolp ',fn))
      (error "defcache: First argument is not a symbol."))
    (when (not (and (numberp ,cache-size)
                    (> ,cache-size 0)
                    (<= ,cache-size most-positive-fixnum)))
      (error "defcache: Key word <cache-size> is either not a positive 
number or is larger than the largest fixnum."))
    (defun ,fn (,arg)
      (declare (notlinline ,fn)) ; Don't let compiler optimize tail calls.
      ,body)
    (setf (symbol-function ',fn) 
      (memoize-cache ,cache-size (symbol-function ',fn) :test ,test))
    ',fn))

