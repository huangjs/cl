;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          filter.lisp
;;;; Purpose:       Filter lists and trees.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: filter.lisp,v 1.4 2003/09/17 01:50:03 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.filter)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


(declaim (ftype (function (list function) list) filter))

(defun filter (ls pruner)
  "Return a new list formed from selecting only those elements of <list> that do
not satisfy <pruner>.  The order of the elements is preserved.
Example: (rsm.filter:filter '(1 2 3 4 5) #'evenp)
          (1 3 5)"
  (let ((que (rsm.queue:create)))
    (dolist (el ls (rsm.queue:nget-list que))
      (unless (funcall pruner el)
        (rsm.queue:enqueue el que)))))


(declaim (ftype (function (list predicate) list) prune-tree))

(defun prune-tree (tree pruner)
  "Returns a pruned version of <tree> where pruned elements satisfy the
predicate, <pruner>.
Example: (rsm.filter:prune-tree '(1 2 (3 4 (5) (6 7) 4) 2) #'oddp)
         (2 (4 (6) 4) 2)"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (rsm.queue:nget-list que))
               ((consp (car tree))
                (let ((c (rec (car tree) (rsm.queue:create))))
                  (when c
                    (rsm.queue:enqueue c que))
                  (rec (cdr tree) que)))
               (t
                (let ((c (car tree)))
                  (unless (funcall pruner c)
                    (rsm.queue:enqueue c que))
                  (rec (cdr tree) que))))))
    (rec tree (rsm.queue:create))))


(declaim (ftype (function (list) list) tree-sig))

(defun tree-sig (tree)
  "Returns the same tree as <tree> with the value t in every leaf.
Example: (rsm.filter:tree-sig '(1 2 (3 4) 6)
         (t t (t t) t)"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (rsm.queue:nget-list que))
               ((consp (car tree))
                (rsm.queue:enqueue (rec (car tree) (rsm.queue:create)) que)
                (rec (cdr tree) que))
               (t
                (rsm.queue:enqueue t que)
                (rec (cdr tree) que)))))
    (rec tree (rsm.queue:create))))


(declaim (ftype (function (predicate function) function) tree-hom))

(defun tree-hom (pruner transformer)
  "Returns a function which takes a tree and returns a pruned, transformed copy.
The tree will be pruned by <pruner> at the leafs and each leaf (that remains)
will be transformed by <transformer>.
Example: (setf *prune* (rsm.filter:tree-hom #'evenp #'(lambda (x) (+ x 10))))
         (funcall *prune* '(1 2 3 (3 4 5 (5 6 7) (7) 8 (9 10))))
   (11 13 (13 15 (15 17) (17) (19)))"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (rsm.queue:nget-list que))
               ((consp (car tree))
                (let ((c (rec (car tree) (rsm.queue:create))))
                  (when c
                    (rsm.queue:enqueue c que))
                  (rec (cdr tree) que)))
               (t
                (let ((c (car tree)))
                  (unless (funcall pruner c)
                    (rsm.queue:enqueue (funcall transformer c) que))
                  (rec (cdr tree) que))))))
    #'(lambda (tree)
        (rec tree (rsm.queue:create)))))


(declaim (ftype (function (list function) list) map-tree))

(defun map-tree (tree func)
  "Maps the function <func> over the leaves of tree <tree>.
Example: (rsm.filter:map-tree  '(1 2 (3 4 (5) 6 7) 8) #'1+)
         (2 3 (4 5 (6) 7 8) 9)"
  (labels 
      ((rec (tree que)
         (cond ((null tree) (rsm.queue:nget-list que))
               ((consp (car tree))
                (rsm.queue:enqueue (rec (car tree) (rsm.queue:create)) que)
                (rec (cdr tree) que))
               (t
                (rsm.queue:enqueue (funcall func (car tree)) que)
                (rec (cdr tree) que)))))
    (rec tree (rsm.queue:create))))


(declaim (ftype (function (list) list) flatten))

(defun flatten (tree)
  "Flattens a tree to a list.
 Example: (rsm.filter:flatten '(1 2 (3 4 (5) 6 7) 8))
          '(1 2 3 4 5 6 7 8)"
  (labels 
      ((rec (tree acc)
         (cond ((null tree) acc)
               ((atom tree) 
                (cons tree acc))
               (t
                (rec (car tree) (rec (cdr tree) acc))))))
    (rec tree nil)))


(declaim (ftype (function (list &key (:from-end t) (:test function)) list) 
                linearize))

(defun linearize (tree &key (from-end nil) (test #'eql))
  "Linearize a tree, removing duplicates (determined equal by <test>).  If
from-end is non null, then duplicate entries are removed from the end rather
than the beginning of the resulting list.
Example: (rsm.filter:linearize '(a b (c d (e f a) d c w q b)))
          (e f a d c w q b)
Example: (rsm.filter:linearize '(a b (c d (e f a) d c w q b)) :from-end t)
          (a b c d e f w q)"
  (delete-duplicates (flatten tree) :from-end from-end :test test))

