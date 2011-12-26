;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          introspect.lisp
;;;; Purpose:       Fuzzy System Introspection Facilities.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: introspect.lisp,v 1.2 2003/09/10 22:19:24 rscottmcintire Exp $
;;;; *************************************************************************

(in-package rsm.fuzzy)

(eval-when (:compile-toplevel)
  (declaim (optimize (speed 3) (debug 0) (safety 1) (space 0))))


;;;; INSPECTION

(defun clear-fuzzy-systems ()
  (clrhash *fuzzy-hash*))

(defmacro find-fuzzy-system (sym)
  `(gethash ,(symbol-name sym) *fuzzy-hash*))

(defmacro set-fuzzy-system (sym)
  `(setf *fuzzy-sys* (gethash ,(symbol-name sym) *fuzzy-hash*)))

(defmacro get-var-val (sym &key (fuzzy-sys *fuzzy-sys*))
  `(var-value (find-var ,sym :fuzzy-sys ,fuzzy-sys)))

(defmacro set-var-val (sym val &key (fuzzy-sys *fuzzy-sys*))
  `(setf (var-value (find-var ,sym :fuzzy-sys ,fuzzy-sys)) ,val))

(defmacro find-adj (sym &key (fuzzy-sys *fuzzy-sys*))
  `(gethash ,(symbol-name sym) ,(adj-hash fuzzy-sys)))

(defmacro find-var (sym &key (fuzzy-sys *fuzzy-sys*))
  `(gethash ,(symbol-name sym) ,(var-hash fuzzy-sys)))

(defmacro find-rule (rule-sym var-sym &key (fuzzy-sys *fuzzy-sys*))
  `(gethash ,(symbol-name rule-sym) 
            (rule-hash (find-var ,var-sym :fuzzy-sys ,fuzzy-sys))))

(defun find-adj-group (name &key (fuzzy-sys *fuzzy-sys*))
  (gethash name (adj-group-hash fuzzy-sys)))

(defun print-vars (&key (fuzzy-sys *fuzzy-sys*))
  "Print out all the fuzzy variables."
  (with-standard-io-syntax
    (maphash #'(lambda (sym var) 
                 (format t "~%var = ~a: " sym) 
                 (print-object var *standard-output*)) (var-hash fuzzy-sys)))
  (values))

(defun print-adjs (&key (fuzzy-sys *fuzzy-sys*))
  "Print out all the fuzzy adjectives."
  (with-standard-io-syntax
    (maphash #'(lambda (sym adj) 
                 (format t "~%adj = ~a: " sym) 
                 (print-object adj *standard-output*)) (adj-hash fuzzy-sys)))
  (values))


(defun get-var (sym &key (fuzzy-sys *fuzzy-sys*))
  "Associates a symbol with a fuzzy variable object."
  (gethash (symbol-name sym) (var-hash fuzzy-sys)))

(defun get-adj (sym &key (fuzzy-sys *fuzzy-sys*))
  "Associates a symbol with a fuzzy adjective object."
  (gethash (symbol-name sym) (adj-hash fuzzy-sys)))

(defun get-adj-group (sym &key (fuzzy-sys *fuzzy-sys*))
  "Associates a symbol with a fuzzy adjective object."
  (gethash (symbol-name sym) (adj-group-hash fuzzy-sys)))

