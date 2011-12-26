;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: conditions.lisp,v 1.1 2003/06/01 15:21:41 rtoy Exp $
;;;
;;; $Log: conditions.lisp,v $
;;; Revision 1.1  2003/06/01 15:21:41  rtoy
;;; Some conditions for matlisp matrix errors.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Error conditions for matlisp

(in-package "MATLISP")

(define-condition matlisp-error (error)
  ())

(define-condition vector-index-error (matlisp-error)
  ((index :reader vector-index-error-index :initarg :index)
   (matrix :reader matrix-index-error-matrix :initarg :matrix))
  (:documentation "An out of bounds index error")
  (:report (lambda (c stream)
	     (let ((m (matrix-index-error-matrix c)))
	     (format stream "~&Out of bounds index for a ~D x ~D vector:  ~D~%"
		     (nrows m) (ncols m)
		     (vector-index-error-index c))))))

(define-condition matrix-index-error (matlisp-error)
  ((row-index :reader matrix-index-error-row :initarg :row-index)
   (col-index :reader matrix-index-error-col :initarg :col-index)
   (matrix :reader matrix-index-error-matrix :initarg :matrix))
  (:documentation "An out of bounds index error")
  (:report (lambda (c stream)
	     (let ((m (matrix-index-error-matrix c)))
	     (format stream "~&Out of bounds index for a ~D x ~D matrix:  (~D, ~D)~%"
		     (nrows m) (ncols m)
		     (matrix-index-error-row c)
		     (matrix-index-error-col c))))))
  
