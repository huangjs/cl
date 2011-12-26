;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage :hjs.util.math
  (:use :cl :hjs.meta.macro :iterate :hjs.meta.essential)
  (:export #:expt-mod
	   #:extremum
	   #:extrema
	   #:n-most-extreme
	   #:pi-1000
	   #:round-to-digits
	   ))

(in-package #:hjs.util.math)

;;;;
;;;; turbo pi -- calculate pi to 1000 decimals >>>fast
;;;;
;;;;   It uses a spigot algorithm. More details in A spigot algorithm
;;;;   for the digits of pi, Stanley Rabinowitz and Stan Wagon,
;;;;   American Mathematical Monthly,March 1995, pp195-203.
(defun pi-1000 ()
  (let ((e 0)
	(f (make-array 3501 :initial-element 2000)))
    (loop for c from 3500 above 0 by 14 do
	 (let ((d 0))
	   (loop for b from c above 0 do
		(let ((g (1- (* 2 b))))
		  (setf d (+ (* d b) (* (aref f b) 10000)))
		  (setf (aref f b) (mod d g))
		  (setf d (floor d g))))
	   (multiple-value-bind (div mod) (floor d 10000)
	     (format t "~4,'0D" (+ e div))
	     (setf e mod)))))) 

(defun round-to-digits (num digits)
  (let ((frac (expt 10 (- digits))))
    (* (round num frac) frac)))