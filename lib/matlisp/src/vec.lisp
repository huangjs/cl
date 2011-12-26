;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copyright (c) 2000 The Regents of the University of California.
;;; All rights reserved. 
;;; 
;;; Permission is hereby granted, without written agreement and without
;;; license or royalty fees, to use, copy, modify, and distribute this
;;; software and its documentation for any purpose, provided that the
;;; above copyright notice and the following two paragraphs appear in all
;;; copies of this software.
;;; 
;;; IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
;;; FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
;;; ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
;;; THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
;;; PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
;;; CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
;;; ENHANCEMENTS, OR MODIFICATIONS.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley,
;;; 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: vec.lisp,v 1.3 2000/07/11 18:02:03 simsek Exp $
;;;
;;; $Log: vec.lisp,v $
;;; Revision 1.3  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.2  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (export '(vec))


(defun vec (start step &optional end)
 "
 Syntax
 ======
 (VEC start [step] end)

 Purpose
 =======
 Creates a row vector containing the sequence

  START, START+STEP, ..., START+(N-1)*STEP

 where       | END-START |
        N =  | --------- | + 1
             |   STEP    |
             --         --
 
 The representations of START,STEP,END are
 assumed to be exact (i.e. the arguments
 are rationalized.

 The type of the elements in the 
 sequence are of the same type as STEP.

 The optional argument STEP defaults to 1. 
"
  (if (not end)
      (progn
	(setq end step)
	(setq step 1)))

  (let* ((start (rationalize start))
	 (type (type-of step))
	 (step (rationalize step))
	 (end (rationalize end))
	 (seq nil))
    (declare (ignore type))

    (if (zerop step)
	(error "STEP equal to 0")) 

    (let* ((size (1+ (floor (/ (- end start) step))))
	   (result (if (> size 0)
		       (make-real-matrix-dim 1  size)
		     nil #| (make-real-matrix) |#)))

      (do ((x start (+ x step))
	   (i 0 (1+ i)))
	  ((if (>= step 0)
	       (> x end)
	     (< x end)) seq)
	(setf (matrix-ref result i) x))
      result)))
  
