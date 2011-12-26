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
;;; $Id: reader.lisp,v 1.5 2005/01/27 19:43:39 rtoy Exp $
;;;
;;; $Log: reader.lisp,v $
;;; Revision 1.5  2005/01/27 19:43:39  rtoy
;;; [ ] wasn't working with a Jan cmucl snapshot.  We don't want to signal
;;; errors when READ-FROM-STRING is at end-of-file in
;;; PARSE-MATRIX-EXPRESSION-1.
;;;
;;; Revision 1.4  2001/02/21 19:36:47  simsek
;;; o Updated to work with Allegro 6.0
;;;
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

(defmacro %push-on-end% (value location)
  `(setf ,location (nconc ,location (list ,value))))

(defmacro %cat% (s1 s2)
  `(setq ,s1 (concatenate 'string ,s1  ,s2)))

(defmacro %ccat% (s c)
  `(%cat% ,s (string ,c)))

(defun null-string-p (s)
  (if (= (length s) 0)
      t
    (if (= (length 
	    (remove #\return
		    (remove #\^m 
			    (remove #\space 
				    (remove #\tab
					    (remove #\linefeed s)))))) 0)
	t
      nil)))

(defun peek-ahead-no-hang (stream &optional 
				  eof-error 
				  eof-value
				  recursive)
  (let ((c nil)
	(c1 nil))
    (loop
     do (progn
	  (setq c1 c)
	  (setq c (read-char-no-hang stream
				   eof-error
				   eof-value
				   recursive)))
     until (case c
	     ((#\^m #\space #\tab #\return #\newline) nil)
	     (nil t)
	     (t t))
     finally (progn

	       #+(and allegro-version>= (version>= 6))
	       (if c
		   (unread-char c stream))

	       #-(and allegro-version>= (version>= 6))
	       (if c
		   (unread-char c stream)
		 (if (or
		      (eql c1 #\newline)
		      (eql c1 #\^m)
		      (eql c1 #\linefeed)
		      (eql c1 #\return))
		     (unread-char c1 stream)
		   ) ;; (if (or ...
		 ) ;; (if c ...
	       c)
     ) ;; (loop ...
    ) ;; (let ((c ...
  ) ;; (defun ...

(defun peek-char-no-hang (stream &optional
				 eof-error
				 eof-value
				 recursive)
  (let ((c (read-char-no-hang stream
			      eof-error
			      eof-value
			      recursive)))
    (if c
	(unread-char c stream))
    c))


(defun make-unknown-matrix (dim &key (element-type t)
				     initial-contents)
  (declare (ignore dim))
  (case element-type
    (double-float (make-real-matrix initial-contents))
    (complex (make-complex-matrix initial-contents))
    (t (let ((type 'double-float))
	 (dolist (row initial-contents)
	   (dolist (elt row)
	     (typecase elt
	       (real t)
	       (complex (setq type 'complex))
	       (t (error "non-numeric element ~a in matrix; wanted (MEMBER REAL COMPLEX)" elt)))))
	 (case type
	   (double-float (make-real-matrix initial-contents))
	   (complex (make-complex-matrix initial-contents)))))))

(defun standard-matrix-p (m)
  (subtypep (type-of m) 'standard-matrix))

(defun make-matrix-from-reader (dimensions-list
				&key (element-type t)
				     initial-contents)

  (if (and (= (car dimensions-list) 0)
	   (= (cadr dimensions-list) 0))
      (return-from make-matrix-from-reader
	nil #|(make-real-matrix)|#))
  (let ((rows nil)
	(nrows (car dimensions-list)))
    
    (dolist (row initial-contents)
      (let ((n 1)
	    (thisrow nil)
	    (first (car row)))
	(if (standard-matrix-p first)
	    (progn
	      (let ((dimensions (size first)))
		(setf n (car dimensions))
		(setf nrows (+ nrows
			       (1- n)))
		(dotimes (i n)
		  (let ((innerrow nil))
		    (dotimes (j (cadr dimensions))
		      (%push-on-end% (matrix-ref first i j) innerrow))
		    (%push-on-end% innerrow thisrow)))))
	  (%push-on-end% (list first) thisrow))
	(dolist (col (cdr row))
	  (let* ((standard-matrix-p (standard-matrix-p col))
		 (dimensions (if standard-matrix-p 			      
				 (size col)
			       '(1 0)))
		 (dimension (car dimensions)))
	    
	    (when (not (= n dimension))
	      (error "make-array: inconsistent number of rows across columns"))
	    (if standard-matrix-p
		(dotimes (i n)
		  (dotimes (j (cadr dimensions))
		    (%push-on-end%  (matrix-ref col i j)  (nth i thisrow))))
	      (%push-on-end% col (car thisrow)))))
	(setf rows  `(,@rows ,@thisrow))))

    (let ((ncols (length (car rows))))
      (dolist (row (cdr rows))
	(if (not (= ncols (length row)))
	    (error "make-array: inconsistent number of columns across rows")))
      
      (make-unknown-matrix (list nrows ncols)
			   :element-type element-type
			   :initial-contents rows))))

  
(defun parse-matrix-expression-1 (rows)
  (let* ((n (length rows))
	 (m 0)
         (matrix-1 nil))
    
    (loop for row in rows
	  do (let ((thisrow nil)
		   (n 0))
	       (%push-on-end% (loop for i upfrom 1
				  do (multiple-value-bind (entry more)
					 (read-from-string (subseq row n)
							   nil)
				       (setf n (+ n more))
				       (if (= more 0)
					   (loop-finish))
				       (if (> i m)
					   (setf m i))
				       (%push-on-end%  entry thisrow))
				  finally (return `(list ,@thisrow) #| thisrow |#  ))
			      matrix-1)))


    (let ((new-matrix nil))
      (dolist (row matrix-1)
	(let ((new-row nil))
	  (dolist (col (cdr row))
	   (if (numberp col)
	       (%push-on-end% col new-row)
	     (return-from parse-matrix-expression-1
		  (list 'make-matrix-from-reader 
			(list 'quote (list n m)) 
			:initial-contents `(list ,@matrix-1)))))
	  (%push-on-end% new-row new-matrix)))

      (make-matrix-from-reader
       (list n m)
       :initial-contents new-matrix))
    
    ))


(defun parse-matrix-expression (stream char)
  (declare (ignore char))
  (let ((rows nil)
        (nested 0)
        (expr ""))
    (loop
        do (let ((c (read-char stream t nil t)))
             (case c
               ((#\; #\,) (if (> nested 0)
			      (%ccat% expr c)
			    (progn
			      (if (not (null-string-p expr))
				  (%push-on-end% expr rows))
			      (setf expr ""))))
               (#\[ (incf nested)
                    (%ccat% expr c))
               (#\] (if (> nested 0)
                        (progn
                          (decf nested)
                          (%ccat% expr c))
                      (progn
                        (if (not (null-string-p expr))
			    (%push-on-end% expr rows))
                        (loop-finish))))
               (t (%ccat% expr c))))
        finally (progn
		  ;; (peek-char t stream  nil nil t)
		  (let ((val (parse-matrix-expression-1 rows)))
		    (if (eql (peek-char-no-hang stream nil nil t)
			     #\')
			(progn 
			  (read-char stream nil nil t)
			  (setq val `(transpose ,val))))
		    (peek-ahead-no-hang stream nil nil t)
		    (return val))))))





(set-macro-character #\] (get-macro-character #\)))
(set-macro-character #\[ #'parse-matrix-expression)
#|
(read-from-string "[ [1 2 ; 3 4] [5 ; 6] ; [7 8 9] ] 1000")
(read-from-string " 1 2 [2]
")
(read-from-string "[1 2] 
  ")
(read-from-string "[1 2; [3 4; [ 5 6 ]
  ] 
 ]
  ")
|#

