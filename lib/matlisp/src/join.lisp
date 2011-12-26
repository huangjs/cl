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
;;; Originally written by Raymond Toy.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: join.lisp,v 1.7 2003/05/31 05:18:33 rtoy Exp $
;;;
;;; $Log: join.lisp,v $
;;; Revision 1.7  2003/05/31 05:18:33  rtoy
;;; Fix typo.
;;;
;;; Revision 1.6  2001/10/26 15:20:33  rtoy
;;; RESHAPE! is working now, so use it.
;;;
;;; Revision 1.5  2001/10/25 21:53:31  rtoy
;;; Two new join methods.  From M. Koerber to support his QR routines.
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.2  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.1  2000/04/14 00:11:12  simsek
;;; o This file is adapted from obsolete files 'matrix-float.lisp'
;;;   'matrix-complex.lisp' and 'matrix-extra.lisp'
;;; o Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (export 'join)

(defgeneric join (a b &optional position)
  (:documentation
   "
  Syntax
  ======
  (JOIN a b [orientation])

  Purpose
  =======
  Append matrix A and matrix B to form one of the following matrices:
 
    [A | B]     [ A ]
                [---]
                [ B ]
 
  The choice is determined by ORIENTATION, which should take the value
  of either :HORIZONTAL, for the left case above, or :VERTICAL for
  the right case above.  For any other value of ORIENTATION, we try to
  figure it out based on the dimensions of the A and B.  For the
  ambiguous case (A and B are square), we join them horizontally by
  default.
"))
  
(defmethod join :before ((a standard-matrix) (b standard-matrix) &optional orientation)
  (let ((n-a (nrows a))
	(m-a (ncols a))
	(n-b (nrows b))
	(m-b (ncols b)))

    (case orientation
       (:horizontal (unless (= n-a n-b)
				   (error "Cannot horizontally join matrices with ~d rows and ~d rows"
					  n-a n-b)))
       (:vertical  (unless (= m-a m-b)
				(error "Cannot vertically join matrices with ~d columns and ~d columns"
				       m-a m-b)))
       (t  (unless (or (= n-a n-b)
		       (= m-a m-b))
	     (error "Unable to join in any direction a ~d x ~d matrix with a ~d x ~d matrix"
		    n-a m-a
		    n-b m-b))))))

(defmethod join ((a real-matrix) (b real-matrix) &optional orientation)
  (case orientation
    (:horizontal
     (let* ((nrows (nrows a))
	    (m-a (ncols a))
	    (ncols (+ m-a (ncols b)))
	    (res (make-real-matrix nrows ncols)))
       (declare (fixnum nrows m-a ncols))
       ;; Copy A
       (dotimes (r nrows)
	 (declare (fixnum r))
	 (dotimes (c m-a)
	   (declare (fixnum c))
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r nrows)
	 (declare (fixnum r))
	 (dotimes (c (ncols b))
	   (declare (fixnum c))
	   (setf (matrix-ref res r (+ c m-a)) (matrix-ref b r c))))
       res))
    (:vertical
     (let* ((ncols (ncols a))
	    (n-a (nrows a))
	    (nrows (+ n-a (nrows b)))
	    (res (make-real-matrix nrows ncols)))
       (declare (fixnum nrows n-a ncols))
       ;; Copy A
       (dotimes (r n-a)
	 (declare (fixnum r))
	 (dotimes (c ncols)
	   (declare (fixnum c))
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r (nrows b))
	 (declare (fixnum r))
	 (dotimes (c ncols)
	   (declare (fixnum c))
	   (setf (matrix-ref res (+ r n-a) c) (matrix-ref b r c))))
       res))
    (t
     (cond ((= (nrows a) (nrows b))
	    (join a b  :horizontal))
	   ((= (ncols a) (ncols b))
	    (join a b  :vertical))
	   (t
	    (error "Unable to determine how to join a ~d X ~d matrix with a ~d x ~d matrix"
		   (nrows a)
		   (ncols a)
		   (nrows b)
		   (ncols b)))))))


(defmethod join ((a complex-matrix) (b complex-matrix) &optional orientation)
  (case orientation
    (:horizontal
     (let* ((nrows (nrows a))
	    (m-a (ncols a))
	    (ncols (+ m-a (ncols b)))
	    (res (make-complex-matrix nrows ncols)))
       ;; Copy A
       (dotimes (r nrows)
	 (dotimes (c m-a)
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r nrows)
	 (dotimes (c (ncols b))
	   (setf (matrix-ref res r (+ c m-a)) (matrix-ref b r c))))
       res))
    (:vertical
     (let* ((ncols (ncols a))
	    (n-a (nrows a))
	    (nrows (+ n-a (nrows b)))
	    (res (make-complex-matrix nrows ncols)))
       ;; Copy A
       (dotimes (r n-a)
	 (dotimes (c ncols)
	   (setf (matrix-ref res r c) (matrix-ref a r c))))
       (dotimes (r (nrows b))
	 (dotimes (c ncols)
	   (setf (matrix-ref res (+ r n-a) c) (matrix-ref b r c))))
       res))
    (t
     (cond ((= (nrows a) (nrows b))
	    (join a b  :horizontal))
	   ((= (ncols a) (ncols b))
	    (join a b  :vertical))
	   (t
	    (error "Unable to determine how to join a ~d X ~d matrix with a ~d x ~d matrix"
		   (nrows a)
		   (ncols a)
		   (nrows b)
		   (ncols b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A couple of new join methods
(defmethod join ((a real-matrix) (b complex-matrix) &optional orientation)
  (join (apply #'reshape! (make-complex-matrix (store a)) (size a))
	b orientation))

(defmethod join ((a complex-matrix) (b real-matrix) &optional orientation)
  (join a
	(apply #'reshape! (make-complex-matrix (store b)) (size b))
	orientation))

