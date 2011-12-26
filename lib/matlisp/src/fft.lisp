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
;;; Originally written by Tunc Simsek, Univ. of California, Berkeley
;;; May 5th, 2000, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: fft.lisp,v 1.11 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: fft.lisp,v $
;;; Revision 1.11  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.10  2004/04/07 18:01:55  rtoy
;;; IFFT! was not scaling the result by 1/n.
;;;
;;; Found and fixed by Mike Koerber.
;;;
;;; Revision 1.9  2003/02/11 13:52:04  rtoy
;;; Fixes for FFT from Mike Koerber:
;;;
;;; o FFT corrected to work on matrices, not just vectors.
;;; o Added FFT!/IFFT! to do in-place computation of FFTs
;;; o FFT/IFFT call FFT! and IFFT!.
;;;
;;; Revision 1.8  2001/10/25 21:48:45  rtoy
;;; IFFT wasn't scaling the result by 1/N for the case of row or column
;;; vectors.  Fixed.  Bug noted by Michael Koerber.
;;;
;;; Revision 1.7  2001/02/21 19:35:23  simsek
;;; o Fixed exporting and docstrings
;;;
;;; Revision 1.6  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.5  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.4  2000/05/12 14:13:37  rtoy
;;; o Change the interface to fft and ifft:  We don't need the wsave
;;;   argument anymore because fft and ifft compute them (and cache them
;;;   in a hash table) as needed.
;;; o Don't export ffti; the user doesn't need access to this anymore.
;;;
;;; Revision 1.3  2000/05/08 17:19:18  rtoy
;;; Changes to the STANDARD-MATRIX class:
;;; o The slots N, M, and NXM have changed names.
;;; o The accessors of these slots have changed:
;;;      NROWS, NCOLS, NUMBER-OF-ELEMENTS
;;;   The old names aren't available anymore.
;;; o The initargs of these slots have changed:
;;;      :nrows, :ncols, :nels
;;;
;;; Revision 1.2  2000/05/05 22:04:13  simsek
;;; o Changed one typo: fftb to ifft
;;;
;;; Revision 1.1  2000/05/05 21:35:54  simsek
;;; o Initial revision
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MATLISP")

#+nil (use-package "DFFTPACK")
#+nil (use-package "BLAS")
#+nil (use-package "LAPACK")
#+nil (use-package "FORTRAN-FFI-ACCESSORS")

#+nil (export '(fft ifft))

(defgeneric fft (x &optional n)
  (:documentation
  "
  Syntax
  ======
  (FFT x [n])

  Purpose
  =======
  Computes the N point discrete Fourier transform (DFT) of X:
   
     For k = 0,...,N-1:            
                                2 pi n
               \\---       -j k ------
                \\                N
    DFT(k) =    /    x(i) e
               /---
            i=0,..,N-1
 
     where the inverse DFT (see IFFT) is:

     For i = 0,...,N-1:            
                                  2 pi k
               \\---         -j i ------
                \\                  N
      X(i) =    /    DFT(k) e
               /---
            k=0,..,N-1
 
  If X is a vector, it is truncated at the end if it has more 
  than N elements and it is padded with zeros at the end if
  it has less than N elements.

  If X is a matrix, the FFT of each column of X is taken.

  The optional argument defaults to length of X when X is 
  a vector and to the number of rows of X when it is a matrix.

  See IFFT
  "))

(defgeneric ifft (x &optional n)
  (:documentation
  "
  Syntax
  ======
  (IFFT x [n])

  Purpose
  =======
  Computes the N point inverse discrete Fourier transform (DFT) of X:
   
     For i = 0,...,N-1:            
                                        2 pi k
                     \\---          j i ------
                 1    \\                  N
      IDFT(i) = ---    /      X(k) e
                 N    /---
                   k=0,..,N-1

     where the DFT (see FFT) is:

     For k = 0,...,N-1:            
                                 2 pi n
               \\---         j k ------
                \\                 N
      X(k) =    /  IDFT(i) e
               /---
            i=0,..,N-1

  If X is a vector, it is truncated at the end if it has more 
  than N elements and it is padded with zeros at the end if
  it has less than N elements.

  If X is a matrix, the IFFT of each column of X is taken.

  The optional argument defaults to length of X when X is 
  a vector and to the number of rows of X when it is a matrix.

  See FFT
  "))

(defun ffti (n)

  ;; Since this function is not in the user
  ;; interface anymore, I'm commenting out
  ;; the doc string.
  ;; "
  ;;  Syntax
  ;;  ======
  ;;  (FFTI n)
  ;;
  ;;  Purpose
  ;;  =======
  ;;  Initializes the vector WSAVE which is used in FFT and IFFT.
  ;;  The prime factorization of N and a tabulation of the
  ;;  trigonometric functions are computed and returned in WSAVE.
  ;;
  ;;  The optional argument WSAVE, if provided, must be a REAL-MATRIX
  ;;  with length > 4*N+15.  The same WSAVE may be used in FFT and IFFT
  ;;  if the arg N given to FFT and IFFT are the same.
  ;; "
  (declare (type (and fixnum (integer 1 *)) n))

  (let ((result (make-array (+ (* 4 n) 15) :element-type 'double-float)))
    (zffti n result)
    result))

;; Create the hash table used to keep track of all of the tables we
;; need for fft and ifft.
(eval-when (load eval compile)
(let ((wsave-hash-table (make-hash-table)))
  (defun lookup-wsave-entry (n)
    "Find the wsave entry for an FFT size of N"
    (let ((entry (gethash n wsave-hash-table)))
      (or entry
	  (setf (gethash n wsave-hash-table) (ffti n)))))
  ;; Just in case we want to start over
  (defun clear-wsave-entries ()
    (clrhash wsave-hash-table))
  ;; Just in case we want to take a peek at what's in the table.
  (defun dump-wsave-entries ()
    (maphash #'(lambda (key val)
		 (format t "Key = ~D, Val = ~A~%" key val))
	     wsave-hash-table))))

#+(or :cmu :sbcl)  
(defmethod fft ((x standard-matrix) &optional n)
  (let* ((n (or n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		    (nrows x))))
	 (wsave (lookup-wsave-entry n))
	 (result (cond ((row-vector-p x) 
			(make-complex-matrix-dim 1 n))
		       ((col-vector-p x)
			(make-complex-matrix-dim n 1))
		       (t (make-complex-matrix-dim n (ncols x))))))

    (if (row-or-col-vector-p x)
	(progn
	  (copy! x result)
	  (zfftf n (store result) wsave)
	  result)

      ;; Do FFT by column...first we copy the array
      (progn
	;; copy X to a working array, RESULT
	(dotimes (j-copy (ncols x))
	  (declare (type fixnum j-copy))
	  
	  ;; Note: we may be asked to truncate if N < (NROWS X)
	  (dotimes (i-copy (min n (nrows x))) 
	    (declare (type fixnum i-copy))
	    (setf (matrix-ref result i-copy j-copy) (matrix-ref x i-copy j-copy))))

	;; Then do FFT by column
	(fft! result)))))


#+:allegro
(defmethod fft ((x standard-matrix) &optional n)
  (let* ((n (or n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		    (nrows x))))
	 (wsave (lookup-wsave-entry n))
	 (tmp (make-complex-matrix-dim n 1))
	 (result (cond ((row-vector-p x) 
			(make-complex-matrix-dim 1 n))
		       ((col-vector-p x)
			(make-complex-matrix-dim n 1))
		       (t (make-complex-matrix-dim n (ncols x))))))

    (if (row-or-col-vector-p x)
	(progn
	  (copy! x result)
	  (zfftf n (store result) wsave))

      (dotimes (j (ncols x))
	(declare (type fixnum j))
	 (dotimes (i (nrows x))
	   (declare (type fixnum i))
	   (setf (matrix-ref tmp i) (matrix-ref x i j)))

	 (zfftf n (store tmp) wsave)

	 (dotimes (i (nrows x))
	   (declare (type fixnum i))
	   (setf (matrix-ref result i j) (matrix-ref tmp i)))

	 ))

      result))


#+(or :cmu :sbcl)
(defmethod ifft ((x standard-matrix) &optional n)
  (let* ((n (or n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		      (nrows x))))
	 (wsave (lookup-wsave-entry n))
	 (result (cond ((row-vector-p x) 
			(make-complex-matrix-dim 1 n))
		       ((col-vector-p x)
			(make-complex-matrix-dim n 1))
		       (t (make-complex-matrix-dim n (ncols x))))))

    (if (row-or-col-vector-p x)
	(progn
	  (copy! x result)
	  (zfftb n (store result) wsave)
	  (scal! (/ (float n 1d0)) result))

      ;; Perform the IFFT by columns...first we copy the array
      (progn
	;; Copy X to the working Array RESULT
	(dotimes (j-copy (ncols x))
	  (declare (type fixnum j-copy))

	  ;; Note: we may be asked to truncate if N < (NROWS X)
	  (dotimes (i-copy (min n (nrows x)))
	    (declare (type fixnum i-copy))
	    (setf (matrix-ref result i-copy j-copy) (matrix-ref x i-copy j-copy))))

	;; The we do the IFFT
	(ifft! result)))))

#+:allegro
(defmethod ifft ((x standard-matrix) &optional n)
  (let* ((n (or n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		    (nrows x))))
	 (wsave (lookup-wsave-entry n))
	 (tmp (make-complex-matrix-dim n 1))
	 (result (cond ((row-vector-p x) 
			(make-complex-matrix-dim 1 n))
		       ((col-vector-p x)
			(make-complex-matrix-dim n 1))
		       (t (make-complex-matrix-dim n (ncols x))))))

    (if (row-or-col-vector-p x)
	(progn
	  (copy! x result)
	  (zfftb n (store result) wsave)
	  (let ((scale-factor (/ (float n 1d0))))
	    (dotimes (k n)
	      (setf (matrix-ref result k)
		    (* scale-factor (matrix-ref result k))))))

      (dotimes (j (ncols x))
	(declare (type fixnum j))

	 (dotimes (i (nrows x))
	   (declare (type fixnum i))
	   (setf (matrix-ref tmp i) (matrix-ref x i j)))

	 (zfftb n (store tmp) wsave)

	 (dotimes (i (nrows x))
	   (declare (type fixnum i))
	   (setf (matrix-ref result i j) (matrix-ref tmp i)))

	 ))

      result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification for destructive FFT's.  The purpose is to eliminate
;; cons'ing for certain application requiring numerous calls to these
;; routines.

(defgeneric fft! (x)
  (:documentation "See FFT but note that the optional N is NOT permitted.
  Performs in place FFT modifying X.  This will only work for a complex matrix."))

(defgeneric ifft! (x)
  (:documentation "See IFFT but note that the optional N is NOT permitted.
  Performs in place IFFT modifying X.  This will only work for a complex matrix."))

#+(or :cmu :sbcl)  
(defmethod fft! ((x complex-matrix))
  (let* ((n (if (row-or-col-vector-p x)
		(max (nrows x) (ncols x))
	      (nrows x)))
	 (wsave (lookup-wsave-entry n)))

    (if (row-or-col-vector-p x)
	(progn
	  (zfftf n (store x) wsave)
	  x)

      (dotimes (j (ncols x) x)
	(declare (type fixnum j))
	(with-vector-data-addresses ((addr-x (store x))
				     (addr-wsave wsave))
	    (incf-sap :complex-double-float addr-x (* j n))
	    (dfftpack::fortran-zfftf n addr-x addr-wsave))))))


#+(or :cmu :sbcl)
(defmethod ifft! ((x complex-matrix))
  (let* ((n (if (row-or-col-vector-p x)
		      (max (nrows x) (ncols x))
		      (nrows x)))
	 (wsave (lookup-wsave-entry n)))

    (if (row-or-col-vector-p x)
	(progn
	  (zfftb n (store x) wsave)
	  (scal! (/ (float n 1d0)) x))

      ;; IFFT by column
      (scal! (/ (float n 1d0))
	     (dotimes (j (ncols x) x)
	       (declare (type fixnum j))
	       (with-vector-data-addresses ((addr-x (store x))
					    (addr-wsave wsave))
		       (incf-sap :complex-double-float addr-x (* j n))
		       (dfftpack::fortran-zfftb n addr-x addr-wsave)))))))

