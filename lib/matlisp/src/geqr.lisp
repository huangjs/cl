;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :matlisp; Base: 10 -*-
;;;
;;; $Id: geqr.lisp,v 1.7 2002/01/20 00:42:25 simsek Exp $
;;;
;;; $Log: geqr.lisp,v $
;;; Revision 1.7  2002/01/20 00:42:25  simsek
;;; o removed a spurious ignore
;;;
;;; Revision 1.6  2002/01/08 19:40:45  rtoy
;;; The functions we use are exported now.
;;;
;;; Revision 1.5  2001/10/29 18:00:28  rtoy
;;; Updates from M. Koerber to support QR routines with column pivoting:
;;;
;;; o Add an integer4 type and allocate-integer4-store routine.
;;; o Add the necessary Fortran routines
;;; o Add Lisp interface to the Fortran routines
;;; o Update geqr for the new routines.
;;;
;;; Revision 1.4  2001/10/29 17:34:34  rtoy
;;; I (RLT) stupidly deleted too much from M. Koerber's update.  This is
;;; his latest version.
;;;
;;; Revision 1.3  2001/10/26 15:19:25  rtoy
;;; Renamed optional SKINNY parameter to ECON.
;;;
;;; Revision 1.2  2001/10/26 13:37:03  rtoy
;;; Correctly handle the case when rows > cols and we want the [q1 q2]
;;; form.  Fix from M. Koerber.
;;;
;;; Revision 1.1  2001/10/25 21:51:58  rtoy
;;; Initial revision for QR routines.
;;;

(in-package "MATLISP")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the methods required to handle general matricies of Real
;; and complex types.  There are numerous other special cases, but
;; they will not be considered for this first release.  mak
(defgeneric geqr! (a)
  (:documentation
   "
 Syntax
 ======
 (GEQR! a)

 Purpose:
 ========

 Use QR or QR! for access to this routine.

 Computes the QR factorization of an M-by-N matrix A: A = Q * R where
 R is an M-by-N upper trapezoidal (triangular) matrix and Q is
 M-by-M unitary matrix.

 Return Values:
 ==============

   [1] Q
   [2] R
      
 If the factorization can not be done, Q and R are set to NIL.

 NOTE:  THIS FUNCTION IS DESTRUCTIVE.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One could simply use LWORK = (MAX 1 N), but this call might result
;; in some optimization in performance.  For small matricies this is
;; probably a 'no-net-gain' operation...but I seldom use small matricies
;; in my work ;-) ... mak
(let ((xx (allocate-real-store 1))
      (work (allocate-real-store 1)))

  (defun dgeqrf-workspace-inquiry (m n)
    (multiple-value-bind (store-a store-tau store-work lwork info)
	(lapack:dgeqrf m n xx m xx work -1 0)

      (declare (ignore store-a store-tau store-work lwork info))

      (values (ceiling (realpart (aref work 0)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((xx (allocate-complex-store 1))
      (work (allocate-complex-store 1)))

  (defun zgeqrf-workspace-inquiry (m n)

    (multiple-value-bind (store-a store-tau store-work lwork info)
	(lapack:zgeqrf m n xx m xx work -1 0)

      (declare (ignore store-a store-tau store-work lwork info))
      
      (values (ceiling (realpart (aref work 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Okay...now we build up the specific method for real and comples
(defmethod geqr! ((a real-matrix))

  (let* ((m (nrows a))
	 (n (ncols a))
	 (k (min m n))			; THESE ROUTINES ONLY RETURN A MINIMUM Q!
	 (tau (allocate-real-store k))	; reflection factors
	 (lwork (dgeqrf-workspace-inquiry m n))	; optimum work array size
	 (work (allocate-real-store lwork))) ; and the work area

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Do the Householder portion of the decomposition
    (multiple-value-bind (q-r new-tau new-work info)
	(lapack:dgeqrf m n (store a) m tau work lwork 0)

      (declare (ignore new-work))
      ;; Q-R and NEW-TAU aren't needed either since the (STORE A) and WORK
      ;; get modified 

      (if (not (zerop info))
	  ;; If INFO is not zero, then an error occured.  Return Nil
	  ;; for the Q and R and print a warning
	  (progn (warn "QR Decomp failed:  Argument ~d in call to DGEQRF is bad" (- info))
		 (values nil nil))

	;; If we are here, then INFO == 0 and all is well...
	(let ((r (make-real-matrix k n)))
	  ;; Extract the matrix R from Q-R
	  (dotimes (row k)
	    (loop for col from row below n do
		  (setf (matrix-ref r row col)
			(aref q-r (fortran-matrix-indexing row col m)))))

	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Now compute Q via DORGQR and return.  This is always
	  ;; the economy representation of Q.  I.e., Q1 in Q = [Q1 Q2]
	  (multiple-value-bind (new-q-r new-work info)
	      (lapack:dorgqr m k k q-r m new-tau work lwork 0)

	    (declare (ignore new-work))

	    (if (not (zerop info))
		(progn (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
		       (values nil nil))

		(let ((q (make-real-matrix m k)))

		  (dotimes (row m)
		    (dotimes (col k)
		      (setf (matrix-ref q row col)
			    (aref new-q-r (fortran-matrix-indexing row col m)))))

		  ;; We're done!
		  (values q r)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod geqr! ((a complex-matrix))

  (let* ((m (nrows a))
	 (n (ncols a))
	 (k (min m n))			; THESE ROUTINES ONLY RETURN A MINIMUM Q!
	 (tau (allocate-complex-store k))	; reflection factors
	 (lwork (zgeqrf-workspace-inquiry m n))	; optimum work array size
	 (work (allocate-complex-store lwork))) ; and the work area

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Do the Householder portion of the decomposition
    (multiple-value-bind (q-r new-tau new-work info)
	(lapack:zgeqrf m n (store a) m tau work lwork 0)

      (declare (ignore new-work))
      ;; Q-R and NEW-TAU aren't needed either since the (STORE A) and WORK
      ;; get modified 

      (if (not (zerop info))
	  ;; If INFO is not zero, then an error occured.  Return Nil
	  ;; for the Q and R and print a warning
	  (progn (warn "QR Decomp failed:  Argument ~d in call to DGEQRF is bad" (- info))
		 (values nil nil))

	;; If we are here, then INFO == 0 and all is well...
	(let ((r (make-complex-matrix k n))
	      (idx-fortran 0))

	  ;; Extract the matrix R from Q-R
	  (dotimes (row k)
	    (loop for col from row below n do
		  (setf idx-fortran (fortran-complex-matrix-indexing row col m)
			(matrix-ref r row col) (complex (aref q-r idx-fortran)
							(aref q-r (1+ idx-fortran))))))
	  
	  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Now compute Q via ZUNGQR and return.  This is always
	  ;; the economy representation of Q.  I.e., Q1 in Q = [Q1 Q2]
	  (multiple-value-bind (new-q-r new-work info)
	      (lapack:zungqr m k k q-r m new-tau work lwork 0)

	    (declare (ignore new-work))

	    (if (not (zerop info))
		(progn (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
		       (values nil nil))

		(let ((q (make-complex-matrix m k)))

		  (dotimes (row m)
		    (dotimes (col k)
		      (setf idx-fortran (fortran-complex-matrix-indexing row col m)
			    (matrix-ref q row col) (complex (aref new-q-r idx-fortran)
							    (aref new-q-r (1+ idx-fortran))))))

		  ;; We're done!
		  (values q r)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric geqp! (a)
  (:documentation
   "
  SYNTAX
  ======
  (DEQP A)
  
  INPUT
  -----
  A    A Matlisp M x N matrix
  
  OUTPUT (VALUES Q R PVT-VEC)
  ------
  Q, R     Matlisp matricies representing the QR composition of A*P
  JPVT     A lisp sequence of integers representing the column pivoting.
	   This is the variable JPVT in LAPACK's [DZ]GEGP3.F: 
  
	   JPVT    (input/output) INTEGER array, dimension (N)
		   On entry, if JPVT(J).ne.0, the J-th column of A is permuted
		   to the front of A*P (a leading column); if JPVT(J)=0,
		   the J-th column of A is a free column.
		   On exit, if JPVT(J)=K, then the J-th column of A*P was the
		   the K-th column of A.

          *** THE EXCEPTION TAKEN HERE IS THAT \"JPVT - 1\" IS RETURNED TO COMPLY ***
          *** WITH THE ZERO BASED INDEXING OF LISP. ***
   
  PURPOSE
  =======
  
  Use QR or QR! for access to this routine.
  
  Computes the QR factorization of an M-by-N matrix A using column pivoting.
  I.e., A*P = Q*R is computed where P is a column pivoting matrix.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((xx (allocate-real-store 1))
      (xxint (allocate-integer4-store 1))
      (work (allocate-real-store 1)))
	  
  (defun dgeqp3-workspace-inquiry (m n)
    (multiple-value-bind (a jpvt tau work info)
	(lapack:dgeqp3 m n xx m xxint xx work -1 0)
      
	(declare (ignore a jpvt tau))
	(unless (zerop info)
	  (error "Error in computing required work space dimensions"))
	
	(values (ceiling (aref work 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod geqp! ((a real-matrix))

  ;; Set up the work spaces need by DGEQP3
  (let* ((m (nrows a))
	 (n (ncols a))
	 (min-nm (min m n))
	 (lda (nrows a))
	 (jpvt (allocate-integer4-store n 0))
	 (tau (allocate-real-store min-nm))
	 (lwork (dgeqp3-workspace-inquiry m n))
	 (work (allocate-real-store lwork)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Okay...off to work...do the basic decomposition
    (multiple-value-bind (store-a jpvt tau work info)
	(lapack:dgeqp3 m n (store a) lda jpvt tau work lwork 0)


      ;; Check for error
      (unless (zerop info)
	(error "QR Decomp faile: Argument ~d in call to DGEQP3 is bad" info))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; If we get here there is no error, construct the R matrix
      (let ((r (make-real-matrix min-nm n)))
	(dotimes (row min-nm)
	  (loop for col from row below n do
		(setf (matrix-ref r row col)
		      (aref store-a (fortran-matrix-indexing row col lda)))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; and we need to construct the Q portion.
	(multiple-value-bind (store-a work info)
	    (lapack:dorgqr m min-nm min-nm store-a m tau work lwork 0)

	  (declare (ignore work))

	  ;; Was there any error?
	  (unless (zerop info)
	    (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
	    (values nil nil nil))

	  ;; No error, so construct the new Q matrix
	  (let ((q (make-real-matrix m min-nm)))

	    (dotimes (row m)
	      (dotimes (col min-nm)
		(setf (matrix-ref q row col)
		      (aref store-a (fortran-matrix-indexing row col lda)))))

	    ;; and return Q, R, and JPVT...
	    (values q r (map (type-of jpvt) #'1- jpvt))))))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((xx (allocate-real-store 1))
      (xxint (allocate-integer4-store 1))
      (work (allocate-real-store 1)))
	  
  (defun zgeqp3-workspace-inquiry (m n)
    (multiple-value-bind (a jpvt tau work rwork info)
	(lapack:zgeqp3 m n xx m xxint xx work -1 xx 0)
      
	(declare (ignore a jpvt tau rwork))
	
	(unless (zerop info)
	  (error "Error in computing required work space dimensions"))
	
	(values (ceiling (aref work 0))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod geqp! ((a complex-matrix))

  ;; Set up the work spaces need by DGEQP3
  (let* ((m (nrows a))
	 (n (ncols a))
	 (min-nm (min m n))
	 (lda (nrows a))
	 (jpvt (allocate-integer4-store n 0))
	 (tau (allocate-complex-store min-nm))
	 (lwork (dgeqp3-workspace-inquiry m n))
	 (work (allocate-complex-store lwork))
	 (rwork (allocate-real-store (* 2 n))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Okay...off to work...do the basic decomposition
    (multiple-value-bind (store-a jpvt tau work rwork info)
	(lapack:zgeqp3 m n (store a) lda jpvt tau work lwork rwork 0)

      (declare (ignore rwork))

      ;; Check for error
      (unless (zerop info)
	(error "QR Decomp faile: Argument ~d in call to ZGEQP3 is bad" info))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; If we get here there is no error, construct the R matrix
      (let ((r (make-complex-matrix min-nm n))
	    (idx-fortran 0))
	
	(dotimes (row min-nm)
	  (loop for col from row below n do
		(setf idx-fortran (fortran-complex-matrix-indexing row col lda)
		      (matrix-ref r row col) (complex (aref store-a idx-fortran)
						      (aref store-a (1+ idx-fortran))))))

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; and we need to construct the Q portion.
	(multiple-value-bind (store-a work info)
	    (lapack:zungqr m min-nm min-nm store-a m tau work lwork 0)

	  (declare (ignore work))

	  ;; Was there any error?
	  (unless (zerop info)
	    (warn "Error in DORGQR in argument ~d.  Returning nil." (- info))
	    (values nil nil nil))

	  ;; No error, so construct the new Q matrix
	  (let ((q (make-complex-matrix m min-nm)))

	    (dotimes (row m)
	      (dotimes (col min-nm)
		(setf idx-fortran (fortran-complex-matrix-indexing row col lda)
		      (matrix-ref q row col) (complex (aref store-a idx-fortran)
						      (aref store-a (1+ idx-fortran))))))

	    ;; and return Q, R, and JPVT...
	    (values q r (map (type-of jpvt) #'1- jpvt))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qr (a &optional (econ t) (pivot nil))
  "
  SYNTAX 
  ======
  (QR A [ECON PIVOT])
  
  INPUT
  -----
  A       A Matlisp matrix of size M x N
  ECON    Produce the economy size QR decomposition which means return Q1 and R1 only.
          T or NIL. Default is T.
  PIVOT   T or NIL.  Default is NIL.  When PIVOT is true column pivoting is used.
  
  OUTPUT: (VALUES Q R PVT)
  ------
  Q  A Matlisp matrix of size M x M
  R  A Matlisp matrix of size M x N
  PVT A Lisp sequence of (SIGNED-BYTE 32)
  
  PURPOSE
  =======
  Compute the QR decompostion of A.  A = Q*R.  When M > N, the QR decomposition
  can be written as
  A*P = [Q1 Q2] * [ R1 ]
                  [ -- ]
                  [ 0  ]

  When ECON == T only Q1 and R1 is return. Otherwise Q and R are returned.
  Note that when ECON == NULL the value of Q2 is taken from the SVD of A;
  this matches the results of OCTAVE and MATLAB.

  When PIVOT == NIL, no column pivoting is used and P == Identity-Matrix

  When PIVOT == T, column pivoting is used to improve accuracy.  In this
  case P represents the rearrangement of columns.  The sequence PVT indicate
  this column pivoting.  Specifically, the I-th column of A*P is the
  \"(ELT PVT I)-th\" column of A.
"

  (cond
   (econ
    (cond
     (pivot  (geqp! (copy a)))
     (t      (geqr! (copy a)))))

   (t
    ;; Okay ... A [Q1 Q2] form was requested, but this only makes sense
    ;; when ROWS > COLS.  When ROWS <= COLS, GEQR! is sufficient
    (if (<= (number-of-rows a) (number-of-cols a))
	(cond
	 (pivot (geqp! (copy a)))
	 (t     (geqr! (copy a))))

      ;; Else we do a QR and an SVD to pick up the null-space of A
      (cond
       (pivot (multiple-value-bind (q1 r1 pvt1)
		  (geqp! (copy a))
		(values
		 ;; Extend the Q part
		 (join q1
		       (matrix-ref (svd (matrix-ref a (seq 0 (1- (number-of-rows a))) (coerce pvt1 'list)) :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 ;; Extend the R part
		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part does not need extending
		 pvt1)))
       
       (t     (multiple-value-bind (q1 r1)
		  (geqr! (copy a))
		(values 

		 (join q1
		       (matrix-ref (svd a :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part is nil since no pivoting is done here
		 nil))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun qr! (a &optional (econ t) (pivot nil))
  "
  SYNTAX 
  ======
  (QR! A [ECON])

  PURPOSE
  =======
  Compute the QR decomposition of A.  See QR.  

  NOTE:  THIS IS A DESTRUCTIVE VERSION.  THE MATRIX A IS OVERWRITTEN WITH Q.
         USE THIS ROUTINE ONLY IF A IS VERY LARGE AND YOU DON'T CARE ABOUT
         IT AFTER THE CALL."

  (cond
   (econ
    (cond
     (pivot  (geqp!  a))
     (t      (geqr!  a))))

   (t
    ;; Okay ... A [Q1 Q2] form was requested, but this only makes sense
    ;; when ROWS > COLS.  When ROWS <= COLS, GEQR! is sufficient
    (if (<= (number-of-rows a) (number-of-cols a))
	(cond
	 (pivot (geqp!  a))
	 (t     (geqr!  a)))

      ;; Else we do a QR and an SVD to pick up the null-space of A
      (cond
       (pivot (multiple-value-bind (q1 r1 pvt1)
		  (geqp!  a)
		(values
		 ;; Extend the Q part
		 (join q1
		       (matrix-ref (svd (matrix-ref a (seq 0 (1- (number-of-rows a))) (coerce pvt1 'list)) :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 ;; Extend the R part
		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part does not need extending
		 pvt1)))
       
       (t     (multiple-value-bind (q1 r1)
		  (geqr!  a)
		(values 

		 (join q1
		       (matrix-ref (svd a :a)	; the Q2 part
				   (seq 0 (1- (number-of-rows a)))
				   (seq (number-of-cols a) (1- (number-of-rows a))))
		       :horizontal)

		 (join r1 (zeros (- (number-of-rows a) (number-of-cols a))
				 (number-of-cols a)) :vertical)

		 ;; The PVT part is nil since no pivoting is done here
		 nil))))))))
