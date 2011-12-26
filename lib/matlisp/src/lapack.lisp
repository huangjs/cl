;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: :lapack; Base: 10 -*-
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
;;; 1999, simsek@eecs.berkeley.edu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id: lapack.lisp,v 1.11 2004/05/24 16:34:22 rtoy Exp $
;;;
;;; $Log: lapack.lisp,v $
;;; Revision 1.11  2004/05/24 16:34:22  rtoy
;;; More SBCL support from Robert Sedgewick.  The previous SBCL support
;;; was incomplete.
;;;
;;; Revision 1.10  2003/05/31 05:19:17  rtoy
;;; Was missing a paren.
;;;
;;; Revision 1.9  2003/05/02 01:15:14  rtoy
;;; Fix arg list for zgetrs:  2 args in wrong order, forget the final INFO
;;; arg.
;;;
;;; Revision 1.8  2002/10/23 00:47:57  rtoy
;;; Fix typo:  zgetrs was missing a closing paren.
;;;
;;; Revision 1.7  2002/09/30 18:28:03  simsek
;;; o Added changes by N.Neuss for getrs functions
;;;
;;; Revision 1.6  2001/10/29 18:00:28  rtoy
;;; Updates from M. Koerber to support QR routines with column pivoting:
;;;
;;; o Add an integer4 type and allocate-integer4-store routine.
;;; o Add the necessary Fortran routines
;;; o Add Lisp interface to the Fortran routines
;;; o Update geqr for the new routines.
;;;
;;; Revision 1.5  2001/10/25 21:51:00  rtoy
;;; Add interface to QR routines.  Mostly done by M. Koerber.
;;;
;;; Revision 1.4  2000/07/11 18:02:03  simsek
;;; o Added credits
;;;
;;; Revision 1.3  2000/07/11 02:11:56  simsek
;;; o Added support for Allegro CL
;;;
;;; Revision 1.2  2000/06/19 22:21:45  rtoy
;;; Define packages elsewhere.
;;;
;;; Revision 1.1  2000/04/14 00:12:48  simsek
;;; Initial revision.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+nil
(defpackage "LAPACK"
#+:cmu  (:use "COMMON-LISP" "ALIEN" "C-CALL" "FORTRAN-FFI-ACCESSORS")
#+:sbcl  (:use "COMMON-LISP" "SB-ALIEN" "SB-C" "FORTRAN-FFI-ACCESSORS")
#+:allegro  (:use "COMMON-LISP" "FOREIGN-FUNCTIONS" "FORTRAN-FFI-ACCESSORS")
  (:export
"DGESV" "DGEEV" "DGETRF" "DGETRS" "DGESVD"
"ZGESV" "ZGEEV" "ZGETRF" "ZGETRS" "ZGESVD" ))

(in-package "LAPACK")


(def-fortran-routine dgesv :void
"
   -- LAPACK driver routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      March 31, 1993

   Purpose
   =======
 
   DGESV computes the solution to a real system of linear equations
      A * X = B,
   where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
 
   The LU decomposition with partial pivoting and row interchanges is
   used to factor A as
      A = P * L * U,
   where P is a permutation matrix, L is unit lower triangular, and U is
   upper triangular.  The factored form of A is then used to solve the
   system of equations A * X = B.
 
   Arguments
   =========
 
   N       (input) INTEGER
           The number of linear equations, i.e., the order of the
           matrix A.  N >= 0.
 
   NRHS    (input) INTEGER
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  NRHS >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
           On entry, the N-by-N coefficient matrix A.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,N).
 
   IPIV    (output) INTEGER array, dimension (N)
           The pivot indices that define the permutation matrix P;
           row i of the matrix was interchanged with row IPIV(i).
 
   B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
           On entry, the N-by-NRHS matrix of right hand side matrix B.
           On exit, if INFO = 0, the N-by-NRHS solution matrix X.
 
   LDB     (input) INTEGER
           The leading dimension of the array B.  LDB >= max(1,N).
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
           > 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
                 has been completed, but the factor U is exactly
                 singular, so the solution could not be computed.
 
"
  (n :integer :input)
  (nrhs :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (ipiv (* :integer) :output)
  (b (* :double-float) :input-output)
  (ldb :integer :input)
  (info :integer :output)
)

(def-fortran-routine dgeev :void
"
   -- LAPACK driver routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999

   Purpose
   =======
 
   DGEEV computes for an N-by-N real nonsymmetric matrix A, the
   eigenvalues and, optionally, the left and/or right eigenvectors.
 
   The right eigenvector v(j) of A satisfies
                    A * v(j) = lambda(j) * v(j)
   where lambda(j) is its eigenvalue.
   The left eigenvector u(j) of A satisfies
                 u(j)**H * A = lambda(j) * u(j)**H
   where u(j)**H denotes the conjugate transpose of u(j).
 
   The computed eigenvectors are normalized to have Euclidean norm
   equal to 1 and largest component real.
 
   Arguments
   =========
 
   JOBVL   (input) CHARACTER*1
           = 'N': left eigenvectors of A are not computed;
           = 'V': left eigenvectors of A are computed.
 
   JOBVR   (input) CHARACTER*1
           = 'N': right eigenvectors of A are not computed;
           = 'V': right eigenvectors of A are computed.
 
   N       (input) INTEGER
           The order of the matrix A. N >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
           On entry, the N-by-N matrix A.
           On exit, A has been overwritten.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,N).
 
   WR      (output) DOUBLE PRECISION array, dimension (N)
   WI      (output) DOUBLE PRECISION array, dimension (N)
           WR and WI contain the real and imaginary parts,
           respectively, of the computed eigenvalues.  Complex
           conjugate pairs of eigenvalues appear consecutively
           with the eigenvalue having the positive imaginary part
           first.
 
   VL      (output) DOUBLE PRECISION array, dimension (LDVL,N)
           If JOBVL = 'V', the left eigenvectors u(j) are stored one
           after another in the columns of VL, in the same order
           as their eigenvalues.
           If JOBVL = 'N', VL is not referenced.
           If the j-th eigenvalue is real, then u(j) = VL(:,j),
           the j-th column of VL.
           If the j-th and (j+1)-st eigenvalues form a complex
           conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
           u(j+1) = VL(:,j) - i*VL(:,j+1).
 
   LDVL    (input) INTEGER
           The leading dimension of the array VL.  LDVL >= 1; if
           JOBVL = 'V', LDVL >= N.
 
   VR      (output) DOUBLE PRECISION array, dimension (LDVR,N)
           If JOBVR = 'V', the right eigenvectors v(j) are stored one
           after another in the columns of VR, in the same order
           as their eigenvalues.
           If JOBVR = 'N', VR is not referenced.
           If the j-th eigenvalue is real, then v(j) = VR(:,j),
           the j-th column of VR.
           If the j-th and (j+1)-st eigenvalues form a complex
           conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
           v(j+1) = VR(:,j) - i*VR(:,j+1).
 
   LDVR    (input) INTEGER
           The leading dimension of the array VR.  LDVR >= 1; if
           JOBVR = 'V', LDVR >= N.
 
   WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK.  LWORK >= max(1,3*N), and
           if JOBVL = 'V' or JOBVR = 'V', LWORK >= 4*N.  For good
           performance, LWORK must generally be larger.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value.
           > 0:  if INFO = i, the QR algorithm failed to compute all the
                 eigenvalues, and no eigenvectors have been computed;
                 elements i+1:N of WR and WI contain eigenvalues which
                 have converged.
 
"
  (jobvl :string :input)
  (jobvr :string :input)
  (n :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (wr (* :double-float) :output)
  (wi (* :double-float) :output)
  (vl (* :double-float) :output)
  (ldvl :integer :input)
  (vr (* :double-float) :output)
  (ldvr :integer :input)
  (work (* :double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output)
)

(def-fortran-routine dgetrf :void
"
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      March 31, 1993

   Purpose
   =======
 
   DGETRF computes an LU factorization of a general M-by-N matrix A
   using partial pivoting with row interchanges.
 
   The factorization has the form
      A = P * L * U
   where P is a permutation matrix, L is lower triangular with unit
   diagonal elements (lower trapezoidal if m > n), and U is upper
   triangular (upper trapezoidal if m < n).
 
   This is the right-looking Level 3 BLAS version of the algorithm.
 
   Arguments
   =========
 
   M       (input) INTEGER
           The number of rows of the matrix A.  M >= 0.
 
   N       (input) INTEGER
           The number of columns of the matrix A.  N >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
           On entry, the M-by-N matrix to be factored.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,M).
 
   IPIV    (output) INTEGER array, dimension (min(M,N))
           The pivot indices; for 1 <= i <= min(M,N), row i of the
           matrix was interchanged with row IPIV(i).
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
           > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
                 has been completed, but the factor U is exactly
                 singular, and division by zero will occur if it is used
                 to solve a system of equations.
 
"
  (m :integer :input)
  (n :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (ipiv (* :integer) :output)
  (info :integer :output)
)

(def-fortran-routine dgetrs :void
"
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      March 31, 1993

   Purpose
   =======

   DGETRS solves a system of linear equations
      A * X = B  or  A' * X = B
   with a general N-by-N matrix A using the LU factorization computed
   by DGETRF.

   Arguments
   =========

   TRANS   (input) CHARACTER*1
           Specifies the form of the system of equations:
           = 'N':  A * X = B  (No transpose)
           = 'T':  A'* X = B  (Transpose)
           = 'C':  A'* X = B  (Conjugate transpose = Transpose)

   N       (input) INTEGER
           The order of the matrix A.  N >= 0.

   NRHS    (input) INTEGER
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  NRHS >= 0.

   A       (input) DOUBLE PRECISION array, dimension (LDA,N)
           The factors L and U from the factorization A = P*L*U
           as computed by DGETRF.

   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,N).

   IPIV    (input) INTEGER array, dimension (N)
           The pivot indices from DGETRF; for 1<=i<=N, row i of the
           matrix was interchanged with row IPIV(i).

   B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
           On entry, the right hand side matrix B.
           On exit, the solution matrix X.

   LDB     (input) INTEGER
           The leading dimension of the array B.  LDB >= max(1,N).

   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value

"
  (trans :string :input)
  (n :integer :input)
  (nhrs :integer :input)
  (a (* :double-float) :input)
  (lda :integer :input)
  (ipiv (* :integer) :input)
  (b (* :double-float) :input-output)
  (ldb :integer :input)
  (info :integer :output)
)

(def-fortran-routine zgetrs :void
"
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      September 30, 1994
 
   Purpose
   =======
 
   ZGETRS solves a system of linear equations
      A * X = B,  A**T * X = B,  or  A**H * X = B
   with a general N-by-N matrix A using the LU factorization computed
   by ZGETRF.
 
   Arguments
   =========
 
   TRANS   (input) CHARACTER*1
           Specifies the form of the system of equations:
           = 'N':  A * X = B     (No transpose)
           = 'T':  A**T * X = B  (Transpose)
           = 'C':  A**H * X = B  (Conjugate transpose)
 
   N       (input) INTEGER
           The order of the matrix A.  N >= 0.
 
   NRHS    (input) INTEGER
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  NRHS >= 0.
 
   A       (input) COMPLEX*16 array, dimension (LDA,N)
           The factors L and U from the factorization A = P*L*U
           as computed by ZGETRF.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,N).
 
   IPIV    (input) INTEGER array, dimension (N)
           The pivot indices from ZGETRF; for 1<=i<=N, row i of the
           matrix was interchanged with row IPIV(i).
 
   B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
           On entry, the right hand side matrix B.
           On exit, the solution matrix X.
 
   LDB     (input) INTEGER
           The leading dimension of the array B.  LDB >= max(1,N).
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
"
  (trans :string :input)
  (n :integer :input)
  (nhrs :integer :input)
  (a (* :complex-double-float) :input)
  (lda :integer :input)
  (ipiv (* :integer) :input)
  (b (* :complex-double-float) :input-output)
  (ldb :integer :input)
  (info :integer :output))

(def-fortran-routine dgesvd :void
"
   -- LAPACK driver routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      October 31, 1999

   Purpose
   =======
 
   DGESVD computes the singular value decomposition (SVD) of a real
   M-by-N matrix A, optionally computing the left and/or right singular
   vectors. The SVD is written
 
        A = U * SIGMA * transpose(V)
 
   where SIGMA is an M-by-N matrix which is zero except for its
   min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
   V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
   are the singular values of A; they are real and non-negative, and
   are returned in descending order.  The first min(m,n) columns of
   U and V are the left and right singular vectors of A.
 
   Note that the routine returns V**T, not V.
 
   Arguments
   =========
 
   JOBU    (input) CHARACTER*1
           Specifies options for computing all or part of the matrix U:
           = 'A':  all M columns of U are returned in array U:
           = 'S':  the first min(m,n) columns of U (the left singular
                   vectors) are returned in the array U;
           = 'O':  the first min(m,n) columns of U (the left singular
                   vectors) are overwritten on the array A;
           = 'N':  no columns of U (no left singular vectors) are
                   computed.
 
   JOBVT   (input) CHARACTER*1
           Specifies options for computing all or part of the matrix
           V**T:
           = 'A':  all N rows of V**T are returned in the array VT;
           = 'S':  the first min(m,n) rows of V**T (the right singular
                   vectors) are returned in the array VT;
           = 'O':  the first min(m,n) rows of V**T (the right singular
                   vectors) are overwritten on the array A;
           = 'N':  no rows of V**T (no right singular vectors) are
                   computed.
 
           JOBVT and JOBU cannot both be 'O'.
 
   M       (input) INTEGER
           The number of rows of the input matrix A.  M >= 0.
 
   N       (input) INTEGER
           The number of columns of the input matrix A.  N >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
           On entry, the M-by-N matrix A.
           On exit,
           if JOBU = 'O',  A is overwritten with the first min(m,n)
                           columns of U (the left singular vectors,
                           stored columnwise);
           if JOBVT = 'O', A is overwritten with the first min(m,n)
                           rows of V**T (the right singular vectors,
                           stored rowwise);
           if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
                           are destroyed.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,M).
 
   S       (output) DOUBLE PRECISION array, dimension (min(M,N))
           The singular values of A, sorted so that S(i) >= S(i+1).
 
   U       (output) DOUBLE PRECISION array, dimension (LDU,UCOL)
           (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
           If JOBU = 'A', U contains the M-by-M orthogonal matrix U;
           if JOBU = 'S', U contains the first min(m,n) columns of U
           (the left singular vectors, stored columnwise);
           if JOBU = 'N' or 'O', U is not referenced.
 
   LDU     (input) INTEGER
           The leading dimension of the array U.  LDU >= 1; if
           JOBU = 'S' or 'A', LDU >= M.
 
   VT      (output) DOUBLE PRECISION array, dimension (LDVT,N)
           If JOBVT = 'A', VT contains the N-by-N orthogonal matrix
           V**T;
           if JOBVT = 'S', VT contains the first min(m,n) rows of
           V**T (the right singular vectors, stored rowwise);
           if JOBVT = 'N' or 'O', VT is not referenced.
 
   LDVT    (input) INTEGER
           The leading dimension of the array VT.  LDVT >= 1; if
           JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
 
   WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK;
           if INFO > 0, WORK(2:MIN(M,N)) contains the unconverged
           superdiagonal elements of an upper bidiagonal matrix B
           whose diagonal is in S (not necessarily sorted). B
           satisfies A = U * B * VT, so it has the same singular values
           as A, and singular vectors related by U and VT.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK. LWORK >= 1.
           LWORK >= MAX(3*MIN(M,N)+MAX(M,N),5*MIN(M,N)).
           For good performance, LWORK should generally be larger.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
           = 0:  successful exit.
           < 0:  if INFO = -i, the i-th argument had an illegal value.
           > 0:  if DBDSQR did not converge, INFO specifies how many
                 superdiagonals of an intermediate bidiagonal form B
                 did not converge to zero. See the description of WORK
                 above for details.
 
"
  (jobu :string :input)
  (jobvt :string :input)
  (m :integer :input)
  (n :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (s (* :double-float) :output)
  (u (* :double-float) :output)
  (ldu :integer :input)
  (vt (* :double-float) :output)
  (ldvt :integer :input)
  (work (* :double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output)
)

(def-fortran-routine zgesv :void
"
   -- LAPACK driver routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      March 31, 1993

   Purpose
   =======
 
   ZGESV computes the solution to a complex system of linear equations
      A * X = B,
   where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
 
   The LU decomposition with partial pivoting and row interchanges is
   used to factor A as
      A = P * L * U,
   where P is a permutation matrix, L is unit lower triangular, and U is
   upper triangular.  The factored form of A is then used to solve the
   system of equations A * X = B.
 
   Arguments
   =========
 
   N       (input) INTEGER
           The number of linear equations, i.e., the order of the
           matrix A.  N >= 0.
 
   NRHS    (input) INTEGER
           The number of right hand sides, i.e., the number of columns
           of the matrix B.  NRHS >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
           On entry, the N-by-N coefficient matrix A.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,N).
 
   IPIV    (output) INTEGER array, dimension (N)
           The pivot indices that define the permutation matrix P;
           row i of the matrix was interchanged with row IPIV(i).
 
   B       (input/output) COMPLEX*16 array, dimension (LDB,NRHS)
           On entry, the N-by-NRHS matrix of right hand side matrix B.
           On exit, if INFO = 0, the N-by-NRHS solution matrix X.
 
   LDB     (input) INTEGER
           The leading dimension of the array B.  LDB >= max(1,N).
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
           > 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
                 has been completed, but the factor U is exactly
                 singular, so the solution could not be computed.
 
"
  (n :integer :input)
  (nrhs :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (ipiv (* :integer) :output)
  (b (* :complex-double-float) :input-output)
  (ldb :integer :input)
  (info :integer :output)
)

(def-fortran-routine zgeev :void
"
   -- LAPACK driver routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999

   Purpose
   =======
 
   ZGEEV computes for an N-by-N complex nonsymmetric matrix A, the
   eigenvalues and, optionally, the left and/or right eigenvectors.
 
   The right eigenvector v(j) of A satisfies
                    A * v(j) = lambda(j) * v(j)
   where lambda(j) is its eigenvalue.
   The left eigenvector u(j) of A satisfies
                 u(j)**H * A = lambda(j) * u(j)**H
   where u(j)**H denotes the conjugate transpose of u(j).
 
   The computed eigenvectors are normalized to have Euclidean norm
   equal to 1 and largest component real.
 
   Arguments
   =========
 
   JOBVL   (input) CHARACTER*1
           = 'N': left eigenvectors of A are not computed;
           = 'V': left eigenvectors of are computed.
 
   JOBVR   (input) CHARACTER*1
           = 'N': right eigenvectors of A are not computed;
           = 'V': right eigenvectors of A are computed.
 
   N       (input) INTEGER
           The order of the matrix A. N >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
           On entry, the N-by-N matrix A.
           On exit, A has been overwritten.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,N).
 
   W       (output) COMPLEX*16 array, dimension (N)
           W contains the computed eigenvalues.
 
   VL      (output) COMPLEX*16 array, dimension (LDVL,N)
           If JOBVL = 'V', the left eigenvectors u(j) are stored one
           after another in the columns of VL, in the same order
           as their eigenvalues.
           If JOBVL = 'N', VL is not referenced.
           u(j) = VL(:,j), the j-th column of VL.
 
   LDVL    (input) INTEGER
           The leading dimension of the array VL.  LDVL >= 1; if
           JOBVL = 'V', LDVL >= N.
 
   VR      (output) COMPLEX*16 array, dimension (LDVR,N)
           If JOBVR = 'V', the right eigenvectors v(j) are stored one
           after another in the columns of VR, in the same order
           as their eigenvalues.
           If JOBVR = 'N', VR is not referenced.
           v(j) = VR(:,j), the j-th column of VR.
 
   LDVR    (input) INTEGER
           The leading dimension of the array VR.  LDVR >= 1; if
           JOBVR = 'V', LDVR >= N.
 
   WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK.  LWORK >= max(1,2*N).
           For good performance, LWORK must generally be larger.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value.
           > 0:  if INFO = i, the QR algorithm failed to compute all the
                 eigenvalues, and no eigenvectors have been computed;
                 elements and i+1:N of W contain eigenvalues which have
                 converged.
 
"
  (jobvl :string :input)
  (jobvr :string :input)
  (n :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (w (* :complex-double-float) :output)
  (vl (* :complex-double-float) :output)
  (ldvl :integer :input)
  (vr (* :complex-double-float) :output)
  (ldvr :integer :input)
  (work (* :complex-double-float) :workspace-output)
  (lwork :integer :input)
  (rwork (* :double-float) :workspace)
  (info :integer :output)
)

(def-fortran-routine zgetrf :void
"
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      September 30, 1994

   Purpose
   =======
 
   ZGETRF computes an LU factorization of a general M-by-N matrix A
   using partial pivoting with row interchanges.
 
   The factorization has the form
      A = P * L * U
   where P is a permutation matrix, L is lower triangular with unit
   diagonal elements (lower trapezoidal if m > n), and U is upper
   triangular (upper trapezoidal if m < n).
 
   This is the right-looking Level 3 BLAS version of the algorithm.
 
   Arguments
   =========
 
   M       (input) INTEGER
           The number of rows of the matrix A.  M >= 0.
 
   N       (input) INTEGER
           The number of columns of the matrix A.  N >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
           On entry, the M-by-N matrix to be factored.
           On exit, the factors L and U from the factorization
           A = P*L*U; the unit diagonal elements of L are not stored.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,M).
 
   IPIV    (output) INTEGER array, dimension (min(M,N))
           The pivot indices; for 1 <= i <= min(M,N), row i of the
           matrix was interchanged with row IPIV(i).
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
           > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
                 has been completed, but the factor U is exactly
                 singular, and division by zero will occur if it is used
                 to solve a system of equations.
 
"
  (m :integer :input)
  (n :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (ipiv (* :integer) :output)
  (info :integer :output)
)

(def-fortran-routine zgesvd :void
"
   -- LAPACK driver routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      October 31, 1999

   Purpose
   =======
 
   ZGESVD computes the singular value decomposition (SVD) of a complex
   M-by-N matrix A, optionally computing the left and/or right singular
   vectors. The SVD is written
 
        A = U * SIGMA * conjugate-transpose(V)
 
   where SIGMA is an M-by-N matrix which is zero except for its
   min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
   V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
   are the singular values of A; they are real and non-negative, and
   are returned in descending order.  The first min(m,n) columns of
   U and V are the left and right singular vectors of A.
 
   Note that the routine returns V**H, not V.
 
   Arguments
   =========
 
   JOBU    (input) CHARACTER*1
           Specifies options for computing all or part of the matrix U:
           = 'A':  all M columns of U are returned in array U:
           = 'S':  the first min(m,n) columns of U (the left singular
                   vectors) are returned in the array U;
           = 'O':  the first min(m,n) columns of U (the left singular
                   vectors) are overwritten on the array A;
           = 'N':  no columns of U (no left singular vectors) are
                   computed.
 
   JOBVT   (input) CHARACTER*1
           Specifies options for computing all or part of the matrix
           V**H:
           = 'A':  all N rows of V**H are returned in the array VT;
           = 'S':  the first min(m,n) rows of V**H (the right singular
                   vectors) are returned in the array VT;
           = 'O':  the first min(m,n) rows of V**H (the right singular
                   vectors) are overwritten on the array A;
           = 'N':  no rows of V**H (no right singular vectors) are
                   computed.
 
           JOBVT and JOBU cannot both be 'O'.
 
   M       (input) INTEGER
           The number of rows of the input matrix A.  M >= 0.
 
   N       (input) INTEGER
           The number of columns of the input matrix A.  N >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
           On entry, the M-by-N matrix A.
           On exit,
           if JOBU = 'O',  A is overwritten with the first min(m,n)
                           columns of U (the left singular vectors,
                           stored columnwise);
           if JOBVT = 'O', A is overwritten with the first min(m,n)
                           rows of V**H (the right singular vectors,
                           stored rowwise);
           if JOBU .ne. 'O' and JOBVT .ne. 'O', the contents of A
                           are destroyed.
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,M).
 
   S       (output) DOUBLE PRECISION array, dimension (min(M,N))
           The singular values of A, sorted so that S(i) >= S(i+1).
 
   U       (output) COMPLEX*16 array, dimension (LDU,UCOL)
           (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
           If JOBU = 'A', U contains the M-by-M unitary matrix U;
           if JOBU = 'S', U contains the first min(m,n) columns of U
           (the left singular vectors, stored columnwise);
           if JOBU = 'N' or 'O', U is not referenced.
 
   LDU     (input) INTEGER
           The leading dimension of the array U.  LDU >= 1; if
           JOBU = 'S' or 'A', LDU >= M.
 
   VT      (output) COMPLEX*16 array, dimension (LDVT,N)
           If JOBVT = 'A', VT contains the N-by-N unitary matrix
           V**H;
           if JOBVT = 'S', VT contains the first min(m,n) rows of
           V**H (the right singular vectors, stored rowwise);
           if JOBVT = 'N' or 'O', VT is not referenced.
 
   LDVT    (input) INTEGER
           The leading dimension of the array VT.  LDVT >= 1; if
           JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
 
   WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK. LWORK >= 1.
           LWORK >=  2*MIN(M,N)+MAX(M,N).
           For good performance, LWORK should generally be larger.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   RWORK   (workspace) DOUBLE PRECISION array, dimension (5*min(M,N))
           On exit, if INFO > 0, RWORK(1:MIN(M,N)-1) contains the
           unconverged superdiagonal elements of an upper bidiagonal
           matrix B whose diagonal is in S (not necessarily sorted).
           B satisfies A = U * B * VT, so it has the same singular
           values as A, and singular vectors related by U and VT.
 
   INFO    (output) INTEGER
           = 0:  successful exit.
           < 0:  if INFO = -i, the i-th argument had an illegal value.
           > 0:  if ZBDSQR did not converge, INFO specifies how many
                 superdiagonals of an intermediate bidiagonal form B
                 did not converge to zero. See the description of RWORK
                 above for details.
 
"
  (jobu :string :input)
  (jobvt :string :input)
  (m :integer :input)
  (n :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (s (* :double-float) :output)
  (u (* :complex-double-float) :output)
  (ldu :integer :input)
  (vt (* :complex-double-float) :output)
  (ldvt :integer :input)
  (work (* :complex-double-float) :workspace-output)
  (lwork :integer :input)
  (rwork (* :double-float) :workspace)
  (info :integer :output)
)

(def-fortran-routine zgeqrf :void
  "
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999
 
   Purpose
   =======
 
   ZGEQRF computes a QR factorization of a complex M-by-N matrix A:
   A = Q * R.
 
   Arguments
   =========
 
   M       (input) INTEGER
           The number of rows of the matrix A.  M >= 0.
 
   N       (input) INTEGER
           The number of columns of the matrix A.  N >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
           On entry, the M-by-N matrix A.
           On exit, the elements on and above the diagonal of the array
           contain the min(M,N)-by-N upper trapezoidal matrix R (R is
           upper triangular if m >= n); the elements below the diagonal,
           with the array TAU, represent the unitary matrix Q as a
           product of min(m,n) elementary reflectors (see Further
           Details).
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,M).
 
   TAU     (output) COMPLEX*16 array, dimension (min(M,N))
           The scalar factors of the elementary reflectors (see Further
           Details).
 
   WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK.  LWORK >= max(1,N).
           For optimum performance LWORK >= N*NB, where NB is
           the optimal blocksize.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
 
   Further Details
   ===============
 
   The matrix Q is represented as a product of elementary reflectors
 
      Q = H(1) H(2) . . . H(k), where k = min(m,n).
 
   Each H(i) has the form
 
      H(i) = I - tau * v * v'
 
   where tau is a complex scalar, and v is a complex vector with
   v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
   and tau in TAU(i).
"
  (m :integer :input)
  (n :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (tau (* :complex-double-float) :workspace-output)
  (work (* :complex-double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output))

(def-fortran-routine zungqr :void
  "
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999
 
   Purpose
   =======
 
   ZUNGQR generates an M-by-N complex matrix Q with orthonormal columns,
   which is defined as the first N columns of a product of K elementary
   reflectors of order M
 
         Q  =  H(1) H(2) . . . H(k)
 
   as returned by ZGEQRF.
 
   Arguments
   =========
 
   M       (input) INTEGER
           The number of rows of the matrix Q. M >= 0.
 
   N       (input) INTEGER
           The number of columns of the matrix Q. M >= N >= 0.
 
   K       (input) INTEGER
           The number of elementary reflectors whose product defines the
           matrix Q. N >= K >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
           On entry, the i-th column must contain the vector which
           defines the elementary reflector H(i), for i = 1,2,...,k, as
           returned by ZGEQRF in the first k columns of its array
           argument A.
           On exit, the M-by-N matrix Q.
 
   LDA     (input) INTEGER
           The first dimension of the array A. LDA >= max(1,M).
 
   TAU     (input) COMPLEX*16 array, dimension (K)
           TAU(i) must contain the scalar factor of the elementary
           reflector H(i), as returned by ZGEQRF.
 
   WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK. LWORK >= max(1,N).
           For optimum performance LWORK >= N*NB, where NB is the
           optimal blocksize.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument has an illegal value
 
"
  (m :integer :input)
  (n :integer :input)
  (k :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (tau (* :complex-double-float) :input)
  (work (* :complex-double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output))
  
(def-fortran-routine dgeqrf :void
  "
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999
 
      .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
      ..
      .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
      ..
 
   Purpose
   =======
 
   DGEQRF computes a QR factorization of a real M-by-N matrix A:
   A = Q * R.
 
   Arguments
   =========
 
   M       (input) INTEGER
           The number of rows of the matrix A.  M >= 0.
 
   N       (input) INTEGER
           The number of columns of the matrix A.  N >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
           On entry, the M-by-N matrix A.
           On exit, the elements on and above the diagonal of the array
           contain the min(M,N)-by-N upper trapezoidal matrix R (R is
           upper triangular if m >= n); the elements below the diagonal,
           with the array TAU, represent the orthogonal matrix Q as a
           product of min(m,n) elementary reflectors (see Further
           Details).
 
   LDA     (input) INTEGER
           The leading dimension of the array A.  LDA >= max(1,M).
 
   TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
           The scalar factors of the elementary reflectors (see Further
           Details).
 
   WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
           On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
           The dimension of the array WORK.  LWORK >= max(1,N).
           For optimum performance LWORK >= N*NB, where NB is
           the optimal blocksize.
 
           If LWORK = -1, then a workspace query is assumed; the routine
           only calculates the optimal size of the WORK array, returns
           this value as the first entry of the WORK array, and no error
           message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
           = 0:  successful exit
           < 0:  if INFO = -i, the i-th argument had an illegal value
 
   Further Details
   ===============
 
   The matrix Q is represented as a product of elementary reflectors
 
      Q = H(1) H(2) . . . H(k), where k = min(m,n).
 
   Each H(i) has the form
 
      H(i) = I - tau * v * v'
 
   where tau is a real scalar, and v is a real vector with
   v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in A(i+1:m,i),
   and tau in TAU(i).
 
"
  (m :integer :input)
  (n :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (tau (* :double-float) :workspace-output)
  (work (* :double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output))

(def-fortran-routine dorgqr :void
  "
      SUBROUTINE DORGQR( M, N, K, A, LDA, TAU, WORK, LWORK, INFO )

   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999
 
      .. Scalar Arguments ..
      INTEGER            INFO, K, LDA, LWORK, M, N
      ..
      .. Array Arguments ..
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
      ..
 
   Purpose
   =======
 
   DORGQR generates an M-by-N real matrix Q with orthonormal columns,
   which is defined as the first N columns of a product of K elementary
   reflectors of order M
 
 	 Q  =  H(1) H(2) . . . H(k)
 
   as returned by DGEQRF.
 
   Arguments
   =========
 
   M       (input) INTEGER
 	   The number of rows of the matrix Q. M >= 0.
 
   N       (input) INTEGER
 	   The number of columns of the matrix Q. M >= N >= 0.
 
   K       (input) INTEGER
 	   The number of elementary reflectors whose product defines the
 	   matrix Q. N >= K >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
 	   On entry, the i-th column must contain the vector which
 	   defines the elementary reflector H(i), for i = 1,2,...,k, as
 	   returned by DGEQRF in the first k columns of its array
 	   argument A.
 	   On exit, the M-by-N matrix Q.
 
   LDA     (input) INTEGER
 	   The first dimension of the array A. LDA >= max(1,M).
 
   TAU     (input) DOUBLE PRECISION array, dimension (K)
 	   TAU(i) must contain the scalar factor of the elementary
 	   reflector H(i), as returned by DGEQRF.
 
   WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
 	   On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
 	   The dimension of the array WORK. LWORK >= max(1,N).
 	   For optimum performance LWORK >= N*NB, where NB is the
 	   optimal blocksize.
 
 	   If LWORK = -1, then a workspace query is assumed; the routine
 	   only calculates the optimal size of the WORK array, returns
 	   this value as the first entry of the WORK array, and no error
 	   message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
 	   = 0:  successful exit
 	   < 0:  if INFO = -i, the i-th argument has an illegal value
"
  (m :integer :input)
  (n :integer :input)
  (k :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (tau (* :double-float) :input)
  (work (* :double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output))

(def-fortran-routine dgeqp3 :void
"
      SUBROUTINE DGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO )
 
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999
 
      .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
      ..
      .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   A( LDA, * ), TAU( * ), WORK( * )
      ..
 
   Purpose
   =======
 
   DGEQP3 computes a QR factorization with column pivoting of a
   matrix A:  A*P = Q*R  using Level 3 BLAS.
 
   Arguments
   =========
 
   M       (input) INTEGER
 	   The number of rows of the matrix A. M >= 0.
 
   N       (input) INTEGER
 	   The number of columns of the matrix A.  N >= 0.
 
   A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
 	   On entry, the M-by-N matrix A.
 	   On exit, the upper triangle of the array contains the
 	   min(M,N)-by-N upper trapezoidal matrix R; the elements below
 	   the diagonal, together with the array TAU, represent the
 	   orthogonal matrix Q as a product of min(M,N) elementary
 	   reflectors.
 
   LDA     (input) INTEGER
 	   The leading dimension of the array A. LDA >= max(1,M).
 
   JPVT    (input/output) INTEGER array, dimension (N)
 	   On entry, if JPVT(J).ne.0, the J-th column of A is permuted
 	   to the front of A*P (a leading column); if JPVT(J)=0,
 	   the J-th column of A is a free column.
 	   On exit, if JPVT(J)=K, then the J-th column of A*P was the
 	   the K-th column of A.
 
   TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
 	   The scalar factors of the elementary reflectors.
 
   WORK    (workspace/output) DOUBLE PRECISION array, dimension (LWORK)
 	   On exit, if INFO=0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
 	   The dimension of the array WORK. LWORK >= 3*N+1.
 	   For optimal performance LWORK >= 2*N+( N+1 )*NB, where NB
 	   is the optimal blocksize.
 
 	   If LWORK = -1, then a workspace query is assumed; the routine
 	   only calculates the optimal size of the WORK array, returns
 	   this value as the first entry of the WORK array, and no error
 	   message related to LWORK is issued by XERBLA.
 
   INFO    (output) INTEGER
 	   = 0: successful exit.
 	   < 0: if INFO = -i, the i-th argument had an illegal value.
 
   Further Details
   ===============
 
   The matrix Q is represented as a product of elementary reflectors
 
      Q = H(1) H(2) . . . H(k), where k = min(m,n).
 
   Each H(i) has the form
 
      H(i) = I - tau * v * v'
 
   where tau is a real/complex scalar, and v is a real/complex vector
   with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
   A(i+1:m,i), and tau in TAU(i).
 
   Based on contributions by
     G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
     X. Sun, Computer Science Dept., Duke University, USA
" 

  (m :integer :input)
  (n :integer :input)
  (a (* :double-float) :input-output)
  (lda :integer :input)
  (jpvt (* :integer) :input-output)
  (tau (* :double-float) :workspace-output)
  (work (* :double-float) :workspace-output)
  (lwork :integer :input)
  (info :integer :output))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def-fortran-routine zgeqp3 :void
  "
      SUBROUTINE ZGEQP3( M, N, A, LDA, JPVT, TAU, WORK, LWORK, RWORK,
     $                   INFO )
 
   -- LAPACK routine (version 3.0) --
      Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
      Courant Institute, Argonne National Lab, and Rice University
      June 30, 1999
 
      .. Scalar Arguments ..
      INTEGER            INFO, LDA, LWORK, M, N
      ..
      .. Array Arguments ..
      INTEGER            JPVT( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( LDA, * ), TAU( * ), WORK( * )
      ..
 
   Purpose
   =======
 
   ZGEQP3 computes a QR factorization with column pivoting of a
   matrix A:  A*P = Q*R  using Level 3 BLAS.
 
   Arguments
   =========
 
   M       (input) INTEGER
 	   The number of rows of the matrix A. M >= 0.
 
   N       (input) INTEGER
 	   The number of columns of the matrix A.  N >= 0.
 
   A       (input/output) COMPLEX*16 array, dimension (LDA,N)
 	   On entry, the M-by-N matrix A.
 	   On exit, the upper triangle of the array contains the
 	   min(M,N)-by-N upper trapezoidal matrix R; the elements below
 	   the diagonal, together with the array TAU, represent the
 	   unitary matrix Q as a product of min(M,N) elementary
 	   reflectors.
 
   LDA     (input) INTEGER
 	   The leading dimension of the array A. LDA >= max(1,M).
 
   JPVT    (input/output) INTEGER array, dimension (N)
 	   On entry, if JPVT(J).ne.0, the J-th column of A is permuted
 	   to the front of A*P (a leading column); if JPVT(J)=0,
 	   the J-th column of A is a free column.
 	   On exit, if JPVT(J)=K, then the J-th column of A*P was the
 	   the K-th column of A.
 
   TAU     (output) COMPLEX*16 array, dimension (min(M,N))
 	   The scalar factors of the elementary reflectors.
 
   WORK    (workspace/output) COMPLEX*16 array, dimension (LWORK)
 	   On exit, if INFO=0, WORK(1) returns the optimal LWORK.
 
   LWORK   (input) INTEGER
 	   The dimension of the array WORK. LWORK >= N+1.
 	   For optimal performance LWORK >= ( N+1 )*NB, where NB
 	   is the optimal blocksize.
 
 	   If LWORK = -1, then a workspace query is assumed; the routine
 	   only calculates the optimal size of the WORK array, returns
 	   this value as the first entry of the WORK array, and no error
 	   message related to LWORK is issued by XERBLA.
 
   RWORK   (workspace) DOUBLE PRECISION array, dimension (2*N)
 
   INFO    (output) INTEGER
 	   = 0: successful exit.
 	   < 0: if INFO = -i, the i-th argument had an illegal value.
 
   Further Details
   ===============
 
   The matrix Q is represented as a product of elementary reflectors
 
      Q = H(1) H(2) . . . H(k), where k = min(m,n).
 
   Each H(i) has the form
 
      H(i) = I - tau * v * v'
 
   where tau is a real/complex scalar, and v is a real/complex vector
   with v(1:i-1) = 0 and v(i) = 1; v(i+1:m) is stored on exit in
   A(i+1:m,i), and tau in TAU(i).
 
   Based on contributions by
     G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
     X. Sun, Computer Science Dept., Duke University, USA
"
  (m :integer :input)
  (n :integer :input)
  (a (* :complex-double-float) :input-output)
  (lda :integer :input)
  (jpvt (* :integer) :input-output)
  (tau (* :complex-double-float) :workspace-output)
  (work (* :complex-double-float) :workspace-output)
  (lwork :integer :input)
  (rwork (* :double-float) :workspace-output)
  (info :integer :output))
