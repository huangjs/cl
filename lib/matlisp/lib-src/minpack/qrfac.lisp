;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :minpack)


(let ((one 1.0d0) (p05 0.05d0) (zero 0.0d0))
  (declare (type double-float zero p05 one))
  (defun qrfac (m n a lda pivot ipvt lipvt rdiag acnorm wa)
    (declare (type (array f2cl-lib:integer4 (*)) ipvt)
             (type f2cl-lib:logical pivot)
             (type (array double-float (*)) wa acnorm rdiag a)
             (type f2cl-lib:integer4 lipvt lda n m))
    (f2cl-lib:with-array-data (a-%data% a-%offset% a)
      (declare (type f2cl-lib:integer4 a-%offset%)
               (type (simple-array double-float (*)) a-%data%)
               (ignorable a-%offset% a-%data%))
      (f2cl-lib:with-array-data (rdiag-%data% rdiag-%offset% rdiag)
        (declare (type f2cl-lib:integer4 rdiag-%offset%)
                 (type (simple-array double-float (*)) rdiag-%data%)
                 (ignorable rdiag-%offset% rdiag-%data%))
        (f2cl-lib:with-array-data (acnorm-%data% acnorm-%offset% acnorm)
          (declare (type f2cl-lib:integer4 acnorm-%offset%)
                   (type (simple-array double-float (*)) acnorm-%data%)
                   (ignorable acnorm-%offset% acnorm-%data%))
          (f2cl-lib:with-array-data (wa-%data% wa-%offset% wa)
            (declare (type f2cl-lib:integer4 wa-%offset%)
                     (type (simple-array double-float (*)) wa-%data%)
                     (ignorable wa-%offset% wa-%data%))
            (f2cl-lib:with-array-data (ipvt-%data% ipvt-%offset% ipvt)
              (declare (type f2cl-lib:integer4 ipvt-%offset%)
                       (type (simple-array f2cl-lib:integer4 (*)) ipvt-%data%)
                       (ignorable ipvt-%offset% ipvt-%data%))
              (prog ((ajnorm 0.0d0) (epsmch 0.0d0) (sum 0.0d0) (temp 0.0d0)
                     (i 0) (j 0) (jp1 0) (k 0) (kmax 0) (minmn 0)
                     (double-float 0.0) (f2cl-lib:array-slice 0.0))
                (declare (type single-float f2cl-lib:array-slice double-float)
                         (type f2cl-lib:integer4 minmn kmax k jp1 j i)
                         (type double-float temp sum epsmch ajnorm))
                '"     **********"
                '""
                '"     subroutine qrfac"
                '""
                '"     this subroutine uses householder transformations with column"
                '"     pivoting (optional) to compute a qr factorization of the"
                '"     m by n matrix a. that is, qrfac determines an orthogonal"
                '"     matrix q, a permutation matrix p, and an upper trapezoidal"
                '"     matrix r with diagonal elements of nonincreasing magnitude,"
                '"     such that a*p = q*r. the householder transformation for"
                '"     column k, k = 1,2,...,min(m,n), is of the form"
                '""
                '"                           t"
                '"           i - (1/u(k))*u*u"
                '""
                '"     where u has zeros in the first k-1 positions. the form of"
                '"     this transformation and the method of pivoting first"
                '"     appeared in the corresponding linpack subroutine."
                '""
                '"     the subroutine statement is"
                '""
                '"       subroutine qrfac(m,n,a,lda,pivot,ipvt,lipvt,rdiag,acnorm,wa)"
                '""
                '"     where"
                '""
                '"       m is a positive integer input variable set to the number"
                '"         of rows of a."
                '""
                '"       n is a positive integer input variable set to the number"
                '"         of columns of a."
                '""
                '"       a is an m by n array. on input a contains the matrix for"
                '"         which the qr factorization is to be computed. on output"
                '"         the strict upper trapezoidal part of a contains the strict"
                '"         upper trapezoidal part of r, and the lower trapezoidal"
                '"         part of a contains a factored form of q (the non-trivial"
                '"         elements of the u vectors described above)."
                '""
                '"       lda is a positive integer input variable not less than m"
                '"         which specifies the leading dimension of the array a."
                '""
                '"       pivot is a logical input variable. if pivot is set true,"
                '"         then column pivoting is enforced. if pivot is set false,"
                '"         then no column pivoting is done."
                '""
                '"       ipvt is an integer output array of length lipvt. ipvt"
                '"         defines the permutation matrix p such that a*p = q*r."
                '"         column j of p is column ipvt(j) of the identity matrix."
                '"         if pivot is false, ipvt is not referenced."
                '""
                '"       lipvt is a positive integer input variable. if pivot is false,"
                '"         then lipvt may be as small as 1. if pivot is true, then"
                '"         lipvt must be at least n."
                '""
                '"       rdiag is an output array of length n which contains the"
                '"         diagonal elements of r."
                '""
                '"       acnorm is an output array of length n which contains the"
                '"         norms of the corresponding columns of the input matrix a."
                '"         if this information is not needed, then acnorm can coincide"
                '"         with rdiag."
                '""
                '"       wa is a work array of length n. if pivot is false, then wa"
                '"         can coincide with rdiag."
                '""
                '"     subprograms called"
                '""
                '"       minpack-supplied ... dpmpar,enorm"
                '""
                '"       fortran-supplied ... dmax1,dsqrt,min0"
                '""
                '"     argonne national laboratory. minpack project. march 1980."
                '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
                '""
                '"     **********"
                '""
                '"     epsmch is the machine precision."
                '""
                (setf epsmch (dpmpar 1))
                '""
                '"     compute the initial column norms and initialize several arrays."
                '""
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref acnorm-%data% (j) ((1 n)) acnorm-%offset%)
                     (enorm m
                      (f2cl-lib:array-slice a
                                            double-float
                                            (1 j)
                                            ((1 lda) (1 n)))))
                    (f2cl-lib:fset
                     (f2cl-lib:fref rdiag-%data% (j) ((1 n)) rdiag-%offset%)
                     (f2cl-lib:fref acnorm-%data% (j) ((1 n)) acnorm-%offset%))
                    (f2cl-lib:fset
                     (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%)
                     (f2cl-lib:fref rdiag-%data% (j) ((1 n)) rdiag-%offset%))
                    (if pivot
                        (f2cl-lib:fset
                         (f2cl-lib:fref ipvt-%data%
                                        (j)
                                        ((1 lipvt))
                                        ipvt-%offset%)
                         j))
                   label10))
                '""
                '"     reduce a to r with householder transformations."
                '""
                (setf minmn (f2cl-lib:min0 m n))
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j minmn) nil)
                  (tagbody
                    (if (not pivot) (go label40))
                    '""
                    '"        bring the column of largest norm into the pivot position."
                    '""
                    (setf kmax j)
                    (f2cl-lib:fdo (k j (f2cl-lib:int-add k 1))
                                  ((> k n) nil)
                      (tagbody
                        (if
                         (>
                          (f2cl-lib:fref rdiag-%data%
                                         (k)
                                         ((1 n))
                                         rdiag-%offset%)
                          (f2cl-lib:fref rdiag-%data%
                                         (kmax)
                                         ((1 n))
                                         rdiag-%offset%))
                         (setf kmax k))
                       label20))
                    (if (= kmax j) (go label40))
                    (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                  ((> i m) nil)
                      (tagbody
                        (setf temp
                                (f2cl-lib:fref a-%data%
                                               (i j)
                                               ((1 lda) (1 n))
                                               a-%offset%))
                        (f2cl-lib:fset
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 n))
                                        a-%offset%)
                         (f2cl-lib:fref a-%data%
                                        (i kmax)
                                        ((1 lda) (1 n))
                                        a-%offset%))
                        (f2cl-lib:fset
                         (f2cl-lib:fref a-%data%
                                        (i kmax)
                                        ((1 lda) (1 n))
                                        a-%offset%)
                         temp)
                       label30))
                    (f2cl-lib:fset
                     (f2cl-lib:fref rdiag-%data% (kmax) ((1 n)) rdiag-%offset%)
                     (f2cl-lib:fref rdiag-%data% (j) ((1 n)) rdiag-%offset%))
                    (f2cl-lib:fset
                     (f2cl-lib:fref wa-%data% (kmax) ((1 n)) wa-%offset%)
                     (f2cl-lib:fref wa-%data% (j) ((1 n)) wa-%offset%))
                    (setf k
                            (f2cl-lib:fref ipvt-%data%
                                           (j)
                                           ((1 lipvt))
                                           ipvt-%offset%))
                    (f2cl-lib:fset
                     (f2cl-lib:fref ipvt-%data% (j) ((1 lipvt)) ipvt-%offset%)
                     (f2cl-lib:fref ipvt-%data%
                                    (kmax)
                                    ((1 lipvt))
                                    ipvt-%offset%))
                    (f2cl-lib:fset
                     (f2cl-lib:fref ipvt-%data%
                                    (kmax)
                                    ((1 lipvt))
                                    ipvt-%offset%)
                     k)
                   label40
                    '""
                    '"        compute the householder transformation to reduce the"
                    '"        j-th column of a to a multiple of the j-th unit vector."
                    '""
                    (setf ajnorm
                            (enorm (f2cl-lib:int-add (f2cl-lib:int-sub m j) 1)
                             (f2cl-lib:array-slice a
                                                   double-float
                                                   (j j)
                                                   ((1 lda) (1 n)))))
                    (if (= ajnorm zero) (go label100))
                    (if
                     (<
                      (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 n)) a-%offset%)
                      zero)
                     (setf ajnorm (- ajnorm)))
                    (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                                  ((> i m) nil)
                      (tagbody
                        (f2cl-lib:fset
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 n))
                                        a-%offset%)
                         (/
                          (f2cl-lib:fref a-%data%
                                         (i j)
                                         ((1 lda) (1 n))
                                         a-%offset%)
                          ajnorm))
                       label50))
                    (f2cl-lib:fset
                     (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 n)) a-%offset%)
                     (+
                      (f2cl-lib:fref a-%data% (j j) ((1 lda) (1 n)) a-%offset%)
                      one))
                    '""
                    '"        apply the transformation to the remaining columns"
                    '"        and update the norms."
                    '""
                    (setf jp1 (f2cl-lib:int-add j 1))
                    (if (< n jp1) (go label100))
                    (f2cl-lib:fdo (k jp1 (f2cl-lib:int-add k 1))
                                  ((> k n) nil)
                      (tagbody
                        (setf sum zero)
                        (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (setf sum
                                    (+ sum
                                       (*
                                        (f2cl-lib:fref a-%data%
                                                       (i j)
                                                       ((1 lda) (1 n))
                                                       a-%offset%)
                                        (f2cl-lib:fref a-%data%
                                                       (i k)
                                                       ((1 lda) (1 n))
                                                       a-%offset%))))
                           label60))
                        (setf temp
                                (/ sum
                                   (f2cl-lib:fref a-%data%
                                                  (j j)
                                                  ((1 lda) (1 n))
                                                  a-%offset%)))
                        (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                                      ((> i m) nil)
                          (tagbody
                            (f2cl-lib:fset
                             (f2cl-lib:fref a-%data%
                                            (i k)
                                            ((1 lda) (1 n))
                                            a-%offset%)
                             (-
                              (f2cl-lib:fref a-%data%
                                             (i k)
                                             ((1 lda) (1 n))
                                             a-%offset%)
                              (* temp
                                 (f2cl-lib:fref a-%data%
                                                (i j)
                                                ((1 lda) (1 n))
                                                a-%offset%))))
                           label70))
                        (if
                         (or (not pivot)
                             (=
                              (f2cl-lib:fref rdiag-%data%
                                             (k)
                                             ((1 n))
                                             rdiag-%offset%)
                              zero))
                         (go label80))
                        (setf temp
                                (/
                                 (f2cl-lib:fref a-%data%
                                                (j k)
                                                ((1 lda) (1 n))
                                                a-%offset%)
                                 (f2cl-lib:fref rdiag-%data%
                                                (k)
                                                ((1 n))
                                                rdiag-%offset%)))
                        (f2cl-lib:fset
                         (f2cl-lib:fref rdiag-%data%
                                        (k)
                                        ((1 n))
                                        rdiag-%offset%)
                         (*
                          (f2cl-lib:fref rdiag-%data%
                                         (k)
                                         ((1 n))
                                         rdiag-%offset%)
                          (f2cl-lib:dsqrt
                           (f2cl-lib:dmax1 zero (- one (expt temp 2))))))
                        (if
                         (>
                          (* p05
                             (expt
                              (/
                               (f2cl-lib:fref rdiag-%data%
                                              (k)
                                              ((1 n))
                                              rdiag-%offset%)
                               (f2cl-lib:fref wa-%data%
                                              (k)
                                              ((1 n))
                                              wa-%offset%))
                              2))
                          epsmch)
                         (go label80))
                        (f2cl-lib:fset
                         (f2cl-lib:fref rdiag-%data%
                                        (k)
                                        ((1 n))
                                        rdiag-%offset%)
                         (enorm (f2cl-lib:int-sub m j)
                          (f2cl-lib:array-slice a
                                                double-float
                                                (jp1 k)
                                                ((1 lda) (1 n)))))
                        (f2cl-lib:fset
                         (f2cl-lib:fref wa-%data% (k) ((1 n)) wa-%offset%)
                         (f2cl-lib:fref rdiag-%data%
                                        (k)
                                        ((1 n))
                                        rdiag-%offset%))
                       label80
                       label90))
                   label100
                    (f2cl-lib:fset
                     (f2cl-lib:fref rdiag-%data% (j) ((1 n)) rdiag-%offset%)
                     (- ajnorm))
                   label110))
                (go end_label)
                '""
                '"     last card of subroutine qrfac."
                '""
               end_label
                (return
                 (values nil nil nil nil nil nil nil nil nil nil))))))))))

