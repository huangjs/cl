;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :minpack)


(let ((p1 0.1d0) (p001 0.001d0) (zero 0.0d0))
  (declare (type double-float zero p001 p1))
  (defun lmpar (n r ldr ipvt diag qtb delta par x sdiag wa1 wa2)
    (declare (type double-float par delta)
             (type (array f2cl-lib:integer4 (*)) ipvt)
             (type (array double-float (*)) wa2 wa1 sdiag x qtb diag r)
             (type f2cl-lib:integer4 ldr n))
    (f2cl-lib:with-array-data (r-%data% r-%offset% r)
      (declare (type f2cl-lib:integer4 r-%offset%)
               (type (simple-array double-float (*)) r-%data%)
               (ignorable r-%offset% r-%data%))
      (f2cl-lib:with-array-data (diag-%data% diag-%offset% diag)
        (declare (type f2cl-lib:integer4 diag-%offset%)
                 (type (simple-array double-float (*)) diag-%data%)
                 (ignorable diag-%offset% diag-%data%))
        (f2cl-lib:with-array-data (qtb-%data% qtb-%offset% qtb)
          (declare (type f2cl-lib:integer4 qtb-%offset%)
                   (type (simple-array double-float (*)) qtb-%data%)
                   (ignorable qtb-%offset% qtb-%data%))
          (f2cl-lib:with-array-data (x-%data% x-%offset% x)
            (declare (type f2cl-lib:integer4 x-%offset%)
                     (type (simple-array double-float (*)) x-%data%)
                     (ignorable x-%offset% x-%data%))
            (f2cl-lib:with-array-data (sdiag-%data% sdiag-%offset% sdiag)
              (declare (type f2cl-lib:integer4 sdiag-%offset%)
                       (type (simple-array double-float (*)) sdiag-%data%)
                       (ignorable sdiag-%offset% sdiag-%data%))
              (f2cl-lib:with-array-data (wa1-%data% wa1-%offset% wa1)
                (declare (type f2cl-lib:integer4 wa1-%offset%)
                         (type (simple-array double-float (*)) wa1-%data%)
                         (ignorable wa1-%offset% wa1-%data%))
                (f2cl-lib:with-array-data (wa2-%data% wa2-%offset% wa2)
                  (declare (type f2cl-lib:integer4 wa2-%offset%)
                           (type (simple-array double-float (*)) wa2-%data%)
                           (ignorable wa2-%offset% wa2-%data%))
                  (f2cl-lib:with-array-data (ipvt-%data% ipvt-%offset% ipvt)
                    (declare (type f2cl-lib:integer4 ipvt-%offset%)
                             (type (simple-array f2cl-lib:integer4 (*))
                              ipvt-%data%)
                             (ignorable ipvt-%offset% ipvt-%data%))
                    (prog ((dxnorm 0.0d0) (dwarf 0.0d0) (fp 0.0d0)
                           (gnorm 0.0d0) (parc 0.0d0) (parl 0.0d0) (paru 0.0d0)
                           (sum 0.0d0) (temp 0.0d0) (i 0) (iter 0) (j 0)
                           (jm1 0) (jp1 0) (k 0) (l 0) (nsing 0))
                      (declare
                       (type f2cl-lib:integer4 nsing l k jp1 jm1 j iter i)
                       (type double-float temp sum paru parl parc gnorm fp
                        dwarf dxnorm))
                      '"     **********"
                      '""
                      '"     subroutine lmpar"
                      '""
                      '"     given an m by n matrix a, an n by n nonsingular diagonal"
                      '"     matrix d, an m-vector b, and a positive number delta,"
                      '"     the problem is to determine a value for the parameter"
                      '"     par such that if x solves the system"
                      '""
                      '"           a*x = b ,     sqrt(par)*d*x = 0 ,"
                      '""
                      '"     in the least squares sense, and dxnorm is the euclidean"
                      '"     norm of d*x, then either par is zero and"
                      '""
                      '"           (dxnorm-delta) .le. 0.1*delta ,"
                      '""
                      '"     or par is positive and"
                      '""
                      '"           abs(dxnorm-delta) .le. 0.1*delta ."
                      '""
                      '"     this subroutine completes the solution of the problem"
                      '"     if it is provided with the necessary information from the"
                      '"     qr factorization, with column pivoting, of a. that is, if"
                      '"     a*p = q*r, where p is a permutation matrix, q has orthogonal"
                      '"     columns, and r is an upper triangular matrix with diagonal"
                      '"     elements of nonincreasing magnitude, then lmpar expects"
                      '"     the full upper triangle of r, the permutation matrix p,"
                      '"     and the first n components of (q transpose)*b. on output"
                      '"     lmpar also provides an upper triangular matrix s such that"
                      '""
                      '"            t   t                   t"
                      '"           p *(a *a + par*d*d)*p = s *s ."
                      '""
                      '"     s is employed within lmpar and may be of separate interest."
                      '""
                      '"     only a few iterations are generally needed for convergence"
                      '"     of the algorithm. if, however, the limit of 10 iterations"
                      '"     is reached, then the output par will contain the best"
                      '"     value obtained so far."
                      '""
                      '"     the subroutine statement is"
                      '""
                      '"       subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag,"
                      '"                        wa1,wa2)"
                      '""
                      '"     where"
                      '""
                      '"       n is a positive integer input variable set to the order of r."
                      '""
                      '"       r is an n by n array. on input the full upper triangle"
                      '"         must contain the full upper triangle of the matrix r."
                      '"         on output the full upper triangle is unaltered, and the"
                      '"         strict lower triangle contains the strict upper triangle"
                      '"         (transposed) of the upper triangular matrix s."
                      '""
                      '"       ldr is a positive integer input variable not less than n"
                      '"         which specifies the leading dimension of the array r."
                      '""
                      '"       ipvt is an integer input array of length n which defines the"
                      '"         permutation matrix p such that a*p = q*r. column j of p"
                      '"         is column ipvt(j) of the identity matrix."
                      '""
                      '"       diag is an input array of length n which must contain the"
                      '"         diagonal elements of the matrix d."
                      '""
                      '"       qtb is an input array of length n which must contain the first"
                      '"         n elements of the vector (q transpose)*b."
                      '""
                      '"       delta is a positive input variable which specifies an upper"
                      '"         bound on the euclidean norm of d*x."
                      '""
                      '"       par is a nonnegative variable. on input par contains an"
                      '"         initial estimate of the levenberg-marquardt parameter."
                      '"         on output par contains the final estimate."
                      '""
                      '"       x is an output array of length n which contains the least"
                      '"         squares solution of the system a*x = b, sqrt(par)*d*x = 0,"
                      '"         for the output par."
                      '""
                      '"       sdiag is an output array of length n which contains the"
                      '"         diagonal elements of the upper triangular matrix s."
                      '""
                      '"       wa1 and wa2 are work arrays of length n."
                      '""
                      '"     subprograms called"
                      '""
                      '"       minpack-supplied ... dpmpar,enorm,qrsolv"
                      '""
                      '"       fortran-supplied ... dabs,dmax1,dmin1,dsqrt"
                      '""
                      '"     argonne national laboratory. minpack project. march 1980."
                      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
                      '""
                      '"     **********"
                      '""
                      '"     dwarf is the smallest positive magnitude."
                      '""
                      (setf dwarf (dpmpar 2))
                      '""
                      '"     compute and store in x the gauss-newton direction. if the"
                      '"     jacobian is rank-deficient, obtain a least squares solution."
                      '""
                      (setf nsing n)
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (f2cl-lib:fref qtb-%data% (j) ((1 n)) qtb-%offset%))
                          (if
                           (and
                            (=
                             (f2cl-lib:fref r-%data%
                                            (j j)
                                            ((1 ldr) (1 n))
                                            r-%offset%)
                             zero)
                            (= nsing n))
                           (setf nsing (f2cl-lib:int-sub j 1)))
                          (if (< nsing n)
                              (f2cl-lib:fset
                               (f2cl-lib:fref wa1-%data%
                                              (j)
                                              ((1 n))
                                              wa1-%offset%)
                               zero))
                         label10))
                      (if (< nsing 1) (go label50))
                      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                    ((> k nsing) nil)
                        (tagbody
                          (setf j
                                  (f2cl-lib:int-add (f2cl-lib:int-sub nsing k)
                                                    1))
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (/
                            (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                            (f2cl-lib:fref r-%data%
                                           (j j)
                                           ((1 ldr) (1 n))
                                           r-%offset%)))
                          (setf temp
                                  (f2cl-lib:fref wa1-%data%
                                                 (j)
                                                 ((1 n))
                                                 wa1-%offset%))
                          (setf jm1 (f2cl-lib:int-sub j 1))
                          (if (< jm1 1) (go label30))
                          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                        ((> i jm1) nil)
                            (tagbody
                              (f2cl-lib:fset
                               (f2cl-lib:fref wa1-%data%
                                              (i)
                                              ((1 n))
                                              wa1-%offset%)
                               (-
                                (f2cl-lib:fref wa1-%data%
                                               (i)
                                               ((1 n))
                                               wa1-%offset%)
                                (*
                                 (f2cl-lib:fref r-%data%
                                                (i j)
                                                ((1 ldr) (1 n))
                                                r-%offset%)
                                 temp)))
                             label20))
                         label30
                         label40))
                     label50
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (setf l
                                  (f2cl-lib:fref ipvt-%data%
                                                 (j)
                                                 ((1 n))
                                                 ipvt-%offset%))
                          (f2cl-lib:fset
                           (f2cl-lib:fref x-%data% (l) ((1 n)) x-%offset%)
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%))
                         label60))
                      '""
                      '"     initialize the iteration counter."
                      '"     evaluate the function at the origin, and test"
                      '"     for acceptance of the gauss-newton direction."
                      '""
                      (setf iter 0)
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                           (*
                            (f2cl-lib:fref diag-%data%
                                           (j)
                                           ((1 n))
                                           diag-%offset%)
                            (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
                         label70))
                      (setf dxnorm (enorm n wa2))
                      (setf fp (- dxnorm delta))
                      (if (<= fp (* p1 delta)) (go label220))
                      '""
                      '"     if the jacobian is not rank deficient, the newton"
                      '"     step provides a lower bound, parl, for the zero of"
                      '"     the function. otherwise set this bound to zero."
                      '""
                      (setf parl zero)
                      (if (< nsing n) (go label120))
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (setf l
                                  (f2cl-lib:fref ipvt-%data%
                                                 (j)
                                                 ((1 n))
                                                 ipvt-%offset%))
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (*
                            (f2cl-lib:fref diag-%data%
                                           (l)
                                           ((1 n))
                                           diag-%offset%)
                            (/
                             (f2cl-lib:fref wa2-%data%
                                            (l)
                                            ((1 n))
                                            wa2-%offset%)
                             dxnorm)))
                         label80))
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (setf sum zero)
                          (setf jm1 (f2cl-lib:int-sub j 1))
                          (if (< jm1 1) (go label100))
                          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                        ((> i jm1) nil)
                            (tagbody
                              (setf sum
                                      (+ sum
                                         (*
                                          (f2cl-lib:fref r-%data%
                                                         (i j)
                                                         ((1 ldr) (1 n))
                                                         r-%offset%)
                                          (f2cl-lib:fref wa1-%data%
                                                         (i)
                                                         ((1 n))
                                                         wa1-%offset%))))
                             label90))
                         label100
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (/
                            (-
                             (f2cl-lib:fref wa1-%data%
                                            (j)
                                            ((1 n))
                                            wa1-%offset%)
                             sum)
                            (f2cl-lib:fref r-%data%
                                           (j j)
                                           ((1 ldr) (1 n))
                                           r-%offset%)))
                         label110))
                      (setf temp (enorm n wa1))
                      (setf parl (/ (/ (/ fp delta) temp) temp))
                     label120
                      '""
                      '"     calculate an upper bound, paru, for the zero of the function."
                      '""
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (setf sum zero)
                          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                        ((> i j) nil)
                            (tagbody
                              (setf sum
                                      (+ sum
                                         (*
                                          (f2cl-lib:fref r-%data%
                                                         (i j)
                                                         ((1 ldr) (1 n))
                                                         r-%offset%)
                                          (f2cl-lib:fref qtb-%data%
                                                         (i)
                                                         ((1 n))
                                                         qtb-%offset%))))
                             label130))
                          (setf l
                                  (f2cl-lib:fref ipvt-%data%
                                                 (j)
                                                 ((1 n))
                                                 ipvt-%offset%))
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (/ sum
                              (f2cl-lib:fref diag-%data%
                                             (l)
                                             ((1 n))
                                             diag-%offset%)))
                         label140))
                      (setf gnorm (enorm n wa1))
                      (setf paru (/ gnorm delta))
                      (if (= paru zero)
                          (setf paru (/ dwarf (f2cl-lib:dmin1 delta p1))))
                      '""
                      '"     if the input par lies outside of the interval (parl,paru),"
                      '"     set par to the closer endpoint."
                      '""
                      (setf par (f2cl-lib:dmax1 par parl))
                      (setf par (f2cl-lib:dmin1 par paru))
                      (if (= par zero) (setf par (/ gnorm dxnorm)))
                      '""
                      '"     beginning of an iteration."
                      '""
                     label150
                      (setf iter (f2cl-lib:int-add iter 1))
                      '""
                      '"        evaluate the function at the current value of par."
                      '""
                      (if (= par zero)
                          (setf par (f2cl-lib:dmax1 dwarf (* p001 paru))))
                      (setf temp (f2cl-lib:dsqrt par))
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (* temp
                              (f2cl-lib:fref diag-%data%
                                             (j)
                                             ((1 n))
                                             diag-%offset%)))
                         label160))
                      (qrsolv n r ldr ipvt wa1 qtb x sdiag wa2)
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                           (*
                            (f2cl-lib:fref diag-%data%
                                           (j)
                                           ((1 n))
                                           diag-%offset%)
                            (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
                         label170))
                      (setf dxnorm (enorm n wa2))
                      (setf temp fp)
                      (setf fp (- dxnorm delta))
                      '""
                      '"        if the function is small enough, accept the current value"
                      '"        of par. also test for the exceptional cases where parl"
                      '"        is zero or the number of iterations has reached 10."
                      '""
                      (if
                       (or (<= (f2cl-lib:dabs fp) (* p1 delta))
                           (and (= parl zero) (<= fp temp) (< temp zero))
                           (= iter 10))
                       (go label220))
                      '""
                      '"        compute the newton correction."
                      '""
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (setf l
                                  (f2cl-lib:fref ipvt-%data%
                                                 (j)
                                                 ((1 n))
                                                 ipvt-%offset%))
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (*
                            (f2cl-lib:fref diag-%data%
                                           (l)
                                           ((1 n))
                                           diag-%offset%)
                            (/
                             (f2cl-lib:fref wa2-%data%
                                            (l)
                                            ((1 n))
                                            wa2-%offset%)
                             dxnorm)))
                         label180))
                      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                    ((> j n) nil)
                        (tagbody
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           (/
                            (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                            (f2cl-lib:fref sdiag-%data%
                                           (j)
                                           ((1 n))
                                           sdiag-%offset%)))
                          (setf temp
                                  (f2cl-lib:fref wa1-%data%
                                                 (j)
                                                 ((1 n))
                                                 wa1-%offset%))
                          (setf jp1 (f2cl-lib:int-add j 1))
                          (if (< n jp1) (go label200))
                          (f2cl-lib:fdo (i jp1 (f2cl-lib:int-add i 1))
                                        ((> i n) nil)
                            (tagbody
                              (f2cl-lib:fset
                               (f2cl-lib:fref wa1-%data%
                                              (i)
                                              ((1 n))
                                              wa1-%offset%)
                               (-
                                (f2cl-lib:fref wa1-%data%
                                               (i)
                                               ((1 n))
                                               wa1-%offset%)
                                (*
                                 (f2cl-lib:fref r-%data%
                                                (i j)
                                                ((1 ldr) (1 n))
                                                r-%offset%)
                                 temp)))
                             label190))
                         label200
                         label210))
                      (setf temp (enorm n wa1))
                      (setf parc (/ (/ (/ fp delta) temp) temp))
                      '""
                      '"        depending on the sign of the function, update parl or paru."
                      '""
                      (if (> fp zero) (setf parl (f2cl-lib:dmax1 parl par)))
                      (if (< fp zero) (setf paru (f2cl-lib:dmin1 paru par)))
                      '""
                      '"        compute an improved estimate for par."
                      '""
                      (setf par (f2cl-lib:dmax1 parl (+ par parc)))
                      '""
                      '"        end of an iteration."
                      '""
                      (go label150)
                     label220
                      '""
                      '"     termination."
                      '""
                      (if (= iter 0) (setf par zero))
                      (go end_label)
                      '""
                      '"     last card of subroutine lmpar."
                      '""
                     end_label
                      (return
                       (values nil
                               nil
                               nil
                               nil
                               nil
                               nil
                               nil
                               par
                               nil
                               nil
                               nil
                               nil)))))))))))))

