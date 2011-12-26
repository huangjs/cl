;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :minpack)


(let ((one 1.0d0) (zero 0.0d0))
  (declare (type double-float zero one))
  (defun dogleg (n r lr diag qtb delta x wa1 wa2)
    (declare (type double-float delta)
             (type (array double-float (*)) wa2 wa1 x qtb diag r)
             (type f2cl-lib:integer4 lr n))
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
            (f2cl-lib:with-array-data (wa1-%data% wa1-%offset% wa1)
              (declare (type f2cl-lib:integer4 wa1-%offset%)
                       (type (simple-array double-float (*)) wa1-%data%)
                       (ignorable wa1-%offset% wa1-%data%))
              (f2cl-lib:with-array-data (wa2-%data% wa2-%offset% wa2)
                (declare (type f2cl-lib:integer4 wa2-%offset%)
                         (type (simple-array double-float (*)) wa2-%data%)
                         (ignorable wa2-%offset% wa2-%data%))
                (prog ((alpha 0.0d0) (bnorm 0.0d0) (epsmch 0.0d0) (gnorm 0.0d0)
                       (qnorm 0.0d0) (sgnorm 0.0d0) (sum 0.0d0) (temp 0.0d0)
                       (i 0) (j 0) (jj 0) (jp1 0) (k 0) (l 0))
                  (declare (type f2cl-lib:integer4 l k jp1 jj j i)
                           (type double-float temp sum sgnorm qnorm gnorm
                            epsmch bnorm alpha))
                  '"     **********"
                  '""
                  '"     subroutine dogleg"
                  '""
                  '"     given an m by n matrix a, an n by n nonsingular diagonal"
                  '"     matrix d, an m-vector b, and a positive number delta, the"
                  '"     problem is to determine the convex combination x of the"
                  '"     gauss-newton and scaled gradient directions that minimizes"
                  '"     (a*x - b) in the least squares sense, subject to the"
                  '"     restriction that the euclidean norm of d*x be at most delta."
                  '""
                  '"     this subroutine completes the solution of the problem"
                  '"     if it is provided with the necessary information from the"
                  '"     qr factorization of a. that is, if a = q*r, where q has"
                  '"     orthogonal columns and r is an upper triangular matrix,"
                  '"     then dogleg expects the full upper triangle of r and"
                  '"     the first n components of (q transpose)*b."
                  '""
                  '"     the subroutine statement is"
                  '""
                  '"       subroutine dogleg(n,r,lr,diag,qtb,delta,x,wa1,wa2)"
                  '""
                  '"     where"
                  '""
                  '"       n is a positive integer input variable set to the order of r."
                  '""
                  '"       r is an input array of length lr which must contain the upper"
                  '"         triangular matrix r stored by rows."
                  '""
                  '"       lr is a positive integer input variable not less than"
                  '"         (n*(n+1))/2."
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
                  '"       x is an output array of length n which contains the desired"
                  '"         convex combination of the gauss-newton direction and the"
                  '"         scaled gradient direction."
                  '""
                  '"       wa1 and wa2 are work arrays of length n."
                  '""
                  '"     subprograms called"
                  '""
                  '"       minpack-supplied ... dpmpar,enorm"
                  '""
                  '"       fortran-supplied ... dabs,dmax1,dmin1,dsqrt"
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
                  '"     first, calculate the gauss-newton direction."
                  '""
                  (setf jj
                          (+ (the f2cl-lib:integer4 (truncate (* n (+ n 1)) 2))
                             1))
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k n) nil)
                    (tagbody
                      (setf j (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1))
                      (setf jp1 (f2cl-lib:int-add j 1))
                      (setf jj (f2cl-lib:int-sub jj k))
                      (setf l (f2cl-lib:int-add jj 1))
                      (setf sum zero)
                      (if (< n jp1) (go label20))
                      (f2cl-lib:fdo (i jp1 (f2cl-lib:int-add i 1))
                                    ((> i n) nil)
                        (tagbody
                          (setf sum
                                  (+ sum
                                     (*
                                      (f2cl-lib:fref r-%data%
                                                     (l)
                                                     ((1 lr))
                                                     r-%offset%)
                                      (f2cl-lib:fref x-%data%
                                                     (i)
                                                     ((1 n))
                                                     x-%offset%))))
                          (setf l (f2cl-lib:int-add l 1))
                         label10))
                     label20
                      (setf temp
                              (f2cl-lib:fref r-%data%
                                             (jj)
                                             ((1 lr))
                                             r-%offset%))
                      (if (/= temp zero) (go label40))
                      (setf l j)
                      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                    ((> i j) nil)
                        (tagbody
                          (setf temp
                                  (f2cl-lib:dmax1 temp
                                                  (f2cl-lib:dabs
                                                   (f2cl-lib:fref r-%data%
                                                                  (l)
                                                                  ((1 lr))
                                                                  r-%offset%))))
                          (setf l (f2cl-lib:int-sub (f2cl-lib:int-add l n) i))
                         label30))
                      (setf temp (* epsmch temp))
                      (if (= temp zero) (setf temp epsmch))
                     label40
                      (f2cl-lib:fset
                       (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                       (/
                        (- (f2cl-lib:fref qtb-%data% (j) ((1 n)) qtb-%offset%)
                           sum)
                        temp))
                     label50))
                  '""
                  '"     test whether the gauss-newton direction is acceptable."
                  '""
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (f2cl-lib:fset
                       (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                       zero)
                      (f2cl-lib:fset
                       (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                       (* (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)
                          (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
                     label60))
                  (setf qnorm (enorm n wa2))
                  (if (<= qnorm delta) (go label140))
                  '""
                  '"     the gauss-newton direction is not acceptable."
                  '"     next, calculate the scaled gradient direction."
                  '""
                  (setf l 1)
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (setf temp
                              (f2cl-lib:fref qtb-%data%
                                             (j)
                                             ((1 n))
                                             qtb-%offset%))
                      (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                                    ((> i n) nil)
                        (tagbody
                          (f2cl-lib:fset
                           (f2cl-lib:fref wa1-%data% (i) ((1 n)) wa1-%offset%)
                           (+
                            (f2cl-lib:fref wa1-%data% (i) ((1 n)) wa1-%offset%)
                            (* (f2cl-lib:fref r-%data% (l) ((1 lr)) r-%offset%)
                               temp)))
                          (setf l (f2cl-lib:int-add l 1))
                         label70))
                      (f2cl-lib:fset
                       (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                       (/ (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                          (f2cl-lib:fref diag-%data%
                                         (j)
                                         ((1 n))
                                         diag-%offset%)))
                     label80))
                  '""
                  '"     calculate the norm of the scaled gradient and test for"
                  '"     the special case in which the scaled gradient is zero."
                  '""
                  (setf gnorm (enorm n wa1))
                  (setf sgnorm zero)
                  (setf alpha (/ delta qnorm))
                  (if (= gnorm zero) (go label120))
                  '""
                  '"     calculate the point along the scaled gradient"
                  '"     at which the quadratic is minimized."
                  '""
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (f2cl-lib:fset
                       (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                       (/
                        (/ (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%)
                           gnorm)
                        (f2cl-lib:fref diag-%data% (j) ((1 n)) diag-%offset%)))
                     label90))
                  (setf l 1)
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (setf sum zero)
                      (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                                    ((> i n) nil)
                        (tagbody
                          (setf sum
                                  (+ sum
                                     (*
                                      (f2cl-lib:fref r-%data%
                                                     (l)
                                                     ((1 lr))
                                                     r-%offset%)
                                      (f2cl-lib:fref wa1-%data%
                                                     (i)
                                                     ((1 n))
                                                     wa1-%offset%))))
                          (setf l (f2cl-lib:int-add l 1))
                         label100))
                      (f2cl-lib:fset
                       (f2cl-lib:fref wa2-%data% (j) ((1 n)) wa2-%offset%)
                       sum)
                     label110))
                  (setf temp (enorm n wa2))
                  (setf sgnorm (/ (/ gnorm temp) temp))
                  '""
                  '"     test whether the scaled gradient direction is acceptable."
                  '""
                  (setf alpha zero)
                  (if (>= sgnorm delta) (go label120))
                  '""
                  '"     the scaled gradient direction is not acceptable."
                  '"     finally, calculate the point along the dogleg"
                  '"     at which the quadratic is minimized."
                  '""
                  (setf bnorm (enorm n qtb))
                  (setf temp
                          (* (/ bnorm gnorm) (/ bnorm qnorm) (/ sgnorm delta)))
                  (setf temp
                          (+
                           (- temp
                              (* (/ delta qnorm) (expt (/ sgnorm delta) 2)))
                           (f2cl-lib:dsqrt
                            (+ (expt (- temp (/ delta qnorm)) 2)
                               (* (- one (expt (/ delta qnorm) 2))
                                  (- one (expt (/ sgnorm delta) 2)))))))
                  (setf alpha
                          (/
                           (* (/ delta qnorm)
                              (- one (expt (/ sgnorm delta) 2)))
                           temp))
                 label120
                  '""
                  '"     form appropriate convex combination of the gauss-newton"
                  '"     direction and the scaled gradient direction."
                  '""
                  (setf temp (* (- one alpha) (f2cl-lib:dmin1 sgnorm delta)))
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                      (f2cl-lib:fset
                       (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                       (+
                        (* temp
                           (f2cl-lib:fref wa1-%data% (j) ((1 n)) wa1-%offset%))
                        (* alpha
                           (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))))
                     label130))
                 label140
                  (go end_label)
                  '""
                  '"     last card of subroutine dogleg."
                  '""
                 end_label
                  (return (values nil nil nil nil nil nil nil nil nil)))))))))))

