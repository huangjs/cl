;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :minpack)


(let ((one 1.0d0))
  (declare (type double-float one))
  (defun r1mpyq (m n a lda v w)
    (declare (type (array double-float (*)) w v a)
             (type f2cl-lib:integer4 lda n m))
    (f2cl-lib:with-array-data (a-%data% a-%offset% a)
      (declare (type f2cl-lib:integer4 a-%offset%)
               (type (simple-array double-float (*)) a-%data%)
               (ignorable a-%offset% a-%data%))
      (f2cl-lib:with-array-data (v-%data% v-%offset% v)
        (declare (type f2cl-lib:integer4 v-%offset%)
                 (type (simple-array double-float (*)) v-%data%)
                 (ignorable v-%offset% v-%data%))
        (f2cl-lib:with-array-data (w-%data% w-%offset% w)
          (declare (type f2cl-lib:integer4 w-%offset%)
                   (type (simple-array double-float (*)) w-%data%)
                   (ignorable w-%offset% w-%data%))
          (prog ((cos 0.0d0) (sin 0.0d0) (temp 0.0d0) (i 0) (j 0) (nmj 0)
                 (nm1 0))
            (declare (type f2cl-lib:integer4 nm1 nmj j i)
                     (type double-float temp sin cos))
            '"     **********"
            '""
            '"     subroutine r1mpyq"
            '""
            '"     given an m by n matrix a, this subroutine computes a*q where"
            '"     q is the product of 2*(n - 1) transformations"
            '""
            '"           gv(n-1)*...*gv(1)*gw(1)*...*gw(n-1)"
            '""
            '"     and gv(i), gw(i) are givens rotations in the (i,n) plane which"
            '"     eliminate elements in the i-th and n-th planes, respectively."
            '"     q itself is not given, rather the information to recover the"
            '"     gv, gw rotations is supplied."
            '""
            '"     the subroutine statement is"
            '""
            '"       subroutine r1mpyq(m,n,a,lda,v,w)"
            '""
            '"     where"
            '""
            '"       m is a positive integer input variable set to the number"
            '"         of rows of a."
            '""
            '"       n is a positive integer input variable set to the number"
            '"         of columns of a."
            '""
            '"       a is an m by n array. on input a must contain the matrix"
            '"         to be postmultiplied by the orthogonal matrix q"
            '"         described above. on output a*q has replaced a."
            '""
            '"       lda is a positive integer input variable not less than m"
            '"         which specifies the leading dimension of the array a."
            '""
            '"       v is an input array of length n. v(i) must contain the"
            '"         information necessary to recover the givens rotation gv(i)"
            '"         described above."
            '""
            '"       w is an input array of length n. w(i) must contain the"
            '"         information necessary to recover the givens rotation gw(i)"
            '"         described above."
            '""
            '"     subroutines called"
            '""
            '"       fortran-supplied ... dabs,dsqrt"
            '""
            '"     argonne national laboratory. minpack project. march 1980."
            '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
            '""
            '"     **********"
            '""
            '"     apply the first set of givens rotations to a."
            '""
            (setf nm1 (f2cl-lib:int-sub n 1))
            (if (< nm1 1) (go label50))
            (f2cl-lib:fdo (nmj 1 (f2cl-lib:int-add nmj 1))
                          ((> nmj nm1) nil)
              (tagbody
                (setf j (f2cl-lib:int-sub n nmj))
                (if
                 (>
                  (f2cl-lib:dabs
                   (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                  one)
                 (setf cos
                         (/ one
                            (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))))
                (if
                 (>
                  (f2cl-lib:dabs
                   (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                  one)
                 (setf sin (f2cl-lib:dsqrt (- one (expt cos 2)))))
                (if
                 (<=
                  (f2cl-lib:dabs
                   (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                  one)
                 (setf sin (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%)))
                (if
                 (<=
                  (f2cl-lib:dabs
                   (f2cl-lib:fref v-%data% (j) ((1 n)) v-%offset%))
                  one)
                 (setf cos (f2cl-lib:dsqrt (- one (expt sin 2)))))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf temp
                            (-
                             (* cos
                                (f2cl-lib:fref a-%data%
                                               (i j)
                                               ((1 lda) (1 n))
                                               a-%offset%))
                             (* sin
                                (f2cl-lib:fref a-%data%
                                               (i n)
                                               ((1 lda) (1 n))
                                               a-%offset%))))
                    (f2cl-lib:fset
                     (f2cl-lib:fref a-%data% (i n) ((1 lda) (1 n)) a-%offset%)
                     (+
                      (* sin
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 n))
                                        a-%offset%))
                      (* cos
                         (f2cl-lib:fref a-%data%
                                        (i n)
                                        ((1 lda) (1 n))
                                        a-%offset%))))
                    (f2cl-lib:fset
                     (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 n)) a-%offset%)
                     temp)
                   label10))
               label20))
            '""
            '"     apply the second set of givens rotations to a."
            '""
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j nm1) nil)
              (tagbody
                (if
                 (>
                  (f2cl-lib:dabs
                   (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
                  one)
                 (setf cos
                         (/ one
                            (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))))
                (if
                 (>
                  (f2cl-lib:dabs
                   (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
                  one)
                 (setf sin (f2cl-lib:dsqrt (- one (expt cos 2)))))
                (if
                 (<=
                  (f2cl-lib:dabs
                   (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
                  one)
                 (setf sin (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%)))
                (if
                 (<=
                  (f2cl-lib:dabs
                   (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))
                  one)
                 (setf cos (f2cl-lib:dsqrt (- one (expt sin 2)))))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (setf temp
                            (+
                             (* cos
                                (f2cl-lib:fref a-%data%
                                               (i j)
                                               ((1 lda) (1 n))
                                               a-%offset%))
                             (* sin
                                (f2cl-lib:fref a-%data%
                                               (i n)
                                               ((1 lda) (1 n))
                                               a-%offset%))))
                    (f2cl-lib:fset
                     (f2cl-lib:fref a-%data% (i n) ((1 lda) (1 n)) a-%offset%)
                     (+
                      (* (- sin)
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 n))
                                        a-%offset%))
                      (* cos
                         (f2cl-lib:fref a-%data%
                                        (i n)
                                        ((1 lda) (1 n))
                                        a-%offset%))))
                    (f2cl-lib:fset
                     (f2cl-lib:fref a-%data% (i j) ((1 lda) (1 n)) a-%offset%)
                     temp)
                   label30))
               label40))
           label50
            (go end_label)
            '""
            '"     last card of subroutine r1mpyq."
            '""
           end_label
            (return (values nil nil nil nil nil nil))))))))

