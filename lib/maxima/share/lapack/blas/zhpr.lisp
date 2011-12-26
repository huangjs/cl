;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(let* ((zero (f2cl-lib:cmplx 0.0 0.0)))
  (declare (type (f2cl-lib:complex16) zero) (ignorable zero))
  (defun zhpr (uplo n alpha x incx ap)
    (declare (type (array f2cl-lib:complex16 (*)) ap x)
             (type (double-float) alpha)
             (type (f2cl-lib:integer4) incx n)
             (type (simple-array character (*)) uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (x f2cl-lib:complex16 x-%data% x-%offset%)
         (ap f2cl-lib:complex16 ap-%data% ap-%offset%))
      (prog ((i 0) (info 0) (ix 0) (j 0) (jx 0) (k 0) (kk 0) (kx 0)
             (temp #C(0.0 0.0)))
        (declare (type (f2cl-lib:integer4) i info ix j jx k kk kx)
                 (type (f2cl-lib:complex16) temp))
        (setf info 0)
        (cond
          ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
           (setf info 1))
          ((< n 0)
           (setf info 2))
          ((= incx 0)
           (setf info 5)))
        (cond
          ((/= info 0)
           (xerbla "ZHPR  " info)
           (go end_label)))
        (if (or (= n 0) (= alpha (f2cl-lib:dble zero))) (go end_label))
        (cond
          ((<= incx 0)
           (setf kx
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incx))))
          ((/= incx 1)
           (setf kx 1)))
        (setf kk 1)
        (cond
          ((lsame uplo "U")
           (cond
             ((= incx 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf k kk)
                     (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                   ((> i
                                       (f2cl-lib:int-add j
                                                         (f2cl-lib:int-sub 1)))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref ap-%data%
                                              (k)
                                              ((1 *))
                                              ap-%offset%)
                                 (+
                                  (f2cl-lib:fref ap-%data%
                                                 (k)
                                                 ((1 *))
                                                 ap-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (i)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                         (setf k (f2cl-lib:int-add k 1))
                        label10))
                     (setf (f2cl-lib:fref ap-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add kk j)
                                            1))
                                          ((1 *))
                                          ap-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref ap-%data%
                                               ((f2cl-lib:int-sub
                                                 (f2cl-lib:int-add kk j)
                                                 1))
                                               ((1 *))
                                               ap-%offset%))
                               (f2cl-lib:dble
                                (*
                                 (f2cl-lib:fref x-%data%
                                                (j)
                                                ((1 *))
                                                x-%offset%)
                                 temp)))
                              'f2cl-lib:complex16)))
                    (t
                     (setf (f2cl-lib:fref ap-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add kk j)
                                            1))
                                          ((1 *))
                                          ap-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref ap-%data%
                                              ((f2cl-lib:int-sub
                                                (f2cl-lib:int-add kk j)
                                                1))
                                              ((1 *))
                                              ap-%offset%))
                              'f2cl-lib:complex16))))
                  (setf kk (f2cl-lib:int-add kk j))
                 label20)))
             (t
              (setf jx kx)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf ix kx)
                     (f2cl-lib:fdo (k kk (f2cl-lib:int-add k 1))
                                   ((> k
                                       (f2cl-lib:int-add kk
                                                         j
                                                         (f2cl-lib:int-sub 2)))
                                    nil)
                       (tagbody
                         (setf (f2cl-lib:fref ap-%data%
                                              (k)
                                              ((1 *))
                                              ap-%offset%)
                                 (+
                                  (f2cl-lib:fref ap-%data%
                                                 (k)
                                                 ((1 *))
                                                 ap-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (ix)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                         (setf ix (f2cl-lib:int-add ix incx))
                        label30))
                     (setf (f2cl-lib:fref ap-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add kk j)
                                            1))
                                          ((1 *))
                                          ap-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref ap-%data%
                                               ((f2cl-lib:int-sub
                                                 (f2cl-lib:int-add kk j)
                                                 1))
                                               ((1 *))
                                               ap-%offset%))
                               (f2cl-lib:dble
                                (*
                                 (f2cl-lib:fref x-%data%
                                                (jx)
                                                ((1 *))
                                                x-%offset%)
                                 temp)))
                              'f2cl-lib:complex16)))
                    (t
                     (setf (f2cl-lib:fref ap-%data%
                                          ((f2cl-lib:int-sub
                                            (f2cl-lib:int-add kk j)
                                            1))
                                          ((1 *))
                                          ap-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref ap-%data%
                                              ((f2cl-lib:int-sub
                                                (f2cl-lib:int-add kk j)
                                                1))
                                              ((1 *))
                                              ap-%offset%))
                              'f2cl-lib:complex16))))
                  (setf jx (f2cl-lib:int-add jx incx))
                  (setf kk (f2cl-lib:int-add kk j))
                 label40)))))
          (t
           (cond
             ((= incx 1)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf (f2cl-lib:fref ap-%data% (kk) ((1 *)) ap-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref ap-%data%
                                               (kk)
                                               ((1 *))
                                               ap-%offset%))
                               (f2cl-lib:dble
                                (* temp
                                   (f2cl-lib:fref x-%data%
                                                  (j)
                                                  ((1 *))
                                                  x-%offset%))))
                              'f2cl-lib:complex16))
                     (setf k (f2cl-lib:int-add kk 1))
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                    (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf (f2cl-lib:fref ap-%data%
                                              (k)
                                              ((1 *))
                                              ap-%offset%)
                                 (+
                                  (f2cl-lib:fref ap-%data%
                                                 (k)
                                                 ((1 *))
                                                 ap-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (i)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                         (setf k (f2cl-lib:int-add k 1))
                        label50)))
                    (t
                     (setf (f2cl-lib:fref ap-%data% (kk) ((1 *)) ap-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref ap-%data%
                                              (kk)
                                              ((1 *))
                                              ap-%offset%))
                              'f2cl-lib:complex16))))
                  (setf kk
                          (f2cl-lib:int-add
                           (f2cl-lib:int-sub (f2cl-lib:int-add kk n) j)
                           1))
                 label60)))
             (t
              (setf jx kx)
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (cond
                    ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                     (setf temp
                             (coerce
                              (* alpha
                                 (f2cl-lib:dconjg
                                  (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)))
                              'f2cl-lib:complex16))
                     (setf (f2cl-lib:fref ap-%data% (kk) ((1 *)) ap-%offset%)
                             (coerce
                              (+
                               (f2cl-lib:dble
                                (f2cl-lib:fref ap-%data%
                                               (kk)
                                               ((1 *))
                                               ap-%offset%))
                               (f2cl-lib:dble
                                (* temp
                                   (f2cl-lib:fref x-%data%
                                                  (jx)
                                                  ((1 *))
                                                  x-%offset%))))
                              'f2cl-lib:complex16))
                     (setf ix jx)
                     (f2cl-lib:fdo (k (f2cl-lib:int-add kk 1)
                                    (f2cl-lib:int-add k 1))
                                   ((> k
                                       (f2cl-lib:int-add kk
                                                         n
                                                         (f2cl-lib:int-sub j)))
                                    nil)
                       (tagbody
                         (setf ix (f2cl-lib:int-add ix incx))
                         (setf (f2cl-lib:fref ap-%data%
                                              (k)
                                              ((1 *))
                                              ap-%offset%)
                                 (+
                                  (f2cl-lib:fref ap-%data%
                                                 (k)
                                                 ((1 *))
                                                 ap-%offset%)
                                  (*
                                   (f2cl-lib:fref x-%data%
                                                  (ix)
                                                  ((1 *))
                                                  x-%offset%)
                                   temp)))
                        label70)))
                    (t
                     (setf (f2cl-lib:fref ap-%data% (kk) ((1 *)) ap-%offset%)
                             (coerce
                              (f2cl-lib:dble
                               (f2cl-lib:fref ap-%data%
                                              (kk)
                                              ((1 *))
                                              ap-%offset%))
                              'f2cl-lib:complex16))))
                  (setf jx (f2cl-lib:int-add jx incx))
                  (setf kk
                          (f2cl-lib:int-add
                           (f2cl-lib:int-sub (f2cl-lib:int-add kk n) j)
                           1))
                 label80))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zhpr fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (1))
                        (fortran-to-lisp::integer4) (double-float)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

