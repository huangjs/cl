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


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dtrmv (uplo trans diag n a lda x incx)
    (declare (type (array double-float (*)) x a)
             (type (f2cl-lib:integer4) incx lda n)
             (type (simple-array character (*)) diag trans uplo))
    (f2cl-lib:with-multi-array-data
        ((uplo character uplo-%data% uplo-%offset%)
         (trans character trans-%data% trans-%offset%)
         (diag character diag-%data% diag-%offset%)
         (a double-float a-%data% a-%offset%)
         (x double-float x-%data% x-%offset%))
      (prog ((nounit nil) (i 0) (info 0) (ix 0) (j 0) (jx 0) (kx 0) (temp 0.0))
        (declare (type f2cl-lib:logical nounit)
                 (type (f2cl-lib:integer4) i info ix j jx kx)
                 (type (double-float) temp))
        (setf info 0)
        (cond
          ((and (not (lsame uplo "U")) (not (lsame uplo "L")))
           (setf info 1))
          ((and (not (lsame trans "N"))
                (not (lsame trans "T"))
                (not (lsame trans "C")))
           (setf info 2))
          ((and (not (lsame diag "U")) (not (lsame diag "N")))
           (setf info 3))
          ((< n 0)
           (setf info 4))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 n)))
           (setf info 6))
          ((= incx 0)
           (setf info 8)))
        (cond
          ((/= info 0)
           (xerbla "DTRMV " info)
           (go end_label)))
        (if (= n 0) (go end_label))
        (setf nounit (lsame diag "N"))
        (cond
          ((<= incx 0)
           (setf kx
                   (f2cl-lib:int-sub 1
                                     (f2cl-lib:int-mul (f2cl-lib:int-sub n 1)
                                                       incx))))
          ((/= incx 1)
           (setf kx 1)))
        (cond
          ((lsame trans "N")
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
                                (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i
                                          (f2cl-lib:int-add j
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (setf (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%)
                                    (+
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref a-%data%
                                                       (i j)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))))
                           label10))
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (*
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref a-%data%
                                                    (j j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))))
                    label20)))
                (t
                 (setf jx kx)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                        (setf temp
                                (f2cl-lib:fref x-%data%
                                               (jx)
                                               ((1 *))
                                               x-%offset%))
                        (setf ix kx)
                        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                                      ((> i
                                          (f2cl-lib:int-add j
                                                            (f2cl-lib:int-sub
                                                             1)))
                                       nil)
                          (tagbody
                            (setf (f2cl-lib:fref x-%data%
                                                 (ix)
                                                 ((1 *))
                                                 x-%offset%)
                                    (+
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref a-%data%
                                                       (i j)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))))
                            (setf ix (f2cl-lib:int-add ix incx))
                           label30))
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)
                                    (*
                                     (f2cl-lib:fref x-%data%
                                                    (jx)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref a-%data%
                                                    (j j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))))
                     (setf jx (f2cl-lib:int-add jx incx))
                    label40)))))
             (t
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (j) ((1 *))) zero)
                        (setf temp
                                (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                        (f2cl-lib:fdo (i n
                                       (f2cl-lib:int-add i
                                                         (f2cl-lib:int-sub 1)))
                                      ((> i (f2cl-lib:int-add j 1)) nil)
                          (tagbody
                            (setf (f2cl-lib:fref x-%data%
                                                 (i)
                                                 ((1 *))
                                                 x-%offset%)
                                    (+
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref a-%data%
                                                       (i j)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))))
                           label50))
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 *))
                                                 x-%offset%)
                                    (*
                                     (f2cl-lib:fref x-%data%
                                                    (j)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref a-%data%
                                                    (j j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))))
                    label60)))
                (t
                 (setf kx
                         (f2cl-lib:int-add kx
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-sub n 1)
                                            incx)))
                 (setf jx kx)
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (cond
                       ((/= (f2cl-lib:fref x (jx) ((1 *))) zero)
                        (setf temp
                                (f2cl-lib:fref x-%data%
                                               (jx)
                                               ((1 *))
                                               x-%offset%))
                        (setf ix kx)
                        (f2cl-lib:fdo (i n
                                       (f2cl-lib:int-add i
                                                         (f2cl-lib:int-sub 1)))
                                      ((> i (f2cl-lib:int-add j 1)) nil)
                          (tagbody
                            (setf (f2cl-lib:fref x-%data%
                                                 (ix)
                                                 ((1 *))
                                                 x-%offset%)
                                    (+
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%)
                                     (* temp
                                        (f2cl-lib:fref a-%data%
                                                       (i j)
                                                       ((1 lda) (1 *))
                                                       a-%offset%))))
                            (setf ix (f2cl-lib:int-sub ix incx))
                           label70))
                        (if nounit
                            (setf (f2cl-lib:fref x-%data%
                                                 (jx)
                                                 ((1 *))
                                                 x-%offset%)
                                    (*
                                     (f2cl-lib:fref x-%data%
                                                    (jx)
                                                    ((1 *))
                                                    x-%offset%)
                                     (f2cl-lib:fref a-%data%
                                                    (j j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%))))))
                     (setf jx (f2cl-lib:int-sub jx incx))
                    label80)))))))
          (t
           (cond
             ((lsame uplo "U")
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                     (if nounit
                         (setf temp
                                 (* temp
                                    (f2cl-lib:fref a-%data%
                                                   (j j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))))
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))
                                    (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                   ((> i 1) nil)
                       (tagbody
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    (i j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%))))
                        label90))
                     (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                             temp)
                    label100)))
                (t
                 (setf jx
                         (f2cl-lib:int-add kx
                                           (f2cl-lib:int-mul
                                            (f2cl-lib:int-sub n 1)
                                            incx)))
                 (f2cl-lib:fdo (j n (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                               ((> j 1) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%))
                     (setf ix jx)
                     (if nounit
                         (setf temp
                                 (* temp
                                    (f2cl-lib:fref a-%data%
                                                   (j j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))))
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j (f2cl-lib:int-sub 1))
                                    (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                                   ((> i 1) nil)
                       (tagbody
                         (setf ix (f2cl-lib:int-sub ix incx))
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    (i j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%))))
                        label110))
                     (setf (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%)
                             temp)
                     (setf jx (f2cl-lib:int-sub jx incx))
                    label120)))))
             (t
              (cond
                ((= incx 1)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%))
                     (if nounit
                         (setf temp
                                 (* temp
                                    (f2cl-lib:fref a-%data%
                                                   (j j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))))
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                    (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    (i j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 *))
                                                    x-%offset%))))
                        label130))
                     (setf (f2cl-lib:fref x-%data% (j) ((1 *)) x-%offset%)
                             temp)
                    label140)))
                (t
                 (setf jx kx)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf temp
                             (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%))
                     (setf ix jx)
                     (if nounit
                         (setf temp
                                 (* temp
                                    (f2cl-lib:fref a-%data%
                                                   (j j)
                                                   ((1 lda) (1 *))
                                                   a-%offset%))))
                     (f2cl-lib:fdo (i (f2cl-lib:int-add j 1)
                                    (f2cl-lib:int-add i 1))
                                   ((> i n) nil)
                       (tagbody
                         (setf ix (f2cl-lib:int-add ix incx))
                         (setf temp
                                 (+ temp
                                    (*
                                     (f2cl-lib:fref a-%data%
                                                    (i j)
                                                    ((1 lda) (1 *))
                                                    a-%offset%)
                                     (f2cl-lib:fref x-%data%
                                                    (ix)
                                                    ((1 *))
                                                    x-%offset%))))
                        label150))
                     (setf (f2cl-lib:fref x-%data% (jx) ((1 *)) x-%offset%)
                             temp)
                     (setf jx (f2cl-lib:int-add jx incx))
                    label160))))))))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dtrmv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (1))
                        (simple-array character (1))
                        (simple-array character (1))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::xerbla fortran-to-lisp::lsame))))

