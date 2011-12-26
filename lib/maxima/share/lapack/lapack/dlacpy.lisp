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

(in-package :lapack)


(defun dlacpy (uplo m n a lda b ldb$)
  (declare (type (array double-float (*)) b a)
           (type (f2cl-lib:integer4) ldb$ lda n m)
           (type (simple-array character (*)) uplo))
  (f2cl-lib:with-multi-array-data
      ((uplo character uplo-%data% uplo-%offset%)
       (a double-float a-%data% a-%offset%)
       (b double-float b-%data% b-%offset%))
    (prog ((i 0) (j 0))
      (declare (type (f2cl-lib:integer4) j i))
      (cond
        ((lsame uplo "U")
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i
                               (min (the f2cl-lib:integer4 j)
                                    (the f2cl-lib:integer4 m)))
                            nil)
               (tagbody
                 (setf (f2cl-lib:fref b-%data%
                                      (i j)
                                      ((1 ldb$) (1 *))
                                      b-%offset%)
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%))
                label10))
            label20)))
        ((lsame uplo "L")
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                           ((> i m) nil)
               (tagbody
                 (setf (f2cl-lib:fref b-%data%
                                      (i j)
                                      ((1 ldb$) (1 *))
                                      b-%offset%)
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%))
                label30))
            label40)))
        (t
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i m) nil)
               (tagbody
                 (setf (f2cl-lib:fref b-%data%
                                      (i j)
                                      ((1 ldb$) (1 *))
                                      b-%offset%)
                         (f2cl-lib:fref a-%data%
                                        (i j)
                                        ((1 lda) (1 *))
                                        a-%offset%))
                label50))
            label60))))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlacpy
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (1))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::lsame))))

