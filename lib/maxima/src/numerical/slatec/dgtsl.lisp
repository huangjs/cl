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

(in-package :slatec)


(defun dgtsl (n c d e b info)
  (declare (type (array double-float (*)) b e d c)
           (type (f2cl-lib:integer4) info n))
  (f2cl-lib:with-multi-array-data
      ((c double-float c-%data% c-%offset%)
       (d double-float d-%data% d-%offset%)
       (e double-float e-%data% e-%offset%)
       (b double-float b-%data% b-%offset%))
    (prog ((t$ 0.0) (k 0) (kb 0) (kp1 0) (nm1 0) (nm2 0))
      (declare (type (f2cl-lib:integer4) nm2 nm1 kp1 kb k)
               (type (double-float) t$))
      (setf info 0)
      (setf (f2cl-lib:fref c-%data% (1) ((1 *)) c-%offset%)
              (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%))
      (setf nm1 (f2cl-lib:int-sub n 1))
      (if (< nm1 1) (go label40))
      (setf (f2cl-lib:fref d-%data% (1) ((1 *)) d-%offset%)
              (f2cl-lib:fref e-%data% (1) ((1 *)) e-%offset%))
      (setf (f2cl-lib:fref e-%data% (1) ((1 *)) e-%offset%) 0.0)
      (setf (f2cl-lib:fref e-%data% (n) ((1 *)) e-%offset%) 0.0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf kp1 (f2cl-lib:int-add k 1))
          (if
           (< (abs (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%))
              (abs (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%)))
           (go label10))
          (setf t$ (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%))
          (setf (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%)
                  (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%))
          (setf (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%) t$)
          (setf t$ (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%))
          (setf (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%)
                  (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%))
          (setf (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%) t$)
          (setf t$ (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%))
          (setf (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%)
                  (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%))
          (setf (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%) t$)
          (setf t$ (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%))
          (setf (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%)
                  (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%))
          (setf (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%) t$)
         label10
          (if (/= (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%) 0.0)
              (go label20))
          (setf info k)
          (go label100)
         label20
          (setf t$
                  (/ (- (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%))
                     (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%)))
          (setf (f2cl-lib:fref c-%data% (kp1) ((1 *)) c-%offset%)
                  (+ (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%)
                     (* t$ (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%))))
          (setf (f2cl-lib:fref d-%data% (kp1) ((1 *)) d-%offset%)
                  (+ (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%)
                     (* t$ (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%))))
          (setf (f2cl-lib:fref e-%data% (kp1) ((1 *)) e-%offset%) 0.0)
          (setf (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%)
                  (+ (f2cl-lib:fref b-%data% (kp1) ((1 *)) b-%offset%)
                     (* t$ (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%))))
         label30))
     label40
      (if (/= (f2cl-lib:fref c-%data% (n) ((1 *)) c-%offset%) 0.0)
          (go label50))
      (setf info n)
      (go label90)
     label50
      (setf nm2 (f2cl-lib:int-sub n 2))
      (setf (f2cl-lib:fref b-%data% (n) ((1 *)) b-%offset%)
              (/ (f2cl-lib:fref b-%data% (n) ((1 *)) b-%offset%)
                 (f2cl-lib:fref c-%data% (n) ((1 *)) c-%offset%)))
      (if (= n 1) (go label80))
      (setf (f2cl-lib:fref b-%data% (nm1) ((1 *)) b-%offset%)
              (/
               (- (f2cl-lib:fref b-%data% (nm1) ((1 *)) b-%offset%)
                  (* (f2cl-lib:fref d-%data% (nm1) ((1 *)) d-%offset%)
                     (f2cl-lib:fref b-%data% (n) ((1 *)) b-%offset%)))
               (f2cl-lib:fref c-%data% (nm1) ((1 *)) c-%offset%)))
      (if (< nm2 1) (go label70))
      (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                    ((> kb nm2) nil)
        (tagbody
          (setf k (f2cl-lib:int-add (f2cl-lib:int-sub nm2 kb) 1))
          (setf (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%)
                  (/
                   (- (f2cl-lib:fref b-%data% (k) ((1 *)) b-%offset%)
                      (* (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%)
                         (f2cl-lib:fref b-%data%
                                        ((f2cl-lib:int-add k 1))
                                        ((1 *))
                                        b-%offset%))
                      (* (f2cl-lib:fref e-%data% (k) ((1 *)) e-%offset%)
                         (f2cl-lib:fref b-%data%
                                        ((f2cl-lib:int-add k 2))
                                        ((1 *))
                                        b-%offset%)))
                   (f2cl-lib:fref c-%data% (k) ((1 *)) c-%offset%)))
         label60))
     label70
     label80
     label90
     label100
      (go end_label)
     end_label
      (return (values nil nil nil nil nil info)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgtsl fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::info)
           :calls 'nil)))

