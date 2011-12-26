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


(defun lsame (ca cb)
  (declare (type (simple-array character (*)) cb ca))
  (f2cl-lib:with-multi-array-data
      ((ca character ca-%data% ca-%offset%)
       (cb character cb-%data% cb-%offset%))
    (prog ((inta 0) (intb 0) (zcode 0) (lsame nil))
      (declare (type f2cl-lib:logical lsame)
               (type (f2cl-lib:integer4) zcode intb inta))
      (setf lsame (coerce (f2cl-lib:fstring-= ca cb) 'f2cl-lib:logical))
      (if lsame (go end_label))
      (setf zcode (f2cl-lib:ichar "Z"))
      (setf inta (f2cl-lib:ichar ca))
      (setf intb (f2cl-lib:ichar cb))
      (cond
        ((or (= zcode 90) (= zcode 122))
         (if (and (>= inta 97) (<= inta 122))
             (setf inta (f2cl-lib:int-sub inta 32)))
         (if (and (>= intb 97) (<= intb 122))
             (setf intb (f2cl-lib:int-sub intb 32))))
        ((or (= zcode 233) (= zcode 169))
         (if
          (or (and (>= inta 129) (<= inta 137))
              (and (>= inta 145) (<= inta 153))
              (and (>= inta 162) (<= inta 169)))
          (setf inta (f2cl-lib:int-add inta 64)))
         (if
          (or (and (>= intb 129) (<= intb 137))
              (and (>= intb 145) (<= intb 153))
              (and (>= intb 162) (<= intb 169)))
          (setf intb (f2cl-lib:int-add intb 64))))
        ((or (= zcode 218) (= zcode 250))
         (if (and (>= inta 225) (<= inta 250))
             (setf inta (f2cl-lib:int-sub inta 32)))
         (if (and (>= intb 225) (<= intb 250))
             (setf intb (f2cl-lib:int-sub intb 32)))))
      (setf lsame (coerce (= inta intb) 'f2cl-lib:logical))
     end_label
      (return (values lsame nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lsame fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (1))
                        (simple-array character (1)))
           :return-values '(nil nil)
           :calls 'nil)))

