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
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((iparam
       (make-array 9
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(0 2 0 10 1 0 0 0 0))))
  (declare (type (simple-array f2cl-lib:integer4 (9)) iparam))
  (defun j4save (iwhich ivalue iset)
    (declare (type f2cl-lib:logical iset) (type (integer) ivalue iwhich))
    (prog ((j4save 0))
      (declare (type (integer) j4save))
      (setf j4save (f2cl-lib:fref iparam (iwhich) ((1 9))))
      (if iset (setf (f2cl-lib:fref iparam (iwhich) ((1 9))) ivalue))
      (go end_label)
     end_label
      (return (values j4save nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::j4save
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((integer) (integer) fortran-to-lisp::logical)
           :return-values '(nil nil nil)
           :calls 'nil)))

