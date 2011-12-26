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


(let ((xmax 0.0)
      (dxrel 0.0)
      (sq2pil 0.9189385332046728)
      (sqpi2l 0.22579135264472744)
      (pi$ 3.141592653589793)
      (first$ nil))
  (declare (type (double-float) xmax dxrel sq2pil sqpi2l pi$)
           (type f2cl-lib:logical first$))
  (setq first$ f2cl-lib:%true%)
  (defun dlngam (x)
    (declare (type (double-float) x))
    (prog ((sinpiy 0.0) (y 0.0) (temp 0.0) (dlngam 0.0))
      (declare (type (double-float) dlngam temp y sinpiy))
      (cond
        (first$
         (setf temp (/ 1.0 (f2cl-lib:flog (f2cl-lib:d1mach 2))))
         (setf xmax (* temp (f2cl-lib:d1mach 2)))
         (setf dxrel (f2cl-lib:fsqrt (f2cl-lib:d1mach 4)))))
      (setf first$ f2cl-lib:%false%)
      (setf y (abs x))
      (if (> y 10.0) (go label20))
      (setf dlngam (f2cl-lib:flog (abs (dgamma x))))
      (go end_label)
     label20
      (if (> y xmax)
          (xermsg "SLATEC" "DLNGAM" "ABS(X) SO BIG DLNGAM OVERFLOWS" 2 2))
      (if (> x 0.0)
          (setf dlngam
                  (+ (- (+ sq2pil (* (- x 0.5) (f2cl-lib:flog x))) x)
                     (d9lgmc y))))
      (if (> x 0.0) (go end_label))
      (setf sinpiy (abs (sin (* pi$ y))))
      (if (= sinpiy 0.0)
          (xermsg "SLATEC" "DLNGAM" "X IS A NEGATIVE INTEGER" 3 2))
      (if (< (abs (/ (- x (f2cl-lib:aint (- x 0.5))) x)) dxrel)
          (xermsg "SLATEC" "DLNGAM"
           "ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER" 1 1))
      (setf dlngam
              (- (+ sqpi2l (* (- x 0.5) (f2cl-lib:flog y)))
                 x
                 (f2cl-lib:flog sinpiy)
                 (d9lgmc y)))
      (go end_label)
     end_label
      (return (values dlngam nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlngam
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::d9lgmc
                                                     fortran-to-lisp::xermsg
                                                     fortran-to-lisp::dgamma
                                                     fortran-to-lisp::d1mach))))

