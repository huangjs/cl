;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :matlisp-lib)

(defun zeroin (ax bx f tol)
  (declare (type double-float tol f bx ax)
   (type (function (double-float) (values double-float &rest t)) f))
  (prog ((a 0.0d0) (b 0.0d0) (c 0.0d0) (d 0.0d0) (e 0.0d0) (eps 0.0d0)
         (fa 0.0d0) (fb 0.0d0) (fc 0.0d0) (tol1 0.0d0) (xm 0.0d0) (p 0.0d0)
         (q 0.0d0) (r 0.0d0) (s 0.0d0) (zeroin 0.0d0))
    (declare (type double-float zeroin s r q p xm tol1 fc fb fa eps e d c b a))
   label10
    (setf eps (f2cl-lib:d1mach 4))
    (setf tol1 (+ eps 1.0d0))
    (setf a ax)
    (setf b bx)
    (setf fa
            (multiple-value-bind
                (ret-val var-0)
                (funcall f a)
              (declare (ignore))
              (when var-0 (setf a var-0))
              ret-val))
    (setf fb
            (multiple-value-bind
                (ret-val var-0)
                (funcall f b)
              (declare (ignore))
              (when var-0 (setf b var-0))
              ret-val))
    (if (or (= fa 0.0d0) (= fb 0.0d0)) (go label20))
    (if (<= (* fa (/ fb (f2cl-lib:dabs fb))) 0.0d0) (go label20))
    (f2cl-lib:fformat 6
                      ("~1@T" "f(ax) and f(bx) do not have different signs,"
                       " zeroin is aborting" "~%")
                      nil)
    (go end_label)
   label20
    (setf c a)
    (setf fc fa)
    (setf d (- b a))
    (setf e d)
   label30
    (if (>= (f2cl-lib:dabs fc) (f2cl-lib:dabs fb)) (go label40))
    (setf a b)
    (setf b c)
    (setf c a)
    (setf fa fb)
    (setf fb fc)
    (setf fc fa)
   label40
    (setf tol1 (+ (* 2.0d0 eps (f2cl-lib:dabs b)) (* 0.5d0 tol)))
    (setf xm (* 0.5d0 (- c b)))
    (if (or (<= (f2cl-lib:dabs xm) tol1) (= fb 0.0d0)) (go label150))
    (if
     (and (>= (f2cl-lib:dabs e) tol1)
          (> (f2cl-lib:dabs fa) (f2cl-lib:dabs fb)))
     (go label50))
    (setf d xm)
    (setf e d)
    (go label110)
   label50
    (setf s (/ fb fa))
    (if (/= a c) (go label60))
    (setf p (* 2.0d0 xm s))
    (setf q (- 1.0d0 s))
    (go label70)
   label60
    (setf q (/ fa fc))
    (setf r (/ fb fc))
    (setf p (* s (- (* 2.0d0 xm q (- q r)) (* (- b a) (- r 1.0d0)))))
    (setf q (* (- q 1.0d0) (- r 1.0d0) (- s 1.0d0)))
   label70
    (if (<= p 0.0d0) (go label80))
    (setf q (- q))
    (go label90)
   label80
    (setf p (- p))
   label90
    (setf s e)
    (setf e d)
    (if
     (or (>= (* 2.0d0 p) (- (* 3.0d0 xm q) (f2cl-lib:dabs (* tol1 q))))
         (>= p (f2cl-lib:dabs (* 0.5d0 s q))))
     (go label100))
    (setf d (/ p q))
    (go label110)
   label100
    (setf d xm)
    (setf e d)
   label110
    (setf a b)
    (setf fa fb)
    (if (<= (f2cl-lib:dabs d) tol1) (go label120))
    (setf b (+ b d))
    (go label140)
   label120
    (if (<= xm 0.0d0) (go label130))
    (setf b (+ b tol1))
    (go label140)
   label130
    (setf b (- b tol1))
   label140
    (setf fb
            (multiple-value-bind
                (ret-val var-0)
                (funcall f b)
              (declare (ignore))
              (when var-0 (setf b var-0))
              ret-val))
    (if (> (* fb (/ fc (f2cl-lib:dabs fc))) 0.0d0) (go label20))
    (go label30)
   label150
    (setf zeroin b)
    (go end_label)
   end_label
    (return (values zeroin nil nil nil nil))))

