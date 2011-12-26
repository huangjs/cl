;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :minpack)


(let ((one 1.0d0)
      (zero 0.0d0)
      (rdwarf 3.8339999999999996d-20)
      (rgiant 1.3040000000000002d+19))
  (declare (type double-float rgiant rdwarf zero one))
  (defun enorm (n x)
    (declare (type (array double-float (*)) x) (type f2cl-lib:integer4 n))
    (f2cl-lib:with-array-data (x-%data% x-%offset% x)
      (declare (type f2cl-lib:integer4 x-%offset%)
               (type (simple-array double-float (*)) x-%data%)
               (ignorable x-%offset% x-%data%))
      (prog ((agiant 0.0d0) (floatn 0.0d0) (s1 0.0d0) (s2 0.0d0) (s3 0.0d0)
             (xabs 0.0d0) (x1max 0.0d0) (x3max 0.0d0) (i 0) (enorm 0.0d0))
        (declare (type f2cl-lib:integer4 i)
                 (type double-float enorm x3max x1max xabs s3 s2 s1 floatn
                  agiant))
        '"     **********"
        '""
        '"     function enorm"
        '""
        '"     given an n-vector x, this function calculates the"
        '"     euclidean norm of x."
        '""
        '"     the euclidean norm is computed by accumulating the sum of"
        '"     squares in three different sums. the sums of squares for the"
        '"     small and large components are scaled so that no overflows"
        '"     occur. non-destructive underflows are permitted. underflows"
        '"     and overflows do not occur in the computation of the unscaled"
        '"     sum of squares for the intermediate components."
        '"     the definitions of small, intermediate and large components"
        '"     depend on two constants, rdwarf and rgiant. the main"
        '"     restrictions on these constants are that rdwarf**2 not"
        '"     underflow and rgiant**2 not overflow. the constants"
        '"     given here are suitable for every known computer."
        '""
        '"     the function statement is"
        '""
        '"       double precision function enorm(n,x)"
        '""
        '"     where"
        '""
        '"       n is a positive integer input variable."
        '""
        '"       x is an input array of length n."
        '""
        '"     subprograms called"
        '""
        '"       fortran-supplied ... dabs,dsqrt"
        '""
        '"     argonne national laboratory. minpack project. march 1980."
        '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
        '""
        '"     **********"
        (setf s1 zero)
        (setf s2 zero)
        (setf s3 zero)
        (setf x1max zero)
        (setf x3max zero)
        (setf floatn (coerce (the f2cl-lib:integer4 n) 'double-float))
        (setf agiant (/ rgiant floatn))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i n) nil)
          (tagbody
            (setf xabs
                    (f2cl-lib:dabs
                     (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%)))
            (if (and (> xabs rdwarf) (< xabs agiant)) (go label70))
            (if (<= xabs rdwarf) (go label30))
            '""
            '"              sum for large components."
            '""
            (if (<= xabs x1max) (go label10))
            (setf s1 (+ one (* s1 (expt (/ x1max xabs) 2))))
            (setf x1max xabs)
            (go label20)
           label10
            (setf s1 (+ s1 (expt (/ xabs x1max) 2)))
           label20
            (go label60)
           label30
            '""
            '"              sum for small components."
            '""
            (if (<= xabs x3max) (go label40))
            (setf s3 (+ one (* s3 (expt (/ x3max xabs) 2))))
            (setf x3max xabs)
            (go label50)
           label40
            (if (/= xabs zero) (setf s3 (+ s3 (expt (/ xabs x3max) 2))))
           label50
           label60
            (go label80)
           label70
            '""
            '"           sum for intermediate components."
            '""
            (setf s2 (+ s2 (expt xabs 2)))
           label80
           label90))
        '""
        '"     calculation of norm."
        '""
        (if (= s1 zero) (go label100))
        (setf enorm (* x1max (f2cl-lib:dsqrt (+ s1 (/ (/ s2 x1max) x1max)))))
        (go label130)
       label100
        (if (= s2 zero) (go label110))
        (if (>= s2 x3max)
            (setf enorm
                    (f2cl-lib:dsqrt
                     (* s2 (+ one (* (/ x3max s2) (* x3max s3)))))))
        (if (< s2 x3max)
            (setf enorm
                    (f2cl-lib:dsqrt (* x3max (+ (/ s2 x3max) (* x3max s3))))))
        (go label120)
       label110
        (setf enorm (* x3max (f2cl-lib:dsqrt s3)))
       label120
       label130
        (go end_label)
        '""
        '"     last card of function enorm."
        '""
       end_label
        (return (values enorm nil nil))))))

