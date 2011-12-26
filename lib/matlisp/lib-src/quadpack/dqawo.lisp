;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqawo
       (f a b omega integr epsabs epsrel result abserr neval ier leniw maxp1
        lenw last$ iwork work)
  (declare (type (array double-float (*)) work)
   (type (array f2cl-lib:integer4 (*)) iwork)
   (type f2cl-lib:integer4 last$ lenw maxp1 leniw ier neval integr)
   (type double-float abserr result epsrel epsabs omega b a))
  (f2cl-lib:with-multi-array-data
      ((iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((limit 0) (lvl 0) (l1 0) (l2 0) (l3 0) (l4 0) (momcom 0))
      (declare (type f2cl-lib:integer4 momcom l4 l3 l2 l1 lvl limit))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (if
       (or (< leniw 2)
           (< maxp1 1)
           (< lenw
              (f2cl-lib:int-add (f2cl-lib:int-mul leniw 2)
                                (f2cl-lib:int-mul maxp1 25))))
       (go label10))
      (setf limit (the f2cl-lib:integer4 (truncate leniw 2)))
      (setf l1 (f2cl-lib:int-add limit 1))
      (setf l2 (f2cl-lib:int-add limit l1))
      (setf l3 (f2cl-lib:int-add limit l2))
      (setf l4 (f2cl-lib:int-add limit l3))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18 var-19
           var-20 var-21 var-22)
          (dqawoe f a b omega integr epsabs epsrel limit 1 maxp1 result abserr
           neval ier last$
           (f2cl-lib:array-slice work double-float (1) ((1 lenw)))
           (f2cl-lib:array-slice work double-float (l1) ((1 lenw)))
           (f2cl-lib:array-slice work double-float (l2) ((1 lenw)))
           (f2cl-lib:array-slice work double-float (l3) ((1 lenw)))
           (f2cl-lib:array-slice iwork f2cl-lib:integer4 (1) ((1 leniw)))
           (f2cl-lib:array-slice iwork f2cl-lib:integer4 (l1) ((1 leniw)))
           momcom (f2cl-lib:array-slice work double-float (l4) ((1 lenw))))
        (declare
         (ignore var-0 var-1 var-2 var-3 var-5 var-6 var-7 var-8 var-9 var-15
          var-16 var-17 var-18 var-19 var-20 var-22))
        (setf integr var-4)
        (setf result var-10)
        (setf abserr var-11)
        (setf neval var-12)
        (setf ier var-13)
        (setf last$ var-14)
        (setf momcom var-21))
      (setf lvl 0)
     label10
      (if (= ier 6) (setf lvl 0))
      (if (/= ier 0)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3)
              (xerror "abnormal return from dqawo" 26 ier lvl)
            (declare (ignore var-0 var-1))
            (setf ier var-2)
            (setf lvl var-3)))
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               integr
               nil
               nil
               result
               abserr
               neval
               ier
               nil
               nil
               nil
               last$
               nil
               nil)))))

