;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqagi
       (f bound inf epsabs epsrel result abserr neval ier limit lenw last$
        iwork work)
  (declare (type (array double-float (*)) work)
   (type (array f2cl-lib:integer4 (*)) iwork)
   (type f2cl-lib:integer4 last$ lenw limit ier neval inf)
   (type double-float abserr result epsrel epsabs bound))
  (f2cl-lib:with-multi-array-data
      ((iwork f2cl-lib:integer4 iwork-%data% iwork-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((lvl 0) (l1 0) (l2 0) (l3 0))
      (declare (type f2cl-lib:integer4 l3 l2 l1 lvl))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (if (or (< limit 1) (< lenw (f2cl-lib:int-mul limit 4))) (go label10))
      (setf l1 (f2cl-lib:int-add limit 1))
      (setf l2 (f2cl-lib:int-add limit l1))
      (setf l3 (f2cl-lib:int-add limit l2))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
           var-11 var-12 var-13 var-14 var-15)
          (dqagie f bound inf epsabs epsrel limit result abserr neval ier
           (f2cl-lib:array-slice work double-float (1) ((1 lenw)))
           (f2cl-lib:array-slice work double-float (l1) ((1 lenw)))
           (f2cl-lib:array-slice work double-float (l2) ((1 lenw)))
           (f2cl-lib:array-slice work double-float (l3) ((1 lenw))) iwork
           last$)
        (declare
         (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-10 var-11 var-12
          var-13 var-14))
        (setf result var-6)
        (setf abserr var-7)
        (setf neval var-8)
        (setf ier var-9)
        (setf last$ var-15))
      (setf lvl 0)
     label10
      (if (= ier 6) (setf lvl 1))
      (if (/= ier 0)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3)
              (xerror "abnormal return from dqagi" 26 ier lvl)
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
               nil
               result
               abserr
               neval
               ier
               nil
               nil
               last$
               nil
               nil)))))

