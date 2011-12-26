;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqwgtf (x omega p2 p3 p4 integr)
  (declare (type f2cl-lib:integer4 integr)
   (type double-float p4 p3 p2 omega x))
  (f2cl-lib:with-multi-array-data
      nil
    (prog ((omx 0.0d0) (dqwgtf 0.0d0))
      (declare (type double-float dqwgtf omx))
      (setf omx (* omega x))
      (f2cl-lib:computed-goto (label10 label20) integr)
     label10
      (setf dqwgtf (f2cl-lib:dcos omx))
      (go label30)
     label20
      (setf dqwgtf (f2cl-lib:dsin omx))
     label30
      (go end_label)
     end_label
      (return (values dqwgtf nil nil nil nil nil nil)))))

