;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqcheb (x fval cheb12 cheb24)
  (declare (type (array double-float (*)) cheb12)
   (type (array double-float (*)) cheb24 fval)
   (type (array double-float (*)) x))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (fval double-float fval-%data% fval-%offset%)
       (cheb24 double-float cheb24-%data% cheb24-%offset%)
       (cheb12 double-float cheb12-%data% cheb12-%offset%))
    (prog ((v (make-array 12 :element-type 'double-float)) (i 0) (j 0)
           (alam 0.0d0) (alam1 0.0d0) (alam2 0.0d0) (part1 0.0d0) (part2 0.0d0)
           (part3 0.0d0))
      (declare (type (array double-float (12)) v)
       (type double-float part3 part2 part1 alam2 alam1 alam)
       (type f2cl-lib:integer4 j i))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 12) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub 26 i))
          (f2cl-lib:fset (f2cl-lib:fref v (i) ((1 12)))
                         (-
                          (f2cl-lib:fref fval-%data%
                                         (i)
                                         ((1 25))
                                         fval-%offset%)
                          (f2cl-lib:fref fval-%data%
                                         (j)
                                         ((1 25))
                                         fval-%offset%)))
          (f2cl-lib:fset (f2cl-lib:fref fval-%data% (i) ((1 25)) fval-%offset%)
                         (+
                          (f2cl-lib:fref fval-%data%
                                         (i)
                                         ((1 25))
                                         fval-%offset%)
                          (f2cl-lib:fref fval-%data%
                                         (j)
                                         ((1 25))
                                         fval-%offset%)))
         label10))
      (setf alam1
              (- (f2cl-lib:fref v (1) ((1 12)))
                 (f2cl-lib:fref v (9) ((1 12)))))
      (setf alam2
              (* (f2cl-lib:fref x-%data% (6) ((1 11)) x-%offset%)
                 (- (f2cl-lib:fref v (3) ((1 12)))
                    (f2cl-lib:fref v (7) ((1 12)))
                    (f2cl-lib:fref v (11) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (4) ((1 13)) cheb12-%offset%)
                     (+ alam1 alam2))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb12-%data% (10) ((1 13)) cheb12-%offset%)
       (- alam1 alam2))
      (setf alam1
              (- (f2cl-lib:fref v (2) ((1 12)))
                 (f2cl-lib:fref v (8) ((1 12)))
                 (f2cl-lib:fref v (10) ((1 12)))))
      (setf alam2
              (- (f2cl-lib:fref v (4) ((1 12)))
                 (f2cl-lib:fref v (6) ((1 12)))
                 (f2cl-lib:fref v (12) ((1 12)))))
      (setf alam
              (+ (* (f2cl-lib:fref x-%data% (3) ((1 11)) x-%offset%) alam1)
                 (* (f2cl-lib:fref x-%data% (9) ((1 11)) x-%offset%) alam2)))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (4) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (4)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (22) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (4) ((1 13)) cheb12-%offset%) alam))
      (setf alam
              (- (* (f2cl-lib:fref x-%data% (9) ((1 11)) x-%offset%) alam1)
                 (* (f2cl-lib:fref x-%data% (3) ((1 11)) x-%offset%) alam2)))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (10) ((1 25)) cheb24-%offset%)
       (+ (f2cl-lib:fref cheb12-%data% (10) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (16) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (10) ((1 13)) cheb12-%offset%) alam))
      (setf part1
              (* (f2cl-lib:fref x-%data% (4) ((1 11)) x-%offset%)
                 (f2cl-lib:fref v (5) ((1 12)))))
      (setf part2
              (* (f2cl-lib:fref x-%data% (8) ((1 11)) x-%offset%)
                 (f2cl-lib:fref v (9) ((1 12)))))
      (setf part3
              (* (f2cl-lib:fref x-%data% (6) ((1 11)) x-%offset%)
                 (f2cl-lib:fref v (7) ((1 12)))))
      (setf alam1 (+ (f2cl-lib:fref v (1) ((1 12))) part1 part2))
      (setf alam2
              (+
               (* (f2cl-lib:fref x-%data% (2) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (3) ((1 12))))
               part3
               (* (f2cl-lib:fref x-%data% (10) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (11) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (2) ((1 13)) cheb12-%offset%)
                     (+ alam1 alam2))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb12-%data% (12) ((1 13)) cheb12-%offset%)
       (- alam1 alam2))
      (setf alam
              (+
               (* (f2cl-lib:fref x-%data% (1) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (2) ((1 12))))
               (* (f2cl-lib:fref x-%data% (3) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (4) ((1 12))))
               (* (f2cl-lib:fref x-%data% (5) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (6) ((1 12))))
               (* (f2cl-lib:fref x-%data% (7) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (8) ((1 12))))
               (* (f2cl-lib:fref x-%data% (9) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (10) ((1 12))))
               (* (f2cl-lib:fref x-%data% (11) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (12) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (2) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (2)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (24) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (2) ((1 13)) cheb12-%offset%) alam))
      (setf alam
              (-
               (+
                (-
                 (+
                  (-
                   (* (f2cl-lib:fref x-%data% (11) ((1 11)) x-%offset%)
                      (f2cl-lib:fref v (2) ((1 12))))
                   (* (f2cl-lib:fref x-%data% (9) ((1 11)) x-%offset%)
                      (f2cl-lib:fref v (4) ((1 12)))))
                  (* (f2cl-lib:fref x-%data% (7) ((1 11)) x-%offset%)
                     (f2cl-lib:fref v (6) ((1 12)))))
                 (* (f2cl-lib:fref x-%data% (5) ((1 11)) x-%offset%)
                    (f2cl-lib:fref v (8) ((1 12)))))
                (* (f2cl-lib:fref x-%data% (3) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (10) ((1 12)))))
               (* (f2cl-lib:fref x-%data% (1) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (12) ((1 12))))))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (12) ((1 25)) cheb24-%offset%)
       (+ (f2cl-lib:fref cheb12-%data% (12) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (14) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (12) ((1 13)) cheb12-%offset%) alam))
      (setf alam1 (+ (- (f2cl-lib:fref v (1) ((1 12))) part1) part2))
      (setf alam2
              (+
               (-
                (* (f2cl-lib:fref x-%data% (10) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (3) ((1 12))))
                part3)
               (* (f2cl-lib:fref x-%data% (2) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (11) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (6) ((1 13)) cheb12-%offset%)
                     (+ alam1 alam2))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (8) ((1 13)) cheb12-%offset%)
                     (- alam1 alam2))
      (setf alam
              (+
               (-
                (* (f2cl-lib:fref x-%data% (5) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (2) ((1 12))))
                (* (f2cl-lib:fref x-%data% (9) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (4) ((1 12))))
                (* (f2cl-lib:fref x-%data% (1) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (6) ((1 12))))
                (* (f2cl-lib:fref x-%data% (11) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (8) ((1 12)))))
               (* (f2cl-lib:fref x-%data% (3) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (10) ((1 12))))
               (* (f2cl-lib:fref x-%data% (7) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (12) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (6) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (6)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (20) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (6) ((1 13)) cheb12-%offset%) alam))
      (setf alam
              (-
               (+
                (-
                 (* (f2cl-lib:fref x-%data% (7) ((1 11)) x-%offset%)
                    (f2cl-lib:fref v (2) ((1 12))))
                 (* (f2cl-lib:fref x-%data% (3) ((1 11)) x-%offset%)
                    (f2cl-lib:fref v (4) ((1 12))))
                 (* (f2cl-lib:fref x-%data% (11) ((1 11)) x-%offset%)
                    (f2cl-lib:fref v (6) ((1 12)))))
                (* (f2cl-lib:fref x-%data% (1) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (8) ((1 12)))))
               (* (f2cl-lib:fref x-%data% (9) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (10) ((1 12))))
               (* (f2cl-lib:fref x-%data% (5) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (12) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (8) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (8)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (18) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (8) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 6) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub 14 i))
          (f2cl-lib:fset (f2cl-lib:fref v (i) ((1 12)))
                         (-
                          (f2cl-lib:fref fval-%data%
                                         (i)
                                         ((1 25))
                                         fval-%offset%)
                          (f2cl-lib:fref fval-%data%
                                         (j)
                                         ((1 25))
                                         fval-%offset%)))
          (f2cl-lib:fset (f2cl-lib:fref fval-%data% (i) ((1 25)) fval-%offset%)
                         (+
                          (f2cl-lib:fref fval-%data%
                                         (i)
                                         ((1 25))
                                         fval-%offset%)
                          (f2cl-lib:fref fval-%data%
                                         (j)
                                         ((1 25))
                                         fval-%offset%)))
         label20))
      (setf alam1
              (+ (f2cl-lib:fref v (1) ((1 12)))
                 (* (f2cl-lib:fref x-%data% (8) ((1 11)) x-%offset%)
                    (f2cl-lib:fref v (5) ((1 12))))))
      (setf alam2
              (* (f2cl-lib:fref x-%data% (4) ((1 11)) x-%offset%)
                 (f2cl-lib:fref v (3) ((1 12)))))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (3) ((1 13)) cheb12-%offset%)
                     (+ alam1 alam2))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb12-%data% (11) ((1 13)) cheb12-%offset%)
       (- alam1 alam2))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (7) ((1 13)) cheb12-%offset%)
                     (- (f2cl-lib:fref v (1) ((1 12)))
                        (f2cl-lib:fref v (5) ((1 12)))))
      (setf alam
              (+
               (* (f2cl-lib:fref x-%data% (2) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (2) ((1 12))))
               (* (f2cl-lib:fref x-%data% (6) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (4) ((1 12))))
               (* (f2cl-lib:fref x-%data% (10) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (6) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (3) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (3)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (23) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (3) ((1 13)) cheb12-%offset%) alam))
      (setf alam
              (* (f2cl-lib:fref x-%data% (6) ((1 11)) x-%offset%)
                 (- (f2cl-lib:fref v (2) ((1 12)))
                    (f2cl-lib:fref v (4) ((1 12)))
                    (f2cl-lib:fref v (6) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (7) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (7)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (19) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (7) ((1 13)) cheb12-%offset%) alam))
      (setf alam
              (+
               (-
                (* (f2cl-lib:fref x-%data% (10) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (2) ((1 12))))
                (* (f2cl-lib:fref x-%data% (6) ((1 11)) x-%offset%)
                   (f2cl-lib:fref v (4) ((1 12)))))
               (* (f2cl-lib:fref x-%data% (2) ((1 11)) x-%offset%)
                  (f2cl-lib:fref v (6) ((1 12))))))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (11) ((1 25)) cheb24-%offset%)
       (+ (f2cl-lib:fref cheb12-%data% (11) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (15) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (11) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 3) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub 8 i))
          (f2cl-lib:fset (f2cl-lib:fref v (i) ((1 12)))
                         (-
                          (f2cl-lib:fref fval-%data%
                                         (i)
                                         ((1 25))
                                         fval-%offset%)
                          (f2cl-lib:fref fval-%data%
                                         (j)
                                         ((1 25))
                                         fval-%offset%)))
          (f2cl-lib:fset (f2cl-lib:fref fval-%data% (i) ((1 25)) fval-%offset%)
                         (+
                          (f2cl-lib:fref fval-%data%
                                         (i)
                                         ((1 25))
                                         fval-%offset%)
                          (f2cl-lib:fref fval-%data%
                                         (j)
                                         ((1 25))
                                         fval-%offset%)))
         label30))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (5) ((1 13)) cheb12-%offset%)
                     (+ (f2cl-lib:fref v (1) ((1 12)))
                        (* (f2cl-lib:fref x-%data% (8) ((1 11)) x-%offset%)
                           (f2cl-lib:fref v (3) ((1 12))))))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (9) ((1 13)) cheb12-%offset%)
                     (- (f2cl-lib:fref fval-%data% (1) ((1 25)) fval-%offset%)
                        (* (f2cl-lib:fref x-%data% (8) ((1 11)) x-%offset%)
                           (f2cl-lib:fref fval-%data%
                                          (3)
                                          ((1 25))
                                          fval-%offset%))))
      (setf alam
              (* (f2cl-lib:fref x-%data% (4) ((1 11)) x-%offset%)
                 (f2cl-lib:fref v (2) ((1 12)))))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (5) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (5)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (21) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (5) ((1 13)) cheb12-%offset%) alam))
      (setf alam
              (-
               (* (f2cl-lib:fref x-%data% (8) ((1 11)) x-%offset%)
                  (f2cl-lib:fref fval-%data% (2) ((1 25)) fval-%offset%))
               (f2cl-lib:fref fval-%data% (4) ((1 25)) fval-%offset%)))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (9) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (9)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (17) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (9) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (1) ((1 13)) cheb12-%offset%)
                     (+ (f2cl-lib:fref fval-%data% (1) ((1 25)) fval-%offset%)
                        (f2cl-lib:fref fval-%data%
                                       (3)
                                       ((1 25))
                                       fval-%offset%)))
      (setf alam
              (+ (f2cl-lib:fref fval-%data% (2) ((1 25)) fval-%offset%)
                 (f2cl-lib:fref fval-%data% (4) ((1 25)) fval-%offset%)))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (1) ((1 25)) cheb24-%offset%)
                     (+
                      (f2cl-lib:fref cheb12-%data%
                                     (1)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (25) ((1 25)) cheb24-%offset%)
       (- (f2cl-lib:fref cheb12-%data% (1) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb12-%data% (13) ((1 13)) cheb12-%offset%)
       (- (f2cl-lib:fref v (1) ((1 12))) (f2cl-lib:fref v (3) ((1 12)))))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (13) ((1 25)) cheb24-%offset%)
       (f2cl-lib:fref cheb12-%data% (13) ((1 13)) cheb12-%offset%))
      (setf alam (/ 1.0d0 6.0d0))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 12) nil)
        (tagbody
          (f2cl-lib:fset
           (f2cl-lib:fref cheb12-%data% (i) ((1 13)) cheb12-%offset%)
           (* (f2cl-lib:fref cheb12-%data% (i) ((1 13)) cheb12-%offset%) alam))
         label40))
      (setf alam (* 0.5d0 alam))
      (f2cl-lib:fset (f2cl-lib:fref cheb12-%data% (1) ((1 13)) cheb12-%offset%)
                     (*
                      (f2cl-lib:fref cheb12-%data%
                                     (1)
                                     ((1 13))
                                     cheb12-%offset%)
                      alam))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb12-%data% (13) ((1 13)) cheb12-%offset%)
       (* (f2cl-lib:fref cheb12-%data% (13) ((1 13)) cheb12-%offset%) alam))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 24) nil)
        (tagbody
          (f2cl-lib:fset
           (f2cl-lib:fref cheb24-%data% (i) ((1 25)) cheb24-%offset%)
           (* (f2cl-lib:fref cheb24-%data% (i) ((1 25)) cheb24-%offset%) alam))
         label50))
      (f2cl-lib:fset (f2cl-lib:fref cheb24-%data% (1) ((1 25)) cheb24-%offset%)
                     (* 0.5d0
                        alam
                        (f2cl-lib:fref cheb24-%data%
                                       (1)
                                       ((1 25))
                                       cheb24-%offset%)))
      (f2cl-lib:fset
       (f2cl-lib:fref cheb24-%data% (25) ((1 25)) cheb24-%offset%)
       (* 0.5d0
          alam
          (f2cl-lib:fref cheb24-%data% (25) ((1 25)) cheb24-%offset%)))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

