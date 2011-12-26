;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqawce
       (f a b c epsabs epsrel limit result abserr neval ier alist blist rlist
        elist iord last$)
  (declare (type (array f2cl-lib:integer4 (*)) iord)
   (type (array double-float (*)) elist rlist blist alist)
   (type f2cl-lib:integer4 last$ ier neval limit)
   (type double-float abserr result epsrel epsabs c b a))
  (f2cl-lib:with-multi-array-data
      ((alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%))
    (prog ((iroff1 0) (iroff2 0) (k 0) (krule 0) (maxerr 0) (nev 0) (nrmax 0)
           (aa 0.0d0) (area 0.0d0) (area1 0.0d0) (area12 0.0d0) (area2 0.0d0)
           (a1 0.0d0) (a2 0.0d0) (bb 0.0d0) (b1 0.0d0) (b2 0.0d0)
           (epmach 0.0d0) (errbnd 0.0d0) (errmax 0.0d0) (error1 0.0d0)
           (erro12 0.0d0) (error2 0.0d0) (errsum 0.0d0) (uflow 0.0d0))
      (declare
       (type double-float uflow errsum error2 erro12 error1 errmax errbnd
        epmach b2 b1 bb a2 a1 area2 area12 area1 area aa)
       (type f2cl-lib:integer4 nrmax nev maxerr krule k iroff2 iroff1))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf ier 6)
      (setf neval 0)
      (setf last$ 0)
      (f2cl-lib:fset
       (f2cl-lib:fref alist-%data% (1) ((1 limit)) alist-%offset%)
       a)
      (f2cl-lib:fset
       (f2cl-lib:fref blist-%data% (1) ((1 limit)) blist-%offset%)
       b)
      (f2cl-lib:fset
       (f2cl-lib:fref rlist-%data% (1) ((1 limit)) rlist-%offset%)
       0.0d0)
      (f2cl-lib:fset
       (f2cl-lib:fref elist-%data% (1) ((1 limit)) elist-%offset%)
       0.0d0)
      (f2cl-lib:fset (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%)
                     0)
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (if
       (or (= c a)
           (= c b)
           (and (<= epsabs 0.0d0)
                (< epsrel (f2cl-lib:dmax1 (* 50.0d0 epmach) 5.0d-29))))
       (go label999))
      (setf aa a)
      (setf bb b)
      (if (<= a b) (go label10))
      (setf aa b)
      (setf bb a)
     label10
      (setf ier 0)
      (setf krule 1)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (dqc25c f aa bb c result abserr krule neval)
        (declare (ignore var-0 var-1 var-2))
        (setf c var-3)
        (setf result var-4)
        (setf abserr var-5)
        (setf krule var-6)
        (setf neval var-7))
      (setf last$ 1)
      (f2cl-lib:fset
       (f2cl-lib:fref rlist-%data% (1) ((1 limit)) rlist-%offset%)
       result)
      (f2cl-lib:fset
       (f2cl-lib:fref elist-%data% (1) ((1 limit)) elist-%offset%)
       abserr)
      (f2cl-lib:fset (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%)
                     1)
      (f2cl-lib:fset
       (f2cl-lib:fref alist-%data% (1) ((1 limit)) alist-%offset%)
       a)
      (f2cl-lib:fset
       (f2cl-lib:fref blist-%data% (1) ((1 limit)) blist-%offset%)
       b)
      (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs result))))
      (if (= limit 1) (setf ier 1))
      (if
       (or
        (< abserr
           (f2cl-lib:dmin1 (* 0.010000000000000002d0 (f2cl-lib:dabs result))
                           errbnd))
        (= ier 1))
       (go label70))
      (f2cl-lib:fset
       (f2cl-lib:fref alist-%data% (1) ((1 limit)) alist-%offset%)
       aa)
      (f2cl-lib:fset
       (f2cl-lib:fref blist-%data% (1) ((1 limit)) blist-%offset%)
       bb)
      (f2cl-lib:fset
       (f2cl-lib:fref rlist-%data% (1) ((1 limit)) rlist-%offset%)
       result)
      (setf errmax abserr)
      (setf maxerr 1)
      (setf area result)
      (setf errsum abserr)
      (setf nrmax 1)
      (setf iroff1 0)
      (setf iroff2 0)
      (f2cl-lib:fdo (last$ 2 (f2cl-lib:int-add last$ 1))
                    ((> last$ limit) nil)
        (tagbody
          (setf a1
                  (f2cl-lib:fref alist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 alist-%offset%))
          (setf b1
                  (* 0.5d0
                     (+
                      (f2cl-lib:fref alist-%data%
                                     (maxerr)
                                     ((1 limit))
                                     alist-%offset%)
                      (f2cl-lib:fref blist-%data%
                                     (maxerr)
                                     ((1 limit))
                                     blist-%offset%))))
          (setf b2
                  (f2cl-lib:fref blist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 blist-%offset%))
          (if (and (<= c b1) (> c a1)) (setf b1 (* 0.5d0 (+ c b2))))
          (if (and (> c b1) (< c b2)) (setf b1 (* 0.5d0 (+ a1 c))))
          (setf a2 b1)
          (setf krule 2)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (dqc25c f a1 b1 c area1 error1 krule nev)
            (declare (ignore var-0 var-1 var-2))
            (setf c var-3)
            (setf area1 var-4)
            (setf error1 var-5)
            (setf krule var-6)
            (setf nev var-7))
          (setf neval (f2cl-lib:int-add neval nev))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (dqc25c f a2 b2 c area2 error2 krule nev)
            (declare (ignore var-0 var-1 var-2))
            (setf c var-3)
            (setf area2 var-4)
            (setf error2 var-5)
            (setf krule var-6)
            (setf nev var-7))
          (setf neval (f2cl-lib:int-add neval nev))
          (setf area12 (+ area1 area2))
          (setf erro12 (+ error1 error2))
          (setf errsum (- (+ errsum erro12) errmax))
          (setf area
                  (- (+ area area12)
                     (f2cl-lib:fref rlist-%data%
                                    (maxerr)
                                    ((1 limit))
                                    rlist-%offset%)))
          (if
           (and
            (<
             (f2cl-lib:dabs
              (-
               (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
               area12))
             (* 1.0d-5 (f2cl-lib:dabs area12)))
            (>= erro12 (* 0.99d0 errmax))
            (= krule 0))
           (setf iroff1 (f2cl-lib:int-add iroff1 1)))
          (if (and (> last$ 10) (> erro12 errmax) (= krule 0))
              (setf iroff2 (f2cl-lib:int-add iroff2 1)))
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
           area1)
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (last$) ((1 limit)) rlist-%offset%)
           area2)
          (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs area))))
          (if (<= errsum errbnd) (go label15))
          (if (and (>= iroff1 6) (> iroff2 20)) (setf ier 2))
          (if (= last$ limit) (setf ier 1))
          (if
           (<= (f2cl-lib:dmax1 (f2cl-lib:dabs a1) (f2cl-lib:dabs b2))
               (* (+ 1.0d0 (* 100.0d0 epmach))
                  (+ (f2cl-lib:dabs a2) (* 1000.0d0 uflow))))
           (setf ier 3))
         label15
          (if (> error2 error1) (go label20))
          (f2cl-lib:fset
           (f2cl-lib:fref alist-%data% (last$) ((1 limit)) alist-%offset%)
           a2)
          (f2cl-lib:fset
           (f2cl-lib:fref blist-%data% (maxerr) ((1 limit)) blist-%offset%)
           b1)
          (f2cl-lib:fset
           (f2cl-lib:fref blist-%data% (last$) ((1 limit)) blist-%offset%)
           b2)
          (f2cl-lib:fset
           (f2cl-lib:fref elist-%data% (maxerr) ((1 limit)) elist-%offset%)
           error1)
          (f2cl-lib:fset
           (f2cl-lib:fref elist-%data% (last$) ((1 limit)) elist-%offset%)
           error2)
          (go label30)
         label20
          (f2cl-lib:fset
           (f2cl-lib:fref alist-%data% (maxerr) ((1 limit)) alist-%offset%)
           a2)
          (f2cl-lib:fset
           (f2cl-lib:fref alist-%data% (last$) ((1 limit)) alist-%offset%)
           a1)
          (f2cl-lib:fset
           (f2cl-lib:fref blist-%data% (last$) ((1 limit)) blist-%offset%)
           b1)
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
           area2)
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (last$) ((1 limit)) rlist-%offset%)
           area1)
          (f2cl-lib:fset
           (f2cl-lib:fref elist-%data% (maxerr) ((1 limit)) elist-%offset%)
           error2)
          (f2cl-lib:fset
           (f2cl-lib:fref elist-%data% (last$) ((1 limit)) elist-%offset%)
           error1)
         label30
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqpsrt limit last$ maxerr errmax elist iord nrmax)
            (declare (ignore var-0 var-1 var-4 var-5))
            (setf maxerr var-2)
            (setf errmax var-3)
            (setf nrmax var-6))
          (if (or (/= ier 0) (<= errsum errbnd)) (go label50))
         label40))
     label50
      (setf result 0.0d0)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k last$) nil)
        (tagbody
          (setf result
                  (+ result
                     (f2cl-lib:fref rlist-%data%
                                    (k)
                                    ((1 limit))
                                    rlist-%offset%)))
         label60))
      (setf abserr errsum)
     label70
      (if (= aa b) (setf result (- result)))
     label999
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               c
               nil
               nil
               nil
               result
               abserr
               neval
               ier
               nil
               nil
               nil
               nil
               nil
               last$)))))

