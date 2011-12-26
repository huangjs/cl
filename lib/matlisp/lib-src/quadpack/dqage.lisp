;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqage
       (f a b epsabs epsrel key limit result abserr neval ier alist blist rlist
        elist iord last$)
  (declare (type (array f2cl-lib:integer4 (*)) iord)
   (type (array double-float (*)) elist rlist blist alist)
   (type f2cl-lib:integer4 last$ ier neval limit key)
   (type double-float abserr result epsrel epsabs b a))
  (f2cl-lib:with-multi-array-data
      ((alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%))
    (prog ((iroff1 0) (iroff2 0) (k 0) (keyf 0) (maxerr 0) (nrmax 0)
           (area 0.0d0) (area1 0.0d0) (area12 0.0d0) (area2 0.0d0) (a1 0.0d0)
           (a2 0.0d0) (b1 0.0d0) (b2 0.0d0) (defabs 0.0d0) (defab1 0.0d0)
           (defab2 0.0d0) (epmach 0.0d0) (errbnd 0.0d0) (errmax 0.0d0)
           (error1 0.0d0) (error2 0.0d0) (erro12 0.0d0) (errsum 0.0d0)
           (resabs 0.0d0) (uflow 0.0d0))
      (declare
       (type double-float uflow resabs errsum erro12 error2 error1 errmax
        errbnd epmach defab2 defab1 defabs b2 b1 a2 a1 area2 area12 area1 area)
       (type f2cl-lib:integer4 nrmax maxerr keyf k iroff2 iroff1))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf ier 0)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0d0)
      (setf abserr 0.0d0)
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
      (if
       (and (<= epsabs 0.0d0)
            (< epsrel (f2cl-lib:dmax1 (* 50.0d0 epmach) 5.0d-29)))
       (setf ier 6))
      (if (= ier 6) (go label999))
      (setf keyf key)
      (if (<= key 0) (setf keyf 1))
      (if (>= key 7) (setf keyf 6))
      (setf neval 0)
      (if (= keyf 1)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk15 f a b result abserr defabs resabs)
            (declare (ignore var-0 var-1 var-2))
            (setf result var-3)
            (setf abserr var-4)
            (setf defabs var-5)
            (setf resabs var-6)))
      (if (= keyf 2)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk21 f a b result abserr defabs resabs)
            (declare (ignore var-0 var-1 var-2))
            (setf result var-3)
            (setf abserr var-4)
            (setf defabs var-5)
            (setf resabs var-6)))
      (if (= keyf 3)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk31 f a b result abserr defabs resabs)
            (declare (ignore var-0 var-1 var-2))
            (setf result var-3)
            (setf abserr var-4)
            (setf defabs var-5)
            (setf resabs var-6)))
      (if (= keyf 4)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk41 f a b result abserr defabs resabs)
            (declare (ignore var-0 var-1 var-2))
            (setf result var-3)
            (setf abserr var-4)
            (setf defabs var-5)
            (setf resabs var-6)))
      (if (= keyf 5)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk51 f a b result abserr defabs resabs)
            (declare (ignore var-0 var-1 var-2))
            (setf result var-3)
            (setf abserr var-4)
            (setf defabs var-5)
            (setf resabs var-6)))
      (if (= keyf 6)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk61 f a b result abserr defabs resabs)
            (declare (ignore var-0 var-1 var-2))
            (setf result var-3)
            (setf abserr var-4)
            (setf defabs var-5)
            (setf resabs var-6)))
      (setf last$ 1)
      (f2cl-lib:fset
       (f2cl-lib:fref rlist-%data% (1) ((1 limit)) rlist-%offset%)
       result)
      (f2cl-lib:fset
       (f2cl-lib:fref elist-%data% (1) ((1 limit)) elist-%offset%)
       abserr)
      (f2cl-lib:fset (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%)
                     1)
      (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs result))))
      (if (and (<= abserr (* 50.0d0 epmach defabs)) (> abserr errbnd))
          (setf ier 2))
      (if (= limit 1) (setf ier 1))
      (if
       (or (/= ier 0)
           (and (<= abserr errbnd) (/= abserr resabs))
           (= abserr 0.0d0))
       (go label60))
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
          (setf a2 b1)
          (setf b2
                  (f2cl-lib:fref blist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 blist-%offset%))
          (if (= keyf 1)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk15 f a1 b1 area1 error1 resabs defab1)
                (declare (ignore var-0 var-1 var-2))
                (setf area1 var-3)
                (setf error1 var-4)
                (setf resabs var-5)
                (setf defab1 var-6)))
          (if (= keyf 2)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk21 f a1 b1 area1 error1 resabs defab1)
                (declare (ignore var-0 var-1 var-2))
                (setf area1 var-3)
                (setf error1 var-4)
                (setf resabs var-5)
                (setf defab1 var-6)))
          (if (= keyf 3)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk31 f a1 b1 area1 error1 resabs defab1)
                (declare (ignore var-0 var-1 var-2))
                (setf area1 var-3)
                (setf error1 var-4)
                (setf resabs var-5)
                (setf defab1 var-6)))
          (if (= keyf 4)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk41 f a1 b1 area1 error1 resabs defab1)
                (declare (ignore var-0 var-1 var-2))
                (setf area1 var-3)
                (setf error1 var-4)
                (setf resabs var-5)
                (setf defab1 var-6)))
          (if (= keyf 5)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk51 f a1 b1 area1 error1 resabs defab1)
                (declare (ignore var-0 var-1 var-2))
                (setf area1 var-3)
                (setf error1 var-4)
                (setf resabs var-5)
                (setf defab1 var-6)))
          (if (= keyf 6)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk61 f a1 b1 area1 error1 resabs defab1)
                (declare (ignore var-0 var-1 var-2))
                (setf area1 var-3)
                (setf error1 var-4)
                (setf resabs var-5)
                (setf defab1 var-6)))
          (if (= keyf 1)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk15 f a2 b2 area2 error2 resabs defab2)
                (declare (ignore var-0 var-1 var-2))
                (setf area2 var-3)
                (setf error2 var-4)
                (setf resabs var-5)
                (setf defab2 var-6)))
          (if (= keyf 2)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk21 f a2 b2 area2 error2 resabs defab2)
                (declare (ignore var-0 var-1 var-2))
                (setf area2 var-3)
                (setf error2 var-4)
                (setf resabs var-5)
                (setf defab2 var-6)))
          (if (= keyf 3)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk31 f a2 b2 area2 error2 resabs defab2)
                (declare (ignore var-0 var-1 var-2))
                (setf area2 var-3)
                (setf error2 var-4)
                (setf resabs var-5)
                (setf defab2 var-6)))
          (if (= keyf 4)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk41 f a2 b2 area2 error2 resabs defab2)
                (declare (ignore var-0 var-1 var-2))
                (setf area2 var-3)
                (setf error2 var-4)
                (setf resabs var-5)
                (setf defab2 var-6)))
          (if (= keyf 5)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk51 f a2 b2 area2 error2 resabs defab2)
                (declare (ignore var-0 var-1 var-2))
                (setf area2 var-3)
                (setf error2 var-4)
                (setf resabs var-5)
                (setf defab2 var-6)))
          (if (= keyf 6)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (dqk61 f a2 b2 area2 error2 resabs defab2)
                (declare (ignore var-0 var-1 var-2))
                (setf area2 var-3)
                (setf error2 var-4)
                (setf resabs var-5)
                (setf defab2 var-6)))
          (setf neval (f2cl-lib:int-add neval 1))
          (setf area12 (+ area1 area2))
          (setf erro12 (+ error1 error2))
          (setf errsum (- (+ errsum erro12) errmax))
          (setf area
                  (- (+ area area12)
                     (f2cl-lib:fref rlist-%data%
                                    (maxerr)
                                    ((1 limit))
                                    rlist-%offset%)))
          (if (or (= defab1 error1) (= defab2 error2)) (go label5))
          (if
           (and
            (<=
             (f2cl-lib:dabs
              (-
               (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
               area12))
             (* 1.0d-5 (f2cl-lib:dabs area12)))
            (>= erro12 (* 0.99d0 errmax)))
           (setf iroff1 (f2cl-lib:int-add iroff1 1)))
          (if (and (> last$ 10) (> erro12 errmax))
              (setf iroff2 (f2cl-lib:int-add iroff2 1)))
         label5
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
           area1)
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (last$) ((1 limit)) rlist-%offset%)
           area2)
          (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs area))))
          (if (<= errsum errbnd) (go label8))
          (if (or (>= iroff1 6) (>= iroff2 20)) (setf ier 2))
          (if (= last$ limit) (setf ier 1))
          (if
           (<= (f2cl-lib:dmax1 (f2cl-lib:dabs a1) (f2cl-lib:dabs b2))
               (* (+ 1.0d0 (* 100.0d0 epmach))
                  (+ (f2cl-lib:dabs a2) (* 1000.0d0 uflow))))
           (setf ier 3))
         label8
          (if (> error2 error1) (go label10))
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
          (go label20)
         label10
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
         label20
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqpsrt limit last$ maxerr errmax elist iord nrmax)
            (declare (ignore var-0 var-1 var-4 var-5))
            (setf maxerr var-2)
            (setf errmax var-3)
            (setf nrmax var-6))
          (if (or (/= ier 0) (<= errsum errbnd)) (go label40))
         label30))
     label40
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
         label50))
      (setf abserr errsum)
     label60
      (if (/= keyf 1)
          (setf neval
                  (f2cl-lib:int-mul
                   (f2cl-lib:int-add (f2cl-lib:int-mul 10 keyf) 1)
                   (f2cl-lib:int-add (f2cl-lib:int-mul 2 neval) 1))))
      (if (= keyf 1)
          (setf neval (f2cl-lib:int-add (f2cl-lib:int-mul 30 neval) 15)))
     label999
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
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
               nil
               nil
               nil
               last$)))))

