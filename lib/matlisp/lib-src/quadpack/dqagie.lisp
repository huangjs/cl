;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqagie
       (f bound inf epsabs epsrel limit result abserr neval ier alist blist
        rlist elist iord last$)
  (declare (type (array f2cl-lib:integer4 (*)) iord)
   (type (array double-float (*)) elist rlist blist alist)
   (type f2cl-lib:integer4 last$ ier neval limit inf)
   (type double-float abserr result epsrel epsabs bound))
  (f2cl-lib:with-multi-array-data
      ((alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%))
    (prog ((res3la (make-array 3 :element-type 'double-float))
           (rlist2 (make-array 52 :element-type 'double-float)) (extrap nil)
           (noext nil) (id 0) (ierro 0) (iroff1 0) (iroff2 0) (iroff3 0)
           (jupbnd 0) (k 0) (ksgn 0) (ktmin 0) (maxerr 0) (nres 0) (nrmax 0)
           (numrl2 0) (abseps 0.0d0) (area 0.0d0) (area1 0.0d0) (area12 0.0d0)
           (area2 0.0d0) (a1 0.0d0) (a2 0.0d0) (boun 0.0d0) (b1 0.0d0)
           (b2 0.0d0) (correc 0.0d0) (defabs 0.0d0) (defab1 0.0d0)
           (defab2 0.0d0) (dres 0.0d0) (epmach 0.0d0) (erlarg 0.0d0)
           (erlast 0.0d0) (errbnd 0.0d0) (errmax 0.0d0) (error1 0.0d0)
           (error2 0.0d0) (erro12 0.0d0) (errsum 0.0d0) (ertest 0.0d0)
           (oflow 0.0d0) (resabs 0.0d0) (reseps 0.0d0) (small 0.0d0)
           (uflow 0.0d0))
      (declare (type (array double-float (52)) rlist2)
       (type (array double-float (3)) res3la)
       (type double-float uflow small reseps resabs oflow ertest errsum erro12
        error2 error1 errmax errbnd erlast erlarg epmach dres defab2 defab1
        defabs correc b2 b1 boun a2 a1 area2 area12 area1 area abseps)
       (type f2cl-lib:integer4 numrl2 nrmax nres maxerr ktmin ksgn k jupbnd
        iroff3 iroff2 iroff1 ierro id)
       (type f2cl-lib:logical noext extrap))
      (setf epmach (f2cl-lib:d1mach 4))
      (setf ier 0)
      (setf neval 0)
      (setf last$ 0)
      (setf result 0.0d0)
      (setf abserr 0.0d0)
      (f2cl-lib:fset
       (f2cl-lib:fref alist-%data% (1) ((1 limit)) alist-%offset%)
       0.0d0)
      (f2cl-lib:fset
       (f2cl-lib:fref blist-%data% (1) ((1 limit)) blist-%offset%)
       1.0d0)
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
      (setf boun bound)
      (if (= inf 2) (setf boun 0.0d0))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (dqk15i f boun inf 0.0d0 1.0d0 result abserr defabs resabs)
        (declare (ignore var-0 var-1 var-2 var-3 var-4))
        (setf result var-5)
        (setf abserr var-6)
        (setf defabs var-7)
        (setf resabs var-8))
      (setf last$ 1)
      (f2cl-lib:fset
       (f2cl-lib:fref rlist-%data% (1) ((1 limit)) rlist-%offset%)
       result)
      (f2cl-lib:fset
       (f2cl-lib:fref elist-%data% (1) ((1 limit)) elist-%offset%)
       abserr)
      (f2cl-lib:fset (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%)
                     1)
      (setf dres (f2cl-lib:dabs result))
      (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel dres)))
      (if (and (<= abserr (* 100.0d0 epmach defabs)) (> abserr errbnd))
          (setf ier 2))
      (if (= limit 1) (setf ier 1))
      (if
       (or (/= ier 0)
           (and (<= abserr errbnd) (/= abserr resabs))
           (= abserr 0.0d0))
       (go label130))
      (setf uflow (f2cl-lib:d1mach 1))
      (setf oflow (f2cl-lib:d1mach 2))
      (f2cl-lib:fset (f2cl-lib:fref rlist2 (1) ((1 52))) result)
      (setf errmax abserr)
      (setf maxerr 1)
      (setf area result)
      (setf errsum abserr)
      (setf abserr oflow)
      (setf nrmax 1)
      (setf nres 0)
      (setf ktmin 0)
      (setf numrl2 2)
      (setf extrap f2cl-lib:%false%)
      (setf noext f2cl-lib:%false%)
      (setf ierro 0)
      (setf iroff1 0)
      (setf iroff2 0)
      (setf iroff3 0)
      (setf ksgn -1)
      (if (>= dres (* (- 1.0d0 (* 50.0d0 epmach)) defabs)) (setf ksgn 1))
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
          (setf erlast errmax)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
              (dqk15i f boun inf a1 b1 area1 error1 resabs defab1)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf area1 var-5)
            (setf error1 var-6)
            (setf resabs var-7)
            (setf defab1 var-8))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
              (dqk15i f boun inf a2 b2 area2 error2 resabs defab2)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf area2 var-5)
            (setf error2 var-6)
            (setf resabs var-7)
            (setf defab2 var-8))
          (setf area12 (+ area1 area2))
          (setf erro12 (+ error1 error2))
          (setf errsum (- (+ errsum erro12) errmax))
          (setf area
                  (- (+ area area12)
                     (f2cl-lib:fref rlist-%data%
                                    (maxerr)
                                    ((1 limit))
                                    rlist-%offset%)))
          (if (or (= defab1 error1) (= defab2 error2)) (go label15))
          (if
           (or
            (>
             (f2cl-lib:dabs
              (-
               (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
               area12))
             (* 1.0d-5 (f2cl-lib:dabs area12)))
            (< erro12 (* 0.99d0 errmax)))
           (go label10))
          (if extrap (setf iroff2 (f2cl-lib:int-add iroff2 1)))
          (if (not extrap) (setf iroff1 (f2cl-lib:int-add iroff1 1)))
         label10
          (if (and (> last$ 10) (> erro12 errmax))
              (setf iroff3 (f2cl-lib:int-add iroff3 1)))
         label15
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
           area1)
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (last$) ((1 limit)) rlist-%offset%)
           area2)
          (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs area))))
          (if (or (>= (f2cl-lib:int-add iroff1 iroff2) 10) (>= iroff3 20))
              (setf ier 2))
          (if (>= iroff2 5) (setf ierro 3))
          (if (= last$ limit) (setf ier 1))
          (if
           (<= (f2cl-lib:dmax1 (f2cl-lib:dabs a1) (f2cl-lib:dabs b2))
               (* (+ 1.0d0 (* 100.0d0 epmach))
                  (+ (f2cl-lib:dabs a2) (* 1000.0d0 uflow))))
           (setf ier 4))
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
          (if (<= errsum errbnd) (go label115))
          (if (/= ier 0) (go label100))
          (if (= last$ 2) (go label80))
          (if noext (go label90))
          (setf erlarg (- erlarg erlast))
          (if (> (f2cl-lib:dabs (- b1 a1)) small)
              (setf erlarg (+ erlarg erro12)))
          (if extrap (go label40))
          (if
           (>
            (f2cl-lib:dabs
             (-
              (f2cl-lib:fref blist-%data% (maxerr) ((1 limit)) blist-%offset%)
              (f2cl-lib:fref alist-%data%
                             (maxerr)
                             ((1 limit))
                             alist-%offset%)))
            small)
           (go label90))
          (setf extrap f2cl-lib:%true%)
          (setf nrmax 2)
         label40
          (if (or (= ierro 3) (<= erlarg ertest)) (go label60))
          (setf id nrmax)
          (setf jupbnd last$)
          (if (> last$ (+ 2 (the f2cl-lib:integer4 (truncate limit 2))))
              (setf jupbnd
                      (f2cl-lib:int-sub (f2cl-lib:int-add limit 3) last$)))
          (f2cl-lib:fdo (k id (f2cl-lib:int-add k 1))
                        ((> k jupbnd) nil)
            (tagbody
              (setf maxerr
                      (f2cl-lib:fref iord-%data%
                                     (nrmax)
                                     ((1 limit))
                                     iord-%offset%))
              (setf errmax
                      (f2cl-lib:fref elist-%data%
                                     (maxerr)
                                     ((1 limit))
                                     elist-%offset%))
              (if
               (>
                (f2cl-lib:dabs
                 (-
                  (f2cl-lib:fref blist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 blist-%offset%)
                  (f2cl-lib:fref alist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 alist-%offset%)))
                small)
               (go label90))
              (setf nrmax (f2cl-lib:int-add nrmax 1))
             label50))
         label60
          (setf numrl2 (f2cl-lib:int-add numrl2 1))
          (f2cl-lib:fset (f2cl-lib:fref rlist2 (numrl2) ((1 52))) area)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5)
              (dqelg numrl2 rlist2 reseps abseps res3la nres)
            (declare (ignore var-1 var-4))
            (setf numrl2 var-0)
            (setf reseps var-2)
            (setf abseps var-3)
            (setf nres var-5))
          (setf ktmin (f2cl-lib:int-add ktmin 1))
          (if (and (> ktmin 5) (< abserr (* 0.001d0 errsum))) (setf ier 5))
          (if (>= abseps abserr) (go label70))
          (setf ktmin 0)
          (setf abserr abseps)
          (setf result reseps)
          (setf correc erlarg)
          (setf ertest
                  (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs reseps))))
          (if (<= abserr ertest) (go label100))
         label70
          (if (= numrl2 1) (setf noext f2cl-lib:%true%))
          (if (= ier 5) (go label100))
          (setf maxerr
                  (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%))
          (setf errmax
                  (f2cl-lib:fref elist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 elist-%offset%))
          (setf nrmax 1)
          (setf extrap f2cl-lib:%false%)
          (setf small (* small 0.5d0))
          (setf erlarg errsum)
          (go label90)
         label80
          (setf small 0.375d0)
          (setf erlarg errsum)
          (setf ertest errbnd)
          (f2cl-lib:fset (f2cl-lib:fref rlist2 (2) ((1 52))) area)
         label90))
     label100
      (if (= abserr oflow) (go label115))
      (if (= (f2cl-lib:int-add ier ierro) 0) (go label110))
      (if (= ierro 3) (setf abserr (+ abserr correc)))
      (if (= ier 0) (setf ier 3))
      (if (and (/= result 0.0d0) (/= area 0.0d0)) (go label105))
      (if (> abserr errsum) (go label115))
      (if (= area 0.0d0) (go label130))
      (go label110)
     label105
      (if (> (/ abserr (f2cl-lib:dabs result)) (/ errsum (f2cl-lib:dabs area)))
          (go label115))
     label110
      (if
       (and (= ksgn -1)
            (<= (f2cl-lib:dmax1 (f2cl-lib:dabs result) (f2cl-lib:dabs area))
                (* defabs 0.010000000000000002d0)))
       (go label130))
      (if
       (or (> 0.010000000000000002d0 (/ result area))
           (> (/ result area) 100.0d0)
           (> errsum (f2cl-lib:dabs area)))
       (setf ier 6))
      (go label130)
     label115
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
         label120))
      (setf abserr errsum)
     label130
      (setf neval (f2cl-lib:int-sub (f2cl-lib:int-mul 30 last$) 15))
      (if (= inf 2) (setf neval (f2cl-lib:int-mul 2 neval)))
      (if (> ier 2) (setf ier (f2cl-lib:int-sub ier 1)))
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

