;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun dqagpe
       (f a b npts2 points epsabs epsrel limit result abserr neval ier alist
        blist rlist elist pts iord level ndin last$)
  (declare (type (array f2cl-lib:integer4 (*)) ndin level iord)
   (type (array double-float (*)) pts elist rlist blist alist points)
   (type f2cl-lib:integer4 last$ ier neval limit npts2)
   (type double-float abserr result epsrel epsabs b a))
  (f2cl-lib:with-multi-array-data
      ((points double-float points-%data% points-%offset%)
       (alist double-float alist-%data% alist-%offset%)
       (blist double-float blist-%data% blist-%offset%)
       (rlist double-float rlist-%data% rlist-%offset%)
       (elist double-float elist-%data% elist-%offset%)
       (pts double-float pts-%data% pts-%offset%)
       (iord f2cl-lib:integer4 iord-%data% iord-%offset%)
       (level f2cl-lib:integer4 level-%data% level-%offset%)
       (ndin f2cl-lib:integer4 ndin-%data% ndin-%offset%))
    (prog ((res3la (make-array 3 :element-type 'double-float))
           (rlist2 (make-array 52 :element-type 'double-float)) (extrap nil)
           (noext nil) (i 0) (id 0) (ierro 0) (ind1 0) (ind2 0) (ip1 0)
           (iroff1 0) (iroff2 0) (iroff3 0) (j 0) (jlow 0) (jupbnd 0) (k 0)
           (ksgn 0) (ktmin 0) (levcur 0) (levmax 0) (maxerr 0)
           (f2cl-lib:nint 0) (nintp1 0) (npts 0) (nres 0) (nrmax 0) (numrl2 0)
           (abseps 0.0d0) (area 0.0d0) (area1 0.0d0) (area12 0.0d0)
           (area2 0.0d0) (a1 0.0d0) (a2 0.0d0) (b1 0.0d0) (b2 0.0d0)
           (correc 0.0d0) (defabs 0.0d0) (defab1 0.0d0) (defab2 0.0d0)
           (dres 0.0d0) (epmach 0.0d0) (erlarg 0.0d0) (erlast 0.0d0)
           (errbnd 0.0d0) (errmax 0.0d0) (error1 0.0d0) (erro12 0.0d0)
           (error2 0.0d0) (errsum 0.0d0) (ertest 0.0d0) (oflow 0.0d0)
           (resa 0.0d0) (resabs 0.0d0) (reseps 0.0d0) (f2cl-lib:sign 0.0d0)
           (temp 0.0d0) (uflow 0.0d0))
      (declare (type (array double-float (52)) rlist2)
       (type (array double-float (3)) res3la)
       (type double-float uflow temp f2cl-lib:sign reseps resabs resa oflow
        ertest errsum error2 erro12 error1 errmax errbnd erlast erlarg epmach
        dres defab2 defab1 defabs correc b2 b1 a2 a1 area2 area12 area1 area
        abseps)
       (type f2cl-lib:integer4 numrl2 nrmax nres npts nintp1 f2cl-lib:nint
        maxerr levmax levcur ktmin ksgn k jupbnd jlow j iroff3 iroff2 iroff1
        ip1 ind2 ind1 ierro id i)
       (type f2cl-lib:logical noext extrap))
      (setf epmach (f2cl-lib:d1mach 4))
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
      (f2cl-lib:fset
       (f2cl-lib:fref level-%data% (1) ((1 limit)) level-%offset%)
       0)
      (setf npts (f2cl-lib:int-sub npts2 2))
      (if
       (or (< npts2 2)
           (<= limit npts)
           (and (<= epsabs 0.0d0)
                (< epsrel (f2cl-lib:dmax1 (* 50.0d0 epmach) 5.0d-29))))
       (setf ier 6))
      (if (= ier 6) (go label999))
      (setf f2cl-lib:sign 1.0d0)
      (if (> a b) (setf f2cl-lib:sign -1.0d0))
      (f2cl-lib:fset (f2cl-lib:fref pts-%data% (1) ((1 npts2)) pts-%offset%)
                     (f2cl-lib:dmin1 a b))
      (if (= npts 0) (go label15))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i npts) nil)
        (tagbody
          (f2cl-lib:fset
           (f2cl-lib:fref pts-%data%
                          ((f2cl-lib:int-add i 1))
                          ((1 npts2))
                          pts-%offset%)
           (f2cl-lib:fref points-%data% (i) ((1 npts2)) points-%offset%))
         label10))
     label15
      (f2cl-lib:fset
       (f2cl-lib:fref pts-%data%
                      ((f2cl-lib:int-add npts 2))
                      ((1 npts2))
                      pts-%offset%)
       (f2cl-lib:dmax1 a b))
      (setf f2cl-lib:nint (f2cl-lib:int-add npts 1))
      (setf a1 (f2cl-lib:fref pts-%data% (1) ((1 npts2)) pts-%offset%))
      (if (= npts 0) (go label40))
      (setf nintp1 (f2cl-lib:int-add f2cl-lib:nint 1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i f2cl-lib:nint) nil)
        (tagbody
          (setf ip1 (f2cl-lib:int-add i 1))
          (f2cl-lib:fdo (j ip1 (f2cl-lib:int-add j 1))
                        ((> j nintp1) nil)
            (tagbody
              (if
               (<= (f2cl-lib:fref pts-%data% (i) ((1 npts2)) pts-%offset%)
                   (f2cl-lib:fref pts-%data% (j) ((1 npts2)) pts-%offset%))
               (go label20))
              (setf temp
                      (f2cl-lib:fref pts-%data% (i) ((1 npts2)) pts-%offset%))
              (f2cl-lib:fset
               (f2cl-lib:fref pts-%data% (i) ((1 npts2)) pts-%offset%)
               (f2cl-lib:fref pts-%data% (j) ((1 npts2)) pts-%offset%))
              (f2cl-lib:fset
               (f2cl-lib:fref pts-%data% (j) ((1 npts2)) pts-%offset%)
               temp)))))
     label20
      (if
       (or
        (/= (f2cl-lib:fref pts-%data% (1) ((1 npts2)) pts-%offset%)
            (f2cl-lib:dmin1 a b))
        (/= (f2cl-lib:fref pts-%data% (nintp1) ((1 npts2)) pts-%offset%)
            (f2cl-lib:dmax1 a b)))
       (setf ier 6))
      (if (= ier 6) (go label999))
     label40
      (setf resabs 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i f2cl-lib:nint) nil)
        (tagbody
          (setf b1
                  (f2cl-lib:fref pts-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 npts2))
                                 pts-%offset%))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk21 f a1 b1 area1 error1 defabs resa)
            (declare (ignore var-0 var-1 var-2))
            (setf area1 var-3)
            (setf error1 var-4)
            (setf defabs var-5)
            (setf resa var-6))
          (setf abserr (+ abserr error1))
          (setf result (+ result area1))
          (f2cl-lib:fset
           (f2cl-lib:fref ndin-%data% (i) ((1 npts2)) ndin-%offset%)
           0)
          (if (and (= error1 resa) (/= error1 0.0d0))
              (f2cl-lib:fset
               (f2cl-lib:fref ndin-%data% (i) ((1 npts2)) ndin-%offset%)
               1))
          (setf resabs (+ resabs defabs))
          (f2cl-lib:fset
           (f2cl-lib:fref level-%data% (i) ((1 limit)) level-%offset%)
           0)
          (f2cl-lib:fset
           (f2cl-lib:fref elist-%data% (i) ((1 limit)) elist-%offset%)
           error1)
          (f2cl-lib:fset
           (f2cl-lib:fref alist-%data% (i) ((1 limit)) alist-%offset%)
           a1)
          (f2cl-lib:fset
           (f2cl-lib:fref blist-%data% (i) ((1 limit)) blist-%offset%)
           b1)
          (f2cl-lib:fset
           (f2cl-lib:fref rlist-%data% (i) ((1 limit)) rlist-%offset%)
           area1)
          (f2cl-lib:fset
           (f2cl-lib:fref iord-%data% (i) ((1 limit)) iord-%offset%)
           i)
          (setf a1 b1)
         label50))
      (setf errsum 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i f2cl-lib:nint) nil)
        (tagbody
          (if (= (f2cl-lib:fref ndin-%data% (i) ((1 npts2)) ndin-%offset%) 1)
              (f2cl-lib:fset
               (f2cl-lib:fref elist-%data% (i) ((1 limit)) elist-%offset%)
               abserr))
          (setf errsum
                  (+ errsum
                     (f2cl-lib:fref elist-%data%
                                    (i)
                                    ((1 limit))
                                    elist-%offset%)))
         label55))
      (setf last$ f2cl-lib:nint)
      (setf neval (f2cl-lib:int-mul 21 f2cl-lib:nint))
      (setf dres (f2cl-lib:dabs result))
      (setf errbnd (f2cl-lib:dmax1 epsabs (* epsrel dres)))
      (if (and (<= abserr (* 100.0d0 epmach resabs)) (> abserr errbnd))
          (setf ier 2))
      (if (= f2cl-lib:nint 1) (go label80))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i npts) nil)
        (tagbody
          (setf jlow (f2cl-lib:int-add i 1))
          (setf ind1 (f2cl-lib:fref iord-%data% (i) ((1 limit)) iord-%offset%))
          (f2cl-lib:fdo (j jlow (f2cl-lib:int-add j 1))
                        ((> j f2cl-lib:nint) nil)
            (tagbody
              (setf ind2
                      (f2cl-lib:fref iord-%data%
                                     (j)
                                     ((1 limit))
                                     iord-%offset%))
              (if
               (>
                (f2cl-lib:fref elist-%data% (ind1) ((1 limit)) elist-%offset%)
                (f2cl-lib:fref elist-%data% (ind2) ((1 limit)) elist-%offset%))
               (go label60))
              (setf ind1 ind2)
              (setf k j)
             label60))
          (if
           (= ind1 (f2cl-lib:fref iord-%data% (i) ((1 limit)) iord-%offset%))
           (go label70))
          (f2cl-lib:fset
           (f2cl-lib:fref iord-%data% (k) ((1 limit)) iord-%offset%)
           (f2cl-lib:fref iord-%data% (i) ((1 limit)) iord-%offset%))
          (f2cl-lib:fset
           (f2cl-lib:fref iord-%data% (i) ((1 limit)) iord-%offset%)
           ind1)
         label70))
      (if (< limit npts2) (setf ier 1))
     label80
      (if (or (/= ier 0) (<= abserr errbnd)) (go label210))
      (f2cl-lib:fset (f2cl-lib:fref rlist2 (1) ((1 52))) result)
      (setf maxerr (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%))
      (setf errmax
              (f2cl-lib:fref elist-%data% (maxerr) ((1 limit)) elist-%offset%))
      (setf area result)
      (setf nrmax 1)
      (setf nres 0)
      (setf numrl2 1)
      (setf ktmin 0)
      (setf extrap f2cl-lib:%false%)
      (setf noext f2cl-lib:%false%)
      (setf erlarg errsum)
      (setf ertest errbnd)
      (setf levmax 1)
      (setf iroff1 0)
      (setf iroff2 0)
      (setf iroff3 0)
      (setf ierro 0)
      (setf uflow (f2cl-lib:d1mach 1))
      (setf oflow (f2cl-lib:d1mach 2))
      (setf abserr oflow)
      (setf ksgn -1)
      (if (>= dres (* (- 1.0d0 (* 50.0d0 epmach)) resabs)) (setf ksgn 1))
      (f2cl-lib:fdo (last$ npts2 (f2cl-lib:int-add last$ 1))
                    ((> last$ limit) nil)
        (tagbody
          (setf levcur
                  (f2cl-lib:int-add
                   (f2cl-lib:fref level-%data%
                                  (maxerr)
                                  ((1 limit))
                                  level-%offset%)
                   1))
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
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk21 f a1 b1 area1 error1 resa defab1)
            (declare (ignore var-0 var-1 var-2))
            (setf area1 var-3)
            (setf error1 var-4)
            (setf resa var-5)
            (setf defab1 var-6))
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqk21 f a2 b2 area2 error2 resa defab2)
            (declare (ignore var-0 var-1 var-2))
            (setf area2 var-3)
            (setf error2 var-4)
            (setf resa var-5)
            (setf defab2 var-6))
          (setf neval (f2cl-lib:int-add neval 42))
          (setf area12 (+ area1 area2))
          (setf erro12 (+ error1 error2))
          (setf errsum (- (+ errsum erro12) errmax))
          (setf area
                  (- (+ area area12)
                     (f2cl-lib:fref rlist-%data%
                                    (maxerr)
                                    ((1 limit))
                                    rlist-%offset%)))
          (if (or (= defab1 error1) (= defab2 error2)) (go label95))
          (if
           (or
            (>
             (f2cl-lib:dabs
              (-
               (f2cl-lib:fref rlist-%data% (maxerr) ((1 limit)) rlist-%offset%)
               area12))
             (* 1.0d-5 (f2cl-lib:dabs area12)))
            (< erro12 (* 0.99d0 errmax)))
           (go label90))
          (if extrap (setf iroff2 (f2cl-lib:int-add iroff2 1)))
          (if (not extrap) (setf iroff1 (f2cl-lib:int-add iroff1 1)))
         label90
          (if (and (> last$ 10) (> erro12 errmax))
              (setf iroff3 (f2cl-lib:int-add iroff3 1)))
         label95
          (f2cl-lib:fset
           (f2cl-lib:fref level-%data% (maxerr) ((1 limit)) level-%offset%)
           levcur)
          (f2cl-lib:fset
           (f2cl-lib:fref level-%data% (last$) ((1 limit)) level-%offset%)
           levcur)
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
          (if (> error2 error1) (go label100))
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
          (go label110)
         label100
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
         label110
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dqpsrt limit last$ maxerr errmax elist iord nrmax)
            (declare (ignore var-0 var-1 var-4 var-5))
            (setf maxerr var-2)
            (setf errmax var-3)
            (setf nrmax var-6))
          (if (<= errsum errbnd) (go label190))
          (if (/= ier 0) (go label170))
          (if noext (go label160))
          (setf erlarg (- erlarg erlast))
          (if (<= (f2cl-lib:int-add levcur 1) levmax)
              (setf erlarg (+ erlarg erro12)))
          (if extrap (go label120))
          (if
           (<=
            (f2cl-lib:int-add
             (f2cl-lib:fref level-%data% (maxerr) ((1 limit)) level-%offset%)
             1)
            levmax)
           (go label160))
          (setf extrap f2cl-lib:%true%)
          (setf nrmax 2)
         label120
          (if (or (= ierro 3) (<= erlarg ertest)) (go label140))
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
               (<=
                (f2cl-lib:int-add
                 (f2cl-lib:fref level-%data%
                                (maxerr)
                                ((1 limit))
                                level-%offset%)
                 1)
                levmax)
               (go label160))
              (setf nrmax (f2cl-lib:int-add nrmax 1))
             label130))
         label140
          (setf numrl2 (f2cl-lib:int-add numrl2 1))
          (f2cl-lib:fset (f2cl-lib:fref rlist2 (numrl2) ((1 52))) area)
          (if (<= numrl2 2) (go label155))
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
          (if (>= abseps abserr) (go label150))
          (setf ktmin 0)
          (setf abserr abseps)
          (setf result reseps)
          (setf correc erlarg)
          (setf ertest
                  (f2cl-lib:dmax1 epsabs (* epsrel (f2cl-lib:dabs reseps))))
          (if (< abserr ertest) (go label170))
         label150
          (if (= numrl2 1) (setf noext f2cl-lib:%true%))
          (if (>= ier 5) (go label170))
         label155
          (setf maxerr
                  (f2cl-lib:fref iord-%data% (1) ((1 limit)) iord-%offset%))
          (setf errmax
                  (f2cl-lib:fref elist-%data%
                                 (maxerr)
                                 ((1 limit))
                                 elist-%offset%))
          (setf nrmax 1)
          (setf extrap f2cl-lib:%false%)
          (setf levmax (f2cl-lib:int-add levmax 1))
          (setf erlarg errsum)
         label160))
     label170
      (if (= abserr oflow) (go label190))
      (if (= (f2cl-lib:int-add ier ierro) 0) (go label180))
      (if (= ierro 3) (setf abserr (+ abserr correc)))
      (if (= ier 0) (setf ier 3))
      (if (and (/= result 0.0d0) (/= area 0.0d0)) (go label175))
      (if (> abserr errsum) (go label190))
      (if (= area 0.0d0) (go label210))
      (go label180)
     label175
      (if (> (/ abserr (f2cl-lib:dabs result)) (/ errsum (f2cl-lib:dabs area)))
          (go label190))
     label180
      (if
       (and (= ksgn -1)
            (<= (f2cl-lib:dmax1 (f2cl-lib:dabs result) (f2cl-lib:dabs area))
                (* resabs 0.010000000000000002d0)))
       (go label210))
      (if
       (or (> 0.010000000000000002d0 (/ result area))
           (> (/ result area) 100.0d0)
           (> errsum (f2cl-lib:dabs area)))
       (setf ier 6))
      (go label210)
     label190
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
         label200))
      (setf abserr errsum)
     label210
      (if (> ier 2) (setf ier (f2cl-lib:int-sub ier 1)))
      (setf result (* result f2cl-lib:sign))
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
               nil
               nil
               nil
               last$)))))

