;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(let ((wg (make-array 5 :element-type 'double-float))
      (xgk (make-array 11 :element-type 'double-float))
      (wgk (make-array 11 :element-type 'double-float)))
  (declare (type (array double-float (11)) wgk xgk)
   (type (array double-float (5)) wg))
  (f2cl-lib:fset (f2cl-lib:fref wg (1) ((1 5))) 0.06667134430868814d0)
  (f2cl-lib:fset (f2cl-lib:fref wg (2) ((1 5))) 0.1494513491505806d0)
  (f2cl-lib:fset (f2cl-lib:fref wg (3) ((1 5))) 0.21908636251598204d0)
  (f2cl-lib:fset (f2cl-lib:fref wg (4) ((1 5))) 0.26926671930999635d0)
  (f2cl-lib:fset (f2cl-lib:fref wg (5) ((1 5))) 0.29552422471475287d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (1) ((1 11))) 0.9956571630258081d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (2) ((1 11))) 0.9739065285171717d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (3) ((1 11))) 0.9301574913557082d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (4) ((1 11))) 0.8650633666889845d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (5) ((1 11))) 0.7808177265864169d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (6) ((1 11))) 0.6794095682990244d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (7) ((1 11))) 0.5627571346686047d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (8) ((1 11))) 0.4333953941292472d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (9) ((1 11))) 0.2943928627014602d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (10) ((1 11))) 0.14887433898163122d0)
  (f2cl-lib:fset (f2cl-lib:fref xgk (11) ((1 11))) 0.0d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (1) ((1 11))) 0.011694638867371874d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (2) ((1 11))) 0.032558162307964725d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (3) ((1 11))) 0.054755896574351995d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (4) ((1 11))) 0.07503967481091996d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (5) ((1 11))) 0.0931254545836976d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (6) ((1 11))) 0.10938715880229764d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (7) ((1 11))) 0.12349197626206584d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (8) ((1 11))) 0.13470921731147334d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (9) ((1 11))) 0.14277593857706009d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (10) ((1 11))) 0.14773910490133849d0)
  (f2cl-lib:fset (f2cl-lib:fref wgk (11) ((1 11))) 0.1494455540029169d0)
  (defun dqk21 (f a b result abserr resabs resasc)
    (declare (type double-float resasc resabs abserr result b a)
     (type (function (double-float) (values double-float &rest t)) f))
    (f2cl-lib:with-multi-array-data
        nil
      (prog ((fv1 (make-array 10 :element-type 'double-float))
             (fv2 (make-array 10 :element-type 'double-float)) (j 0) (jtw 0)
             (jtwm1 0) (absc 0.0d0) (centr 0.0d0) (dhlgth 0.0d0) (epmach 0.0d0)
             (fc 0.0d0) (fsum 0.0d0) (fval1 0.0d0) (fval2 0.0d0) (hlgth 0.0d0)
             (resg 0.0d0) (resk 0.0d0) (reskh 0.0d0) (uflow 0.0d0))
        (declare (type (array double-float (10)) fv2 fv1)
         (type double-float uflow reskh resk resg hlgth fval2 fval1 fsum fc
          epmach dhlgth centr absc)
         (type f2cl-lib:integer4 jtwm1 jtw j))
        (setf epmach (f2cl-lib:d1mach 4))
        (setf uflow (f2cl-lib:d1mach 1))
        (setf centr (* 0.5d0 (+ a b)))
        (setf hlgth (* 0.5d0 (- b a)))
        (setf dhlgth (f2cl-lib:dabs hlgth))
        (setf resg 0.0d0)
        (setf fc
                (multiple-value-bind
                    (ret-val var-0)
                    (funcall f centr)
                  (declare (ignore))
                  (when var-0 (setf centr var-0))
                  ret-val))
        (setf resk (* (f2cl-lib:fref wgk (11) ((1 11))) fc))
        (setf resabs (f2cl-lib:dabs resk))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 5) nil)
          (tagbody
            (setf jtw (f2cl-lib:int-mul 2 j))
            (setf absc (* hlgth (f2cl-lib:fref xgk (jtw) ((1 11)))))
            (setf fval1 (funcall f (- centr absc)))
            (setf fval2 (funcall f (+ centr absc)))
            (f2cl-lib:fset (f2cl-lib:fref fv1 (jtw) ((1 10))) fval1)
            (f2cl-lib:fset (f2cl-lib:fref fv2 (jtw) ((1 10))) fval2)
            (setf fsum (+ fval1 fval2))
            (setf resg (+ resg (* (f2cl-lib:fref wg (j) ((1 5))) fsum)))
            (setf resk (+ resk (* (f2cl-lib:fref wgk (jtw) ((1 11))) fsum)))
            (setf resabs
                    (+ resabs
                       (* (f2cl-lib:fref wgk (jtw) ((1 11)))
                          (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
           label10))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 5) nil)
          (tagbody
            (setf jtwm1 (f2cl-lib:int-sub (f2cl-lib:int-mul 2 j) 1))
            (setf absc (* hlgth (f2cl-lib:fref xgk (jtwm1) ((1 11)))))
            (setf fval1 (funcall f (- centr absc)))
            (setf fval2 (funcall f (+ centr absc)))
            (f2cl-lib:fset (f2cl-lib:fref fv1 (jtwm1) ((1 10))) fval1)
            (f2cl-lib:fset (f2cl-lib:fref fv2 (jtwm1) ((1 10))) fval2)
            (setf fsum (+ fval1 fval2))
            (setf resk (+ resk (* (f2cl-lib:fref wgk (jtwm1) ((1 11))) fsum)))
            (setf resabs
                    (+ resabs
                       (* (f2cl-lib:fref wgk (jtwm1) ((1 11)))
                          (+ (f2cl-lib:dabs fval1) (f2cl-lib:dabs fval2)))))
           label15))
        (setf reskh (* resk 0.5d0))
        (setf resasc
                (* (f2cl-lib:fref wgk (11) ((1 11)))
                   (f2cl-lib:dabs (- fc reskh))))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j 10) nil)
          (tagbody
            (setf resasc
                    (+ resasc
                       (* (f2cl-lib:fref wgk (j) ((1 11)))
                          (+
                           (f2cl-lib:dabs
                            (- (f2cl-lib:fref fv1 (j) ((1 10))) reskh))
                           (f2cl-lib:dabs
                            (- (f2cl-lib:fref fv2 (j) ((1 10))) reskh))))))
           label20))
        (setf result (* resk hlgth))
        (setf resabs (* resabs dhlgth))
        (setf resasc (* resasc dhlgth))
        (setf abserr (f2cl-lib:dabs (* (- resk resg) hlgth)))
        (if (and (/= resasc 0.0d0) (/= abserr 0.0d0))
            (setf abserr
                    (* resasc
                       (f2cl-lib:dmin1 1.0d0
                                       (expt (/ (* 200.0d0 abserr) resasc)
                                             1.5d0)))))
        (if (> resabs (/ uflow (* 50.0d0 epmach)))
            (setf abserr (f2cl-lib:dmax1 (* epmach 50.0d0 resabs) abserr)))
        (go end_label)
       end_label
        (return (values nil nil nil result abserr resabs resasc))))))

