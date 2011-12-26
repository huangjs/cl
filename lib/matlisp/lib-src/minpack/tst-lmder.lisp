;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "MINPACK")


(defstruct (refnum (:predicate is-refnum-p))
  (nprob 0 :type f2cl-lib:integer4)
  (nfev 0 :type f2cl-lib:integer4)
  (njev 0 :type f2cl-lib:integer4))


(let* ()
  (defparameter *refnum-common-block* (make-refnum)))



(let ((nread 5) (nwrite 6) (one 1.0d0) (ten 10.0d0))
  (declare (type double-float ten one) (type f2cl-lib:integer4 nwrite nread))
  (defun tlmder ()
    (let ()
      (symbol-macrolet ((njev (refnum-njev *refnum-common-block*))
                        (nfev (refnum-nfev *refnum-common-block*))
                        (nprob (refnum-nprob *refnum-common-block*)))
        (prog ((ntries 0) (n 0) (m 0) (lwa 0) (ldfjac 0) (k 0) (info 0) (ic 0)
               (i 0) (nx (make-array 60 :element-type 'f2cl-lib:integer4))
               (np (make-array 60 :element-type 'f2cl-lib:integer4))
               (nj (make-array 60 :element-type 'f2cl-lib:integer4))
               (nf (make-array 60 :element-type 'f2cl-lib:integer4))
               (na (make-array 60 :element-type 'f2cl-lib:integer4))
               (ma (make-array 60 :element-type 'f2cl-lib:integer4))
               (iwa (make-array 40 :element-type 'f2cl-lib:integer4))
               (tol 0.0d0) (fnorm2 0.0d0) (fnorm1 0.0d0) (factor 0.0d0)
               (x (make-array 40 :element-type 'double-float))
               (wa (make-array 265 :element-type 'double-float))
               (fvec (make-array 65 :element-type 'double-float))
               (fnm (make-array 60 :element-type 'double-float))
               (fjac (make-array 2600 :element-type 'double-float)))
          (declare (type (array double-float (2600)) fjac)
                   (type (array double-float (60)) fnm)
                   (type (array double-float (65)) fvec)
                   (type (array double-float (265)) wa)
                   (type (array double-float (40)) x)
                   (type double-float factor fnorm1 fnorm2 tol)
                   (type (array f2cl-lib:integer4 (40)) iwa)
                   (type (array f2cl-lib:integer4 (60)) ma na nf nj np nx)
                   (type f2cl-lib:integer4 i ic info k ldfjac lwa m n ntries))
          '"     **********"
          '""
          '"     this program tests codes for the least-squares solution of"
          '"     m nonlinear equations in n variables. it consists of a driver"
          '"     and an interface subroutine fcn. the driver reads in data,"
          '"     calls the nonlinear least-squares solver, and finally prints"
          '"     out information on the performance of the solver. this is"
          '"     only a sample driver, many other drivers are possible. the"
          '"     interface subroutine fcn is necessary to take into account the"
          '"     forms of calling sequences used by the function and jacobian"
          '"     subroutines in the various nonlinear least-squares solvers."
          '""
          '"     subprograms called"
          '""
          '"       user-supplied ...... fcn"
          '""
          '"       minpack-supplied ... dpmpar,enorm,initpt,lmder1,ssqfcn"
          '""
          '"       fortran-supplied ... dsqrt"
          '""
          '"     argonne national laboratory. minpack project. march 1980."
          '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
          '""
          '"     **********"
          '""
          '"     logical input unit is assumed to be number 5."
          '"     logical output unit is assumed to be number 6."
          '""
          '""
          (setf tol (f2cl-lib:dsqrt (dpmpar 1)))
          (setf ldfjac 65)
          (setf lwa 265)
          (setf ic 0)
         label10
          '""
          '"     describe the possible functions"
          '""
          (f2cl-lib:fformat t
                            (("~A~%"))
                            "Enter prob no, dimensions n, m, and the number of tries")
          (f2cl-lib:fformat t
                            (("~A~%"))
                            " 1: linear function, full rank. m >= n")
          (f2cl-lib:fformat t (("~A~%")) " 2: linear function, rank 1. m >= n")
          (f2cl-lib:fformat t
                            (("~A~%"))
                            " 3: linear function, rank 1, zero cols, rows. m >= n")
          (f2cl-lib:fformat t (("~A~%")) " 4: Rosenbrock, m=2, n=2")
          (f2cl-lib:fformat t (("~A~%")) " 5: Helical valley, m=3, n=3")
          (f2cl-lib:fformat t (("~A~%")) " 6: Powell singular, m=4, n=4")
          (f2cl-lib:fformat t (("~A~%")) " 7: Freudenstein and Roth, m=2, n=2")
          (f2cl-lib:fformat t (("~A~%")) " 8: Bard, m=15, n=3")
          (f2cl-lib:fformat t (("~A~%")) " 9: Kowalik and Osborne, m=11, n=4")
          (f2cl-lib:fformat t (("~A~%")) "10: Meyer, m=16, n=3")
          (f2cl-lib:fformat t
                            (("~A~%"))
                            "11: Watson, m=31, n=2-31 (6 or 9 typical)")
          (f2cl-lib:fformat t
                            (("~A~%"))
                            "12: Box 3-D, m>=n, n=3. (m=10 typical)")
          (f2cl-lib:fformat t
                            (("~A~%"))
                            "13: Jennrich and Sampson, m>=n, n=2. (m=10 typical)")
          (f2cl-lib:fformat t
                            (("~A~%"))
                            "14: Brown and Dennis, m>=n, n=4. (m=20 typical)")
          (f2cl-lib:fformat t (("~A~%")) "15: Chebyquad, m>=n")
          (f2cl-lib:fformat t (("~A~%")) "16: Brown almost-linear, m=n")
          (f2cl-lib:fformat t (("~A~%")) "17: Osborne 1 function, m=33, n=5")
          (f2cl-lib:fformat t (("~A~%")) "18: Osborne 2 function, m=65, n=11")
          (f2cl-lib:fformat t (("~A~%")) "-1: exit")
          (f2cl-lib:fortran_comment
           "***WARNING:  READ statement may not be translated correctly!")
          (setf nprob (read))
          (setf n (read))
          (setf m (read))
          (setf ntries (read))
          (f2cl-lib:fortran_comment
           "***WARNING: Preceding READ statements may not be correct!")
          (if (<= nprob 0) (go label30))
          (setf factor one)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k ntries) nil)
            (tagbody
              (setf ic (f2cl-lib:int-add ic 1))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3)
                  (initpt n x nprob factor)
                (declare (ignore var-1))
                (setf n var-0)
                (setf nprob var-2)
                (setf factor var-3))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4)
                  (ssqfcn m n x fvec nprob)
                (declare (ignore var-2 var-3))
                (setf m var-0)
                (setf n var-1)
                (setf nprob var-4))
              (setf fnorm1 (enorm m fvec))
              (f2cl-lib:fformat nwrite
                                ("~%" "~%" "~%" "~%" "~5@T" " PROBLEM" 1
                                 (("~5D")) "~5@T" " DIMENSIONS" 2 (("~5D"))
                                 "~5@T" "~%" "~%" "~%")
                                nprob
                                n
                                m)
              (setf nfev 0)
              (setf njev 0)
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                   var-10 var-11)
                  (lmder1 #'fcnj m n x fvec fjac ldfjac tol info iwa wa lwa)
                (declare
                 (ignore var-0 var-3 var-4 var-5 var-7 var-9 var-10 var-11))
                (setf m var-1)
                (setf n var-2)
                (setf ldfjac var-6)
                (setf info var-8))
              (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4)
                  (ssqfcn m n x fvec nprob)
                (declare (ignore var-2 var-3))
                (setf m var-0)
                (setf n var-1)
                (setf nprob var-4))
              (setf fnorm2 (enorm m fvec))
              (f2cl-lib:fset (f2cl-lib:fref np (ic) ((1 60))) nprob)
              (f2cl-lib:fset (f2cl-lib:fref na (ic) ((1 60))) n)
              (f2cl-lib:fset (f2cl-lib:fref ma (ic) ((1 60))) m)
              (f2cl-lib:fset (f2cl-lib:fref nf (ic) ((1 60))) nfev)
              (f2cl-lib:fset (f2cl-lib:fref nj (ic) ((1 60))) njev)
              (f2cl-lib:fset (f2cl-lib:fref nx (ic) ((1 60))) info)
              (f2cl-lib:fset (f2cl-lib:fref fnm (ic) ((1 60))) fnorm2)
              (f2cl-lib:fformat nwrite
                                ("~5@T" " INITIAL L2 NORM OF THE RESIDUALS" 1
                                 (("~15,7,2,0,'*,,'DE")) "~%" "~%" "~5@T"
                                 " FINAL L2 NORM OF THE RESIDUALS  " 1
                                 (("~15,7,2,0,'*,,'DE")) "~%" "~%" "~5@T"
                                 " NUMBER OF FUNCTION EVALUATIONS  " 1
                                 (("~10D")) "~%" "~%" "~5@T"
                                 " NUMBER OF JACOBIAN EVALUATIONS  " 1
                                 (("~10D")) "~%" "~%" "~5@T" " EXIT PARAMETER"
                                 "~18@T" 1 (("~10D")) "~%" "~%" "~5@T"
                                 " FINAL APPROXIMATE SOLUTION" "~%" "~%" t
                                 ("~5@T" 5 (("~15,7,2,0,'*,,'DE"))) "~%")
                                fnorm1
                                fnorm2
                                nfev
                                njev
                                info
                                (do ((i 1 (f2cl-lib:int-add i 1))
                                     (ret nil
                                          (append ret
                                                  (list
                                                   (f2cl-lib:fref x
                                                                  (i)
                                                                  ((1 40)))))))
                                    ((> i n) ret)
                                  (declare (type f2cl-lib:integer4 i))))
              (setf factor (* ten factor))
             label20))
          (go label10)
         label30
          (f2cl-lib:fformat nwrite
                            ("1SUMMARY OF " 1 (("~3D")) " CALLS TO LMDER1" "~%"
                             "~%")
                            ic)
          (f2cl-lib:fformat nwrite
                            (" NPROB   N    M   NFEV  NJEV  INFO  FINAL L2 NORM"
                             "~%" "~%")
                            nil)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ic) nil)
            (tagbody
              (f2cl-lib:fformat nwrite
                                (3 (("~5D")) 3 (("~6D")) "~1@T" 1
                                 (("~15,7,2,0,'*,,'DE")) "~%")
                                (f2cl-lib:fref np (i) ((1 60)))
                                (f2cl-lib:fref na (i) ((1 60)))
                                (f2cl-lib:fref ma (i) ((1 60)))
                                (f2cl-lib:fref nf (i) ((1 60)))
                                (f2cl-lib:fref nj (i) ((1 60)))
                                (f2cl-lib:fref nx (i) ((1 60)))
                                (f2cl-lib:fref fnm (i) ((1 60))))
             label40))
          '""
          '"     last card of driver."
          '""
         end_label
          (return nil))))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "MINPACK")



(defun fcnj (m n x fvec fjac ldfjac iflag)
  (declare (type (array double-float (*)) fjac fvec x)
           (type f2cl-lib:integer4 iflag ldfjac n m))
  (let ()
    (symbol-macrolet ((njev (refnum-njev *refnum-common-block*))
                      (nfev (refnum-nfev *refnum-common-block*))
                      (nprob (refnum-nprob *refnum-common-block*)))
      (f2cl-lib:with-array-data (x-%data% x-%offset% x)
        (declare (type f2cl-lib:integer4 x-%offset%)
                 (type (simple-array double-float (*)) x-%data%)
                 (ignorable x-%offset% x-%data%))
        (f2cl-lib:with-array-data (fvec-%data% fvec-%offset% fvec)
          (declare (type f2cl-lib:integer4 fvec-%offset%)
                   (type (simple-array double-float (*)) fvec-%data%)
                   (ignorable fvec-%offset% fvec-%data%))
          (f2cl-lib:with-array-data (fjac-%data% fjac-%offset% fjac)
            (declare (type f2cl-lib:integer4 fjac-%offset%)
                     (type (simple-array double-float (*)) fjac-%data%)
                     (ignorable fjac-%offset% fjac-%data%))
            (prog ()
              (declare)
              '"     **********"
              '""
              '"     the calling sequence of fcn should be identical to the"
              '"     calling sequence of the function subroutine in the nonlinear"
              '"     least-squares solver. fcn should only call the testing"
              '"     function and jacobian subroutines ssqfcn and ssqjac with"
              '"     the appropriate value of problem number (nprob)."
              '""
              '"     subprograms called"
              '""
              '"       minpack-supplied ... ssqfcn,ssqjac"
              '""
              '"     argonne national laboratory. minpack project. march 1980."
              '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
              '""
              '"     **********"
              (if (= iflag 1)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4)
                      (ssqfcn m n x fvec nprob)
                    (declare (ignore var-2 var-3))
                    (setf m var-0)
                    (setf n var-1)
                    (setf nprob var-4)))
              (if (= iflag 2)
                  (multiple-value-bind
                      (var-0 var-1 var-2 var-3 var-4 var-5)
                      (ssqjac m n x fjac ldfjac nprob)
                    (declare (ignore var-2 var-3))
                    (when var-0 (setf m var-0))
                    (when var-1 (setf n var-1))
                    (when var-4 (setf ldfjac var-4))
                    (when var-5 (setf nprob var-5))))
              (if (= iflag 1) (setf nfev (f2cl-lib:int-add nfev 1)))
              (if (= iflag 2) (setf njev (f2cl-lib:int-add njev 1)))
              (go end_label)
              '""
              '"     last card of interface subroutine fcn."
              '""
             end_label
              (return (values m n nil nil nil ldfjac nil)))))))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "MINPACK")


(let ((zero 0.0d0)
      (one 1.0d0)
      (two 2.0d0)
      (three 3.0d0)
      (four 4.0d0)
      (five 5.0d0)
      (eight 8.0d0)
      (ten 10.0d0)
      (c14 14.0d0)
      (c20 20.0d0)
      (c29 29.0d0)
      (c45 45.0d0)
      (c100 100.0d0)
      (v (make-array 11 :element-type 'double-float)))
  (declare (type (array double-float (11)) v)
           (type double-float c100 c45 c29 c20 c14 ten eight five four three
            two one zero))
  (f2cl-lib:fset (f2cl-lib:fref v (1) ((1 11))) 4.0d0)
  (f2cl-lib:fset (f2cl-lib:fref v (2) ((1 11))) 2.0d0)
  (f2cl-lib:fset (f2cl-lib:fref v (3) ((1 11))) 1.0d0)
  (f2cl-lib:fset (f2cl-lib:fref v (4) ((1 11))) 0.5d0)
  (f2cl-lib:fset (f2cl-lib:fref v (5) ((1 11))) 0.25d0)
  (f2cl-lib:fset (f2cl-lib:fref v (6) ((1 11))) 0.167d0)
  (f2cl-lib:fset (f2cl-lib:fref v (7) ((1 11))) 0.125d0)
  (f2cl-lib:fset (f2cl-lib:fref v (8) ((1 11))) 0.1d0)
  (f2cl-lib:fset (f2cl-lib:fref v (9) ((1 11))) 0.0833d0)
  (f2cl-lib:fset (f2cl-lib:fref v (10) ((1 11))) 0.0714d0)
  (f2cl-lib:fset (f2cl-lib:fref v (11) ((1 11))) 0.0625d0)
  (defun ssqjac (m n x fjac ldfjac nprob)
    (declare (type (array double-float (*)) fjac x)
             (type f2cl-lib:integer4 nprob ldfjac n m))
    (f2cl-lib:with-array-data (x-%data% x-%offset% x)
      (declare (type f2cl-lib:integer4 x-%offset%)
               (type (simple-array double-float (*)) x-%data%)
               (ignorable x-%offset% x-%data%))
      (f2cl-lib:with-array-data (fjac-%data% fjac-%offset% fjac)
        (declare (type f2cl-lib:integer4 fjac-%offset%)
                 (type (simple-array double-float (*)) fjac-%data%)
                 (ignorable fjac-%offset% fjac-%data%))
        (labels ((dfloat (ivar)
                   (coerce ivar 'double-float)))
          (declare
           (ftype (function (f2cl-lib:integer4) (values double-float &rest t))
            dfloat))
          (prog ((div 0.0d0) (dx 0.0d0) (prod 0.0d0) (s2 0.0d0) (temp 0.0d0)
                 (ti 0.0d0) (tmp1 0.0d0) (tmp2 0.0d0) (tmp3 0.0d0) (tmp4 0.0d0)
                 (tpi 0.0d0) (i 0) (ivar 0) (j 0) (k 0) (mm1 0) (nm1 0))
            (declare (type f2cl-lib:integer4 nm1 mm1 k j ivar i)
                     (type double-float tpi tmp4 tmp3 tmp2 tmp1 ti temp s2 prod
                      dx div))
            '"     **********"
            '""
            '"     subroutine ssqjac"
            '""
            '"     this subroutine defines the jacobian matrices of eighteen"
            '"     nonlinear least squares problems. the problem dimensions are"
            '"     as described in the prologue comments of ssqfcn."
            '""
            '"     the subroutine statement is"
            '""
            '"       subroutine ssqjac(m,n,x,fjac,ldfjac,nprob)"
            '""
            '"     where"
            '""
            '"       m and n are positive integer input variables. n must not"
            '"         exceed m."
            '""
            '"       x is an input array of length n."
            '""
            '"       fjac is an m by n output array which contains the jacobian"
            '"         matrix of the nprob function evaluated at x."
            '""
            '"       ldfjac is a positive integer input variable not less than m"
            '"         which specifies the leading dimension of the array fjac."
            '""
            '"       nprob is a positive integer variable which defines the"
            '"         number of the problem. nprob must not exceed 18."
            '""
            '"     subprograms called"
            '""
            '"       fortran-supplied ... datan,dcos,dexp,dsin,dsqrt"
            '""
            '"     argonne national laboratory. minpack project. march 1980."
            '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
            '""
            '"     **********"
            '""
            '"     jacobian routine selector."
            '""
            (f2cl-lib:computed-goto
             (label10 label40 label70 label130 label140 label150 label180
              label190 label210 label230 label250 label310 label330 label350
              label370 label400 label460 label480)
             nprob)
            '""
            '"     linear function - full rank."
            '""
           label10
            (setf temp (/ two (dfloat m)))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     (- temp))
                   label20))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (j j)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (+
                  (f2cl-lib:fref fjac-%data%
                                 (j j)
                                 ((1 ldfjac) (1 n))
                                 fjac-%offset%)
                  one))
               label30))
            (go label500)
            '""
            '"     linear function - rank 1."
            '""
           label40
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     (* (dfloat i) (dfloat j)))
                   label50))
               label60))
            (go label500)
            '""
            '"     linear function - rank 1 with zero columns and rows."
            '""
           label70
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     zero)
                   label80))
               label90))
            (setf nm1 (f2cl-lib:int-sub n 1))
            (setf mm1 (f2cl-lib:int-sub m 1))
            (if (< nm1 2) (go label120))
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j nm1) nil)
              (tagbody
                (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                              ((> i mm1) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     (* (dfloat (f2cl-lib:int-sub i 1)) (dfloat j)))
                   label100))
               label110))
           label120
            (go label500)
            '""
            '"     rosenbrock function."
            '""
           label130
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             (* (- c20) (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             ten)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             (- one))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             zero)
            (go label500)
            '""
            '"     helical valley function."
            '""
           label140
            (setf tpi (* eight (f2cl-lib:datan one)))
            (setf temp
                    (+ (expt (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) 2)
                       (expt (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                             2)))
            (setf tmp1 (* tpi temp))
            (setf tmp2 (f2cl-lib:dsqrt temp))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             (/ (* c100 (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)) tmp1))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             (/ (* (- c100) (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%))
                tmp1))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 3) ((1 ldfjac) (1 n)) fjac-%offset%)
             ten)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             (/ (* ten (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)) tmp2))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             (/ (* ten (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)) tmp2))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 3) ((1 ldfjac) (1 n)) fjac-%offset%)
             zero)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (3 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             zero)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (3 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             zero)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (3 3) ((1 ldfjac) (1 n)) fjac-%offset%)
             one)
            (go label500)
            '""
            '"     powell singular function."
            '""
           label150
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j 4) nil)
              (tagbody
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i 4) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     zero)
                   label160))
               label170))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             one)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             ten)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 3) ((1 ldfjac) (1 n)) fjac-%offset%)
             (f2cl-lib:dsqrt five))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 4) ((1 ldfjac) (1 n)) fjac-%offset%)
             (-
              (f2cl-lib:fref fjac-%data%
                             (2 3)
                             ((1 ldfjac) (1 n))
                             fjac-%offset%)))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (3 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             (* two
                (- (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                   (* two (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)))))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (3 3) ((1 ldfjac) (1 n)) fjac-%offset%)
             (* (- two)
                (f2cl-lib:fref fjac-%data%
                               (3 2)
                               ((1 ldfjac) (1 n))
                               fjac-%offset%)))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (4 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             (* two
                (f2cl-lib:dsqrt ten)
                (- (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                   (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%))))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (4 4) ((1 ldfjac) (1 n)) fjac-%offset%)
             (-
              (f2cl-lib:fref fjac-%data%
                             (4 1)
                             ((1 ldfjac) (1 n))
                             fjac-%offset%)))
            (go label500)
            '""
            '"     freudenstein and roth function."
            '""
           label180
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             one)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (1 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             (-
              (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                 (- ten
                    (* three (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))))
              two))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 1) ((1 ldfjac) (1 n)) fjac-%offset%)
             one)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data% (2 2) ((1 ldfjac) (1 n)) fjac-%offset%)
             (-
              (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                 (+ two
                    (* three (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))))
              c14))
            (go label500)
            '""
            '"     bard function."
            '""
           label190
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 15) nil)
              (tagbody
                (setf tmp1 (dfloat i))
                (setf tmp2 (dfloat (f2cl-lib:int-sub 16 i)))
                (setf tmp3 tmp1)
                (if (> i 8) (setf tmp3 tmp2))
                (setf tmp4
                        (expt
                         (+
                          (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                             tmp2)
                          (* (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                             tmp3))
                         2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- one))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/ (* tmp1 tmp2) tmp4))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/ (* tmp1 tmp3) tmp4))
               label200))
            (go label500)
            '""
            '"     kowalik and osborne function."
            '""
           label210
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 11) nil)
              (tagbody
                (setf tmp1
                        (* (f2cl-lib:fref v (i) ((1 11)))
                           (+ (f2cl-lib:fref v (i) ((1 11)))
                              (f2cl-lib:fref x-%data%
                                             (2)
                                             ((1 n))
                                             x-%offset%))))
                (setf tmp2
                        (+
                         (* (f2cl-lib:fref v (i) ((1 11)))
                            (+ (f2cl-lib:fref v (i) ((1 11)))
                               (f2cl-lib:fref x-%data%
                                              (3)
                                              ((1 n))
                                              x-%offset%)))
                         (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/ (- tmp1) tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/
                  (* (- (f2cl-lib:fref v (i) ((1 11))))
                     (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%))
                  tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (*
                  (f2cl-lib:fref fjac-%data%
                                 (i 1)
                                 ((1 ldfjac) (1 n))
                                 fjac-%offset%)
                  (f2cl-lib:fref fjac-%data%
                                 (i 2)
                                 ((1 ldfjac) (1 n))
                                 fjac-%offset%)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 4)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/
                  (f2cl-lib:fref fjac-%data%
                                 (i 3)
                                 ((1 ldfjac) (1 n))
                                 fjac-%offset%)
                  (f2cl-lib:fref v (i) ((1 11)))))
               label220))
            (go label500)
            '""
            '"     meyer function."
            '""
           label230
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 16) nil)
              (tagbody
                (setf temp
                        (+ (* five (dfloat i))
                           c45
                           (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)))
                (setf tmp1
                        (/ (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                           temp))
                (setf tmp2 (f2cl-lib:dexp tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 tmp2)
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/ (* (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) tmp2)
                    temp))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- tmp1)
                    (f2cl-lib:fref fjac-%data%
                                   (i 2)
                                   ((1 ldfjac) (1 n))
                                   fjac-%offset%)))
               label240))
            (go label500)
            '""
            '"     watson function."
            '""
           label250
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 29) nil)
              (tagbody
                (setf div (/ (dfloat i) c29))
                (setf s2 zero)
                (setf dx one)
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (setf s2
                            (+ s2
                               (* dx
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 n))
                                                 x-%offset%))))
                    (setf dx (* div dx))
                   label260))
                (setf temp (* two div s2))
                (setf dx (/ one div))
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     (* dx (- (dfloat (f2cl-lib:int-sub j 1)) temp)))
                    (setf dx (* div dx))
                   label270))
               label280))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (f2cl-lib:fdo (i 30 (f2cl-lib:int-add i 1))
                              ((> i 31) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     zero)
                   label290))
               label300))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data%
                            (30 1)
                            ((1 ldfjac) (1 n))
                            fjac-%offset%)
             one)
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data%
                            (31 1)
                            ((1 ldfjac) (1 n))
                            fjac-%offset%)
             (* (- two) (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
            (f2cl-lib:fset
             (f2cl-lib:fref fjac-%data%
                            (31 2)
                            ((1 ldfjac) (1 n))
                            fjac-%offset%)
             one)
            (go label500)
            '""
            '"     box 3-dimensional function."
            '""
           label310
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp (dfloat i))
                (setf tmp1 (/ temp ten))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- tmp1)
                    (f2cl-lib:dexp
                     (* (- tmp1)
                        (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* tmp1
                    (f2cl-lib:dexp
                     (* (- tmp1)
                        (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)))))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- (f2cl-lib:dexp (- temp)) (f2cl-lib:dexp (- tmp1))))
               label320))
            (go label500)
            '""
            '"     jennrich and sampson function."
            '""
           label330
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp (dfloat i))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- temp)
                    (f2cl-lib:dexp
                     (* temp
                        (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- temp)
                    (f2cl-lib:dexp
                     (* temp
                        (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)))))
               label340))
            (go label500)
            '""
            '"     brown and dennis function."
            '""
           label350
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp (/ (dfloat i) five))
                (setf ti (f2cl-lib:dsin temp))
                (setf tmp1
                        (-
                         (+ (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                            (* temp
                               (f2cl-lib:fref x-%data%
                                              (2)
                                              ((1 n))
                                              x-%offset%)))
                         (f2cl-lib:dexp temp)))
                (setf tmp2
                        (-
                         (+ (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                            (* ti
                               (f2cl-lib:fref x-%data%
                                              (4)
                                              ((1 n))
                                              x-%offset%)))
                         (f2cl-lib:dcos temp)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* two tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* temp
                    (f2cl-lib:fref fjac-%data%
                                   (i 1)
                                   ((1 ldfjac) (1 n))
                                   fjac-%offset%)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* two tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 4)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* ti
                    (f2cl-lib:fref fjac-%data%
                                   (i 3)
                                   ((1 ldfjac) (1 n))
                                   fjac-%offset%)))
               label360))
            (go label500)
            '""
            '"     chebyquad function."
            '""
           label370
            (setf dx (/ one (dfloat n)))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf tmp1 one)
                (setf tmp2
                        (-
                         (* two
                            (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
                         one))
                (setf temp (* two tmp2))
                (setf tmp3 zero)
                (setf tmp4 two)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     (* dx tmp4))
                    (setf ti (- (+ (* four tmp2) (* temp tmp4)) tmp3))
                    (setf tmp3 tmp4)
                    (setf tmp4 ti)
                    (setf ti (- (* temp tmp2) tmp1))
                    (setf tmp1 tmp2)
                    (setf tmp2 ti)
                   label380))
               label390))
            (go label500)
            '""
            '"     brown almost-linear function."
            '""
           label400
            (setf prod one)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf prod
                        (* (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                           prod))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fjac-%data%
                                    (i j)
                                    ((1 ldfjac) (1 n))
                                    fjac-%offset%)
                     one)
                   label410))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (j j)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 two)
               label420))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf temp (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
                (if (/= temp zero) (go label440))
                (setf temp one)
                (setf prod one)
                (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                              ((> k n) nil)
                  (tagbody
                    (if (/= k j)
                        (setf prod
                                (*
                                 (f2cl-lib:fref x-%data%
                                                (k)
                                                ((1 n))
                                                x-%offset%)
                                 prod)))
                   label430))
               label440
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (n j)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (/ prod temp))
               label450))
            (go label500)
            '""
            '"     osborne 1 function."
            '""
           label460
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 33) nil)
              (tagbody
                (setf temp (* ten (dfloat (f2cl-lib:int-sub i 1))))
                (setf tmp1
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%))
                            temp)))
                (setf tmp2
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (5) ((1 n)) x-%offset%))
                            temp)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- one))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 4)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* temp (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 5)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* temp (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) tmp2))
               label470))
            (go label500)
            '""
            '"     osborne 2 function."
            '""
           label480
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 65) nil)
              (tagbody
                (setf temp (/ (dfloat (f2cl-lib:int-sub i 1)) ten))
                (setf tmp1
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (5) ((1 n)) x-%offset%))
                            temp)))
                (setf tmp2
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (6) ((1 n)) x-%offset%))
                            (expt
                             (- temp
                                (f2cl-lib:fref x-%data%
                                               (9)
                                               ((1 n))
                                               x-%offset%))
                             2))))
                (setf tmp3
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (7) ((1 n)) x-%offset%))
                            (expt
                             (- temp
                                (f2cl-lib:fref x-%data%
                                               (10)
                                               ((1 n))
                                               x-%offset%))
                             2))))
                (setf tmp4
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (8) ((1 n)) x-%offset%))
                            (expt
                             (- temp
                                (f2cl-lib:fref x-%data%
                                               (11)
                                               ((1 n))
                                               x-%offset%))
                             2))))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 1)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 2)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 3)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- tmp3))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 4)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (- tmp4))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 5)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* temp (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 6)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                    (expt
                     (- temp (f2cl-lib:fref x-%data% (9) ((1 n)) x-%offset%))
                     2)
                    tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 7)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                    (expt
                     (- temp (f2cl-lib:fref x-%data% (10) ((1 n)) x-%offset%))
                     2)
                    tmp3))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 8)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%)
                    (expt
                     (- temp (f2cl-lib:fref x-%data% (11) ((1 n)) x-%offset%))
                     2)
                    tmp4))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 9)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- two)
                    (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                    (f2cl-lib:fref x-%data% (6) ((1 n)) x-%offset%)
                    (- temp (f2cl-lib:fref x-%data% (9) ((1 n)) x-%offset%))
                    tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 10)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- two)
                    (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                    (f2cl-lib:fref x-%data% (7) ((1 n)) x-%offset%)
                    (- temp (f2cl-lib:fref x-%data% (10) ((1 n)) x-%offset%))
                    tmp3))
                (f2cl-lib:fset
                 (f2cl-lib:fref fjac-%data%
                                (i 11)
                                ((1 ldfjac) (1 n))
                                fjac-%offset%)
                 (* (- two)
                    (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%)
                    (f2cl-lib:fref x-%data% (8) ((1 n)) x-%offset%)
                    (- temp (f2cl-lib:fref x-%data% (11) ((1 n)) x-%offset%))
                    tmp4))
               label490))
           label500
            (go end_label)
            '""
            '"     last card of subroutine ssqjac."
            '""
           end_label
            (return (values m n nil nil ldfjac nprob))))))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "MINPACK")


(let ((zero 0.0d0)
      (half 0.5d0)
      (one 1.0d0)
      (two 2.0d0)
      (three 3.0d0)
      (five 5.0d0)
      (seven 7.0d0)
      (ten 10.0d0)
      (twenty 20.0d0)
      (twntf 25.0d0)
      (c1 1.2d0)
      (c2 0.25d0)
      (c3 0.39d0)
      (c4 0.41500000000000004d0)
      (c5 0.02d0)
      (c6 4000.0d0)
      (c7 250.0d0)
      (c8 0.30000000000000004d0)
      (c9 0.4d0)
      (c10 1.5d0)
      (c11 0.01d0)
      (c12 1.3d0)
      (c13 0.65d0)
      (c14 0.7000000000000001d0)
      (c15 0.6000000000000001d0)
      (c16 4.5d0)
      (c17 5.5d0))
  (declare
   (type double-float c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2
    c1 twntf twenty ten seven five three two one half zero))
  (defun initpt (n x nprob factor)
    (declare (type double-float factor)
             (type (array double-float (*)) x)
             (type f2cl-lib:integer4 nprob n))
    (f2cl-lib:with-array-data (x-%data% x-%offset% x)
      (declare (type f2cl-lib:integer4 x-%offset%)
               (type (simple-array double-float (*)) x-%data%)
               (ignorable x-%offset% x-%data%))
      (labels ((dfloat (ivar)
                 (coerce ivar 'double-float)))
        (declare
         (ftype (function (f2cl-lib:integer4) (values double-float &rest t))
          dfloat))
        (prog ((h 0.0d0) (ivar 0) (j 0))
          (declare (type f2cl-lib:integer4 j ivar) (type double-float h))
          '"     **********"
          '""
          '"     subroutine initpt"
          '""
          '"     this subroutine specifies the standard starting points for the"
          '"     functions defined by subroutine ssqfcn. the subroutine returns"
          '"     in x a multiple (factor) of the standard starting point. for"
          '"     the 11th function the standard starting point is zero, so in"
          '"     this case, if factor is not unity, then the subroutine returns"
          '"     the vector  x(j) = factor, j=1,...,n."
          '""
          '"     the subroutine statement is"
          '""
          '"       subroutine initpt(n,x,nprob,factor)"
          '""
          '"     where"
          '""
          '"       n is a positive integer input variable."
          '""
          '"       x is an output array of length n which contains the standard"
          '"         starting point for problem nprob multiplied by factor."
          '""
          '"       nprob is a positive integer input variable which defines the"
          '"         number of the problem. nprob must not exceed 18."
          '""
          '"       factor is an input variable which specifies the multiple of"
          '"         the standard starting point. if factor is unity, no"
          '"         multiplication is performed."
          '""
          '"     argonne national laboratory. minpack project. march 1980."
          '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
          '""
          '"     **********"
          '""
          '"     selection of initial point."
          '""
          (f2cl-lib:computed-goto
           (label10 label10 label10 label30 label40 label50 label60 label70
            label80 label90 label100 label120 label130 label140 label150
            label170 label190 label200)
           nprob)
          '""
          '"     linear function - full rank or rank 1."
          '""
         label10
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                             one)
             label20))
          (go label210)
          '""
          '"     rosenbrock function."
          '""
         label30
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                         (- c1))
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) one)
          (go label210)
          '""
          '"     helical valley function."
          '""
         label40
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                         (- one))
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) zero)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) zero)
          (go label210)
          '""
          '"     powell singular function."
          '""
         label50
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) three)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                         (- one))
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) zero)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%) one)
          (go label210)
          '""
          '"     freudenstein and roth function."
          '""
         label60
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) half)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                         (- two))
          (go label210)
          '""
          '"     bard function."
          '""
         label70
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) one)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) one)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) one)
          (go label210)
          '""
          '"     kowalik and osborne function."
          '""
         label80
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) c2)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) c3)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) c4)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%) c3)
          (go label210)
          '""
          '"     meyer function."
          '""
         label90
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) c5)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) c6)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) c7)
          (go label210)
          '""
          '"     watson function."
          '""
         label100
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                             zero)
             label110))
          (go label210)
          '""
          '"     box 3-dimensional function."
          '""
         label120
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) zero)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) ten)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                         twenty)
          (go label210)
          '""
          '"     jennrich and sampson function."
          '""
         label130
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) c8)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) c9)
          (go label210)
          '""
          '"     brown and dennis function."
          '""
         label140
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) twntf)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) five)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                         (- five))
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%)
                         (- one))
          (go label210)
          '""
          '"     chebyquad function."
          '""
         label150
          (setf h (/ one (dfloat (f2cl-lib:int-add n 1))))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                             (* (dfloat j) h))
             label160))
          (go label210)
          '""
          '"     brown almost-linear function."
          '""
         label170
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                             half)
             label180))
          (go label210)
          '""
          '"     osborne 1 function."
          '""
         label190
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) half)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) c10)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                         (- one))
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%) c11)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (5) ((1 n)) x-%offset%) c5)
          (go label210)
          '""
          '"     osborne 2 function."
          '""
         label200
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) c12)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) c13)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) c13)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%) c14)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (5) ((1 n)) x-%offset%) c15)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (6) ((1 n)) x-%offset%) three)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (7) ((1 n)) x-%offset%) five)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (8) ((1 n)) x-%offset%) seven)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (9) ((1 n)) x-%offset%) two)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (10) ((1 n)) x-%offset%) c16)
          (f2cl-lib:fset (f2cl-lib:fref x-%data% (11) ((1 n)) x-%offset%) c17)
         label210
          '""
          '"     compute multiple of initial point."
          '""
          (if (= factor one) (go label260))
          (if (= nprob 11) (go label230))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                             (* factor
                                (f2cl-lib:fref x-%data%
                                               (j)
                                               ((1 n))
                                               x-%offset%)))
             label220))
          (go label250)
         label230
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fset (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                             factor)
             label240))
         label250
         label260
          (go end_label)
          '""
          '"     last card of subroutine initpt."
          '""
         end_label
          (return (values n nil nprob factor)))))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format single-float))

(in-package "MINPACK")


(let ((zero 0.0d0)
      (zp25 0.25d0)
      (zp5 0.5d0)
      (one 1.0d0)
      (two 2.0d0)
      (five 5.0d0)
      (eight 8.0d0)
      (ten 10.0d0)
      (c13 13.0d0)
      (c14 14.0d0)
      (c29 29.0d0)
      (c45 45.0d0)
      (v (make-array 11 :element-type 'double-float))
      (y1 (make-array 15 :element-type 'double-float))
      (y2 (make-array 11 :element-type 'double-float))
      (y3 (make-array 16 :element-type 'double-float))
      (y4 (make-array 33 :element-type 'double-float))
      (y5 (make-array 65 :element-type 'double-float)))
  (declare (type (array double-float (65)) y5)
           (type (array double-float (33)) y4)
           (type (array double-float (16)) y3)
           (type (array double-float (15)) y1)
           (type (array double-float (11)) y2 v)
           (type double-float c45 c29 c14 c13 ten eight five two one zp5 zp25
            zero))
  (f2cl-lib:fset (f2cl-lib:fref v (1) ((1 11))) 4.0d0)
  (f2cl-lib:fset (f2cl-lib:fref v (2) ((1 11))) 2.0d0)
  (f2cl-lib:fset (f2cl-lib:fref v (3) ((1 11))) 1.0d0)
  (f2cl-lib:fset (f2cl-lib:fref v (4) ((1 11))) 0.5d0)
  (f2cl-lib:fset (f2cl-lib:fref v (5) ((1 11))) 0.25d0)
  (f2cl-lib:fset (f2cl-lib:fref v (6) ((1 11))) 0.167d0)
  (f2cl-lib:fset (f2cl-lib:fref v (7) ((1 11))) 0.125d0)
  (f2cl-lib:fset (f2cl-lib:fref v (8) ((1 11))) 0.1d0)
  (f2cl-lib:fset (f2cl-lib:fref v (9) ((1 11))) 0.0833d0)
  (f2cl-lib:fset (f2cl-lib:fref v (10) ((1 11))) 0.0714d0)
  (f2cl-lib:fset (f2cl-lib:fref v (11) ((1 11))) 0.0625d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (1) ((1 15))) 0.13999999999999999d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (2) ((1 15))) 0.18000000000000002d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (3) ((1 15))) 0.22000000000000003d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (4) ((1 15))) 0.25d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (5) ((1 15))) 0.29d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (6) ((1 15))) 0.32000000000000006d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (7) ((1 15))) 0.35000000000000003d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (8) ((1 15))) 0.39d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (9) ((1 15))) 0.37000000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (10) ((1 15))) 0.58d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (11) ((1 15))) 0.73d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (12) ((1 15))) 0.96d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (13) ((1 15))) 1.34d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (14) ((1 15))) 2.1d0)
  (f2cl-lib:fset (f2cl-lib:fref y1 (15) ((1 15))) 4.39d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (1) ((1 11))) 0.1957d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (2) ((1 11))) 0.1947d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (3) ((1 11))) 0.17350000000000002d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (4) ((1 11))) 0.16000000000000003d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (5) ((1 11))) 0.0844d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (6) ((1 11))) 0.06269999999999999d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (7) ((1 11))) 0.045599999999999995d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (8) ((1 11))) 0.0342d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (9) ((1 11))) 0.0323d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (10) ((1 11))) 0.0235d0)
  (f2cl-lib:fset (f2cl-lib:fref y2 (11) ((1 11))) 0.0246d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (1) ((1 16))) 34780.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (2) ((1 16))) 28610.000000000004d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (3) ((1 16))) 23650.000000000004d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (4) ((1 16))) 19630.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (5) ((1 16))) 16370.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (6) ((1 16))) 13720.000000000002d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (7) ((1 16))) 11540.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (8) ((1 16))) 9744.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (9) ((1 16))) 8261.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (10) ((1 16))) 7030.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (11) ((1 16))) 6005.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (12) ((1 16))) 5147.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (13) ((1 16))) 4427.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (14) ((1 16))) 3820.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (15) ((1 16))) 3307.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y3 (16) ((1 16))) 2872.0d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (1) ((1 33))) 0.844d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (2) ((1 33))) 0.908d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (3) ((1 33))) 0.932d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (4) ((1 33))) 0.9359999999999999d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (5) ((1 33))) 0.925d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (6) ((1 33))) 0.908d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (7) ((1 33))) 0.8810000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (8) ((1 33))) 0.8500000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (9) ((1 33))) 0.8180000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (10) ((1 33))) 0.784d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (11) ((1 33))) 0.751d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (12) ((1 33))) 0.718d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (13) ((1 33))) 0.685d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (14) ((1 33))) 0.658d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (15) ((1 33))) 0.6280000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (16) ((1 33))) 0.6030000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (17) ((1 33))) 0.58d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (18) ((1 33))) 0.558d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (19) ((1 33))) 0.538d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (20) ((1 33))) 0.522d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (21) ((1 33))) 0.506d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (22) ((1 33))) 0.49000000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (23) ((1 33))) 0.47800000000000004d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (24) ((1 33))) 0.467d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (25) ((1 33))) 0.4570000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (26) ((1 33))) 0.44800000000000006d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (27) ((1 33))) 0.438d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (28) ((1 33))) 0.431d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (29) ((1 33))) 0.42400000000000004d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (30) ((1 33))) 0.42000000000000004d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (31) ((1 33))) 0.414d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (32) ((1 33))) 0.41100000000000003d0)
  (f2cl-lib:fset (f2cl-lib:fref y4 (33) ((1 33))) 0.40599999999999997d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (1) ((1 65))) 1.366d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (2) ((1 65))) 1.191d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (3) ((1 65))) 1.112d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (4) ((1 65))) 1.013d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (5) ((1 65))) 0.9910000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (6) ((1 65))) 0.885d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (7) ((1 65))) 0.8310000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (8) ((1 65))) 0.8470000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (9) ((1 65))) 0.786d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (10) ((1 65))) 0.7250000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (11) ((1 65))) 0.746d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (12) ((1 65))) 0.679d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (13) ((1 65))) 0.6080000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (14) ((1 65))) 0.655d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (15) ((1 65))) 0.6160000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (16) ((1 65))) 0.606d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (17) ((1 65))) 0.602d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (18) ((1 65))) 0.626d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (19) ((1 65))) 0.651d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (20) ((1 65))) 0.7240000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (21) ((1 65))) 0.649d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (22) ((1 65))) 0.649d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (23) ((1 65))) 0.6940000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (24) ((1 65))) 0.6440000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (25) ((1 65))) 0.6240000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (26) ((1 65))) 0.661d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (27) ((1 65))) 0.6120000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (28) ((1 65))) 0.558d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (29) ((1 65))) 0.533d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (30) ((1 65))) 0.49500000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (31) ((1 65))) 0.5d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (32) ((1 65))) 0.42300000000000004d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (33) ((1 65))) 0.395d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (34) ((1 65))) 0.375d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (35) ((1 65))) 0.37200000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (36) ((1 65))) 0.391d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (37) ((1 65))) 0.396d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (38) ((1 65))) 0.405d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (39) ((1 65))) 0.42800000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (40) ((1 65))) 0.42900000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (41) ((1 65))) 0.523d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (42) ((1 65))) 0.562d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (43) ((1 65))) 0.6070000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (44) ((1 65))) 0.653d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (45) ((1 65))) 0.672d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (46) ((1 65))) 0.7080000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (47) ((1 65))) 0.633d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (48) ((1 65))) 0.668d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (49) ((1 65))) 0.645d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (50) ((1 65))) 0.6320000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (51) ((1 65))) 0.5910000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (52) ((1 65))) 0.559d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (53) ((1 65))) 0.597d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (54) ((1 65))) 0.625d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (55) ((1 65))) 0.739d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (56) ((1 65))) 0.71d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (57) ((1 65))) 0.7290000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (58) ((1 65))) 0.7200000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (59) ((1 65))) 0.6360000000000001d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (60) ((1 65))) 0.581d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (61) ((1 65))) 0.42800000000000005d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (62) ((1 65))) 0.292d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (63) ((1 65))) 0.16200000000000003d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (64) ((1 65))) 0.098d0)
  (f2cl-lib:fset (f2cl-lib:fref y5 (65) ((1 65))) 0.054000000000000006d0)
  (defun ssqfcn (m n x fvec nprob)
    (declare (type (array double-float (*)) fvec x)
             (type f2cl-lib:integer4 nprob n m))
    (f2cl-lib:with-array-data (x-%data% x-%offset% x)
      (declare (type f2cl-lib:integer4 x-%offset%)
               (type (simple-array double-float (*)) x-%data%)
               (ignorable x-%offset% x-%data%))
      (f2cl-lib:with-array-data (fvec-%data% fvec-%offset% fvec)
        (declare (type f2cl-lib:integer4 fvec-%offset%)
                 (type (simple-array double-float (*)) fvec-%data%)
                 (ignorable fvec-%offset% fvec-%data%))
        (labels ((dfloat (ivar)
                   (coerce ivar 'double-float)))
          (declare
           (ftype (function (f2cl-lib:integer4) (values double-float &rest t))
            dfloat))
          (prog ((div 0.0d0) (dx 0.0d0) (prod 0.0d0) (sum 0.0d0) (s1 0.0d0)
                 (s2 0.0d0) (temp 0.0d0) (ti 0.0d0) (tmp1 0.0d0) (tmp2 0.0d0)
                 (tmp3 0.0d0) (tmp4 0.0d0) (tpi 0.0d0) (i 0) (iev 0) (ivar 0)
                 (j 0) (nm1 0))
            (declare (type f2cl-lib:integer4 nm1 j ivar iev i)
                     (type double-float tpi tmp4 tmp3 tmp2 tmp1 ti temp s2 s1
                      sum prod dx div))
            '"     **********"
            '""
            '"     subroutine ssqfcn"
            '""
            '"     this subroutine defines the functions of eighteen nonlinear"
            '"     least squares problems. the allowable values of (m,n) for"
            '"     functions 1,2 and 3 are variable but with m .ge. n."
            '"     for functions 4,5,6,7,8,9 and 10 the values of (m,n) are"
            '"     (2,2),(3,3),(4,4),(2,2),(15,3),(11,4) and (16,3), respectively."
            '"     function 11 (watson) has m = 31 with n usually 6 or 9."
            '"     however, any n, n = 2,...,31, is permitted."
            '"     functions 12,13 and 14 have n = 3,2 and 4, respectively, but"
            '"     allow any m .ge. n, with the usual choices being 10,10 and 20."
            '"     function 15 (chebyquad) allows m and n variable with m .ge. n."
            '"     function 16 (brown) allows n variable with m = n."
            '"     for functions 17 and 18, the values of (m,n) are"
            '"     (33,5) and (65,11), respectively."
            '""
            '"     the subroutine statement is"
            '""
            '"       subroutine ssqfcn(m,n,x,fvec,nprob)"
            '""
            '"     where"
            '""
            '"       m and n are positive integer input variables. n must not"
            '"         exceed m."
            '""
            '"       x is an input array of length n."
            '""
            '"       fvec is an output array of length m which contains the nprob"
            '"         function evaluated at x."
            '""
            '"       nprob is a positive integer input variable which defines the"
            '"         number of the problem. nprob must not exceed 18."
            '""
            '"     subprograms called"
            '""
            '"       fortran-supplied ... datan,dcos,dexp,dsin,dsqrt,dsign"
            '""
            '"     argonne national laboratory. minpack project. march 1980."
            '"     burton s. garbow, kenneth e. hillstrom, jorge j. more"
            '""
            '"     **********"
            '""
            '"     function routine selector."
            '""
            (f2cl-lib:computed-goto
             (label10 label40 label70 label110 label120 label130 label140
              label150 label170 label190 label210 label250 label270 label290
              label310 label360 label390 label410)
             nprob)
            '""
            '"     linear function - full rank."
            '""
           label10
            (setf sum zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
               label20))
            (setf temp (+ (/ (* two sum) (dfloat m)) one))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- temp))
                (if (<= i n)
                    (f2cl-lib:fset
                     (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                     (+ (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                        (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%))))
               label30))
            (go label430)
            '""
            '"     linear function - rank 1."
            '""
           label40
            (setf sum zero)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (* (dfloat j)
                              (f2cl-lib:fref x-%data%
                                             (j)
                                             ((1 n))
                                             x-%offset%))))
               label50))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (* (dfloat i) sum) one))
               label60))
            (go label430)
            '""
            '"     linear function - rank 1 with zero columns and rows."
            '""
           label70
            (setf sum zero)
            (setf nm1 (f2cl-lib:int-sub n 1))
            (if (< nm1 2) (go label90))
            (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                          ((> j nm1) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (* (dfloat j)
                              (f2cl-lib:fref x-%data%
                                             (j)
                                             ((1 n))
                                             x-%offset%))))
               label80))
           label90
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (* (dfloat (f2cl-lib:int-sub i 1)) sum) one))
               label100))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (m) ((1 m)) fvec-%offset%)
             (- one))
            (go label430)
            '""
            '"     rosenbrock function."
            '""
           label110
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (1) ((1 m)) fvec-%offset%)
             (* ten
                (- (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                   (expt (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) 2))))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (2) ((1 m)) fvec-%offset%)
             (- one (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
            (go label430)
            '""
            '"     helical valley function."
            '""
           label120
            (setf tpi (* eight (f2cl-lib:datan one)))
            (setf tmp1
                    (coerce
                     (f2cl-lib:dsign zp25
                                     (f2cl-lib:fref x-%data%
                                                    (2)
                                                    ((1 n))
                                                    x-%offset%))
                     'double-float))
            (if (> (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) zero)
                (setf tmp1
                        (/
                         (f2cl-lib:datan
                          (/ (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                             (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
                         tpi)))
            (if (< (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) zero)
                (setf tmp1
                        (+
                         (/
                          (f2cl-lib:datan
                           (/ (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                              (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
                          tpi)
                         zp5)))
            (setf tmp2
                    (f2cl-lib:dsqrt
                     (+
                      (expt (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) 2)
                      (expt (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                            2))))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (1) ((1 m)) fvec-%offset%)
             (* ten
                (- (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                   (* ten tmp1))))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (2) ((1 m)) fvec-%offset%)
             (* ten (- tmp2 one)))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (3) ((1 m)) fvec-%offset%)
             (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%))
            (go label430)
            '""
            '"     powell singular function."
            '""
           label130
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (1) ((1 m)) fvec-%offset%)
             (+ (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                (* ten (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (2) ((1 m)) fvec-%offset%)
             (* (f2cl-lib:dsqrt five)
                (- (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                   (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%))))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (3) ((1 m)) fvec-%offset%)
             (expt
              (- (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                 (* two (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)))
              2))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (4) ((1 m)) fvec-%offset%)
             (* (f2cl-lib:dsqrt ten)
                (expt
                 (- (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                    (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%))
                 2)))
            (go label430)
            '""
            '"     freudenstein and roth function."
            '""
           label140
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (1) ((1 m)) fvec-%offset%)
             (+ (- (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) c13)
                (*
                 (-
                  (* (- five (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))
                     (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))
                  two)
                 (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (2) ((1 m)) fvec-%offset%)
             (+ (- (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) c29)
                (*
                 (-
                  (* (+ one (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))
                     (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))
                  c14)
                 (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))))
            (go label430)
            '""
            '"     bard function."
            '""
           label150
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 15) nil)
              (tagbody
                (setf tmp1 (dfloat i))
                (setf tmp2 (dfloat (f2cl-lib:int-sub 16 i)))
                (setf tmp3 tmp1)
                (if (> i 8) (setf tmp3 tmp2))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (f2cl-lib:fref y1 (i) ((1 15)))
                    (+ (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                       (/ tmp1
                          (+
                           (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                              tmp2)
                           (* (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                              tmp3))))))
               label160))
            (go label430)
            '""
            '"     kowalik and osborne function."
            '""
           label170
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 11) nil)
              (tagbody
                (setf tmp1
                        (* (f2cl-lib:fref v (i) ((1 11)))
                           (+ (f2cl-lib:fref v (i) ((1 11)))
                              (f2cl-lib:fref x-%data%
                                             (2)
                                             ((1 n))
                                             x-%offset%))))
                (setf tmp2
                        (+
                         (* (f2cl-lib:fref v (i) ((1 11)))
                            (+ (f2cl-lib:fref v (i) ((1 11)))
                               (f2cl-lib:fref x-%data%
                                              (3)
                                              ((1 n))
                                              x-%offset%)))
                         (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (+ (f2cl-lib:fref y2 (i) ((1 11)))
                    (/
                     (* (- (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%))
                        tmp1)
                     tmp2)))
               label180))
            (go label430)
            '""
            '"     meyer function."
            '""
           label190
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 16) nil)
              (tagbody
                (setf temp
                        (+ (* five (dfloat i))
                           c45
                           (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)))
                (setf tmp1
                        (/ (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                           temp))
                (setf tmp2 (f2cl-lib:dexp tmp1))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (* (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) tmp2)
                    (f2cl-lib:fref y3 (i) ((1 16)))))
               label200))
            (go label430)
            '""
            '"     watson function."
            '""
           label210
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 29) nil)
              (tagbody
                (setf div (/ (dfloat i) c29))
                (setf s1 zero)
                (setf dx one)
                (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (setf s1
                            (+ s1
                               (* (dfloat (f2cl-lib:int-sub j 1))
                                  dx
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 n))
                                                 x-%offset%))))
                    (setf dx (* div dx))
                   label220))
                (setf s2 zero)
                (setf dx one)
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (setf s2
                            (+ s2
                               (* dx
                                  (f2cl-lib:fref x-%data%
                                                 (j)
                                                 ((1 n))
                                                 x-%offset%))))
                    (setf dx (* div dx))
                   label230))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- s1 (expt s2 2) one))
               label240))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (30) ((1 m)) fvec-%offset%)
             (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (31) ((1 m)) fvec-%offset%)
             (- (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)
                (expt (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) 2)
                one))
            (go label430)
            '""
            '"     box 3-dimensional function."
            '""
           label250
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp (dfloat i))
                (setf tmp1 (/ temp ten))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (+
                  (-
                   (f2cl-lib:dexp
                    (* (- tmp1)
                       (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
                   (f2cl-lib:dexp
                    (* (- tmp1)
                       (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%))))
                  (* (- (f2cl-lib:dexp (- temp)) (f2cl-lib:dexp (- tmp1)))
                     (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%))))
               label260))
            (go label430)
            '""
            '"     jennrich and sampson function."
            '""
           label270
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp (dfloat i))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (+ two (* two temp))
                    (f2cl-lib:dexp
                     (* temp (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)))
                    (f2cl-lib:dexp
                     (* temp
                        (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%)))))
               label280))
            (go label430)
            '""
            '"     brown and dennis function."
            '""
           label290
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (setf temp (/ (dfloat i) five))
                (setf tmp1
                        (-
                         (+ (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                            (* temp
                               (f2cl-lib:fref x-%data%
                                              (2)
                                              ((1 n))
                                              x-%offset%)))
                         (f2cl-lib:dexp temp)))
                (setf tmp2
                        (-
                         (+ (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                            (* (f2cl-lib:dsin temp)
                               (f2cl-lib:fref x-%data%
                                              (4)
                                              ((1 n))
                                              x-%offset%)))
                         (f2cl-lib:dcos temp)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (+ (expt tmp1 2) (expt tmp2 2)))
               label300))
            (go label430)
            '""
            '"     chebyquad function."
            '""
           label310
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 zero)
               label320))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf tmp1 one)
                (setf tmp2
                        (-
                         (* two
                            (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%))
                         one))
                (setf temp (* two tmp2))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i m) nil)
                  (tagbody
                    (f2cl-lib:fset
                     (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                     (+ (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                        tmp2))
                    (setf ti (- (* temp tmp2) tmp1))
                    (setf tmp1 tmp2)
                    (setf tmp2 ti)
                   label330))
               label340))
            (setf dx (/ one (dfloat n)))
            (setf iev -1)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i m) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (* dx (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)))
                (if (> iev 0)
                    (f2cl-lib:fset
                     (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                     (+ (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                        (/ one (- (expt (dfloat i) 2) one)))))
                (setf iev (f2cl-lib:int-sub iev))
               label350))
            (go label430)
            '""
            '"     brown almost-linear function."
            '""
           label360
            (setf sum (- (dfloat (f2cl-lib:int-add n 1))))
            (setf prod one)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
                (setf sum
                        (+ sum
                           (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)))
                (setf prod
                        (* (f2cl-lib:fref x-%data% (j) ((1 n)) x-%offset%)
                           prod))
               label370))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i n) nil)
              (tagbody
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (+ (f2cl-lib:fref x-%data% (i) ((1 n)) x-%offset%) sum))
               label380))
            (f2cl-lib:fset
             (f2cl-lib:fref fvec-%data% (n) ((1 m)) fvec-%offset%)
             (- prod one))
            (go label430)
            '""
            '"     osborne 1 function."
            '""
           label390
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 33) nil)
              (tagbody
                (setf temp (* ten (dfloat (f2cl-lib:int-sub i 1))))
                (setf tmp1
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%))
                            temp)))
                (setf tmp2
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (5) ((1 n)) x-%offset%))
                            temp)))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (f2cl-lib:fref y4 (i) ((1 33)))
                    (+ (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%)
                       (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) tmp1)
                       (* (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%)
                          tmp2))))
               label400))
            (go label430)
            '""
            '"     osborne 2 function."
            '""
           label410
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 65) nil)
              (tagbody
                (setf temp (/ (dfloat (f2cl-lib:int-sub i 1)) ten))
                (setf tmp1
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (5) ((1 n)) x-%offset%))
                            temp)))
                (setf tmp2
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (6) ((1 n)) x-%offset%))
                            (expt
                             (- temp
                                (f2cl-lib:fref x-%data%
                                               (9)
                                               ((1 n))
                                               x-%offset%))
                             2))))
                (setf tmp3
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (7) ((1 n)) x-%offset%))
                            (expt
                             (- temp
                                (f2cl-lib:fref x-%data%
                                               (10)
                                               ((1 n))
                                               x-%offset%))
                             2))))
                (setf tmp4
                        (f2cl-lib:dexp
                         (* (- (f2cl-lib:fref x-%data% (8) ((1 n)) x-%offset%))
                            (expt
                             (- temp
                                (f2cl-lib:fref x-%data%
                                               (11)
                                               ((1 n))
                                               x-%offset%))
                             2))))
                (f2cl-lib:fset
                 (f2cl-lib:fref fvec-%data% (i) ((1 m)) fvec-%offset%)
                 (- (f2cl-lib:fref y5 (i) ((1 65)))
                    (+ (* (f2cl-lib:fref x-%data% (1) ((1 n)) x-%offset%) tmp1)
                       (* (f2cl-lib:fref x-%data% (2) ((1 n)) x-%offset%) tmp2)
                       (* (f2cl-lib:fref x-%data% (3) ((1 n)) x-%offset%) tmp3)
                       (* (f2cl-lib:fref x-%data% (4) ((1 n)) x-%offset%)
                          tmp4))))
               label420))
           label430
            (go end_label)
            '""
            '"     last card of subroutine ssqfcn."
            '""
           end_label
            (return (values m n nil nil nprob))))))))

