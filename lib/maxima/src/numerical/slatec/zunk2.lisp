;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((zeror 0.0)
      (zeroi 0.0)
      (coner 1.0)
      (cr1r 1.0)
      (cr1i 1.7320508075688772)
      (cr2r -0.5)
      (cr2i -0.8660254037844386)
      (hpi 1.5707963267948966)
      (pi$ 3.141592653589793)
      (aic 1.2655121234846454)
      (cipr
       (make-array 4
                   :element-type 'double-float
                   :initial-contents '(1.0 0.0 -1.0 0.0)))
      (cipi
       (make-array 4
                   :element-type 'double-float
                   :initial-contents '(0.0 -1.0 0.0 1.0))))
  (declare (type (double-float) zeror zeroi coner cr1r cr1i cr2r cr2i hpi pi$
                                aic)
           (type (simple-array double-float (4)) cipr cipi))
  (defun zunk2 (zr zi fnu kode mr n yr yi nz tol elim alim)
    (declare (type (simple-array double-float (*)) yi yr)
             (type (f2cl-lib:integer4) nz n mr kode)
             (type (double-float) alim elim tol fnu zi zr))
    (prog ((bry (make-array 3 :element-type 'double-float))
           (asumr (make-array 2 :element-type 'double-float))
           (asumi (make-array 2 :element-type 'double-float))
           (bsumr (make-array 2 :element-type 'double-float))
           (bsumi (make-array 2 :element-type 'double-float))
           (phir (make-array 2 :element-type 'double-float))
           (phii (make-array 2 :element-type 'double-float))
           (argr (make-array 2 :element-type 'double-float))
           (argi (make-array 2 :element-type 'double-float))
           (zeta1r (make-array 2 :element-type 'double-float))
           (zeta1i (make-array 2 :element-type 'double-float))
           (zeta2r (make-array 2 :element-type 'double-float))
           (zeta2i (make-array 2 :element-type 'double-float))
           (cyr (make-array 2 :element-type 'double-float))
           (cyi (make-array 2 :element-type 'double-float))
           (cssr (make-array 3 :element-type 'double-float))
           (csrr (make-array 3 :element-type 'double-float)) (i 0) (ib 0)
           (iflag 0) (ifn 0) (il 0) (in 0) (inu 0) (iuf 0) (k 0) (kdflg 0)
           (kflag 0) (kk 0) (nai 0) (ndai 0) (nw 0) (idum 0) (j 0) (ipard 0)
           (ic 0) (aarg 0.0) (aii 0.0) (air 0.0) (ang 0.0) (aphi 0.0)
           (argdi 0.0) (argdr 0.0) (asc 0.0) (ascle 0.0) (asumdi 0.0)
           (asumdr 0.0) (bsumdi 0.0) (bsumdr 0.0) (car$ 0.0) (cki 0.0)
           (ckr 0.0) (crsc 0.0) (cscl 0.0) (csgni 0.0) (csi 0.0) (cspni 0.0)
           (cspnr 0.0) (csr 0.0) (c1i 0.0) (c1r 0.0) (c2i 0.0) (c2m 0.0)
           (c2r 0.0) (daii 0.0) (dair 0.0) (fmr 0.0) (fn 0.0) (fnf 0.0)
           (phidi 0.0) (phidr 0.0) (pti 0.0) (ptr 0.0) (rast 0.0) (razr 0.0)
           (rs1 0.0) (rzi 0.0) (rzr 0.0) (sar 0.0) (sgn 0.0) (sti 0.0)
           (str 0.0) (s1i 0.0) (s1r 0.0) (s2i 0.0) (s2r 0.0) (yy 0.0) (zbi 0.0)
           (zbr 0.0) (zet1di 0.0) (zet1dr 0.0) (zet2di 0.0) (zet2dr 0.0)
           (zni 0.0) (znr 0.0) (zri 0.0) (zrr 0.0))
      (declare (type (simple-array double-float (3)) cssr csrr bry)
               (type (simple-array double-float (2)) zeta2r zeta2i zeta1r
                                                     zeta1i phir phii cyr cyi
                                                     bsumr bsumi asumr asumi
                                                     argr argi)
               (type (double-float) zrr zri znr zni zet2dr zet2di zet1dr zet1di
                                    zbr zbi yy s2r s2i s1r s1i str sti sgn sar
                                    rzr rzi rs1 razr rast ptr pti phidr phidi
                                    fnf fn fmr dair daii c2r c2m c2i c1r c1i
                                    csr cspnr cspni csi csgni cscl crsc ckr cki
                                    car$ bsumdr bsumdi asumdr asumdi ascle asc
                                    argdr argdi aphi ang air aii aarg)
               (type (f2cl-lib:integer4) ic ipard j idum nw ndai nai kk kflag
                                         kdflg k iuf inu in il ifn iflag ib i))
      (setf kdflg 1)
      (setf nz 0)
      (setf cscl (/ 1.0 tol))
      (setf crsc tol)
      (setf (f2cl-lib:fref cssr (1) ((1 3))) cscl)
      (setf (f2cl-lib:fref cssr (2) ((1 3))) coner)
      (setf (f2cl-lib:fref cssr (3) ((1 3))) crsc)
      (setf (f2cl-lib:fref csrr (1) ((1 3))) crsc)
      (setf (f2cl-lib:fref csrr (2) ((1 3))) coner)
      (setf (f2cl-lib:fref csrr (3) ((1 3))) cscl)
      (setf (f2cl-lib:fref bry (1) ((1 3)))
              (/ (* 1000.0 (f2cl-lib:d1mach 1)) tol))
      (setf (f2cl-lib:fref bry (2) ((1 3)))
              (/ 1.0 (f2cl-lib:fref bry (1) ((1 3)))))
      (setf (f2cl-lib:fref bry (3) ((1 3))) (f2cl-lib:d1mach 2))
      (setf zrr zr)
      (setf zri zi)
      (if (>= zr 0.0) (go label10))
      (setf zrr (- zr))
      (setf zri (- zi))
     label10
      (setf yy zri)
      (setf znr zri)
      (setf zni (- zrr))
      (setf zbr zrr)
      (setf zbi zri)
      (setf inu (f2cl-lib:int fnu))
      (setf fnf (- fnu inu))
      (setf ang (* (- hpi) fnf))
      (setf car$ (cos ang))
      (setf sar (sin ang))
      (setf c2r (* hpi sar))
      (setf c2i (* (- hpi) car$))
      (setf kk (f2cl-lib:int-add (mod inu 4) 1))
      (setf str
              (- (* c2r (f2cl-lib:fref cipr (kk) ((1 4))))
                 (* c2i (f2cl-lib:fref cipi (kk) ((1 4))))))
      (setf sti
              (+ (* c2r (f2cl-lib:fref cipi (kk) ((1 4))))
                 (* c2i (f2cl-lib:fref cipr (kk) ((1 4))))))
      (setf csr (- (* cr1r str) (* cr1i sti)))
      (setf csi (+ (* cr1r sti) (* cr1i str)))
      (if (> yy 0.0) (go label20))
      (setf znr (- znr))
      (setf zbi (- zbi))
     label20
      (setf j 2)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf j (f2cl-lib:int-sub 3 j))
          (setf fn (+ fnu (f2cl-lib:int-sub i 1)))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
              (zunhj znr zni fn 0 tol (f2cl-lib:fref phir (j) ((1 2)))
               (f2cl-lib:fref phii (j) ((1 2)))
               (f2cl-lib:fref argr (j) ((1 2)))
               (f2cl-lib:fref argi (j) ((1 2)))
               (f2cl-lib:fref zeta1r (j) ((1 2)))
               (f2cl-lib:fref zeta1i (j) ((1 2)))
               (f2cl-lib:fref zeta2r (j) ((1 2)))
               (f2cl-lib:fref zeta2i (j) ((1 2)))
               (f2cl-lib:fref asumr (j) ((1 2)))
               (f2cl-lib:fref asumi (j) ((1 2)))
               (f2cl-lib:fref bsumr (j) ((1 2)))
               (f2cl-lib:fref bsumi (j) ((1 2))))
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf (f2cl-lib:fref phir (j) ((1 2))) var-5)
            (setf (f2cl-lib:fref phii (j) ((1 2))) var-6)
            (setf (f2cl-lib:fref argr (j) ((1 2))) var-7)
            (setf (f2cl-lib:fref argi (j) ((1 2))) var-8)
            (setf (f2cl-lib:fref zeta1r (j) ((1 2))) var-9)
            (setf (f2cl-lib:fref zeta1i (j) ((1 2))) var-10)
            (setf (f2cl-lib:fref zeta2r (j) ((1 2))) var-11)
            (setf (f2cl-lib:fref zeta2i (j) ((1 2))) var-12)
            (setf (f2cl-lib:fref asumr (j) ((1 2))) var-13)
            (setf (f2cl-lib:fref asumi (j) ((1 2))) var-14)
            (setf (f2cl-lib:fref bsumr (j) ((1 2))) var-15)
            (setf (f2cl-lib:fref bsumi (j) ((1 2))) var-16))
          (if (= kode 1) (go label30))
          (setf str (+ zbr (f2cl-lib:fref zeta2r (j) ((1 2)))))
          (setf sti (+ zbi (f2cl-lib:fref zeta2i (j) ((1 2)))))
          (setf rast (coerce (realpart (/ fn (zabs str sti))) 'double-float))
          (setf str (* str rast rast))
          (setf sti (* (- sti) rast rast))
          (setf s1r (- (f2cl-lib:fref zeta1r (j) ((1 2))) str))
          (setf s1i (- (f2cl-lib:fref zeta1i (j) ((1 2))) sti))
          (go label40)
         label30
          (setf s1r
                  (- (f2cl-lib:fref zeta1r (j) ((1 2)))
                     (f2cl-lib:fref zeta2r (j) ((1 2)))))
          (setf s1i
                  (- (f2cl-lib:fref zeta1i (j) ((1 2)))
                     (f2cl-lib:fref zeta2i (j) ((1 2)))))
         label40
          (setf rs1 s1r)
          (if (> (abs rs1) elim) (go label70))
          (if (= kdflg 1) (setf kflag 2))
          (if (< (abs rs1) alim) (go label50))
          (setf aphi
                  (coerce
                   (realpart
                    (zabs (f2cl-lib:fref phir (j) ((1 2)))
                     (f2cl-lib:fref phii (j) ((1 2)))))
                   'double-float))
          (setf aarg
                  (coerce
                   (realpart
                    (zabs (f2cl-lib:fref argr (j) ((1 2)))
                     (f2cl-lib:fref argi (j) ((1 2)))))
                   'double-float))
          (setf rs1
                  (- (+ rs1 (f2cl-lib:flog aphi))
                     (* 0.25 (f2cl-lib:flog aarg))
                     aic))
          (if (> (abs rs1) elim) (go label70))
          (if (= kdflg 1) (setf kflag 1))
          (if (< rs1 0.0) (go label50))
          (if (= kdflg 1) (setf kflag 3))
         label50
          (setf c2r
                  (- (* (f2cl-lib:fref argr (j) ((1 2))) cr2r)
                     (* (f2cl-lib:fref argi (j) ((1 2))) cr2i)))
          (setf c2i
                  (+ (* (f2cl-lib:fref argr (j) ((1 2))) cr2i)
                     (* (f2cl-lib:fref argi (j) ((1 2))) cr2r)))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zairy c2r c2i 0 2 air aii nai idum)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf air var-4)
            (setf aii var-5)
            (setf nai var-6)
            (setf idum var-7))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zairy c2r c2i 1 2 dair daii ndai idum)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf dair var-4)
            (setf daii var-5)
            (setf ndai var-6)
            (setf idum var-7))
          (setf str
                  (- (* dair (f2cl-lib:fref bsumr (j) ((1 2))))
                     (* daii (f2cl-lib:fref bsumi (j) ((1 2))))))
          (setf sti
                  (+ (* dair (f2cl-lib:fref bsumi (j) ((1 2))))
                     (* daii (f2cl-lib:fref bsumr (j) ((1 2))))))
          (setf ptr (- (* str cr2r) (* sti cr2i)))
          (setf pti (+ (* str cr2i) (* sti cr2r)))
          (setf str
                  (+ ptr
                     (- (* air (f2cl-lib:fref asumr (j) ((1 2))))
                        (* aii (f2cl-lib:fref asumi (j) ((1 2)))))))
          (setf sti
                  (+ pti
                     (+ (* air (f2cl-lib:fref asumi (j) ((1 2))))
                        (* aii (f2cl-lib:fref asumr (j) ((1 2)))))))
          (setf ptr
                  (- (* str (f2cl-lib:fref phir (j) ((1 2))))
                     (* sti (f2cl-lib:fref phii (j) ((1 2))))))
          (setf pti
                  (+ (* str (f2cl-lib:fref phii (j) ((1 2))))
                     (* sti (f2cl-lib:fref phir (j) ((1 2))))))
          (setf s2r (- (* ptr csr) (* pti csi)))
          (setf s2i (+ (* ptr csi) (* pti csr)))
          (setf str (* (exp s1r) (f2cl-lib:fref cssr (kflag) ((1 3)))))
          (setf s1r (* str (cos s1i)))
          (setf s1i (* str (sin s1i)))
          (setf str (- (* s2r s1r) (* s2i s1i)))
          (setf s2i (+ (* s1r s2i) (* s2r s1i)))
          (setf s2r str)
          (if (/= kflag 1) (go label60))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (zuchk s2r s2i nw (f2cl-lib:fref bry (1) ((1 3))) tol)
            (declare (ignore var-0 var-1 var-3 var-4))
            (setf nw var-2))
          (if (/= nw 0) (go label70))
         label60
          (if (<= yy 0.0) (setf s2i (- s2i)))
          (setf (f2cl-lib:fref cyr (kdflg) ((1 2))) s2r)
          (setf (f2cl-lib:fref cyi (kdflg) ((1 2))) s2i)
          (setf (f2cl-lib:fref yr (i) ((1 n)))
                  (* s2r (f2cl-lib:fref csrr (kflag) ((1 3)))))
          (setf (f2cl-lib:fref yi (i) ((1 n)))
                  (* s2i (f2cl-lib:fref csrr (kflag) ((1 3)))))
          (setf str csi)
          (setf csi (- csr))
          (setf csr str)
          (if (= kdflg 2) (go label85))
          (setf kdflg 2)
          (go label80)
         label70
          (if (> rs1 0.0) (go label320))
          (if (< zr 0.0) (go label320))
          (setf kdflg 1)
          (setf (f2cl-lib:fref yr (i) ((1 n))) zeror)
          (setf (f2cl-lib:fref yi (i) ((1 n))) zeroi)
          (setf nz (f2cl-lib:int-add nz 1))
          (setf str csi)
          (setf csi (- csr))
          (setf csr str)
          (if (= i 1) (go label80))
          (if
           (and (= (f2cl-lib:fref yr ((f2cl-lib:int-sub i 1)) ((1 n))) zeror)
                (= (f2cl-lib:fref yi ((f2cl-lib:int-sub i 1)) ((1 n))) zeroi))
           (go label80))
          (setf (f2cl-lib:fref yr ((f2cl-lib:int-sub i 1)) ((1 n))) zeror)
          (setf (f2cl-lib:fref yi ((f2cl-lib:int-sub i 1)) ((1 n))) zeroi)
          (setf nz (f2cl-lib:int-add nz 1))
         label80))
      (setf i n)
     label85
      (setf razr (coerce (realpart (/ 1.0 (zabs zrr zri))) 'double-float))
      (setf str (* zrr razr))
      (setf sti (* (- zri) razr))
      (setf rzr (* (+ str str) razr))
      (setf rzi (* (+ sti sti) razr))
      (setf ckr (* fn rzr))
      (setf cki (* fn rzi))
      (setf ib (f2cl-lib:int-add i 1))
      (if (< n ib) (go label180))
      (setf fn (+ fnu (f2cl-lib:int-sub n 1)))
      (setf ipard 1)
      (if (/= mr 0) (setf ipard 0))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9 var-10
             var-11 var-12 var-13 var-14 var-15 var-16)
          (zunhj znr zni fn ipard tol phidr phidi argdr argdi zet1dr zet1di
           zet2dr zet2di asumdr asumdi bsumdr bsumdi)
        (declare (ignore var-0 var-1 var-2 var-3 var-4))
        (setf phidr var-5)
        (setf phidi var-6)
        (setf argdr var-7)
        (setf argdi var-8)
        (setf zet1dr var-9)
        (setf zet1di var-10)
        (setf zet2dr var-11)
        (setf zet2di var-12)
        (setf asumdr var-13)
        (setf asumdi var-14)
        (setf bsumdr var-15)
        (setf bsumdi var-16))
      (if (= kode 1) (go label90))
      (setf str (+ zbr zet2dr))
      (setf sti (+ zbi zet2di))
      (setf rast (coerce (realpart (/ fn (zabs str sti))) 'double-float))
      (setf str (* str rast rast))
      (setf sti (* (- sti) rast rast))
      (setf s1r (- zet1dr str))
      (setf s1i (- zet1di sti))
      (go label100)
     label90
      (setf s1r (- zet1dr zet2dr))
      (setf s1i (- zet1di zet2di))
     label100
      (setf rs1 s1r)
      (if (> (abs rs1) elim) (go label105))
      (if (< (abs rs1) alim) (go label120))
      (setf aphi (coerce (realpart (zabs phidr phidi)) 'double-float))
      (setf rs1 (+ rs1 (f2cl-lib:flog aphi)))
      (if (< (abs rs1) elim) (go label120))
     label105
      (if (> rs1 0.0) (go label320))
      (if (< zr 0.0) (go label320))
      (setf nz n)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref yr (i) ((1 n))) zeror)
          (setf (f2cl-lib:fref yi (i) ((1 n))) zeroi)
         label106))
      (go end_label)
     label120
      (setf s1r (f2cl-lib:fref cyr (1) ((1 2))))
      (setf s1i (f2cl-lib:fref cyi (1) ((1 2))))
      (setf s2r (f2cl-lib:fref cyr (2) ((1 2))))
      (setf s2i (f2cl-lib:fref cyi (2) ((1 2))))
      (setf c1r (f2cl-lib:fref csrr (kflag) ((1 3))))
      (setf ascle (f2cl-lib:fref bry (kflag) ((1 3))))
      (f2cl-lib:fdo (i ib (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf c2r s2r)
          (setf c2i s2i)
          (setf s2r (+ (- (* ckr c2r) (* cki c2i)) s1r))
          (setf s2i (+ (* ckr c2i) (* cki c2r) s1i))
          (setf s1r c2r)
          (setf s1i c2i)
          (setf ckr (+ ckr rzr))
          (setf cki (+ cki rzi))
          (setf c2r (* s2r c1r))
          (setf c2i (* s2i c1r))
          (setf (f2cl-lib:fref yr (i) ((1 n))) c2r)
          (setf (f2cl-lib:fref yi (i) ((1 n))) c2i)
          (if (>= kflag 3) (go label130))
          (setf str (abs c2r))
          (setf sti (abs c2i))
          (setf c2m (max str sti))
          (if (<= c2m ascle) (go label130))
          (setf kflag (f2cl-lib:int-add kflag 1))
          (setf ascle (f2cl-lib:fref bry (kflag) ((1 3))))
          (setf s1r (* s1r c1r))
          (setf s1i (* s1i c1r))
          (setf s2r c2r)
          (setf s2i c2i)
          (setf s1r (* s1r (f2cl-lib:fref cssr (kflag) ((1 3)))))
          (setf s1i (* s1i (f2cl-lib:fref cssr (kflag) ((1 3)))))
          (setf s2r (* s2r (f2cl-lib:fref cssr (kflag) ((1 3)))))
          (setf s2i (* s2i (f2cl-lib:fref cssr (kflag) ((1 3)))))
          (setf c1r (f2cl-lib:fref csrr (kflag) ((1 3))))
         label130))
     label180
      (if (= mr 0) (go end_label))
      (setf nz 0)
      (setf fmr (coerce (the f2cl-lib:integer4 mr) 'double-float))
      (setf sgn (coerce (- (f2cl-lib:dsign pi$ fmr)) 'double-float))
      (setf csgni sgn)
      (if (<= yy 0.0) (setf csgni (- csgni)))
      (setf ifn (f2cl-lib:int-sub (f2cl-lib:int-add inu n) 1))
      (setf ang (* fnf sgn))
      (setf cspnr (cos ang))
      (setf cspni (sin ang))
      (if (= (mod ifn 2) 0) (go label190))
      (setf cspnr (- cspnr))
      (setf cspni (- cspni))
     label190
      (setf csr (* sar csgni))
      (setf csi (* car$ csgni))
      (setf in (f2cl-lib:int-add (mod ifn 4) 1))
      (setf c2r (f2cl-lib:fref cipr (in) ((1 4))))
      (setf c2i (f2cl-lib:fref cipi (in) ((1 4))))
      (setf str (+ (* csr c2r) (* csi c2i)))
      (setf csi (+ (* (- csr) c2i) (* csi c2r)))
      (setf csr str)
      (setf asc (f2cl-lib:fref bry (1) ((1 3))))
      (setf iuf 0)
      (setf kk n)
      (setf kdflg 1)
      (setf ib (f2cl-lib:int-sub ib 1))
      (setf ic (f2cl-lib:int-sub ib 1))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf fn (+ fnu (f2cl-lib:int-sub kk 1)))
          (if (> n 2) (go label175))
         label172
          (setf phidr (f2cl-lib:fref phir (j) ((1 2))))
          (setf phidi (f2cl-lib:fref phii (j) ((1 2))))
          (setf argdr (f2cl-lib:fref argr (j) ((1 2))))
          (setf argdi (f2cl-lib:fref argi (j) ((1 2))))
          (setf zet1dr (f2cl-lib:fref zeta1r (j) ((1 2))))
          (setf zet1di (f2cl-lib:fref zeta1i (j) ((1 2))))
          (setf zet2dr (f2cl-lib:fref zeta2r (j) ((1 2))))
          (setf zet2di (f2cl-lib:fref zeta2i (j) ((1 2))))
          (setf asumdr (f2cl-lib:fref asumr (j) ((1 2))))
          (setf asumdi (f2cl-lib:fref asumi (j) ((1 2))))
          (setf bsumdr (f2cl-lib:fref bsumr (j) ((1 2))))
          (setf bsumdi (f2cl-lib:fref bsumi (j) ((1 2))))
          (setf j (f2cl-lib:int-sub 3 j))
          (go label210)
         label175
          (if (and (= kk n) (< ib n)) (go label210))
          (if (or (= kk ib) (= kk ic)) (go label172))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
              (zunhj znr zni fn 0 tol phidr phidi argdr argdi zet1dr zet1di
               zet2dr zet2di asumdr asumdi bsumdr bsumdi)
            (declare (ignore var-0 var-1 var-2 var-3 var-4))
            (setf phidr var-5)
            (setf phidi var-6)
            (setf argdr var-7)
            (setf argdi var-8)
            (setf zet1dr var-9)
            (setf zet1di var-10)
            (setf zet2dr var-11)
            (setf zet2di var-12)
            (setf asumdr var-13)
            (setf asumdi var-14)
            (setf bsumdr var-15)
            (setf bsumdi var-16))
         label210
          (if (= kode 1) (go label220))
          (setf str (+ zbr zet2dr))
          (setf sti (+ zbi zet2di))
          (setf rast (coerce (realpart (/ fn (zabs str sti))) 'double-float))
          (setf str (* str rast rast))
          (setf sti (* (- sti) rast rast))
          (setf s1r (- str zet1dr))
          (setf s1i (- sti zet1di))
          (go label230)
         label220
          (setf s1r (- zet2dr zet1dr))
          (setf s1i (- zet2di zet1di))
         label230
          (setf rs1 s1r)
          (if (> (abs rs1) elim) (go label280))
          (if (= kdflg 1) (setf iflag 2))
          (if (< (abs rs1) alim) (go label240))
          (setf aphi (coerce (realpart (zabs phidr phidi)) 'double-float))
          (setf aarg (coerce (realpart (zabs argdr argdi)) 'double-float))
          (setf rs1
                  (- (+ rs1 (f2cl-lib:flog aphi))
                     (* 0.25 (f2cl-lib:flog aarg))
                     aic))
          (if (> (abs rs1) elim) (go label280))
          (if (= kdflg 1) (setf iflag 1))
          (if (< rs1 0.0) (go label240))
          (if (= kdflg 1) (setf iflag 3))
         label240
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zairy argdr argdi 0 2 air aii nai idum)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf air var-4)
            (setf aii var-5)
            (setf nai var-6)
            (setf idum var-7))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
              (zairy argdr argdi 1 2 dair daii ndai idum)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf dair var-4)
            (setf daii var-5)
            (setf ndai var-6)
            (setf idum var-7))
          (setf str (- (* dair bsumdr) (* daii bsumdi)))
          (setf sti (+ (* dair bsumdi) (* daii bsumdr)))
          (setf str (+ str (- (* air asumdr) (* aii asumdi))))
          (setf sti (+ sti (+ (* air asumdi) (* aii asumdr))))
          (setf ptr (- (* str phidr) (* sti phidi)))
          (setf pti (+ (* str phidi) (* sti phidr)))
          (setf s2r (- (* ptr csr) (* pti csi)))
          (setf s2i (+ (* ptr csi) (* pti csr)))
          (setf str (* (exp s1r) (f2cl-lib:fref cssr (iflag) ((1 3)))))
          (setf s1r (* str (cos s1i)))
          (setf s1i (* str (sin s1i)))
          (setf str (- (* s2r s1r) (* s2i s1i)))
          (setf s2i (+ (* s2r s1i) (* s2i s1r)))
          (setf s2r str)
          (if (/= iflag 1) (go label250))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (zuchk s2r s2i nw (f2cl-lib:fref bry (1) ((1 3))) tol)
            (declare (ignore var-0 var-1 var-3 var-4))
            (setf nw var-2))
          (if (= nw 0) (go label250))
          (setf s2r zeror)
          (setf s2i zeroi)
         label250
          (if (<= yy 0.0) (setf s2i (- s2i)))
          (setf (f2cl-lib:fref cyr (kdflg) ((1 2))) s2r)
          (setf (f2cl-lib:fref cyi (kdflg) ((1 2))) s2i)
          (setf c2r s2r)
          (setf c2i s2i)
          (setf s2r (* s2r (f2cl-lib:fref csrr (iflag) ((1 3)))))
          (setf s2i (* s2i (f2cl-lib:fref csrr (iflag) ((1 3)))))
          (setf s1r (f2cl-lib:fref yr (kk) ((1 n))))
          (setf s1i (f2cl-lib:fref yi (kk) ((1 n))))
          (if (= kode 1) (go label270))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (zs1s2 zrr zri s1r s1i s2r s2i nw asc alim iuf)
            (declare (ignore var-0 var-1 var-7 var-8))
            (setf s1r var-2)
            (setf s1i var-3)
            (setf s2r var-4)
            (setf s2i var-5)
            (setf nw var-6)
            (setf iuf var-9))
          (setf nz (f2cl-lib:int-add nz nw))
         label270
          (setf (f2cl-lib:fref yr (kk) ((1 n)))
                  (+ (- (* s1r cspnr) (* s1i cspni)) s2r))
          (setf (f2cl-lib:fref yi (kk) ((1 n)))
                  (+ (* s1r cspni) (* s1i cspnr) s2i))
          (setf kk (f2cl-lib:int-sub kk 1))
          (setf cspnr (- cspnr))
          (setf cspni (- cspni))
          (setf str csi)
          (setf csi (- csr))
          (setf csr str)
          (if (or (/= c2r 0.0) (/= c2i 0.0)) (go label255))
          (setf kdflg 1)
          (go label290)
         label255
          (if (= kdflg 2) (go label295))
          (setf kdflg 2)
          (go label290)
         label280
          (if (> rs1 0.0) (go label320))
          (setf s2r zeror)
          (setf s2i zeroi)
          (go label250)
         label290))
      (setf k n)
     label295
      (setf il (f2cl-lib:int-sub n k))
      (if (= il 0) (go end_label))
      (setf s1r (f2cl-lib:fref cyr (1) ((1 2))))
      (setf s1i (f2cl-lib:fref cyi (1) ((1 2))))
      (setf s2r (f2cl-lib:fref cyr (2) ((1 2))))
      (setf s2i (f2cl-lib:fref cyi (2) ((1 2))))
      (setf csr (f2cl-lib:fref csrr (iflag) ((1 3))))
      (setf ascle (f2cl-lib:fref bry (iflag) ((1 3))))
      (setf fn
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add inu il))
                      'double-float))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i il) nil)
        (tagbody
          (setf c2r s2r)
          (setf c2i s2i)
          (setf s2r (+ s1r (* (+ fn fnf) (- (* rzr c2r) (* rzi c2i)))))
          (setf s2i (+ s1i (* (+ fn fnf) (+ (* rzr c2i) (* rzi c2r)))))
          (setf s1r c2r)
          (setf s1i c2i)
          (setf fn (- fn 1.0))
          (setf c2r (* s2r csr))
          (setf c2i (* s2i csr))
          (setf ckr c2r)
          (setf cki c2i)
          (setf c1r (f2cl-lib:fref yr (kk) ((1 n))))
          (setf c1i (f2cl-lib:fref yi (kk) ((1 n))))
          (if (= kode 1) (go label300))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (zs1s2 zrr zri c1r c1i c2r c2i nw asc alim iuf)
            (declare (ignore var-0 var-1 var-7 var-8))
            (setf c1r var-2)
            (setf c1i var-3)
            (setf c2r var-4)
            (setf c2i var-5)
            (setf nw var-6)
            (setf iuf var-9))
          (setf nz (f2cl-lib:int-add nz nw))
         label300
          (setf (f2cl-lib:fref yr (kk) ((1 n)))
                  (+ (- (* c1r cspnr) (* c1i cspni)) c2r))
          (setf (f2cl-lib:fref yi (kk) ((1 n)))
                  (+ (* c1r cspni) (* c1i cspnr) c2i))
          (setf kk (f2cl-lib:int-sub kk 1))
          (setf cspnr (- cspnr))
          (setf cspni (- cspni))
          (if (>= iflag 3) (go label310))
          (setf c2r (abs ckr))
          (setf c2i (abs cki))
          (setf c2m (max c2r c2i))
          (if (<= c2m ascle) (go label310))
          (setf iflag (f2cl-lib:int-add iflag 1))
          (setf ascle (f2cl-lib:fref bry (iflag) ((1 3))))
          (setf s1r (* s1r csr))
          (setf s1i (* s1i csr))
          (setf s2r ckr)
          (setf s2i cki)
          (setf s1r (* s1r (f2cl-lib:fref cssr (iflag) ((1 3)))))
          (setf s1i (* s1i (f2cl-lib:fref cssr (iflag) ((1 3)))))
          (setf s2r (* s2r (f2cl-lib:fref cssr (iflag) ((1 3)))))
          (setf s2i (* s2i (f2cl-lib:fref cssr (iflag) ((1 3)))))
          (setf csr (f2cl-lib:fref csrr (iflag) ((1 3))))
         label310))
      (go end_label)
     label320
      (setf nz -1)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nz nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zunk2 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (double-float))
           :return-values '(nil nil nil nil nil nil nil nil fortran-to-lisp::nz
                            nil nil nil)
           :calls '(fortran-to-lisp::zs1s2 fortran-to-lisp::zuchk
                    fortran-to-lisp::zairy fortran-to-lisp::zabs
                    fortran-to-lisp::zunhj fortran-to-lisp::d1mach))))

