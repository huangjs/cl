;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls nil)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :minpack)


(let ((dmach (make-array 3 :element-type 'double-float)))
  (declare (type (array double-float (3)) dmach))
  (f2cl-lib:fset (f2cl-lib:fref dmach (1) ((1 3))) 2.2204460492599998d-16)
  (f2cl-lib:fset (f2cl-lib:fref dmach (2) ((1 3))) 2.2250738585199997d-308)
  (f2cl-lib:fset (f2cl-lib:fref dmach (3) ((1 3))) 1.79769313485d+308)
  (defun dpmpar (i)
    (declare (type f2cl-lib:integer4 i))
    (prog ((maxmag (make-array 4 :element-type 'f2cl-lib:integer4))
           (minmag (make-array 4 :element-type 'f2cl-lib:integer4))
           (mcheps (make-array 4 :element-type 'f2cl-lib:integer4))
           (dpmpar 0.0d0) (equivalence 0.0))
      (declare (type single-float equivalence)
               (type double-float dpmpar)
               (type (array f2cl-lib:integer4 (4)) mcheps minmag maxmag))
      '"     **********"
      '""
      '"     function dpmpar"
      '""
      '"     this function provides double precision machine parameters"
      '"     when the appropriate set of data statements is activated (by"
      '"     removing the c from column 1) and all other data statements are"
      '"     rendered inactive. most of the parameter values were obtained"
      '"     from the corresponding bell laboratories port library function."
      '""
      '"     the function statement is"
      '""
      '"       double precision function dpmpar(i)"
      '""
      '"     where"
      '""
      '"       i is an integer input variable set to 1, 2, or 3 which"
      '"         selects the desired machine parameter. if the machine has"
      '"         t base b digits and its smallest and largest exponents are"
      '"         emin and emax, respectively, then these parameters are"
      '""
      '"         dpmpar(1) = b**(1 - t), the machine precision,"
      '""
      '"         dpmpar(2) = b**(emin - 1), the smallest magnitude,"
      '""
      '"         dpmpar(3) = b**emax*(1 - b**(-t)), the largest magnitude."
      '""
      '"     argonne national laboratory. minpack project. november 1996."
      '"     burton s. garbow, kenneth e. hillstrom, jorge j. more'"
      '""
      '"     **********"
      '"****NOT TRANSLATED: (EQUIVALENCE (DMACH (1) |,| MCHEPS (1)))"
      '"****NOT TRANSLATED: (EQUIVALENCE (DMACH (2) |,| MINMAG (1)))"
      '"****NOT TRANSLATED: (EQUIVALENCE (DMACH (3) |,| MAXMAG (1)))"
      '""
      '"     machine constants for the ibm 360/370 series,"
      '"     the amdahl 470/v6, the icl 2900, the itel as/6,"
      '"     the xerox sigma 5/7/9 and the sel systems 85/86."
      '""
      '"     data mcheps(1),mcheps(2) / z34100000, z00000000 /"
      '"     data minmag(1),minmag(2) / z00100000, z00000000 /"
      '"     data maxmag(1),maxmag(2) / z7fffffff, zffffffff /"
      '""
      '"     machine constants for the honeywell 600/6000 series."
      '""
      '"     data mcheps(1),mcheps(2) / o606400000000, o000000000000 /"
      '"     data minmag(1),minmag(2) / o402400000000, o000000000000 /"
      '"     data maxmag(1),maxmag(2) / o376777777777, o777777777777 /"
      '""
      '"     machine constants for the cdc 6000/7000 series."
      '""
      '"     data mcheps(1) / 15614000000000000000b /"
      '"     data mcheps(2) / 15010000000000000000b /"
      '""
      '"     data minmag(1) / 00604000000000000000b /"
      '"     data minmag(2) / 00000000000000000000b /"
      '""
      '"     data maxmag(1) / 37767777777777777777b /"
      '"     data maxmag(2) / 37167777777777777777b /"
      '""
      '"     machine constants for the pdp-10 (ka processor)."
      '""
      (f2cl-lib:fortran_comment "     data mcheps(1),mcheps(2) / \\"
                                114400000000
                                |,|
                                |\\|
                                "000000000000 /")
      (f2cl-lib:fortran_comment "     data minmag(1),minmag(2) / \\"
                                33400000000
                                |,|
                                |\\|
                                "000000000000 /")
      (f2cl-lib:fortran_comment "     data maxmag(1),maxmag(2) / \\"
                                377777777777
                                |,|
                                |\\|
                                "344777777777 /")
      '""
      '"     machine constants for the pdp-10 (ki processor)."
      '""
      (f2cl-lib:fortran_comment "     data mcheps(1),mcheps(2) / \\"
                                104400000000
                                |,|
                                |\\|
                                "000000000000 /")
      (f2cl-lib:fortran_comment "     data minmag(1),minmag(2) / \\"
                                400000000
                                |,|
                                |\\|
                                "000000000000 /")
      (f2cl-lib:fortran_comment "     data maxmag(1),maxmag(2) / \\"
                                377777777777
                                |,|
                                |\\|
                                "377777777777 /")
      '""
      '"     machine constants for the pdp-11."
      '""
      '"     data mcheps(1),mcheps(2) /   9472,      0 /"
      '"     data mcheps(3),mcheps(4) /      0,      0 /"
      '""
      '"     data minmag(1),minmag(2) /    128,      0 /"
      '"     data minmag(3),minmag(4) /      0,      0 /"
      '""
      '"     data maxmag(1),maxmag(2) /  32767,     -1 /"
      '"     data maxmag(3),maxmag(4) /     -1,     -1 /"
      '""
      '"     machine constants for the burroughs 6700/7700 systems."
      '""
      '"     data mcheps(1) / o1451000000000000 /"
      '"     data mcheps(2) / o0000000000000000 /"
      '""
      '"     data minmag(1) / o1771000000000000 /"
      '"     data minmag(2) / o7770000000000000 /"
      '""
      '"     data maxmag(1) / o0777777777777777 /"
      '"     data maxmag(2) / o7777777777777777 /"
      '""
      '"     machine constants for the burroughs 5700 system."
      '""
      '"     data mcheps(1) / o1451000000000000 /"
      '"     data mcheps(2) / o0000000000000000 /"
      '""
      '"     data minmag(1) / o1771000000000000 /"
      '"     data minmag(2) / o0000000000000000 /"
      '""
      '"     data maxmag(1) / o0777777777777777 /"
      '"     data maxmag(2) / o0007777777777777 /"
      '""
      '"     machine constants for the burroughs 1700 system."
      '""
      '"     data mcheps(1) / zcc6800000 /"
      '"     data mcheps(2) / z000000000 /"
      '""
      '"     data minmag(1) / zc00800000 /"
      '"     data minmag(2) / z000000000 /"
      '""
      '"     data maxmag(1) / zdffffffff /"
      '"     data maxmag(2) / zfffffffff /"
      '""
      '"     machine constants for the univac 1100 series."
      '""
      '"     data mcheps(1),mcheps(2) / o170640000000, o000000000000 /"
      '"     data minmag(1),minmag(2) / o000040000000, o000000000000 /"
      '"     data maxmag(1),maxmag(2) / o377777777777, o777777777777 /"
      '""
      '"     machine constants for the data general eclipse s/200."
      '""
      '"     note - it may be appropriate to include the following card -"
      '"     static dmach(3)"
      '""
      '"     data minmag/20k,3*0/,maxmag/77777k,3*177777k/"
      '"     data mcheps/32020k,3*0/"
      '""
      '"     machine constants for the harris 220."
      '""
      '"     data mcheps(1),mcheps(2) / '20000000, '00000334 /"
      '"     data minmag(1),minmag(2) / '20000000, '00000201 /"
      '"     data maxmag(1),maxmag(2) / '37777777, '37777577 /"
      '""
      '"     machine constants for the cray-1."
      '""
      '"     data mcheps(1) / 0376424000000000000000b /"
      '"     data mcheps(2) / 0000000000000000000000b /"
      '""
      '"     data minmag(1) / 0200034000000000000000b /"
      '"     data minmag(2) / 0000000000000000000000b /"
      '""
      '"     data maxmag(1) / 0577777777777777777777b /"
      '"     data maxmag(2) / 0000007777777777777776b /"
      '""
      '"     machine constants for the prime 400."
      '""
      '"     data mcheps(1),mcheps(2) / :10000000000, :00000000123 /"
      '"     data minmag(1),minmag(2) / :10000000000, :00000100000 /"
      '"     data maxmag(1),maxmag(2) / :17777777777, :37777677776 /"
      '""
      '"     machine constants for the vax-11."
      '""
      '"     data mcheps(1),mcheps(2) /   9472,  0 /"
      '"     data minmag(1),minmag(2) /    128,  0 /"
      '"     data maxmag(1),maxmag(2) / -32769, -1 /"
      '""
      '"     machine constants for ieee machines."
      '""
      '""
      (setf dpmpar (f2cl-lib:fref dmach (i) ((1 3))))
      (go end_label)
      '""
      '"     last card of function dpmpar."
      '""
     end_label
      (return (values dpmpar nil)))))

