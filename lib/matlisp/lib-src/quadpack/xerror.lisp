;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerror (messg nmessg nerr level)
  (declare (type f2cl-lib:integer4 level nerr nmessg)
   (type (simple-array base-char (*)) messg))
  (f2cl-lib:with-multi-array-data
      ((messg base-char messg-%data% messg-%offset%))
    (prog ()
      (declare)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (xerrwv messg nmessg nerr level 0 0 0 0 0.0 0.0)
        (declare (ignore var-4 var-5 var-6 var-7 var-8 var-9))
        (setf messg var-0)
        (setf nmessg var-1)
        (setf nerr var-2)
        (setf level var-3))
      (go end_label)
     end_label
      (return (values messg nmessg nerr level)))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerrwv (messg nmessg nerr level ni i1 i2 nr r1 r2)
  (declare (type single-float r2 r1)
   (type f2cl-lib:integer4 nr i2 i1 ni level nerr nmessg)
   (type (simple-array base-char (*)) messg))
  (f2cl-lib:with-multi-array-data
      ((messg base-char messg-%data% messg-%offset%))
    (prog ((lun (make-array 5 :element-type 'f2cl-lib:integer4))
           (form
            (make-array '(37)
                        :element-type
                        'base-char
                        :initial-element
                        #\Space))
           (lfirst
            (make-array '(20)
                        :element-type
                        'base-char
                        :initial-element
                        #\Space))
           (ifatal 0) (i 0) (iunit 0) (kunit 0) (isizef 0) (isizei 0) (nunit 0)
           (mkntrl 0) (llevel 0) (lerr 0) (lmessg 0) (kount 0) (junk 0)
           (kdummy 0) (maxmes 0) (lkntrl 0))
      (declare
       (type f2cl-lib:integer4 lkntrl maxmes kdummy junk kount lmessg lerr
        llevel mkntrl nunit isizei isizef kunit iunit i ifatal)
       (type (simple-array base-char (20)) lfirst)
       (type (simple-array base-char (37)) form)
       (type (array f2cl-lib:integer4 (5)) lun))
      (setf lkntrl (j4save 2 0 f2cl-lib:%false%))
      (setf maxmes (j4save 4 0 f2cl-lib:%false%))
      (if (and (> nmessg 0) (/= nerr 0) (>= level -1) (<= level 2))
          (go label10))
      (if (> lkntrl 0) (xerprt "FATAL ERROR IN..." 17))
      (xerprt "XERROR -- INVALID INPUT" 23)
      (if (> lkntrl 0) (fdump))
      (if (> lkntrl 0) (xerprt "JOB ABORT DUE TO FATAL ERROR." 29))
      (if (> lkntrl 0)
          (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4)
              (xersav " " 0 0 0 kdummy)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf kdummy var-4)))
      (xerabt "XERROR -- INVALID INPUT" 23)
      (go end_label)
     label10
      (setf junk (j4save 1 nerr f2cl-lib:%true%))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (xersav messg nmessg nerr level kount)
        (declare (ignore))
        (setf messg var-0)
        (setf nmessg var-1)
        (setf nerr var-2)
        (setf level var-3)
        (setf kount var-4))
      (f2cl-lib:f2cl-set-string lfirst messg (string 20))
      (setf lmessg nmessg)
      (setf lerr nerr)
      (setf llevel level)
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (xerctl lfirst lmessg lerr llevel lkntrl)
        (declare (ignore var-0 var-4))
        (setf lmessg var-1)
        (setf lerr var-2)
        (setf llevel var-3))
      (setf lmessg nmessg)
      (setf lerr nerr)
      (setf llevel level)
      (setf lkntrl (f2cl-lib:max0 -2 (f2cl-lib:min0 2 lkntrl)))
      (setf mkntrl (f2cl-lib:iabs lkntrl))
      (if (and (< llevel 2) (= lkntrl 0)) (go label100))
      (if
       (or (and (= llevel -1) (> kount (f2cl-lib:min0 1 maxmes)))
           (and (= llevel 0) (> kount maxmes))
           (and (= llevel 1) (> kount maxmes) (= mkntrl 1))
           (and (= llevel 2) (> kount (f2cl-lib:max0 1 maxmes))))
       (go label100))
      (if (<= lkntrl 0) (go label20))
      (xerprt " " 1)
      (if (= llevel -1)
          (xerprt "WARNING MESSAGE...THIS MESSAGE WILL ONLY BE PRINTED ONCE."
           57))
      (if (= llevel 0) (xerprt "WARNING IN..." 13))
      (if (= llevel 1) (xerprt "RECOVERABLE ERROR IN..." 23))
      (if (= llevel 2) (xerprt "FATAL ERROR IN..." 17))
     label20
      (multiple-value-bind
          (var-0 var-1)
          (xerprt messg lmessg)
        (declare (ignore))
        (setf messg var-0)
        (setf lmessg var-1))
      (multiple-value-bind
          (var-0 var-1)
          (xgetua lun nunit)
        (declare (ignore var-0))
        (setf nunit var-1))
      (setf isizei
              (f2cl-lib:int
               (+ (f2cl-lib:log10 (f2cl-lib:ffloat (f2cl-lib:i1mach 9))) 1.0)))
      (setf isizef
              (f2cl-lib:int
               (+
                (f2cl-lib:log10
                 (expt (f2cl-lib:ffloat (f2cl-lib:i1mach 10))
                       (f2cl-lib:i1mach 11)))
                1.0)))
      (f2cl-lib:fdo (kunit 1 (f2cl-lib:int-add kunit 1))
                    ((> kunit nunit) nil)
        (tagbody
          (setf iunit (f2cl-lib:fref lun (kunit) ((1 5))))
          (if (= iunit 0) (setf iunit (f2cl-lib:i1mach 4)))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i
                            (min (the f2cl-lib:integer4 ni)
                                 (the f2cl-lib:integer4 2)))
                         nil)
            (tagbody
              (f2cl-lib:fformat form
                                ("(11X,21HIN ABOVE MESSAGE, I" 1 (("~1D"))
                                 "=,I" 1 (("~2D")) ")   " "~%")
                                i
                                isizei)
              (if (= i 1) (f2cl-lib:fformat iunit (("~A~%")) i1))
              (if (= i 2) (f2cl-lib:fformat iunit (("~A~%")) i2))
             label22))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i
                            (min (the f2cl-lib:integer4 nr)
                                 (the f2cl-lib:integer4 2)))
                         nil)
            (tagbody
              (f2cl-lib:fformat form
                                ("(11X,21HIN ABOVE MESSAGE, R" 1 (("~1D"))
                                 "=,E" 1 (("~2D")) "." 1 (("~2D")) ")" "~%")
                                i
                                (f2cl-lib:int-add isizef 10)
                                isizef)
              (if (= i 1) (f2cl-lib:fformat iunit (("~A~%")) r1))
              (if (= i 2) (f2cl-lib:fformat iunit (("~A~%")) r2))
             label24))
          (if (<= lkntrl 0) (go label40))
          (f2cl-lib:fformat iunit (" ERROR NUMBER =" 1 (("~10D")) "~%") lerr)
         label40
         label50))
      (if (> lkntrl 0) (fdump))
     label100
      (setf ifatal 0)
      (if (or (= llevel 2) (and (= llevel 1) (= mkntrl 2))) (setf ifatal 1))
      (if (<= ifatal 0) (go end_label))
      (if (or (<= lkntrl 0) (> kount (f2cl-lib:max0 1 maxmes))) (go label120))
      (if (= llevel 1) (xerprt "JOB ABORT DUE TO UNRECOVERED ERROR." 35))
      (if (= llevel 2) (xerprt "JOB ABORT DUE TO FATAL ERROR." 29))
      (multiple-value-bind
          (var-0 var-1 var-2 var-3 var-4)
          (xersav " " -1 0 0 kdummy)
        (declare (ignore var-0 var-1 var-2 var-3))
        (setf kdummy var-4))
     label120
      (if (and (= llevel 2) (> kount (f2cl-lib:max0 1 maxmes)))
          (setf lmessg 0))
      (multiple-value-bind
          (var-0 var-1)
          (xerabt messg lmessg)
        (declare (ignore))
        (setf messg var-0)
        (setf lmessg var-1))
      (go end_label)
     end_label
      (return (values messg nmessg nerr level nil nil nil nil nil nil)))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(let ((mestab (f2cl-lib:f2cl-init-string (10) (20)))
      (nertab (make-array 10 :element-type 'f2cl-lib:integer4))
      (levtab (make-array 10 :element-type 'f2cl-lib:integer4))
      (kount (make-array 10 :element-type 'f2cl-lib:integer4))
      (kountx 0))
  (declare (type f2cl-lib:integer4 kountx)
   (type (array f2cl-lib:integer4 (10)) kount levtab nertab)
   (type (array (simple-array base-char (20)) (10)) mestab))
  (f2cl-lib:fset (f2cl-lib:fref kount (1) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (2) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (3) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (4) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (5) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (6) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (7) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (8) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (9) ((1 10))) 0)
  (f2cl-lib:fset (f2cl-lib:fref kount (10) ((1 10))) 0)
  (setq kountx 0)
  (defun xersav (messg nmessg nerr level icount)
    (declare (type f2cl-lib:integer4 icount level nerr nmessg)
     (type (simple-array base-char (*)) messg))
    (f2cl-lib:with-multi-array-data
        ((messg base-char messg-%data% messg-%offset%))
      (prog ((mes
              (make-array '(20)
                          :element-type
                          'base-char
                          :initial-element
                          #\Space))
             (lun (make-array 5 :element-type 'f2cl-lib:integer4)) (ii 0) (i 0)
             (iunit 0) (kunit 0) (nunit 0))
        (declare (type f2cl-lib:integer4 nunit kunit iunit i ii)
         (type (array f2cl-lib:integer4 (5)) lun)
         (type (simple-array base-char (20)) mes))
        (if (> nmessg 0) (go label80))
        (if (= (f2cl-lib:fref kount (1) ((1 10))) 0) (go end_label))
        (multiple-value-bind
            (var-0 var-1)
            (xgetua lun nunit)
          (declare (ignore var-0))
          (setf nunit var-1))
        (f2cl-lib:fdo (kunit 1 (f2cl-lib:int-add kunit 1))
                      ((> kunit nunit) nil)
          (tagbody
            (setf iunit (f2cl-lib:fref lun (kunit) ((1 5))))
            (if (= iunit 0) (setf iunit (f2cl-lib:i1mach 4)))
            (f2cl-lib:fformat iunit
                              ("0          ERROR MESSAGE SUMMARY" "~%"
                               " MESSAGE START             NERR     LEVEL     COUNT"
                               "~%")
                              nil)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 10) nil)
              (tagbody
                (if (= (f2cl-lib:fref kount (i) ((1 10))) 0) (go label30))
                (f2cl-lib:fformat iunit
                                  ("~1@T" 1 (("~20A")) 3 (("~10D")) "~%")
                                  (f2cl-lib:fref mestab (i) ((1 10)))
                                  (f2cl-lib:fref nertab (i) ((1 10)))
                                  (f2cl-lib:fref levtab (i) ((1 10)))
                                  (f2cl-lib:fref kount (i) ((1 10))))
               label20))
           label30
            (if (/= kountx 0)
                (f2cl-lib:fformat iunit
                                  ("0OTHER ERRORS NOT INDIVIDUALLY TABULATED="
                                   1 (("~10D")) "~%")
                                  kountx))
            (f2cl-lib:fformat iunit ("~1@T" "~%") nil)
           label60))
        (if (< nmessg 0) (go end_label))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 10) nil)
          (tagbody
           label70
            (f2cl-lib:fset (f2cl-lib:fref kount (i) ((1 10))) 0)))
        (setf kountx 0)
        (go end_label)
       label80
        (f2cl-lib:f2cl-set-string mes messg (string 20))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i 10) nil)
          (tagbody
            (setf ii i)
            (if (= (f2cl-lib:fref kount (i) ((1 10))) 0) (go label110))
            (if (f2cl-lib:fstring-/= mes (f2cl-lib:fref mestab (i) ((1 10))))
                (go label90))
            (if (/= nerr (f2cl-lib:fref nertab (i) ((1 10)))) (go label90))
            (if (/= level (f2cl-lib:fref levtab (i) ((1 10)))) (go label90))
            (go label100)
           label90))
        (setf kountx (f2cl-lib:int-add kountx 1))
        (setf icount 1)
        (go end_label)
       label100
        (f2cl-lib:fset (f2cl-lib:fref kount (ii) ((1 10)))
                       (f2cl-lib:int-add (f2cl-lib:fref kount (ii) ((1 10)))
                                         1))
        (setf icount (f2cl-lib:fref kount (ii) ((1 10))))
        (go end_label)
       label110
        (f2cl-lib:f2cl-set-string (f2cl-lib:fref mestab (ii) ((1 10)))
                                  mes
                                  (string 20))
        (f2cl-lib:fset (f2cl-lib:fref nertab (ii) ((1 10))) nerr)
        (f2cl-lib:fset (f2cl-lib:fref levtab (ii) ((1 10))) level)
        (f2cl-lib:fset (f2cl-lib:fref kount (ii) ((1 10))) 1)
        (setf icount 1)
        (go end_label)
       end_label
        (return (values messg nmessg nerr level icount))))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xgetua (iunita n)
  (declare (type f2cl-lib:integer4 n)
   (type (array f2cl-lib:integer4 (*)) iunita))
  (f2cl-lib:with-multi-array-data
      ((iunita f2cl-lib:integer4 iunita-%data% iunita-%offset%))
    (prog ((f2cl-lib:index 0) (i 0))
      (declare (type f2cl-lib:integer4 i f2cl-lib:index))
      (setf n (j4save 5 0 f2cl-lib:%false%))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf f2cl-lib:index (f2cl-lib:int-add i 4))
          (if (= i 1) (setf f2cl-lib:index 3))
          (f2cl-lib:fset
           (f2cl-lib:fref iunita-%data% (i) ((1 5)) iunita-%offset%)
           (j4save f2cl-lib:index 0 f2cl-lib:%false%))
         label30))
      (go end_label)
     end_label
      (return (values nil n)))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun fdump ()
  (f2cl-lib:with-multi-array-data
      nil
    (prog () (declare) (go end_label) end_label (return (values)))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(let ((iparam (make-array 9 :element-type 'f2cl-lib:integer4)))
  (declare (type (array f2cl-lib:integer4 (9)) iparam))
  (f2cl-lib:fset (f2cl-lib:fref iparam (1) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (2) ((1 9))) 2)
  (f2cl-lib:fset (f2cl-lib:fref iparam (3) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (4) ((1 9))) 10)
  (f2cl-lib:fset (f2cl-lib:fref iparam (5) ((1 9))) 1)
  (f2cl-lib:fset (f2cl-lib:fref iparam (6) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (7) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (8) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (9) ((1 9))) 0)
  (defun j4save (iwhich ivalue iset)
    (declare (type f2cl-lib:logical iset)
     (type f2cl-lib:integer4 ivalue iwhich))
    (f2cl-lib:with-multi-array-data
        nil
      (prog ((j4save 0))
        (declare (type f2cl-lib:integer4 j4save))
        (setf j4save (f2cl-lib:fref iparam (iwhich) ((1 9))))
        (if iset
            (f2cl-lib:fset (f2cl-lib:fref iparam (iwhich) ((1 9))) ivalue))
        (go end_label)
       end_label
        (return (values j4save nil nil nil))))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerabt (messg nmessg)
  (declare (type f2cl-lib:integer4 nmessg)
   (type (simple-array base-char (*)) messg))
  (f2cl-lib:with-multi-array-data
      ((messg base-char messg-%data% messg-%offset%))
    (prog () (declare) end_label (return (values messg nmessg)))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerctl (messg1 nmessg nerr level kontrl)
  (declare (type f2cl-lib:integer4 kontrl level nerr nmessg)
   (type (simple-array base-char (*)) messg1))
  (f2cl-lib:with-multi-array-data
      ((messg1 base-char messg1-%data% messg1-%offset%))
    (prog ()
      (declare)
      (go end_label)
     end_label
      (return (values nil nmessg nerr level nil)))))

;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "QUADPACK")


(defun xerprt (messg nmessg)
  (declare (type f2cl-lib:integer4 nmessg)
   (type (simple-array base-char (*)) messg))
  (f2cl-lib:with-multi-array-data
      ((messg base-char messg-%data% messg-%offset%))
    (prog ((lun (make-array 5 :element-type 'f2cl-lib:integer4)) (last$ 0)
           (f2cl-lib:ichar 0) (iunit 0) (kunit 0) (lenmes 0) (nunit 0))
      (declare
       (type f2cl-lib:integer4 nunit lenmes kunit iunit f2cl-lib:ichar last$)
       (type (array f2cl-lib:integer4 (5)) lun))
      (multiple-value-bind
          (var-0 var-1)
          (xgetua lun nunit)
        (declare (ignore var-0))
        (setf nunit var-1))
      (setf lenmes (f2cl-lib:len messg))
      (f2cl-lib:fdo (kunit 1 (f2cl-lib:int-add kunit 1))
                    ((> kunit nunit) nil)
        (tagbody
          (setf iunit (f2cl-lib:fref lun (kunit) ((1 5))))
          (if (= iunit 0) (setf iunit (f2cl-lib:i1mach 4)))
          (f2cl-lib:fdo (f2cl-lib:ichar 1 (f2cl-lib:int-add f2cl-lib:ichar 72))
                        ((> f2cl-lib:ichar lenmes) nil)
            (tagbody
              (setf last$
                      (f2cl-lib:min0 (f2cl-lib:int-add f2cl-lib:ichar 71)
                                     lenmes))
             label
              (f2cl-lib:fformat iunit
                                ("~1@T" ("~A") "~%")
                                (f2cl-lib:fref-string messg
                                                      (f2cl-lib:ichar last$)))
             label10))
         label20))
      (go end_label)
     end_label
      (return (values messg nmessg)))))

