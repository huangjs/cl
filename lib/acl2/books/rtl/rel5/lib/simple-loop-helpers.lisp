(IN-PACKAGE "ACL2")

(set-enforce-redundancy t)

(include-book "rtl")
(include-book "rtlarr")
(include-book "arith")
(include-book "log")
(local (include-book "../support/simple-loop-helpers"))

(set-inhibit-warnings "theory") ; avoid warning in the next event
(local (in-theory nil))
;(set-inhibit-warnings) ; restore theory warnings (optional)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other helpful stuff;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFCONST *EXPT-2-32*
  (EXPT 2 32))

(DEFTHM BITS-31-0
  (IMPLIES (AND (NATP I)
                (< I *EXPT-2-32*))
           (EQUAL (BITS I 31 0)
                  I)))

(DEFTHM BVECP-BITN
  (BVECP (BITN Y I) 1))

(DEFTHM BITN-SETBITN-NOT-EQUAL

; This holds without needing (CASE-SPLIT (BVECP Y 1)).

  (IMPLIES (AND (NOT (EQUAL N K))
                (CASE-SPLIT (< 0 W))
                (CASE-SPLIT (< N W))
                (CASE-SPLIT (< K W))
                (CASE-SPLIT (<= 0 K))
                (CASE-SPLIT (INTEGERP W))
                (CASE-SPLIT (INTEGERP N))
                (<= 0 N)
                (CASE-SPLIT (INTEGERP K)))
           (EQUAL (BITN (SETBITN X W N Y) K)
                  (BITN X K))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic theory for counting up, non-arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ENCAPSULATE
 (($$LOOP_0$ADJ (Y+ I) T)
  ($$LOOP_0$HIGH () T))

 (LOCAL
  (DEFUN $$LOOP_0$HIGH () 3))

 (DEFTHM NATP-$$LOOP_0$HIGH
   (AND (INTEGERP ($$LOOP_0$HIGH))
        (<= 0 ($$LOOP_0$HIGH)))
   :RULE-CLASSES :TYPE-PRESCRIPTION)

 (LOCAL
  (DEFUN $$LOOP_0$ADJ (Y+ I)
    (DECLARE (IGNORE I))
    Y+))

 (DEFTHM BITN-$$LOOP_0$ADJ ; $$LOOP_0-ADJ-PASS-THROUGH
   (IMPLIES (AND (NATP I)
                 (NATP J)
                 (NOT (EQUAL I J))
                 (<= I ($$LOOP_0$HIGH))
                 (<= J ($$LOOP_0$HIGH)))
            (EQUAL (BITN ($$LOOP_0$ADJ Y+ I) J)
                   (BITN Y+ J)))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_0$ADJ))))

 (DEFTHM BITN-$$LOOP_0$ADJ-$$LOOP_0$ADJ ; $$LOOP_0-ADJ-ABSORB-UNDER
   (IMPLIES (AND (NATP I)
                 (<= I ($$LOOP_0$HIGH))
                 (NATP J)
                 (<= J ($$LOOP_0$HIGH))
                 (<= I J))
            (EQUAL (BITN ($$LOOP_0$ADJ ($$LOOP_0$ADJ Y+ I)
                                       J)
                         J)
                   (BITN ($$LOOP_0$ADJ Y+ J) J)))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_0$ADJ)))))

(DEFUN $$LOOP_0 (Y+ I)
  (DECLARE (XARGS :MEASURE (NFIX (- (1+ ($$LOOP_0$HIGH)) I))))
  (IF (AND (NATP I) (<= I ($$LOOP_0$HIGH)))
      ($$LOOP_0 ($$LOOP_0$ADJ Y+ I)
                (+ I 1))
      Y+))

(DEFTHM BITN-$$LOOP_0
  (IMPLIES (AND (NATP I)
                (NATP J)
                (<= I ($$LOOP_0$HIGH))
                (<= J ($$LOOP_0$HIGH)))
           (EQUAL (BITN ($$LOOP_0 Y+ I) J)
                  (IF (<= I J)
                      (BITN ($$LOOP_0$ADJ Y+ J) J)
                      (BITN Y+ J)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic theory for counting down, non-arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ENCAPSULATE
 (($$LOOP_1$ADJ (Y+ I) T)
  ($$LOOP_1$LOW () T)
  ($$LOOP_1$HIGH () t))

 (LOCAL
  (DEFUN $$LOOP_1$LOW () 2))

 (LOCAL
  (DEFUN $$LOOP_1$HIGH () 4))

 (DEFTHM NATP-$$LOOP_1$LOW
   (AND (INTEGERP ($$LOOP_1$LOW))
        (<= 0 ($$LOOP_1$LOW)))
   :RULE-CLASSES :TYPE-PRESCRIPTION)

 (LOCAL
  (DEFUN $$LOOP_1$ADJ (Y+ I)
    (DECLARE (IGNORE I))
    Y+))

 (DEFTHM BITN-$$LOOP_1$ADJ ; $$LOOP_1-ADJ-PASS-THROUGH
   (IMPLIES (AND (NATP I)
                 (NATP J)
                 (NOT (EQUAL I J))
                 (>= I ($$LOOP_1$LOW))
                 (>= J ($$LOOP_1$LOW))
                 (< I ($$LOOP_1$HIGH))
                 (< J ($$LOOP_1$HIGH)))
            (EQUAL (BITN ($$LOOP_1$ADJ Y+ I) J)
                   (BITN Y+ J)))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_1$ADJ))))

 (DEFTHM BITN-$$LOOP_1$ADJ-$$LOOP_1$ADJ ; $$LOOP_1-ADJ-ABSORB-UNDER
   (IMPLIES (AND (NATP I)
                 (>= I ($$LOOP_1$low))
                 (NATP J)
                 (>= J ($$LOOP_1$LOW))
                 (< I ($$LOOP_1$HIGH))
                 (< J ($$LOOP_1$HIGH))
                 (<= J I))
            (EQUAL (BITN ($$LOOP_1$ADJ ($$LOOP_1$ADJ Y+ I)
                                       J)
                         J)
                   (BITN ($$LOOP_1$ADJ Y+ J) J)))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_1$ADJ)))))

(DEFUN $$LOOP_1 (Y+ I)
  (DECLARE (XARGS :MEASURE (NFIX (1+ I))))
  (IF (AND (NATP I) (>= I ($$LOOP_1$LOW)))
      ($$LOOP_1 ($$LOOP_1$ADJ Y+ I)
                (- I 1))
      Y+))

(DEFTHM BITN-$$LOOP_1
  (IMPLIES (AND (NATP I)
                (NATP J)
                (>= I ($$LOOP_1$LOW))
                (>= J ($$LOOP_1$LOW))
                (< I ($$LOOP_1$HIGH))
                (< J ($$LOOP_1$HIGH)))
           (EQUAL (BITN ($$LOOP_1 Y+ I) J)
                  (IF (>= I J)
                      (BITN ($$LOOP_1$ADJ Y+ J) J)
                      (BITN Y+ J)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic theory for counting up, arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ENCAPSULATE
 (($$LOOP_2$ADJ (Y+ I) T)
  ($$LOOP_2$HIGH () T))

 (LOCAL
  (DEFUN $$LOOP_2$HIGH () 3))

 (DEFTHM NATP-$$LOOP_2$HIGH
   (AND (INTEGERP ($$LOOP_2$HIGH))
        (<= 0 ($$LOOP_2$HIGH)))
   :RULE-CLASSES :TYPE-PRESCRIPTION)

 (LOCAL
  (DEFUN $$LOOP_2$ADJ (Y+ I)
    (DECLARE (IGNORE I))
    Y+))

 (DEFTHM AG-$$LOOP_2$ADJ ; $$LOOP_2-ADJ-PASS-THROUGH
   (IMPLIES (AND (NATP I)
                 (NATP J)
                 (NOT (EQUAL I J))
                 (<= I ($$LOOP_2$HIGH))
                 (<= J ($$LOOP_2$HIGH)))
            (EQUAL (AG J ($$LOOP_2$ADJ Y+ I))
                   (AG J Y+)))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_2$ADJ))))

 (DEFTHM AG-$$LOOP_2$ADJ-$$LOOP_2$ADJ ; $$LOOP_2-ADJ-ABSORB-UNDER
   (IMPLIES (AND (NATP I)
                 (<= I ($$LOOP_2$HIGH))
                 (NATP J)
                 (<= J ($$LOOP_2$HIGH))
                 (<= I J))
            (EQUAL (AG J ($$LOOP_2$ADJ ($$LOOP_2$ADJ Y+ I)
                                       J))
                   (AG J ($$LOOP_2$ADJ Y+ J))))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_2$ADJ)))))

(DEFUN $$LOOP_2 (Y+ I)
  (DECLARE (XARGS :MEASURE (NFIX (- (1+ ($$LOOP_2$HIGH)) I))))
  (IF (AND (NATP I) (<= I ($$LOOP_2$HIGH)))
      ($$LOOP_2 ($$LOOP_2$ADJ Y+ I)
                (+ I 1))
      Y+))

(DEFTHM AG-$$LOOP_2
  (IMPLIES (AND (NATP I)
                (NATP J)
                (<= I ($$LOOP_2$HIGH))
                (<= J ($$LOOP_2$HIGH)))
           (EQUAL (AG J ($$LOOP_2 Y+ I))
                  (IF (<= I J)
                      (AG J ($$LOOP_2$ADJ Y+ J))
                      (AG J Y+)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic theory for counting down, arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ENCAPSULATE
 (($$LOOP_3$ADJ (Y+ I) T)
  ($$LOOP_3$LOW () T)
  ($$LOOP_3$HIGH () t))

 (LOCAL
  (DEFUN $$LOOP_3$LOW () 2))

 (LOCAL
  (DEFUN $$LOOP_3$HIGH () 4))

 (DEFTHM NATP-$$LOOP_3$LOW
   (AND (INTEGERP ($$LOOP_3$LOW))
        (<= 0 ($$LOOP_3$LOW)))
   :RULE-CLASSES :TYPE-PRESCRIPTION)

 (LOCAL
  (DEFUN $$LOOP_3$ADJ (Y+ I)
    (DECLARE (IGNORE I))
    Y+))

 (DEFTHM AG-$$LOOP_3$ADJ
   (IMPLIES (AND (NATP I)
                 (NATP J)
                 (NOT (EQUAL I J))
                 (>= I ($$LOOP_3$LOW))
                 (>= J ($$LOOP_3$LOW))
                 (< I ($$LOOP_3$HIGH))
                 (< J ($$LOOP_3$HIGH)))
            (EQUAL (AG J ($$LOOP_3$ADJ Y+ I))
                   (AG J Y+)))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_3$ADJ))))

 (DEFTHM AG-$$LOOP_3$ADJ-$$LOOP_3$ADJ
   (IMPLIES (AND (NATP I)
                 (>= I ($$LOOP_3$low))
                 (NATP J)
                 (>= J ($$LOOP_3$low))
                 (< I ($$LOOP_3$HIGH))
                 (< J ($$LOOP_3$HIGH))
                 (<= J I))
            (EQUAL (AG J ($$LOOP_3$ADJ ($$LOOP_3$ADJ Y+ I)
                                       J))
                   (AG J ($$LOOP_3$ADJ Y+ J))))
   :HINTS (("Goal" :IN-THEORY (ENABLE $$LOOP_3$ADJ)))))

(DEFUN $$LOOP_3 (Y+ I)
  (DECLARE (XARGS :MEASURE (NFIX (1+ I))))
  (IF (AND (NATP I) (>= I ($$LOOP_3$LOW)))
      ($$LOOP_3 ($$LOOP_3$ADJ Y+ I)
                (- I 1))
      Y+))

(DEFTHM AG-$$LOOP_3
  (IMPLIES (AND (NATP I)
                (NATP J)
                (>= I ($$LOOP_3$LOW))
                (>= J ($$LOOP_3$LOW))
                (< I ($$LOOP_3$HIGH))
                (< J ($$LOOP_3$HIGH)))
           (EQUAL (AG J ($$LOOP_3 Y+ I))
                  (IF (>= I J)
                      (AG J ($$LOOP_3$ADJ Y+ J))
                      (AG J Y+)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellany
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;not in support/simple-loop-helpers since would be redefined here (which is illegal)

(deftheory simple-loop-thy-0
  (union-theories '(if1) (theory 'minimal-theory)))

(deftheory simple-loop-thy-1
  (union-theories
   '(bitn-setbitn-not-equal
     ag-diff-as
     bits-31-0
     natp)
   (theory 'simple-loop-thy-0)))

(in-theory (enable setbits))
