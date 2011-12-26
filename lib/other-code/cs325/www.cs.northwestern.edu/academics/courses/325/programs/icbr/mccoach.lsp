;;; *********************************************************************
;;; Copyright (c) 1989, Lawrence Erlbaum Assoc. All rights reserved.
;;; 
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS. The author makes no warranty
;;; about the software, its performance or its conformity to any
;;; specification.
;;; *********************************************************************


(DEFUN COACH (SLOTS)
 (LET ((INSTANCE (SLOTS->MOP SLOTS '(M-STRATEGY) T)))
  (AND (GET-FILLER 'NEW-SOLUTION INSTANCE)
       INSTANCE)))

(DEFUN COMPARE-CONSTRAINT (CONSTRAINT FILLER SLOTS)
 (FUNCALL (GET-FILLER 'COMPARE-FN CONSTRAINT)
          (INDIRECT-FILLER 'ROLE CONSTRAINT FILLER)
          (INDIRECT-FILLER 'TO CONSTRAINT SLOTS)))

(DEFUN INDIRECT-FILLER (ROLE MOP SLOTS)
 (LET ((PATH-MOP (GET-FILLER ROLE MOP)))
  (PATH-FILLER (COND ((NULL PATH-MOP) NIL)
                     ((ABSTP 'M-PATH PATH-MOP)
                      (GROUP->LIST PATH-MOP))
                     (T (LIST PATH-MOP)))
               SLOTS)))

(DEFUN NOT-EQL (X Y) (AND X Y (NOT (EQL X Y))))

(DEFUN REPAIR-1-EXCLUSION-P (CONSTRAINT FILLER SLOTS)
 (DECLARE (IGNORE CONSTRAINT))
 (LET ((DOWN-1
         (PATH-FILLER '(EXPLANATION CAUSE) FILLER))
       (DOWN-2
         (PATH-FILLER '(REPAIR-1 EXPLANATION CAUSE)
                       SLOTS)))
  (FORMAT T "~&----------------")
  (FORMAT T "~&Checking for mutual exclusions")
  (FORMAT T "~&  between ~S and ~S" DOWN-1 DOWN-2)
  (FOR (SLOT-1 :IN (MOP-SLOTS DOWN-1))
       (SLOT-2 :IN (MOP-SLOTS DOWN-2))
     :FIRST (CYCLE-EXCLUSION-P
              (GROUP->LIST (SLOT-FILLER SLOT-1))
              (GROUP->LIST (SLOT-FILLER SLOT-2))))))

(DEFUN CYCLE-EXCLUSION-P (CYCLE-1 CYCLE-2)
 (FOR (EVENT-1 :IN CYCLE-1)
    :FIRST
      (FOR (EVENT-2 :IN CYCLE-2)
         :FIRST (EVENT-EXCLUSION-P EVENT-1 EVENT-2))))

(DEFUN EVENT-EXCLUSION-P (EVENT-1 EVENT-2)
 (LET ((ACTOR (GET-FILLER 'ACTOR EVENT-1)))
   (AND (ABSTP 'M-DEFENSIVE-PLAYER ACTOR)
        (EQL (GET-FILLER 'ACTOR EVENT-2) ACTOR)
        (SLOTS->MOP `((THIS-EVENT ,EVENT-1)
                      (THAT-EVENT ,EVENT-2))
                    '(M-EXCLUSION)
                    NIL)
        (PROGN (FORMAT T "~&~S excludes ~S"
                         EVENT-1 EVENT-2)
               T))))

(DEFUN ANDP (CONSTRAINT FILLER SLOTS)
 (FOR (SLOT :IN
        (MOP-SLOTS (ROLE-FILLER 'OBJECT CONSTRAINT)))
    :ALWAYS (SATISFIEDP (SLOT-FILLER SLOT)
                        FILLER SLOTS)))

(DEFUN GET-DETECTED-EVENT (PATTERN MOP)
 (FORMAT T "~&----------------")
 (LET ((CAUSE (PATH-FILLER '(EXPLANATION CAUSE)
                (INDIRECT-FILLER 'ROLE PATTERN MOP))))
  (FORMAT T "~&Looking for a detected event in ~S"
             CAUSE)
  (LET ((EVENT
          (FOR (SLOT :IN (MOP-SLOTS CAUSE))
             :FIRST (FOR (SUBSLOT :IN
                           (MOP-SLOTS
                             (SLOT-FILLER SLOT)))
                       :FIRST (DEFENSIVE-DETECT-INFO
                                (SLOT-FILLER SUBSLOT))))))
   (AND EVENT
        (PROGN (FORMAT T "~&Detected event found")
               EVENT)))))

(DEFUN DEFENSIVE-DETECT-INFO (EVENT)
 (AND (ABSTP 'M-DETECT EVENT)
      (ABSTP 'M-DEFENSIVE-PLAYER
             (ROLE-FILLER 'ACTOR EVENT))
      (ROLE-FILLER 'INFO EVENT)))

(DEFUN DELAY-STEP (STEP PLAY)
 (COND ((NULL PLAY) NIL)
       ((EQL (CAR PLAY) STEP)
        (DELAYED-INSERT STEP (CDR PLAY)))
       (T (CONS (CAR PLAY)
                (DELAY-STEP STEP (CDR PLAY))))))

(DEFUN DELAYED-INSERT (STEP PLAY)
 (OR (AND (OR (NULL PLAY)
              (ENABLESP STEP (CAR PLAY)))
          (CONS STEP PLAY))
     (CONS (CAR PLAY)
           (DELAYED-INSERT STEP (CDR PLAY)))))

(DEFUN ENABLESP (EVENT NEXT-EVENT)
 (SLOTS->MOP `((ANTE ,EVENT) (CNSQ ,NEXT-EVENT))
             '(M-ENABLE) NIL))

(DEFUN MERGE-PLAYS (PLAY-1 PLAY-2 STEP)
 (FORMAT T "~&----------------")
 (FORMAT T "~&Merging plays")
 (LIST->GROUP
   (DELAY-STEP STEP
     (MERGE-STEPS (GROUP->LIST PLAY-1)
                  (GROUP->LIST PLAY-2)))))

(DEFUN MERGE-STEPS (STEPS-1 STEPS-2)
 (LET ((NEW-STEPS (REMOVE-GOAL-STEP STEPS-1)))
  (APPEND NEW-STEPS
          (FOR (STEP :IN STEPS-2)
             :WHEN (NOT (MEMBER STEP NEW-STEPS))
             :SAVE STEP))))

(DEFUN REMOVE-GOAL-STEP (STEPS)
 (FOR (STEP :IN STEPS)
    :WHEN (NOT (ABSTP 'M-RUN-TO-GOAL STEP))
    :SAVE STEP))

(DEFUN DELAYABLEP (EVENT DOWN)
 (FORMAT T "~&----------------")
 (FORMAT T "~&Seeing if ~S can be delayed" EVENT)
 (COND ((FOR (NEXT-EVENT :IN (NEXT-EVENTS EVENT DOWN))
           :ALWAYS (NOT (ENABLESP EVENT NEXT-EVENT)))
        (FORMAT T "~&~S can be delayed" EVENT)
        T) 
       (T (FORMAT T "~&~S can not be delayed" EVENT)
          NIL)))

(DEFUN NEXT-EVENTS (EVENT DOWN)
 (CDR
  (FOR (SLOT :IN (MOP-SLOTS DOWN))
     :FIRST (MEMBER EVENT
              (GROUP->LIST (SLOT-FILLER SLOT))))))

(DEFUN MAKE-DECEPTION (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (FORMAT T "~&----------------")
 (FORMAT T "~&Trying M-DECEIVE")
 (OR (CALC-DECEPTION
       'DETECTED-EVENT-1 'REPAIR-1 'REPAIR-2 MOP)
     (CALC-DECEPTION
       'DETECTED-EVENT-2 'REPAIR-2 'REPAIR-1 MOP)))

(DEFUN CALC-DECEPTION (DETECTED-ROLE ROLE-1 ROLE-2 MOP)
 (LET ((EVENT (GET-FILLER DETECTED-ROLE MOP))
       (REPAIR-1 (GET-FILLER ROLE-1 MOP))
       (REPAIR-2 (GET-FILLER ROLE-2 MOP)))
  (LET ((DOWN (PATH-FILLER '(EXPLANATION CAUSE)
                           REPAIR-1))
        (PLAY-1 (GET-FILLER 'SOLUTION REPAIR-1))
        (PLAY-2 (GET-FILLER 'SOLUTION REPAIR-2)))
   (FORMAT T "~&----------------")
   (FORMAT T "~&Trying to make ~S look like ~S"
             PLAY-2 PLAY-1)
   (AND (DELAYABLEP EVENT DOWN)
        (MERGE-PLAYS PLAY-2 PLAY-1
            (GET-ABST-IN-GROUP EVENT PLAY-1))))))

(DEFUN GET-ABST-IN-GROUP (MOP GROUP)
 (FOR (SLOT :IN (MOP-SLOTS GROUP))
    :WHEN (ABSTP (SLOT-FILLER SLOT) MOP)
    :FIRST (SLOT-FILLER SLOT)))

(DEFUN COACH-DEMO ()
 (LET ((INSTANCE
         (COACH '((REPAIR-1 I-M-BOOTLEG-REPAIR)
                  (REPAIR-2 I-M-SWEEP-REPAIR)))))
  (INSIST COACH-DEMO (NOT (NULL INSTANCE)))
  (FORMAT T "~&----------------")
  (FORMAT T "~&The new play is ")
  (DPH (ROLE-FILLER 'NEW-SOLUTION INSTANCE))
  (FORMAT T "~&----------------")
  INSTANCE))
