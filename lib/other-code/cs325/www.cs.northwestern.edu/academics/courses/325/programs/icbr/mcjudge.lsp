;;; *********************************************************************
;;; Copyright (c) 1989, Lawrence Erlbaum Assoc. All rights reserved.

;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS. The author makes no warranty
;;; about the software, its performance or its conformity to any
;;; specification.
;;; *********************************************************************


(DEFUN JUDGE (SLOTS)
 (LET ((INSTANCE (SLOTS->MOP SLOTS '(M-CRIME) T)))
  (AND (GET-FILLER 'SENTENCE INSTANCE)
       INSTANCE)))

(DEFUN RANGE-CONSTRAINT (CONSTRAINT FILLER SLOTS)
 (DECLARE (IGNORE SLOTS))
 (AND (NUMBERP FILLER)
      (LET ((BELOW (ROLE-FILLER 'BELOW CONSTRAINT))
            (ABOVE (ROLE-FILLER 'ABOVE CONSTRAINT)))
       (AND (OR (NULL BELOW) (< FILLER BELOW))
            (OR (NULL ABOVE) (< ABOVE FILLER))))))

(DEFUN CALC-ESCALATIONS (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (FORMAT T "~&----------------")
 (FORMAT T "~&Calculating escalations in ~S" MOP)
 (LIST->GROUP
   (LET ((PREV-SEVERITY 0))
    (FOR (EVENT :IN (GROUP->LIST
                      (ROLE-FILLER 'EVENTS MOP)))
       :SAVE (LET ((THIS-SEVERITY
                     (PATH-FILLER '(ACTION SEVERITY)
                                  EVENT)))
              (LET ((RESULT (- THIS-SEVERITY
                               PREV-SEVERITY)))
               (SETF PREV-SEVERITY THIS-SEVERITY)
               RESULT))))))

(DEFUN CALC-MOTIVES (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (FORMAT T "~&----------------")
 (FORMAT T "~&Calculating motives in ~S" MOP)
 (LIST->GROUP
   (LET ((PREV-MOTIVE 0))
    (FOR (ESCALATION :IN (GROUP->LIST
                           (GET-FILLER 'ESCALATIONS
                                       MOP)))
       :SAVE (SETF PREV-MOTIVE
                   (MOP-CALC
                     `((ROLE MOTIVE)
                       (ESCALATION ,ESCALATION)
                       (PREV-MOTIVE ,PREV-MOTIVE)
                       )))))))

(DEFUN MOP-CALC (SLOTS)
 (LET ((INSTANCE (SLOTS->MOP SLOTS '(M-CALC) NIL)))
  (AND INSTANCE
       (GET-FILLER 'VALUE INSTANCE))))

(DEFUN ADJUST-SENTENCE (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (FORMAT T "~&----------------")
 (FORMAT T "~&~S applied, ~S events from the end"
           MOP (GET-FILLER 'INDEX MOP))
 (ADJUST-FN
     (GET-FILLER 'OLD-SENTENCE MOP)
     (GET-FILLER 'WEIGHT MOP)
     (GET-FILLER 'INDEX MOP)
     (GET-FILLER 'DIRECTION MOP)))

(DEFUN ADJUST-FN (X Y INDEX DIRECTION)
 (+ X (* X (+ Y (COND ((< INDEX 2) 0.25) (T 0.0)))
           DIRECTION)))

(DEFUN ADAPT-SENTENCE (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (LET ((OLD-MOP (GET-FILLER 'OLD MOP)))
  (LET ((OLD-SIZE (GROUP-SIZE
                    (GET-FILLER 'EVENTS OLD-MOP)))
        (SIZE (GROUP-SIZE (GET-FILLER 'EVENTS MOP)))
        (OLD-SENTENCE (GET-FILLER 'SENTENCE OLD-MOP)))
   (FORMAT T "~&----------------")
   (FORMAT T "~&Adapting the sentence in ~S" OLD-MOP)
   (OR (FOR (OLD-POS :IN (MAKE-M-N OLD-SIZE 1))
            (POS :IN (MAKE-M-N SIZE 1))
          :FIRST
            (MOP-CALC
              `((ROLE SENTENCE) (INDEX ,(- SIZE POS))
                (OLD-SENTENCE ,OLD-SENTENCE)
                ,@(CRIME-COMPARE-SLOTS OLD-MOP OLD-POS
                    '(OLD-ACTION OLD-MOTIVE
                      OLD-SEVERITY))
                ,@(CRIME-COMPARE-SLOTS MOP POS
                    '(THIS-ACTION THIS-MOTIVE
                      THIS-SEVERITY)))))
       (PROGN (FORMAT T "~&----------------")
              (FORMAT T "~&No major difference found")
              (FORMAT T "~&Using old sentence")
              OLD-SENTENCE)))))

(DEFUN CRIME-COMPARE-SLOTS (MOP POS ROLES)
 (LET ((PATHS `((EVENTS ,POS ACTION)
                (MOTIVES ,POS)
                (OUTCOMES ,POS STATE SEVERITY))))
  (INSIST CRIME-COMPARE-SLOTS
          (EQL (LENGTH ROLES) (LENGTH PATHS)))
  (FOR (ROLE :IN ROLES) (PATH :IN PATHS)
     :SAVE (MAKE-SLOT ROLE (PATH-FILLER PATH MOP)))))

(DEFUN COMPARE-CONSTRAINT (CONSTRAINT FILLER SLOTS)
 (FUNCALL (GET-FILLER 'COMPARE-FN CONSTRAINT)
          FILLER
          (INDIRECT-FILLER 'TO CONSTRAINT SLOTS)))

(DEFUN INDIRECT-FILLER (ROLE MOP SLOTS)
 (GET-FILLER (GET-FILLER ROLE MOP) SLOTS))

(SETF *CASE1*
      '((CRIME-TYPE I-M-HOMICIDE)
        (DEFENDANT I-M-TED) (VICTIM I-M-AL)
        (EVENTS M-GROUP
                (1 M-FIGHT-EVENT
                   (ACTION I-M-SLASH) 
                   (ACTOR I-M-TED) (OBJECT I-M-AL)
                   (FREQ I-M-ONCE))
                (2 M-FIGHT-EVENT
                   (ACTION I-M-SLASH)
                   (ACTOR I-M-AL) (OBJECT I-M-TED)
                   (FREQ I-M-ONCE))
                (3 M-FIGHT-EVENT
                   (ACTION I-M-STAB)
                   (ACTOR I-M-TED) (OBJECT I-M-AL)
                   (FREQ I-M-REPEATEDLY)))
        (OUTCOMES M-GROUP
                  (1 M-FIGHT-OUTCOME
                     (STATE I-M-CUT) (ACTOR I-M-AL))
                  (2 M-FIGHT-OUTCOME
                     (STATE I-M-CUT) (ACTOR I-M-TED))
                  (3 M-FIGHT-OUTCOME 
                     (STATE I-M-DEAD) (ACTOR I-M-AL)))
        (SENTENCE 40)))

(SETF *CASE2*
      '((CRIME-TYPE I-M-HOMICIDE)
        (DEFENDANT I-M-RANDY) (VICTIM I-M-CHUCK)
        (EVENTS M-GROUP 
          (1 M-FIGHT-EVENT
             (ACTION I-M-STRIKE)
             (ACTOR I-M-RANDY) (OBJECT I-M-CHUCK)
             (FREQ I-M-REPEATEDLY))
          (2 M-FIGHT-EVENT
             (ACTION I-M-STRIKE)
             (ACTOR I-M-CHUCK) (OBJECT I-M-RANDY)
             (FREQ I-M-REPEATEDLY))
          (3 M-FIGHT-EVENT
             (ACTION I-M-SLASH)
             (ACTOR I-M-RANDY) (OBJECT I-M-CHUCK) 
             (FREQ I-M-ONCE))
          (4 M-FIGHT-EVENT
             (ACTION I-M-SLASH)
             (ACTOR I-M-CHUCK) (OBJECT I-M-RANDY)
             (FREQ I-M-ONCE))
          (5 M-FIGHT-EVENT
             (ACTION I-M-STAB)
             (ACTOR I-M-RANDY) (OBJECT I-M-CHUCK)
             (FREQ I-M-REPEATEDLY)))
        (OUTCOMES M-GROUP 
          (1 M-FIGHT-OUTCOME
             (STATE I-M-BRUISED) (ACTOR I-M-CHUCK))
          (2 M-FIGHT-OUTCOME
             (STATE I-M-BRUISED) (ACTOR I-M-RANDY))
          (3 M-FIGHT-OUTCOME
             (STATE I-M-CUT) (ACTOR I-M-CHUCK))
          (4 M-FIGHT-OUTCOME
             (STATE I-M-CUT) (ACTOR I-M-RANDY))
          (5 M-FIGHT-OUTCOME
             (STATE I-M-DEAD) (ACTOR I-M-CHUCK)))))

(SETF *CASE3*
      '((CRIME-TYPE I-M-HOMICIDE)
        (DEFENDANT I-M-TIM) (VICTIM I-M-DAVID)
        (EVENTS M-GROUP
          (1 M-FIGHT-EVENT
             (ACTION I-M-SLAP)
             (ACTOR I-M-DAVID) (OBJECT I-M-TIM)
             (FREQ I-M-SEVERAL-TIMES))
          (2 M-FIGHT-EVENT
             (ACTION I-M-STRIKE)
             (ACTOR I-M-TIM) (OBJECT I-M-DAVID) 
            (FREQ I-M-SEVERAL-TIMES))
          (3 M-FIGHT-EVENT
             (ACTION I-M-KNOCK-DOWN)
             (ACTOR I-M-DAVID) (OBJECT I-M-TIM)
             (FREQ I-M-ONCE))
          (4 M-FIGHT-EVENT
             (ACTION I-M-STAB)
             (ACTOR I-M-TIM) (OBJECT I-M-DAVID)
             (FREQ I-M-SEVERAL-TIMES)))
        (OUTCOMES M-GROUP
          (1 M-FIGHT-OUTCOME
             (STATE I-M-BRUISED) (ACTOR I-M-TIM))
          (2 M-FIGHT-OUTCOME
             (STATE I-M-BRUISED) (ACTOR I-M-DAVID))
          (3 M-FIGHT-OUTCOME
             (STATE I-M-KNOCKED-DOWN) (ACTOR I-M-TIM))
          (4 M-FIGHT-OUTCOME
             (STATE I-M-DEAD) (ACTOR I-M-DAVID)))))

(DEFVAR *CASE1* NIL)
(DEFVAR *CASE2* NIL)
(DEFVAR *CASE3* NIL)

(DEFUN JUDGE-DEMO ()
 (RUN-JUDGE *CASE1* '*CASE1*)
 (RUN-JUDGE *CASE2* '*CASE2*)
 (RUN-JUDGE *CASE3* '*CASE3*))

(DEFUN RUN-JUDGE (CASE CASE-NAME)
 (FORMAT T "~&----------------")
 (FORMAT T "~&Sentencing ~S in ~S"
           (ROLE-FILLER 'DEFENDANT CASE) CASE-NAME)
 (LET ((INSTANCE (JUDGE (FORMS->SLOTS CASE))))
  (INSIST JUDGE-DEMO (NOT (NULL INSTANCE)))
  (FORMAT T "~&Sentence in ~S is ~S years"
            INSTANCE (ROLE-FILLER 'SENTENCE INSTANCE))
  INSTANCE))
