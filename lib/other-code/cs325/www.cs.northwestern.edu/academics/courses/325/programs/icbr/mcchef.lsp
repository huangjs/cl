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

(DEFVAR *BAD-RECIPE* NIL)
(DEFVAR *BAD-STEP* NIL)
(DEFVAR *BAD-RECIPE-EXPLANATION* NIL)
(DEFVAR *BAD-STEP* NIL)
(DEFVAR *RECIPE-REPAIR* NIL)
(DEFVAR *GOOD-RECIPE* NIL)


(DEFUN CHEF (SLOTS)
 (LET ((INSTANCE (SLOTS->MOP SLOTS '(M-RECIPE) T)))
  (AND (GET-FILLER 'STEPS INSTANCE)
       INSTANCE)))

(DEFUN MOP-SUBST (NEW OLD MOP)
 (COND ((EQL MOP OLD) NEW)
       ((NULL (MOP-SLOTS MOP)) MOP)
       (T (LET ((SLOTS (MOP-SLOTS MOP)))
           (LET ((NEW-SLOTS
                   (SLOTS-SUBST NEW OLD SLOTS)))
            (COND ((EQUAL NEW-SLOTS SLOTS) MOP)
                  ((GROUPP MOP)
                   (LIST->GROUP
                     (FOR (SLOT :IN NEW-SLOTS)
                        :SAVE (SLOT-FILLER SLOT))))
                  (T (RAISE-ABST
                       (FORMS->SLOTS NEW-SLOTS)
                       MOP))))))))

(DEFUN SLOTS-SUBST (NEW OLD SLOTS)
 (FOR (SLOT :IN SLOTS)
    :FILTER
      (LET ((FILLER
              (MOP-SUBST NEW OLD (SLOT-FILLER SLOT))))
       (AND FILLER
            (MAKE-SLOT (SLOT-ROLE SLOT) FILLER)))))
         
(DEFUN RAISE-ABST (SLOTS MOP)
 (LET ((ABST (FOR (ABST :IN (MOP-ALL-ABSTS MOP))
                :WHEN (SLOTS-ABSTP ABST SLOTS)
                :FIRST ABST)))
  (AND ABST (SLOTS->MOP SLOTS (LIST ABST) NIL))))

(DEFUN GET-PRECONS (INGRED)
 (FORMAT T "~&----------------")
 (FORMAT T "~&Getting preconditions for ~S" INGRED)
 (AND INGRED
      (SLOTS->MOP `((INGRED ,INGRED))
                  '(M-PRECONS)
                  NIL)))

(DEFUN MAKE-MOP (PATTERN MOP)
 (LET ((SLOTS (FOR (SLOT :IN (MOP-SLOTS PATTERN))
                 :WHEN (NOT (EQL (SLOT-ROLE SLOT) 'MOP))
                 :SAVE SLOT)))
  (SLOTS->MOP (REPLACE-SLOTS SLOTS MOP)
              (LIST (GET-FILLER 'MOP PATTERN))
              T)))

(DEFUN REPLACE-SLOTS (SLOTS MOP)
 (FOR (SLOT :IN SLOTS)
    :SAVE `(,(SLOT-ROLE SLOT)
            ,(LET ((FILLER (SLOT-FILLER SLOT)))
              (COND ((ABSTP 'M-ROLE FILLER)
                     (ROLE-FILLER FILLER MOP))
                    ((ABSTP 'M-PATH FILLER)
                     (PATH-FILLER (GROUP->LIST FILLER) MOP))
                    (T FILLER))))))

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

(DEFUN GROUP-MEMBER (MOP GROUP)
 (AND (GROUPP GROUP)
      (FOR (SLOT :IN (MOP-SLOTS GROUP))
         :FIRST (EQL (SLOT-FILLER SLOT) MOP))))

(DEFUN GROUP-SPLICE (NEW OLD GROUP)
 (LIST->GROUP
  (FOR (MOP :IN (GROUP->LIST GROUP))
     :SPLICE (COND ((EQL MOP OLD) NEW)
                   (T (LIST MOP))))))

(DEFUN GROUP-INSERT (MOP GROUP)
 (COND ((NULL MOP) GROUP)
       ((GROUP-MEMBER MOP GROUP) GROUP)
       (T (LIST->GROUP (APPEND (GROUP->LIST GROUP)
                               (LIST MOP))))))

(DEFUN ADD-PRECONS (PRECON-MOP STEPS-SLOTS)
 (COND ((NULL PRECON-MOP) STEPS-SLOTS)
       (T
        (FORMAT T "~&Adding preconditions to recipe")
        (FOR (SLOT :IN STEPS-SLOTS)
           :SAVE (MERGE-STEP PRECON-MOP SLOT)))))

(DEFUN MERGE-STEP (PRECON-MOP SLOT)
 (LET ((ROLE (SLOT-ROLE SLOT))
       (OLD-FILLER (SLOT-FILLER SLOT)))
  (LET ((NEW-FILLER
          (GROUP-INSERT (GET-FILLER ROLE PRECON-MOP)
                        OLD-FILLER)))
    (COND ((EQL NEW-FILLER OLD-FILLER) SLOT)
          (T (MAKE-SLOT ROLE NEW-FILLER))))))

(DEFUN ADAPT-STEPS (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (LET ((RECIPE (GET-FILLER 'OLD MOP)))
  (FORMAT T "~&----------------")
  (FORMAT T "~&Adapting the steps in ~S" RECIPE)
  (LET ((STEPS (MOP-SLOTS (GET-FILLER 'STEPS RECIPE))))
   (AND (FOR (INGRED-ROLE :IN '(MEAT VEGE VEGE2))
          :ALWAYS
            (SETF STEPS
                  (SUBST-INGRED
                    (GET-FILLER INGRED-ROLE MOP)
                    (GET-FILLER INGRED-ROLE RECIPE)
                    STEPS)))
        (SLOTS->MOP STEPS '(M-RECIPE-STEPS) NIL)))))

(DEFUN SUBST-INGRED (NEW-INGRED OLD-INGRED STEPS)
 (COND ((OR (NULL NEW-INGRED)
            (EQL NEW-INGRED OLD-INGRED))
        STEPS)
       (T (ADD-PRECONS (GET-PRECONS NEW-INGRED)
            (COND ((NULL OLD-INGRED) STEPS)
                  (T (FORMAT T "~&Substituting ~S for ~S"
                               NEW-INGRED OLD-INGRED)
                     (SLOTS-SUBST NEW-INGRED
                                  OLD-INGRED
                                  STEPS)))))))

(DEFUN CHEF-REPAIR (SLOTS)
 (LINK-ABST (ROLE-FILLER 'SOLUTION SLOTS)
            'M-FAILED-SOLUTION)
 (LET ((INSTANCE (SLOTS->MOP SLOTS '(M-REPAIR) T)))
  (AND (GET-FILLER 'REPAIRED-SOLUTION INSTANCE)
       INSTANCE)))

(DEFUN ISA-CONSTRAINT (CONSTRAINT FILLER SLOTS)
 (ABSTP (ROLE-FILLER (ROLE-FILLER 'ROLE CONSTRAINT)
                     SLOTS)
         FILLER))

(DEFUN RESET-ROLE-FILLER (ROLE MOP NEW-VALUE)
 (LET ((OLD-VALUE (ROLE-FILLER ROLE MOP)))
  (INSIST RESET-ROLE-FILLER (NOT (NULL OLD-VALUE)))
  (COND ((EQL OLD-VALUE NEW-VALUE) MOP)
        (T (RAISE-ABST
              (FOR (SLOT :IN (MOP-SLOTS MOP))
                 :SAVE (COND ((EQL (SLOT-ROLE SLOT)
                                   ROLE)
                              (MAKE-SLOT ROLE
                                         NEW-VALUE))
                             (T SLOT)))
              MOP)))))

(DEFUN SPLIT-STEP (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (LET ((STEP (PATH-FILLER '(EXPLANATION CAUSE) MOP))
       (OBJECT
         (PATH-FILLER '(EXPLANATION FAILURE OBJECT)
                      MOP)))
  (LET ((STEP-OBJECT (ROLE-FILLER 'OBJECT STEP)))
   (LIST->GROUP
     (LIST
       (RESET-ROLE-FILLER 'OBJECT STEP OBJECT)
       (RESET-ROLE-FILLER 'OBJECT STEP
         (GROUP-SPLICE NIL OBJECT STEP-OBJECT)))))))

(DEFUN APPLY-REPAIR (PATTERN MOP)
 (DECLARE (IGNORE PATTERN))
 (LET ((REPAIR (GET-FILLER 'REPAIR MOP))
       (CAUSE (PATH-FILLER '(EXPLANATION CAUSE) MOP))
       (RECIPE (ROLE-FILLER 'SOLUTION MOP)))
  (LET ((STEPS (ROLE-FILLER 'STEPS RECIPE)))
   (RESET-ROLE-FILLER 'STEPS RECIPE
     (FOR (SLOT :IN (MOP-SLOTS STEPS))
        :WHEN (GROUP-MEMBER CAUSE (SLOT-FILLER SLOT))
        :FIRST (RESET-ROLE-FILLER (SLOT-ROLE SLOT)
                  STEPS
                  (GROUP-SPLICE (GROUP->LIST REPAIR)
                     CAUSE
                     (SLOT-FILLER SLOT))))))))

(DEFUN GENERALIZE-REPAIR (REPAIR INPUT-ROLES)
 (LET ((SOLUTION
         (GET-FILLER 'REPAIRED-SOLUTION REPAIR)))
  (LET ((SLOTS (GENERALIZE-SLOTS SOLUTION INPUT-ROLES
                  (PATH-FILLER '(EXPLANATION MAPPING)
                               REPAIR)))
        (ABSTS (MOP-ABSTS SOLUTION)))
   (FOR (SLOT :IN SLOTS)
      :DO (SLOTS->MOP
             (FORMS->SLOTS
                `((,(SLOT-ROLE SLOT)
                   M-NOT
                   (OBJECT ,(SLOT-FILLER SLOT)))))
             ABSTS T))
   (SLOTS->MOP SLOTS ABSTS T))))

(DEFUN GENERALIZE-SLOTS (MOP ROLES MAPS)
 (FOR (ROLE :IN ROLES)
    :FILTER (GENERALIZE-SLOT ROLE
                             (ROLE-FILLER ROLE MOP)
                             MAPS)))

(DEFUN GENERALIZE-SLOT (ROLE MOP MAPS)
 (AND MOP
      (LET ((ABST (GENERALIZE-MOP MOP MAPS)))
       (AND ABST (MAKE-SLOT ROLE ABST)))))

(DEFUN GENERALIZE-MOP (MOP MAPS)
 (FOR (SLOT :IN (MOP-SLOTS MAPS))
    :WHEN (EQL (ROLE-FILLER 'SPEC (SLOT-FILLER SLOT))
               MOP)
    :FIRST (ROLE-FILLER 'ABST (SLOT-FILLER SLOT))))

(DEFUN CHEF-EXPLAIN (MOP)
 (SLOTS->MOP
   `(INSTANCE
     (FAILURE ,MOP)
     (CAUSE ,*BAD-STEP*)
     (RULE M-RULE)
     (MAPPING 
       ,(SLOTS->MOP
          (FORMS->SLOTS
            '((1 M-MAP INSTANCE
                 (ABST M-MEAT) (SPEC I-M-BEEF))
              (2 M-MAP INSTANCE
                 (ABST M-CRISP-VEGETABLE)
                 (SPEC I-M-BROCCOLI))))
          '(M-MAP-GROUP)
          T)))
   '(M-EXPLANATION)
   T))

(DEFUN RUN-CHEF (SLOTS)
 (LET ((RECIPE (CHEF SLOTS)))
  (AND RECIPE
       (PROGN (FORMAT T "~&----------------")
              (FORMAT T "~&The steps in ~S are:"
                        RECIPE)
              (DPH (ROLE-FILLER 'STEPS RECIPE))
              RECIPE))))

(DEFUN CHEF1 ()
 (SETF *BAD-RECIPE*
       (RUN-CHEF '((MEAT I-M-BEEF) (VEGE I-M-BROCCOLI)
                   (TASTE I-M-HOT)
                   (STYLE I-M-STIR-FRY))))
 (SETF *BAD-STEP*
       (PATH-FILLER `(STEPS STIR-FRY-STEPS 1)
                    *BAD-RECIPE*))
 *BAD-RECIPE*)

(DEFUN CHEF2 ()
 (SETF *BAD-RECIPE-EXPLANATION*
       (CHEF-EXPLAIN 
         (SLOTS->MOP '((STATE I-M-SOGGY)
                       (OBJECT I-M-BROCCOLI))
                     '(M-FAILURE)
                     T))))

(DEFUN CHEF3 ()
 (SETF *RECIPE-REPAIR*
   (CHEF-REPAIR
      `((SOLUTION ,*BAD-RECIPE*)
        (EXPLANATION ,*BAD-RECIPE-EXPLANATION*))))
 (SETF *GOOD-RECIPE*
       (ROLE-FILLER 'REPAIRED-SOLUTION
                    *RECIPE-REPAIR*)))

(DEFUN CHEF4 ()
 (GENERALIZE-REPAIR *RECIPE-REPAIR*
                    '(MEAT VEGE VEGE2 TASTE STYLE)))

(DEFUN CHEF5 ()
 (RUN-CHEF '((MEAT I-M-CHICKEN) (VEGE I-M-SNOW-PEAS)
             (STYLE I-M-STIR-FRY))))

(DEFUN CHEF6 ()
 (RUN-CHEF '((MEAT I-M-BEEF) (VEGE I-M-GREEN-PEPPERS)
             (STYLE I-M-STIR-FRY))))

(DEFUN CHEF-DEMO ()
 (FORMAT T "~&----------------")
 (FORMAT T "~&Beef and broccoli recipe")
 (INSIST CHEF-DEMO (CHEF1))
 (FORMAT T "~&----------------")
 (FORMAT T "~&Kludging soggy broccoli failure")
 (LINK-ABST *BAD-RECIPE* 'M-FAILED-SOLUTION)
 (FORMAT T "~&Cause is ~S" *BAD-STEP*)
 (FORMAT T "~&Kludging explanation")
 (INSIST CHEF-DEMO (CHEF2))
 (FORMAT T "~&Explanation is ~S"
           *BAD-RECIPE-EXPLANATION*)
 (FORMAT T "~&----------------")
 (FORMAT T "~&Trying to repair ~S" *BAD-RECIPE*)
 (INSIST CHEF-DEMO (CHEF3))
 (FORMAT T "~&Repaired beef and broccoli recipe is ~S"
           *GOOD-RECIPE*)
 (FORMAT T "~&----------------")
 (FORMAT T "~&The repaired steps are:")
 (DPH (ROLE-FILLER 'STEPS *GOOD-RECIPE*))
 (FORMAT T "~&----------------")
 (FORMAT T "~&Generalizing repair")
 (INSIST CHEF-DEMO (CHEF4))
 (FORMAT T "~&New hierarchies:")
 (DAH 'M-RECIPE)
 (DAH 'M-FAILED-SOLUTION)
 (FORMAT T "~&----------------")
 (FORMAT T "~&Chicken and snow-peas recipe")
 (INSIST CHEF-DEMO (CHEF5))
 (FORMAT T "~&----------------")
 (FORMAT T "~&Beef and green peppers recipe")
 (INSIST CHEF-DEMO (CHEF6)))
