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

(DEFVAR *SENT-POS* NIL)
(DEFVAR *PRED-SLOTS-TABLE* NIL)
(DEFVAR *PREDICTIONS* NIL)

(DEFUN DMAP (WORD-LIST)
 (FOR (WORD :IN WORD-LIST)
    :DO (FORMAT T "~&~%Reading ~S" WORD)
        (LET ((PREDS (GET-TRIGGERED-PREDS WORD *SENT-POS*)))
         (COND (PREDS
                (FOR (PRED :IN PREDS)
                   :DO (ADVANCE-PRED PRED WORD *SENT-POS*))
                (SETF *SENT-POS* (+ *SENT-POS* 1))))))
 (DP (+ *SENT-POS* 1)))

(DEFINE-TABLE MOP-PREDS (MOP) (MOP-TABLE 'PREDS))

(DEFUN PREDICTED-MOPS ()
 (TABLE-KEYS (MOP-TABLE 'PREDS)))

(SETF *PRED-SLOTS-TABLE* NIL)
(DEFINE-TABLE PRED-SLOTS (PRED) *PRED-SLOTS-TABLE*)

(SETF *PREDICTIONS* NIL)
(DEFINE-TABLE POS-PREDS (POS) *PREDICTIONS*)

(DEFUN DMAP-INIT ()
 (SETF *PREDICTIONS* NIL)
 (SETF *PRED-SLOTS-TABLE* NIL)
 (SETF *SENT-POS* 1))

(DEFUN PRED-FILLER (ROLE PRED)
 (ROLE-FILLER ROLE (PRED-SLOTS PRED)))

(DEFUN ADD-PRED-FILLER (ROLE PRED VALUE)
 (INSIST ADD-PRED-FILLER
         (MOPP VALUE) (NULL (PRED-FILLER ROLE PRED)))
 (SETF (PRED-SLOTS PRED)
       (CONS (MAKE-SLOT ROLE VALUE)
             (PRED-SLOTS PRED)))
 PRED)

(DEFUN PRED-TARGET (PRED) (CAR PRED))
(DEFUN PRED-PHRASE (PRED) (CAR (CDR PRED)))
(DEFUN PRED-BASE (PRED) (CAR (CDR (CDR PRED))))
(DEFUN PRED-START (PRED) (CAR (CDR (CDR (CDR PRED)))))

(DEFUN MAKE-PRED (PHRASE BASE SLOTS START)
 (LET ((TARGET (GET-TARGET PHRASE BASE)))
  (INSIST MAKE-PRED (OR (NULL PHRASE) TARGET))
  (LET ((PRED (LIST TARGET PHRASE BASE START)))
   (SETF (PRED-SLOTS PRED) SLOTS)
   PRED)))

(DEFUN GET-TARGET (PHRASE BASE)
 (COND ((NULL PHRASE) NIL)
       ((ROLE-SPECIFIERP (CAR PHRASE))
        (GET-FILLER (ROLE-SPECIFIER-ROLE (CAR PHRASE))
                    BASE))
       (T (CAR PHRASE))))

(DEFUN ROLE-SPECIFIERP (X) (LISTP X))
(DEFUN ROLE-SPECIFIER-ROLE (X) (CAR X))

(DEFUN GET-TRIGGERED-PREDS (MOP START)
 (APPEND (MOP-DYNAMIC-PREDICTIONS MOP START)
         (MOP-DEFAULT-PREDICTIONS MOP)))

(DEFUN MOP-DYNAMIC-PREDICTIONS (MOP START)
 (FOR (PRED :IN (POS-PREDS START))
    :WHEN (REFERSP (PRED-TARGET PRED) MOP)
    :SAVE PRED))

(DEFUN REFERSP (MOP1 MOP2)
 (OR (ABSTP MOP1 MOP2) (ABSTP MOP2 MOP1)))

(DEFUN MOP-DEFAULT-PREDICTIONS (MOP)
 (FOR (TARGET :IN (PREDICTED-MOPS))
    :WHEN (ABSTP TARGET MOP)
    :SPLICE (FOR (PRED :IN (MOP-PREDS TARGET))
               :SAVE PRED)))

(DEFUN ADVANCE-PRED (PRED MOP START)
 (LET ((PHRASE (PRED-PHRASE PRED)))
  (LET ((NEW-PRED (MAKE-PRED (CDR PHRASE)
                             (PRED-BASE PRED)
                             (PRED-SLOTS PRED)
                             (OR (PRED-START PRED)
				 START))))
   (COND ((ROLE-SPECIFIERP (CAR PHRASE))
          (ADD-PRED-FILLER
             (ROLE-SPECIFIER-ROLE (CAR PHRASE))
             NEW-PRED MOP)))
   (COND ((NULL (CDR PHRASE))
          (PRED->MOP NEW-PRED))
         (T (LET ((NEXT-POS (+ *SENT-POS* 1)))
             (SETF (POS-PREDS NEXT-POS)
                   (CONS NEW-PRED
                         (POS-PREDS NEXT-POS)))))))))

(DEFUN PRED->MOP (PRED)
 (LET ((MOP (SLOTS->MOP (PRED-SLOTS PRED)
                        (LIST (PRED-BASE PRED))
                        T))
       (START (PRED-START PRED)))
   (FORMAT T "~&Activating ~S" MOP)
   (FOR (NEXT-PRED :IN (GET-TRIGGERED-PREDS MOP START))
      :DO (ADVANCE-PRED NEXT-PRED MOP START))))

(DEFMACRO DEFPHRASE (MOP &REST PHRASE)
 `(LET ((PRED (MAKE-PRED ',PHRASE ',MOP NIL NIL)))
   (LET ((TARGET (PRED-TARGET PRED)))
    (SETF (MOP-PREDS TARGET)
          (CONS PRED (MOP-PREDS TARGET)))
    ',PHRASE)))

(DEFUN DP (N)
 (DISPLAY-PREDS (POS-PREDS N))
 NIL)

(DEFUN DAP ()
 (FOR (N :IN (TABLE-KEYS *PREDICTIONS*))
    :DO (FORMAT T "~&Predictions on Position ~S" N)
        (DISPLAY-PREDS (POS-PREDS N)))
 NIL)

(DEFUN DDP ()
 (FOR (MOP :IN (PREDICTED-MOPS))
    :DO (DISPLAY-PREDS (MOP-PREDS MOP)))
 NIL)

(DEFUN DISPLAY-PREDS (PREDS)
 (LET ((I 0))
  (FOR (PRED :IN PREDS)
     :DO (FORMAT T "~&~S: ~S ==> ~S in ~S"
                   (SETF I (+ I 1))
                   (PRED-TARGET PRED)
                   (CAR (PRED-PHRASE PRED))
                   (PRED-BASE PRED)))))

(DEFVAR *SENT1* NIL)
(DEFVAR *SENT2* NIL)


(DEFUN DMAP-DEMO ()
 (FORMAT T "~&----------------")
 (DMAP-INIT)
 (SETF *SENT1*
       '(MILTON FRIEDMAN SAYS INTEREST RATES WILL RISE
         BECAUSE OF THE MONETARY EXPLOSION PERIOD))
 (FORMAT T "~&Parsing ~S" *SENT1*)
 (DMAP *SENT1*)
 (FORMAT T "~&----------------")
 (DMAP-INIT)
 (SETF *SENT2*
       '(ARTHUR LAFFER SAYS INTEREST RATES WILL DROP
         PERIOD))
 (FORMAT T "~&Parsing ~S" *SENT2*)
 (DMAP *SENT2*)
 (DPH (FIND-INSTANCE 'M-MTRANS))
 NIL)

(DEFUN FIND-INSTANCE (ABST)
  (FIND ABST (ALL-MOPS) 
        :TEST #'(LAMBDA (ABST SPEC) 
                  (AND (INSTANCE-MOPP SPEC)
                       (ABSTP ABST SPEC)))))