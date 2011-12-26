;;; A simple MOP system
;;; ----------------------------------------------------------------------
;;; File: mops.lisp
;;; Author: Chris Riesbeck


#|
06/06/2006: [CKR]
Problem: no control over where new concept names were put
Change: added :package parameter to add-instance and find-instances

06/06/2006: [CKR]
Change: Updated packaging code to standard CL2 

11/16/98 [CKR]
Problem: instances added when not wanted in dmap.lisp
Cause: find-instances automatically added instances
Change: added :add keyword parameter to find-instances

9/13/95 [CKR]
Problem: the package was not getting common-lisp in some CL2's 
Cause: CLTL2 is not a defined feature in all CL2's
Change: Use (OR (find-package ...) (find-package ...)) form.

11/3/94 [CKR]
Problem: :lisp/:common-lisp conflicts
Change: added #+:cltl2 forms

11/1/94 [CKR]
Problem: compiled MOPS code not working in XlispStat 3.39
Cause: XlispStat 3.39 compiler seems to break if LOOP redefined
Change: replaced all uses of LOOP

10/19/94 [CKR]
Problem: No :mops package.
Change:  Add package calls.

|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage #:mops
  (:use #:common-lisp #:frames)
  (:export #:defmop #:definstance #:mop-p #:instance-p #:find-instances 
           #:add-instance #:role-slot #:role-filler #:slot-role #:slot-filler 
           #:inherit-filler #:<-
           #:abstp #:slots-of #:absts-of #:specs-of #:all-absts-of)
  )

(in-package #:mops)

(defmacro defmop (name &optional absts &rest slots)
  `(add-frame ',name 
              :type :mop 
              :abstractions ',(or absts 'm-mop-root)
              :slots ',slots))

(defmacro definstance (name &optional absts &rest slots)
  `(add-frame ',name 
              :type :instance
              :abstractions ',(or absts 'm-mop-root)
              :slots ',slots))

(defun add-instance (abst slots &key package)
  (add-frame (make-instance-name abst :package package)
             :package package
             :type :instance
             :abstractions (list abst)
             :slots slots))

(defun make-instance-name (abst &key package)
  (gentemp (string abst) (or package *package*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mop-p (name &aux (frame (frame-of name)))
  (and frame (eql (frame-type frame) :mop)))

(defun instance-p (name &aux (frame (frame-of name)))
  (and frame (eql (frame-type frame) :instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-instances (abst slots &key add package)
  (or (and (instance-p abst) (list abst))
      (mapcan #'(lambda (spec)
                  (when (has-slots-p spec slots)
                    (find-instances spec slots)))
              (specs-of abst))
      (if add
        (list (add-instance abst slots :package package))
        nil)))

(defun has-slots-p (name slots)
  (or (null slots)
      (destructuring-bind (role filler &rest more-slots) slots
        (and (has-slot-p name role filler)
             (has-slots-p name more-slots)))))

(defun has-slot-p (name role filler)
  (let ((name-slot (role-slot name role)))
    (and (not (null name-slot))
         (abstp (slot-filler name-slot) filler))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #+:cltl2 :common-lisp-user #-:cltl2 :user)
  
(provide "mops")

