;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to check memory
;;; -------------------------------------------------
;;; - File: chkmem.lisp
;;; - Author: Chris Riesbeck
;;; - Most recent update: 10/25/95

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (CHECK-MEMORY) => no values
;;;   Prints messages about possible MOP definition problems.


(require "tables")
(use-package :tables)

(require "frames")
(use-package :frames)

(require "mops")
(use-package :mops)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *known-roots*
   '(m-animal m-action m-behavior m-feature m-index)
   "Basic concepts under which all others should fall.")

(defvar *known-slots*
   '((m-index :animal :action :behavior :feature))
   "Table of slots certain concepts should always have.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find possible memory problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-memory ()
  (format t "~2%Checking for concepts with neither abstractions nor specializations.~%")
  (check-for-orphans)
  (format t "~2%Checking for concepts not under ~S.~%" *known-roots*)
  (check-for-unknowns *known-roots*)
  (format t "~2%Checking for concepts with slots with undefined fillers.~%")
  (check-for-undefined-fillers)
  (format t "~2%Checking for concepts with important slots missing.~%")
  (check-for-missing-roles *known-slots*)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find isolated concepts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-for-orphans ()
  (map-memory
     #'(lambda (concept)
         (when (and (leaf-concept-p concept)
                    (root-concept-p concept)
                    (not (member concept *known-roots*)))
           (format t "~&~S is an unconnected concept~%" concept)))))

            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find concepts not under standard roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-for-unknowns (roots)
  (map-memory
   #'(lambda (concept)
       (unless (list-abstp roots concept)
         (format t "~&  ~S~%" concept)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undefined fillers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
(defun check-for-undefined-fillers (&optional abst)
  (map-memory
     #'(lambda (concept)
         (when (or (null abst) (abstp abst concept))
           (let ((slots (get-undefined-slots concept)))
             (unless (null slots)
               (report-undefined-fillers concept slots)))))))

(defun report-undefined-fillers (concept slots)
  (format t "~&~S has undefined fillers~%" concept)
  (print-slots slots))

(defun print-slots (slots)
  (dolist (slot slots)
    (format t "~3T~S ~S~%"
            (slot-role slot) (slot-filler slot))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Find missing slots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun check-for-missing-roles (slot-table)
  (map-memory
   #'(lambda (concept)
       (let ((missing-roles (get-missing-roles concept slot-table)))
         (unless (null missing-roles)
           (format t "~&~S is missing the slots ~S.~%"
                   concept missing-roles))))))

(defun get-missing-roles (concept slot-table)
  (loop for (abst . roles) in slot-table
        when (and (not (eql abst concept))
                  (abstp abst concept))
          append (collect-missing-roles concept roles)))

(defun collect-missing-roles (concept roles)
  (loop for role in roles
        when (null (role-slot concept role))
          collect role))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-abstp (absts concept)
  (some #'(lambda (abst) (abstp abst concept))
        absts))

(defun leaf-concept-p (concept)
  (null (specs-of concept)))

(defun root-concept-p (concept)
  (null (absts-of concept)))

(defun get-undefined-slots (concept)
  (remove-if #'defined-slot-p (slots-of concept)))

(defun defined-slot-p (slot)
  (defined-slot-filler-p (slot-filler slot)))

(defun defined-slot-filler-p (concept)
   (cond ((symbolp concept) (mop-p concept))
     ((atom concept) t)
     (t (every #'defined-slot-filler-p concept))))

(defun map-memory (fn)
  (map-table #'(lambda (name frame)
                 (declare (ignore frame))
                 (funcall fn name))
             (frame-of))
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide "chkmem")
