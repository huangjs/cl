;;; A simple frame system
;;; ----------------------------------------------------------------------
;;; - File: frames.lisp
;;; - Author: Chris Riesbeck
;;;
;;; 06/06/06 Added :package parameter to add-frame [CKR]
;;; 03/04/04 Export all symbols used in a frame [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "tables")
  )

(cl:defpackage #:frames
  (:use #:common-lisp #:tables)
  (:export #:frame-of #:->frame #:add-frame 
           #:clear-memory #:frame-type 
           #:slots-of #:absts-of #:specs-of #:all-absts-of 
           #:role-slot #:slot-role #:slot-filler #:role-filler
           #:inherit-filler #:<- #:abstp)
  )


(in-package #:frames)


(deftable frame-of)

(defstruct frame 
  name type slots absts specs all-absts)

(defstruct slot role filler)

(defun add-frame (name &key (package *package*) type abstractions slots)
  (export name package)
  (export-symbols-in abstractions package)
  (export-symbols-in slots package)
  (let ((frame (->frame name)))
    (setf (frame-type frame) type)
    (update-absts name
                  (if (listp abstractions)
                      abstractions
                      (list abstractions)))
    (setf (frame-slots frame) (collect-slots slots))
    name))

(defun collect-slots (l)
  (cond ((null l) nil)
    (t (cons (make-slot :role (car l)
                        :filler (cadr l))
             (collect-slots (cddr l))))))

(defun ->frame (name &key type)
  (or (frame-of name)
      (setf (frame-of name)
            (make-frame :name name :type type :all-absts (list name)))))

(defun clear-memory () (clear-table (frame-of)))

(defun export-symbols-in (l package)
  (cond ((and (symbolp l) (eql (symbol-package l) package))
         (export l))
        ((atom l) nil)
        (t (export-symbols-in (car l) package)
           (export-symbols-in (cdr l) package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slots-of (name &aux (frame (frame-of name)))
  (if frame (frame-slots frame)))
        
(defun absts-of (name &aux (frame (frame-of name)))
  (if frame (frame-absts frame)))
        
(defun specs-of (name &aux (frame (frame-of name)))
  (if frame (frame-specs frame)))

(defun all-absts-of (name &aux (frame (frame-of name)))
  (if frame (frame-all-absts frame) (list name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abstp (abst spec)
 (or (eql abst spec)
     (not (null (member abst (all-absts-of spec))))))

(defun inherit (name fn)
  (some #'(lambda (abst) (funcall fn abst)) 
        (all-absts-of name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abstraction hierarchy bookkeeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (UPDATE-ABSTS name abstractions) => undefined
;;;    Changes name to have the given abstractions, and updates
;;;    memory accordingly.  This may involve removing old
;;;    abstraction links, and recalculating the all-absts field for
;;;    name and its children.

(defun update-absts (name absts)
  (let ((old-absts (absts-of name))
        (new-absts (remove-redundant-absts absts)))
    (unless (set-equalp old-absts new-absts)
      (unlink-old-absts name old-absts new-absts)
      (link-new-absts name old-absts new-absts)
      (update-all-absts name))))

;;; (SET-EQUALP set1 set2) => true or false
;;;    Returns true if the two list have the same elements.  The
;;;    keyword arguments are passed to SUBSETP.

(defun set-equalp (set1 set2)  
  (and (subsetp set1 set2) (subsetp set2 set1)))

;;; (REMOVE-REDUNDANT-ABSTS absts) => absts
;;;    Returns absts, minus those items that are redundant.
;;; (REDUNDANT-ABST-P name absts) => true or false
;;;    returns true if name is redundant with respect to absts.
;;;
;;; An item is redundant with respect to a list if there is something
;;; strictly more specific than it in the list.

(defun remove-redundant-absts (absts)
  (remove-if #'(lambda (abst)
                 (redundant-abst-p abst absts))
             absts))

(defun redundant-abst-p (name absts)
  (and (not (null (specs-of name)))
       (member name absts :test #'strict-abstp)))

(defun strict-abstp (abst spec)
  (and (not (eql abst spec))
       (abstp abst spec)))

;;; (UNLINK-OLD-ABSTS name old-absts new-absts) => undefined
;;;   Unlinks name from any abstraction in old-absts that isn't
;;;   in new-absts.
;;; (LINK-NEW-ABSTS name old-absts new-absts) => undefined
;;;   Links name to any abstraction in new-absts that isn't
;;;   in old-absts.

(defun unlink-old-absts (name old-absts new-absts)
  (dolist (old-abst old-absts)
    (unless (member old-abst new-absts)
      (unlink-abst name old-abst))))

(defun link-new-absts (name old-absts new-absts)
  (dolist (new-abst new-absts)
    (unless (member new-abst old-absts)
      (link-abst name new-abst))))

;;; (LINK-ABST spec abst) => undefined
;;;   Makes an immediate abstraction link from spec to abst.
;;;   If abst is already an abstraction of spec, nothing happens
;;;   If spec is an abstraction of abst, an error is signalled.
;;; (UNLINK-ABST spec abst) => undefined
;;;   Removes the immediate abstraction link from spec to abst,
;;;   if any.

(defun link-abst (spec abst)
  (cond ((abstp abst spec) nil)
        ((abstp spec abst)
         (error "~S can't be an abstraction of ~S" spec abst))
        (t
         (link-parent spec abst)
         (link-child spec abst))))

(defun unlink-abst (spec abst)
  (unlink-parent spec abst)
  (unlink-child spec abst))

;;; (LINK-PARENT spec abst) => undefined
;;;   Makes a parent link from spec to abst.
;;; (LINK-CHILD spec abst) => undefined
;;;   Makes a child link from abst to spec.

(defun link-parent (spec abst)
 (push abst (frame-absts (->frame spec))))

(defun link-child (spec abst)
  (push spec (frame-specs (->frame abst))))

;;; (UNLINK-PARENT spec abst) => undefined
;;;   Removes the parent link from spec to abst, if any.
;;; (UNLINK-CHILD spec abst) => undefined
;;;   Removes the child link from abst to spec, if any.

(defun unlink-parent (spec abst)
  (let ((spec-frame (frame-of spec)))
    (when spec-frame
      (setf (frame-absts spec-frame)
            (remove abst (frame-absts spec-frame))))))

(defun unlink-child (spec abst)
  (let ((abst-frame (frame-of abst)))
    (when abst-frame
      (setf (frame-specs abst-frame)
            (remove spec (frame-specs abst-frame))))))

;;; (UPDATE-ALL-ABSTS name) => undefined
;;;    Called when abstraction links are changed to recursively
;;;    fix the all-absts field for name and its children.

(defun update-all-absts (name)
  (let ((frame (->frame name)))
    (setf (frame-all-absts frame)
          (calc-all-absts name))
    (dolist (spec (frame-specs frame)) (update-all-absts spec))))

;;; (CALC-ALL-ABSTS ) => list of abstractions
;;;    Returns all the abstractions for name, in order.

(defun calc-all-absts (name)
  (cons name
        (remove-duplicates
         (mapcan #'(lambda (abst)
                     (copy-list (all-absts-of abst)))
                 (absts-of name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun role-slot (name role) 
  (find role (slots-of name) :key #'slot-role))
    
(defun role-filler (name role) 
  (let ((slot (role-slot name role)))
    (and slot (slot-filler slot))))

(defun inherit-filler (name role)
  (inherit name #'(lambda (abst) (role-filler abst role))))
        
(defun <- (name &rest roles)
  (do ((roles roles (rest roles))
       (name name (inherit-filler name (first roles))))
      ((or (endp roles) (null name)) name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #+:cltl2 :common-lisp-user #-:cltl2 :user)
  
(provide "frames")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change log
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
03/04/04 [CKR]
Exported CLEAR-MEMORY

3/2/04 [CKR]
Problem: find-instances :color pink didn't find mops with pink
Cause: add-frame pig (mammal) :color pink) only exports pig
Change: export all symbols used in a frame definition

9/13/95 [CKR]
Problem: the package was not getting common-lisp in some CL2's 
Cause: CLTL2 is not a defined feature in all CL2's
Change: Use (OR (find-package ...) (find-package ...)) form.

11/3/94 [CKR]
Problem: :lisp/:common-lisp conflicts
Change: added #+:cltl2 forms

11/3/94 [CKR]
Problem: :lisp/:common-lisp conflicts
Change: added #+:cltl2 forms

11/1/94 [CKR]
Problem: remove-redundant-absts unacceptably slow
Cause: uses an N**2 algorithm
Change: added specs-of check to avoid most of the work

11/1/94 [CKR]
Problem: compiled FRAMES code not working in XlispStat 3.39
Cause: XlispStat 3.39 compiler seems to break if LOOP redefined
Change: replaced all uses of LOOP

10/26/94 [CKR]
Problem: NIL appearing in ALL-ABSTS lists
Cause: test for atom in add-frame instead of test for listp
Change:  used listp

10/19/94 [CKR]
Problem: No :frames package.
Change:  Add package calls.

|#
