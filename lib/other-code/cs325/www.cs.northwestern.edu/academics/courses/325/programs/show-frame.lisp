;;; A simple frame system
;;; ------------------------------------------------------------
;;; - File: frames.lisp
;;; - Authors: Chris Riesbeck, Will Fitzgerald
;;;   Inspired by code by Matt Togliatti, C25, Fall 90.
;;; - Most recent update: 11/15/94
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (SHOW-MEMORY concept [stream slots-p])
;;;    Prints the tree of concepts under concept. Stream defaults
;;;    to standard output. If slots-p is true, slots for each
;;;    concept are printed. A concept's subtree and slots are
;;;    printed once, even if the concept has multiple parents.
;;; (SHOW-FRAME concept)
;;;    Pretty-prints the slots of a concept, the slots of the fillers
;;;    and so on.
;;;
;;; Exs.
;;;   > (require "show-frame")
;;;   > (use-package :frames)
;;;   > (show-memory 'm-index)
;;;   ... prints the tree under M-INDEX
;;;   > (show-memory 'm-index t t)
;;;   ... prints the tree under M-INDEX, and the slots of each concept
;;;   > (show-frame 'i-cheetah-chase-antelope)
;;;   ... prints the slots of I-CHEETAH-CHASE-ANTELOPE and of its fillers

(require "frames")
(in-package :frames)

(export '(show-memory show-frame))

(defun show-memory (name
                    &optional (stream *standard-output*) slots-p
                    &aux shown)
  (labels ((show (name prefix)
             (let ((specs (specs-of name))
                   (slots (and slots-p (slots-of name))))
               (cond ((member name shown)
                      (format stream
                              (if (or specs slots) "~S...~%" "~S~%") name))
                 (t
                  (format stream "~S~%" name)
                  (push name shown)
                  (when slots
                    (let ((bar (if specs "|" " ")))
                      (dolist (slot slots)
                         (if (slot-p slot)
                             (format stream "~A ~A ~S ~S~%" prefix bar
                                     (slot-role slot)
                                     (slot-filler slot))
				;; :CONSTRAINTS or :METHODS
                             (format stream "~A ~A ~S are:~%"
                                     prefix bar slot)))))
                  (when specs
                    (do ((next-prefix (format nil "~A |   " prefix))
                         (last-prefix (format nil "~A     " prefix))
                         (l specs (cdr l)))
                        ((null (cdr l))
                         (format stream "~A +-- " prefix)
                         (show (car l) last-prefix))
                        (format stream "~A |-- " prefix)
                        (show (car l) next-prefix))))))))
    (show name "")
    name))

;;; Printing internal frame structures
;;; ----------------------------------------------------------------------


;;; (SHOW-FRAME name) => no values
;;;   Prints the internals of the frame named in a readable fashion.
;;;
;;; SHOW-FRAME recursively prints the internal structure of all parts
;;; of the frame, if they haven't already been printed.

(defvar *frames-shown* nil)

(defun show-frame (name)
  (let ((*frames-shown* '()))
    (pprint-frame-info name 4)
    (values)))

;;; (PPRINT-FRAME-INFO name left-margin) => undefined

;;; PPRINT-FRAME-INFO prints internal frame structures in a readable
;;; fashion, indented left-margin number of spaces.

(defun pprint-frame-info (name left-margin)
  (unless (member name *frames-shown*)
    (push name *frames-shown*)
    (let ((frame (frame-of name)))
      (unless (null frame)
        (pprint-frame-type frame left-margin)
        (pprint-frame-absts frame left-margin)
        (pprint-frame-slots frame left-margin)))))

(defun pprint-frame-type (frame left-margin)
  (let ((type (frame-type frame)))
    (when type
      (format t "~&~VTTYPE ~S~%" left-margin type))))

(defun pprint-frame-absts (frame left-margin)
  (loop for abst in (frame-absts frame)
        do (format t "~&~VTISA ~S~%" left-margin abst)))

(defun pprint-frame-slots (frame left-margin)
  (loop for slot in (frame-slots frame)
        do (format t "~&~VT~S ~S~%" 
                   left-margin 
                   (slot-role slot)
                   (slot-filler slot))
        (pprint-frame-info (slot-filler slot)
                           (+ left-margin 2))))

(provide "show-frame")
