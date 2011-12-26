;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple test file for dmap-lite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defines a small memory.  Clears any previous memory!!!
;;;
;;; LOADS into current package, e.g., CL-USER.
;;;
;;; Browse mops with (BROWSE 'concept-name)

;;; MODULES
;;; -------

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  )

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package "MOPS")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clear memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(frames::clear-memory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmop m-human (m-root))
(defmop m-economist (m-human))
(defmop m-variable (m-root))
(defmop m-change (m-root))
(defmop m-event (m-root))
(defmop m-change-event (m-event) 
  :variable m-variable :change m-change)
(defmop m-communication-event (m-event)
  :actor m-human :info m-event)

(definstance m-milton-friedman (m-economist))
(definstance m-interest-rates (m-variable))
(definstance m-increase (m-change))

(provide "mop-test")

