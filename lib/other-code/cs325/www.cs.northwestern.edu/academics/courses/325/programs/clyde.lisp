;;; A toy MOP memory -- Clyde gets sick from eating
;;; peanuts.

;;; Note that this doesn't follow our general naming
;;; conventions of m-xxx for general concepts
;;; and i-m-xxx.n for individuals.

;;; Updates
;;; 03/02/04 added Wilbur and Jumbo [CKR]

;;; MODULES
;;; -------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  )

;;; PACKAGES
;;; --------

;;; There is no explicit EXPORT. DEFMOP and DEFINSTANCE
;;; automatically export the concepts they define.

(cl:defpackage #:clyde-memory
  (:use #:common-lisp #:mops)
  )

(in-package #:clyde-memory)


;;; CONCEPTS

(defmop mammal (animal)
        :numHearts 1
        :numEyes 2)

(defmop elephant (mammal)
        :color gray)

(defmop pig (mammal)
        :color pink)

(definstance clyde-1 (elephant)
             :name clyde
             :age 15
             :color white)

(definstance jumbo-1 (elephant)
             :name jumbo
             :age 40)

(definstance wilbur-1 (pig)
             :name wilbur
             :age 2)

(definstance event-1 (event)
             :actor clyde-1
             :action ingest
             :object peanuts-1)

(definstance state-1 (state)
             :actor clyde-1
             :state nauseous)

(definstance causal-1 (causal)
             :ante event-1
             :conse state-1)


(provide "clyde")
