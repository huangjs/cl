;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Purpose:       Package definition for Modal Logic.
;;;; Author:        R. Scott McIntire
;;;; Date Started:  Aug 2003
;;;;
;;;; $Id: package.lisp,v 1.2 2003/09/10 22:19:25 rscottmcintire Exp $
;;;; *************************************************************************

(in-package #:cl-user)

(defpackage rsm.modal
  (:use #:cl)
  (:shadow #:and #:or #:not)
  (:documentation
   "A package to support modal logic. Basically, a modal system supports a 
logic system augmented with agents and \"agent knowledge operators\" along 
with a collection of possibility worlds. A modal system is described by 
a Kripke system which connects agents to worlds and a structure that 
represents the basic propositions in the logic system. In our treatment, 
all worlds and agents are represented by non-negative integers.

Export Summary:

defmodal    : Macro to define a modal system.
agent-knows : Predicate that determines if an agent knows that a 
              proposition is true.
is-true?    : Predicate that determines if a proposition is true in a 
              given world.
make-world  : Makes a modal system.
satisfies?  : Predicate that determines if a proposition is true in all 
              worlds.

clear-modal-systems : Remove all modal systems.
get-modal-system    : Get a modal system.
set-modal-system    : Set the modal system to use.
")
  (:export
   #:defmodal
   #:agent-knows
   #:is-true?
   #:make-world
   #:satisfies?
   #:clear-modal-systems
   #:get-modal-system
   #:set-modal-system
   ))
